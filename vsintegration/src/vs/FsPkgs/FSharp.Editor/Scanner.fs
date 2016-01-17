// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System.Collections.Generic
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Classification
open Microsoft.CodeAnalysis.Text
open Microsoft.FSharp.Compiler.SourceCodeServices

//    Notes:
//      - SetLineText() is called one line at a time.
//      - An instance of FSharpScanner is associated with exactly one buffer (IVsTextLines).
type internal FSharpScanner(makeLineTokenizer : string -> LineTokenizer) = 
    let mutable lineTokenizer = makeLineTokenizer ""

    let mutable extraColorizations = None
    let tryFindExtraInfo ((* newSnapshot:Lazy<ITextSnapshot>, *) line, c1, c2) = 
        match extraColorizations with 
        | None -> None
        | Some ((* hasTextChanged, *) table:IDictionary<_,_>) -> 
             match table.TryGetValue line with 
             | false,_ -> None
             | true,entries -> 
                 entries |> Array.tryPick (fun ((((_,sc),(_,ec)) as range),t) ->
                     ignore range 
#if COLORIZE_TYPES
                     // If we are colorizing type names, then a lot more late-colorization is going on, and we have to be more precise and
                     // check snapshots. However it is not clear where to get the new snapshot from, or if it is expensive to get it.
                     // This is one of the reasons why COLORIZE_TYPES is not enabled.
                     if sc <= c1 &&  c2+1 <= ec (* && not (hasTextChanged (newSnapshot.Force(),range)) *) then 
#else
                     // If we are only colorizing query keywords, and not types, then we can check the exact token range, and that tends to be enough 
                     // to get pretty good incremental accuracy (while waiting for a re-typecheck to refresh results completely)
                     if sc = c1 &&  c2+1 = ec then 
#endif
                         Some t 
                     else 
                         None)

    /// Decode compiler TokenColorKind into VS TokenColor.
    let lookupTokenColor colorKind = 
        match colorKind with
        | TokenColorKind.Comment -> ClassificationTypeNames.Comment
        | TokenColorKind.Identifier -> ClassificationTypeNames.Identifier
        | TokenColorKind.Keyword -> ClassificationTypeNames.Keyword
        | TokenColorKind.String -> ClassificationTypeNames.StringLiteral
        | TokenColorKind.Text -> ClassificationTypeNames.Text
        | TokenColorKind.UpperIdentifier -> ClassificationTypeNames.Identifier
        | TokenColorKind.Number -> ClassificationTypeNames.NumericLiteral
        | TokenColorKind.InactiveCode -> ClassificationTypeNames.ExcludedCode 
        | TokenColorKind.PreprocessorKeyword -> ClassificationTypeNames.PreprocessorKeyword 
        | TokenColorKind.Operator -> ClassificationTypeNames.Operator
#if COLORIZE_TYPES
        | TokenColorKind.TypeName -> enum 9              // Custom index into colorable item array, 1-based index, see array of FSharpColorableItem in servicem.fs
#endif
        | TokenColorKind.Default | _ -> ClassificationTypeNames.Text

    let lineStateMap = new Dictionary<int, LexState>()

    member this.GetLexStateForLine(lineNumber) =
        match lineStateMap.TryGetValue(lineNumber) with
        | (true, lextState) -> lextState
        | (false, _) -> 0L

    member this.SetLexStateForLine(lineNumber, lexState) =
        match lineStateMap.ContainsKey(lineNumber) with
        | true -> lineStateMap.Item(lineNumber) <- lexState
        | false -> lineStateMap.Add(lineNumber, lexState)

    member this.ScanTokenWithDetails lexState =
        let tokenInfoOption, newLexState = lineTokenizer.ScanToken(!lexState)
        lexState := newLexState
        tokenInfoOption
            
    member this.ScanTokenAndProvideInfoAboutIt(line:TextLine, classifiedSpans:List<ClassifiedSpan>, lexState) =
        let colorInfoOption, newLexState = lineTokenizer.ScanToken(!lexState)
        lexState := newLexState
        match colorInfoOption with 
        | None -> false 
        | Some colorInfo -> 
            let color = 
                // Upgrade identifiers to keywords based on extra info
                match colorInfo.ColorClass with 
                | TokenColorKind.Identifier 
                | TokenColorKind.UpperIdentifier -> 
                    match tryFindExtraInfo (line.LineNumber, colorInfo.LeftColumn, colorInfo.RightColumn) with 
                    | None -> TokenColorKind.Identifier 
                    | Some info -> info // extra info found
                | c -> c

            let classificationType = lookupTokenColor color
            let span = new TextSpan(line.Start + colorInfo.LeftColumn, colorInfo.RightColumn - colorInfo.LeftColumn + 1)
            let classifiedSpan = new ClassifiedSpan(classificationType, span)
            classifiedSpans.Add(classifiedSpan)
            true

    member this.GetTokenInformationAt(document : Document, line : int, col : int) =
        let lexState = ref 0L
        lexState := this.GetLexStateForLine(line)
            
        let text = Async.AwaitTask(document.GetTextAsync()) |> Async.RunSynchronously
        let currentLine = text.Lines.Item(line)
        let lineText = currentLine.Text.ToString(currentLine.Span)
        this.SetLineText lineText
          
        let rec searchForToken () =                
            match this.ScanTokenWithDetails lexState with
            |   None -> None
            |   Some ti as result ->
                if col >= ti.LeftColumn && col <= ti.RightColumn then
                    result
                else
                    searchForToken()
        searchForToken ()
             

    // This is called one line at a time.
    member this.SetLineText lineText = 
        lineTokenizer <- makeLineTokenizer lineText

    /// Adjust the set of extra colorizations and return a sorted list of changed lines.
    member __.SetExtraColorizations (tokens: (Microsoft.FSharp.Compiler.SourceCodeServices.Range * Microsoft.FSharp.Compiler.SourceCodeServices.TokenColorKind)[]) = 
        if tokens.Length = 0 && extraColorizations.IsNone then 
            [| |] 
        else
            let newExtraColorizationsKeyed = dict (tokens |> Seq.groupBy (fun (((sl,_),(_,_)), _) -> sl) |> Seq.map (fun (k,v) -> (k, Seq.toArray v))) 
            let oldExtraColorizationsKeyedOpt = extraColorizations
            extraColorizations <- Some newExtraColorizationsKeyed
            let changedLines = 
                match oldExtraColorizationsKeyedOpt with
                | None -> newExtraColorizationsKeyed.Keys |> Seq.toArray
                | Some oldExtraColorizationsKeyed -> 
                   // When only colorizing query keywords, in most situations we expect both oldExtraColorizationsKeyed and newExtraColorizationsKeyed to be relatively small
                   let inOneButNotTheOther = HashSet(oldExtraColorizationsKeyed.Keys)
                   inOneButNotTheOther.SymmetricExceptWith(newExtraColorizationsKeyed.Keys)
                   let inBoth = HashSet(oldExtraColorizationsKeyed.Keys)
                   inBoth.IntersectWith(newExtraColorizationsKeyed.Keys)
                   inBoth.RemoveWhere(fun i -> newExtraColorizationsKeyed.[i] = oldExtraColorizationsKeyed.[i]) |> ignore
                   Array.append (Seq.toArray inOneButNotTheOther) (Seq.toArray inBoth)
            Array.sortInPlace changedLines
            changedLines

type internal LexStateMap() = 
    let lineStateMap = new Dictionary<int, LexState>()

    member this.GetLexStateForLine(lineNumber) =
        match lineStateMap.TryGetValue(lineNumber) with
        | (true, lextState) -> lextState
        | (false, _) -> 0L

    member this.SetLexStateForLine(lineNumber, lexState) =
        match lineStateMap.ContainsKey(lineNumber) with
        | true -> lineStateMap.Item(lineNumber) <- lexState
        | false -> lineStateMap.Add(lineNumber, lexState)


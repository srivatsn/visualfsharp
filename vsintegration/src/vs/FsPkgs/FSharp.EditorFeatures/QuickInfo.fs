// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.ComponentModel.Composition
open System.Collections.Generic

open Internal.Utilities.Debug

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Editor
open Microsoft.CodeAnalysis.Editor.Shared.Utilities
open Microsoft.CodeAnalysis.Editor.Implementation.IntelliSense.QuickInfo
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Utilities
open Microsoft.VisualStudio.FSharp.LanguageService

module internal OperatorToken =

    let asIdentifierFromInfo (token : TokenInformation) =
        // Typechecker reports information about all values in the same fashion no matter whether it is named value (let binding) or operator
        // here we piggyback on this fact and just pretend that we need data time for identifier
        let tagOfIdentToken = Microsoft.FSharp.Compiler.Parser.tagOfToken(Microsoft.FSharp.Compiler.Parser.IDENT "")
        let endCol = token.RightColumn + 1 // EndIndex from GetTokenInfoAt points to the last operator char, but here it should point to column 'after' the last char 
        tagOfIdentToken, token.LeftColumn, endCol
    
[<ExportQuickInfoProvider("FSharpQuickInfoProvider", "F#")>]
type FSharpQuickInfoProvider [<ImportingConstructor>] (textBufferFactoryService : ITextBufferFactoryService,
                                                       contentTypeRegistryService : IContentTypeRegistryService,
                                                       typeMap : ClassificationTypeMap,
                                                       serviceProvider : SVsServiceProvider) =

    let sp = ServiceProvider(serviceProvider.GetService)
    let documentationProvider = XmlDocumentation.Provider(sp.XmlService, sp.DTE) :> IdealDocumentationProvider

    interface IQuickInfoProvider with
        member this.GetItemAsync(document, position, _) =
            async {
                
                let parts, span = this.GetDataTipTextParts(document, position)
                let content = new ClassifiableDeferredContent(new List<SymbolDisplayPart>(parts), textBufferFactoryService, contentTypeRegistryService, typeMap)
                let item = new QuickInfoItem(span, content)
                return item
            } |> Async.StartAsTask

    member this.CreateTextOnlySymbolDisplayPart(text: string) =
        [SymbolDisplayPart(SymbolDisplayPartKind.Text, null, text)]

    member this.GetDataTipTextParts(document : Document, position : int) =
            // in cases like 'A<int>' when cursor in on '<' there is an ambiguity that cannot be resolved based only on lexer information
            // '<' can be treated both as operator and as part of identifier
            // in this case we'll do 2 passes:
            // 1. treatTokenAsIdentifier=false - we'll pick raw token under the cursor and try find it among resolved names, is attempt was successful - great we are done, otherwise
            // 2. treatTokenAsIdentifier=true - even if raw token was recognized as operator we'll use different branch 
            // that calls QuickParse.GetCompleteIdentifierIsland and then tries previous column...
            let rec getDataTip(alwaysTreatTokenAsIdentifier) =
                let scanner = DocumentData.GetScanner(document)
                let text = Async.AwaitTask(document.GetTextAsync()) |> Async.RunSynchronously
                let linePosition = text.Lines.GetLinePosition(position)
                let line, col = linePosition.Line, linePosition.Character
                let currentLine = text.Lines.Item(line)
                let lineText = currentLine.Text.ToString(currentLine.Span)
                let diagnosticTipSpan = text.Lines.GetTextSpan(LinePositionSpan(LinePosition(line, col), LinePosition(line, col + 1)))

                let tokenOption = scanner.GetTokenInformationAt(document, line, col)
                
                match tokenOption with 
                | None -> ([], diagnosticTipSpan)
                | Some token ->
#if DEBUG
                    use t = Trace.Call("LanguageService",
                                       "GetDataTipText",
                                       fun _->sprintf " line=%d col=%d tokeninfo=%A" line col token)
#endif

                    try
                        // If we're not on the first column; we don't find any identifier, we also look at the previous one
                        // This allows us to do Ctrl+K, I in this case:  let f$ x = x  
                        // Note: this is triggered by hovering over the next thing after 'f' as well - even in 
                        //   case like "f(x)" when hovering over "(", but MPF doesn't show tooltip in that case
                        // Note: MPF also doesn't show the tooltip if we're past the end of the line (Ctrl+K, I after 
                        //  the last character on the line), so tooltip isn't shown in that case (suggestion 4371)
                    
                        // Try the actual column first...
                        let tokenTag, col, possibleIdentifier, makeSecondAttempt =
                          if token.ColorClass = TokenColorKind.Operator && not alwaysTreatTokenAsIdentifier then                      
                              let tag, startCol, endCol = OperatorToken.asIdentifierFromInfo token                      
                              let op = lineText.Substring(startCol, endCol - startCol)
                              tag, startCol, Some(op, endCol, false), true
                          else
                              match (QuickParse.GetCompleteIdentifierIsland false lineText col) with
                              | None when col > 0 -> 
                                  // Try the previous column & get the token info for it
                                  let tokenTag = 
                                      let token = scanner.GetTokenInformationAt(document,line,col - 1).Value
                                      token.Tag
                                  let possibleIdentifier = QuickParse.GetCompleteIdentifierIsland false lineText (col - 1)
                                  tokenTag, col - 1, possibleIdentifier, false
                              | _ as poss -> token.Tag, col, poss, false

#if DEBUG
                        let isDiagnostic = false //TODO: Keyboard.IsKeyPressed Keyboard.Keys.Shift
#else
                        let isDiagnostic = false
#endif 
                        match possibleIdentifier with 
                        | None -> (if isDiagnostic then this.CreateTextOnlySymbolDisplayPart("No identifier found at this position.") else []),diagnosticTipSpan
                        | Some (s,colAtEndOfNames, isQuotedIdentifier) -> 
                            let typedResultsOption = DocumentData.GetTypedResults(document)
                            match typedResultsOption with 
                            | Some typedResults ->
                                // REVIEW: Need to capture and display XML
                                let diagnosticText lead = 
                                    let errorText = String.Concat(typedResults.Errors |> Seq.truncate 5 |> Seq.map(fun pi->sprintf "%s\n" pi.Message)|>Seq.toArray)
                                    let errorText = match errorText.Length with 0->"" | _->"Errors:\n"+errorText
                                    let dataTipText = sprintf "%s\nIsland(col=%d,token=%d):\n%A\n%s%s" lead col tokenTag possibleIdentifier (document.Project.Name) errorText
                                    dataTipText

                                if typedResults.HasFullTypeCheckInfo then 
                                    let qualId  = PrettyNaming.GetLongNameFromString s
#if DEBUG                            
                                    Trace.PrintLine("LanguageService", (fun () -> sprintf "Got qualId = %A" qualId))
#endif
                                    let parserState = None
                                                
                                    // Corrrect the identifier (e.g. to correctly handle active pattern names that end with "BAR" token)
                                    let tokenTag = QuickParse.CorrectIdentifierToken s tokenTag
                                    let dataTip = typedResults.GetDataTipText((line, colAtEndOfNames), lineText, qualId, tokenTag)
#if DEBUG
                                    Trace.PrintLine("LanguageService", fun () -> sprintf "Got datatip=%A" dataTip)
#endif
                                    match dataTip with
                                    | DataTipText [] when makeSecondAttempt -> getDataTip true
                                    | _ -> 
                                    if isDiagnostic then 
                                        let text = sprintf "plid:%A\ndataTip:\n%A" qualId dataTip
                                        let text = 
                                            match parserState with
                                            | None -> text
                                            | Some lines ->
                                                sprintf "%s\n%s\n" text (String.concat "\n" lines)
                                        this.CreateTextOnlySymbolDisplayPart(diagnosticText text), diagnosticTipSpan
                                    else
                                        let dataTipText =  XmlDocumentation.BuildDataTipSymbolDisplayParts(documentationProvider, dataTip)

                                        // The data tip is located w.r.t. the start of the last identifier
                                        let sizeFixup = if isQuotedIdentifier then 4 else 0
                                        let lastStringLength = (qualId |> List.rev |> List.head).Length  + sizeFixup
#if DEBUG
                                        Trace.PrintLine("LanguageService", (fun () -> sprintf "Got dataTip = %A, colOfEndOfText = %d, lastStringLength = %d, line = %d" dataTipText colAtEndOfNames lastStringLength line))
#endif

                                        // This is the span of text over which the data tip is active. If the mouse moves away from it then the
                                        // data tip goes away
                                        let dataTipSpan = text.Lines.GetTextSpan(LinePositionSpan(LinePosition(line, max 0 (colAtEndOfNames-lastStringLength)), LinePosition(line, colAtEndOfNames)))
                                        (dataTipText, dataTipSpan)                                
                                else
                                   this.CreateTextOnlySymbolDisplayPart("Bug: TypeCheckInfo option was None"), diagnosticTipSpan

                            | None -> (if isDiagnostic then this.CreateTextOnlySymbolDisplayPart("No typed results available.") else []),diagnosticTipSpan
                    with e-> 
                        Assert.Exception(e)
                        reraise()

            getDataTip false



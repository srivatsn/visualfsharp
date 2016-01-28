// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.LanguageService
open Microsoft.VisualStudio.FSharp.LanguageService
open System
open System.Text
open System.Collections.Generic
open Internal.Utilities.Collections
open Microsoft.VisualStudio
open EnvDTE
open EnvDTE80
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text

/// Documentation helpers.
module internal XmlDocumentation =
    /// If the XML comment starts with '<' not counting whitespace then treat it as a literal XML comment.
    /// Otherwise, escape it and surround it with <summary></summary>
    let ProcessXml(xml:string) =
        if String.IsNullOrEmpty(xml) then xml
        else
            let trimmedXml = xml.TrimStart([|' ';'\r';'\n'|])
            if trimmedXml.Length>0 then
                if trimmedXml.[0] <> '<' then 
                    // This code runs for local/within-project xmldoc tooltips, but not for cross-project or .XML - for that see ast.fs in the compiler
                    let escapedXml = System.Security.SecurityElement.Escape(xml)
                    "<summary>" + escapedXml + "</summary>"
                else 
                    "<root>" + xml + "</root>"
            else xml

    /// Provide Xml Documentation             
    type Provider(xmlIndexService:IVsXMLMemberIndexService, dte: DTE) = 
        /// Index of assembly name to xml member index.
        let mutable xmlCache = new AgedLookup<string,IVsXMLMemberIndex>(10,areSame=(fun (x,y) -> x = y))
        
        let events = dte.Events :?> Events2
        let solutionEvents = events.SolutionEvents        
        do solutionEvents.add_AfterClosing(fun () -> 
            xmlCache.Clear())

        let HasTrailingEndOfLine(sb:StringBuilder) = 
            if sb.Length = 0 then true
            else
                let c = sb.[sb.Length-1]
                c = '\r' || c = '\n'
                
        let AppendHardLine(sb:StringBuilder) =
            sb.AppendLine() |> ignore            
       
        let AppendOnNewLine (sb:StringBuilder) (line:string) =
            if line.Length>0 then 
                if not(HasTrailingEndOfLine(sb)) then 
                    sb.AppendLine("")|>ignore
                sb.Append(line.TrimEnd([|' '|]))|>ignore                      
                    
        let AppendSummary (sb:StringBuilder) (memberData:IVsXMLMemberData3) = 
            let ok,summary = memberData.GetSummaryText()
            if Com.Succeeded(ok) then 
                AppendOnNewLine sb summary
            else 
                // Failed, but still show the summary because it may contain an error message.
                if summary<>null then AppendOnNewLine sb summary
            
    #if DEBUG // Keep under DEBUG so that it can keep building.

        let _AppendTypeParameters (sb:StringBuilder) (memberData:IVsXMLMemberData3) = 
            let ok,count = memberData.GetTypeParamCount()
            if Com.Succeeded(ok) && count>0 then 
                if not(HasTrailingEndOfLine(sb)) then 
                    AppendHardLine(sb)
                for param in 0..count do
                    let ok,name,text = memberData.GetTypeParamTextAt(param)
                    if Com.Succeeded(ok) then 
                        AppendOnNewLine sb (sprintf "%s - %s" name text)
                
        let _AppendRemarks (sb:StringBuilder) (memberData:IVsXMLMemberData3) = 
            let ok,remarksText = memberData.GetRemarksText()
            if Com.Succeeded(ok) then 
                AppendOnNewLine sb remarksText            
    #endif

        let AppendParameters (sb:StringBuilder) (memberData:IVsXMLMemberData3) = 
            let ok,count = memberData.GetParamCount()
            if Com.Succeeded(ok) && count > 0 then 
                if not(HasTrailingEndOfLine(sb)) then 
                    AppendHardLine(sb)
                    AppendHardLine(sb)
                for param in 0..(count-1) do
                    let ok,name,text = memberData.GetParamTextAt(param)
                    if Com.Succeeded(ok) then 
                        AppendOnNewLine sb (sprintf "%s: %s" name text)

        let AppendParameter (sb:StringBuilder, memberData:IVsXMLMemberData3, paramName:string) =
            let ok,count = memberData.GetParamCount()
            if Com.Succeeded(ok) && count > 0 then 
                if not(HasTrailingEndOfLine(sb)) then 
                    AppendHardLine(sb)
                for param in 0..(count-1) do
                    let ok,name,text = memberData.GetParamTextAt(param)
                    if Com.Succeeded(ok) && name = paramName then 
                        AppendOnNewLine sb text

        let _AppendReturns (sb:StringBuilder) (memberData:IVsXMLMemberData3) = 
            let ok,returnsText = memberData.GetReturnsText()
            if Com.Succeeded(ok) then 
                if not(HasTrailingEndOfLine(sb)) then 
                    AppendHardLine(sb)
                    AppendHardLine(sb)
                AppendOnNewLine sb returnsText
            
        let AppendExceptions (sb:StringBuilder) (memberData:IVsXMLMemberData3) = 
            let ok,count = memberData.GetExceptionCount()
            if Com.Succeeded(ok) && count > 0 then 
                if count > 0 then 
                    AppendHardLine sb
                    AppendHardLine sb
                    AppendOnNewLine sb Strings.ExceptionsHeader
                    for exc in 0..count do
                        let ok,typ,_text = memberData.GetExceptionTextAt(exc)
                        if Com.Succeeded(ok) then 
                            AppendOnNewLine sb (sprintf "    %s" typ )
                
        /// Retrieve the pre-existing xml index or None
        let GetMemberIndexOfAssembly(assemblyName) =
            match xmlCache.TryGet(assemblyName) with 
            | Some(memberIndex) -> Some(memberIndex)
            | None -> 
                let ok,memberIndex = xmlIndexService.CreateXMLMemberIndex(assemblyName)
                if Com.Succeeded(ok) then 
                    let ok = memberIndex.BuildMemberIndex()
                    if Com.Succeeded(ok) then 
                        xmlCache.Put(assemblyName,memberIndex)
                        Some(memberIndex)
                    else None
                else None

        let AppendMemberData(appendTo:StringBuilder,memberData:IVsXMLMemberData3,showExceptions:bool,showParameters:bool) =
            AppendHardLine appendTo
            AppendSummary appendTo memberData
//          AppendParameters appendTo memberData
//          AppendTypeParameters appendTo memberData
            if (showParameters) then 
                AppendParameters appendTo memberData
                // Not showing returns because there's no resource localization in language service to place the "returns:" text
                // AppendReturns appendTo memberData
            if (showExceptions) then AppendExceptions appendTo memberData
//          AppendRemarks appendTo memberData

        interface IdealDocumentationProvider with 
            /// Append the given processed XML formatted into the string builder
            override this.AppendDocumentationFromProcessedXML
                            ( /// StringBuilder to append to
                              appendTo:StringBuilder,
                              /// The processed XML text.
                              processedXml:string,
                              /// Whether to show exceptions
                              showExceptions:bool,
                              /// Whether to show parameters and return
                              showParameters:bool,
                              /// Name of parameter
                              paramName:string option
                             ) = 
                let ok,xml = xmlIndexService.GetMemberDataFromXML(processedXml)
                if Com.Succeeded(ok) then 
                    if paramName.IsSome then
                        AppendParameter(appendTo, xml:?>IVsXMLMemberData3, paramName.Value)
                    else
                        AppendMemberData(appendTo,xml:?>IVsXMLMemberData3,showExceptions,showParameters)

            /// Append Xml documentation contents into the StringBuilder
            override this.AppendDocumentation
                            ( /// StringBuilder to append to
                              appendTo:StringBuilder,
                              /// Name of the library file
                              filename:string,
                              /// Signature of the comment
                              signature:string,
                              /// Whether to show exceptions
                              showExceptions:bool,
                              /// Whether to show parameters and return
                              showParameters:bool,
                              /// Name of parameter
                              paramName:string option                            
                             ) = 
                try     
                    match GetMemberIndexOfAssembly(filename) with
                    | Some(index) ->
                        let _,idx = index.ParseMemberSignature(signature)
                        if idx <> 0u then
                            let ok,xml = index.GetMemberXML(idx)
                            let processedXml = ProcessXml(xml)
                            if Com.Succeeded(ok) then 
                                (this:>IdealDocumentationProvider).AppendDocumentationFromProcessedXML(appendTo,processedXml,showExceptions,showParameters, paramName)
                    | None -> ()
                with e-> 
                    Assert.Exception(e)
                    reraise()    
 
    /// Append an XmlCommnet to the segment.
    let AppendXmlComment(documentationProvider:IdealDocumentationProvider, segment:StringBuilder, xml, showExceptions, showParameters, paramName) =
        match xml with
        | XmlCommentNone -> ()
        | XmlCommentSignature(filename,signature) -> 
            segment.Append("\n") |> ignore
            documentationProvider.AppendDocumentation(segment,filename,signature,showExceptions,showParameters, paramName)
        | XmlCommentText(rawXml) ->
            let processedXml = ProcessXml(rawXml)
            segment.Append("\n") |> ignore
            documentationProvider.AppendDocumentationFromProcessedXML(segment,processedXml,showExceptions,showParameters, paramName)

    /// Common sanitation for data tip segment
    let CleanDataTipSegment(segment:StringBuilder) =     
        segment.Replace("\r", "")
          .Replace("\n\n\n","\n\n")
          .Replace("\n\n\n","\n\n") 
          .ToString()
          .Trim([|'\n'|])

    let CreateTextOnlySymbolDisplayPart(text: string) =
        [SymbolDisplayPart(SymbolDisplayPartKind.Text, null, text)]

    let ScanAndCreateSymbolDisplayParts(text : string) =
        let scanner =  new FSharpScanner(fun source ->
                let sourceTokenizer = SourceTokenizer([],"")
                sourceTokenizer.CreateLineTokenizer(source))
        let lexState = ref 0L

        let sourceText = SourceText.From(text)

        let scanOneLineAndCreateParts(line : TextLine, lexState) = 
            let lineText = line.Text.ToString(line.Span)
            scanner.SetLineText(lineText)

            let getSymbolDisplayKind colorClass =
                match colorClass with 
                | TokenColorKind.Comment -> SymbolDisplayPartKind.Text
                | TokenColorKind.Identifier -> SymbolDisplayPartKind.Text
                | TokenColorKind.Keyword -> SymbolDisplayPartKind.Keyword
                | TokenColorKind.String -> SymbolDisplayPartKind.StringLiteral
                | TokenColorKind.Text -> SymbolDisplayPartKind.Text
                | TokenColorKind.UpperIdentifier -> SymbolDisplayPartKind.Text
                | TokenColorKind.Number -> SymbolDisplayPartKind.NumericLiteral
                | TokenColorKind.InactiveCode -> SymbolDisplayPartKind.Text
                | TokenColorKind.PreprocessorKeyword -> SymbolDisplayPartKind.Keyword
                | TokenColorKind.Operator -> SymbolDisplayPartKind.Operator
#if COLORIZE_TYPES
                | TokenColorKind.TypeName -> SymbolDisplayPartKind.ClassName
#endif
                | TokenColorKind.Default | _ -> SymbolDisplayPartKind.Text

            let rec scanAndCreateParts(parts, lexState) = 
                let tokenInfoOpt = scanner.ScanTokenWithDetails(lexState)
                match tokenInfoOpt with
                | None -> parts
                | Some tokenInfo -> 
                    let partKind = getSymbolDisplayKind tokenInfo.ColorClass
                    let span = LinePositionSpan(LinePosition(line.LineNumber, tokenInfo.LeftColumn), LinePosition(line.LineNumber, tokenInfo.RightColumn + 1))
                    let tokenText = line.Text.ToString(line.Text.Lines.GetTextSpan(span))
                    let part = new SymbolDisplayPart(partKind, null, tokenText)
                    let newParts = part :: parts
                    scanAndCreateParts(newParts, lexState)

            let newLinePart = new SymbolDisplayPart(SymbolDisplayPartKind.Punctuation, null, "\n")
            let parts = newLinePart :: scanAndCreateParts([], lexState)
            List.rev(parts)

        sourceText.Lines |> Seq.fold(fun acc line -> acc @ scanOneLineAndCreateParts(line, lexState)) []

    /// Build a data tip text string with xml comments injected.
    let BuildTipText(documentationProvider:IdealDocumentationProvider, dataTipText:DataTipElement list, showText, showExceptions, showParameters, showOverloadText) = 
        let maxLinesInText = 45
        let Format(dataTipElement:DataTipElement) =
            let segment, symbolDisplayParts = 
                match dataTipElement with 
                | DataTipElementNone->"", []
                | DataTipElement(text,xml) -> 
                    let segment = StringBuilder()
                    if showText then 
                        segment.Append(text) |> ignore

                    let xmlSegment = StringBuilder()
                    AppendXmlComment(documentationProvider, xmlSegment, xml, showExceptions, showParameters, None)
                    let cleanSegment = CleanDataTipSegment(segment)
                    let cleanXmlSegment = CleanDataTipSegment(xmlSegment)
                    let segmentText = if not(String.IsNullOrEmpty(cleanXmlSegment)) then cleanSegment + "\n" + cleanXmlSegment else cleanSegment
                    let segmentParts  = if not(String.IsNullOrEmpty(cleanXmlSegment)) then ScanAndCreateSymbolDisplayParts(cleanSegment) @ CreateTextOnlySymbolDisplayPart("\n") @ CreateTextOnlySymbolDisplayPart(cleanXmlSegment) else ScanAndCreateSymbolDisplayParts(cleanSegment)
                    segmentText, segmentParts
                | DataTipElementParameter(text, xml, paramName) ->
                    let segment = StringBuilder()
                    if showText then 
                        segment.Append(text) |> ignore
                        
                    let xmlSegment = StringBuilder()
                    AppendXmlComment(documentationProvider, xmlSegment, xml, showExceptions, showParameters, Some paramName)
                    let cleanSegment = CleanDataTipSegment(segment)
                    let cleanXmlSegment = CleanDataTipSegment(xmlSegment)
                    let segmentText = if not(String.IsNullOrEmpty(cleanXmlSegment)) then cleanSegment + "\n" + cleanXmlSegment else cleanSegment
                    let segmentParts  = if not(String.IsNullOrEmpty(cleanXmlSegment)) then ScanAndCreateSymbolDisplayParts(cleanSegment) @ CreateTextOnlySymbolDisplayPart("\n") @ CreateTextOnlySymbolDisplayPart(cleanXmlSegment) else ScanAndCreateSymbolDisplayParts(cleanSegment)
                    segmentText, segmentParts
                | DataTipElementGroup(overloads) -> 
                    let segment = StringBuilder()
                    let xmlSegment = StringBuilder()
                    let overloads = Array.ofList overloads
                    let len = Array.length overloads
                    if len >= 1 then 
                        if showOverloadText then 
                            let AppendOverload(text,_) = 
                                if not(String.IsNullOrEmpty(text)) then
                                    segment.Append("\n").Append(text) |> ignore

                            AppendOverload(overloads.[0])
                            if len >= 2 then AppendOverload(overloads.[1])
                            if len >= 3 then AppendOverload(overloads.[2])
                            if len >= 4 then AppendOverload(overloads.[3])
                            if len >= 5 then AppendOverload(overloads.[4])
                            if len >= 6 then segment.Append("\n").Append(PrettyNaming.FormatAndOtherOverloadsString(len-5)) |> ignore

                        let _,xml = overloads.[0]
                        AppendXmlComment(documentationProvider, xmlSegment, xml, showExceptions, showParameters, None)
                    let cleanSegment = CleanDataTipSegment(segment)
                    let cleanXmlSegment = CleanDataTipSegment(xmlSegment)
                    let segmentText = if not(String.IsNullOrEmpty(cleanXmlSegment)) then cleanSegment + "\n" + cleanXmlSegment else cleanSegment
                    let segmentParts  = if not(String.IsNullOrEmpty(cleanXmlSegment)) then ScanAndCreateSymbolDisplayParts(cleanSegment) @ CreateTextOnlySymbolDisplayPart("\n") @ CreateTextOnlySymbolDisplayPart(cleanXmlSegment) else ScanAndCreateSymbolDisplayParts(cleanSegment)
                    segmentText, segmentParts
                | DataTipElementCompositionError(errText) -> (errText, CreateTextOnlySymbolDisplayPart(errText))
            segment, symbolDisplayParts

        let segments = dataTipText |> List.map Format |> List.filter (fun (d, _)->d<>null) |> Array.ofList

        let text = segments |> Seq.fold(fun acc (text, _) -> acc + "\n-------------\n" + text) ""
        let parts = segments |> Seq.fold(fun acc (_, parts) -> acc @ CreateTextOnlySymbolDisplayPart("\n-------------\n") @ parts) []
        let parts = if parts.IsEmpty then [] else parts |> List.tail

        let lines = text.Split([|'\n'|],maxLinesInText+1) // Need one more than max to determine whether there is truncation.
        let truncate = lines.Length>maxLinesInText            
        let lines = lines |> Seq.truncate maxLinesInText 
        let lines = if truncate then Seq.append lines ["..."] else lines
        let lines = lines |> Seq.toArray
        let join = String.Join("\n",lines)

        join, parts

    let BuildDataTipSymbolDisplayParts(documentationProvider:IdealDocumentationProvider, DataTipText(dataTipText)) =
        let _ , parts = BuildTipText(documentationProvider,dataTipText,true, true, false, true) 
        parts

    let BuildDataTipText(documentationProvider:IdealDocumentationProvider, DataTipText(dataTipText)) = 
        let text , _ = BuildTipText(documentationProvider,dataTipText,true, true, false, true) 
        text

    let BuildMethodOverloadTipText(documentationProvider:IdealDocumentationProvider, DataTipText(dataTipText)) = 
        let text , _ = BuildTipText(documentationProvider,dataTipText,false, false, true, false) 
        text

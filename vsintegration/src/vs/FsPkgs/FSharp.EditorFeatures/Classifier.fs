// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System.Composition
open System.Collections.Generic
open System.Threading

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Classification
open Microsoft.CodeAnalysis.Editor
open Microsoft.CodeAnalysis.Editor.Implementation.Classification
open Microsoft.CodeAnalysis.Editor.Shared.Utilities
open Microsoft.CodeAnalysis.Host.Mef
open Microsoft.CodeAnalysis.Text

open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Tagging

open Microsoft.FSharp.Compiler.SourceCodeServices

[<ExportLanguageService(typeof<IEditorClassificationService>, "F#")>]
type FSharpEditorClassificationService() =
    interface IEditorClassificationService with
        override this.AddLexicalClassifications(_,_ ,_ ,_) = () 
        
        override this.AddSyntacticClassificationsAsync(document, textSpan, result, cancellationToken) = 
            async {
                let! text = document.GetTextAsync(cancellationToken) |> Async.AwaitTask
            
                let startLine = text.Lines.GetLineFromPosition(textSpan.Start)
                let endLine = text.Lines.GetLineFromPosition(textSpan.End)

                let lexState = ref 0L
                let scanner = DocumentData.GetScanner(document)

                for lineNumber = startLine.LineNumber to endLine.LineNumber do
                    let currentLine = text.Lines.Item(lineNumber)
                    lexState := scanner.GetLexStateForLine(lineNumber)

                    let lineText = currentLine.Text.ToString(currentLine.Span)
                    scanner.SetLineText(lineText)

                    while scanner.ScanTokenAndProvideInfoAboutIt(currentLine, result, lexState) do ()

                    scanner.SetLexStateForLine(lineNumber, lexState.Value)

            } |> Async.StartAsTask :> _

        override this.AddSemanticClassificationsAsync(_,_,_,_) = Tasks.Task.FromResult(null) :> _
        override this.AdjustStaleClassification(_, classifiedSpan : ClassifiedSpan) = classifiedSpan

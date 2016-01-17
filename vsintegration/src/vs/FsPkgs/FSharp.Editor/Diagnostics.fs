// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Threading.Tasks
open System.Collections.Immutable
open System.Linq

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Diagnostics
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Host.Mef
open Microsoft.CodeAnalysis.SolutionCrawler
open Microsoft.CodeAnalysis.CodeActions
open Microsoft.CodeAnalysis.CodeFixes

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices

[<ExportLanguageService(typeof<IDocumentDifferenceService>, "F#")>]
type FSharpDocumentDifferenceService() =
    interface IDocumentDifferenceService with
        member this.GetDifferenceAsync(_,_,_) =
            // Just say that the entire document changed. We don't do analysis to be incremental.
            Task.FromResult(new DocumentDifferenceResult(InvocationReasons.DocumentChanged))
      
[<DiagnosticAnalyzer("F#")>]
type FSharpDocumentAnalyzer() = 
    inherit DocumentDiagnosticAnalyzer()
    
    override this.SupportedDiagnostics = ImmutableArray.Create()

    override this.AnalyzeSyntaxAsync(document, addDiagnostic, cancellationToken) =
        async {
            let! text = document.GetTextAsync(cancellationToken) |> Async.AwaitTask

            if not(String.IsNullOrEmpty(document.FilePath)) then 
                let typedResultsOption = DocumentData.GetTypedResults(document)

                match typedResultsOption with
                | Some typedResults ->
                    for error in typedResults.Errors do 
                        let severity = 
                            match error.Severity with
                            | Severity.Warning -> DiagnosticSeverity.Warning
                            | Severity.Error -> DiagnosticSeverity.Error
                        let descriptor = new DiagnosticDescriptor(error.Id, LocalizableString.op_Implicit(""), LocalizableString.op_Implicit(error.Message), error.Subcategory, severity, true, LocalizableString.op_Implicit(""), "", null)
                        
                        let location =
                            match (error.StartLine, error.EndLine) with 
                            | (-1, _) -> Location.None
                            | (_, -1) -> Location.None
                            | _ -> let linePositionSpan =  LinePositionSpan(LinePosition(error.StartLine, error.StartColumn), LinePosition(error.EndLine, error.EndColumn))
                                   Location.Create(error.FileName, text.Lines.GetTextSpan(linePositionSpan) , linePositionSpan)
    
                        let diagnostic = Diagnostic.Create(descriptor, location)
                        addDiagnostic.Invoke(diagnostic)
                | None -> ()
            
        } |> Async.StartAsTask :> _

    override this.AnalyzeSemanticsAsync(_,_,_) =
        Task.FromResult(null) :> _
        
[<ExportCodeFixProvider("F#")>]
type FSharpGenerateTypeFixer() = 
    inherit CodeFixProvider()

    override this.FixableDiagnosticIds =  ImmutableArray.Create("FS39")

    override this.GetFixAllProvider() = null

    override this.RegisterCodeFixesAsync(context) =
        async {
            let document = context.Document
            let! text = document.GetTextAsync(context.CancellationToken) |> Async.AwaitTask

            let functionName = text.GetSubText(context.Span).ToString()

            let currentLine = text.Lines.GetLineFromPosition(context.Span.Start)
            let currentLineText = text.GetSubText(currentLine.Span).ToString()
            let indentationLevel = currentLineText |> Seq.findIndex(fun c -> c <> ' ')

            let generatedCode = Enumerable.Repeat(" ", indentationLevel).Aggregate(fun x y -> x + y) + "let " + functionName + "() = ()\r\n"

            let newSpan = TextSpan(currentLine.Span.Start, 0)
            let textChange = TextChange(newSpan, generatedCode)
            let newDocument = document.WithText(text.WithChanges(textChange))

            let codeAction = CodeAction.Create("Generate function", fun ct -> Task.Run(fun _ -> newDocument))
            context.RegisterCodeFix(codeAction, context.Diagnostics)
        } |> Async.StartAsTask :> _



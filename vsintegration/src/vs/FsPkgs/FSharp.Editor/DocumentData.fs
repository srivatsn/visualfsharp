// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System.Runtime.CompilerServices
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.FSharp.Compiler.SourceCodeServices

type internal DocumentData() = 
    static member private scannerData = new ConditionalWeakTable<Document, FSharpScanner>()
    static member private typedResultsData = new ConditionalWeakTable<Document, TypeCheckResults>()

    static member GetScanner(document : Document) = 
        let createScanner(document : Document) = 
            new FSharpScanner(fun source ->
                // Note: in theory, the next few lines do not need to be recomputed every line.  Instead we could just cache the tokenizer
                // and only update it when e.g. the project system notifies us there is an important change (e.g. a file rename, etc).
                // In practice we have been there, and always screwed up some non-unit-tested/testable corner-cases.
                // So this is not ideal from a perf perspective, but it is easy to reason about the correctness.
                let filename = document.FilePath
                let defines = []
                let sourceTokenizer = SourceTokenizer(defines,filename)
                sourceTokenizer.CreateLineTokenizer(source))

        DocumentData.scannerData.GetValue(document, ConditionalWeakTable.CreateValueCallback(createScanner))

    static member GetTypedResults(document : Document) =
        let createTypedResults(document : Document) =
            let text = document.GetTextAsync() |> Async.AwaitTask |> Async.RunSynchronously
            let interactiveChecker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty(fun _ -> ()))
            let hostProjectService = document.Project.Solution.Workspace.Services.GetService<IHostProjectService>()
            let hostProject = hostProjectService.GetHostProject(document.Project.Id)
            let checkOptions = hostProject.CheckOptions

            let untypedParse = interactiveChecker.UntypedParse(document.FilePath, text.ToString(), checkOptions)
                
            try
                interactiveChecker.StartBackgroundCompile(checkOptions)
                interactiveChecker.WaitForBackgroundCompile()
            with
                | _ -> ()
    
            let version = document.GetTextVersionAsync() |> Async.AwaitTask |> Async.RunSynchronously
                
            let neverObsolete() = false
            let typedResults = 
                match interactiveChecker.TypeCheckSource(untypedParse, document.FilePath, version.GetHashCode(), text.ToString(), checkOptions, IsResultObsolete(neverObsolete), text.Container.GetTextBuffer()) with 
                | NoAntecedant -> None
                | Aborted -> 
                    // isResultObsolete returned true during the type check.
                    None
                | TypeCheckSucceeded results -> Some results
            typedResults

        let found, typedResults = DocumentData.typedResultsData.TryGetValue(document)
        match found with 
        | true -> Some typedResults
        | false -> match createTypedResults(document) with
                   | None -> None
                   | Some typedResults as typedResultsOption -> 
                        DocumentData.typedResultsData.Add(document, typedResults)
                        typedResultsOption


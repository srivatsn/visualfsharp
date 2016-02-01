// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.LanguageService

open System
open System.Collections.Generic
open System.Runtime.InteropServices

open Microsoft.FSharp.Compiler.SourceCodeServices

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.SolutionCrawler
open Microsoft.VisualStudio
open Microsoft.VisualStudio.LanguageServices
open Microsoft.VisualStudio.LanguageServices.Implementation.LanguageService
open Microsoft.VisualStudio.LanguageServices.Implementation.ProjectSystem
open Microsoft.VisualStudio.LanguageServices.Implementation
open Microsoft.VisualStudio.Shell.Interop

module FSharpConstants = 
    // These are the IDs from fslangservice.dll
    [<Literal>]
    let packageGuidString               = "871D2A70-12A2-4e42-9440-425DD92A4116"
    [<Literal>]
    let languageServiceGuidString       = Microsoft.VisualStudio.FSharp.Shared.FSharpCommonConstants.languageServiceGuidString

    [<Literal>]
    let FSharpLanguageName = "F#"
    [<Literal>]
    let FSharpContentType = "F#"


[<Guid(FSharpConstants.languageServiceGuidString)>]
type FSharpLanguageService(package : FSharpPackage) = 
    inherit AbstractLanguageService<FSharpPackage, FSharpLanguageService, FSharpProject>(package)

    override this.ContentTypeName = FSharpConstants.FSharpContentType
    override this.LanguageName = FSharpConstants.FSharpLanguageName
    override this.RoslynLanguageName = FSharpConstants.FSharpLanguageName

    override this.LanguageServiceId = new Guid(FSharpConstants.languageServiceGuidString)
    override this.DebuggerLanguageId = DebuggerEnvironment.GetLanguageID()

    override this.CreateContext(_,_,_,_,_) = raise(System.NotImplementedException())

    override this.SetupNewTextView(view) =
        base.SetupNewTextView(view)
        let workspace = this.Package.ComponentModel.GetService<VisualStudioWorkspaceImpl>();
        let sp = new ServiceProvider(this.SystemServiceProvider.GetService)

        // Ensure that we have a project in the workspace for this document.
        let (_, buffer) = view.GetBuffer()
        let filename = VsTextLines.GetFilename buffer
        let result = VsRunningDocumentTable.FindDocumentWithoutLocking(sp.Rdt,filename)
        match result with
        | Some (hier, _) ->
            match hier with
            | :? IProvideProjectSite as siteProvider ->
                let site = siteProvider.GetProjectSite()

                let projectId = workspace.ProjectTracker.GetOrCreateProjectIdForPath(site.ProjectFileName(), site.ProjectFileName())
                if obj.ReferenceEquals(workspace.ProjectTracker.GetProject(projectId), null) then
                    let fsharpProject = new FSharpProject(hier, this.SystemServiceProvider, workspace, site.ProjectFileName());
                    fsharpProject.Initialize(hier, site)                    
            | _ -> ()
        | _ -> ()

and [<Guid("4EB7CCB7-4336-4FFD-B12B-396E9FD079A9")>]
    FSharpEditorFactory(package : FSharpPackage) =
    inherit AbstractEditorFactory(package)

    override this.ContentTypeName = FSharpConstants.FSharpContentType
    override this.GetFormattedTextChanges(_, _, _, _) = System.Collections.Generic.List<Text.TextChange>() :> System.Collections.Generic.IList<Text.TextChange>

and [<Guid(FSharpConstants.packageGuidString)>]
    FSharpPackage() = 
    inherit AbstractPackage<FSharpPackage, FSharpLanguageService, FSharpProject>()
    
    override this.RoslynLanguageName = FSharpConstants.FSharpLanguageName

    override this.Initialize() = 
        base.Initialize()

    override this.CreateWorkspace() = this.ComponentModel.GetService<VisualStudioWorkspaceImpl>()
    
    override this.CreateLanguageService() = 
        let language = new FSharpLanguageService(this)
        language

    override this.CreateEditorFactories() = 
        //let factory = FSharpEditorFactory(this) :> IVsEditorFactory
        [] :> IEnumerable<IVsEditorFactory>

    override this.RegisterMiscellaneousFilesWorkspaceInformation(_) = ()


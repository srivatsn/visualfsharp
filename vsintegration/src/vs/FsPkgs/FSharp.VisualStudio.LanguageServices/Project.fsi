// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.LanguageService
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.LanguageServices.Implementation.ProjectSystem
open System

module internal ProjectSiteOptions = 
    val Create : IProjectSite*string -> CheckOptions

[<Class>]
type FSharpProject = 
    inherit AbstractProject
    new : hierarychy:IVsHierarchy * serviceProvider:IServiceProvider * visualStudioWorkspaceImpl:VisualStudioWorkspaceImpl * projectName:string -> FSharpProject
    member internal CheckOptions : CheckOptions
    member internal AddReference : filePath:string -> int
    member internal RemoveReference : filePath:string -> unit
    member internal Initialize : hier: IVsHierarchy * site:IProjectSite -> unit
    member internal OnProjectSettingsChanged : hier: IVsHierarchy * site:IProjectSite -> unit

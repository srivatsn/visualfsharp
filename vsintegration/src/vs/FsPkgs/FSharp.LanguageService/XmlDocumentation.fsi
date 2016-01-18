// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.LanguageService
open System
open System.Text
open Microsoft.CodeAnalysis
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.FSharp.Compiler.SourceCodeServices
open EnvDTE

module internal XmlDocumentation = 
    type Provider =
        interface IdealDocumentationProvider
        new : xmlIndexService:IVsXMLMemberIndexService * dte: DTE -> Provider

    val BuildDataTipSymbolDisplayParts :  IdealDocumentationProvider * DataTipText -> SymbolDisplayPart list

    /// Build a data tip text string with xml comments injected.
    val BuildDataTipText :  IdealDocumentationProvider * DataTipText -> string

    /// Build a data tip text string with xml comments injected.
    val BuildMethodOverloadTipText :  IdealDocumentationProvider * DataTipText -> string

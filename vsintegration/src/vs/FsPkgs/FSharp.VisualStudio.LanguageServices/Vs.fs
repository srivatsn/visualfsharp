// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.LanguageService
open System
open System.IO
open System.Collections
open System.Collections.Generic
open System.Reflection
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.OLE.Interop
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 
open Internal.Utilities.Debug
open Com 
open System.Runtime.InteropServices


/// Strongly typed QueryService    
type internal ServiceProvider(getService:Type->obj) = 
    member private sp.GetService<'S,'T>():'T = unbox(box(getService (typeof<'T>)))
    member sp.Rdt:IVsRunningDocumentTable = downcast (getService (typeof<SVsRunningDocumentTable>))
    member sp.XmlService:IVsXMLMemberIndexService = downcast (getService (typeof<SVsXMLMemberIndexService>))
    member sp.DTE:EnvDTE.DTE = downcast (getService (typeof<SDTE>))
    static member Stub = ServiceProvider(fun _t->raise (Error.UseOfUnitializedServiceProvider))

module internal VsUserData = 
    let vsBufferMoniker = Guid("978A8E17-4DF8-432A-9623-D530A26452BC")

    // This is the file name of the buffer.
    let GetBufferMonker(ud:IVsUserData) : string = 
        downcast Com.ThrowOnFailure1(ud.GetData(ref vsBufferMoniker))
       
/// Isolate IVsTextLines as much as possible to ease transition into new editor architecture
module internal VsTextLines =
    // Get the length of the given line.
    let LengthOfLine (buffer:IVsTextBuffer) (line:int) : int = 
        ThrowOnFailure1(buffer.GetLengthOfLine(line))
    /// Get the text for a particular line.
    let LineText (buffer:IVsTextLines) line = 
        ThrowOnFailure1(buffer.GetLineText(line, 0, line, LengthOfLine buffer line))
    /// Get the color state
    let TextColorState (buffer:IVsTextLines) : IVsTextColorState= unbox(box(buffer))
    /// Get the filename of the given buffer (via IVsUserData). Not all buffers have a file. This will be an exception.
    let GetFilename(buffer : IVsTextLines) =
        let ud = (box buffer) :?> IVsUserData
        VsUserData.GetBufferMonker(ud)

module internal VsRunningDocumentTable = 
    let FindDocumentWithoutLocking(rdt:IVsRunningDocumentTable, url:string) : (IVsHierarchy * IVsTextLines) option =
        let (hr:int, hier:IVsHierarchy, _itemid:uint32, unkData:IntPtr, _cookie:uint32) = rdt.FindAndLockDocument(uint32 _VSRDTFLAGS.RDT_NoLock, url)
        try
            if Com.Succeeded(hr) then 
                let bufferObject = 
                    if unkData=IntPtr.Zero then null
                    else Marshal.GetObjectForIUnknown(unkData)
                let buffer = 
                    match bufferObject with 
                    | :? IVsTextLines as tl -> tl
                    | _ -> null
                Some(hier, buffer)
            else None
        finally 
            if IntPtr.Zero <> unkData then Marshal.Release(unkData)|>ignore

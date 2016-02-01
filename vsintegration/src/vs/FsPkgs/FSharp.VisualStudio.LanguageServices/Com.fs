// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.LanguageService
open Microsoft.VisualStudio.FSharp.LanguageService
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 
open Microsoft.VisualStudio
open System.Runtime.InteropServices
open Internal.Utilities.Debug
open System

/// Helper methods for interoperating with COM                
module internal Com = 
   /// Execute managed code and return COM error information
    let Method methodDescription f = 
        Trace.Print("LanguageService", fun () -> sprintf "Enter %s\n" methodDescription)
        try 
            f() 
            Trace.Print("LanguageService", fun () -> sprintf "Exit %s normally\n" methodDescription)
            VSConstants.S_OK
        with e -> 
            Trace.Print("LanguageService", fun () -> sprintf "Exit %s with exception %A\n" methodDescription e)
            VSConstants.E_FAIL

    let ThrowOnFailure0(hr) = 
        ErrorHandler.ThrowOnFailure(hr)  |> ignore
        
    let ThrowOnFailure1(hr,res) = 
        ErrorHandler.ThrowOnFailure(hr) |> ignore; 
        res
        
    let ThrowOnFailure2(hr,res1,res2) = 
        ErrorHandler.ThrowOnFailure(hr) |> ignore; 
        res1,res2
        
    let ThrowOnFailure3(hr,res1,res2,res3) = 
        ErrorHandler.ThrowOnFailure(hr) |> ignore; 
        res1,res2,res3

    let ThrowOnFailure4(hr,res1,res2,res3,res4) = 
        ErrorHandler.ThrowOnFailure(hr) |> ignore; 
        res1,res2,res3,res4
        
    let ThrowOnFailure7(hr,res1,res2,res3,res4,res5,res6,res7) = 
        ErrorHandler.ThrowOnFailure(hr) |> ignore; 
        res1,res2,res3,res4,res5,res6,res7
        
    let Succeeded hr = 
        // REVIEW: Not the correct check for succeeded
        hr = VSConstants.S_OK
        
    let BoolToHResult = function
          true -> VSConstants.S_OK 
        | false -> VSConstants.S_FALSE;            


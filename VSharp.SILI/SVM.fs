﻿namespace VSharp

open System.Reflection

module public SVM =

    let private interpret assemblyPath qualifiedTypeName (m : MethodInfo) =
        printfn "=========== Interpreting %s.%s: ===========" qualifiedTypeName m.Name
        let state = State.empty in
        let declaringType = Types.FromDotNetType(m.DeclaringType) in
        let this, state =
            match m with
            | _ when m.IsStatic -> (Concrete(null, declaringType), state)
            | _ ->
                let instance, state = Memory.allocateSymbolicInstance "" state declaringType in
                if Terms.IsHeapRef instance then (instance, state)
                else
                    let key = "external data" in
                    let state = State.push state [(key, instance)] in
                    (Memory.referenceToVariable state key true, state)
        in
        Interpreter.decompileAndReduceMethod state this [] qualifiedTypeName m.Name assemblyPath (fun (term, state) ->
        printfn "=========== Results: ==========="
        printfn "SVM result: %s" (toString term)
        printfn "SVM environment: %s" (toString state))

    let private runType assemblyPath (t : System.Type) =
        let qualifiedTypeName = t.FullName in
        let disabledTests = ["Calculator"; (*"Conditional"; "Arithmetics"; "Fibonacci"; "Lambdas"; "GCD";*) "ClassesSimple"] in
        if List.forall (fun keyword -> not(qualifiedTypeName.Contains(keyword))) disabledTests then
            t.GetMethods() |> Array.iter (interpret assemblyPath qualifiedTypeName)

    let public Run (assembly : Assembly) =
        printfn "Running assembly %s..." assembly.FullName
        let path = JetBrains.Util.FileSystemPath.Parse(assembly.Location) in
        assembly.GetTypes() |> Array.iter (runType path)
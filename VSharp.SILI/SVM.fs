namespace VSharp.Interpreter

open VSharp
open VSharp.Core
open System.Collections.Generic
open System.Reflection
open Logger

module public SVM =

    let private init = lazy(Core.API.Configure (new Activator()) (new SymbolicInterpreter()))

    let private prepareAndInvoke (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) invoke =
        init.Force()
        let qualifiedTypeName = m.DeclaringType.AssemblyQualifiedName
        let metadataMethodOption = DecompilerServices.methodInfoToMetadataMethod assemblyPath qualifiedTypeName m
        match metadataMethodOption with
        | None ->
            printLog Warning "WARNING: metadata method for %s.%s not found!" qualifiedTypeName m.Name
        | Some metadataMethod ->
            dictionary.Add(m, null)
            invoke ({ metadataMethod = metadataMethod; state = {v = Memory.EmptyState}}) (fun summary ->
//            System.Console.WriteLine("For {0}.{1} got {2}!", m.DeclaringType.Name, m.Name, ControlFlow.ResultToTerm result)
            dictionary.[m] <- summary)

    let private interpretEntryPoint (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) =
        assert(m.IsStatic)
        prepareAndInvoke dictionary assemblyPath m InterpretEntryPoint

    // get_IsInvalid -> cast to IntPtr (should be user defined)
    // get_Name, ToString, GetAccessControl, SetAccessControl, LastIndexOf -> reduceParameterModifierExpression not implemented
    // CreateSubKey, DeleteSubKey, OpenSubKey -> extern IsAscii not implemented
    // DeleteValue, OpenBaseKey, OpenRemoteBaseKey, get_SubKeyCount, get_View, get_Handle, FromHandle, GetSubKeyNames, get_ValueCount, GetValueNames, GetValueKind -> logical operations not implemented
    // GetObjectData, GetBaseException, Flatten -> looping (symbolic list)
    // get_TargetFrameworkName, GetData, CompareTo -> too long, no unsafe
    // TryGetSwitch, SetSwitch -> reduceLockStatement not implemented
    // Equals -> extern System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.Equals(System.Object, System.Object) not implemented
    // GetHashCode -> extern System.Int32 System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(System.Object) not implemented
    // GetType -> extern System.Type System.Object.GetType(this) not implemented
    // Handle -> forik
    // Invoke, BeginInvoke, EndInvoke -> no body in decompiled method
    // Create -> sizeOf : could not create such program
    // Deconstruct -> couldNotLoadType
    // get_BaseDirectory -> extern System.Char System.String.get_Chars(this, System.Int32) not implemented
    // Reverse -> extern System.Boolean System.Array.TrySZReverse(System.Array, System.Int32, System.Int32) not implemented
    // Sort -> too long, no unsafe
    // TrueForAll -> not implemented in delegate
    // Initialize -> extern System.Void System.Array.Initialize(this) not implemented
    // Resize -> typeLoadException
    // CreateInstance -> delegate
    // Copy, ConstrainedCopy -> extern System.Void System.Array.Copy(System.Array, System.Int32, System.Array, System.Int32, System.Int32, System.Boolean) not implemented
    // GetValue, SetValue -> extern System.Void System.Array.InternalGetReference(this, System.Void*, System.Int32, System.Int32*) not implemented
    // get_LongLength -> extern System.Int64 System.Array.get_LongLength(this) not implemented

//    let private explore (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) =
//        let ingnoredMethods =
//            set["IndexOf"; "LastIndexOf"; "Reverse"; "Sort"; "TrueForAll"; "Initialize"; "Resize"; "CreateInstance"; "Copy"; "ConstrainedCopy"; "Clear"; "GetValue"; "SetValue"; "get_LongLength"; "GetLength"; "GetUpperBound"; "GetLowerBound"; "Clone"]
////            set["GetValue"; "SetValue"; "Close"; "Flush"; "Dispose"; "CreateSubKey"; "DeleteSubKey"; "DeleteSubKeyTree"; "DeleteValue"; "GetValueNames"; "GetValueKind"; "GetAccessControl"; "SetAccessControl"; "GetBaseException";
////                "OpenBaseKey"; "OpenRemoteBaseKey"; "OpenSubKey"; "get_SubKeyCount"; "get_View"; "get_Handle"; "FromHandle"; "GetSubKeyNames"; "get_ValueCount"; "get_Name"; "ToString"; "get_IsInvalid"; "GetObjectData"; "Flatten";
////                "get_TargetFrameworkName"; "GetData"; "TryGetSwitch"; "SetSwitch"; "Equals"; "GetHashCode"; "GetType"; "Invoke"; "BeginInvoke"; "EndInvoke"; "Create"; "CompareTo"; "Deconstruct"; "Handle"; "get_BaseDirectory"]
//        if ingnoredMethods.Contains m.Name then ()
//        else prepareAndInvoke dictionary assemblyPath m Explore

    let private explore (dictionary : System.Collections.IDictionary) assemblyPath (m : MethodInfo) =
        prepareAndInvoke dictionary assemblyPath m Explore

    let private exploreType ignoreList ep dictionary assemblyPath (t : System.Type) =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        let bindingFlags = BindingFlags.Instance ||| BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.DeclaredOnly
        if List.forall (fun keyword -> not(t.AssemblyQualifiedName.Contains(keyword))) ignoreList && t.IsPublic then
            t.GetMethods(bindingFlags) |> FSharp.Collections.Array.iter (fun m -> if m <> ep && not m.IsAbstract then explore dictionary assemblyPath m)

    let private replaceLambdaLines str =
        System.Text.RegularExpressions.Regex.Replace(str, @"@\d+(\+|\-)\d*\[Microsoft.FSharp.Core.Unit\]", "")

    let private resultToString (kvp : KeyValuePair<_, functionSummary>) =
        let summary = kvp.Value
        sprintf "%O\nHEAP:\n%s" summary.result (summary.state |> Memory.Dump |> replaceLambdaLines)

    let public ConfigureSolver (solver : ISolver) =
        Core.API.ConfigureSolver solver

    let public Run (assembly : Assembly) (ignoreList : List<_>) =
        let ignoreList = List.ofSeq ignoreList
        let dictionary = new Dictionary<MethodInfo, functionSummary>()
        let path = JetBrains.Util.FileSystemPath.Parse(assembly.Location)
        let ep = assembly.EntryPoint
        assembly.GetTypes() |> FSharp.Collections.Array.iter (exploreType ignoreList ep dictionary path)
        if ep <> null then interpretEntryPoint dictionary path ep
        System.Linq.Enumerable.ToDictionary(dictionary :> IEnumerable<_>, (fun kvp -> kvp.Key), resultToString) :> IDictionary<_, _>

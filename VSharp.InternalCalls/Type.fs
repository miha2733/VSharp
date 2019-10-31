namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Type --------------------------------

module Type =

    let internal GetTypeFromHandle (state : state) (args : term list) =
        assert (List.length args = 1)
        let handle = List.head args
        let t =
            match handle.term with
            |  Concrete(:? System.RuntimeTypeHandle as handle, _) -> System.Type.GetTypeFromHandle handle
            | _ -> __notImplemented__()
        let termTypeOfSystemType = Types.FromDotNetType state (t.GetType())
        Concrete t termTypeOfSystemType, state

    let internal GetType (state : state) (args : term list) =
        assert(List.length args = 1)
        let obj = List.head args
        let dotNetTyp = obj |> Terms.TypeOf |> Types.ToDotNetType
        let termTypOfTyp = Types.FromDotNetType state (dotNetTyp.GetType())
        Concrete dotNetTyp termTypOfTyp, state

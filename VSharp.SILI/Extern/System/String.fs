namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal String =

    let ctorOfCharArray (state : state) (args : term list) =
        assert (List.length args = 2)
        let this, array = List.item 0 args, List.item 1 args
        let string, state = Memory.StringCtorOfCharArray state array
        ControlFlow.ThrowOrReturn this, snd <| Memory.Mutate state this string

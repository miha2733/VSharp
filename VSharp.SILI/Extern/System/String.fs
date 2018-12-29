namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal String =

    let ctorOfCharArray state args =
        assert (List.length args = 2)
        let this, array = List.item 0 args, List.item 1 args
        let string, state = Memory.StringCtorOfCharArray state array
        ControlFlow.ThrowOrReturn this, snd <| Memory.Mutate state this string

    let getLength state args =
        assert(List.length args = 1)
        mapfst ControlFlow.ThrowOrReturn <| Memory.StringLength state (List.head args)

    let useRandowHash (state : state) (args : term list) =
        Return True, state

    let InternalMarvin32HashString state args =
        assert(List.length args = 3)
        mapfst ControlFlow.ThrowOrReturn <| Memory.StringHashCode state (List.head args)

    let IsStringInterned state args =
        assert(List.length args = 2)
        mapfst ControlFlow.ThrowOrReturn <| Memory.IsInternedString state (List.item 1 args)

    let InternString state args =
        assert(List.length args = 2)
        mapfst ControlFlow.ThrowOrReturn <| Memory.InternString state (List.item 1 args)

    let InternalIsInterned state args =
        assert(List.length args = 1)
        mapfst ControlFlow.ThrowOrReturn <| Memory.IsInternedString state (List.head args)

    let InternalIntern state args =
        assert(List.length args = 1)
        mapfst ControlFlow.ThrowOrReturn <| Memory.InternString state (List.head args)

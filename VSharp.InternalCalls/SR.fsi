namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal SR =

    [<Implements("System.String System.SR.get_Arg_OverflowException()")>]
    val internal get_Arg_OverflowException : state -> term list -> term * state

    [<Implements("System.String System.SR.get_Arg_IndexOutOfRangeException()")>]
    val internal get_Arg_IndexOutOfRangeException : state -> term list -> term * state

    [<Implements("System.String System.SR.get_Arg_NullReferenceException()")>]
    val internal get_Arg_NullReferenceException : state -> term list -> term * state

    [<Implements("System.String System.SR.get_Arg_ArrayTypeMismatchException()")>]
    val internal get_Arg_ArrayTypeMismatchException : state -> term list -> term * state
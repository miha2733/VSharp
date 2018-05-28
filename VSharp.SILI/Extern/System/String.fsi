namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter

// ------------------------------- mscorlib.System.String -------------------------------

module internal String =

    [<Implements("System.Void System.String..ctor(this, System.Char[])")>]
    val ctorOfCharArray : state -> term list -> statementResult * state

    [<Implements("System.Int32 System.String.get_Length(this)")>]
    val getLength : state -> term list -> statementResult * state

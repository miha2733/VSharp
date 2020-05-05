namespace VSharp

open VSharp.Core

type public TypeSolver() =
    interface ISolver with
        override x.SolvePathCondition _ = Unknown

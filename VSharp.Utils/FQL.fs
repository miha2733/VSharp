namespace VSharp

open FSharpx.Collections

type FQL = Deque // TODO: deque может не подойти, так как (возможно) необходимо мутировать элементы (например, для подмены fql при мутации локации)

module FQL =

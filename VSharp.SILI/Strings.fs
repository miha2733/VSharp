namespace VSharp

open JetBrains.Decompiler.Ast
open Types.Constructor
open VSharp.Arrays
open Terms
open Memory
open State

module internal Strings =

    let makeString metadata (str : string) =
        let fields =
            let time = tick() in
            let length = String.length str
            let stringTermLength = Concrete length lengthTermType metadata in
            let arraySource = (str + "\000").ToCharArray()
            let valMaker i = MakeNumber arraySource.[i] metadata
            let keyMaker i mtd = makeIntegerArray metadata (fun _ -> MakeNumber i mtd) 1
            let array = makeLinearConreteArray metadata keyMaker valMaker (length + 1) (Numeric typedefof<char>)
            Heap.ofSeq (seq [ MakeStringKey "System.String.m_StringLength", (stringTermLength, time, time);
                              MakeStringKey "System.String.m_FirstChar", (array, time, time) ])
        in
        Struct fields VSharp.String metadata

    let (|ConcreteStringArray|_|) = function
        | Array({term = Concrete(one, _)}, {term = Concrete (length, _)}, lower, [_, DefaultInstantiator termType], contents, _, ArrayType (typex, ConcreteDimension 1))
            when
                let zero = MakeZeroAddress Metadata.empty in
                one :?> int = 1 && fst3 lower.[zero] = zero && termType = Numeric typedefof<char> && typex = Numeric typedefof<char> ->
              Some(ConcreteStringArray(unbox length, contents))
        | _ -> None

    let getKeyOfString metadata state concreteCase symbolicCase term =

        let contentIsConcrete xs = //TODO: FIXME
            Cps.Seq.foldlk (fun a (key, (value, _, _)) k ->
                match key.term, value.term with
                | Array({term = Concrete(one, _)}, {term = Concrete (length, _)}, _, [_, DefaultInstantiator _], contents, _, ArrayType (_, ConcreteDimension 1)), Concrete(y, _)
                    when length :?> int = 1 && one :?> int = 1 ->
                        match Terms.term (fst3 <| contents.[MakeZeroAddress Metadata.empty]) with
                        | Concrete(num, _) -> k <| Set.add (unbox num, unbox y) a
                        | _ -> None
                | _ -> None)
                Set.empty
                xs
                (fun x -> Some x)

        let fullArray length value xs : seq<int * char> =
            let indices : Set<int> = Set.map fst xs |> Set.difference (Set.ofSeq [0 .. length - 1])
            Seq.append <| Set.toSeq xs <| Seq.map (fun k -> k, value) indices

        deref metadata state term
        ||> Common.unionHandler
            (fun state string ->
            match string.term with
            | Struct(fields, String) ->
                match fields.[MakeStringKey "System.String.m_FirstChar"] |> fst3 |> Terms.term with
                //Almost concrete string
                | ConcreteStringArray(length, contents) ->
                    Heap.toSeq contents
                    // TODO: discard null-terminator here
                    |> contentIsConcrete
                    |> function
                        //Concrete string
                        | Some set ->
                            let discardLast = Seq.rev >> Seq.tail >> Seq.rev
                            let time = tick() in
                            fullArray length (unbox <| System.Activator.CreateInstance(typedefof<char>)) set
                            |> Seq.sortBy (fun (k, v) -> k)
                            |> discardLast //TODO: test hash
                            |> Seq.map snd
                            |> Seq.toArray
                            |> System.String //System.String.Concat
                            |> fun string -> concreteCase string metadata, state
                        //Partially-symbolic string
                        //Can contain symbolic values
                        | None -> symbolicCase string, state
                //Full-symbolic string
                //Can be made by "makeSymbolicInstance" function
                | Array(_, _, _, _, _, _, _) -> symbolicCase string, state
                | _ -> internalfail "wrong string!"
            | _ -> internalfail "not a string!")

    let getHashCodeOfLitExp metadata (ast : ILiteralExpression) = MakeNumber (ast.Value.Value.ToString().GetHashCode()) metadata

    let getHashCode metadata state term =
        let pointer = refToInt term in
        getKeyOfString metadata state (fun string -> MakeNumber <| string.GetHashCode()) (always pointer) term

    let inline inInternPool (s : state) string = Heap.contains string s.iPool

    let internal allocateInInternPool metadata (state : state) term =
        let time = tick() in
        let key, state = getKeyOfString metadata state MakeConcreteString id term in
        { state with iPool = state.iPool.Add(key, (term, time, time)) }

    let internal internLiterals state ast k =

        let rec internLiteralsHelper state acc (ast : INode) k =

            let makeAndInternString mtd state stringLiteral =
                let time = tick() in
                let stringStruct = makeString mtd stringLiteral in
                let string, s = allocateInHeap mtd state stringStruct in
                { s with iPool = s.iPool.Add (stringStruct, (string, time, time)) }

            if ast = null
            then k (state, List.empty)
            else
                let mtd = State.mkMetadata ast state in
                let empty : list<string> = []
                let interned = DecompilerServices.getPropertyOfNode<string list> ast "InternedLiterals" empty
                if not <| List.isEmpty interned
                then k (List.fold (fun state x -> if inInternPool state <| MakeStringKey x then state else makeAndInternString mtd state x) state interned, List.append acc interned)
                else
                    let newState, acc =
                        match ast with
                        | :? ILiteralExpression as ast
                            when FromConcreteMetadataType ast.Value.Type = String && not <| inInternPool state (MakeStringKey <| ast.Value.Value.ToString()) ->
                                let stringLiteral = ast.Value.Value.ToString()
                                makeAndInternString mtd state stringLiteral, stringLiteral :: acc
                        | _ -> state, acc
                    in
                    Cps.Seq.foldlk (fun (state, acc) x k -> internLiteralsHelper state acc x k) (newState, List.empty) ast.Children (fun (state, interned) -> //goes here
                    k (state, List.append acc interned))

        internLiteralsHelper state List.empty ast (fun (state, interned) ->
        if ast <> null
        then do DecompilerServices.setPropertyOfNode ast "InternedLiterals" interned //this
        k state)

    let ctorOfCharArray mtd state this term =
        let array, state = deref mtd state term
        match array.term with
        | Array({term = Concrete(one, _)}, length, lower, instantiator, contents, lengths, ArrayType (typex, ConcreteDimension 1))
            when one :?> int = 1 && typex = Numeric typedefof<char> ->
                let time = tick() in
                let one = MakeNumber 1 mtd in
                let arrLength = add mtd length one in
                let indexLength = makeIntegerArray mtd (always <| length) 1 in
                let contentsWithZero = Heap.add indexLength (MakeNumber '\000' mtd, time, time) contents in
                let stringArray = makeArray mtd arrLength contentsWithZero instantiator typex
                let fields =
                    let time = tick() in
                    Heap.ofSeq (seq [ MakeStringKey "System.String.m_StringLength", (length, time, time);
                                      MakeStringKey "System.String.m_FirstChar", (stringArray, time, time) ])
                in
                Struct fields VSharp.String mtd |> mutate mtd state this //this,... // |> snd
        | _ -> internalfail "invalid char array!"

    let internal Intern metadata (state : state) term =
        let lambda state string =
            let key, state = getKeyOfString metadata state MakeConcreteString id string in
            let state =
                if inInternPool state key
                then state
                else allocateInInternPool metadata state string
            in
            let refToString = readPoolLocation state key in
            (refToString, state)
        in
        Common.unionHandler lambda term state

    let internal IsInterned metadata state term = __notImplemented__()
//        let lambda state string =
//            let pointer = refToInt string in
//            let key, state = getKeyOfString metadata state MakeConcreteString id string in
//            let refToString =
//                let lambda state (poolKey, (stringRef, _, _)) k =
//                    Common.statedConditionalExecution state
//                        (fun state k -> k (simplifyEquality metadata key poolKey, state))
//                        (fun state k -> k (stringRef, state))
//                        (fun state k -> k (MakeNullRef String metadata <| Memory.tick(), state))
//                        merge
//                        merge2
//                        id
//                        (fun (result, state) -> k (ControlFlow.resultToTerm result, state))
//                in
//                Cps.Seq.foldlk () state (toSeq state.iPool) id
////                if inInternPool state key
////                then readPoolLocation state key
////                else MakeNullRef String metadata <| Memory.tick()
//            in
//            (refToString, state)
//        in
//        Common.unionHandler lambda term state

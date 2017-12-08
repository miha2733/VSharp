namespace VSharp

open JetBrains.Decompiler.Ast
open Types
open Constructor
open MemoryCell
open Memory
open Arrays
open State

module internal Strings =

    let makeString metadata (str : string) =
        let fields =
            let time = tick() in
            let length = String.length str in
            let stringTermLength = Concrete length lengthTermType metadata in
            let arraySource = (str + "\000").ToCharArray() in
            let valMaker i = MakeNumber arraySource.[i] metadata in
            let keyMaker i mtd = makeIntegerArray metadata (fun _ -> MakeNumber i mtd) 1 in
            let array = makeLinearConreteArray metadata keyMaker valMaker (length + 1) (Numeric typedefof<char>) in
            Heap.ofSeq (seq [ MakeStringKey "System.String.m_StringLength", { value = stringTermLength; created = time; modified = time };
                              MakeStringKey "System.String.m_FirstChar", { value = array; created = time; modified = time } ])
        in
        Struct fields stringType metadata

    let internal getKeyOfString mtd state term k =
        deref mtd state term
        ||> Common.unionHandler k

    let (|ConcreteStringArray|_|) = function
        | Array({term = Concrete(one, _)}, {term = Concrete (length, _)}, lower, [_, DefaultInstantiator(_, termType)], contents, _, ArrayType (typex, ConcreteDimension 1))
            when
                let zero = MakeZeroAddress Metadata.empty in
                one :?> int = 1 && lower.[zero].value = zero && termType = Numeric typedefof<char> && typex = Numeric typedefof<char> ->
              Some(ConcreteStringArray(unbox length, contents))
        | _ -> None

    let internal getHashCode metadata state term =

        let contentIsConcrete xs =
            Cps.Seq.foldlk (fun a (key, cell) k ->
                match key.term, cell.value.term with
                | Array({term = Concrete(one, _)}, {term = Concrete (length, _)}, _, [_, DefaultInstantiator _], contents, _, ArrayType (_, ConcreteDimension 1)), Concrete(y, _)
                    when length :?> int = 1 && one :?> int = 1 ->
                        match Terms.term contents.[MakeZeroAddress Metadata.empty].value with
                        | Concrete(num, _) -> k <| Set.add (unbox num, unbox y) a
                        | _ -> None
                | _ -> None)
                Set.empty
                xs
                (fun x -> Some x)

        let fullArray length value xs : seq<int * char> =
            let indices : Set<int> = Set.map fst xs |> Set.difference (Set.ofSeq [0 .. length - 1])
            Seq.append <| Set.toSeq xs <| Seq.map (fun k -> k, value) indices

        getKeyOfString metadata state term (fun state ->
            Terms.term >> function
            | Struct(fields, StringType) ->
                match Terms.term fields.[MakeStringKey "System.String.m_FirstChar"].value with
                //Almost concrete string
                | ConcreteStringArray(length, contents) ->
                    Heap.toSeq contents
                    |> contentIsConcrete
                    |> function
                        //Concrete string
                        | Some set ->
                            let discardLast = Seq.rev >> Seq.tail >> Seq.rev in
                            let time = tick() in
                            fullArray length (unbox <| System.Activator.CreateInstance(typedefof<char>)) set
                            |> Seq.sortBy (fun (k, v) -> k)
                            |> discardLast
                            |> Seq.map snd
                            |> Seq.toArray
                            |> System.String
                            |> fun string -> MakeNumber (string.GetHashCode()) metadata, state
                        //Partially-symbolic string
                        //Can contain symbolic values
                        | None -> refToInt term, state
                //Full-symbolic string
                //Can be made by "makeSymbolicInstance" function
                | Array(_, _, _, _, _, _, _) -> refToInt term, state
                | _ -> internalfail "wrong string!"
            | _ -> internalfail "not a string!")

    let internal allocateInInternPool metadata state term =
        let time = tick() in
        snd <| getKeyOfString metadata state term (fun state key ->
        key, { state with iPool = state.iPool.Add(key, { value = term; created = time; modified = time }) })

    let inline private inInternPool (s : state) string = Heap.contains string s.iPool

    let internal internLiterals state ast k =

        let rec internLiteralsHelper state acc (ast : INode) k =

            let makeAndInternString mtd state stringLiteral =
                let time = tick() in
                let stringStruct = makeString mtd stringLiteral in
                let string, s = allocateInHeap mtd state stringStruct in
                { s with iPool = s.iPool.Add (stringStruct, { value = string; created = time; modified = time }) }

            if ast = null
            then k (state, List.empty)
            else
                let mtd = State.mkMetadata ast state in
                let empty : list<string> = [] in
                let interned = DecompilerServices.getPropertyOfNode<string list> ast "InternedLiterals" empty
                if not <| List.isEmpty interned
                then k (List.fold (fun state x -> if inInternPool state <| makeString mtd x then state else makeAndInternString mtd state x) state interned, List.append acc interned)
                else
                    let newState, acc =
                        match ast with
                        | :? ILiteralExpression as ast
                            when FromConcreteMetadataType ast.Value.Type = stringType && ast.Value.Value.ToString() |> makeString mtd |> inInternPool state |> not ->
                                let stringLiteral = ast.Value.Value.ToString() in
                                makeAndInternString mtd state stringLiteral, stringLiteral :: acc
                        | _ -> state, acc
                    in
                    Cps.Seq.foldlk (fun (state, acc) x k -> internLiteralsHelper state acc x k) (newState, List.empty) ast.Children (fun (state, interned) ->
                    k (state, List.append acc interned))

        internLiteralsHelper state List.empty ast (fun (state, interned) ->
        if ast <> null
        then do DecompilerServices.setPropertyOfNode ast "InternedLiterals" interned
        k state)

    let private internCommon heapAction value mtd state term =
        let lambda state string =
            getKeyOfString mtd state string (fun state key ->
            let time = tick() in
            let cell = { value = value; created = time; modified = time } in
            let lazyInstatiator = fun () -> cell, heapAction key cell in
            let gvas, iPool = heapDeref mtd time lazyInstatiator state.iPool key (Reference stringType) System.UInt32.MaxValue in
            let gvs = List.map (fun (g, _, v) -> (g, v.value)) gvas in
            Merging.merge gvs, withPool state iPool)
        in
        Common.unionHandler lambda term state

    let internal Intern mtd state term = internCommon (fun key cell -> state.iPool.Add(key, cell)) term mtd state term

    let internal IsInterned mtd state term = internCommon (fun _ _ -> state.iPool) (MakeNullRef stringType mtd) mtd state term

    let internal ctorOfCharArray mtd state this term =
        deref mtd state term
        ||> Common.unionHandler (fun state ->
            Terms.term >> function
            | Array({term = Concrete(one, _)}, length, lower, instantiator, contents, lengths, ArrayType (typex, _))
                when one :?> int = 1 && typex = Numeric typedefof<char> ->
                    let time = tick() in
                    let one = MakeNumber 1 mtd in
                    let arrLength = add mtd length one in
                    let indexLength = makeIntegerArray mtd (always <| length) 1 in
                    let contentsWithZero = Heap.add indexLength { value = MakeNumber '\000' mtd; created = time; modified = time } contents in
                    let stringArray = makeArray mtd arrLength contentsWithZero instantiator typex
                    let fields =
                        let time = tick() in
                        Heap.ofSeq (seq [ MakeStringKey "System.String.m_StringLength", { value = length; created = time; modified = time };
                                          MakeStringKey "System.String.m_FirstChar", { value = stringArray; created = time; modified = time } ])
                    in
                    this, Struct fields stringType mtd |> mutate mtd state this |> snd
            | _ -> internalfail "invalid char array!")

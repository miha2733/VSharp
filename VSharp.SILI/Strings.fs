namespace VSharp

open JetBrains.Decompiler.Ast
open Types.Constructor
open VSharp.Array
open State

module internal Strings =

    let makeString metadata length (str : obj) = //TODO: - length
        let fields =
            let time = Memory.tick() in
            let content = Heap.ofSeq <| Seq.mapi (fun i x ->
                (MakeNumber i metadata, (MakeNumber x metadata, time, time))) (Seq.append (str :?> seq<char>) ['0']) in
            let arrayTermLength = Concrete (length + 1) lengthTermType metadata in
            let stringTermLength = Concrete length lengthTermType metadata in
            let string = Array [|MakeNumber 0 metadata|] None content [|arrayTermLength|] (Numeric typeof<char>) metadata in
            let time = Memory.tick() in
            Heap.ofSeq (seq [ MakeStringKey "System.String.m_StringLength", (stringTermLength, time, time);
                              MakeStringKey "System.String.m_FirstChar", (string, time, time) ])
        in
        Struct fields VSharp.String metadata

    let getHashCodeOfLitExp metadata (ast : ILiteralExpression) = MakeNumber (ast.Value.Value.ToString().GetHashCode()) metadata

    let fullArray lowerBound length value xs : seq<int * char>=
        let indices : Set<int> = Set.map fst xs |> Set.difference (Set.ofSeq [lowerBound .. lowerBound + length - 1])
        Seq.append <| Set.toSeq xs <| Seq.map (fun k -> k, value) indices

    let inline contentIsConcrete xs =
        Cps.Seq.foldlk (fun a (key, (value, _, _)) k ->
            match key.term, value.term with
            | Concrete(x, _), Concrete(y, _) -> k <| Set.add (unbox x, unbox y) a
            | _ -> None)
            Set.empty
            xs
            (fun x -> Some x)

    let getHashCode state term metadata =
        let pointer = Operators.refToInt term in
        let string = Terms.term <| State.readHeapLocation state pointer in
        match string with
        | Struct(fields, String) ->
            match Terms.term <| fst3 fields.[MakeStringKey "System.String.m_FirstChar"] with
            //Concrete string
            | Array([|{term = Concrete (lowerBound, _)}|], None, contents, [|{term = Concrete (length, _)}|], elType) ->
                Heap.toSeq contents
                // TODO: discard null-terminator here
                |> contentIsConcrete
                |> (function
                    //Concrete string
                    | Some set ->
                        let discardLast = Seq.rev >> Seq.tail >> Seq.rev
                        let realType = Types.ToDotNetType elType in
                        let time = Memory.tick() in
                        fullArray (unbox lowerBound) (unbox length) (unbox <| System.Activator.CreateInstance(realType)) set
                        |> Seq.sortBy (fun (k, v) -> k)
                        |> discardLast //TODO: test hash
                        |> Seq.map snd
                        |> System.String.Concat //TODO: to array ar smth
                        |> fun x -> MakeNumber (x.GetHashCode()) metadata
                    //Partially-symbolic string
                    | None -> MakeNumber pointer metadata)
            //Full-symbolic string
            | Array(_, _, _, _, _) -> MakeNumber pointer metadata
            | _ -> internalfail "wrong string"
        | _ -> internalfail "not a string"

    let inline inInternPool ((s, h, m, i, f, p) : state) hash = Heap.contains hash i

    let internal allocateInInternPool metadata ((s, h, m, i, f, p) : state) term =
        let time = Memory.tick() in
        let key = getHashCode (s, h, m, i, f, p) term metadata in
        (s, h, m, i.Add(key, (term, time, time)), f, p)

    let rec internal internLiterals state (ast : INode) k =
        if ast <> null// && not (unbox <| DecompilerServices.getPropertyOfNode ast "IsInterned" false)
        then
            let newState =
                let mtd = State.mkMetadata ast state in
                match ast with
                | :? ILiteralExpression as ast
                    when FromConcreteMetadataType ast.Value.Type = String && not (inInternPool state <| getHashCodeOfLitExp mtd ast) ->
                        let hash = getHashCodeOfLitExp mtd ast in
                        let obj = ast.Value.Value in //TODO: breakpoint here/ where was interned Arg_Divide_By_Zero
//                        if obj = box "Arg_DivideByZero" then
//                            internalfail "breakpoint"
                        let stringLength = String.length <| obj.ToString() in
                        let string, (s, h, m, i, f, p) = makeString mtd stringLength obj |> Memory.allocateInHeap mtd state in
                        let time = Memory.tick() in
                        //do DecompilerServices.setPropertyOfNode ast "IsInterned" true
                        (s, h, m, i.Add(hash, (string, time, time)), f, p)
                | _ -> state
            in
            Cps.Seq.foldlk internLiterals newState ast.Children k
        else k state

    let internal Intern metadata (state : state) =
        let lambda state term =
            let hash = getHashCode state term metadata in
            let state =
                if inInternPool state hash
                then state
                else allocateInInternPool metadata state term
            in
            let refToString = readPoolLocation state hash in
            (refToString, state)
        in
        function
        | UnionT gvs -> Merging.guardedStateMap lambda gvs state
        | term -> lambda state term

    let internal IsInterned metadata state =
        let lambda term =
            let hash = getHashCode state term metadata in
            let refToString =
                if inInternPool state hash
                then readPoolLocation state hash
                else MakeNull String metadata (Memory.tick())
            in
            refToString
        in
        function
        | UnionT gvs -> Merging.guardedMap lambda gvs
        | term -> lambda term

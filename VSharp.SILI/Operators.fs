namespace VSharp

open JetBrains.Decompiler.Ast

[<AutoOpen>]
module internal Operators =


// --------------------------------Strings--------------------------------


//    let internal simplifyConc mtd state t x y k =
//        let string1 =
//        string1

    let rec internal simplifyConcatenation mtd state x y k = __notImplemented__()
//        Common.simplifyGenericBinary "concatenation" state x y k
//                              (__unreachable__())
//                              (fun x y state k -> simplifyConc mtd isChecked state t x y k defaultCase)
//                              (fun x y state k -> simplifyConcatenation mtd state x y k)

    let internal simplifyStringOperation mtd state op x y k =
        match op with
        | OperationType.Add -> simplifyConcatenation mtd state x y k
        | OperationType.Equal
        | OperationType.NotEqual
        | _ -> __notImplemented__()

    let internal isStringOperation op t1 t2 =
        (t1 = PointerType String || Types.IsBottom t1) && (t2 = PointerType String || Types.IsBottom t2) &&
        match op with
        | OperationType.Add
        | OperationType.Equal
        | OperationType.NotEqual -> true
        | _ -> false

//------------------------------------------------------------------------

    let rec internal refToInt term =
        match term.term with
        | Error _ -> term
        | Concrete(null, _) -> Concrete 0 Types.pointerType term.metadata
        | HeapRef(((addr, _), _), _) -> addr
        | Union gvs -> Merging.guardedMap refToInt gvs
        | _ -> term

    let rec internal referenceEqual mtd p1 p2 =
        let addr1 = refToInt p1 in
        let addr2 = refToInt p2 in
        if not(Terms.IsInteger addr1 || Terms.IsInteger addr2) then
            internalfail "reference comparing non-reference types"
        Arithmetics.simplifyEqual mtd addr1 addr2 id

    let simplifyBinaryOperation mtd op isChecked state t left right k =
        let t1 = Terms.TypeOf left in
        let t2 = Terms.TypeOf right in
        match op with
        | op when Propositional.isLogicalOperation op t1 t2 ->
            Propositional.simplifyBinaryConnective mtd op left right (withSnd state >> k)
        | op when Arithmetics.isArithmeticalOperation op t1 t2 ->
            Arithmetics.simplifyBinaryOperation mtd op state left right isChecked t k
        | op when isStringOperation op t1 t2 ->
            simplifyStringOperation mtd state op left right k
        | op when Pointers.isPointerOperation op t1 t2 ->
            Pointers.simplifyBinaryOperation mtd op state left right k
        | _ -> __notImplemented__()

    let ksimplifyEquality mtd x y k =
        simplifyBinaryOperation mtd JetBrains.Decompiler.Ast.OperationType.Equal false State.empty typeof<bool> x y (fst >> k)

    let simplifyEquality mtd x y =
        ksimplifyEquality mtd x y id

    let (===) x y = ksimplifyEquality Metadata.empty x y id
    let (!==) x y = ksimplifyEquality Metadata.empty x y (!!)

    let simplifyUnaryOperation mtd op isChecked state t arg k =
        match t with
        | Bool -> Propositional.simplifyUnaryConnective mtd op arg (withSnd state >> k)
        | Numeric t -> Arithmetics.simplifyUnaryOperation mtd op state arg isChecked t k
        | String -> __notImplemented__()
        | _ -> __notImplemented__()

    let simplifyArraysEquality mtd state x y =

        let inline eqTypes mtd type1 type2 = simplifyAnd mtd (Common.is mtd type1 type2) (Common.is mtd type2 type1) id

        let simplifyGInstantiatorEquality mtd gInstor1 gInstor2 =

            let instorEq mtd x y =
                match x, y with
                | DefaultInstantiator typ1, DefaultInstantiator typ2 -> eqTypes mtd typ1 typ2
                | LazyInstantiator(term1, typ1), LazyInstantiator(term2, typ2) -> simplifyAnd mtd (eqTypes mtd typ1 typ2) (simplifyEquality mtd term1 term2) id
                | _ -> __notImplemented__()

            List.fold (fun (acc : Term) (g1, instor1) -> 
                simplifyOr mtd acc (List.fold (fun acc (g2, instor2) -> 
                    simplifyAnd mtd acc (implies (simplifyAnd mtd g1 g2 id) (instorEq mtd instor1 instor2) mtd) id) (Terms.MakeTrue mtd) gInstor2) id)
                (Terms.MakeTrue mtd)
                gInstor1

        let inline heapDotsEquality h1 h2 = 
            fst <| Heap.cmpHeaps h1 h2 (List.fold (fun acc (k, v) -> simplifyAnd mtd acc (h1.[k] |> fst3 |> simplifyEquality mtd v) id) <| Terms.MakeTrue mtd)

        match x.term, y.term with
        | Array(dim1, len1, lb1, instor1, content1, l1, t1), Array(dim2, len2, lb2, instor2, content2, l2, t2) ->
            Propositional.conjunction mtd <|
                seq[
                    simplifyEquality mtd dim1 dim2;
                    simplifyEquality mtd len1 len2;
                    heapDotsEquality lb1 lb2;
                    simplifyGInstantiatorEquality mtd instor1 instor2;
                    heapDotsEquality content1 content2;
                    heapDotsEquality l1 l2;
                    eqTypes mtd t1 t2
                ]
        | _ -> internalfail "not array!"

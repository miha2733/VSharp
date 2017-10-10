namespace VSharp

open JetBrains.Decompiler.Ast

[<AutoOpen>]
module internal Operators =


// --------------------------------Strings--------------------------------

    let internal simplifyConcatenation mtd x y = //TODO: important?
        match x.term, y.term with
        | Concrete(xval, _), Concrete(yval, _) ->
            let mtd' = Metadata.combine3 mtd x.metadata y.metadata in
            MakeConcreteString (VSharp.CSharpUtils.Calculator.Add(xval, yval, typedefof<string>) :?> string) mtd'
        | _ -> Terms.MakeBinary OperationType.Add x y false String mtd

    let internal simplifyStringOperation mtd op x y =
        match op with
        | OperationType.Add -> simplifyConcatenation mtd x y
        | OperationType.Equal
        | OperationType.NotEqual
        | _ -> __notImplemented__()

    let internal isStringOperation op t1 t2 =
        Types.IsString t1 && Types.IsString t2 &&
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
            simplifyStringOperation mtd op left right |> (withSnd state >> k)
        | op when Pointers.isPointerOperation op t1 t2 ->
            Pointers.simplifyBinaryOperation mtd op state left right k
        | _ -> __notImplemented__()

//    let performBinaryOperation (caller : LocationBinding) state op left right isChecked t k =
//        let t1 = Terms.TypeOf left in
//        let t2 = Terms.TypeOf right in
//        let mtd = State.mkMetadata caller state in
//        match op with
//        | op when Propositional.isLogicalOperation op t1 t2 ->
//            Propositional.simplifyBinaryConnective mtd op left right (withSnd state >> k)
//        | op when Arithmetics.isArithmeticalOperation op t1 t2 ->
//            Arithmetics.simplifyBinaryOperation mtd op state left right isChecked t k
//        | op when isStringOperation op t1 t2 ->
//            simplifyStringOperation mtd op left right |> (withSnd state >> k)
//        | _ ->
//            match op with
//            | OperationType.Equal -> referenceEqual mtd left right |> (withSnd state >> k)
//            | OperationType.NotEqual ->
//                let equal = referenceEqual mtd left right in
//                Propositional.simplifyNegation mtd equal (withSnd state >> k)
//            | _ -> __notImplemented__()

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

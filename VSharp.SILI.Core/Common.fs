namespace VSharp.Core

#nowarn "69"

open VSharp

//type SolverResult = Sat | Unsat | Unknown
//type ISolver =
//    abstract Solve : term -> SolverResult
//    abstract SolvePathCondition : term -> term list -> SolverResult

module internal Common =
//    let mutable private solver : ISolver option = None
//    let configureSolver s = solver <- Some s
//    let private solve term =
//        match solver with
//        | Some s -> s.Solve term
//        | None -> Unknown
//    let private solvePC term pc =
//        match solver with
//        | Some s -> s.SolvePathCondition term pc
//        | None -> Unknown

// ------------------------------- Simplification -------------------------------

    let simplifyPairwiseCombinations = Propositional.simplifyPairwiseCombinations

    let simplifyConcreteBinary simplify t _ _ xval yval _ _ =
        simplify t xval yval

    let rec simplifyGenericUnary name x matched concrete unmatched =
        match x.term with
        | Concrete(xval, typeofX) -> concrete x xval typeofX |> matched
        | GuardedValues(guards, values) ->
            Cps.List.mapk (fun term matched -> simplifyGenericUnary name term matched concrete unmatched) values (fun values' ->
                Merging.merge (List.zip guards values') |> matched)
        | _ -> unmatched x matched

    let rec simplifyGenericBinary _ x y matched concrete unmatched repeat =
        match x.term, y.term with
        | Concrete(xval, typeOfX), Concrete(yval, typeOfY) -> concrete x y xval yval typeOfX typeOfY |> matched
        | Union(gvsx), Union(gvsy) ->
            let compose (gx, vx) (gy, vy) matched = repeat vx vy (fun xy -> (gx &&& gy, xy) |> matched)
            let join (gx, vx) k = Cps.List.mapk (compose (gx, vx)) gvsy k
            Cps.List.mapk join gvsx (fun gvss -> Merging.merge (List.concat gvss) |> matched)
        | GuardedValues(guardsX, valuesX), _ ->
            Cps.List.mapk (fun x matched -> repeat x y matched) valuesX (fun values' ->
            Merging.merge (List.zip guardsX values') |> matched)
        | _, GuardedValues(guardsY, valuesY) ->
            Cps.List.mapk (fun y matched -> repeat x y matched) valuesY (fun values' ->
            Merging.merge (List.zip guardsY values') |> matched)
        | _ -> unmatched x y matched

// ---------------------------------------- Branching ---------------------------------------

    let commonStatelessConditionalExecutionPCk pc conditionInvocation thenBranch elseBranch merge merge2 k =
        let execution condition k =
            thenBranch (fun thenResult ->
            elseBranch (fun elseResult ->
            k <| merge2 condition !!condition thenResult elseResult))
        let chooseBranch condition k =
            let thenCondition = condition::pc |> conjunction
            let elseCondition = (!!condition)::pc |> conjunction
            match thenCondition, elseCondition with
            | False, _ -> elseBranch k
            | _, False -> thenBranch k
            | _ -> execution condition k
        conditionInvocation (fun condition ->
        Merging.commonGuardedApplyk chooseBranch condition merge k)

    let commonStatelessConditionalExecutionk conditionInvocation thenBranch elseBranch merge merge2 k =
        commonStatelessConditionalExecutionPCk [] conditionInvocation thenBranch elseBranch merge merge2 k

    let statelessConditionalExecutionWithMergePCk pc conditionInvocation thenBranch elseBranch k = commonStatelessConditionalExecutionPCk pc conditionInvocation thenBranch elseBranch Merging.merge Merging.merge2Terms k
    let statelessConditionalExecutionWithMergek conditionInvocation thenBranch elseBranch k = statelessConditionalExecutionWithMergePCk [] conditionInvocation thenBranch elseBranch k
    let statelessConditionalExecutionWithMerge conditionInvocation thenBranch elseBranch = statelessConditionalExecutionWithMergek conditionInvocation thenBranch elseBranch id

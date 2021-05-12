open Ppxlib

let rec case_to_cases ~ctxt case =
  match case with
  | { pc_lhs; pc_guard = Some guard; pc_rhs } ->
      (* collect or-patterns into list *)
      (* TODO fold over all constructors to support or-patterns in other patterns *)
      let rec f a p =
        match p.ppat_desc with Ppat_or (p1, p2) -> f (f a p2) p1 | _ -> p :: a
      in
      let ps = f [] pc_lhs in
      List.map (fun p -> Ast_helper.Exp.case p ~guard pc_rhs) ps
  | x -> [ { x with pc_rhs = expand ~ctxt x.pc_rhs } ]

and function_expand ~ctxt cases =
  let new_cases = cases |> List.map (case_to_cases ~ctxt) |> List.flatten in
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_helper.Exp.function_ ~loc new_cases

and match_expand ~ctxt expr cases =
  let new_cases = cases |> List.map (case_to_cases ~ctxt) |> List.flatten in
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  Ast_helper.Exp.match_ ~loc expr new_cases

and expand ~ctxt e =
  match e.pexp_desc with
  | Pexp_function cases -> function_expand ~ctxt cases
  | Pexp_match (e, cases) -> match_expand ~ctxt e cases
  | _ -> e

let extension =
  Extension.V3.declare "distr" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand

let rule = Ppxlib.Context_free.Rule.extension extension

let () = Driver.register_transformation ~rules:[ rule ] "distr_guards"

open Ppxlib
open Ppx_precisely_common

(** Detects arity using a function apply map *)
let rec detect_arity exp arity args =
  match exp with
    | [%expr fun [%p? a] -> [%e? rest]] -> detect_arity rest (arity + 1) (a::args)
    | _ ->
      (* checks for 0-arity by looking for the unit Lident *)
      match args with
        | [[%pat? ()]] -> (0, args, exp)
        | _ -> (arity, args, exp)

let rec buildFun ~loc args rest =
  match args with
    | [] -> bs ~loc rest
    | arg::args -> 
      let f = [%expr fun [%p arg] -> [%e rest]] in
      buildFun ~loc args f

let expandWorkletDef ~loc pat exp =
  let (arity, args, rest) = detect_arity exp 1 [pat] in
  let f = buildFun ~loc args rest in
  match arity with 
    | 0 -> [%expr ReanimatedWorklet.worklet0 ([%e f])]
    | 1 -> [%expr ReanimatedWorklet.worklet1 ([%e f])]
    | 2 -> [%expr ReanimatedWorklet.worklet2 ([%e f])]
    | 3 -> [%expr ReanimatedWorklet.worklet3 ([%e f])]
    | 4 -> [%expr ReanimatedWorklet.worklet4 ([%e f])]
    | 5 -> [%expr ReanimatedWorklet.worklet5 ([%e f])]
    | 6 -> [%expr ReanimatedWorklet.worklet6 ([%e f])]
    | 7 -> [%expr ReanimatedWorklet.worklet7 ([%e f])]
    | _ -> Location.raise_errorf ~loc "Unknown arity %d" arity 

(** Defines a builder for the WorkletExtension. It always takes 2 patterns and produce
    an expression. *)
module WorkletExtension (Spec: sig
  (** type of pattern 1 *)
  type pat1
  (** type of pattern 2 *)
  type pat2

  (** the ppxlib pattern that generates pat1 and pat2 *)
  val pattern: unit -> (expression, pat1 -> pat2 -> 'a, 'a) Ast_pattern.t
  val transform: loc:Location.t -> pat1 -> pat2 -> expression

  (** Name for the `let` expr position, e.g. `let%{shorthand} a = b` *)
  val shorthand: string option
  (** Name for the `let` structure item, e.g. `let%{topLevel} a = b;;` *)
  val topLevel: string option
  (** Name for the expression matcher, e.g. `[%{name} b]` *)
  val name: string
end) = struct

  let getLocOf f ~ctxt =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    f ~loc

  let exprPattern () = Ast_pattern.(single_expr_payload (Spec.pattern ()))

  let letExprPattern () = Ast_pattern.(single_expr_payload (pexp_let nonrecursive (value_binding ~pat:__ ~expr:(Spec.pattern ()) ^:: nil ) __))

  let stmtPattern () = Ast_pattern.(pstr (pstr_value nonrecursive (value_binding ~pat:__ ~expr:(Spec.pattern ()) ^:: nil) ^::nil))

  let transformStmt f ~ctxt a b c =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    [%stri let [%p a] = [%e f ~loc b c]]

  let transformLetExpr f ~ctxt a b c d =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    [%expr let [%p a] = [%e f ~loc b c] in [%e d]]

  let expr = Extension.V3.declare Spec.name 
    Extension.Context.Expression
    (exprPattern ())
    (getLocOf Spec.transform)

  let stmt = match Spec.topLevel with
    | Some topLevel ->
        Extension.V3.declare topLevel
        Extension.Context.Structure_item
          (stmtPattern ())
          (transformStmt Spec.transform)
    | None -> expr

  let letexpr = match Spec.shorthand with
    | Some shorthand ->
        Extension.V3.declare shorthand
        Extension.Context.Expression
        (letExprPattern ())
        (transformLetExpr Spec.transform)
    | None -> expr

  let exec ~loc ({pexp_loc; _} as exp) = Ast_pattern.parse (Spec.pattern ()) loc exp (Spec.transform ~loc:pexp_loc)

  let parseExpr ~loc ?onError exp =
    Ast_pattern.parse (exprPattern ()) loc exp ?on_error:onError (Spec.transform ~loc)

  let parseStmt ~loc ?onError stm next =
    let buildlet pat b c =
      let expr = Spec.transform ~loc b c in
      Ast_builder.Default.(pexp_let ~loc Nonrecursive [value_binding ~loc ~pat ~expr] next)
    in
    Ast_pattern.parse (stmtPattern ()) loc stm ?on_error:onError buildlet

  module Rule = struct
    let expr = Context_free.Rule.extension expr
    let letexpr = Context_free.Rule.extension letexpr
    let stmt = Context_free.Rule.extension stmt
  end
end

module Apply = WorkletExtension(struct 
  type pat1 = expression
  type pat2 = (arg_label * expression) list

  let name = "apply"
  let shorthand = Some "app"

  let topLevel = None

  let pattern () = Ast_pattern.(pexp_apply __ __)

  let transform ~loc = expandFunctionApply ~loc (fun arity ->
    match arity with
      | 0
      | 1 -> [%expr ReanimatedWorklet.exec ]
      | 2 -> [%expr ReanimatedWorklet.exec2 ]
      | 3 -> [%expr ReanimatedWorklet.exec3 ]
      | 4 -> [%expr ReanimatedWorklet.exec4  ]
      | 5 -> [%expr ReanimatedWorklet.exec5 ]
      | 6 -> [%expr ReanimatedWorklet.exec6 ]
      | 7 -> [%expr ReanimatedWorklet.exec7 ]
      | _ -> Location.raise_errorf ~loc "Unknown arity %d" arity
  )
end)

module Ui = WorkletExtension(struct 
  type pat1 = expression
  type pat2 = (arg_label * expression) list

  let name = "runOnUI"
  let shorthand = None
  let topLevel = None

  let pattern () = Ast_pattern.(pexp_apply __ __)

  let transform ~loc = expandFunctionApply ~loc (fun arity ->
    match arity with
      | 0
      | 1 -> [%expr ReanimatedWorklet.runOnUI ]
      | 2 -> [%expr ReanimatedWorklet.runOnUI2 ]
      | 3 -> [%expr ReanimatedWorklet.runOnUI3 ]
      | 4 -> [%expr ReanimatedWorklet.runOnUI4 ]
      | 5 -> [%expr ReanimatedWorklet.runOnUI5 ]
      | 6 -> [%expr ReanimatedWorklet.runOnUI6 ]
      | 7 -> [%expr ReanimatedWorklet.runOnUI7 ]
      | _ -> Location.raise_errorf ~loc "Unknown arity %d" arity
  )
end)

module Js = WorkletExtension(struct 
  type pat1 = expression
  type pat2 = (arg_label * expression) list

  let name = "runOnJS"
  let shorthand = None
  let topLevel = None

  let pattern () = Ast_pattern.(pexp_apply __ __)

  let transform ~loc = expandFunctionApply ~loc (fun arity ->
    match arity with
      | 0
      | 1 -> [%expr ReanimatedWorklet.runOnJS ]
      | 2 -> [%expr ReanimatedWorklet.runOnJS2 ]
      | 3 -> [%expr ReanimatedWorklet.runOnJS3 ]
      | 4 -> [%expr ReanimatedWorklet.runOnJS4 ]
      | 5 -> [%expr ReanimatedWorklet.runOnJS5 ]
      | 6 -> [%expr ReanimatedWorklet.runOnJS6 ]
      | 7 -> [%expr ReanimatedWorklet.runOnJS7 ]
      | _ -> Location.raise_errorf ~loc "Unknown arity %d" arity
  )
end)

module Worklet = WorkletExtension(struct 
  type pat1 = pattern
  type pat2 = expression
  let pattern () = Ast_pattern.(pexp_fun nolabel none __ __)

  let transform = expandWorkletDef
  let name = "worklet"
  let topLevel = Some "wklt"
  let shorthand = Some "wklt_"
end)

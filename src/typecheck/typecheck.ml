open Ast

exception TypeError of string

let rec type_exp env e : typ * exp =
  match e with
  | IntLit i -> (TInt, IntLit i)
  | StringLit s -> (TString, StringLit s)
  | BoolLit b -> (TBool, BoolLit b)
  (* Array *)
  (* Location *)
  (* Record *)
  (* FuncCall *)
  | BinOp (e1, op, e2) ->
      let t1, e1' = type_exp env e1 in
      let t2, e2' = type_exp env e2 in
      let t =
        match (t1, op, t2) with
        | TBool, (And | Or | Eq | NotEq), TBool -> TBool
        | TInt, (LessThan | LessEqual | GreaterThan | GreaterEqual), TInt ->
            TBool
        | TInt, (Plus | Minus | Mult | Div | Rem), TInt -> TInt
        | _ ->
            raise
              (TypeError
                 ( "operator " ^ show_binop op
                 ^ " applied to expressions of type " ^ show_t t1 ^ " and "
                 ^ show_t t2 ) )
      in
      (t, BinOp (e1', op, e2'))
  | UnOp (op, e) -> (
      let t, e' = type_exp env e in
      match (op, t) with
      | Not, TBool -> (TBool, UnOp (op, e'))
      | Neg, TInt -> (TInt, UnOp (op, e'))
      | _ ->
          raise
            (TypeError
               ( "operator " ^ show_unop op ^ " applied to expression of type "
               ^ show_t t ) ) )
  | Assign (l, _) -> (
    match l with
    | FieldAccess (_, _) -> raise (TypeError "Record fields are immutable")
    | _ -> failwith "TODO: Not implemented" )
  (* If *)
  (* While *)
  (* Break *)
  (* Let *)
  (* Seq *)
  (* Match *)
  | _ -> failwith "TODO: Not implemented"

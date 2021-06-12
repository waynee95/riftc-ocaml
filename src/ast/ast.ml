type id = string

(* Rift basic data types *)
type typ = TInt | TString | TBool | TArray of typ | TCustom of string

type field = id * typ

type binop =
  | Plus
  | Minus
  | Mult
  | Div
  | Rem
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | And
  | Or
  | Eq
  | NotEq

type unop = Not | Neg

type exp =
  | IntLit of int64
  | StringLit of string
  | BoolLit of bool
  | Array of exp list
  | Location of location
  | Record of id * exp list
  | FuncCall of {name: id; args: exp list}
  | BinOp of exp * binop * exp
  | UnOp of unop * exp
  | Assign of location * exp
  | If of {cond: exp; true': exp; false': exp}
  | While of exp * exp
  | Break
  | Let of {decls: decl list; body: exp list}
  | Seq of exp list
(* TODO: Match *)

and location =
  | Id of id
  | FieldAccess of location * id
  | Index of location * exp

and decl =
  | RecordDecl of id * field list
  (* TODO: EnumDecl *)
  | VarDecl of {name: id; typ: typ option; value: exp; vartyp: vartyp}
  | FuncDecl of {name: id; params: field list; returnTyp: typ option; body: exp}
  | ExternDecl of {name: id; params: field list; returnTyp: typ option}

(* Determines whether a variable is immutable=val or mutable=var *)
and vartyp = Var | Val

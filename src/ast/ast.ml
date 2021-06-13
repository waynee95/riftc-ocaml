type scope = Global | Parameter | Local

type id = string

type typ = TInt | TString | TBool | TArray of typ | TCustom of string

let rec show_t typ =
  match typ with
  | TInt -> "i64"
  | TString -> "string"
  | TBool -> "bool"
  | TArray t' -> "[" ^ show_t t' ^ "]"
  | TCustom t' -> t'

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

let show_binop op =
  match op with
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Rem -> "%"
  | LessThan -> "<"
  | LessEqual -> "<="
  | GreaterThan -> ">"
  | GreaterEqual -> ">="
  | And -> "&&"
  | Or -> "||"
  | Eq -> "=="
  | NotEq -> "!="

type unop = Not | Neg

let show_unop op = match op with Not -> "!" | Neg -> "-"

type exp =
  | IntLit of int64
  | StringLit of string
  | BoolLit of bool
  | Array of exp list
  | Location of location
  | Record of id * exp list
  | FuncCall of {name: id; args: exp list option}
  | BinOp of exp * binop * exp
  | UnOp of unop * exp
  | Assign of location * exp
  | If of {cond: exp; true': exp; false': exp}
  | While of exp * exp
  | Break
  | Let of {decls: decl list; body: exp}
  | Seq of exp list
  | Match of exp * match_case list

and location =
  | Id of id * scope option
  | FieldAccess of location * id
  | Index of location * exp

and decl =
  | RecordDecl of id * field list
  | EnumDecl of id * decl list
  | VariantDecl of id * typ list option
  | VarDecl of {name: id; typ: typ option; value: exp; vartyp: vartyp}
  | FuncDecl of {name: id; params: field list; returnTyp: typ option; body: exp}
  | ExternDecl of {name: id; params: field list; returnTyp: typ option}

(* Determines whether a variable is immutable=val or mutable=var *)
and vartyp = Var | Val

and match_case = MatchCase of pattern * exp

and pattern =
  | ValuePattern of exp
  | RecordPattern of pattern list
  | ConstructorPattern of id * pattern list option
  | VariablePattern of id
  | CatchAll

%{
open Ast
%}

%token <string> ID
%token <string> TYPE_ID
%token <int64> INT_LIT
%token <string> STRING_LIT
%token TYPE_INT             "i64"
%token TYPE_BOOL            "bool"
%token TYPE_STRING          "string"
%token TRUE                 "true"
%token FALSE                "false"
%token COMMA                ","
%token COLON                ":"
%token SEMICOLON            ";"
%token LPAREN               "("
%token RPAREN               ")"
%token LBRACKET             "["
%token RBRACKET             "]"
%token LCURLY               "{"
%token RCURLY               "}"
%token DOT                  "."
%token ARROW                "=>"
%token BAR                  "|"
%token PLUS                 "+"
%token MINUS                "-"
%token MULT                 "*"
%token DIV                  "/"
%token REM                  "%"
%token LT                   "<"
%token LE                   "<="
%token GT                   ">"
%token GE                   ">="
%token AND                  "&&"
%token OR                   "||"
%token EQ                   "=="
%token NOTEQ                "!="
%token NOT                  "!"
%token ASSIGN               "="
%token IF                   "if"
%token THEN                 "then"
%token ELSE                 "else"
%token WHILE                "while"
%token DO                   "do"
%token LET                  "let"
%token IN                   "in"
%token END                  "end"
%token BREAK                "break"
%token FN                   "fn"
%token EXTERN               "extern"
%token VAR                  "var"
%token VAL                  "val"
%token MATCH                "match"
%token WITH                 "with"
%token TYPE                 "type"
%token EOF

/* from lowest precedence */
%nonassoc ELSE
%nonassoc DO

%right ARROW
%right ASSIGN

%left OR
%left AND
%left EQ NOTEQ
%left LT LE GE GT
%left PLUS MINUS
%left MULT DIV REM
%right NOT

/* to highest precedence */

%start <Ast.exp> prog

%%

prog: exp EOF { $1 }

exp:
  | literal { $1 }
  | "[" separated_list(COMMA, exp) "]" { Array($2) }
  | TYPE_ID "{" separated_list(COMMA, exp) "}" { Record($1, $3) }
  | lvalue { Location($1) }
  | ID "(" separated_list(COMMA, exp) ")" { FuncCall { name=$1; args=$3 } }
  | "(" separated_list(SEMICOLON, exp) ")" { Seq($2) }
  | "-" exp { UnOp(Neg, $2) }
  | "!" exp { UnOp(Not, $2) }
  | exp binop exp { BinOp($1, $2, $3) }
  | lvalue "=" exp { Assign($1, $3) }
  | "if" exp "then" exp "else" exp { If { cond=$2; true'=$4; false'=$6 } }
  | "let" list(decl) "in" separated_list(SEMICOLON, exp) "end" { Let { decls=$2; body=Seq($4) } }
  | "while" exp "do" exp { While($2, $4) }
  | "break" { Break }
  | "match" exp "with" nonempty_list(matchcase) { Match($2, $4) }

matchcase:
  | "|" pattern "=>" exp { MatchCase($2, $4) }

pattern:
  | literal { ValuePattern($1) }
  (* TODO: Constructor *)
  | ID { VariablePattern($1) }
  | "{" separated_list(COMMA, pattern) "}" { RecordPattern($2) }
  | "else" { CatchAll }

lvalue:
  | ID { Id($1) }
  | lvalue "." ID { FieldAccess($1, $3) }
  | lvalue "[" exp "]" { Index($1, $3) }

decl:
  | "type" TYPE_ID "=" "{" separated_list(COMMA, field) "}" { RecordDecl($2, $5) }
  | "type" TYPE_ID "=" separated_nonempty_list(BAR, variant_decl) { EnumDecl($2, $4) }
  | vartyp ID maybe_typ_sig "=" exp { VarDecl { name=$2; typ=$3; value=$5; vartyp=$1 } }
  | "fn" ID "(" separated_list(COMMA, field) ")" maybe_typ_sig "=" exp
    {
      FuncDecl { name=$2; params=$4; returnTyp=$6; body=$8 }
    }
  | "extern" ID "(" separated_list(COMMA, field) ")" maybe_typ_sig
    {
      ExternDecl { name=$2; params=$4; returnTyp=$6; }
    }

variant_decl:
  | TYPE_ID "(" separated_nonempty_list(COMMA, typ) ")" { VariantDecl($1, Some($3)) }
  | TYPE_ID { VariantDecl($1, None) }

field:
  | ID ":" typ { ($1, $3) }

vartyp:
  | "var" { Var }
  | "val" { Val }

maybe_typ_sig:
  |   { None }
  | ":" typ { Some($2) }

typ:
  | "i64" { TInt }
  | "bool" { TBool }
  | "string" { TString }
  | "[" typ "]" { TArray($2) }
  | TYPE_ID { TCustom($1) }
  (* TODO: FuncTyp *)

literal:
  | INT_LIT { IntLit($1) }
  | STRING_LIT { StringLit($1) }
  | "true" { BoolLit(true) }
  | "false" { BoolLit(false) }

%inline binop:
  | "+" { Plus }
  | "-" { Minus }
  | "*" { Mult }
  | "/" { Div }
  | "%" { Rem }
  | "<" { LessThan }
  | "<=" { LessEqual }
  | ">" { GreaterThan }
  | ">=" { GreaterEqual }
  | "&&" { And }
  | "||" { Or }
  | "==" { Eq }
  | "!=" { NotEq }


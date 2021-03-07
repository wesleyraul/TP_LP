%%

%name PlcParser

%pos int

%term VAR | FUN | REC | END | FN
    | NOT | AND
    | PLUS | MINUS | TIMES | DIV
    | EQ | DIF | LEQ | LT
    | COLON | SEMIC | ARROW | EQARROW| COMMA
    | IF | THEN | ELSE
    | MATCH | WITH
    | HD | TL | ISE | CONS
    | PRINT
    | RPAR | LPAR | RBRACK | LBRACK | RBRACE | LBRACE
    | BAR | UNDER
    | NIL | BOOL | INT
    | NAME of string | NAT of int
    | EOF

%nonterm Prog of expr
    | Decl of string * expr
    | Expr of expr
    | AtomExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr
    | MatchExpr of expr
    | CondExpr of expr
    | Args of plcType * string
    | Params of plcType
    | TypedVar of plcType * string
    | Type of plcType
    | AtomType of plcType
    | Types of plcType

%right SEMIC ARROW CONS

%left ELSE AND EQ DIF LT LEQ PLUS MINUS TIMES DIV LBRACK

%nonassoc IF NOT HD TL ISE PRINT NAME

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr) 
    | Decl SEMIC Prog (Let(#1 Decl, #2 Decl, Prog))

Decl : VAR NAME EQ Expr ((NAME, Expr))
    | FUN NAME Args EQ Expr ((NAME, Anon(Args, Expr)))
    | FUN REC NAME Args COLON Type EQ Expr ((NAME, MakeFun())) ##

Expr : AtomExpr (AtomExpr)
    | AppExpr (AppExpr)
    | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
    | MATCH Expr WITH MatchExpr (Match(Expr, [MatchExpr]))
    | NOT Expr (Prim1("!", Expr))
    | MINUS Expr (Prim1("-", Expr))
    | HD Expr (Prim1("hd", Expr))
    | TL Expr (Prim1("tl", Expr))
    | ISE Expr (Prim1("ise", Expr))
    | PRINT Expr (Prim1("print", Expr))
    | Expr AND Expr (Prim2("&&", Expr1, Expr2))
    | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
    | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
    | Expr TIMES Expr (Prim2("*", Expr1, Expr2))
    | Expr DIV Expr (Prim2("/", Expr1, Expr2))
    | Expr EQ Expr (Prim2("=", Expr1, Expr2))
    | Expr DIF Expr (Prim2("!=", Expr1, Expr2))
    | Expr LT Expr (Prim2("<", Expr1, Expr2))
    | Expr LEQ Expr (Prim2("<=", Expr1, Expr2))
    | Expr CONS Expr (Prim2("::", Expr1, Expr2))
    | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
    | Expr LBRACK NAT RBRACK (Item(NAT, Expr)) #

AtomExpr : Const (Const)
    | NAME (Var(NAME)) #
    | LBRACE Prog RBRACE (Prog)
    | LPAR Expr RPAR (Expr) #
    | LPAR Comps RPAR (List [Comps])
    | FN Args EQARROW Expr END (Anon(Args, Expr)) #

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2)) #
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr)) #

Const : TRUE (ConB(TRUE)) | FALSE (ConB(FALSE)) 
    | NAT (ConI(NAT))
    | LPAR RPAR (List []) #
    | LPAR Type LBRACK RBRACK RPAR (ESeq(SeqT Type)) #

Comps : Expr COMMA Expr ([Expr1, Expr2]) #
    | Expr COMMA Comps ([Expr, Comps]) #

MatchExpr : END () # 
    | BAR CondExpr ARROW Expr MatchExpr ((CondExpr, Expr), MatchExpr) #

CondExpr : Expr (SOME (Expr)) #
    | UNDER (NONE)

Args : LPAR RPAR ((ListT [], "()")) #
    | LPAR Params RPAR (Params) #

Params : TypedVar ((#1 TypedVar, #2 TypedVar))
    | TypedVar COMMA Params (TypedVar, Params) #

TypedVar : Type NAME ((Type, NAME)) #

Type : AtomType (AtomType)
    | LPAR Types RPAR (Types) #
    | LBRACK Type RBRACK (SeqT Type) #
    | Type ARROW Type (FunT(Type1, Type2)) #

AtomType : NIL (ListT [])
    | BOOL (BoolT)
    | INT (IntT)
    | LPAR Type RPAR (Type) #

Types : Type COMMA Type (ListT [Type; Type])
    | Type COMMA Types (ListT [Type; Types])
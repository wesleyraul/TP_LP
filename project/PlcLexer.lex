(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword (s, lpos, rpos) =
    case s of
        "var" => VAR(lpos,rpos)
        | "Bool" => BOOL(lpos,rpos)
        | "else" => ELSE (lpos,rpos)
        | "end" => END(lpos,rpos)
        | "false" => FALSE(lpos,rpos)
        | "fn" => FN(lpos,rpos)
        | "fun" => FUN(lpos,rpos)
        | "hd" => HD(lpos,rpos)
        | "if" => IF(lpos,rpos)
        | "Int" => INT(lpos,rpos)
        | "ise" => ISE(lpos,rpos)
        | "match" => MATCH(lpos,rpos)
        | "Nil" => NIL(lpos,rpos)
        | "print" => PRINT(lpos,rpos)
        | "rec" => REC(lpos,rpos)
        | "then" => THEN(lpos,rpos)
        | "tl" => TL(lpos,rpos)
        | "true" => TRUE(lpos,rpos)
        | "with" => WITH(lpos,rpos)
        | _   => NAME (s, lpos, rpos)

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

fun strToInt s =
    case Int.fromString s of
        SOME i => i
        | NOME => raise Fail ("could not convert string'"^s^"'to integer")
 
(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha = [A-Za-z];
digit = [0-9];
whitespace = [\ \t];
identifier = [a-zA-Z_][a-zA-Z_0-9]*;

%%
\n => (lineNumber := !lineNumber+1; lex());
{whitespace}+ => (lex());
{digit}+ => (NAT (strToInt(yytext), yypos, yypos));
{identifier} => (keyword(yytext, yypos, yypos));


"!" => (NOT(yypos, yypos));
"&&"=> (AND(yypos, yypos));
"+" => (PLUS(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (TIMES(yypos, yypos));
"/" => (DIV(yypos, yypos));
"=" => (EQ(yypos, yypos));
"!=" => (DIF(yypos, yypos));
"<" => (LT(yypos, yypos));
"<=" => (LEQ(yypos,yypos));
"::" => (CONS(yypos,yypos));
";" => (SEMIC(yypos, yypos));
"then" =>(THEN(yypos, yypos));
"else" => (ELSE(yypos, yypos));
"if" => (IF(yypos, yypos));
"match" => (MATCH(yypos, yypos));
"hd" => (HD(yypos, yypos));
"tl" => (TL(yypos, yypos));
"ise" => (ISE(yypos, yypos));
"print" => (PRINT(yypos, yypos));
"[" => (LBRACK(yypos, yypos));
"]" => (RBRACK(yypos, yypos));

"end" => (END(yypos,yypos));
"var" => (VAR(yypos, yypos));
"rec" => (REC(yypos, yypos));

"," => (COMMA(yypos, yypos));
"{" => (LBRACE(yypos, yypos));
"}" => (RBRACE(yypos, yypos));
"("=> (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
"fn" => (FN(yypos, yypos));
"|" => (BAR(yypos, yypos));
"_" => (UNDER(yypos, yypos));



"true" => (TRUE(yypos, yypos));
"false" => (FALSE(yypos, yypos));
"=>" => (EQARROW(yypos, yypos));
"->" => (ARROW(yypos, yypos));

. =>(error("\n***Lexer erro: bad character *** \n"); raise Fail ("Lexer erro: bad character" ^ yytext));

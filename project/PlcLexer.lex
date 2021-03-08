(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword (s, lpos, rpos) =
    case s of
        "var" => Var(lpos,rpos)
        | "Bool" => Bool(lpos,rpos)
        | "else" => Else (lpos,rpos)
        | "end" => End(lpos,rpos)
        | "false" => false(lpos,rpos)
        | "fn" => Fn(lpos,rpos)
        | "fun" => Fun(lpos,rpos)
        | "hd" => Hd(lpos,rpos)
        | "if" => If(lpos,rpos)
        | "Int" => Int(lpos,rpos)
        | "ise" => Ise(lpos,rpos)
        | "match" => Match(lpos,rpos)
        | "Nil" => Nil(lpos,rpos)
        | "print" => Print(lpos,rpos)
        | "rec" => Rec(lpos,rpos)
        | "then" => Then(lpos,rpos)
        | "tl" => Tl(lpos,rpos)
        | "true" => True(lpos,rpos)
        | "with" => Whith(lpos,rpos)
        | _   => Name (s, lpos, rpos)

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
{digit}+ => (CINT (strToInt(yytext), yypos, yypos));
{identifier} => (keyword(yytext, yypos, yypos));

"!" => (NOT(yypos))
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


. =>(error("\n***Lexer erro: bad character *** \n"); raise Fail ("Lexer erro: bad character" ^yytext));

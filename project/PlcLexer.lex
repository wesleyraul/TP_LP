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
        | "let" => Let (lpos, rpos)
        | "val" => Val (lpos, rpos)
        | "fn" => Fn (lpos, rpos)
        | "fun" => Fun (lpos, rpos)
        | "if" => If (lpos, rpos)
        | "in" => In (lpos, rpos)
        | "else" => Else(lpos, rpos)
        | "end" => End (lpos, rpos)
        | "of" => Of(lpos, rpos)
        | "case" => Case (lpos, rpos)
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
{whitespace+} => (lex());
{digit}+ => (CINT (strToInt(yytext), yypos, yypos));
{identifier} => (keyword(yytext, yypos, yypos));
"+" => (Plus(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (MULTI(yypos, yypos));
"/" => (DIV(yypos, yypos));
"=" => (EQ(yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
";" => (SEMIC(yypos, yypos));

. =>(error("\n***Lexer erro: bad character *** \n"); raise Fail ("Lexer erro: bad character" ^yytext));

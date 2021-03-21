CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Plc.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

val e1 = fromString "15";
run e1;

val e2 = fromString "true";
run e2;

val e3 = fromString "()";
run e3;

val e4 = fromString "(6, false)";
run e4;

val e5 = fromString "(6, false) [1]";
run e5;

val e6 = fromString "([Bool] [])";
run e6;

val e7 = fromString "print x; true";
run e7;

val e8 = fromString "3::7::t";
run e8;

val e9 = fromString "fn (Int x) => -x end";
run e9;

val e10 = fromString "var x = 9; x + 1";
run e10;

val e11 = fromString "fun f(Int x) = x; f(1)";
run e11;

val e12 = fromString "match x with | 0 -> 1 | _ -> -1 end";
run e12;

val e13 = fromString "fun rec f(Int n): Int = if n <= 0 then 0 else n + f(n-1); f(5)";
run e13;

val e14 = fromString "fun rec f1(Int x): Int = x + 1; f1(12)";
run e14;

val e15 = fromString "fun rec fact(Int x): Int = if x = 0 then 1 else x*(fact (x-1)); fact(5)";
run e15;

val e16 = fromString "var n = 1; if n <= 0 then 0 else n";
run e16;

val e17 = fromString ("fun add (Int x, Int y) = x + y; add(1, 2)");
run e17;
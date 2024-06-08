
#use "tag-parser.ml";;

type case = {name: string; input: sexpr; expected: expr};;

let cases = [
{name = "TEST_1"; input = ScmPair (ScmSymbol "let", ScmPair (ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmNumber (ScmRational(42,1)), ScmNil)), ScmPair (ScmPair (ScmSymbol "y", ScmPair (ScmNumber (ScmRational(42,1)), ScmNil)), ScmNil)), ScmPair (ScmSymbol "y", ScmNil))); expected = ScmApplic (ScmLambdaSimple (["x"; "y"], ScmVar "y"),[ScmConst (ScmNumber (ScmRational(42,1))); ScmConst (ScmNumber (ScmRational(42,1)))])};
{name = "TEST_2"; input = ScmPair (ScmSymbol "let", ScmPair (ScmNil, ScmPair (ScmNumber (ScmRational(72,1)), ScmNil))); expected = ScmApplic (ScmLambdaSimple ([], ScmConst (ScmNumber (ScmRational(72,1)))), [])};
{name = "TEST_3"; input = ScmPair (ScmSymbol "let", ScmPair (ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmNumber (ScmRational(73,1)), ScmNil)), ScmNil), ScmPair (ScmSymbol "x", ScmNil))); expected = ScmApplic (ScmLambdaSimple (["x"], ScmVar "x"), [ScmConst (ScmNumber (ScmRational(73,1)))])};
{name = "TEST_4"; input = ScmPair (ScmSymbol "let", ScmPair (ScmNil, ScmPair (ScmPair (ScmSymbol "begin", ScmPair (ScmNumber (ScmRational(94,1)), ScmPair (ScmNumber (ScmRational(94,1)), ScmPair (ScmNumber (ScmRational(94,1)), ScmNil)))), ScmNil))); expected = ScmApplic(ScmLambdaSimple ([],ScmSeq[ScmConst (ScmNumber (ScmRational(94,1))); ScmConst (ScmNumber (ScmRational(94,1)));ScmConst (ScmNumber (ScmRational(94,1)))]),[])};
{name = "TEST_5"; input = ScmPair (ScmSymbol "let", ScmPair (ScmPair (ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational(116,1)), ScmNil)), ScmPair (ScmPair (ScmSymbol "b", ScmPair (ScmNumber (ScmRational(116,1)), ScmNil)), ScmNil)), ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil))), ScmNil))); expected = ScmApplic (ScmLambdaSimple (["a"; "b"], ScmApplic (ScmVar "+", [ScmVar "a"; ScmVar "b"])),[ScmConst (ScmNumber (ScmRational(116,1))); ScmConst (ScmNumber (ScmRational(116,1)))])};
{name = "TEST_6"; input = ScmPair (ScmSymbol "let", ScmPair (ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmChar 'x', ScmNil)), ScmNil), ScmPair (ScmSymbol "x", ScmNil))); expected = ScmApplic (ScmLambdaSimple (["x"], ScmVar "x"), [ScmConst (ScmChar 'x')])};
{name = "TEST_7"; input = ScmPair (ScmSymbol "let", ScmPair (ScmPair (ScmPair (ScmSymbol "t", ScmPair (ScmBoolean true, ScmNil)), ScmPair (ScmPair (ScmSymbol "th", ScmPair (ScmNumber (ScmRational(36,1)), ScmNil)), ScmPair (ScmPair (ScmSymbol "el", ScmPair (ScmNumber (ScmRational(36,1)), ScmNil)), ScmNil))), ScmPair (ScmPair (ScmSymbol "if", ScmPair (ScmSymbol "t", ScmPair (ScmSymbol "th", ScmPair (ScmSymbol "el", ScmNil)))), ScmNil))); expected = ScmApplic (ScmLambdaSimple (["t"; "th"; "el"], ScmIf (ScmVar "t", ScmVar "th", ScmVar "el")),[ScmConst (ScmBoolean true); ScmConst (ScmNumber (ScmRational(36,1)));ScmConst (ScmNumber (ScmRational(36,1)))])};
{name = "TEST_9"; input = ScmPair (ScmSymbol "let", ScmPair (ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmString "asd", ScmNil)), ScmNil), ScmPair (ScmPair (ScmSymbol "begin", ScmPair (ScmSymbol "x", ScmNil)), ScmNil))); expected = ScmApplic (ScmLambdaSimple (["x"], ScmVar "x"), [ScmConst (ScmString "asd")])};
{name = "TEST_10"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil)), ScmPair (ScmSymbol "a", ScmNil)))  ; expected = ScmLambdaSimple (["a"; "b"], ScmVar "a")};
{name = "TEST_11"; input = ScmPair (ScmSymbol "lambda", ScmPair (ScmPair (ScmSymbol "a", ScmNil), ScmPair (ScmSymbol "a", ScmNil)))  ; expected = ScmLambdaSimple (["a"], ScmVar "a")};
{name = "TEST_12"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmNil)),ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmNil))), ScmNil))); expected = ScmLambdaSimple (["x"; "y"], ScmApplic (ScmVar "+", [ScmVar "x"; ScmVar "y"]))};
{name = "TEST_13"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmPair (ScmSymbol "z", ScmNil))),ScmPair(ScmPair (ScmSymbol "if",ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmPair (ScmSymbol "z", ScmNil)))),ScmNil))); expected = ScmLambdaSimple (["x"; "y"; "z"], ScmIf (ScmVar "x", ScmVar "y", ScmVar "z"))};
{name = "TEST_14"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmPair (ScmSymbol "z", ScmNil))),ScmPair(ScmPair (ScmSymbol "begin",ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmPair (ScmSymbol "z", ScmNil)))),ScmNil))); expected = ScmLambdaSimple (["x"; "y"; "z"], ScmSeq [ScmVar "x"; ScmVar "y"; ScmVar "z"])};
{name = "TEST_15"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmNil)),ScmPair (ScmPair (ScmSymbol "set", ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmNil))), ScmNil))); expected = ScmLambdaSimple (["x"; "y"], ScmApplic (ScmVar "set", [ScmVar "x"; ScmVar "y"]))};
{name = "TEST_16"; input = ScmPair (ScmSymbol "lambda",ScmPair(ScmPair (ScmSymbol "x",ScmPair (ScmSymbol "y", ScmPair (ScmSymbol "z", ScmPair (ScmSymbol "w", ScmNil)))),ScmPair(ScmPair (ScmSymbol "if",ScmPair (ScmSymbol "x",ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmSymbol "y", ScmPair (ScmSymbol "z", ScmNil))),ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmSymbol "z", ScmPair (ScmSymbol "w", ScmNil))), ScmNil)))),ScmNil))); expected = ScmLambdaSimple (["x"; "y"; "z"; "w"],ScmIf (ScmVar "x", ScmApplic (ScmVar "+", [ScmVar "y"; ScmVar "z"]),ScmApplic (ScmVar "+", [ScmVar "z"; ScmVar "w"])))};
{name = "TEST_17"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmNil)),ScmPair(ScmPair (ScmSymbol "or", ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmPair (ScmSymbol "z", ScmNil))), ScmNil)),    ScmNil))) ; expected = ScmLambdaSimple (["x"; "y"], ScmApplic (ScmVar "x", [ScmVar "y"; ScmVar "z"]))};
{name = "TEST_18"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmSymbol "vs")),ScmPair(ScmPair (ScmSymbol "begin",ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmPair (ScmSymbol "vs", ScmNil)))),ScmNil))); expected = ScmLambdaOpt (["x"; "y"], "vs", ScmSeq [ScmVar "x"; ScmVar "y"; ScmVar "vs"])};
{name = "TEST_19"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "x", ScmSymbol "vs"),ScmPair (ScmPair (ScmSymbol "if", ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "vs", ScmNil))), ScmNil))); expected = ScmLambdaOpt (["x"], "vs", ScmIf (ScmVar "x", ScmVar "vs", ScmConst ScmVoid))};
{name = "TEST_20"; input = ScmPair (ScmSymbol "lambda", ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmSymbol "vs")),ScmPair(ScmPair (ScmSymbol "and",ScmPair (ScmNumber (ScmRational(55,1)), ScmPair (ScmNumber (ScmRational(55,1)), ScmPair (ScmNumber (ScmRational(55,1)), ScmNil)))),ScmNil))); expected = ScmLambdaOpt (["x"; "y"], "vs",ScmIf (ScmConst (ScmNumber (ScmRational(55,1))),ScmIf (ScmConst (ScmNumber (ScmRational(55,1))), ScmConst (ScmNumber (ScmRational(55,1))),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)))};
{name = "TEST_21"; input = ScmPair (ScmSymbol "lambda",ScmPair(ScmPair (ScmSymbol "a",ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmPair (ScmSymbol "d", ScmSymbol "vs")))),ScmPair(ScmPair (ScmSymbol "if",ScmPair (ScmPair (ScmSymbol "u003e", ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil))),ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmSymbol "c", ScmPair (ScmSymbol "d", ScmNil))),ScmPair (ScmPair (ScmSymbol "list", ScmPair (ScmSymbol "vs", ScmNil)), ScmNil)))),ScmNil))); expected = ScmLambdaOpt (["a"; "b"; "c"; "d"], "vs",ScmIf (ScmApplic (ScmVar "u003e", [ScmVar "a"; ScmVar "b"]),ScmApplic (ScmVar "+", [ScmVar "c"; ScmVar "d"]), ScmApplic (ScmVar "list", [ScmVar "vs"])))};
{name = "TEST_22"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "b", ScmSymbol "vs"),ScmPair(ScmPair (ScmSymbol "begin",ScmPair (ScmSymbol "b",ScmPair(ScmPair (ScmSymbol "define", ScmPair (ScmSymbol "x", ScmPair (ScmNumber (ScmRational(41,1)), ScmNil))),ScmPair(ScmPair (ScmSymbol "set",ScmPair (ScmSymbol "b",ScmPair(ScmPair (ScmSymbol "+", ScmPair (ScmSymbol "x", ScmPair (ScmNumber (ScmRational(41,1)), ScmNil))),ScmNil))),ScmNil)))),ScmNil))); expected = ScmLambdaOpt (["b"], "vs",ScmSeq[ScmVar "b"; ScmDef (ScmVar "x", ScmConst (ScmNumber (ScmRational(41,1))));ScmApplic (ScmVar "set",[ScmVar "b"; ScmApplic (ScmVar "+", [ScmVar "x"; ScmConst (ScmNumber (ScmRational(41,1)))])])])};
{name = "TEST_23"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmSymbol "vs")),ScmPair(ScmPair (ScmSymbol "cond",ScmPair (ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational(30,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "b", ScmPair (ScmNumber (ScmRational(30,1)), ScmNil)),ScmPair(ScmPair (ScmSymbol "else",ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil))),ScmNil)),ScmNil)))),ScmNil))); expected = ScmLambdaOpt (["a"; "b"], "vs",ScmIf (ScmVar "a", ScmConst (ScmNumber (ScmRational(30,1))),ScmIf (ScmVar "b", ScmConst (ScmNumber (ScmRational(30,1))),ScmApplic (ScmVar "+", [ScmVar "a"; ScmVar "b"]))))};
{name = "TEST_24"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "x", ScmSymbol "vs"), ScmPair (ScmSymbol "vs", ScmNil)))   ; expected = ScmLambdaOpt (["x"], "vs", ScmVar "vs")};
{name = "TEST_25"; input = ScmPair (ScmSymbol "lambda",ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmSymbol "vs")),ScmPair(ScmPair (ScmSymbol "and",ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmPair (ScmSymbol "vs", ScmNil)))),ScmNil))); expected = ScmLambdaOpt (["x"; "y"], "vs",ScmIf (ScmVar "x", ScmIf (ScmVar "y", ScmVar "vs", ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)))};
{name = "TEST_26"; input = ScmPair (ScmSymbol "let*",ScmPair(ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmNumber (ScmRational(42,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "y", ScmPair (ScmNumber (ScmRational(42,1)), ScmNil)), ScmNil)),ScmPair (ScmSymbol "y", ScmNil))); expected = ScmApplic(ScmLambdaSimple (["x"],ScmApplic (ScmLambdaSimple (["y"], ScmVar "y"), [ScmConst (ScmNumber (ScmRational(42,1)))])),[ScmConst (ScmNumber (ScmRational(42,1)))])};
{name = "TEST_27"; input = ScmPair (ScmSymbol "let*",ScmPair(ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmNumber (ScmRational(58,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "y", ScmPair (ScmNumber (ScmRational(58,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "z", ScmPair (ScmNumber (ScmRational(58,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational(58,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "b", ScmPair (ScmNumber (ScmRational(58,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "c", ScmPair (ScmNumber (ScmRational(58,1)), ScmNil)), ScmNil)))))),ScmPair(ScmPair (ScmSymbol "begin",ScmPair (ScmSymbol "x",ScmPair (ScmSymbol "y",ScmPair (ScmSymbol "z",ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil))))))),ScmNil))); expected = ScmApplic(ScmLambdaSimple (["x"],ScmApplic(ScmLambdaSimple (["y"],ScmApplic(ScmLambdaSimple (["z"],ScmApplic(ScmLambdaSimple (["a"],ScmApplic(ScmLambdaSimple (["b"],ScmApplic(ScmLambdaSimple (["c"],ScmSeq [ScmVar "x"; ScmVar "y"; ScmVar "z"; ScmVar "a"; ScmVar "b"; ScmVar "c"]),[ScmConst (ScmNumber (ScmRational(58,1)))])),[ScmConst (ScmNumber (ScmRational(58,1)))])),[ScmConst (ScmNumber (ScmRational(58,1)))])),[ScmConst (ScmNumber (ScmRational(58,1)))])),[ScmConst (ScmNumber (ScmRational(58,1)))])),[ScmConst (ScmNumber (ScmRational(58,1)))])};
{name = "TEST_28"; input = ScmPair (ScmSymbol "let*",ScmPair(ScmPair (ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational(91,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "b", ScmPair (ScmNumber (ScmRational(91,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "c", ScmPair (ScmNumber (ScmRational(91,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "d", ScmPair (ScmNumber (ScmRational(91,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "e", ScmPair (ScmNumber (ScmRational(91,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "f", ScmPair (ScmNumber (ScmRational(91,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "g", ScmPair (ScmNumber (ScmRational(91,1)), ScmNil)), ScmNil))))))),ScmPair(ScmPair (ScmSymbol "and",ScmPair (ScmSymbol "a",ScmPair (ScmSymbol "b",ScmPair (ScmSymbol "c",ScmPair (ScmSymbol "d",ScmPair (ScmSymbol "e", ScmPair (ScmSymbol "f", ScmPair (ScmSymbol "g", ScmNil)))))))),ScmNil))); expected = ScmApplic(ScmLambdaSimple (["a"],ScmApplic(ScmLambdaSimple (["b"],ScmApplic(ScmLambdaSimple (["c"],ScmApplic(ScmLambdaSimple (["d"],ScmApplic(ScmLambdaSimple (["e"],ScmApplic(ScmLambdaSimple (["f"],ScmApplic(ScmLambdaSimple (["g"],ScmIf (ScmVar "a",ScmIf (ScmVar "b",ScmIf (ScmVar "c",ScmIf (ScmVar "d",ScmIf (ScmVar "e",ScmIf (ScmVar "f", ScmVar "g", ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false))),[ScmConst (ScmNumber (ScmRational(91,1)))])),[ScmConst (ScmNumber (ScmRational(91,1)))])),[ScmConst (ScmNumber (ScmRational(91,1)))])),[ScmConst (ScmNumber (ScmRational(91,1)))])),[ScmConst (ScmNumber (ScmRational(91,1)))])),[ScmConst (ScmNumber (ScmRational(91,1)))])),[ScmConst (ScmNumber (ScmRational(91,1)))])};
{name = "TEST_29"; input = ScmPair (ScmSymbol "let*",ScmPair (ScmNil,ScmPair(ScmPair (ScmSymbol "begin",ScmPair (ScmNumber (ScmRational(104,1)), ScmPair (ScmNumber (ScmRational(104,1)), ScmPair (ScmNumber (ScmRational(104,1)), ScmNil)))),ScmNil))); expected = ScmApplic(ScmLambdaSimple ([],ScmSeq[ScmConst (ScmNumber (ScmRational(104,1))); ScmConst (ScmNumber (ScmRational(104,1)));ScmConst (ScmNumber (ScmRational(104,1)))]),[])};
{name = "TEST_30"; input = ScmPair (ScmSymbol "let*",ScmPair(ScmPair (ScmPair (ScmSymbol "a", ScmPair (ScmNumber (ScmRational(92,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "b", ScmPair (ScmNumber (ScmRational(92,1)), ScmNil)), ScmNil)),ScmPair (ScmPair (ScmSymbol "+", ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil))), ScmNil))); expected = ScmApplic(ScmLambdaSimple (["a"],ScmApplic (ScmLambdaSimple (["b"], ScmApplic (ScmVar "+", [ScmVar "a"; ScmVar "b"])),[ScmConst (ScmNumber (ScmRational(92,1)))])),[ScmConst (ScmNumber (ScmRational(92,1)))])};
{name = "TEST_31"; input = ScmPair (ScmSymbol "let*",ScmPair (ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmChar 'x', ScmNil)), ScmNil),ScmPair (ScmSymbol "x", ScmNil))); expected = ScmApplic (ScmLambdaSimple (["x"], ScmVar "x"), [ScmConst (ScmChar 'x')])};
{name = "TEST_32"; input = ScmPair (ScmSymbol "let*",ScmPair(ScmPair (ScmPair (ScmSymbol "t", ScmPair (ScmBoolean true, ScmNil)),ScmPair (ScmPair (ScmSymbol "th", ScmPair (ScmNumber (ScmRational(40,1)), ScmNil)),ScmPair (ScmPair (ScmSymbol "el", ScmPair (ScmNumber (ScmRational(40,1)), ScmNil)), ScmNil))),ScmPair(ScmPair (ScmSymbol "if",ScmPair (ScmBoolean true, ScmPair (ScmNumber (ScmRational(40,1)), ScmPair (ScmNumber (ScmRational(40,1)), ScmNil)))),ScmNil))); expected = ScmApplic(ScmLambdaSimple (["t"],ScmApplic(ScmLambdaSimple (["th"],ScmApplic(ScmLambdaSimple (["el"],ScmIf (ScmConst (ScmBoolean true), ScmConst (ScmNumber (ScmRational(40,1))),ScmConst (ScmNumber (ScmRational(40,1))))),[ScmConst (ScmNumber (ScmRational(40,1)))])),[ScmConst (ScmNumber (ScmRational(40,1)))])),[ScmConst (ScmBoolean true)])};
{name = "TEST_33"; input = ScmPair (ScmSymbol "let*", ScmPair (ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmString "asd", ScmNil)), ScmNil),ScmPair(ScmPair (ScmSymbol "define", ScmPair (ScmSymbol "y", ScmPair (ScmNumber (ScmRational (12,1)), ScmNil))),ScmNil))); expected = ScmApplic(ScmLambdaSimple (["x"], ScmDef (ScmVar "y", ScmConst (ScmNumber (ScmRational (12,1))))),[ScmConst (ScmString "asd")])};
{name = "TEST_34"; input = ScmPair (ScmSymbol "let*", ScmPair (ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmString "asd", ScmNil)), ScmNil),ScmPair(ScmPair (ScmSymbol "begin",ScmPair(ScmPair (ScmSymbol "define",ScmPair (ScmSymbol "y", ScmPair (ScmNumber (ScmRational(12,1)), ScmNil))),ScmPair(ScmPair (ScmSymbol "set", ScmPair (ScmSymbol "y", ScmPair (ScmNumber (ScmRational (12,1)), ScmNil))),ScmNil))),ScmNil))); expected = ScmApplic(ScmLambdaSimple (["x"],ScmSeq[ScmDef (ScmVar "y", ScmConst (ScmNumber (ScmRational(12,1))));ScmApplic (ScmVar "set", [ScmVar "y"; ScmConst (ScmNumber (ScmRational(12,1)))])]),[ScmConst (ScmString "asd")])};
{name = "TEST_35"; input = ScmPair (ScmSymbol "let*", ScmPair (ScmPair (ScmPair (ScmSymbol "x", ScmPair (ScmString "asd", ScmNil)), ScmNil),ScmPair (ScmPair (ScmSymbol "begin", ScmPair (ScmSymbol "x", ScmNil)), ScmNil))); expected = ScmApplic (ScmLambdaSimple (["x"], ScmVar "x"), [ScmConst (ScmString "asd")])};
{name = "TEST_36"; input = ScmPair (ScmSymbol "cond",ScmPair(ScmPair (ScmNumber (ScmRational(28,1)), ScmPair (ScmNumber (ScmRational(28,1)), ScmPair (ScmNumber (ScmRational(28,1)), ScmNil))),ScmPair(ScmPair (ScmNumber (ScmRational(28,1)), ScmPair (ScmNumber (ScmRational(28,1)), ScmPair (ScmNumber (ScmRational(28,1)), ScmNil))),ScmNil))); expected = ScmIf (ScmConst (ScmNumber (ScmRational(28,1))),ScmSeq [ScmConst (ScmNumber (ScmRational(28,1))); ScmConst (ScmNumber (ScmRational(28,1)))],ScmIf (ScmConst (ScmNumber (ScmRational(28,1))),ScmSeq [ScmConst (ScmNumber (ScmRational(28,1))); ScmConst (ScmNumber (ScmRational(28,1)))],ScmConst ScmVoid))};
{name = "TEST_37"; input = ScmPair (ScmSymbol "cond",ScmPair(ScmPair (ScmNumber (ScmRational(116,1)), ScmPair (ScmNumber (ScmRational(116,1)), ScmPair (ScmNumber (ScmRational(116,1)), ScmNil))),ScmPair(ScmPair (ScmNumber (ScmRational(116,1)), ScmPair (ScmNumber (ScmRational(116,1)), ScmPair (ScmNumber (ScmRational(116,1)), ScmNil))),ScmPair(ScmPair (ScmSymbol "else",ScmPair (ScmNumber (ScmRational(116,1)), ScmPair (ScmNumber (ScmRational(116,1)), ScmPair (ScmNumber (ScmRational(116,1)), ScmNil)))),ScmNil)))); expected = ScmIf (ScmConst (ScmNumber (ScmRational(116,1))),ScmSeq [ScmConst (ScmNumber (ScmRational(116,1))); ScmConst (ScmNumber (ScmRational(116,1)))],ScmIf (ScmConst (ScmNumber (ScmRational(116,1))),ScmSeq [ScmConst (ScmNumber (ScmRational(116,1))); ScmConst (ScmNumber (ScmRational(116,1)))],ScmSeq[ScmConst (ScmNumber (ScmRational(116,1))); ScmConst (ScmNumber (ScmRational(116,1)));ScmConst (ScmNumber (ScmRational(116,1)))]))};
{name = "TEST_38"; input = ScmPair (ScmSymbol "cond",ScmPair(ScmPair (ScmNumber (ScmRational(83,1)), ScmPair (ScmNumber (ScmRational(83,1)), ScmPair (ScmNumber (ScmRational(83,1)), ScmNil))),ScmPair(ScmPair (ScmSymbol "else",ScmPair (ScmNumber (ScmRational(83,1)), ScmPair (ScmNumber (ScmRational(83,1)), ScmPair (ScmNumber (ScmRational(83,1)), ScmNil)))),ScmPair(ScmPair (ScmNumber (ScmRational(83,1)), ScmPair (ScmNumber (ScmRational(83,1)), ScmPair (ScmNumber (ScmRational(83,1)), ScmNil))),ScmNil)))); expected = ScmIf (ScmConst (ScmNumber (ScmRational(83,1))),ScmSeq [ScmConst (ScmNumber (ScmRational(83,1))); ScmConst (ScmNumber (ScmRational(83,1)))],ScmSeq[ScmConst (ScmNumber (ScmRational(83,1))); ScmConst (ScmNumber (ScmRational(83,1)));ScmConst (ScmNumber (ScmRational(83,1)))])};
{name = "TEST_39"; input = ScmPair (ScmSymbol "and",ScmPair (ScmNumber (ScmRational(101,1)),ScmPair (ScmNumber (ScmRational(101,1)),ScmPair (ScmNumber (ScmRational(101,1)),ScmPair (ScmNumber (ScmRational(101,1)),ScmPair (ScmNumber (ScmRational(101,1)),ScmPair (ScmNumber (ScmRational(101,1)),ScmPair (ScmNumber (ScmRational(101,1)),ScmPair (ScmNumber (ScmRational(101,1)),ScmPair (ScmNumber (ScmRational(101,1)), ScmPair (ScmNumber (ScmRational(101,1)), ScmNil))))))))))); expected = ScmIf (ScmConst (ScmNumber (ScmRational(101,1))),ScmIf (ScmConst (ScmNumber (ScmRational(101,1))),ScmIf (ScmConst (ScmNumber (ScmRational(101,1))),ScmIf (ScmConst (ScmNumber (ScmRational(101,1))),ScmIf (ScmConst (ScmNumber (ScmRational(101,1))),ScmIf (ScmConst (ScmNumber (ScmRational(101,1))),ScmIf (ScmConst (ScmNumber (ScmRational(101,1))),ScmIf (ScmConst (ScmNumber (ScmRational(101,1))),ScmIf (ScmConst (ScmNumber (ScmRational(101,1))), ScmConst (ScmNumber (ScmRational(101,1))),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false)),ScmConst (ScmBoolean false))};
{name = "TEST_40"; input = ScmPair (ScmSymbol "define",ScmPair (ScmPair (ScmSymbol "square", ScmPair (ScmSymbol "x", ScmNil)),ScmPair (ScmPair (ScmSymbol "*", ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "x", ScmNil))), ScmNil))); expected = ScmDef (ScmVar "square",ScmLambdaSimple (["x"], ScmApplic (ScmVar "*", [ScmVar "x"; ScmVar "x"])))};
{name = "TEST_41"; input = ScmPair (ScmSymbol "define",ScmPair(ScmPair (ScmSymbol "square",ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))),ScmPair(ScmPair (ScmSymbol "*",ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))),ScmNil))); expected = ScmDef (ScmVar "square",ScmLambdaSimple (["a"; "b"; "c"],ScmApplic (ScmVar "*", [ScmVar "a"; ScmVar "b"; ScmVar "c"])))};
{name = "TEST_42"; input = ScmPair (ScmSymbol "define",ScmPair(ScmPair (ScmSymbol "applic",ScmPair (ScmSymbol "fun",ScmPair (ScmSymbol "a",ScmPair (ScmSymbol "b",ScmPair (ScmSymbol "c", ScmPair (ScmSymbol "d", ScmPair (ScmSymbol "e", ScmNil))))))),ScmPair(ScmPair (ScmSymbol "fun",ScmPair (ScmSymbol "a",ScmPair (ScmSymbol "b",ScmPair (ScmSymbol "c", ScmPair (ScmSymbol "d", ScmPair (ScmSymbol "e", ScmNil)))))),ScmNil))); expected = ScmDef (ScmVar "applic",ScmLambdaSimple (["fun"; "a"; "b"; "c"; "d"; "e"],ScmApplic (ScmVar "fun", [ScmVar "a"; ScmVar "b"; ScmVar "c"; ScmVar "d"; ScmVar "e"])))};
{name = "TEST_43"; input = ScmPair (ScmSymbol "define",ScmPair(ScmPair (ScmSymbol "if_fun",ScmPair (ScmSymbol "if_test",ScmPair (ScmSymbol "if_then", ScmPair (ScmSymbol "if_else", ScmNil)))),ScmPair(ScmPair (ScmSymbol "if",ScmPair (ScmSymbol "if_test",ScmPair (ScmSymbol "if_then", ScmPair (ScmSymbol "if_else", ScmNil)))),ScmNil))); expected = ScmDef (ScmVar "if_fun",ScmLambdaSimple (["if_test"; "if_then"; "if_else"],ScmIf (ScmVar "if_test", ScmVar "if_then", ScmVar "if_else")))};
{name = "TEST_44"; input = ScmPair (ScmSymbol "define",ScmPair (ScmPair (ScmSymbol "pairing", ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil))),ScmPair(ScmPair (ScmSymbol "quote",ScmPair (ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil)), ScmNil)),ScmNil))); expected = ScmDef (ScmVar "pairing",ScmLambdaSimple (["a"; "b"],ScmConst (ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil)))))};
{name = "TEST_45"; input = ScmPair (ScmString "should not", ScmPair (ScmString "be", ScmPair (ScmString "list", ScmNil))); expected = ScmApplic (ScmConst (ScmString "should not"),[ScmConst (ScmString "be"); ScmConst (ScmString "list")])};
{name = "TEST_46"; input = ScmPair (ScmSymbol "quote",ScmPair (ScmPair (ScmString "should", ScmPair (ScmString "be", ScmString "list")), ScmNil)); expected = ScmConst (ScmPair (ScmString "should", ScmPair (ScmString "be", ScmString "list")))};
{name = "TEST_47"; input = ScmPair (ScmSymbol "quote",ScmPair(ScmPair (ScmString "vary",ScmPair (ScmString "long",ScmPair (ScmNumber (ScmRational(103,1)),ScmPair (ScmNumber (ScmReal(103.3)),ScmPair (ScmNumber (ScmReal (-4.2)),ScmPair (ScmString "complex", ScmPair (ScmChar 'x', ScmPair (ScmString "list", ScmNil)))))))),ScmNil)); expected = ScmConst(ScmPair (ScmString "vary",ScmPair (ScmString "long",ScmPair (ScmNumber (ScmRational(103,1)),ScmPair (ScmNumber (ScmReal(103.3)),ScmPair (ScmNumber (ScmReal (-4.2)),ScmPair (ScmString "complex",ScmPair (ScmChar 'x', ScmPair (ScmString "list", ScmNil)))))))))};
{name = "TEST_48"; input = ScmPair (ScmSymbol "quote",ScmPair(ScmPair (ScmSymbol "a",ScmPair (ScmSymbol "b",ScmPair (ScmSymbol "c", ScmPair (ScmSymbol "d", ScmPair (ScmSymbol "f", ScmSymbol "g"))))),ScmNil)); expected = ScmConst(ScmPair (ScmSymbol "a",ScmPair (ScmSymbol "b",ScmPair (ScmSymbol "c", ScmPair (ScmSymbol "d", ScmPair (ScmSymbol "f", ScmSymbol "g"))))))};
{name = "TEST_49"; input = ScmPair (ScmSymbol "quote",ScmPair(ScmPair (ScmString "a long long string",ScmPair (ScmString "another long one",ScmPair(ScmPair (ScmString "some numbers",ScmPair (ScmNumber (ScmRational(30,1)), ScmPair (ScmNumber (ScmRational(30,1)), ScmNumber (ScmRational(30,1))))),ScmPair (ScmString "Named ScmChar", ScmChar 'x')))),ScmNil)); expected = ScmConst(ScmPair (ScmString "a long long string",ScmPair (ScmString "another long one",ScmPair(ScmPair (ScmString "some numbers",ScmPair (ScmNumber (ScmRational(30,1)), ScmPair (ScmNumber (ScmRational(30,1)), ScmNumber (ScmRational(30,1))))),ScmPair (ScmString "Named ScmChar", ScmChar 'x')))))};
{name = "TEST_50"; input = ScmPair (ScmSymbol "quote",ScmPair(ScmPair (ScmNumber (ScmReal (-0.2)),ScmPair (ScmNumber (ScmReal(42.3)),ScmPair(ScmPair (ScmNumber (ScmRational(42,1)),ScmPair (ScmNumber (ScmRational(42,1)), ScmPair (ScmNumber (ScmRational(42,1)), ScmNil))),ScmPair (ScmPair (ScmNumber (ScmRational(42,1)), ScmNumber (ScmRational(42,1))),ScmPair (ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmSymbol "c")),ScmPair (ScmSymbol "d", ScmSymbol "f")))))),ScmNil)); expected = ScmConst(ScmPair (ScmNumber (ScmReal (-0.2)),ScmPair (ScmNumber (ScmReal(42.3)),ScmPair(ScmPair (ScmNumber (ScmRational(42,1)),ScmPair (ScmNumber (ScmRational(42,1)), ScmPair (ScmNumber (ScmRational(42,1)), ScmNil))),ScmPair (ScmPair (ScmNumber (ScmRational(42,1)), ScmNumber (ScmRational(42,1))),ScmPair (ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmSymbol "c")),ScmPair (ScmSymbol "d", ScmSymbol "f")))))))};
{name = "TEST_51"; input = ScmPair (ScmSymbol "quote",ScmPair(ScmPair (ScmNumber (ScmReal(42.3)),ScmPair (ScmNumber (ScmReal (-0.2)),ScmPair (ScmNumber (ScmReal(42.3)),ScmPair (ScmNumber (ScmRational(42,1)),ScmPair (ScmNumber (ScmReal(42.3)), ScmPair (ScmNumber (ScmRational(42,1)), ScmNumber (ScmRational(42,1)))))))),ScmNil)); expected = ScmConst(ScmPair (ScmNumber (ScmReal(42.3)),ScmPair (ScmNumber (ScmReal (-0.2)),ScmPair (ScmNumber (ScmReal(42.3)),ScmPair (ScmNumber (ScmRational(42,1)),ScmPair (ScmNumber (ScmReal(42.3)), ScmPair (ScmNumber (ScmRational(42,1)), ScmNumber (ScmRational(42,1)))))))))};
{name = "TEST_52"; input = ScmPair (ScmSymbol "quote",ScmPair(ScmPair (ScmNumber (ScmRational(42,1)),ScmPair (ScmPair (ScmNumber (ScmRational(42,1)), ScmPair (ScmNumber (ScmRational(42,1)), ScmNil)),ScmPair (ScmPair (ScmNumber (ScmRational(42,1)), ScmPair (ScmNumber (ScmReal(42.3)), ScmNil)),ScmPair(ScmPair (ScmSymbol "c",ScmPair (ScmSymbol "d",ScmPair (ScmPair (ScmSymbol "f", ScmSymbol "g"), ScmString "Hello World"))),ScmSymbol "z")))),ScmNil)); expected = ScmConst(ScmPair (ScmNumber (ScmRational(42,1)),ScmPair (ScmPair (ScmNumber (ScmRational(42,1)), ScmPair (ScmNumber (ScmRational(42,1)), ScmNil)),ScmPair (ScmPair (ScmNumber (ScmRational(42,1)), ScmPair (ScmNumber (ScmReal(42.3)), ScmNil)),ScmPair(ScmPair (ScmSymbol "c",ScmPair (ScmSymbol "d",ScmPair (ScmPair (ScmSymbol "f", ScmSymbol "g"), ScmString "Hello World"))),ScmSymbol "z")))))};
{name = "TEST_53"; input = ScmString ""; expected = ScmConst (ScmString "")};


(* {name = "Test_54"; input = ScmPair (ScmSymbol "begin", ScmNil); expected = exn}; *)
{name = "Test_55"; input =  ScmPair (ScmSymbol "begin", ScmPair (ScmSymbol "x", ScmNil)); expected = ScmVar "x"};
{name = "Test_56"; input =  ScmPair(ScmSymbol "begin", ScmPair (ScmSymbol "x", ScmPair (ScmSymbol "y", ScmNil))); expected = ScmSeq [ScmVar "x"; ScmVar "y"]};
(* {name = "Test_57"; input = ScmPair (ScmSymbol "cond", ScmNil); expected = exn};
{name = "Test_58"; input = ScmPair (ScmSymbol "cond", ScmPair (ScmNil, ScmNil)); expected = exn};
{name = "Test_59"; input = ScmPair (ScmSymbol "cond", ScmPair (ScmPair (ScmNil, ScmNil), ScmNil)); expected = exn};
{name = "Test_60"; input = ScmPair(ScmSymbol "cond",ScmPair(ScmPair (ScmPair (ScmSymbol "h?", ScmPair (ScmSymbol "x", ScmNil)), ScmNil),ScmNil)); expected = exn};
{name = "Test_61"; input = ScmPair(ScmSymbol "cond",ScmPair(ScmPair(ScmPair (ScmSymbol "h?", ScmPair (ScmSymbol "x", ScmNil)),ScmPair (ScmSymbol "=>", ScmNil)),ScmNil)); expected = exn}; *)
{name = "Test_62"; input = ScmPair(ScmSymbol "cond",ScmPair(ScmPair (ScmPair (ScmSymbol "h?", ScmPair (ScmSymbol "x", ScmNil)),ScmPair(ScmSymbol "=>", ScmPair(ScmPair (ScmSymbol "p", ScmPair (ScmSymbol "q", ScmNil)), ScmNil))),ScmNil)); expected = ScmApplic(ScmLambdaSimple (["value"; "f"; "rest"],ScmIf (ScmVar "value",ScmApplic (ScmApplic (ScmVar "f", []), [ScmVar "value"]),ScmApplic (ScmVar "rest", []))),[ScmApplic (ScmVar "h?", [ScmVar "x"]);ScmLambdaSimple ([], ScmApplic (ScmVar "p", [ScmVar "q"]));ScmLambdaSimple ([], ScmConst ScmVoid)])};
{name = "Test_63"; input = ScmPair(ScmSymbol "cond", ScmPair(ScmPair(ScmSymbol "else", ScmPair (ScmNumber (ScmRational (1, 1)), ScmNil)),ScmPair(ScmPair(ScmPair (ScmSymbol "eq?", ScmPair(ScmNumber (ScmRational (1, 1)),ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil))),ScmPair (ScmBoolean true, ScmNil)),ScmNil))); expected = ScmConst (ScmNumber (ScmRational (1, 1)))};
(* {name = "Test_64"; input = ScmPair(ScmSymbol "cond", ScmPair (ScmPair (ScmSymbol "else", ScmNil), ScmNil)); expected = exn};
{name = "Test_65"; input = ScmPair (ScmSymbol "lambda", ScmPair (ScmNil, ScmNil)); expected = exn};
{name = "Test_66"; input = ScmPair(ScmSymbol "lambda",ScmPair(ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "a", ScmNil)),ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))); expected = exn};
{name = "Test_67"; input = ScmPair(ScmSymbol "lambda", ScmPair(ScmPair (ScmSymbol "a", ScmSymbol "a"),ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))); expected = exn};
{name = "Test_68"; input = ScmPair(ScmSymbol "lambda",ScmPair(ScmPair(ScmSymbol "a",ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmSymbol "c"))),ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil))); expected = exn}; *)
{name = "Test_69"; input = ScmPair
 (ScmSymbol "cond",
  ScmPair
   (ScmPair
     (ScmPair
       (ScmSymbol ">",
        ScmPair
         (ScmNumber (ScmRational (1, 1)),
          ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil))),
      ScmPair (ScmBoolean true, ScmNil)),
    ScmPair
     (ScmPair
       (ScmPair
         (ScmSymbol ">",
          ScmPair
           (ScmNumber (ScmRational (2, 1)),
            ScmPair (ScmNumber (ScmRational (1, 1)), ScmNil))),
        ScmPair
         (ScmPair
           (ScmSymbol "cond",
            ScmPair
             (ScmPair
               (ScmPair
                 (ScmSymbol "and",
                  ScmPair
                   (ScmNumber (ScmRational (1, 1)),
                    ScmPair
                     (ScmNumber (ScmRational (2, 1)),
                      ScmPair
                       (ScmNumber (ScmRational (3, 1)),
                        ScmPair (ScmBoolean false, ScmNil))))),
                ScmPair
                 (ScmPair
                   (ScmSymbol "let*",
                    ScmPair
                     (ScmPair
                       (ScmPair
                         (ScmSymbol "a",
                          ScmPair (ScmNumber (ScmRational (1, 1)), ScmNil)),
                        ScmPair
                         (ScmPair
                           (ScmSymbol "b", ScmPair (ScmSymbol "a", ScmNil)),
                          ScmNil)),
                      ScmPair
                       (ScmPair
                         (ScmSymbol "+",
                          ScmPair
                           (ScmSymbol "b", ScmPair (ScmSymbol "a", ScmNil))),
                        ScmNil))),
                  ScmNil)),
              ScmPair
               (ScmPair
                 (ScmSymbol "else",
                  ScmPair
                   (ScmNumber (ScmRational (4, 1)),
                    ScmPair
                     (ScmNumber (ScmRational (5, 1)),
                      ScmPair
                       (ScmNumber (ScmRational (6, 1)),
                        ScmPair (ScmNumber (ScmRational (7, 1)), ScmNil))))),
                ScmNil))),
          ScmNil)),
      ScmPair
       (ScmPair
         (ScmPair
           (ScmSymbol "and",
            ScmPair
             (ScmBoolean true,
              ScmPair
               (ScmBoolean true,
                ScmPair (ScmNumber (ScmRational (1, 1)), ScmNil)))),
          ScmPair
           (ScmSymbol "=>",
            ScmPair
             (ScmPair
               (ScmSymbol "display",
                ScmPair (ScmString "hello fucker!", ScmNil)),
              ScmNil))),
        ScmPair
         (ScmPair
           (ScmSymbol "else",
            ScmPair
             (ScmNumber (ScmRational (8, 1)),
              ScmPair
               (ScmNumber (ScmRational (9, 1)),
                ScmPair (ScmNumber (ScmRational (10, 1)), ScmNil)))),
          ScmNil))))) ; 
          expected =
  ScmIf
 (ScmApplic (ScmVar ">",
   [ScmConst (ScmNumber (ScmRational (1, 1)));
    ScmConst (ScmNumber (ScmRational (2, 1)))]),
 ScmConst (ScmBoolean true),
 ScmIf
  (ScmApplic (ScmVar ">",
    [ScmConst (ScmNumber (ScmRational (2, 1)));
     ScmConst (ScmNumber (ScmRational (1, 1)))]),
  ScmIf
   (ScmIf (ScmConst (ScmNumber (ScmRational (1, 1))),
     ScmIf (ScmConst (ScmNumber (ScmRational (2, 1))),
      ScmIf (ScmConst (ScmNumber (ScmRational (3, 1))),
       ScmConst (ScmBoolean false), ScmConst (ScmBoolean false)),
      ScmConst (ScmBoolean false)),
     ScmConst (ScmBoolean false)),
   ScmApplic
    (ScmLambdaSimple (["a"],
      ScmApplic
       (ScmLambdaSimple (["b"],
         ScmApplic (ScmVar "+", [ScmVar "b"; ScmVar "a"])),
       [ScmVar "a"])),
    [ScmConst (ScmNumber (ScmRational (1, 1)))]),
   ScmSeq
    [ScmConst (ScmNumber (ScmRational (4, 1)));
     ScmConst (ScmNumber (ScmRational (5, 1)));
     ScmConst (ScmNumber (ScmRational (6, 1)));
     ScmConst (ScmNumber (ScmRational (7, 1)))]),
  ScmApplic
   (ScmLambdaSimple (["value"; "f"; "rest"],
     ScmIf (ScmVar "value",
      ScmApplic (ScmApplic (ScmVar "f", []), [ScmVar "value"]),
      ScmApplic (ScmVar "rest", []))),
   [ScmIf (ScmConst (ScmBoolean true),
     ScmIf (ScmConst (ScmBoolean true),
      ScmConst (ScmNumber (ScmRational (1, 1))), ScmConst (ScmBoolean false)),
     ScmConst (ScmBoolean false));
    ScmLambdaSimple ([],
     ScmApplic (ScmVar "display", [ScmConst (ScmString "hello fucker!")]));
    ScmLambdaSimple ([],
     ScmSeq
      [ScmConst (ScmNumber (ScmRational (8, 1)));
       ScmConst (ScmNumber (ScmRational (9, 1)));
       ScmConst (ScmNumber (ScmRational (10, 1)))])])))};

{name = "Test_70"; input =ScmPair(ScmSymbol "if", ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil))) ; expected = ScmIf (ScmVar "a", ScmVar "b", ScmConst ScmVoid)};
(* {name = "Test_71"; input = ScmPair(ScmSymbol "define",ScmPair (ScmString "x", ScmPair (ScmNumber (ScmRational (4, 1)), ScmNil))); expected = exn};
{name = "Test_72"; input = ScmPair(ScmSymbol "set!",ScmPair (ScmString "x", ScmPair (ScmNumber (ScmRational (4, 1)), ScmNil))); expected = exn}; *)



 {name = "Boolean"; input = ScmBoolean true;
  expected = ScmConst (ScmBoolean true)};
 {name = "Char"; input = ScmChar 'a'; expected = ScmConst (ScmChar 'a')};
 {name = "String"; input = ScmString "test";
  expected = ScmConst (ScmString "test")};
 {name = "Rational"; input = ScmNumber (ScmRational (1, 2));
  expected = ScmConst (ScmNumber (ScmRational (1, 2)))};
 {name = "Real"; input = ScmNumber (ScmReal 3.1415);
  expected = ScmConst (ScmNumber (ScmReal 3.1415))};
 {name = "Vector";
  input =
   ScmPair
    (ScmSymbol "quote",
     ScmPair
      (ScmPair
        (ScmSymbol "quote", ScmPair (ScmVector [ScmNil; ScmChar '\n'], ScmNil)),
       ScmNil));
  expected =
   ScmConst
    (ScmPair
      (ScmSymbol "quote", ScmPair (ScmVector [ScmNil; ScmChar '\n'], ScmNil)))};
 {name = "Symbol"; input = ScmSymbol "TestSym"; expected = ScmVar "TestSym"};
 {name = "If";
  input =
   ScmPair
    (ScmSymbol "if",
     ScmPair
      (ScmPair (ScmSymbol "a", ScmNil),
       ScmPair (ScmString "then", ScmPair (ScmString "else", ScmNil))));
  expected =
   ScmIf (ScmApplic (ScmVar "a", []), ScmConst (ScmString "then"),
    ScmConst (ScmString "else"))};
 {name = "Seq";
  input =
   ScmPair
    (ScmSymbol "begin",
     ScmPair
      (ScmSymbol "TestVar1",
       ScmPair
        (ScmPair
          (ScmSymbol "if",
           ScmPair (ScmBoolean true, ScmPair (ScmSymbol "TestVar2", ScmNil))),
         ScmNil)));
  expected =
   ScmSeq
    [ScmVar "TestVar1";
     ScmIf (ScmConst (ScmBoolean true), ScmVar "TestVar2", ScmConst ScmVoid)]};
 {name = "Set";
  input =
   ScmPair
    (ScmSymbol "set!",
     ScmPair
      (ScmSymbol "TestVar",
       ScmPair
        (ScmPair
          (ScmSymbol "begin",
           ScmPair
            (ScmPair
              (ScmSymbol "display", ScmPair (ScmSymbol "TestVar", ScmNil)),
             ScmPair
              (ScmPair
                (ScmChar '\n', ScmPair (ScmNumber (ScmReal 0.1), ScmNil)),
               ScmNil))),
         ScmNil)));
  expected =
   ScmSet (ScmVar "TestVar",
    ScmSeq
     [ScmApplic (ScmVar "display", [ScmVar "TestVar"]);
      ScmApplic (ScmConst (ScmChar '\n'), [ScmConst (ScmNumber (ScmReal 0.1))])])};
 {name = "Def";
  input =
   ScmPair
    (ScmSymbol "define",
     ScmPair
      (ScmSymbol "TestVar1",
       ScmPair
        (ScmPair
          (ScmSymbol "set!",
           ScmPair
            (ScmSymbol "TestVar2",
             ScmPair (ScmNumber (ScmRational (-1, 1)), ScmNil))),
         ScmNil)));
  expected =
   ScmDef (ScmVar "TestVar1",
    ScmSet (ScmVar "TestVar2", ScmConst (ScmNumber (ScmRational (-1, 1)))))};
 {name = "Or";
  input =
   ScmPair
    (ScmSymbol "or",
     ScmPair
      (ScmSymbol "a",
       ScmPair
        (ScmPair
          (ScmSymbol "if",
           ScmPair
            (ScmSymbol "a",
             ScmPair (ScmBoolean false, ScmPair (ScmSymbol "a", ScmNil)))),
         ScmNil)));
  expected =
   ScmOr
    [ScmVar "a"; ScmIf (ScmVar "a", ScmConst (ScmBoolean false), ScmVar "a")]};
  {name = "LambdaSimple";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmPair
        (ScmSymbol "var1",
         ScmPair (ScmSymbol "var2", ScmPair (ScmSymbol "var3", ScmNil))),
       ScmPair
        (ScmPair
          (ScmSymbol "if",
           ScmPair
            (ScmPair
              (ScmSymbol "=",
               ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "b", ScmNil))),
             ScmPair
              (ScmPair
                (ScmSymbol "cons",
                 ScmPair
                  (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)),
                   ScmPair
                    (ScmPair
                      (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)),
                     ScmNil))),
               ScmPair
                (ScmPair
                  (ScmSymbol "cons",
                   ScmPair
                    (ScmPair
                      (ScmSymbol "quote", ScmPair (ScmSymbol "a", ScmNil)),
                     ScmPair
                      (ScmPair
                        (ScmSymbol "quote", ScmPair (ScmSymbol "b", ScmNil)),
                       ScmNil))),
                 ScmNil)))),
         ScmNil)));
  expected =
   ScmLambdaSimple (["var1"; "var2"; "var3"],
    ScmIf (ScmApplic (ScmVar "=", [ScmVar "a"; ScmVar "b"]),
     ScmApplic (ScmVar "cons",
      [ScmConst (ScmSymbol "a"); ScmConst (ScmSymbol "a")]),
     ScmApplic (ScmVar "cons",
      [ScmConst (ScmSymbol "a"); ScmConst (ScmSymbol "b")])))};
 {name = "LambdaSimple-implicit-seq";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmPair
        (ScmSymbol "var1",
         ScmPair (ScmSymbol "var2", ScmPair (ScmSymbol "var3", ScmNil))),
       ScmPair
        (ScmSymbol "var1",
         ScmPair (ScmSymbol "var2", ScmPair (ScmSymbol "var3", ScmNil)))));
  expected =
   ScmLambdaSimple (["var1"; "var2"; "var3"],
    ScmSeq [ScmVar "var1"; ScmVar "var2"; ScmVar "var3"])};
 {name = "LambdaOpt";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmPair
        (ScmSymbol "var1", ScmPair (ScmSymbol "Var2", ScmSymbol "OptVar")),
       ScmPair
        (ScmPair
          (ScmSymbol "lambda",
           ScmPair
            (ScmPair (ScmSymbol "Var3", ScmSymbol "OptVar"),
             ScmPair (ScmSymbol "OptVar", ScmNil))),
         ScmNil)));
  expected =
   ScmLambdaOpt (["var1"; "Var2"], "OptVar",
    ScmLambdaOpt (["Var3"], "OptVar", ScmVar "OptVar"))};
 {name = "LambdaOpt-implicit-seq";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmPair
        (ScmSymbol "var1", ScmPair (ScmSymbol "Var2", ScmSymbol "OptVar")),
       ScmPair
        (ScmPair
          (ScmSymbol "lambda",
           ScmPair
            (ScmPair (ScmSymbol "Var3", ScmSymbol "OptVar"),
             ScmPair (ScmSymbol "OptVar", ScmPair (ScmSymbol "Var3", ScmNil)))),
         ScmNil)));
  expected =
   ScmLambdaOpt (["var1"; "Var2"], "OptVar",
    ScmLambdaOpt (["Var3"], "OptVar", ScmSeq [ScmVar "OptVar"; ScmVar "Var3"]))};
  {name = "LambdaVar";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmSymbol "var", ScmPair (ScmPair (ScmSymbol "var", ScmNil), ScmNil)));
  expected = ScmLambdaOpt ([], "var", ScmApplic (ScmVar "var", []))};
 {name = "LambdaVar-implicit-seq";
  input =
   ScmPair
    (ScmSymbol "lambda",
     ScmPair
      (ScmSymbol "var",
       ScmPair
        (ScmPair
          (ScmSymbol "set!",
           ScmPair (ScmSymbol "var", ScmPair (ScmSymbol "+", ScmNil))),
         ScmPair
          (ScmPair
            (ScmSymbol "or",
             ScmPair
              (ScmPair (ScmSymbol "var", ScmNil),
               ScmPair (ScmPair (ScmSymbol "var", ScmNil), ScmNil))),
           ScmNil))));
  expected =
   ScmLambdaOpt ([], "var",
    ScmSeq
     [ScmSet (ScmVar "var", ScmVar "+");
      ScmOr [ScmApplic (ScmVar "var", []); ScmApplic (ScmVar "var", [])]])};
 {name = "Or-nil"; input = ScmPair (ScmSymbol "or", ScmNil);
  expected = ScmConst (ScmBoolean false)};
 {name = "Or-single";
  input = ScmPair (ScmSymbol "or", ScmPair (ScmString "or", ScmNil));
  expected = ScmConst (ScmString "or")};
 {name = "And";
  input =
   ScmPair
    (ScmSymbol "and",
     ScmPair
      (ScmPair (ScmSymbol "or", ScmPair (ScmSymbol "var", ScmNil)),
       ScmPair (ScmBoolean true, ScmPair (ScmSymbol "var", ScmNil))));
  expected =
   ScmIf (ScmVar "var",
    ScmIf (ScmConst (ScmBoolean true), ScmVar "var",
     ScmConst (ScmBoolean false)),
    ScmConst (ScmBoolean false))};
 {name = "MIT-define-var";
  input =
   ScmPair
    (ScmSymbol "define",
     ScmPair
      (ScmPair (ScmSymbol "list", ScmSymbol "l"),
       ScmPair (ScmSymbol "l", ScmNil)));
  expected = ScmDef (ScmVar "list", ScmLambdaOpt ([], "l", ScmVar "l"))};
 {name = "MIT-define-opt";
  input =
   ScmPair
    (ScmSymbol "define",
     ScmPair
      (ScmPair (ScmSymbol "facts", ScmPair (ScmSymbol "n", ScmSymbol "ns")),
       ScmPair
        (ScmPair
          (ScmSymbol "if",
           ScmPair
            (ScmPair (ScmSymbol "zero?", ScmPair (ScmSymbol "n", ScmNil)),
             ScmPair
              (ScmPair
                (ScmSymbol "if",
                 ScmPair
                  (ScmPair
                    (ScmSymbol "null?", ScmPair (ScmSymbol "ns", ScmNil)),
                   ScmPair
                    (ScmNumber (ScmRational (1, 1)),
                     ScmPair
                      (ScmPair
                        (ScmSymbol "apply",
                         ScmPair
                          (ScmSymbol "facts",
                           ScmPair
                            (ScmPair
                              (ScmSymbol "car",
                               ScmPair (ScmSymbol "ns", ScmNil)),
                             ScmPair
                              (ScmPair
                                (ScmSymbol "cdr",
                                 ScmPair (ScmSymbol "ns", ScmNil)),
                               ScmNil)))),
                       ScmNil)))),
               ScmPair
                (ScmPair
                  (ScmSymbol "*",
                   ScmPair
                    (ScmSymbol "n",
                     ScmPair
                      (ScmPair
                        (ScmSymbol "apply",
                         ScmPair
                          (ScmSymbol "facts",
                           ScmPair
                            (ScmPair
                              (ScmSymbol "-",
                               ScmPair
                                (ScmSymbol "n",
                                 ScmPair
                                  (ScmNumber (ScmRational (1, 1)), ScmNil))),
                             ScmPair (ScmSymbol "ns", ScmNil)))),
                       ScmNil))),
                 ScmNil)))),
         ScmNil)));
  expected =
   ScmDef (ScmVar "facts",
    ScmLambdaOpt (["n"], "ns",
     ScmIf (ScmApplic (ScmVar "zero?", [ScmVar "n"]),
      ScmIf (ScmApplic (ScmVar "null?", [ScmVar "ns"]),
       ScmConst (ScmNumber (ScmRational (1, 1))),
       ScmApplic (ScmVar "apply",
        [ScmVar "facts"; ScmApplic (ScmVar "car", [ScmVar "ns"]);
         ScmApplic (ScmVar "cdr", [ScmVar "ns"])])),
      ScmApplic (ScmVar "*",
       [ScmVar "n";
        ScmApplic (ScmVar "apply",
         [ScmVar "facts";
          ScmApplic (ScmVar "-",
           [ScmVar "n"; ScmConst (ScmNumber (ScmRational (1, 1)))]);
          ScmVar "ns"])]))))};
 {name = "MIT-define";
  input =
   ScmPair
    (ScmSymbol "define",
     ScmPair
      (ScmPair (ScmSymbol "foo", ScmPair (ScmSymbol "sym", ScmNil)),
       ScmPair
        (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "sym", ScmNil)),
         ScmNil)));
  expected =
   ScmDef (ScmVar "foo", ScmLambdaSimple (["sym"], ScmConst (ScmSymbol "sym")))};
 {name = "Let";
  input =
   ScmPair
    (ScmSymbol "let",
     ScmPair
      (ScmPair
        (ScmPair
          (ScmSymbol "var1",
           ScmPair
            (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "value1", ScmNil)),
             ScmNil)),
         ScmPair
          (ScmPair
            (ScmSymbol "var2",
             ScmPair
              (ScmPair
                (ScmSymbol "and",
                 ScmPair
                  (ScmSymbol "value2", ScmPair (ScmSymbol "value3", ScmNil))),
               ScmNil)),
           ScmNil)),
       ScmPair (ScmSymbol "body1", ScmPair (ScmSymbol "body2", ScmNil))));
  expected =
   ScmApplic
    (ScmLambdaSimple (["var1"; "var2"],
      ScmSeq [ScmVar "body1"; ScmVar "body2"]),
    [ScmConst (ScmSymbol "value1");
     ScmIf (ScmVar "value2", ScmVar "value3", ScmConst (ScmBoolean false))])};
 {name = "Let*-nil";
  input =
   ScmPair
    (ScmSymbol "let*",
     ScmPair
      (ScmNil,
       ScmPair
        (ScmPair
          (ScmSymbol "and",
           ScmPair
            (ScmSymbol "TestVar1", ScmPair (ScmSymbol "TestVar2", ScmNil))),
         ScmNil)));
  expected =
   ScmApplic
    (ScmLambdaSimple ([],
      ScmIf (ScmVar "TestVar1", ScmVar "TestVar2", ScmConst (ScmBoolean false))),
    [])};
 {name = "Let*";
  input =
   ScmPair
    (ScmSymbol "let*",
     ScmPair
      (ScmPair
        (ScmPair
          (ScmSymbol "a", ScmPair (ScmNumber (ScmRational (1, 1)), ScmNil)),
         ScmPair
          (ScmPair
            (ScmSymbol "b", ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil)),
           ScmPair
            (ScmPair
              (ScmSymbol "c", ScmPair (ScmNumber (ScmRational (3, 2)), ScmNil)),
             ScmNil))),
       ScmPair
        (ScmPair
          (ScmSymbol "+",
           ScmPair
            (ScmSymbol "a",
             ScmPair (ScmSymbol "b", ScmPair (ScmSymbol "c", ScmNil)))),
         ScmNil)));
  expected =
   ScmApplic
    (ScmLambdaSimple (["a"],
      ScmApplic
       (ScmLambdaSimple (["b"],
         ScmApplic
          (ScmLambdaSimple (["c"],
            ScmApplic (ScmVar "+", [ScmVar "a"; ScmVar "b"; ScmVar "c"])),
          [ScmConst (ScmNumber (ScmRational (3, 2)))])),
       [ScmConst (ScmNumber (ScmRational (2, 1)))])),
    [ScmConst (ScmNumber (ScmRational (1, 1)))])};
 {name = "Letrec-nil";
  input =
   ScmPair
    (ScmSymbol "letrec",
     ScmPair
      (ScmNil,
       ScmPair
        (ScmPair (ScmSymbol "a", ScmPair (ScmSymbol "a", ScmNil)), ScmNil)));
  expected =
   ScmApplic (ScmLambdaSimple ([], ScmApplic (ScmVar "a", [ScmVar "a"])), [])};
 {name = "Letrec";
  input =
   ScmPair
    (ScmSymbol "letrec",
     ScmPair
      (ScmPair
        (ScmPair
          (ScmSymbol "fact",
           ScmPair
            (ScmPair
              (ScmSymbol "lambda",
               ScmPair
                (ScmSymbol "n",
                 ScmPair
                  (ScmPair
                    (ScmSymbol "let",
                     ScmPair
                      (ScmPair
                        (ScmPair
                          (ScmSymbol "n",
                           ScmPair
                            (ScmPair
                              (ScmSymbol "car",
                               ScmPair (ScmSymbol "n", ScmNil)),
                             ScmNil)),
                         ScmNil),
                       ScmPair
                        (ScmPair
                          (ScmSymbol "if",
                           ScmPair
                            (ScmPair
                              (ScmSymbol "zero?",
                               ScmPair (ScmSymbol "n", ScmNil)),
                             ScmPair
                              (ScmNumber (ScmRational (1, 1)),
                               ScmPair
                                (ScmPair
                                  (ScmSymbol "*",
                                   ScmPair
                                    (ScmSymbol "n",
                                     ScmPair
                                      (ScmPair
                                        (ScmSymbol "fact",
                                         ScmPair
                                          (ScmPair
                                            (ScmSymbol "-",
                                             ScmPair
                                              (ScmSymbol "n",
                                               ScmPair
                                                (ScmNumber (ScmRational (1, 1)),
                                                 ScmNil))),
                                           ScmNil)),
                                       ScmNil))),
                                 ScmNil)))),
                         ScmNil))),
                   ScmNil))),
             ScmNil)),
         ScmNil),
       ScmPair
        (ScmPair
          (ScmSymbol "fact", ScmPair (ScmNumber (ScmRational (3, 1)), ScmNil)),
         ScmNil)));
  expected =
   ScmApplic
    (ScmLambdaSimple (["fact"],
      ScmSeq
       [ScmSet (ScmVar "fact",
         ScmLambdaOpt ([], "n",
          ScmApplic
           (ScmLambdaSimple (["n"],
             ScmIf (ScmApplic (ScmVar "zero?", [ScmVar "n"]),
              ScmConst (ScmNumber (ScmRational (1, 1))),
              ScmApplic (ScmVar "*",
               [ScmVar "n";
                ScmApplic (ScmVar "fact",
                 [ScmApplic (ScmVar "-",
                   [ScmVar "n"; ScmConst (ScmNumber (ScmRational (1, 1)))])])]))),
           [ScmApplic (ScmVar "car", [ScmVar "n"])])));
        ScmApplic (ScmVar "fact", [ScmConst (ScmNumber (ScmRational (3, 1)))])]),
    [ScmConst (ScmSymbol "whatever")])};
 {name = "Cond";
  input =
   ScmPair
    (ScmSymbol "cond",
     ScmPair
      (ScmPair
        (ScmSymbol "var-a",
         ScmPair (ScmSymbol "var-b", ScmPair (ScmSymbol "var-c", ScmNil))),
       ScmPair
        (ScmPair (ScmSymbol "var-d", ScmPair (ScmSymbol "var-e", ScmNil)),
         ScmPair
          (ScmPair (ScmBoolean false, ScmPair (ScmBoolean true, ScmNil)),
           ScmNil))));
  expected =
   ScmIf (ScmVar "var-a", ScmSeq [ScmVar "var-b"; ScmVar "var-c"],
    ScmIf (ScmVar "var-d", ScmVar "var-e",
     ScmIf (ScmConst (ScmBoolean false), ScmConst (ScmBoolean true),
      ScmConst ScmVoid)))};
 {name = "Cond-else";
  input =
   ScmPair
    (ScmSymbol "cond",
     ScmPair
      (ScmPair
        (ScmSymbol "var-a",
         ScmPair (ScmSymbol "var-b", ScmPair (ScmSymbol "var-c", ScmNil))),
       ScmPair
        (ScmPair (ScmSymbol "var-d", ScmPair (ScmSymbol "var-e", ScmNil)),
         ScmPair
          (ScmPair
            (ScmSymbol "else",
             ScmPair
              (ScmPair
                (ScmSymbol "and",
                 ScmPair (ScmNumber (ScmRational (1, 1)), ScmNil)),
               ScmNil)),
           ScmPair
            (ScmPair (ScmBoolean false, ScmPair (ScmBoolean true, ScmNil)),
             ScmNil)))));
  expected =
   ScmIf (ScmVar "var-a", ScmSeq [ScmVar "var-b"; ScmVar "var-c"],
    ScmIf (ScmVar "var-d", ScmVar "var-e",
     ScmConst (ScmNumber (ScmRational (1, 1)))))};
 {name = "Cond-arrow";
  input =
   ScmPair
    (ScmSymbol "define",
     ScmPair
      (ScmPair (ScmSymbol "foo", ScmPair (ScmSymbol "b", ScmNil)),
       ScmPair
        (ScmPair
          (ScmSymbol "let",
           ScmPair
            (ScmPair
              (ScmPair
                (ScmSymbol "b",
                 ScmPair
                  (ScmPair (ScmSymbol "not", ScmPair (ScmSymbol "b", ScmNil)),
                   ScmNil)),
               ScmNil),
             ScmPair
              (ScmPair
                (ScmSymbol "cond",
                 ScmPair
                  (ScmPair
                    (ScmSymbol "b",
                     ScmPair
                      (ScmSymbol "=>", ScmPair (ScmSymbol "foo", ScmNil))),
                   ScmPair
                    (ScmPair
                      (ScmSymbol "else",
                       ScmPair
                        (ScmPair
                          (ScmSymbol "display",
                           ScmPair (ScmSymbol "b", ScmNil)),
                         ScmNil)),
                     ScmNil))),
               ScmNil))),
         ScmNil)));
  expected =
   ScmDef (ScmVar "foo",
    ScmLambdaSimple (["b"],
     ScmApplic
      (ScmLambdaSimple (["b"],
        ScmApplic
         (ScmLambdaSimple (["value"; "f"; "rest"],
           ScmIf (ScmVar "value",
            ScmApplic (ScmApplic (ScmVar "f", []), [ScmVar "value"]),
            ScmApplic (ScmVar "rest", []))),
         [ScmVar "b"; ScmLambdaSimple ([], ScmVar "foo");
          ScmLambdaSimple ([], ScmApplic (ScmVar "display", [ScmVar "b"]))])),
      [ScmApplic (ScmVar "not", [ScmVar "b"])])))};
 {name = "QQ";
  input = ScmPair (ScmSymbol "quasiquote", ScmPair (ScmSymbol "a", ScmNil));
  expected = ScmConst (ScmSymbol "a")};
 {name = "QQ-nil";
  input = ScmPair (ScmSymbol "quasiquote", ScmPair (ScmNil, ScmNil));
  expected = ScmConst ScmNil};
 {name = "QQ-unquote";
  input =
   ScmPair
    (ScmSymbol "quasiquote",
     ScmPair
      (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "a", ScmNil)), ScmNil));
  expected = ScmVar "a"};
 {name = "QQ-unquote-splicing";
  input =
   ScmPair
    (ScmSymbol "quasiquote",
     ScmPair
      (ScmPair
        (ScmSymbol "unquote-splicing",
         ScmPair
          (ScmPair
            (ScmSymbol "+",
             ScmPair
              (ScmNumber (ScmRational (1, 1)),
               ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil))),
           ScmNil)),
       ScmNil));
  expected =
   ScmConst
    (ScmPair
      (ScmSymbol "unquote-splicing",
       ScmPair
        (ScmPair
          (ScmSymbol "+",
           ScmPair
            (ScmNumber (ScmRational (1, 1)),
             ScmPair (ScmNumber (ScmRational (2, 1)), ScmNil))),
         ScmNil)))};
 {name = "QQ-list";
  input =
   ScmPair
    (ScmSymbol "quasiquote",
     ScmPair
      (ScmPair
        (ScmChar 'a',
         ScmPair
          (ScmSymbol "b",
           ScmPair
            (ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "c", ScmNil)),
             ScmPair
              (ScmPair
                (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "d", ScmNil)),
               ScmPair
                (ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "e", ScmNil)),
                 ScmPair (ScmPair (ScmSymbol "f", ScmNil), ScmNil)))))),
       ScmNil));
  expected =
   ScmApplic (ScmVar "cons",
    [ScmConst (ScmChar 'a');
     ScmApplic (ScmVar "cons",
      [ScmConst (ScmSymbol "b");
       ScmApplic (ScmVar "cons",
        [ScmVar "c";
         ScmApplic (ScmVar "append",
          [ScmVar "d";
           ScmApplic (ScmVar "cons",
            [ScmApplic (ScmVar "cons",
              [ScmConst (ScmSymbol "quote");
               ScmApplic (ScmVar "cons",
                [ScmConst (ScmSymbol "e"); ScmConst ScmNil])]);
             ScmApplic (ScmVar "cons",
              [ScmApplic (ScmVar "cons",
                [ScmConst (ScmSymbol "f"); ScmConst ScmNil]);
               ScmConst ScmNil])])])])])])};
 {name = "QQ-vector";
  input =
   ScmPair
    (ScmSymbol "quasiquote",
     ScmPair
      (ScmVector
        [ScmSymbol "a";
         ScmPair (ScmSymbol "unquote", ScmPair (ScmSymbol "b", ScmNil));
         ScmPair
          (ScmSymbol "unquote-splicing", ScmPair (ScmSymbol "c", ScmNil));
         ScmString "d"],
       ScmNil));
  expected =
   ScmApplic (ScmVar "list->vector",
    [ScmApplic (ScmVar "cons",
      [ScmConst (ScmSymbol "a");
       ScmApplic (ScmVar "cons",
        [ScmVar "b";
         ScmApplic (ScmVar "append",
          [ScmVar "c";
           ScmApplic (ScmVar "cons",
            [ScmConst (ScmString "d"); ScmConst ScmNil])])])])])}
];;



let test_case case =
try
let actual = Tag_Parser.tag_parse_expression case.input in
if (expr_eq actual case.expected) then "PASS" else "FAILURE"
with
| X_syntax_error(s, msg) -> Printf.sprintf "Exception: Syntax Error message: %s for sexpr: %s" msg (string_of_sexpr s)
| X_reserved_word(s) -> Printf.sprintf "Exception: Reserved Word: %s" s
| X_not_implemented -> Printf.sprintf "Exception: Syntax not yet implemented"
| _ -> "Unknown Failure"

let test_cases cases =
let names, results =  (List.map (fun case -> case.name) cases),(List.map test_case cases) in
List.map2 (fun name result -> Printf.sprintf "%s: %s" result name) names results;;

List.map (Printf.printf "%s\n") (test_cases cases);;

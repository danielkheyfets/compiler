
#use "semantic-analyser.ml";;

type 'a test_case = {name: string; test: 'a -> expr' ; input: 'a; expected: expr'}

type case =
| ExprCase of expr test_case
| Expr'Case of expr' test_case

let cases = [
ExprCase {name = "lexical annotation"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmLambdaSimple (["x"],
   ScmApplic (ScmVar "list",
    [ScmApplic (ScmVar "+",
      [ScmVar "x"; ScmConst (ScmNumber (ScmRational (1, 1)))]);
     ScmLambdaSimple (["y"], ScmSet (ScmVar "x", ScmVar "y"))]));
expected =
   ScmLambdaSimple' (["x"],
   ScmApplic' (ScmVar' (VarFree "list"),
    [ScmApplic' (ScmVar' (VarFree "+"),
      [ScmVar' (VarParam ("x", 0)); ScmConst' (ScmNumber (ScmRational (1, 1)))]);
     ScmLambdaSimple' (["y"],
      ScmSet' (VarBound ("x", 0, 0), ScmVar' (VarParam ("y", 0))))]))};

ExprCase {name = "Test_lexical_1"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmLambdaSimple (["x"; "y"],
 ScmLambdaSimple (["z"; "w"],
  ScmLambdaSimple (["u"; "v"],
   ScmApplic (ScmVar "+",
    [ScmVar "x"; ScmVar "y"; ScmVar "z"; ScmVar "w"; ScmVar "u"; ScmVar "v"]))));
expected =
   ScmLambdaSimple' (["x"; "y"], 
 ScmLambdaSimple' (["z"; "w"],
  ScmLambdaSimple' (["u"; "v"],
   ScmApplic' (ScmVar' (VarFree "+"),
    [ScmVar' (VarBound ("x", 1, 0)); ScmVar' (VarBound ("y", 1, 1));
     ScmVar' (VarBound ("z", 0, 0)); ScmVar' (VarBound ("w", 0, 1));
     ScmVar' (VarParam ("u", 0)); ScmVar' (VarParam ("v", 1))]))))};

ExprCase {name = "Test_lexical_2"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmLambdaSimple (["x"],
 ScmSeq
  [ScmVar "x";
   ScmLambdaSimple (["y"],
    ScmApplic (ScmVar "x",
     [ScmVar "y";
      ScmLambdaSimple (["z"], ScmApplic (ScmVar "x", [ScmVar "y"; ScmVar "z"]))]))]);
expected =
   ScmLambdaSimple' (["x"],
 ScmSeq'
  [ScmVar' (VarParam ("x", 0));
   ScmLambdaSimple' (["y"],
    ScmApplic' (ScmVar' (VarBound ("x", 0, 0)),
     [ScmVar' (VarParam ("y", 0));
      ScmLambdaSimple' (["z"],
       ScmApplic' (ScmVar' (VarBound ("x", 1, 0)),
        [ScmVar' (VarBound ("y", 0, 0)); ScmVar' (VarParam ("z", 0))]))]))])};

ExprCase {name = "Test_lexical_3"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmDef (ScmVar "count",
 ScmApplic
  (ScmLambdaSimple (["n"],
    ScmLambdaSimple ([],
     ScmSeq
      [ScmSet (ScmVar "n",
        ScmApplic (ScmVar "+",
         [ScmVar "n"; ScmConst (ScmNumber (ScmRational (1, 1)))]));
       ScmVar "n"])),
  [ScmConst (ScmNumber (ScmRational (0, 1)))]));
expected =
   ScmDef' (VarFree "count",
 ScmApplic'
  (ScmLambdaSimple' (["n"],
    ScmLambdaSimple' ([],
     ScmSeq'
      [ScmSet' (VarBound ("n", 0, 0),
        ScmApplic' (ScmVar' (VarFree "+"),
         [ScmVar' (VarBound ("n", 0, 0));
          ScmConst' (ScmNumber (ScmRational (1, 1)))]));
       ScmVar' (VarBound ("n", 0, 0))])),
  [ScmConst' (ScmNumber (ScmRational (0, 1)))]))};

ExprCase {name = "Test_lexical_4"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmDef (ScmVar "foo",
 ScmLambdaSimple (["q"; "a"; "z"],
  ScmLambdaSimple (["w"; "s"; "x"],
   ScmLambdaSimple (["e"; "d"; "c"],
    ScmLambdaSimple (["r"; "f"; "v"],
     ScmLambdaSimple (["t"; "g"; "b"],
      ScmApplic (ScmVar "+",
       [ScmVar "q"; ScmVar "a"; ScmVar "z"; ScmVar "w"; ScmVar "s"; ScmVar "x";
        ScmVar "e"; ScmVar "d"; ScmVar "c"; ScmVar "r"; ScmVar "f"; ScmVar "v";
        ScmVar "t"; ScmVar "g"; ScmVar "b"])))))));
expected =
   ScmDef' (VarFree "foo",
 ScmLambdaSimple' (["q"; "a"; "z"],
  ScmLambdaSimple' (["w"; "s"; "x"],
   ScmLambdaSimple' (["e"; "d"; "c"],
    ScmLambdaSimple' (["r"; "f"; "v"],
     ScmLambdaSimple' (["t"; "g"; "b"],
      ScmApplic' (ScmVar' (VarFree "+"),
       [ScmVar' (VarBound ("q", 3, 0)); ScmVar' (VarBound ("a", 3, 1));
        ScmVar' (VarBound ("z", 3, 2)); ScmVar' (VarBound ("w", 2, 0));
        ScmVar' (VarBound ("s", 2, 1)); ScmVar' (VarBound ("x", 2, 2));
        ScmVar' (VarBound ("e", 1, 0)); ScmVar' (VarBound ("d", 1, 1));
        ScmVar' (VarBound ("c", 1, 2)); ScmVar' (VarBound ("r", 0, 0));
        ScmVar' (VarBound ("f", 0, 1)); ScmVar' (VarBound ("v", 0, 2));
        ScmVar' (VarParam ("t", 0)); ScmVar' (VarParam ("g", 1));
        ScmVar' (VarParam ("b", 2))])))))))};

ExprCase {name = "Test_lexical_5"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmLambdaSimple (["x"],
 ScmLambdaSimple (["y"; "z"], ScmLambdaSimple (["t"], ScmVar "x")));
expected =
  ScmLambdaSimple' (["x"],
 ScmLambdaSimple' (["y"; "z"],
  ScmLambdaSimple' (["t"], ScmVar' (VarBound ("x", 1, 0)))))};

ExprCase {name = "Test_lexical_6"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmLambdaOpt (["x"], "a",
 ScmLambdaSimple (["y"; "z"],
  ScmLambdaSimple (["t"], ScmSeq [ScmVar "x"; ScmVar "a"])));
expected =
  ScmLambdaOpt' (["x"], "a",    
 ScmLambdaSimple' (["y"; "z"],
  ScmLambdaSimple' (["t"],
   ScmSeq' [ScmVar' (VarBound ("x", 1, 0)); ScmVar' (VarBound ("a", 1, 1))])))};

ExprCase {name = "Test_lexical_7"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmDef (ScmVar "fact",
 ScmLambdaSimple (["n"],
  ScmIf (ScmApplic (ScmVar "zero?", [ScmVar "n"]),
   ScmConst (ScmNumber (ScmRational (1, 1))),
   ScmApplic (ScmVar "*",
    [ScmVar "n";
     ScmApplic (ScmVar "fact",
      [ScmApplic (ScmVar "-",
        [ScmVar "n"; ScmConst (ScmNumber (ScmRational (1, 1)))])])]))));
expected =
  ScmDef' (VarFree "fact", 
 ScmLambdaSimple' (["n"],
  ScmIf'
   (ScmApplic' (ScmVar' (VarFree "zero?"), [ScmVar' (VarParam ("n", 0))]),
   ScmConst' (ScmNumber (ScmRational (1, 1))),
   ScmApplic' (ScmVar' (VarFree "*"),
    [ScmVar' (VarParam ("n", 0));
     ScmApplic' (ScmVar' (VarFree "fact"),
      [ScmApplic' (ScmVar' (VarFree "-"),
        [ScmVar' (VarParam ("n", 0));
         ScmConst' (ScmNumber (ScmRational (1, 1)))])])]))))};

ExprCase {name = "Test_lexical_8"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmLambdaSimple (["x"; "y"],
 ScmLambdaSimple (["z"; "w"],
  ScmOr [ScmVar "x"; ScmVar "y"; ScmVar "z"; ScmVar "w"]));
expected =
  ScmLambdaSimple' (["x"; "y"], 
 ScmLambdaSimple' (["z"; "w"],
  ScmOr'
   [ScmVar' (VarBound ("x", 0, 0)); ScmVar' (VarBound ("y", 0, 1));
    ScmVar' (VarParam ("z", 0)); ScmVar' (VarParam ("w", 1))]))};

ExprCase {name = "Test_lexical_9"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
  ScmLambdaOpt ([], "x",
 ScmLambdaOpt ([], "y", ScmApplic (ScmVar "+", [ScmVar "x"; ScmVar "y"])));
expected =
  ScmLambdaOpt' ([], "x", 
 ScmLambdaOpt' ([], "y",
  ScmApplic' (ScmVar' (VarFree "+"),
   [ScmVar' (VarBound ("x", 0, 0)); ScmVar' (VarParam ("y", 0))])))};

ExprCase {name = "Test_lexical_10"; test = Semantic_Analysis.annotate_lexical_addresses;
input =
 ScmLambdaOpt ([], "x",
 ScmLambdaOpt ([], "y",
  ScmSeq
   [ScmVar "x"; ScmVar "y"; ScmConst (ScmNumber (ScmRational (1, 1)));
    ScmConst (ScmNumber (ScmRational (2, 1)));
    ScmConst (ScmNumber (ScmRational (3, 1)))]));
expected =
  ScmLambdaOpt' ([], "x",
 ScmLambdaOpt' ([], "y",
  ScmSeq'
   [ScmVar' (VarBound ("x", 0, 0)); ScmVar' (VarParam ("y", 0));
    ScmConst' (ScmNumber (ScmRational (1, 1)));
    ScmConst' (ScmNumber (ScmRational (2, 1)));
    ScmConst' (ScmNumber (ScmRational (3, 1)))]))};


Expr'Case {name = "TP annotation"; test = Semantic_Analysis.annotate_tail_calls;
input =
   ScmLambdaSimple' (["x"],
   ScmApplic' (ScmVar' (VarFree "list"),
    [ScmApplic' (ScmVar' (VarFree "+"),
      [ScmVar' (VarParam ("x", 0)); ScmConst' (ScmNumber (ScmRational (1, 1)))]);
     ScmLambdaSimple' (["y"],
      ScmSet' (VarBound ("x", 0, 0), ScmVar' (VarParam ("y", 0))))]));
expected =
   ScmLambdaSimple' (["x"],
    ScmApplicTP' (ScmVar' (VarFree "list"),
     [ScmApplic' (ScmVar' (VarFree "+"),
       [ScmVar' (VarParam ("x", 0)); ScmConst' (ScmNumber (ScmRational (1, 1)))]);
      ScmLambdaSimple' (["y"],
       ScmSet' (VarBound ("x", 0, 0), ScmVar' (VarParam ("y", 0))))]))};

Expr'Case {name = "Test_TP_1"; test = Semantic_Analysis.annotate_tail_calls;
input =
 ScmLambdaSimple' (["x"],
   ScmApplic' (ScmVar' (VarFree "f"),
    [ScmApplic' (ScmVar' (VarFree "g"),
      [ScmApplic' (ScmVar' (VarFree "g"), [ScmVar' (VarParam ("x", 0))])])]));
expected =
  ScmLambdaSimple' (["x"],
 ScmApplicTP' (ScmVar' (VarFree "f"),
  [ScmApplic' (ScmVar' (VarFree "g"),
    [ScmApplic' (ScmVar' (VarFree "g"), [ScmVar' (VarParam ("x", 0))])])]))};

Expr'Case {name = "Test_TP_2"; test = Semantic_Analysis.annotate_tail_calls;
input =
 ScmLambdaSimple' (["x"],
   ScmApplic' (ScmVar' (VarFree "f"),
    [ScmLambdaSimple' (["y"],
      ScmApplic' (ScmVar' (VarFree "g"),
       [ScmVar' (VarBound ("x", 0, 0)); ScmVar' (VarParam ("y", 0))]))]));
expected =
  ScmLambdaSimple' (["x"],
 ScmApplicTP' (ScmVar' (VarFree "f"),
  [ScmLambdaSimple' (["y"],
    ScmApplicTP' (ScmVar' (VarFree "g"),
     [ScmVar' (VarBound ("x", 0, 0)); ScmVar' (VarParam ("y", 0))]))]))};


Expr'Case {name = "Test_TP_3"; test = Semantic_Analysis.annotate_tail_calls;
input =
ScmLambdaSimple' (["x"; "y"; "z"; "w"],
   ScmIf'
    (ScmApplic' (ScmVar' (VarFree "foo?"), [ScmVar' (VarParam ("x", 0))]),
    ScmApplic' (ScmVar' (VarFree "goo"), [ScmVar' (VarParam ("y", 1))]),
    ScmApplic' (ScmVar' (VarFree "boo"),
     [ScmApplic' (ScmVar' (VarFree "doo"), [ScmVar' (VarParam ("z", 2))])])));
expected =
  ScmLambdaSimple' (["x"; "y"; "z"; "w"],
 ScmIf' (ScmApplic' (ScmVar' (VarFree "foo?"), [ScmVar' (VarParam ("x", 0))]),
  ScmApplicTP' (ScmVar' (VarFree "goo"), [ScmVar' (VarParam ("y", 1))]),
  ScmApplicTP' (ScmVar' (VarFree "boo"),
   [ScmApplic' (ScmVar' (VarFree "doo"), [ScmVar' (VarParam ("z", 2))])])))};

Expr'Case {name = "Test_TP_4"; test = Semantic_Analysis.annotate_tail_calls;
input =
ScmLambdaSimple' (["x"; "y"; "z"],
   ScmApplic' (ScmVar' (VarFree "f"),
    [ScmIf'
      (ScmApplic' (ScmVar' (VarFree "g?"), [ScmVar' (VarParam ("x", 0))]),      
      ScmApplic' (ScmVar' (VarFree "h"), [ScmVar' (VarParam ("y", 1))]),        
      ScmApplic' (ScmVar' (VarFree "w"), [ScmVar' (VarParam ("z", 2))]))]));
expected =
  ScmLambdaSimple' (["x"; "y"; "z"],   
 ScmApplicTP' (ScmVar' (VarFree "f"),
  [ScmIf' (ScmApplic' (ScmVar' (VarFree "g?"), [ScmVar' (VarParam ("x", 0))]),
    ScmApplic' (ScmVar' (VarFree "h"), [ScmVar' (VarParam ("y", 1))]),
    ScmApplic' (ScmVar' (VarFree "w"), [ScmVar' (VarParam ("z", 2))]))]))};

Expr'Case {name = "Test_TP_5"; test = Semantic_Analysis.annotate_tail_calls;
input =
ScmLambdaSimple' (["a"; "b"],
   ScmSeq'
    [ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarParam ("a", 0))]);
     ScmApplic' (ScmVar' (VarFree "g"),
      [ScmVar' (VarParam ("a", 0)); ScmVar' (VarParam ("b", 1))]);
     ScmApplic' (ScmVar' (VarFree "display"),
      [ScmConst' (ScmString "done!\n")])]);
expected =
 ScmLambdaSimple' (["a"; "b"],
 ScmSeq'
  [ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarParam ("a", 0))]);
   ScmApplic' (ScmVar' (VarFree "g"),
    [ScmVar' (VarParam ("a", 0)); ScmVar' (VarParam ("b", 1))]);
   ScmApplicTP' (ScmVar' (VarFree "display"),
    [ScmConst' (ScmString "done!\n")])])};


Expr'Case {name = "Test_TP_6"; test = Semantic_Analysis.annotate_tail_calls;
input =
ScmLambdaSimple' ([],
   ScmIf' (ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarFree "x")]),
    ScmIf' (ScmApplic' (ScmVar' (VarFree "g"), [ScmVar' (VarFree "y")]),
     ScmApplic' (ScmVar' (VarFree "h"), [ScmVar' (VarFree "z")]),
     ScmConst' (ScmBoolean false)),
    ScmConst' (ScmBoolean false)));
expected =
 ScmLambdaSimple' ([],
 ScmIf' (ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarFree "x")]),
  ScmIf' (ScmApplic' (ScmVar' (VarFree "g"), [ScmVar' (VarFree "y")]),
   ScmApplicTP' (ScmVar' (VarFree "h"), [ScmVar' (VarFree "z")]),
   ScmConst' (ScmBoolean false)),
  ScmConst' (ScmBoolean false)))};

Expr'Case {name = "Test_TP_7"; test = Semantic_Analysis.annotate_tail_calls;
input =
ScmLambdaSimple' ([],
   ScmOr'
    [ScmApplic' (ScmVar' (VarFree "f"),
      [ScmApplic' (ScmVar' (VarFree "g"), [ScmVar' (VarFree "x")])]);
     ScmVar' (VarFree "y")]);
expected =
 ScmLambdaSimple' ([],
 ScmOr'
  [ScmApplic' (ScmVar' (VarFree "f"),
    [ScmApplic' (ScmVar' (VarFree "g"), [ScmVar' (VarFree "x")])]);
   ScmVar' (VarFree "y")])};

Expr'Case {name = "Test_TP_8"; test = Semantic_Analysis.annotate_tail_calls;
input =
ScmLambdaSimple' ([],
   ScmSet' (VarFree "x",
    ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarFree "y")])));
expected =
 ScmLambdaSimple' ([], 
 ScmSet' (VarFree "x",
  ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarFree "y")])))};

Expr'Case {name = "Test_TP_9"; test = Semantic_Analysis.annotate_tail_calls;
input = ScmLambdaSimple' ([],  
   ScmSet' (VarFree "x",
    ScmApplic' (ScmVar' (VarFree "f"),
     [ScmLambdaSimple' (["y"],
       ScmApplic' (ScmVar' (VarFree "g"),
        [ScmVar' (VarFree "x"); ScmVar' (VarParam ("y", 0))]))])));
expected = ScmLambdaSimple' ([], 
 ScmSet' (VarFree "x",
  ScmApplic' (ScmVar' (VarFree "f"),
   [ScmLambdaSimple' (["y"],
     ScmApplicTP' (ScmVar' (VarFree "g"),
      [ScmVar' (VarFree "x"); ScmVar' (VarParam ("y", 0))]))])))};


Expr'Case {name = "Test_TP_10"; test = Semantic_Analysis.annotate_tail_calls;
input = ScmLambdaSimple' (["x"; "y"; "z"],
   ScmIf' (ScmApplic' (ScmVar' (VarFree "f?"), [ScmVar' (VarParam ("x", 0))]),
    ScmApplic' (ScmVar' (VarFree "g"), [ScmVar' (VarParam ("y", 1))]),
    ScmIf' (ScmApplic' (ScmVar' (VarFree "g?"), [ScmVar' (VarParam ("x", 0))]),
     ScmSeq'
      [ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarParam ("x", 0))]);
       ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarParam ("y", 1))])],
     ScmSeq'
      [ScmApplic' (ScmVar' (VarFree "h"), [ScmVar' (VarParam ("x", 0))]);
       ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarParam ("y", 1))]);
       ScmApplic' (ScmVar' (VarFree "g"),
        [ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarParam ("x", 0))])])])));
expected = ScmLambdaSimple' (["x"; "y"; "z"],
 ScmIf' (ScmApplic' (ScmVar' (VarFree "f?"), [ScmVar' (VarParam ("x", 0))]),
  ScmApplicTP' (ScmVar' (VarFree "g"), [ScmVar' (VarParam ("y", 1))]),
  ScmIf' (ScmApplic' (ScmVar' (VarFree "g?"), [ScmVar' (VarParam ("x", 0))]),
   ScmSeq'
    [ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarParam ("x", 0))]);
     ScmApplicTP' (ScmVar' (VarFree "f"), [ScmVar' (VarParam ("y", 1))])],
   ScmSeq'
    [ScmApplic' (ScmVar' (VarFree "h"), [ScmVar' (VarParam ("x", 0))]);
     ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarParam ("y", 1))]);
     ScmApplicTP' (ScmVar' (VarFree "g"),
      [ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarParam ("x", 0))])])])))};

Expr'Case {name = "Test_TP_11"; test = Semantic_Analysis.annotate_tail_calls;
input = ScmApplic'
   (ScmLambdaSimple' (["x"; "y"],
     ScmApplic' (ScmVar' (VarFree "goo"),
      [ScmApplic' (ScmVar' (VarFree "boo"), [ScmVar' (VarParam ("x", 0))]);
       ScmVar' (VarParam ("y", 1))])),
   [ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarFree "y")]);
    ScmApplic' (ScmVar' (VarFree "g"), [ScmVar' (VarFree "x")])]);
expected = ScmApplic'
 (ScmLambdaSimple' (["x"; "y"],
   ScmApplicTP' (ScmVar' (VarFree "goo"),
    [ScmApplic' (ScmVar' (VarFree "boo"), [ScmVar' (VarParam ("x", 0))]);
     ScmVar' (VarParam ("y", 1))])),
 [ScmApplic' (ScmVar' (VarFree "f"), [ScmVar' (VarFree "y")]);
  ScmApplic' (ScmVar' (VarFree "g"), [ScmVar' (VarFree "x")])]) };

Expr'Case {name = "Test_TP_12"; test = Semantic_Analysis.annotate_tail_calls;
input =ScmIf'
   (ScmApplic'
     (ScmLambdaSimple' ([],
       ScmApplic' (ScmVar' (VarFree "foo"), [ScmVar' (VarFree "x")])),
     []),
   ScmConst' (ScmNumber (ScmRational (4, 1))),
   ScmConst' (ScmNumber (ScmRational (5, 1)))) ;
expected =ScmIf'      
 (ScmApplic'
   (ScmLambdaSimple' ([],
     ScmApplicTP' (ScmVar' (VarFree "foo"), [ScmVar' (VarFree "x")])),
   []),
 ScmConst' (ScmNumber (ScmRational (4, 1))),
 ScmConst' (ScmNumber (ScmRational (5, 1))))};

Expr'Case {name = "Test_TP_13"; test = Semantic_Analysis.annotate_tail_calls;
input = ScmIf' (ScmApplic' (ScmVar' (VarFree "foo"), [ScmVar' (VarFree "x")]),
   ScmLambdaSimple' ([],
    ScmApplic' (ScmVar' (VarFree "goo"),
     [ScmConst' (ScmNumber (ScmRational (5, 1)))])),
   ScmLambdaSimple' ([],
    ScmApplic' (ScmVar' (VarFree "hoo"),
     [ScmConst' (ScmNumber (ScmRational (6, 1)))])));
expected = ScmIf' (ScmApplic' (ScmVar' (VarFree "foo"), [ScmVar' (VarFree "x")]),
 ScmLambdaSimple' ([],
  ScmApplicTP' (ScmVar' (VarFree "goo"),
   [ScmConst' (ScmNumber (ScmRational (5, 1)))])),
 ScmLambdaSimple' ([],
  ScmApplicTP' (ScmVar' (VarFree "hoo"),
   [ScmConst' (ScmNumber (ScmRational (6, 1)))])))};

 Expr'Case {name = "Test_TP_14"; test = Semantic_Analysis.annotate_tail_calls;
input = ScmLambdaSimple' ([],
   ScmSeq'
    [ScmLambdaSimple' ([],
      ScmApplic' (ScmVar' (VarFree "foo"),
       [ScmConst' (ScmNumber (ScmRational (5, 1)))]));
     ScmApplic' (ScmVar' (VarFree "what?"), [ScmVar' (VarFree "x")]);
     ScmLambdaSimple' ([],
      ScmApplic' (ScmVar' (VarFree "hoo"),
       [ScmConst' (ScmNumber (ScmRational (7, 1)))]));
     ScmApplic' (ScmVar' (VarFree "doo"), [])]);
expected = ScmLambdaSimple' ([],
 ScmSeq'
  [ScmLambdaSimple' ([],
    ScmApplicTP' (ScmVar' (VarFree "foo"),
     [ScmConst' (ScmNumber (ScmRational (5, 1)))]));
   ScmApplic' (ScmVar' (VarFree "what?"), [ScmVar' (VarFree "x")]);
   ScmLambdaSimple' ([],
    ScmApplicTP' (ScmVar' (VarFree "hoo"),
     [ScmConst' (ScmNumber (ScmRational (7, 1)))]));
   ScmApplicTP' (ScmVar' (VarFree "doo"), [])])}; 

 Expr'Case {name = "Test_TP_15"; test = Semantic_Analysis.annotate_tail_calls;
input = ScmLambdaSimple' ([],
   ScmOr'
    [ScmApplic'
      (ScmLambdaSimple' ([], ScmApplic' (ScmVar' (VarFree "foo"), [])),
      []);
     ScmApplic' (ScmVar' (VarFree "not-tail?"), [ScmVar' (VarFree "x")]);
     ScmApplic' (ScmVar' (VarFree "goo"), [ScmVar' (VarFree "x")])]);
expected = ScmLambdaSimple' ([],
 ScmOr'
  [ScmApplic'
    (ScmLambdaSimple' ([], ScmApplicTP' (ScmVar' (VarFree "foo"), [])),
    []);
   ScmApplic' (ScmVar' (VarFree "not-tail?"), [ScmVar' (VarFree "x")]);
   ScmApplicTP' (ScmVar' (VarFree "goo"), [ScmVar' (VarFree "x")])])}; 
 
 Expr'Case {name = "Test_TP_16"; test = Semantic_Analysis.annotate_tail_calls;
input = ScmDef' (VarFree "x",
   ScmLambdaSimple' ([], ScmApplic' (ScmVar' (VarFree "foo"), [])));
expected = ScmDef' (VarFree "x",
 ScmLambdaSimple' ([], ScmApplicTP' (ScmVar' (VarFree "foo"), [])))}; 

 Expr'Case {name = "Test_TP_17"; test = Semantic_Analysis.annotate_tail_calls;
input = ScmLambdaSimple' (["s"],
   ScmApplic' (ScmVar' (VarFree "apply"),
    [ScmVar' (VarFree "f"); ScmVar' (VarParam ("s", 0))]));
expected = ScmLambdaSimple' (["s"],
 ScmApplicTP' (ScmVar' (VarFree "apply"),
  [ScmVar' (VarFree "f"); ScmVar' (VarParam ("s", 0))]))}; 

Expr'Case {name = "box"; test = Semantic_Analysis.box_set;
input =
   ScmLambdaSimple' (["x"],
       ScmApplicTP' (ScmVar' (VarFree "list"),
        [ScmApplic' (ScmVar' (VarFree "+"),
          [ScmVar' (VarParam ("x", 0)); ScmConst' (ScmNumber (ScmRational (1, 1)))]);
         ScmLambdaSimple' (["y"],
          ScmSet' (VarBound ("x", 0, 0), ScmVar' (VarParam ("y", 0))))]));
expected =
  ScmLambdaSimple' (["x"],
   ScmSeq'
    [ScmSet' (VarParam ("x", 0), ScmBox' (VarParam ("x", 0)));
     ScmApplicTP' (ScmVar' (VarFree "list"),
      [ScmApplic' (ScmVar' (VarFree "+"),
        [ScmBoxGet' (VarParam ("x", 0));
         ScmConst' (ScmNumber (ScmRational (1, 1)))]);
       ScmLambdaSimple' (["y"],
        ScmBoxSet' (VarBound ("x", 0, 0), ScmVar' (VarParam ("y", 0))))])])};
];;


let var_eq v1 v2 =
match v1, v2 with
  | VarFree (name1), VarFree (name2) -> String.equal name1 name2
  | VarBound (name1, major1, minor1), VarBound (name2, major2, minor2) ->
    major1 = major2 && minor1 = minor2 && (String.equal name1 name2)
  | VarParam (name1, index1), VarParam (name2, index2) ->
       index1 = index2 && (String.equal name1 name2)
  | _ -> false

let list_eq eq l1 l2 = (List.length l1) = (List.length l2) && List.for_all2 eq l1 l2;;

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | ScmConst' (sexpr1), ScmConst' (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar' (var1), ScmVar' (var2) -> var_eq var1 var2
  | ScmIf' (test1, dit1, dif1), ScmIf' (test2, dit2, dif2) -> (expr'_eq test1 test2) &&
                                            (expr'_eq dit1 dit2) &&
                                              (expr'_eq dif1 dif2)
  | (ScmSeq' (exprs1), ScmSeq' (exprs2) | ScmOr' (exprs1), ScmOr' (exprs2)) ->
        list_eq expr'_eq exprs1 exprs2
  | (ScmSet' (var1, val1), ScmSet' (var2, val2) | ScmDef' (var1, val1), ScmDef' (var2, val2)) ->
        (var_eq var1 var2) && (expr'_eq val1 val2)
  | ScmLambdaSimple' (vars1, body1), ScmLambdaSimple' (vars2, body2) ->
     (list_eq String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmLambdaOpt' (vars1, var1, body1), ScmLambdaOpt' (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (list_eq String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmApplic' (e1, args1), ScmApplic' (e2, args2) ->
     (expr'_eq e1 e2) && (list_eq expr'_eq args1 args2)
  | ScmApplicTP' (e1, args1), ScmApplicTP' (e2, args2) ->
      (expr'_eq e1 e2) && (list_eq expr'_eq args1 args2)
  | ScmBox' (v1), ScmBox' (v2) -> var_eq v1 v2
  | ScmBoxGet' (v1), ScmBoxGet' (v2) -> var_eq v1 v2
  | ScmBoxSet' (v1, e1), ScmBoxSet' (v2, e2) -> (var_eq v1 v2) && (expr'_eq e1 e2)
  | _ -> false;;

let unannotate_lexical_address = function
| (VarFree name | VarParam (name, _) | VarBound (name, _, _)) -> ScmVar name

let rec unanalyze expr' =
match expr' with
  | ScmConst' s -> ScmConst(s)
  | ScmVar' var -> unannotate_lexical_address var
  | ScmBox' var -> ScmApplic(ScmVar "box", [unannotate_lexical_address var])
  | ScmBoxGet' var -> unannotate_lexical_address var
  | ScmBoxSet' (var, expr') -> ScmSet (unannotate_lexical_address var, unanalyze expr')
  | ScmIf' (test, dit, dif) -> ScmIf (unanalyze test, unanalyze dit, unanalyze dif)
  | ScmSeq' expr's -> ScmSeq (List.map unanalyze expr's)
  | ScmSet' (var, expr') -> ScmSet (unannotate_lexical_address var, unanalyze expr')
  | ScmDef' (var, expr') -> ScmDef (unannotate_lexical_address var, unanalyze expr')
  | ScmOr' expr's -> ScmOr (List.map unanalyze expr's)
  | ScmLambdaSimple' (params, expr') ->
        ScmLambdaSimple (params, unanalyze expr')
  | ScmLambdaOpt'(params, param, expr') ->
        ScmLambdaOpt (params, param, unanalyze expr')
  | (ScmApplic' (expr', expr's) | ScmApplicTP' (expr', expr's)) ->
        ScmApplic (unanalyze expr', List.map unanalyze expr's);;

let string_of_expr' expr' =
    string_of_expr (unanalyze expr');;

let case_name case =
match case with
| ExprCase c -> Printf.sprintf "Expr-%s" c.name
| Expr'Case c -> Printf.sprintf "Expr'-%s" c.name

let test_case case =

try
let actual, expected = match case with
| ExprCase c -> (c.test c.input), c.expected
| Expr'Case c -> (c.test c.input), c.expected in
if (expr'_eq actual expected) then "PASS" else "FAILURE"
with
| X_not_yet_implemented -> Printf.sprintf "Exception: Syntax not yet implemented"
| _ -> "Unknown Failure"

let test_cases cases =
let names, results =  (List.map case_name cases),(List.map test_case cases) in
List.map2 (fun name result -> Printf.sprintf "%s: %s" result name) names results;;

List.map (Printf.printf "%s\n") (test_cases cases);;

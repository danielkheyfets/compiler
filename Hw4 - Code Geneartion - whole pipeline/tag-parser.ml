#use "reader.ml";;

type expr =
  | ScmConst of sexpr
  | ScmVar of string
  | ScmIf of expr * expr * expr
  | ScmSeq of expr list
  | ScmSet of expr * expr
  | ScmDef of expr * expr
  | ScmOr of expr list
  | ScmLambdaSimple of string list * expr
  | ScmLambdaOpt of string list * string * expr
  | ScmApplic of expr * (expr list);;

exception X_syntax_error of sexpr * string;;
exception X_reserved_word of string;;
exception X_proper_list_error;;
exception X_not_implemented;;

let rec list_to_proper_list = function
| [] -> ScmNil
| hd::[] -> ScmPair (hd, ScmNil)
| hd::tl -> ScmPair (hd, list_to_proper_list tl);;

let rec list_to_improper_list = function
| [] -> raise X_proper_list_error
| hd::[] -> hd
| hd::tl -> ScmPair (hd, list_to_improper_list tl);;

let rec scm_append scm_list sexpr =
match scm_list with
| ScmNil -> sexpr
| ScmPair (car, cdr) -> ScmPair (car, scm_append cdr sexpr)
| _ -> raise (X_syntax_error (scm_list, "Append expects a proper list"))

let rec scm_map f sexpr =
match sexpr with
| ScmNil -> ScmNil
| ScmPair (car, cdr) -> ScmPair (f car, scm_map f cdr)
| _ -> raise (X_syntax_error (sexpr, "Map expects a list"));;

let rec scm_zip f sexpr1 sexpr2 =
match sexpr1, sexpr2 with
| ScmNil, ScmNil -> ScmNil
| ScmPair (car1, cdr1), ScmPair (car2, cdr2) -> ScmPair (f car1 car2, scm_zip f cdr1 cdr2)
| _, _ ->
    let sepxrs = list_to_proper_list [ScmSymbol "sexpr1:"; sexpr1; ScmSymbol "sexpr2:"; sexpr2] in
    raise (X_syntax_error (sepxrs, "Zip expects 2 lists of equal length"));;

let rec scm_list_to_list = function
| ScmPair (hd, tl) -> hd::(scm_list_to_list tl)
| ScmNil -> []
| sexpr -> raise (X_syntax_error (sexpr, "Expected proper list"));;

let rec scm_is_list = function
| ScmPair (hd, tl) -> scm_is_list tl
| ScmNil -> true
| _ -> false

let rec scm_list_length = function
| ScmPair (hd, tl) -> 1 + (scm_list_length tl)
| ScmNil -> 0
| sexpr -> raise (X_syntax_error (sexpr, "Expected proper list"));;

let rec untag expr =
let untag_vars vars = List.map (fun s -> ScmSymbol s) vars in
let untag_tagged_list tag exprs = list_to_proper_list (ScmSymbol tag::(List.map untag exprs)) in

let untag_lambda_opt vars var body =
let vars = match vars with
| [] -> ScmSymbol var
| _ -> list_to_improper_list (untag_vars (vars@[var])) in
list_to_proper_list ([ScmSymbol "lambda"; vars]@body) in

match expr with
| (ScmConst (ScmSymbol(_) as sexpr)
    | ScmConst (ScmNil as sexpr)
    | ScmConst (ScmPair (_, _) as sexpr)) -> list_to_proper_list [ScmSymbol "quote"; sexpr]
| ScmConst s -> s
| ScmVar (name) -> ScmSymbol(name)
| ScmIf (test, dit, ScmConst (ScmVoid)) -> untag_tagged_list "if" [test; dit]
| ScmIf (test, dit, dif) -> untag_tagged_list "if" [test; dit; dif]
| ScmSeq(exprs) -> untag_tagged_list "begin" exprs
| ScmSet (var, value) -> untag_tagged_list "set!" [var; value]
| ScmDef (var, value) -> untag_tagged_list "define" [var; value]
| ScmOr (exprs) -> untag_tagged_list "or" exprs
| ScmLambdaSimple (vars, ScmSeq(body)) ->
    let vars = list_to_proper_list (untag_vars vars) in
    let body = List.map untag body in
    list_to_proper_list ([ScmSymbol "lambda"; vars]@body)
| ScmLambdaSimple (vars, body) ->
    let vars = list_to_proper_list (untag_vars vars) in
    list_to_proper_list ([ScmSymbol "lambda"; vars; untag body])
| ScmLambdaOpt (vars, var, ScmSeq(body)) ->
    let body = List.map untag body in
    untag_lambda_opt vars var body
| ScmLambdaOpt (vars, var, body) ->
    let body = [untag body] in
    untag_lambda_opt vars var body
| ScmApplic(procedure, args) -> list_to_proper_list (List.map untag (procedure::args));;


let rec string_of_expr expr =
string_of_sexpr (untag expr)

let scm_number_eq n1 n2 =
match n1, n2 with
| ScmRational(numerator1, denominator1), ScmRational(numerator2, denominator2) ->
        numerator1 = numerator2 && denominator1 = denominator2
| ScmReal(real1), ScmReal(real2) -> abs_float(real1 -. real2) < 0.001
| _, _ -> false

let rec sexpr_eq s1 s2 =
match s1, s2 with
| (ScmVoid, ScmVoid) | (ScmNil, ScmNil)  -> true
| ScmBoolean(bool1), ScmBoolean(bool2) -> bool1 = bool2
| ScmChar(char1), ScmChar(char2) -> char1 = char2
| ScmString(string1), ScmString(string2) -> String.equal string1 string2
| ScmSymbol(symbol1), ScmSymbol(symbol2) -> String.equal symbol1 symbol2
| ScmNumber(number1), ScmNumber(number2) -> scm_number_eq number1 number2
| ScmVector(sexprs1), ScmVector(sexprs2) -> List.for_all2 sexpr_eq sexprs1 sexprs2
| ScmPair(car1, cdr1), ScmPair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
| _, _ -> false

let rec expr_eq e1 e2 =
  match e1, e2 with
  | ScmConst (sexpr1), ScmConst (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar (var1), ScmVar (var2) -> String.equal var1 var2
  | ScmIf (test1, dit1, dif1), ScmIf (test2, dit2, dif2) -> (expr_eq test1 test2) &&
                                            (expr_eq dit1 dit2) &&
                                              (expr_eq dif1 dif2)
  | (ScmSeq(exprs1), ScmSeq(exprs2) | ScmOr (exprs1), ScmOr (exprs2)) ->
        List.for_all2 expr_eq exprs1 exprs2
  | (ScmSet (var1, val1), ScmSet (var2, val2) | ScmDef (var1, val1), ScmDef (var2, val2)) ->
        (expr_eq var1 var2) && (expr_eq val1 val2)
  | ScmLambdaSimple (vars1, body1), ScmLambdaSimple (vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) && (expr_eq body1 body2)
  | ScmLambdaOpt (vars1, var1, body1), ScmLambdaOpt (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) && (expr_eq body1 body2)
  | ScmApplic (e1, args1), ScmApplic (e2, args2) ->
     (expr_eq e1 e2) && (List.for_all2 expr_eq args1 args2)
  | _ -> false;;

module type TAG_PARSER = sig
  val tag_parse_expression : sexpr -> expr
end;; 

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "unquote";
   "unquote-splicing"];;
  


let rec dup_exist = function
    | [] -> (false,"")
    | hd::tl -> if (List.exists ((=) hd) tl) then (true,hd) else dup_exist tl

let rec parameter_list sexpr = 
    match sexpr with
    | ScmPair(ScmSymbol(x),ScmSymbol(y)) -> [x]
    | ScmPair(ScmSymbol(x),ScmPair(y)) -> [x] @ (parameter_list (ScmPair y))
    | ScmPair(x,y) -> raise (X_syntax_error(sexpr, "arguments are not all symbols"))
    | _ -> raise (X_syntax_error(sexpr, "Invalid syntax to improper list arguments"))

let rec optional_args sexpr = 
        match sexpr with
    | ScmPair(ScmSymbol(x),ScmSymbol(y)) -> y
    | ScmPair(ScmSymbol(x),ScmPair(y)) -> (optional_args (ScmPair y))
    | _ -> raise (X_syntax_error(sexpr, "Invalid syntax to improper list arguments"))


let rec tag_parse_expression sexpr =
  let sexpr = macro_expand sexpr in
  match sexpr with 
  | ScmNil -> ScmConst(ScmNil)
  | ScmVoid -> ScmConst(ScmVoid)
  | ScmBoolean(x) -> ScmConst(ScmBoolean(x))
  | ScmChar(x) -> ScmConst(ScmChar(x))
  | ScmNumber(x) -> ScmConst(ScmNumber(x))
  | ScmString(x) -> ScmConst(ScmString(x))
  | ScmPair(ScmSymbol("quote"),ScmPair(x,ScmNil)) -> ScmConst(x)
  | ScmSymbol(x) -> if (List.mem x reserved_word_list) then raise (X_reserved_word x) else ScmVar(x)
  | ScmPair(ScmSymbol("if"), x) -> tag_if x
  | ScmPair(ScmSymbol("or"), x) -> tag_or x
  | ScmPair(ScmSymbol("lambda"), x)-> tag_lambda x
  | ScmPair(ScmSymbol("define"), x)-> tag_define x
  | ScmPair(ScmSymbol("set!"), x)-> tag_set x
  | ScmPair(ScmSymbol("begin"), x)-> tag_begin x
  | ScmPair(x,y) -> tag_applic x y
  | _ -> raise (X_syntax_error (sexpr, "Sexpr structure not recognized"))

and tag_begin sexpr = 
  let lst = if ((scm_list_length sexpr) == 0) then raise (X_syntax_error(sexpr, "Invalid begin - empty sequence")) else scm_list_to_list sexpr in
  let lst = List.map tag_parse_expression lst in
  if ((List.length lst) == 1) then (List.hd lst) else ScmSeq(lst)

and tag_set sexpr = 
  match sexpr with
  | ScmPair(ScmSymbol(x), ScmPair(value,ScmNil)) -> ScmSet((tag_parse_expression (ScmSymbol(x))), (tag_parse_expression value))
  | ScmPair(ScmSymbol(x), value) -> ScmSet((tag_parse_expression (ScmSymbol(x))), (tag_parse_expression value))
  | _ -> raise (X_syntax_error (sexpr, "Expected variable on LHS of set!"))


and tag_applic first rest = 
  let e1 = tag_parse_expression first in
  let en = scm_list_to_list rest in
  let en = List.map tag_parse_expression en in
  ScmApplic(e1, en)

and tag_define sexpr = match sexpr with
  | ScmPair(ScmSymbol(x),ScmPair(value,ScmNil)) -> ScmDef( (tag_parse_expression (ScmSymbol x)), tag_parse_expression value)
  | _ ->  raise (X_syntax_error (sexpr, "Expected variable on LHS of define or missing data on RHS"))

and tag_lambda sexpr = match sexpr with
  | ScmPair(ScmSymbol(x),body) -> tag_variadic_lambda x body
  | ScmPair(args,body) -> if (scm_is_list args) then (tag_simple_lambda args body) else (tag_optional_lambda args body)
  | _ -> raise (X_syntax_error (sexpr, "Sexpr structure not recognized")) 

and body_to_exp body = 
  let b = scm_list_to_list body in
  let exp_body = match b with
    | [] -> raise (X_syntax_error (body, "lambda sexpr has empty body"))
    | [exp_0] -> [tag_parse_expression exp_0]
    | _-> List.map (fun a -> tag_parse_expression a) b in
  exp_body

and tag_variadic_lambda arg body =
    let exp_body = body_to_exp body in
    let my_variadic_lambda = if (List.length exp_body) == 1 then ScmLambdaOpt([],arg,List.hd exp_body) else ScmLambdaOpt([],arg,ScmSeq(exp_body)) in
    my_variadic_lambda

and tag_simple_lambda args body = 
  let args = scm_list_to_list args in 
  let sargs = List.map (fun a -> match a with
                            | ScmSymbol(x) -> x
                            | _ -> raise (X_syntax_error (a, "Sexpr not a symbol"))) args in
  let (isDupe, arg) = dup_exist sargs in
  let exp_body = if (isDupe) then (raise (X_syntax_error(ScmSymbol(arg),"Duplicates in args"))) else (body_to_exp body) in
  let my_simple_lambda = if (List.length exp_body) == 1 then ScmLambdaSimple(sargs,List.hd exp_body) else ScmLambdaSimple(sargs,ScmSeq(exp_body)) in
  my_simple_lambda
  
and tag_optional_lambda args body =
  let sargs = parameter_list args in
  let (isDup,arg) = dup_exist sargs in
  let op_arg = optional_args args in
  let has_dupe = isDup || (List.mem op_arg sargs) in
  let exp_body = if (has_dupe) then (raise (X_syntax_error(ScmSymbol(arg),"Duplicates in args"))) else (body_to_exp body) in
  let my_opt_lambda =  if (List.length exp_body) == 1 then ScmLambdaOpt(sargs,op_arg,List.hd exp_body) else ScmLambdaOpt(sargs,op_arg,ScmSeq(exp_body)) in
  my_opt_lambda

and tag_if sexpr = match sexpr with
  | ScmPair(test,ScmPair(dit,ScmPair(dif,ScmNil))) -> ScmIf(tag_parse_expression test, tag_parse_expression dit, tag_parse_expression dif)
  | ScmPair(test,ScmPair(dit,ScmNil)) -> ScmIf(tag_parse_expression test, tag_parse_expression dit, ScmConst(ScmVoid))
  | _ -> raise (X_syntax_error (sexpr, "Sexpr structure not recognized")) 

and tag_or sexpr = match sexpr with
  | ScmNil -> ScmConst(ScmBoolean false)
  | ScmPair(x,ScmNil) -> tag_parse_expression x
  | ScmPair(x) -> ScmOr(List.map tag_parse_expression (scm_list_to_list (ScmPair(x))))
  | _ -> raise (X_syntax_error (sexpr, "Sexpr structure not recognized")) 

and expand_let binds body =
  let f_names = (function | (ScmPair(var,value))-> var | _ -> raise (X_syntax_error(binds,"Invalid let expression"))) in
  let f_vals = (function  |  (ScmPair(var,ScmPair(value,ScmNil)))-> value | _ -> raise (X_syntax_error(binds,"Invalid let expression"))) in
  let params = scm_map f_names binds in
  let args  =  scm_map f_vals binds in
  ScmPair(ScmPair(ScmSymbol("lambda"),ScmPair(params,body)),args)

and expand_letrec binds body = 
  let init = ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol "whatever", ScmNil)) in
  let f_names_only = (function | (ScmPair(var,value))-> var | _ -> raise (X_syntax_error(binds,"Invalid let expression"))) in
  let f_set = (fun name_i exp_i -> match name_i, exp_i with
                 | ScmSymbol(n), ex -> ScmPair(ScmSymbol("set!"), ScmPair(ScmSymbol(n),ScmPair(ex,ScmNil))) | _ -> raise (X_syntax_error(binds,"Invalid letrec expression"))) in
  let f_names_init = (function | (ScmPair(var,value))-> ScmPair(var,ScmPair(init,ScmNil)) | _ -> raise (X_syntax_error(binds,"Invalid let expression"))) in
  let f_vals = (function  |  (ScmPair(var,ScmPair(value,ScmNil)))-> value | _ -> raise (X_syntax_error(binds,"Invalid let expression"))) in
  let my_binds = scm_map f_names_init binds in
  let names = scm_map f_names_only binds in
  let values = scm_map f_vals binds in
  let set_expr_i  =  scm_zip f_set names values in
  let my_body = scm_append set_expr_i body in
  ScmPair(ScmSymbol("let"),ScmPair(my_binds,my_body))

and expand_define var args body = 
  let value = ScmPair(ScmSymbol("lambda"),ScmPair(args,body)) in
  ScmPair(ScmSymbol("define"), ScmPair(var,ScmPair(value,ScmNil)))

and make_named_lambda name body = 
  ScmPair(ScmSymbol name, ScmPair (ScmPair(ScmSymbol "lambda",ScmPair (ScmNil,body)),ScmNil))

and expand_cond rib = match rib with
  | ScmNil -> ScmVoid
  | ScmPair(ScmPair(test,ScmPair(ScmSymbol("=>"),body)),rest) -> if ((scm_list_length body) == 0) then raise (X_syntax_error(rib,"empty body after arrow")) else
          let args = ScmPair(ScmPair (ScmSymbol "value", ScmPair ((macro_expand test), ScmNil)), ScmPair((make_named_lambda "f" (macro_expand body)),
                    ScmPair((make_named_lambda "rest" (ScmPair ((expand_cond rest), ScmNil))), ScmNil))) in
          let tail = ScmPair(ScmSymbol "if",ScmPair(ScmSymbol "value",ScmPair(ScmPair(
                      ScmPair (ScmSymbol "f", ScmNil), ScmPair (ScmSymbol "value", ScmNil)),
                      ScmPair (ScmPair (ScmSymbol "rest", ScmNil), ScmNil)))) in
          macro_expand (ScmPair(ScmSymbol("let"),ScmPair(args,ScmPair(tail,ScmNil))))
  | ScmPair(ScmPair(ScmSymbol("else"), body),rest)-> if ((scm_list_length body) == 0) then raise (X_syntax_error(rib,"empty body after else")) else
          ScmPair(ScmSymbol("begin"),(macro_expand body))
  | ScmPair(ScmPair(test,body),rest) ->
          scm_append (ScmPair(ScmSymbol "if",ScmPair((macro_expand test),ScmPair(ScmPair (ScmSymbol "begin", (macro_expand body)),ScmNil)))) (ScmPair((expand_cond rest),ScmNil))
  | _ -> raise (X_syntax_error(rib,"Invalid syntax of cond-expr"))

and expand_quansiqoute sexpr = 
  match sexpr with
  | ScmNil -> (** () -> '() *)
                ScmPair (ScmSymbol "quote", ScmPair (ScmNil, ScmNil))   
  | ScmSymbol(x) -> (** x -> 'x *)
                  ScmPair (ScmSymbol "quote", ScmPair (ScmSymbol(x), ScmNil)) 
  | ScmPair (ScmSymbol "unquote", ScmPair (x, ScmNil)) -> (** ,x -> x *)
                   x  
  | ScmPair (ScmSymbol "unquote-splicing", ScmPair (x, ScmNil)) -> 
                  ScmPair(ScmSymbol "quote",ScmPair(ScmPair(ScmSymbol "unquote-splicing", ScmPair (x, ScmNil)),ScmNil))
  | ScmVector(x) ->
                let lst = expand_quansiqoute (list_to_proper_list x) in
                 ScmPair(ScmSymbol("list->vector"), ScmPair(lst,ScmNil))
  | ScmPair(ScmPair (ScmSymbol "unquote-splicing", ScmPair (exp, ScmNil)),b) -> 
                  ScmPair(ScmSymbol "append", ScmPair (exp, ScmPair ((expand_quansiqoute b), ScmNil)))
  | ScmPair(a,b) -> (** (a b) -> (cons 'a (cons 'b '())) *)
                  ScmPair(ScmSymbol "cons",ScmPair ((expand_quansiqoute a), ScmPair ((expand_quansiqoute b), ScmNil)))

  | _ -> sexpr

and macro_expand sexpr =
match sexpr with
  | ScmPair(ScmSymbol("let"), ScmPair(binds,body)) -> expand_let binds body
  | ScmPair(ScmSymbol("let*"), ScmPair(ScmNil,body)) -> macro_expand (ScmPair(ScmSymbol("let"), ScmPair(ScmNil,body)))
  | ScmPair(ScmSymbol("let*"), ScmPair(ScmPair(rib,ScmNil),body)) -> macro_expand (ScmPair(ScmSymbol("let"), ScmPair(ScmPair(rib,ScmNil),body)))
  | ScmPair(ScmSymbol("let*"), ScmPair(ScmPair(rib,ribs),body)) -> 
                                                                  let b = macro_expand (ScmPair(ScmSymbol("let*"),ScmPair(ribs,body))) in
                                                                  macro_expand (ScmPair(ScmSymbol("let"), ScmPair(ScmPair(rib,ScmNil),ScmPair(b,ScmNil))))
  | ScmPair(ScmSymbol("letrec"),ScmPair(binds,body)) -> macro_expand (expand_letrec binds body)
  | ScmPair(ScmSymbol("and"), ScmNil) -> ScmBoolean(true)
  | ScmPair(ScmSymbol("and"), ScmPair (x, ScmNil)) -> x
  | ScmPair(ScmSymbol("and"), ScmPair(rib,ribs)) -> 
                                    let rest = macro_expand (ScmPair(ScmSymbol("and"),ribs)) in
                                    ScmPair(ScmSymbol("if"),ScmPair(rib,ScmPair(rest,ScmPair(ScmBoolean(false),ScmNil))))
  | ScmPair(ScmSymbol("cond"), ribs) -> if (scm_list_length ribs) == 0 then raise (X_syntax_error(sexpr,"Invalid syntax of cond-expr")) 
                                        else expand_cond ribs
  | ScmPair(ScmSymbol("define"), ScmPair(ScmPair(var,args),body)) -> expand_define var args body
  | ScmPair(ScmSymbol("quasiquote"),ScmPair(x,ScmNil)) -> expand_quansiqoute x
  | _ -> sexpr

end;; 


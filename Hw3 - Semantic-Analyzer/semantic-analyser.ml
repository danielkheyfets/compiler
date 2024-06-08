(* semantic-analyser.ml
 * The semantic analysis phase of the compiler
 *)

#use "tag-parser.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

type var' = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | ScmConst' of sexpr
  | ScmVar' of var'
  | ScmBox' of var'
  | ScmBoxGet' of var'
  | ScmBoxSet' of var' * expr'
  | ScmIf' of expr' * expr' * expr'
  | ScmSeq' of expr' list
  | ScmSet' of var' * expr'
  | ScmDef' of var' * expr'
  | ScmOr' of expr' list
  | ScmLambdaSimple' of string list * expr'
  | ScmLambdaOpt' of string list * string * expr'
  | ScmApplic' of expr' * (expr' list)
  | ScmApplicTP' of expr' * (expr' list);;



let var_eq v1 v2 =
match v1, v2 with
  | VarFree (name1), VarFree (name2) -> String.equal name1 name2
  | VarBound (name1, major1, minor1), VarBound (name2, major2, minor2) ->
    major1 = major2 && minor1 = minor2 && (String.equal name1 name2)
  | VarParam (name1, index1), VarParam (name2, index2) ->
       index1 = index2 && (String.equal name1 name2)
  | _ -> false

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | ScmConst' (sexpr1), ScmConst' (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar' (var1), ScmVar' (var2) -> var_eq var1 var2
  | ScmIf' (test1, dit1, dif1), ScmIf' (test2, dit2, dif2) -> (expr'_eq test1 test2) &&
                                            (expr'_eq dit1 dit2) &&
                                              (expr'_eq dif1 dif2)
  | (ScmSeq' (exprs1), ScmSeq' (exprs2) | ScmOr' (exprs1), ScmOr' (exprs2)) ->
        List.for_all2 expr'_eq exprs1 exprs2
  | (ScmSet' (var1, val1), ScmSet' (var2, val2) | ScmDef' (var1, val1), ScmDef' (var2, val2)) ->
        (var_eq var1 var2) && (expr'_eq val1 val2)
  | ScmLambdaSimple' (vars1, body1), ScmLambdaSimple' (vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmLambdaOpt' (vars1, var1, body1), ScmLambdaOpt' (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmApplic' (e1, args1), ScmApplic' (e2, args2) ->
     (expr'_eq e1 e2) && (List.for_all2 expr'_eq args1 args2)
  | ScmApplicTP' (e1, args1), ScmApplicTP' (e2, args2) ->
      (expr'_eq e1 e2) && (List.for_all2 expr'_eq args1 args2)
  | ScmBox' (v1), ScmBox' (v2) -> var_eq v1 v2
  | ScmBoxGet' (v1), ScmBoxGet' (v2) -> var_eq v1 v2
  | ScmBoxSet' (v1, e1), ScmBoxSet' (v2, e2) -> (var_eq v1 v2) && (expr'_eq e1 e2)
  | _ -> false;;


module type SEMANTIC_ANALYSIS = sig
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
  val run_semantics : expr -> expr'
  val string_of_expr' : expr' -> string
end;; (* end of module type SEMANTIC_ANALYSIS *)

module Semantic_Analysis : SEMANTIC_ANALYSIS = struct



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

  let rec lookup_in_rib name = function
    | [] -> None
    | name' :: rib ->
       if name = name'
       then Some(0)
       else (match (lookup_in_rib name rib) with
             | None -> None
             | Some minor -> Some (minor + 1));;

  let rec lookup_in_env name = function
    | [] -> None
    | rib :: env ->
       (match (lookup_in_rib name rib) with
        | None ->
           (match (lookup_in_env name env) with
            | None -> None
            | Some(major, minor) -> Some(major + 1, minor))
        | Some minor -> Some(0, minor));;

  let tag_lexical_address_for_var name params env = 
    match (lookup_in_rib name params) with
    | None ->
       (match (lookup_in_env name env) with
        | None -> VarFree name
        | Some(major, minor) -> VarBound(name, major, minor))
    | Some minor -> VarParam(name, minor);;

  (* run this first! *)
  let annotate_lexical_addresses pe = 
   let rec run pe params env =
      match pe with
      | ScmConst(x) -> ScmConst'(x)
      | ScmVar(x) -> ScmVar'(tag_lexical_address_for_var x params env)
      | ScmIf(test,dit,dif) -> 
          let test = run test params env in
          let dit = run dit params env in
          let dif = run dif params env in      
          ScmIf'(test,dit,dif)
      | ScmOr(lst) ->
          let f x = run x params env in 
          let lst = List.map f lst in
          ScmOr'(lst)
      | ScmLambdaSimple(parameters,body) ->
          let b = run body parameters ([params]@env) in
          ScmLambdaSimple'(parameters,b)
      | ScmLambdaOpt(parameters,opt,body) ->
          let b = run body (parameters @ [opt]) ([params]@env) in
          ScmLambdaOpt'(parameters,opt,b)
      | ScmDef(ScmVar(var),value) -> ScmDef'((tag_lexical_address_for_var var params env),(run value params env))
      | ScmSet(ScmVar(x),y) -> ScmSet'((tag_lexical_address_for_var x params env),(run y params env))
      | ScmSeq(lst) ->
          let f x = run x params env in 
          let lst = List.map f lst in
          ScmSeq'(lst)
      | ScmApplic(e1, en) ->
          let e1 = run e1 params env in
          let f x = run x params env in
          let en = List.map f en in
          ScmApplic'(e1,en)
      | _ -> raise X_this_should_not_happen
   in 
   run pe [] [];;

  let rec rdc_rac s =
    match s with
    | [e] -> ([], e)
    | e :: s ->
       let (rdc, rac) = rdc_rac s
       in (e :: rdc, rac)
    | _ -> raise X_this_should_not_happen;;
  
  (* run this second! *)
  let annotate_tail_calls pe =
    let rec run pe in_tail =
        match pe with
        | ScmApplic'(f,exprs) -> check_applic f exprs in_tail
        | ScmIf'(test,dit,dif) -> check_if test dit dif in_tail
        | ScmSeq'(lst) -> ScmSeq'(check_lst lst in_tail)
        | ScmSet'(var,body) -> check_body var body true 
        | ScmOr'(lst) -> ScmOr'(check_lst lst in_tail)
        | ScmDef'(var, body) -> check_body var body false
        | ScmLambdaSimple'(params,body)->
            let body = run body true in
            ScmLambdaSimple'(params,body)
        | ScmLambdaOpt'(params,p,body)-> 
            let body = run body true in
            ScmLambdaOpt'(params,p,body)
        | expr -> expr 
    
    and check_body var body isSet =
      let body = run body false in
      if(isSet) then ScmSet'(var,body) else ScmDef'(var,body)
    
    and check_if test dit dif in_tail = 
      let test = run test false in
      let dit = run dit in_tail in
      let dif = run dif in_tail in
      ScmIf'(test,dit,dif) 
    
    and check_applic f exprs in_tail = 
      let f_map x = run x false in
      let exprs = List.map f_map exprs in
      let f = run f false in
      if(in_tail) then ScmApplicTP'(f,exprs) else ScmApplic'(f,exprs)  
    
    and check_lst lst in_tail =
      let (elments,last) = rdc_rac lst in
      let f_map x = run x false in
      let elments = List.map f_map elments in
      let last = if (in_tail) then run last true else run last false in
      (elments@[last])

      in
  run pe false;;

  (* boxing *)

let find_writes name enclosing_lambda expr = 
    let rec run enclosing_lambda expr flag = 
      match expr with
      | ScmApplicTP'(e,e_lst) -> check_applic enclosing_lambda e e_lst flag
      | ScmApplic'(e,e_lst) -> check_applic enclosing_lambda e e_lst flag
      | ScmIf'(test,dit,dif) -> check_if enclosing_lambda test dit dif flag
      | ScmSeq'(e_lst) -> check_lst enclosing_lambda e_lst flag
      | ScmSet'(var,body) -> check_set var body enclosing_lambda flag
      | ScmOr'(e_lst) -> check_lst enclosing_lambda e_lst flag
      | ScmDef'(var, body) -> run enclosing_lambda body flag
      | ScmLambdaSimple'(params,body)-> 
          if(List.mem name params) 
          then [] else
          if (flag) then run enclosing_lambda body flag else run expr body true
      | ScmLambdaOpt'(params,p,body)-> 
          if((List.mem name params) || (name = p)) then [] else 
          if (flag) then run enclosing_lambda body flag else run expr body true
      | _ -> []
    and check_lst enclosing_lambda e_lst flag = 
      let writes = List.fold_left (fun cs exp -> (cs @ (run enclosing_lambda exp flag))) [] e_lst in
      writes
    and check_if enclosing_lambda test dit dif flag = 
      let cs_test = run enclosing_lambda test flag in
      let cs_dit = run enclosing_lambda dit flag in
      let cs_dif = run enclosing_lambda dif flag in
      (cs_test@cs_dit@cs_dif)
    and check_applic enclosing_lambda e e_lst flag =
      let cs_e = run enclosing_lambda e flag in
      let cs_lst = check_lst enclosing_lambda e_lst flag in
      (cs_e@cs_lst)
    and check_set var body enclosing_lambda flag = 
      let is_name =
        match var with
        | VarFree(n) -> n = name
        | VarParam(n,i) -> n = name
        | VarBound(n,i,j) -> n = name
        in
      let body = run enclosing_lambda body flag in
      if(is_name) then ([enclosing_lambda]@body) else body
    
    in
    run enclosing_lambda expr false;;
 

let find_reads name enclosing_lambda expr = 
    let rec run enclosing_lambda expr flag = 
      match expr with
      | ScmVar'(var) -> check_var var enclosing_lambda
      | ScmApplicTP'(e,e_lst) -> check_applic enclosing_lambda e e_lst flag
      | ScmApplic'(e,e_lst) -> check_applic enclosing_lambda e e_lst flag
      | ScmIf'(test,dit,dif) -> check_if enclosing_lambda test dit dif flag
      | ScmSeq'(e_lst) -> check_lst enclosing_lambda e_lst flag
      | ScmSet'(var,body) -> check_set body enclosing_lambda flag
      | ScmOr'(e_lst) -> check_lst enclosing_lambda e_lst flag
      | ScmDef'(var, body) -> run enclosing_lambda body flag
      | ScmLambdaSimple'(params,body)-> 
          if(List.mem name params) 
          then [] else
          if (flag) then (run enclosing_lambda body flag) else (run expr body true)
      | ScmLambdaOpt'(params,p,body)-> 
          if((List.mem name params) || (name = p)) then [] else 
          if (flag) then (run enclosing_lambda body flag) else (run expr body true)
      | _ -> []
    
    and  check_set body enclosing_lambda flag =
      match body with
      | ScmVar'(x) -> []
      | _ -> run enclosing_lambda body flag
    and check_lst enclosing_lambda e_lst flag = 
      let reads = List.fold_left (fun cs exp -> (cs @ (run enclosing_lambda exp flag))) [] e_lst in
      reads
    and check_if enclosing_lambda test dit dif flag = 
      let cs_test = run enclosing_lambda test flag in
      let cs_dit = run enclosing_lambda dit flag in
      let cs_dif = run enclosing_lambda dif flag in
      (cs_test@cs_dit@cs_dif)
    and check_applic enclosing_lambda e e_lst flag =
      let cs_e = run enclosing_lambda e flag in
      let cs_lst = check_lst enclosing_lambda e_lst flag in
      (cs_e@cs_lst)
    and check_var var enclosing_lambda = 
      let is_name =
        match var with
        | VarFree(n) -> n = name
        | VarParam(n,i) -> n = name
        | VarBound(n,i,j) -> n = name
        in
      if(is_name) then [enclosing_lambda] else []
    
    in
    run enclosing_lambda expr false;;
 
  let rec setBox_getBox expr name = 
    match expr with
    | ScmVar'(v) -> if(is_var_name v name) then ScmBoxGet'(v) else ScmVar'(v)
    | ScmApplicTP'(e,e_lst) -> set_get_applic e e_lst name true
    | ScmApplic'(e,e_lst) -> set_get_applic e e_lst name false
    | ScmIf'(test,dit,dif) -> set_get__if test dit dif name
    | ScmSeq'(e_lst) -> ScmSeq'(set_get_lst e_lst name)
    | ScmSet'(var,body) -> set_get_body var body name true
    | ScmBoxSet'(var,body) -> ScmBoxSet'(var,(setBox_getBox body name))
    | ScmOr'(e_lst) -> ScmOr'(set_get_lst e_lst name)
    | ScmDef'(var, body) -> set_get_body var body name false
    | ScmLambdaSimple'(params,body)-> if (List.mem name params) then expr else
        let body = setBox_getBox body name in
        ScmLambdaSimple'(params,body)
    | ScmLambdaOpt'(params,p,body)->  if (List.mem name params || p = name) then expr else
        let body = setBox_getBox body name in
        ScmLambdaOpt'(params,p,body)
    | e -> e 
  and set_get_applic e e_lst name isTP = 
      let e = setBox_getBox e name in
      let f exp = setBox_getBox exp name in
      let e_lst = List.map f e_lst in
      if (isTP) then ScmApplicTP'(e,e_lst) else ScmApplic'(e,e_lst)
  and set_get__if test dit dif name =
      let test = setBox_getBox test name in
      let dit = setBox_getBox dit name in
      let dif = setBox_getBox dif name in
      ScmIf'(test,dit,dif)
  and set_get_lst lst name = 
      let lst = List.map (fun exp -> setBox_getBox exp name) lst in
      lst
  and set_get_body var body name isSet =
      let body = setBox_getBox body name in
      let bool = is_var_name var name in
      if (isSet) then
        if (bool) then ScmBoxSet'(var,body) else ScmSet'(var,body)
      else ScmDef'(var,body)
  and is_var_name v name =
     match v with
        | VarParam(p,i) -> p = name
        | VarFree(p) -> p = name
        | VarBound (p,i,j) -> p = name
   

   let make_tup_minor lst name= 
    let rec run lst minor = 
      match lst with
      | [] -> raise X_this_should_not_happen
      | n :: rest -> if (n = name) then (n,minor) else (run rest (minor + 1))
    in
    run lst 0;;
    

  let rec box_set expr = 
    match expr with
    | ScmApplicTP'(e,e_lst) -> box_applic e e_lst true
    | ScmApplic'(e,e_lst) -> box_applic e e_lst false
    | ScmIf'(test,dit,dif) -> box_if test dit dif
    | ScmSeq'(e_lst) -> ScmSeq'(box_lst e_lst) 
    | ScmSet'(var,body) -> box_body var body true
    | ScmBoxSet'(var,body) -> ScmBoxSet'(var,(box_set body))
    | ScmOr'(e_lst) -> ScmOr'(box_lst e_lst)
    | ScmDef'(var, body) -> box_body var body false 
    | ScmLambdaSimple'(params,body)-> box_simple params body expr 
    | ScmLambdaOpt'(params,p,body)-> box_opt params p body expr
    | e -> e 
  and box_applic e e_lst isTP = 
      let e = box_set e in
      let e_lst = List.map box_set e_lst in
      if (isTP) then ScmApplicTP'(e,e_lst) else ScmApplic'(e,e_lst)
  and box_if test dit dif =
      let test = box_set test in
      let dit = box_set dit in
      let dif = box_set dif in
      ScmIf'(test,dit,dif)
  and box_lst lst = 
      let lst = List.map box_set lst in
      lst
  and box_body var body isSet = 
      let body = box_set body in
      if (isSet) then ScmSet'(var, body) else ScmDef'(var,body)
  and box_simple params body enclosing_lambda=
      if ((List.length params) = 0) then ScmLambdaSimple'(params,(box_set body))
           else
          let body = make_lambda_body params body enclosing_lambda false "p" in      
          ScmLambdaSimple'(params,body)
  and box_opt params p body enclosing_lambda = 
      if ((List.length params) = 0) 
      then 
        let (name,b) = find_box body enclosing_lambda p in
        if (b) 
            then
              let add_set = ScmSet'(VarParam(name,0),ScmBox'(VarParam(name,0))) in
              let body = setBox_getBox body name in
              let body = match body with       
                | ScmSeq'(b) -> ScmSeq'([add_set]@b)
                | _ -> ScmSeq'([add_set;body]) in
              ScmLambdaOpt'(params,p,(box_set body))
            else
            ScmLambdaOpt'(params,p,(box_set body))
      else 
        let body = make_lambda_body params body enclosing_lambda true p in   
        ScmLambdaOpt'(params,p,body)
  
  
  and make_lambda_body params body enclosing_lambda isOpT p = 
    let to_box_slst = if (isOpT) then (List.map (find_box body enclosing_lambda) params) @[(find_box body enclosing_lambda p)] 
                      else (List.map (find_box body enclosing_lambda) params) in	
    let to_box_string_lst = List.map (fun (x,y) -> x) (List.filter (fun (x,b) -> b) to_box_slst) in
    if ((List.length to_box_string_lst) > 0)
      then
      let tupled_list_name_minor = List.map (make_tup_minor params) to_box_string_lst in
      let add_sets (name,minor) = ScmSet'(VarParam(name,minor),ScmBox'(VarParam(name,minor))) in
      let set_lst = List.map add_sets tupled_list_name_minor in
      let body = List.fold_left (fun b name -> (setBox_getBox b name)) body to_box_string_lst in
      let body = match body with  
        | ScmSeq'(b) -> ScmSeq'(set_lst@b)
        | _ -> ScmSeq'(set_lst@[body]) in
      (box_set body)
    else
      (box_set body)


  and find_box body enclosing_lambda name  =  
      let reads = find_reads name enclosing_lambda body in
      let writes = find_writes name enclosing_lambda body in
      if ((List.length reads) = 0 || (List.length writes) = 0) then (name,false) 
      else
        let f = fun l1 l2 -> (List.exists (fun a -> (List.exists ((<>) a) l2)) l1) in 
        let can_box = f reads writes in
        if(can_box) then (name,true) else (name,false)

  
  let run_semantics expr =
    box_set
      (annotate_tail_calls
         (annotate_lexical_addresses expr))


end;;
 (* end of module Semantic_Analysis *)

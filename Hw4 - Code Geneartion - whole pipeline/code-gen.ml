#use "semantic-analyser.ml";;
exception X_this_should_not_happen;;
(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (sexpr * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr' that has been annotated 
     by the semantic analyser.
   *)
  val generate : (sexpr * (int * string)) list -> (string * int) list -> expr' -> string
end;;


module Code_Gen : CODE_GEN = struct
  
    let hard_coded_freeVars = 
    [
    (* Type queries  *)
    "boolean?"; "flonum?"; "rational?";
    "pair?"; "null?"; "char?"; "string?";
    "procedure?"; "symbol?";
    (* String procedures *)
    "string-length"; "string-ref"; "string-set!";
    "make-string"; "symbol->string";
    (* Type conversions *)
    "char->integer"; "integer->char"; "exact->inexact";
    (* Identity test *)
    "eq?";
    (* Arithmetic ops *)
    "+"; "*"; "/"; "="; "<";
    (* Additional rational numebr ops *)
    "numerator"; "denominator"; "gcd";
    (* you can add yours here *) 
    "cons";"car";"cdr";"set-car!";"set-cdr!";"apply"
    ];;



  let rec collect_ScmConst_records ast =
    match ast with
    | ScmConst'(x) -> [x]
    | ScmBoxSet'(x,y) -> collect_ScmConst_records y
    | ScmIf'(test,dit,dif) -> 
      let test = collect_ScmConst_records test in
      let dit = collect_ScmConst_records dit in
      let dif = collect_ScmConst_records dif in
      test@dit@dif
    | ScmSeq'(lst) -> List.fold_left (fun free_lst exp -> (free_lst @ (collect_ScmConst_records exp))) [] lst
    | ScmSet'(var,exp) -> collect_ScmConst_records exp
    | ScmDef'(var,exp) -> collect_ScmConst_records exp
    | ScmOr'(lst) -> List.fold_left (fun free_lst exp -> (free_lst @ (collect_ScmConst_records exp))) [] lst
    | ScmLambdaSimple'(params,body) -> collect_ScmConst_records body
    | ScmLambdaOpt'(params,op,body) -> collect_ScmConst_records body
    | ScmApplic'(x,lst) -> (collect_ScmConst_records x) @ (List.fold_left (fun free_lst exp -> (free_lst @ (collect_ScmConst_records exp))) [] lst)
    | ScmApplicTP'(x,lst) -> (collect_ScmConst_records x) @ (List.fold_left (fun free_lst exp -> (free_lst @ (collect_ScmConst_records exp))) [] lst)
    | _ -> []
  
  let rec collect_FreeVars_records ast=
    match ast with
    | ScmVar'(var) -> checkVar var
    | ScmBoxSet'(x,y) -> collect_FreeVars_records y
    | ScmIf'(test,dit,dif) -> 
      let test = collect_FreeVars_records test in
      let dit = collect_FreeVars_records dit in
      let dif = collect_FreeVars_records dif in
      test@dit@dif
    | ScmSeq'(lst) -> List.fold_left (fun const_lst exp -> (const_lst @ (collect_FreeVars_records exp))) [] lst
    | ScmSet'(var,exp) -> checkVar var@(collect_FreeVars_records exp)
    | ScmDef'(var,exp) -> checkVar var@(collect_FreeVars_records exp)
    | ScmOr'(lst) -> List.fold_left (fun const_lst exp -> (const_lst @ (collect_FreeVars_records exp))) [] lst
    | ScmLambdaSimple'(params,body) -> collect_FreeVars_records body
    | ScmLambdaOpt'(params,op,body) -> collect_FreeVars_records body
    | ScmApplic'(x,lst) -> (collect_FreeVars_records x) @ (List.fold_left (fun const_lst exp -> (const_lst @ (collect_FreeVars_records exp))) [] lst)
    | ScmApplicTP'(x,lst) -> (collect_FreeVars_records x) @ (List.fold_left (fun const_lst exp -> (const_lst @ (collect_FreeVars_records exp))) [] lst)
    | _ -> []
  and checkVar var = 
    match var with
    | VarFree(x) -> [x]
    | _-> []

  let rec add_sub_constants sexpr = 
    match sexpr with
    | ScmSymbol(x) -> [ScmString(x);sexpr]
    | ScmVector(x) -> (List.fold_left (fun lst sexpr -> lst@(add_sub_constants sexpr)) [] x)@[sexpr]
    | ScmPair(car,cdr) -> (add_sub_constants car)@(add_sub_constants cdr)@[sexpr]
    | _-> [sexpr] 
  
  let remove_dups lst = List.fold_left (fun lst sexpr -> if (not (List.mem sexpr lst)) then lst@[sexpr] else lst) [] lst;;
  
  let size_of sexpr= 
  match sexpr with
  | ScmVoid -> 1
  | ScmNil -> 1
  | ScmBoolean(x) -> 2
  | ScmChar(x) -> 2
  | ScmString(x) -> 1+8+(String.length x)
  | ScmSymbol(x)-> 1 + 8
  | ScmNumber(ScmRational(x,y))-> 1 + 8 + 8
  | ScmNumber(ScmReal(x))-> 1 + 8
  | ScmVector(lst)-> 1 + 8 + ((List.length lst)*8)
  | ScmPair(car,cdr) -> 1 + 8 + 8

  let get_offset lst sexpr = fst (List.assoc sexpr lst);;
  
  let rec make_vecotr_args sexpr_lst lst= 
    match sexpr_lst with
    | [] -> ""
    | [e] -> Printf.sprintf "const_tbl+%d" (get_offset lst e)
    | e :: rest -> (Printf.sprintf "const_tbl+%d, " (get_offset lst e))^make_vecotr_args rest lst

  let generate_record sexpr lst offset= 
    match sexpr with
    | ScmVoid -> (lst@[(sexpr,(offset, "db T_VOID"))],(offset+(size_of sexpr)))
    | ScmNil ->  (lst@[(sexpr,(offset, "db T_NIL"))],(offset+(size_of sexpr)))
    | ScmBoolean(x) -> 
      let asm = if x then "db T_BOOL, 1" else "db T_BOOL, 0" in
      (lst@[(sexpr,(offset,asm))],(offset+(size_of sexpr)))
    | ScmChar(x) ->  (lst@[(sexpr,(offset,(Printf.sprintf "MAKE_LITERAL_CHAR(%d)" (int_of_char x))))],(offset+(size_of sexpr)))
    | ScmNumber(ScmRational(a,b))-> (lst@[(sexpr,(offset,(Printf.sprintf "MAKE_LITERAL_RATIONAL(%d,%d)" a b)))],(offset+(size_of sexpr)))
    | ScmNumber(ScmReal(x))-> (lst@[(sexpr,(offset, (Printf.sprintf "MAKE_LITERAL_REAL(%s)" (string_of_float x))))],(offset+(size_of sexpr)))
    | ScmString(x) -> (lst@[(sexpr,(offset,(Printf.sprintf "MAKE_LITERAL_STRING \"%s\"" x)))],(offset+(size_of sexpr)))
    | ScmSymbol(x)->  (lst@[(sexpr,(offset, (Printf.sprintf "MAKE_LITERAL_SYMBOL(const_tbl+%d)" (get_offset lst (ScmString(x))))))],(offset+(size_of sexpr)))
    | ScmVector(sexpr_lst)-> 
        let x = make_vecotr_args sexpr_lst lst in 
        (lst@[(sexpr,(offset, (Printf.sprintf "MAKE_LITERAL_VECTOR %s" x)))],(offset+(size_of sexpr)))
    | ScmPair(car,cdr) -> 
              let car_offset = get_offset lst car in           
              let cdr_offset = get_offset lst cdr in
              (lst@[(sexpr,(offset, (Printf.sprintf "MAKE_LITERAL_PAIR(const_tbl+%d, const_tbl+%d)" car_offset cdr_offset)))],(offset+(size_of sexpr)))
  
  let generate_indexed_tuples lst =
    let rec run lst index =
    match lst with
    | [] -> []
    | hd :: tl -> [(hd,index)]@(run tl (index+8)) (**Pointer to SOB are 8 bytes *)
    in
    run lst 0;;

  let make_consts_tbl asts =
    let sexpr_lst = List.fold_left (fun lst ast -> lst @ collect_ScmConst_records ast) [] asts in (**STEP 1:  Scan the AST (one recursive pass) & collect the sexprs in all Const records*)
    let sexpr_lst = remove_dups sexpr_lst in (**STEP 2: Convert the list to a set (removing duplicates)*)
    let sexpr_lst = List.fold_left (fun lst sexpr -> lst@ (add_sub_constants sexpr)) [] sexpr_lst in (**STEP 3: Expand the list to include all sub-constants*)
    let sexpr_lst = [ScmVoid;ScmNil;ScmBoolean false;ScmBoolean true]@sexpr_lst in (**add the 4 inits void,nil,false,true*)
    let sexpr_lst = remove_dups sexpr_lst in (**STEP 4: Convert the resulting list into a set (remove all duplicates, again)*)
    let (const_table,offset) = List.fold_left (fun (acc_lst,offset) sexpr ->  (generate_record sexpr acc_lst offset)) ([],0) sexpr_lst in (**STEP 5: Go over the list, from first to last, and create the constants-table*)                                                 
    const_table;;


  let make_fvars_tbl asts =
    let free_vars = List.fold_left (fun lst ast -> lst@ collect_FreeVars_records ast) [] asts in (**STEP 1:  Scan the AST (one recursive pass) & collect the string in all free var records*)
    let free_vars = hard_coded_freeVars @ free_vars in
    let free_vars = remove_dups free_vars in (**STEP 2: Convert the list to a set (removing duplicates)*)
    let free_vars_tbl = generate_indexed_tuples free_vars in
    free_vars_tbl;;


  let rec rdc_rac s =
    match s with
    | [e] -> ([], e)
    | e :: s ->
       let (rdc, rac) = rdc_rac s
       in (e :: rdc, rac)
    | _ -> raise X_this_should_not_happen;;

(**simple generator of index for labels *)
let index = ref 0;;
let generateIndx () =
  let i = !index in
  (index := !index + 1); (string_of_int i)

  let generate consts fvars e = 
    let rec run e env_size =
    match e with
      | ScmConst'(x) -> gen_const x 
      | ScmVar'(var) -> getVar var
      | ScmBox'(VarParam(name,minor)) -> box name minor
      | ScmBoxGet'(var) -> gen_BoxGet var
      | ScmBoxSet'(var,expr) -> gen_BoxSet var expr env_size
      | ScmIf'(test,dit,dif) -> gen_if test dit dif env_size
      | ScmSeq'(lst) -> gen_Seq lst env_size
      | ScmSet'(var,expr) -> setVar var expr env_size
      | ScmDef'(var,expr) -> gen_def var expr env_size
      | ScmOr'(exprs) -> gen_Or exprs env_size
      | ScmLambdaSimple'(params,body) -> gen_lambda_simple params body env_size (generateIndx())
      | ScmLambdaOpt'(params,opt,body) -> gen_lambda_opt params opt body env_size (generateIndx())
      | ScmApplic'(proc,args) -> gen_applic proc args env_size
      | ScmApplicTP'(proc,args) -> gen_applicTP proc args env_size
      | _ -> raise X_this_should_not_happen

  and gen_const x  =
      let offset = get_offset consts x in
      Printf.sprintf "mov rax, const_tbl+%d" offset

  and box name minor =
    let comment = Printf.sprintf ";; creating code for Box for VarParam: [%s] and minor: [%d]\n" name minor in
    comment^
    Printf.sprintf "mov rbx, PVAR(%d)\n" minor ^
    "MALLOC rax, WORD_SIZE\n" ^
    "mov qword [rax], rbx"
        
  and gen_def var expr env_size =
    let comment = ";; creating code for Define\n" in
    let v = setVar var expr env_size in
    comment^v
  and gen_BoxSet var expr env_size =
    let comment = ";; creating code for BoxSet\n" in
    comment ^
    (run expr env_size) ^ "\n" ^ 
    "push rax\n" ^
    (getVar var) ^ "\n" ^
    "pop qword [rax]\n" ^
    "mov rax, SOB_VOID_ADDRESS"
  
  and gen_BoxGet var =
      let comment = ";; creating code for BoxGet\n" in
      let asm_var = (getVar var) ^ "\n"in
      comment ^ asm_var ^ "mov rax, qword [rax]"
  
  and gen_if test dit dif env_size =
    let index = generateIndx () in
    let l_exit = "Lexit"^index in
    let l_else = "Lelse"^index in
    let comment = Printf.sprintf ";; creating code for If statment with %s and %s\n" l_else l_exit in
    let asm_if = (run test env_size) ^ "\n" ^
      "cmp rax, SOB_FALSE_ADDRESS\n" ^
      (Printf.sprintf "je %s\n" l_else) ^
      (run dit env_size) ^ "\n" ^ 
      (Printf.sprintf "jmp %s\n" l_exit) ^ 
      (Printf.sprintf "%s:\n" l_else) ^
      (run dif env_size) ^ "\n" ^
      (Printf.sprintf "%s:" l_exit) in
    (comment^asm_if)
    

  and gen_Or exprs env_size =
    let (expr_lst,last_expr) = rdc_rac exprs in
    let index = generateIndx () in
    let l_exit = "Lexit"^index in
    let comment = Printf.sprintf ";; creating code for Or list exprs with %s\n" l_exit in
    let check_false = Printf.sprintf "cmp rax, SOB_FALSE_ADDRESS\n" ^ 
                      (Printf.sprintf "jne %s\n" l_exit) in
    let asm_exprs = List.fold_left (fun acc expr -> acc ^ (run expr env_size) ^ "\n" ^ check_false) "" expr_lst in
    let asm_last = run last_expr env_size in
    let asm_or = asm_exprs^asm_last^"\n"^(Printf.sprintf "%s:" l_exit) in
    (comment^asm_or)

  and gen_Seq lst env_size=
    let comment = ";; creating code for Sequence list exprs\n" in
    comment ^ (List.fold_left (fun acc expr -> acc^(run expr env_size)^ "\n") "" lst) ^ "\n"

  and setVar var expr env_size =
    match var with
    | VarParam(name,minor) -> (**Parameters/set *)
      let comment = Printf.sprintf ";; creating Parameters-set for variable [%s] with minor: [%d]\n" name minor in
      let asm_expr = run expr env_size in
      comment^
      asm_expr^"\n"^
      (Printf.sprintf "mov qword [rbp+8*(4+%d)], rax\n" minor) ^
       "mov rax, SOB_VOID_ADDRESS"
       
    | VarBound(name,major,minor) -> (**BoundVar/set *)
      let comment = Printf.sprintf ";; creating BoundVar-set for variable [%s] with major: [%d] , minor: [%d]\n" name major minor in
      let asm_expr = run expr env_size in
      comment ^
      asm_expr^"\n"^
      "mov rbx, qword [rbp+8*2]\n"^
      Printf.sprintf "mov rbx, qword [rbx+8*%d]\n" major ^
      Printf.sprintf "mov qword [rbx+8*%d], rax\n" minor ^
      "mov rax, SOB_VOID_ADDRESS"
    | VarFree(name) -> (**FreeVar/set *)
      let comment = Printf.sprintf ";; creating FreeVar-set for variable [%s]\n" name in 
      let asm_expr = run expr env_size in
      let offset = List.assoc name fvars in
      comment ^
      asm_expr^"\n"^
      (Printf.sprintf "mov qword [fvar_tbl+%d], rax\n" offset) ^
      "mov rax, SOB_VOID_ADDRESS"

  and getVar = function
    | VarParam(name,minor) -> (**Parameters/get *)
        let comment = Printf.sprintf ";; creating Parameters-get for variable [%s] with minor: [%d]\n" name minor in
        comment ^
        Printf.sprintf "mov rax, qword [rbp+8*(4+%d)]" minor
    | VarBound(name,major,minor) -> (**BoundVar/get *)
        let comment = Printf.sprintf ";; creating BoundVar-get for variable [%s] with major: [%d] , minor: [%d]\n" name major minor in
        comment ^
        "mov rax, qword [rbp+8*2]\n" ^
        Printf.sprintf "mov rax, qword [rax+8*%d]\n" major ^
        Printf.sprintf "mov rax, qword [rax+8*%d]" minor
    | VarFree(name) -> (**FreeVar/get *)
        let comment = Printf.sprintf ";; creating FreeVar-get for variable [%s]\n" name in 
        let offset = List.assoc name fvars in
        comment^
        Printf.sprintf "mov rax, qword [fvar_tbl+%d]" offset

   and gen_lambda_opt params p body env_size index =
      ";; Generate code for Lambda Opt with depth of " ^ (string_of_int env_size) ^" \n" ^
     (gen_base_lambda env_size index) ^ 
     (gen_lcode_opt params body env_size index)

  and gen_lcode_opt params body env_size index =
    let fix_stack_opt = 
    "mov rdx, PARAM_COUNT      ;; rdx = n + magic\n" ^
    "mov r10, rdx     ;; save for after the shift later\n" ^
    "dec rdx         ;; no need for magic\n" ^
    "lea rcx, [rdx-"^ (string_of_int (List.length params))^"]  ;;PARAM_COUNT - ARGS_COUNT = amount of sob to add to list\n"^
    "cmp rcx, 0\n" ^
    "jle finished_fixing"^index^"\n" ^
    ";; else there is at least 1\n" ^
    "dec rdx      ;; 0-indexed\n" ^
    "mov rbx, PVAR(rdx)   ;; last sob optional arg\n" ^
    ";; construct the list of optionals\n"^
    "MAKE_PAIR(rax,rbx,SOB_NIL_ADDRESS)\n"^
    "dec rdx\n"^
    "dec rcx\n"^
    "make_list_loop" ^ index ^ ":\n" ^
    "\tcmp rcx, 0\n" ^
    "\tje end_make_list_loop"^index^"\n"^
    "\tmov rbx, PVAR(rdx)\n" ^ 
    "\tMAKE_PAIR(r12,rbx,rax)\n" ^
    "\tmov rax,r12\n" ^
    "\tdec rdx\n" ^
    "\tdec rcx\n"^
    "\tjmp make_list_loop" ^ index ^"\n" ^
    "end_make_list_loop"^index^":\n" ^
    ";; fix the stack -> shift up\n" ^
    "push SOB_NIL_ADDRESS    ;;magic\n"^
    "push rax     ;; push pointer to opt list\n"^
    "lea r11, [rdx+3] ;; the new amount of PARAM_COUNT to push later including magic\n" ^
    "copy_rest_args_loop"^index^":\n"^
    "\tcmp rdx, 0\n" ^
    "\tjl end_copy_rest_args_loop"^index^"\n"^
    "\t push PVAR(rdx)\n"^
    "\tdec rdx\n"^
    "\tjmp copy_rest_args_loop"^index^"\n"^
    "end_copy_rest_args_loop"^index^":\n"^
    ";; added all args to stack\n" ^
    "push r11   ;; new PARAM count\n"^
    "push ENV_ON_STACK\n"^
    "push OLD_RETURN_ADDRESS\n"^
    "push qword [rbp]   ;;push old rbp\n" ^
    " ;; Fix thr stack -> shift frame\n" ^
    "add r11, 4\n     ;; 4 = old rbp, old ret, param count, env\n" ^
    "MY_SHIFT_FRAME r11\n" ^
    "pop rbp\n"^
    "lea rsp, [rsp+WORD_SIZE*(r10+4)]     ;; 4 = old rbp ,env,old ret, param_count\n" ^
    "push rbp\n"^
    "mov rbp, rsp\n"^
    "finished_fixing"^index^":\n"
    in
    "Lcode"^index^":\n" ^
    "\tpush rbp\n" ^
    "\tmov rbp, rsp\n" ^
    fix_stack_opt ^
    (run body (env_size + 1)) ^ "\n" ^
    "\tleave\n" ^
    "\tret\n" ^
    "Lcont"^index^":\n" 


  and gen_base_lambda env_size index =
      ";; Creating ExtEnv for Lambda  \n"^
      "mov rcx, " ^ (string_of_int env_size) ^ "    ;; rcx = |Env|\n" ^
      "cmp rcx, 0\n"^
      "je empty_env"^index^"\n"^
      "MALLOC rax, " ^ (string_of_int ((env_size + 1)*8)) ^ " ;; allocate the ExtEnv -> 1 + |Env|\n" ^
      "mov rbx, 0   ;; i = 0\n"^
      "mov rdx, 1   ;; j = 1\n" ^
      "mov r8, ENV_ON_STACK\n" ^
      "copy_minors_loop"^index^":\n"^
      "\tcmp rbx, rcx       ;; i < |Env|\n"^
      "\tjge end_copy_minors_loop"^index ^ "\n" ^
      "\tmov r9, ENV(r8,rbx)     ;; r9 = Env[i]\n" ^
      "\tmov ENV(rax,rdx), r9  ;; ExtEnv[j] = Env[i]\n" ^
      "\tinc rbx    ;; i++\n" ^
      "\tinc rdx    ;; j++\n" ^
      "\tjmp copy_minors_loop" ^ index ^ "\n" ^
      "end_copy_minors_loop"^index ^ ":\n" ^
      ";; at this point rax points to ExtEnv[0] and all minors vectors are copyed to ExtEnv\n" ^
      "\n ;; Allocate ExtEnv[0] to point to a vector to store the parameters\n" ^
      "mov rcx, PARAM_COUNT   ;; notice that MAGIC is included in PARAM_COUNT\n" ^
      "shl rcx, 3 ;; n*WORD_SIZE\n" ^
      "MALLOC rbx, rcx    ;; rbx pointer to vector of size n*WORD_SIZE\n" ^
      ";; copy the paramters to new vector\n" ^
      "mov rcx, PARAM_COUNT\n" ^
      "mov rdx, 0   ;; i = 0\n" ^
      "copy_params_loop"^index^":\n"^
      "\tcmp rdx, rcx\n" ^
      "\tjge end_copy_params_loop"^index^"\n"^
      "\tmov r8, PVAR(rdx)   ;; r8 = Param_i\n"^
      "\tmov VECTOR(rbx, rdx), r8    ;; arr[i] = param_i\n"^
      "\tinc rdx    ;; i++\n" ^
      "\tjmp copy_params_loop"^index^"\n"^
      "end_copy_params_loop"^index^":\n"^
      ";; assign ExtEnv[0] = new copied vector\n"^
      "mov ENV(rax,0), rbx\n" ^
      "\n ;;at this point we have and ExtEnv that rax points to\n" ^
      ";; make closure from new env and code\n"^
      "mov rbx, rax        ;; rbx = ExtEnv\n" ^
      "MAKE_CLOSURE(rax,rbx,Lcode"^index^")\n" ^
      "jmp Lcont"^index^"\n"^
      "empty_env"^index^":\n"^
      "MALLOC rbx, WORD_SIZE\n" ^
      "mov qword [rbx], SOB_NIL_ADDRESS\n" ^
      "MAKE_CLOSURE(rax,rbx,Lcode"^index^")\n" ^
      "jmp Lcont"^index^"\n"

  and gen_lambda_simple params body env_size index =
     ";; Generate code for Lambda Simple with depth of " ^ (string_of_int env_size) ^" \n" ^
     (gen_base_lambda env_size index) ^ 
     (gen_lcode_simple body env_size index)

    
    and gen_lcode_simple body env_size index =
    "Lcode"^index^":\n" ^
    "\tpush rbp\n" ^
    "\tmov rbp, rsp\n" ^
    (run body (env_size + 1)) ^ "\n" ^
    "\tleave\n" ^
    "\tret\n" ^
    "Lcont"^index^":\n" 


    and gen_applic proc args env_size =
      ";;gen Applic " ^ (Printf.sprintf "\n;;proc: [%s]\n" (Semantic_Analysis.string_of_expr' proc)) ^
      "push SOB_NIL_ADDRESS      ;; magic\n"^
      (List.fold_right (fun arg str -> str^(run arg env_size)^"\npush rax\n") args "") ^
      (Printf.sprintf "push %d\n    ;; push param count => magic included\n" (List.length args + 1)) ^
      (run proc env_size)^"\n" ^
      "cmp byte [rax], T_CLOSURE    ;;verify that rax has type closure\n"^
      "jne non_proc_error\n" ^
      "CLOSURE_ENV rbx, rax     ;; rbx = Env\n" ^
      "push rbx           ;; push env\n" ^
      "CLOSURE_CODE rax, rax      ;; rax = Code\n" ^
      "call rax          ;; go to code\n" ^
      "add rsp, WORD_SIZE    ;; pop env\n" ^
      "pop rbx       ;; pop arg couint (encluding magic)\n" ^
      "lea rsp, [rsp+WORD_SIZE*rbx]\n"

    and gen_applicTP proc args env_size = 
      ";;gen ApplicTP " ^ (Printf.sprintf "\n;;proc: [%s]\n" (Semantic_Analysis.string_of_expr' proc)) ^
      "push SOB_NIL_ADDRESS      ;; magic\n"^
      (List.fold_right (fun arg str -> str^(run arg env_size)^"\npush rax\n") args "") ^
      (Printf.sprintf "push %d\n    ;; push param count => magic included\n" (List.length args + 1)) ^
      (run proc env_size)^"\n" ^
      "cmp byte [rax], T_CLOSURE    ;;verify that rax has type closure\n"^
      "jne non_proc_error\n" ^
      "mov rcx, PARAM_COUNT   ;; save for rsp after shifting the frame\n" ^
      "CLOSURE_ENV rbx, rax     ;; rbx = Env\n" ^
      "push rbx           ;; push env\n" ^
      "CLOSURE_CODE rax, rax      ;; rax = Code\n" ^
      "push OLD_RETURN_ADDRESS\n" ^
      "push qword [rbp]   ;;push old rbp\n" ^
      " ;; Fix thr stack -> shift frame\n" ^
      "SHIFT_FRAME " ^ (string_of_int ((List.length args) + 5)) ^ "\n" ^
      "pop rbp\n" ^
      "lea rsp, [rsp + WORD_SIZE*(rcx+4)]   ;; 4 = old rbp ,env,old ret, param_count\n" ^
      "jmp rax\n"
    in
    run e 0;;
end;;

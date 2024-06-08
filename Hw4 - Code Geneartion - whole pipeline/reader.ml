(* reader.ml
 * A skeleton for the reader for the 2021-2022 course on compiler-construction
 *)

#use "pc.ml";;

let rec gcd a b =
  match (a, b) with
  | (0, b) -> b
  | (a, 0) -> a
  | (a, b) -> gcd b (a mod b);;

type scm_number =
  | ScmRational of (int * int)
  | ScmReal of float;;

type sexpr =
  | ScmVoid
  | ScmNil
  | ScmBoolean of bool
  | ScmChar of char
  | ScmString of string
  | ScmSymbol of string
  | ScmNumber of scm_number
  | ScmVector of (sexpr list)
  | ScmPair of (sexpr * sexpr);;

module type READER = sig
    val nt_sexpr : sexpr PC.parser
end;; (* end of READER signature *)

module Reader : READER = struct
open PC;;

let unitify nt = pack nt (fun _ -> ());;

(*help functions*)
let nt_is_positive = 
    let nt1 = pack (char '+') (fun _ -> true) in
    let nt2 = pack (char '-') (fun _-> false) in
    let nt1 = disj nt1 nt2 in
    let nt1 = pack (maybe nt1) (function 
                                | None -> true
                                | Some b -> b) in
    nt1;;
let maybeify nt none_value = pack (maybe nt) (function
                            | None -> none_value
                            | Some x -> x);;
let smallest_fraction a b = 
    let _gcd = gcd a b in
    ScmRational(a/_gcd,b/_gcd);;

let make_nt_hex a f =
    let nt1 = word_ci "x" in
    let nt2 = range '0' '9' in
    let nt2 = pack nt2 ( let delta = int_of_char '0' in
                        fun ch -> (int_of_char ch) - delta) in
    let nt3 = range a f in
    let nt3 = pack nt3 ( let delta = int_of_char a - 10 in
                        fun ch -> (int_of_char ch) - delta) in
    let nt4 = pack (caten nt2 nt3) (fun(o,z) -> 16 * o + z) in 
    let nt2 = disj (disj nt2 nt3) nt4 in
    let nt2 = plus nt2 in
    let nt2 = pack nt2 (fun digits -> List.fold_left (fun a b -> 16 * a + b) 0 digits) in
    let nt1 = caten nt1 nt2 in
    let nt1 = pack nt1 (fun (_,n) -> n)in
    let nt1 = only_if nt1 (fun n-> n>=0 && n<=255) in
    nt1;;
(*---------------*)
(*my_parsers*)
let positive_char = char '+';;
let negative_char = char '-';;
let pos_or_neg = disj positive_char negative_char;;
let maybe_pos_or_neg = maybe pos_or_neg;;
let natural_numbers = plus (range '0' '9');;
let slash_char = char '/';;
let b_slash_char = char (char_of_int 92);;
let double_q_char = char (char_of_int 34);;
let tilda_char = char (char_of_int 126);;
(*Meta chars *)
let db_slash_char = pack (word_ci "\\\\") (fun _ -> '\\');;
let b_slash_q_char = pack (word_ci "\\\"") (fun _ ->  '\"');;
let tab_char = pack (word_ci "\\t") (fun _-> '\t');;
let f_char = pack (word_ci "\\f") (fun _-> '\012');;
let newline_char = pack (word_ci "\\n") (fun _-> '\n');;
let return_char = pack (word_ci "\\r") (fun _-> '\r');;
let double_tilda_char = pack (word_ci "~~") (fun _-> '~');;
(************)
let open_bracket = char '(';;
let close_bracket = char ')';;
let dot_char = char '.';;
(*---------------*)
let rec nt_whitespace str =
  const (fun ch -> ch <= ' ') str
and nt_end_of_line_or_file str =
  let nt1 = unitify (char '\n') in
  let nt2 = unitify nt_end_of_input in
  let nt1 = disj nt1 nt2 in
  nt1 str
and nt_line_comment str =
  let _start_comment_ = char ';' in
  let _pure_comment_ = star (diff nt_any nt_end_of_line_or_file) in
  let _comment_ = caten (caten _start_comment_ _pure_comment_) nt_end_of_line_or_file in
  let _comment_ = unitify _comment_ in
  _comment_ str
and nt_paired_comment str =
  let open_b = char '{' in
  let close_b = char '}' in
  let option_1 = disj_list[(unitify nt_char);(unitify nt_string);nt_comment] in
  let o1_or_boc = disj option_1 (unitify (one_of "{}")) in 
  let option_2 = unitify (diff nt_any o1_or_boc) in
  let options = disj option_2 option_1 in
  let options = star options in
  let paired = unitify (caten open_b (caten options close_b)) in
  paired str
and nt_sexpr_comment str = 
  let nt1 = word "#;" in
  let nt1 = unitify (caten nt1 nt_sexpr) in
  nt1 str
and nt_comment str =
  disj_list
    [nt_line_comment;
     nt_paired_comment;
     nt_sexpr_comment] str
and nt_skip_star str =
  let nt1 = disj (unitify nt_whitespace) nt_comment in
  let nt1 = unitify (star nt1) in
  nt1 str
and make_skipped_star (nt : 'a parser) =
  let nt1 = caten nt_skip_star (caten nt nt_skip_star) in
  let nt1 = pack nt1 (fun (_, (e, _)) -> e) in
  nt1
and nt_int str =
  let nt_nat = pack natural_numbers (fun n_list -> (int_of_string (list_to_string n_list))) in
  let _digits_ = caten nt_is_positive nt_nat in
  let _integer_ = pack _digits_ (fun (is_positive, n) -> if is_positive then  n else (-n)) in
  _integer_ str
and nt_frac str = 
  let not_zero = fun n -> (int_of_string (list_to_string n)) != 0 in
  let n1 = caten nt_int slash_char in
  let n1 = pack n1 (fun (num,s) -> num) in
  let n1 = pack (caten n1 (only_if natural_numbers not_zero)) (fun (u,n_list) -> (u, (int_of_string (list_to_string n_list)))) in
  let n1  = pack n1 (fun (u,d) -> smallest_fraction u d) in
  n1 str
and nt_integer_part str = pack natural_numbers (fun n -> float_of_string (list_to_string n)) str
and nt_mantissa str = pack natural_numbers (fun n -> float_of_string ("0."^(list_to_string n))) str
and nt_exponent str =
  let pref = disj_list [(word_ci "e"); (word_ci "*10^") ; (word_ci "*10**")] in
  let nt1 = pack (caten pref nt_int) (fun (_,n) -> float_of_int n) in
  nt1 str
and nt_float str =
  let nt_dot = word "." in
  let man = maybeify nt_mantissa 0.0 in
  let exp = maybeify nt_exponent 0.0 in
  let float_a = caten (caten (caten nt_integer_part nt_dot) man) exp in
  let float_a = pack float_a (fun (((i,_),m),e) -> (i +. m) *. (10. ** e)) in
  let float_b = caten (caten nt_dot nt_mantissa) exp in
  let float_b = pack float_b (fun ((_,m),e) -> m *. (10. ** e)) in
  let float_c = caten nt_integer_part nt_exponent in
  let float_c = pack float_c (fun (n,e) -> n *. (10. ** e)) in
  let f = disj_list [float_a;float_b;float_c] in
  let f = pack (caten nt_is_positive f) (fun (is_pos,n) -> if is_pos then ScmReal(n) else ScmReal(-.n)) in
  f str
and nt_number str = 
  let nt1 = nt_float in
  let nt2 = nt_frac in
  let nt3 = pack nt_int (fun n -> ScmRational(n, 1)) in
  let nt1 = disj nt1 (disj nt2 nt3) in
  let nt1 = pack nt1 (fun r -> ScmNumber r) in
  let nt1 = not_followed_by nt1 nt_symbol_char in
  nt1 str
and nt_boolean str =
  let n1 = pack (word_ci "#f") (fun _ -> false) in
  let n2 = pack (word_ci "#t") (fun _ -> true) in
  let n1 = disj n1 n2 in
  let n1 = not_followed_by n1 nt_symbol_char in
  let n1 = pack n1 (fun b -> ScmBoolean(b)) in
  n1 str
and nt_char_simple str = 
  let nt1 = pack (range '!' '~') (fun c -> ScmChar(c)) in
  let nt1 = not_followed_by nt1 nt_symbol_char in
  nt1 str
and make_named_char char_name ch = (pack (word_ci char_name) (fun _ -> ScmChar ch))
and nt_char_named str =
  let nt1 =
      disj_list [(make_named_char "newline" '\n');
                  (make_named_char "nul" '\000');
                  (make_named_char "page" '\012');
                  (make_named_char "return" '\r');
                  (make_named_char "space" ' ');
                  (make_named_char "tab" '\t')] in
  nt1 str
and nt_char_hex str =
    let nt1 = disj (make_nt_hex 'a' 'f') (make_nt_hex 'A' 'F') in
    let nt1 = pack nt1 (fun ch -> char_of_int ch) in
    nt1 str
and nt_char str =
  let _cp_ = word "#\\" in
  let _cp_visible_simple = pack (caten _cp_ nt_char_simple) (fun (cp,sc)-> sc) in
  let _cp_named_char = pack (caten _cp_ nt_char_named) (fun (cp,nc) -> nc) in
  let _cp_hex_char = pack (caten _cp_ nt_char_hex) (fun (cp,ch) -> ScmChar ch) in
  let nt = disj_list [_cp_hex_char;_cp_named_char;_cp_visible_simple] in
  nt str
and nt_symbol_char str = 
  let _digits_ = range '0' '9' in
  let _a_z_A_Z = range_ci 'a' 'z' in
  let _symbol_char = disj_list [_digits_;_a_z_A_Z;
                                (char '!');(char '$');
                                (char '^');(char '*');
                                (char '-');(char '_');
                                (char '=');(char '+');
                                (char '<');(char '>');
                                (char '?');(char '/');
                                (char ':')] in
  _symbol_char str
and nt_symbol str =
  let nt1 = plus nt_symbol_char in
  let nt1 = pack nt1 list_to_string in
  let nt1 = pack nt1 (fun name -> ScmSymbol name) in
  let nt1 = diff nt1 nt_number in
  nt1 str
and nt_dynamicString str = 
  let nt1 = word "~{" in
  let nt2 = nt_sexpr in
  let nt3 = word "}" in
  let nt1 = caten (caten nt1 nt2) nt3 in
  let nt1 = pack nt1 (fun ((_,sexpr),_) -> ScmPair (ScmSymbol "format", ScmPair (ScmString "~a", ScmPair (sexpr,ScmNil)))) in
  nt1 str
and nt_staticString str = 
  let s_lit_ch = disj_list [(char '\\');(char '"');(char '~')] in
  let s_lit_ch = diff nt_any s_lit_ch in
  let s_meta_ch = disj_list [db_slash_char;b_slash_q_char;tab_char;f_char;newline_char;return_char;double_tilda_char] in
  let s_hex_ch = pack (caten (caten (char '\\') nt_char_hex) (char ';')) (fun ((_,ch),_) -> ch) in
  let static =  plus (disj_list [s_meta_ch;s_lit_ch;s_hex_ch]) in
  let static = pack static (fun ch_list -> ScmString (list_to_string ch_list)) in 
  static str
and nt_string str = 
  let nt_s = disj nt_dynamicString nt_staticString in
  let nt_s = star nt_s in
  let nt_s = caten (caten (char '"') nt_s) (char '"') in
  let nt_s = pack nt_s (fun ((_,lst),_) -> match lst with
                                  |[] -> ScmString ""
                                  |[x] -> x
                                  |_ ->
                                    let res = List.fold_right (fun a b -> ScmPair(a,b)) lst ScmNil in
                                     ScmPair(ScmSymbol "string-append",res)) in
  nt_s str
and nt_vector str =
  let nt1 = word "#(" in
  let nt2 = caten nt_skip_star (char ')') in
  let nt2 = pack nt2 (fun _ -> ScmVector []) in
  let nt3 = plus nt_sexpr in
  let nt4 = char ')' in
  let nt3 = caten nt3 nt4 in
  let nt3 = pack nt3 (fun (sexprs, _) -> ScmVector sexprs) in
  let nt2 = disj nt2 nt3 in
  let nt1 = caten nt1 nt2 in
  let nt1 = pack nt1 (fun (_, sexpr) -> sexpr) in
  nt1 str
and nt_list str =
  let make_pair x y = ScmPair(x,y) in 
  let skipped = pack (caten open_bracket (caten nt_skip_star close_bracket)) (fun _ -> ScmNil) in
  let proper_list = caten (caten open_bracket (star nt_sexpr)) close_bracket in
  let proper_list = pack proper_list (fun ((o,sexprs),c)-> List.fold_right make_pair sexprs ScmNil) in
  let improper_list = caten (caten (caten (caten open_bracket (plus nt_sexpr)) dot_char) nt_sexpr) close_bracket in
  let improper_list = pack improper_list (fun ((((o,sexp_plus),dot),sexp),c)-> List.fold_right make_pair sexp_plus sexp) in
  let nt_l = disj_list [skipped;proper_list;improper_list] in
  nt_l str
and nt_quoted_forms str = 
  let _quote = pack (caten (char (char_of_int 39)) nt_sexpr) (fun (q,sexpr) -> ScmPair(ScmSymbol "quote",ScmPair(sexpr,ScmNil))) in
  let _quasi_q = pack (caten (char '`') nt_sexpr) (fun (q,sexpr) -> ScmPair(ScmSymbol "quasiquote",ScmPair(sexpr,ScmNil))) in
  let _unquoted = pack (caten (char ',') nt_sexpr) (fun (c,sexpr) -> ScmPair(ScmSymbol "unquote",ScmPair(sexpr,ScmNil))) in
  let _unquoted_spl = pack (caten (word_ci ",@") nt_sexpr) (fun (c,sexpr) -> ScmPair(ScmSymbol "unquote-splicing",ScmPair(sexpr,ScmNil))) in
  let all = disj_list [_quote;_quasi_q;_unquoted;_unquoted_spl] in
  all str
and nt_sexpr str =
  let nt1 =
    disj_list [nt_number; nt_boolean; nt_char; nt_symbol;
               nt_string; nt_vector; nt_list; nt_quoted_forms] in
  let nt1 = make_skipped_star nt1 in
  nt1 str;;
end;; (* end of struct Reader  *)


let rec string_of_sexpr = function
  | ScmVoid -> "#<void>"
  | ScmNil -> "()"
  | ScmBoolean(false) -> "#f"
  | ScmBoolean(true) -> "#t"
  | ScmChar('\n') -> "#\\newline"
  | ScmChar('\r') -> "#\\return"
  | ScmChar('\012') -> "#\\page"
  | ScmChar('\t') -> "#\\tab"
  | ScmChar(' ') -> "#\\space"
  | ScmChar(ch) ->
     if (ch < ' ')
     then let n = int_of_char ch in
          Printf.sprintf "#\\x%x" n
     else Printf.sprintf "#\\%c" ch
  | ScmString(str) ->
     Printf.sprintf "\"%s\""
       (String.concat ""
          (List.map
             (function
              | '\n' -> "\\n"
              | '\012' -> "\\f"
              | '\r' -> "\\r"
              | '\t' -> "\\t"
              | ch ->
                 if (ch < ' ')
                 then Printf.sprintf "\\x%x;" (int_of_char ch)
                 else Printf.sprintf "%c" ch)
             (string_to_list str)))
  | ScmSymbol(sym) -> sym
  | ScmNumber(ScmRational(0, _)) -> "0"
  | ScmNumber(ScmRational(num, 1)) -> Printf.sprintf "%d" num
  | ScmNumber(ScmRational(num, -1)) -> Printf.sprintf "%d" (- num)
  | ScmNumber(ScmRational(num, den)) -> Printf.sprintf "%d/%d" num den
  | ScmNumber(ScmReal(x)) -> Printf.sprintf "%f" x
  | ScmVector(sexprs) ->
     let strings = List.map string_of_sexpr sexprs in
     let inner_string = String.concat " " strings in
     Printf.sprintf "#(%s)" inner_string
  | ScmPair(ScmSymbol "quote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "'%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "quasiquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "`%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote-splicing",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",@%s" (string_of_sexpr sexpr)
  | ScmPair(car, cdr) ->
     string_of_sexpr' (string_of_sexpr car) cdr
and string_of_sexpr' car_string = function
  | ScmNil -> Printf.sprintf "(%s)" car_string
  | ScmPair(cadr, cddr) ->
     let new_car_string =
       Printf.sprintf "%s %s" car_string (string_of_sexpr cadr) in
     string_of_sexpr' new_car_string cddr
  | cdr ->
     let cdr_string = (string_of_sexpr cdr) in
     Printf.sprintf "(%s . %s)" car_string cdr_string;;


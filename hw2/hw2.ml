type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal
;;

let rec convert_rules nt rule = 
    match rule with
    | [] -> []
    | (lhs, rhs)::t -> 
        if lhs = nt then rhs::convert_rules nt t
        else convert_rules nt t
;;

let rec convert_grammar gram1 = 
    match gram1 with 
    | (start, rule) -> (start, fun nt -> convert_rules nt rule )  
;;

let test_gram gram2 = 
    match gram2 with
    | (start, rule) -> rule start
;;

(* (Expr,
    function
    | Expr ->
        [[N Term; N Binop; N Expr];
        [N Term]]
    ...
    | Num ->
        [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
        [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]) *)

let rec matcher start rule rule_fun accept d frag = 
    match rule_fun with
    | [] -> None
    | h_rule::t_rule -> 
        let match_suffix = match_rule start rule h_rule accept (d@[start, h_rule]) frag in
        match match_suffix with
        | None -> matcher start rule t_rule accept d frag
        | Some acc -> Some acc

and match_rule start rule rule_out accept d frag = 
    match rule_out with
    | [] -> accept d frag
    | _  -> 
        match frag with 
        | [] -> None
        | h_frag::t_frag -> 
            match rule_out with
            | [] -> None
            | (T h_rule)::t_rule -> 
                if h_rule <> h_frag then None
                else match_rule start rule t_rule accept d t_frag
            | (N h_rule)::t_rule -> 
                let update_accept = match_rule start rule t_rule accept in 
                matcher h_rule rule (rule h_rule) update_accept d frag
;;

(* (Expr,
    function
    | Expr ->
        [[N Term; N Binop; N Expr];
        [N Term]]
    ...
    | Num ->
        [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
        [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]) *)

let parse_prefix gram accept frag =
    let d = [] in
    match gram with 
    | (start, rule) -> matcher start rule (rule start) accept d frag 
;; 

(* parse_prefix awkish_grammar accept_all ["9"; "+"; "$"; "1"; "+"] *)


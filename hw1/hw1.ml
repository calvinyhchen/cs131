let rec subset a b =
    match a with 
    | [] -> true
    | head::rest -> List.exists ( fun x -> x = head ) b && subset rest b ;;

let equal_sets a b =
    subset a b && subset b a ;;

let set_union a b =
    List.append a b ;;

let rec set_intersection a b = 
    match a with
    | [] -> []
    | head::rest -> 
        if List.exists ( fun x -> x = head ) b then head::set_intersection rest b 
        else set_intersection rest b ;;

let rec set_diff a b = 
    match a with
    | [] -> []
    | head::rest ->
        if List.exists( fun x -> x = head ) b then set_diff rest b
        else head::set_diff rest b ;; 

let rec computed_fixed_point eq f x =
    match eq (f x) x with
    | true -> x
    | false -> computed_fixed_point eq f (f x) ;;


let rec recfunc f p x = 
    match p with
    | 0 -> x 
    | _ -> recfunc f (p-1) (f x) ;;

let rec computed_periodic_point eq f p x =
    match p with 
    | 0 -> x
    | _ -> 
        if eq (recfunc f p x) x then x
        else computed_periodic_point eq f p (f x) ;;

let rec while_away s p x = 
    match p x with
    | false -> []
    | true -> x::while_away s p (s x)  ;;

let rec rle_decode lp = 
    match lp with
    | [] -> []
    | (lhs, rhs)::rest -> 
        match lhs with
         | 0 -> rle_decode rest
         | _ -> rhs::rle_decode (((lhs-1), rhs)::rest) ;;

type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal ;;


let is_terminal rule = 
    match rule with
    | T _ -> true
    | N _ -> false ;;

let rec all_terminal rules = 
    match rules with
    | [] -> true
    | head::rest ->
        if is_terminal head then all_terminal rest
        else false ;;

let rec build_terminal g = 
    match g with
    | [] -> []
    | head::rest ->
        if all_terminal (snd head) then head::build_terminal rest
        else build_terminal rest ;;

let rec find remain_rule terminating_rules = 
    match terminating_rules with
    | [] -> false
    | head::rest -> 
        if remain_rule = head then true
        else find remain_rule rest ;;


let rec remain_non rules =
    match rules with
    | [] -> []
    | head::rest -> (N (fst head))::remain_non rest ;;

let rec all_nonterminal rhs = 
    match rhs with 
    | [] -> []
    | head::rest ->
        if is_terminal head then all_nonterminal rest
        else head::all_nonterminal rest ;;

let rec check_rhs rhs terminal_rules = 
    let non_terminal = all_nonterminal rhs in
    let remain_lhs = remain_non terminal_rules in
    subset non_terminal remain_lhs ;;

let rec update_rules remain_rules terminal_rules = 
    match remain_rules with
    | [] -> terminal_rules
    | head::rest -> 
        match find head terminal_rules with
        | true -> update_rules rest terminal_rules
        | false -> 
            if check_rhs (snd head) terminal_rules then update_rules rest (head::terminal_rules)
            else update_rules rest terminal_rules ;;

let rec sort_rules origin_rules terminals =
    match origin_rules with
    | [] -> []
    | head::rest ->
    if find head terminals then head::sort_rules rest terminals
    else sort_rules rest terminals ;;

let rec rec_rules unknown_rules terminal_rules origin_rules= 
    let terminals = update_rules unknown_rules terminal_rules in
    match equal_sets terminals terminal_rules with
    | true -> sort_rules origin_rules terminals
    | false -> rec_rules unknown_rules terminals origin_rules;;

let filter_blind_alleys g = 
    let terminal_rules = build_terminal (snd g) in
    let remain_rules = set_diff (snd g) terminal_rules in
    (fst g), rec_rules remain_rules terminal_rules (snd g)  ;;


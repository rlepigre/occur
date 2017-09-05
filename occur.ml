module S = Set.Make(String)
type words = S.t

let print_words : out_channel -> words -> unit = fun oc ws ->
  let first = ref true in
  let print w = 
    if !first then
      begin
        first := false;
        Printf.fprintf oc "%s" (if w = "" then "." else w)
      end
    else Printf.fprintf oc " %s" (if w = "" then "." else w)
  in
  S.iter print ws

type name = string

let l = "l"
let r = "r"

type ty =
  | B of name
  | F of ty * ty

let names : ty -> name list =
  fun t ->
    let rec names acc t =
      match t with
      | B(x)   -> x::acc
      | F(u,v) -> names (names acc u) v
    in
    List.sort_uniq String.compare (names [] t)

(* Pos(B,C) *)
let rec pos : name -> ty -> words =
  fun b t ->
    match t with
    | B(c) when c = b -> S.singleton ""
    | B(c) (* c<>b *) -> S.empty
    | F(u,v)          ->
        let su = S.fold (fun p s -> S.add (l ^ p) s) (pos b u) S.empty in
        let sv = S.fold (fun p s -> S.add (r ^ p) s) (pos b v) S.empty in
        S.union su sv

let rec pos_p : ty -> words =
  function
  | B(_)   -> S.singleton ""
  | F(u,v) ->
      let su = S.fold (fun p s -> S.add (l ^ p) s) (pos_m u) S.empty in
      let sv = S.fold (fun p s -> S.add (r ^ p) s) (pos_p v) S.empty in
      S.union su sv

    and pos_m : ty -> words =
  function
  | B(_)   -> S.empty
  | F(u,v) ->
      let su = S.fold (fun p s -> S.add (l ^ p) s) (pos_p u) S.empty in
      let sv = S.fold (fun p s -> S.add (r ^ p) s) (pos_m v) S.empty in
      S.union su sv

let parser ty (p : [`A | `F]) =
  | b:''[a-z]''              when p = `A -> B(b)
  | '(' t:(ty `F) ')'        when p = `A -> t
  | u:(ty `A) "->" v:(ty `F) when p = `F -> F(u,v)
  | t:(ty `A)                when p = `F -> t
let ty = ty `F

let parse : string -> ty =
  fun s ->
    try Earley.parse_string ty (Earley.blank_regexp ''[ ]*'') s
    with _ -> Printf.eprintf "Parse error...\n"; exit (-1)

let work : (ty -> S.t) -> string -> unit =
  fun f s ->
    print_words stdout (f (parse s));
    Printf.printf "\n%!"

let red   : string -> string =
  fun s -> "\027[31m" ^ s ^ "\027[0m"

let green : string -> string =
  fun s -> "\027[32m" ^ s ^ "\027[0m"

let print : out_channel -> ty -> unit =
  fun oc t ->
    let pos = pos_p t in
    let neg = pos_m t in
    let rec print path oc t =
      match t with
      | B(x) when S.mem path pos ->
          output_string oc (green x)
      | B(x) when S.mem path neg ->
          output_string oc (red x)
      | B(x) (* invariant *)     ->
          output_string oc x
      | F(u,v)                   ->
          let (lp,rp) = match u with B(_) -> ("","") | F(_,_) -> ("(",")") in
          Printf.fprintf oc "%s%a%s -> %a"
            lp (print (path^l)) u rp (print (path^r)) v
    in
    print "" oc t

let report_list : string -> unit =
  fun s ->
    let t = parse s in
    let show c =
      let all_c = pos c t in
      let c_pos = S.inter all_c (pos_p t) in
      let c_neg = S.inter all_c (pos_m t) in
      if not (S.is_empty c_pos) then
        begin
          Printf.printf "%s+: " c;
          print_words stdout c_pos;
          Printf.printf "\n%!";
        end;
      if not (S.is_empty c_neg) then
        begin
          Printf.printf "%s-: " c;
          print_words stdout c_neg;
          Printf.printf "\n%!";
        end
    in
    List.iter show (names t)

let report : string -> unit =
  fun s -> Printf.printf "%a\n" print (parse s)

let _ =
  match Sys.argv with
  | [|_;"+";s|]                          -> work pos_p s
  | [|_;"-";s|]                          -> work pos_m s
  | [|_;c  ;s|] when String.length c = 1 -> work (pos c) s
  | [|_;"-l";s|]                         -> report_list s
  | [|_;s|]                              -> report s
  | _                                    ->
      let p = Sys.argv.(0) in
      Printf.eprintf "Usage:\n";
      Printf.eprintf "  %s C TYPE   # positions of C in TYPE\n%!"      p;
      Printf.eprintf "  %s + TYPE   # positive positions in TYPE\n%!"  p;
      Printf.eprintf "  %s - TYPE   # negative positions in TYPE\n%!"  p;
      Printf.eprintf "  %s -l TYPE  # list variance of occurences\n%!" p;
      Printf.eprintf "  %s TYPE     # show variance using colors\n%!"  p;
      Printf.eprintf "  %s -h       # show this usage message\n%!"     p

(** Emit Typst from an AST *)

open Ast

let map_concat (f : 'a -> string) (ll : 'a list list) =
  List.map (fun l -> String.concat "" l) (List.map (fun l -> List.map f l) ll)

let concat_extra_args (f : 'a -> string) (extra : 'a list list) = 
  List.map (fun arg -> "[" ^ arg ^ "]") (map_concat f extra)

let emit_expr debug e =
  let rec aux e =
    match e with
    | Text s ->
      if debug then
        Printf.printf "Emitting text \"%s\"\n" s;
      s
    | Subscript subscripts ->
      let subs_str = String.concat "" (List.map aux subscripts) in
      if subs_str = "" then "_" else "_(" ^ subs_str ^ ")"
    | Superscript superscripts ->
      let supers_str = String.concat "" (List.map aux superscripts) in
      if supers_str = "" then "^" else "^(" ^ supers_str ^ ")"
    | Quote content ->
      let content_str = String.concat "" (List.map aux content) in
      if debug then
        Printf.printf "Emitting quote with content \"%s\"\n" content_str;
      "'" ^ content_str ^ "'"
    | DoubleQuote content ->
      let content_str = String.concat "" (List.map aux content) in
      if debug then
        Printf.printf "Emitting double quote with content \"%s\"\n" content_str;
      "\"" ^ content_str ^ "\""
    | Func (("cite" | "citet" | "citep" | "ref" | "cref" | "Cref" | "eqref"), refs::extra) ->
      let refs_str = "@" ^ String.concat "" (List.map aux refs) 
        |> String.fold_left
          (fun acc s -> if s <> ' ' then acc ^ (String.make 1 s) else acc) ""
        |> String.split_on_char ','
        |> String.concat " @"
      in
      let extra_str = concat_extra_args aux extra in
      if debug then
        Printf.printf "Emitting citation \"%s\"\n" refs_str;
      refs_str ^ String.concat "" extra_str
    | Func ("frac", args) ->
      let str_args = map_concat aux args in
      let (num_str, den_str) = match str_args with
       | num_str :: den_str :: _ -> (num_str, den_str)
       | _ -> failwith "Error: \\frac requires two arguments"
      in
      if debug then
        Printf.printf "Emitting fraction with numerator \"%s\" and denominator \"%s\"\n"
          num_str den_str;
      "(" ^ num_str ^ ") / (" ^ den_str ^ ")"
    | Func ("label", label) ->
      let label_str = String.concat "" (map_concat aux label) in
      if debug then
        Printf.printf "Emitting label \"%s\"\n" label_str;
      "<" ^ label_str ^ ">"
    | Func ("mathbb", [arg]::extra) ->
      let arg_str = aux arg in
      let extra_str = concat_extra_args aux extra in
      if debug then
        Printf.printf "Emitting mathbb with argument \"%s\"\n" arg_str;
      if String.length arg_str = 1 then
        arg_str^arg_str^(String.concat "" extra_str)
      else
        "bb(" ^ arg_str ^ ")"^(String.concat "" extra_str)
    | Func ("textbf", args) ->
      let args_str = String.concat "" (map_concat aux args) in
      if debug then
        Printf.printf "Emitting textbf with content \"%s\"\n" args_str;
      "*" ^ args_str ^ "*"
    | Func ("textit", args) ->
      let args_str = String.concat "" (map_concat aux args) in
      if debug then
        Printf.printf "Emitting textit with content \"%s\"\n" args_str;
      "_" ^ args_str ^ "_"
    | Func ("text", args) ->
      let args_str = String.concat "" (map_concat aux args) in
      if debug then
        Printf.printf "Emitting textit with content \"%s\"\n" args_str;
      "\"" ^ args_str ^ "\""
    | Func (name, args) ->
      let (name, n_args) = match Typst_dict.lookup name with
        | Some (t, n_args) -> t, n_args
        | None -> name, None
      in
      (match args with
      | [] ->
        if debug then
          Printf.printf "Emitting func \"%s\"\n" name;
        name
      | args ->
        let str_args = map_concat aux args in
        let args_str = match n_args with
          | None ->
              "(" ^ String.concat ", " str_args ^ ")"
          | Some 0 ->
            let args_str = List.map (fun arg -> "[" ^ arg ^ "]") str_args in
            String.concat "" args_str
          | Some n ->
            let real_args = List.take n str_args in
            let extra_args = List.drop n str_args in
            let real_args_str = "(" ^ String.concat ", " real_args ^ ")" in
            let extra_args_str = List.map (fun arg -> "[" ^ arg ^ "]") extra_args in
            real_args_str ^ String.concat "" extra_args_str
        in
        if debug then
          Printf.printf "Emitting func \"%s\" with args \"%s\"\n" name args_str;
        name ^ args_str)
    | Env (("equation" | "equation*" | "align" | "align*"), [], content) ->
      let content_str = String.concat "" (List.map aux content) in
      if debug then
        Printf.printf "Emitting math environment with content \"%s\"\n" content_str;
      "$" ^ content_str ^ "$"
    | Env (name, args, content) ->
      let content_str = String.concat "" (List.map aux content) in
      (match args with
      | [] ->
        if debug then
          Printf.printf "Emitting environment \"%s\"\n" name;
        "#" ^ name ^ "([" ^ content_str ^ "])"
      | args ->
        let str_args = map_concat aux args in
        let args_str = String.concat ", " (List.map (fun arg -> "[" ^ arg ^ "]") str_args) in
        if debug then
          Printf.printf "Emitting environment \"%s\" with args \"%s\"\n" name args_str;
        "#" ^ name ^ "(" ^ args_str ^ ", " ^ "[" ^ content_str ^ "])")
  in
  aux e

let emit debug exprs =
  List.fold_left (fun acc e -> acc ^ emit_expr debug e) "" exprs

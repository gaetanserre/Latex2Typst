let typst_equiv ?(n_args : int = 0) name =
  (name, n_args)

let builtin : (string, string * int) Hashtbl.t = Hashtbl.create 64

let () =
  List.iter (fun (k, v) -> Hashtbl.add builtin k v)
  [
    "int", typst_equiv "integral";
    "iint", typst_equiv "integral.double";
    "to", typst_equiv "->";
    "rightarrow", typst_equiv "->";
    "gets", typst_equiv "<-";
    "leftarrow", typst_equiv "<-";
    "rightsquigarrow", typst_equiv "~~>";
    "leftsquigarrow", typst_equiv "<~~";
    "infty", typst_equiv "oo";
    "mapsto", typst_equiv "|->";
    "leq", typst_equiv "<=";
    "le", typst_equiv "<";
    "geq", typst_equiv ">=";
    "ge", typst_equiv ">=";
    "neq", typst_equiv "!=";
    "ne", typst_equiv "!=";
    "coloneq", typst_equiv ":=";
    "iff", typst_equiv "<==>";
    "implies", typst_equiv "=>";
    "impliedby", typst_equiv "<=";
    "sim", typst_equiv "tilde";
    "propto", typst_equiv"prop";
    "subseteq", typst_equiv "subset.eq";
    "mathcal", typst_equiv "cal" ~n_args:1;
    "mathbb", typst_equiv "bb" ~n_args:1;
    "mathfrak", typst_equiv "frak" ~n_args:1;
    "mathbf", typst_equiv "bold" ~n_args:1;
    "mathsf", typst_equiv "sans" ~n_args:1;
    "cdot", typst_equiv "dot";
    "ldots", typst_equiv "...";
    "cdots", typst_equiv "dots.h.c";
    "vdots", typst_equiv "dots.v";
    "ddots", typst_equiv "dots.down";
    "iddots", typst_equiv "dots.up";
    "cap", typst_equiv "inter";
    "cup", typst_equiv "union";
    "notin", typst_equiv "in.not";
    "ni", typst_equiv"in.rev";
    "lVert", typst_equiv "||";
    "rVert", typst_equiv "||";
    "ll", typst_equiv "<<";
    "gg", typst_equiv ">>";
    "middle", typst_equiv "mid";
    "land", typst_equiv "and";
    "lor", typst_equiv"or";
    "circ", typst_equiv "compose";
    "hfill", typst_equiv "#h(1fr)";
    "vfill", typst_equiv "#v(1fr)";
    "blacksquare", typst_equiv "qed";
    "N", typst_equiv"NN";
    "Z", typst_equiv "ZZ";
    "R", typst_equiv "RR";
    "C", typst_equiv "CC";
    "Q", typst_equiv "QQ";
    "left", typst_equiv "";
    "right", typst_equiv "";
    "Big", typst_equiv "";
    "big", typst_equiv "";
    "bigg", typst_equiv "";
    "Bigg", typst_equiv "";
    "Bigl", typst_equiv "";
    "bigl", typst_equiv "";
    "biggl", typst_equiv "";
    "Biggl", typst_equiv "";
    "Bigr", typst_equiv "";
    "bigr", typst_equiv "";
    "biggr", typst_equiv "";
    "Biggr", typst_equiv "";
    "#", typst_equiv "hash";
    "otimes", typst_equiv "times.o";
    "vec", typst_equiv "arrow" ~n_args:1;
  ]

let custom : (string, string * int option) Hashtbl.t = Hashtbl.create 16

let load_custom path =
  let ic = open_in path in
  (try
    while true do
      let line = String.trim (input_line ic) in
      if line <> "" && line.[0] <> '#' then
        match String.split_on_char ' ' line
              |> List.filter (fun s -> s <> "") with
        | latex :: typst :: rest ->
          let n_args = 
            match rest with
            | n_str :: _ -> (try Some (int_of_string n_str) with _ -> None)
            | [] -> None
          in
          Hashtbl.replace custom latex (typst, n_args)
        | _ ->
          Printf.eprintf "Warning: ignoring malformed line in custom file: %s\n" line
    done
  with End_of_file -> ());
  close_in ic

let default_custom_file = "custom_latex_commands.txt"

let lookup cmd =
  match Hashtbl.find_opt custom cmd with
  | Some _ as r -> r
  | None ->
    match Hashtbl.find_opt builtin cmd with
    | Some (s, n_args) -> Some (s, Some n_args)
    | None -> None


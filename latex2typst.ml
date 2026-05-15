(** typst2latex – entry point *)

let usage = {|
Usage: typst2latex [OPTIONS] <file.tex>

Options:
  -c <path>   Use a custom command mapping file (default: "custom_latex_commands.txt")
  -h          Show this help message
  -d          Enable debug output
|}

let () =
  let custom_file = ref None in
  let input_file  = ref None in
  let debug       = ref false in

  let rec parse = function
    | [] -> ()
    | "-h" :: _ ->
      print_string usage; exit 0
    | "-d" :: rest ->
      debug := true;
      parse rest
    | "-c" :: path :: rest ->
      custom_file := Some path;
      parse rest
    | "-c" :: [] ->
      Printf.eprintf "Error: -c requires a file path argument\n%s" usage;
      exit 1
    | arg :: _ when String.length arg > 0 && arg.[0] = '-' ->
      Printf.eprintf "Unknown option: %s\n%s" arg usage;
      exit 1
    | path :: rest ->
      (match !input_file with
       | None   -> input_file := Some path
       | Some _ -> Printf.eprintf "Error: multiple entry files specified\n"; exit 1);
      parse rest
  in
  parse (List.tl (Array.to_list Sys.argv));

  let input_path = match !input_file with
    | Some p -> p
    | None   -> Printf.eprintf "Error: no entry file specified\n%s" usage; exit 1
  in

  (match !custom_file with
   | Some path ->
     if Sys.file_exists path then
       Typst_dict.load_custom path
     else begin
       Printf.eprintf "Error: unknown custom commands file: %s\n" path;
       exit 1
     end
   | None ->
     if Sys.file_exists Typst_dict.default_custom_file then
       Typst_dict.load_custom Typst_dict.default_custom_file);

  let input =
    let ic = open_in input_path in
    let n  = in_channel_length ic in
    let s  = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    Bytes.to_string s
  in

  let lexbuf   = Sedlexing.Utf8.from_string input in
  
  (* Incremental API with token display *)
  let module I = Parser.MenhirInterpreter in
  let token_count = ref 0 in
  
  let rec loop checkpoint =
    match checkpoint with
    | Parser.MenhirInterpreter.InputNeeded _env ->
        let tok = Lexer.token !debug lexbuf in
        incr token_count;
        let startp = fst (Sedlexing.lexing_positions lexbuf)
        and endp = snd (Sedlexing.lexing_positions lexbuf) in
        let checkpoint = I.offer checkpoint (tok, startp, endp) in
        loop checkpoint
    | Shifting (_current, _next, _) ->
        let checkpoint = I.resume checkpoint in
        loop checkpoint
    | AboutToReduce (_env, _) ->
        let checkpoint = I.resume checkpoint in
        loop checkpoint
    | HandlingError env ->
        let state_num = I.current_state_number env in
        Printf.printf "[State %d] Handling error\n%!" state_num;
        let pos = fst (Sedlexing.lexing_positions lexbuf) in
        Printf.eprintf "Parse error at line %d, column %d after %d tokens\n"
          pos.Lexing.pos_lnum
          (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
          !token_count;
        exit 1
    | Accepted v -> v
    | Rejected -> exit 1
  in

  let ast =
    try
      let initial_pos = fst (Sedlexing.lexing_positions lexbuf) in
      let checkpoint = Parser.Incremental.prog initial_pos in
      loop checkpoint
    with Parser.Error ->
      let pos = fst (Sedlexing.lexing_positions lexbuf) in
      Printf.eprintf "Parse error at line %d, column %d\n"
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol);
      exit 1
  in
  print_string (Printer.emit !debug ast ^ "\n")
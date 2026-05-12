(** Sedlex-based lexer for LaTeX math expressions *)

open Parser

let token_impl lexbuf =
  match%sedlex lexbuf with

  | "\\begin" -> BEGIN

  | "\\end" -> END

  | '\\' -> TEXT ""

  | '\\', Plus (Compl ('{' | '}' | '[' | ']' | '(' | ')' | '\\' | ' ' | '_' | '^'
    | ',' | ';' | '.' | '$' | '|' | '\n')) ->
    let cmd = Sedlexing.Utf8.sub_lexeme lexbuf 1 (Sedlexing.lexeme_length lexbuf - 1) in
    FUNC cmd

  | '{' -> LBRACE
  | '}' -> RBRACE

  | '[' -> LBRACKET
  | ']' -> RBRACKET

  | "\\{" -> TEXT "{"
  | "\\}" -> TEXT "}"

  | "$$" -> DOLLARS

  | "\n" -> NEWLINE

  | "_" -> UNDERSCORE
  | "^" -> CARET

  | "\\\\" -> TEXT "\\"

  | Plus (Compl ('{' | '}' | '[' | ']' | '\\' | '$' | '\n' | '_' | '^')) ->
    TEXT (Sedlexing.Utf8.lexeme lexbuf)

  | eof -> EOF

  | any ->
    TEXT (Sedlexing.Utf8.lexeme lexbuf)

  | _ ->
    TEXT (Sedlexing.Utf8.lexeme lexbuf)

let print_token tok =
  match tok with
  | FUNC s -> Printf.sprintf "FUNC(%s)" s
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | DOLLARS -> "DOLLARS"
  | NEWLINE -> "NEWLINE"
  | UNDERSCORE -> "UNDERSCORE"
  | CARET -> "CARET"
  | BEGIN -> "BEGIN"
  | END -> "END"
  | EOF -> "EOF"
  | TEXT s -> Printf.sprintf "TEXT(%s)" s

let token debug lexbuf =
  let tok = token_impl lexbuf in
  if debug then
    Printf.printf "Lexed token: %s\n" (print_token tok);
  tok
(** Sedlex-based lexer for LaTeX math expressions *)

open Parser

let token_impl lexbuf =
  match%sedlex lexbuf with

  | "\\begin" -> BEGIN

  | "\\end" -> END

  | '\\' -> TEXT ""

  | '`' -> BACKTICK
  | '\'' -> APOSTROPHE

  | '\\', Plus (Compl ('{' | '}' | '[' | ']' | '(' | ')' | '\\' | ' ' | '_' | '^'
    | ',' | ';' | '.' | '$' | '|' | '\n' | '`' | '\'')) ->
    let cmd = Sedlexing.Utf8.sub_lexeme lexbuf 1 (Sedlexing.lexeme_length lexbuf - 1) in
    FUNC cmd

  | '{' -> LBRACE
  | '}' -> RBRACE

  | '[' -> LBRACKET
  | ']' -> RBRACKET

  | '(' -> LPAR
  | ')' -> RPAR

  | "\\{" -> TEXT "{"
  | "\\}" -> TEXT "}"

  | "$$" -> DOLLARS

  | "\\[" -> BEGIN_MATH_BLOCK
  | "\\]" -> END_MATH_BLOCK

  | "\\(" -> BEGIN_MATH
  | "\\)" -> END_MATH

  | "\n" -> NEWLINE

  | "_" -> UNDERSCORE
  | "^" -> CARET

  | "\\\\" -> TEXT "\\"

  | Plus (Compl ('{' | '}' | '[' | ']' | '\\' | '$' | '\n' | '_' | '^' | '`' | '\'' | '(' | ')')) ->
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
  | LPAR -> "LPAR"
  | RPAR -> "RPAR"
  | DOLLARS -> "DOLLARS"
  | BEGIN_MATH_BLOCK -> "BEGIN_MATH_BLOCK"
  | END_MATH_BLOCK -> "END_MATH_BLOCK"
  | BEGIN_MATH -> "BEGIN_MATH"
  | END_MATH -> "END_MATH"
  | NEWLINE -> "NEWLINE"
  | UNDERSCORE -> "UNDERSCORE"
  | CARET -> "CARET"
  | BACKTICK -> "BACKTICK"
  | APOSTROPHE -> "APOSTROPHE"
  | BEGIN -> "BEGIN"
  | END -> "END"
  | EOF -> "EOF"
  | TEXT s -> Printf.sprintf "TEXT(%s)" s

let token debug lexbuf =
  let tok = token_impl lexbuf in
  if debug then
    Printf.printf "Lexed token: %s\n" (print_token tok);
  tok
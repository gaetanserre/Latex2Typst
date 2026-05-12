%{
(* AST types *)
%}

%token <string> TEXT
%token <string> FUNC
%token LBRACE RBRACE LBRACKET RBRACKET
%token DOLLARS NEWLINE
%token BEGIN END
%token UNDERSCORE CARET
%token BACKTICK APOSTROPHE
%token EOF

%start <Ast.expr list> prog

%%

prog:
  | exprs = list(expr); EOF { exprs }

brace_arg:
  | e = delimited(LBRACE, nonempty_list(expr_no_bracket), RBRACE) { e }

arg:
  | arg = brace_arg { arg }
  | e = delimited(LBRACKET, nonempty_list(expr_no_bracket), RBRACKET) { e }

env:
  | BEGIN LBRACE name=TEXT RBRACE args=list(arg)
    el=nonempty_list(expr)
    END LBRACE TEXT RBRACE { Ast.Env (name, args, el) }

expr_no_bracket:
  | t = TEXT { Ast.Text t }
  | DOLLARS { Ast.Text "$" }
  | NEWLINE { Ast.Text "\n" }
  | UNDERSCORE arg = option(delimited(LBRACE, list(expr), RBRACE))
    { Ast.Subscript (Option.value arg ~default:[]) }
  | CARET arg = option(delimited(LBRACE, list(expr), RBRACE))
    { Ast.Superscript (Option.value arg ~default:[]) }
  | BACKTICK el=nonempty_list(expr_no_quote) APOSTROPHE { Ast.Quote el }
  | BACKTICK BACKTICK el=nonempty_list(expr_no_quote) APOSTROPHE APOSTROPHE { Ast.DoubleQuote el }
  | name = FUNC args = list(arg) { Ast.Func (name, args) }
  | env=env { env }

expr_no_quote:
  | e=expr_no_bracket { e }
  | LBRACKET { Ast.Text "[" }
  | RBRACKET { Ast.Text "]" }

expr:
  | e=expr_no_quote { e }
  | APOSTROPHE { Ast.Text "'" }
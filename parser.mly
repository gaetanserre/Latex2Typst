%{
(* AST types *)
%}

%token <string> TEXT
%token <string> FUNC
%token LBRACE RBRACE LBRACKET RBRACKET RPAR LPAR NEWLINE
%token DOLLARS BEGIN_MATH END_MATH BEGIN_MATH_BLOCK END_MATH_BLOCK
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
    el = nonempty_list(expr)
    END LBRACE TEXT RBRACE { Ast.Env (name, args, el) }

expr_base:
| t = TEXT { Ast.Text t }
| DOLLARS { Ast.Text "$" }
| BEGIN_MATH_BLOCK { Ast.Text "$\n" }
| BEGIN_MATH { Ast.Text "$" }
| END_MATH_BLOCK { Ast.Text "\n$" }
| END_MATH { Ast.Text "$" }
| NEWLINE { Ast.Text "\n" }
| UNDERSCORE arg = option(delimited(LBRACE, list(expr), RBRACE))
    { Ast.Subscript (Option.value arg ~default:[]) }
| CARET arg = option(delimited(LBRACE, list(expr), RBRACE))
    { Ast.Superscript (Option.value arg ~default:[]) }
| BACKTICK BACKTICK el = nonempty_list(expr_no_quote) APOSTROPHE APOSTROPHE
    { Ast.DoubleQuote el }
| BACKTICK el = nonempty_list(expr_no_quote) APOSTROPHE
    { Ast.Quote el }
| name = FUNC args = list(arg) { Ast.Func (name, args) }
| env = env { env }
| LPAR el=list(expr) RPAR { Ast.Par el }

expr_no_quote:
| e = expr_base { e }
| LBRACKET { Ast.Text "[" }
| RBRACKET { Ast.Text "]" }

expr_no_bracket:
| e = expr_base { e }
| APOSTROPHE { Ast.Text "'" }

expr:
| e = expr_base { e }
| APOSTROPHE { Ast.Text "'" }
| LBRACKET { Ast.Text "[" }
| RBRACKET { Ast.Text "]" }
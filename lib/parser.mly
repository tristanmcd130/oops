%{
	open Ast
%}
%token <float> NUMBER
%token <string> STRING
%token <string> ID
%token NULL
%token TRUE
%token FALSE
%token COMMA
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token EQUAL
%token DOT
%token DEF
%token END
%token CLASS
%token EXTENDS
%token USES
%token TRAIT
%token SUPER
%token IF
%token THEN
%token ELSEIF
%token ELSE
%token LET
%token IN
%token AND
%token OR
%token NOT
%token IMPORT
%token FUN
%token MATCH
%token CASE
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT
%token LT
%token LE
%token EQ
%token NE
%token GT
%token GE
%token CONS
%token COLON
%token EOF
%left OR
%left AND
%left LT LE EQ NE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc NOT
%start <t> prog
%%

prog: b = block; EOF	{b}

block: ss = stmt*	{match ss with [] -> ANull | [x] -> x | xs -> ABlock xs}

stmt:
	| a = assign											{a}
	| a = dot_assign										{a}
	| d = def												{d}
	| CLASS; n = ID; e = extends?; u = uses; ds = def*; END	{AClass (n, e, u, List.map (fun (ADef (n, ps, b)) -> (n, ps, b)) ds)}
	| TRAIT; n = ID; ams = ID*; ms = def*; END				{ATrait (n, ams, List.map (fun (ADef (n, ps, b)) -> (n, ps, b)) ms)}
	| IMPORT; f = STRING									{AImport f}
	| e = exp												{e}

exp:
	| NULL																		{ANull}
	| TRUE																		{ABool true}
	| FALSE																		{ABool false}
	| n = NUMBER																{ANumber n}
	| s = STRING																{AString s}
	| LBRACKET; es = separated_list(COMMA, exp); RBRACKET						{AList es}
	| LBRACE; es = separated_list(COMMA, dict_entry); RBRACE					{ADict es}
	| FUN; LPAREN; ps = separated_list(COMMA, ID); RPAREN; e = exp; END			{ALambda (ps, e)}
	| v = ID																	{AVar v}
	| o = unary_op; e = exp														{ACall (ADot (e, o), [])}
	| e1 = exp; o = binary_op; e2 = exp											{match o with "::" -> ACall (ADot (e2, o), [e1]) | _ -> ACall (ADot (e1, o), [e2])}
	| e = exp; LBRACKET; i = exp; RBRACKET										{ACall (ADot (e, "[]"), [i])}
	| f = exp; LPAREN; a = separated_list(COMMA, exp); RPAREN					{ACall (f, a)}
	| e = exp; DOT; f = ID														{ADot (e, f)}
	| SUPER; DOT; m = ID; LPAREN; a = separated_list(COMMA, exp); RPAREN		{ASuper (m, a)}
	| IF; c = exp; THEN; t = block; es = else_if*; e = else_; END				{AIf ((c, t) :: es @ e)}
	| LET; a = separated_nonempty_list(COMMA, any_assign); IN; b = block; END	{ALet (a, b)}
	| MATCH; e = exp; cs = case*; END											{AMatch (e, cs)}
	| LPAREN; e = exp; RPAREN													{e}

dict_entry: k = exp; COLON v = exp	{(k, v)}

%inline unary_op:
	| NOT	{"not"}
	| MINUS	{"u-"}

%inline binary_op:
	| OR		{"or"}
	| AND		{"and"}
	| LT		{"<"}
	| LE		{"<="}
	| EQ		{"=="}
	| NE		{"!="}
	| GT		{">"}
	| GE		{">="}
	| CONS		{"::"}
	| PLUS		{"+"}
	| MINUS		{"-"}
	| STAR		{"*"}
	| SLASH		{"/"}
	| PERCENT	{"%"}

assign: n = ID; EQUAL; v = exp	{AAssign (n, v)}

dot_assign: e = exp; DOT; f = ID; EQUAL v = exp	{ADotAssign (e, f, v)}

def: DEF; n = fun_name; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{ADef (n, ps, b)}

fun_name:
	| NOT					{"not"}
	| MINUS					{"u-"}
	| OR					{"or"}
	| AND					{"and"}
	| LT					{"<"}
	| LE					{"<="}
	| EQ					{"=="}
	| NE					{"!="}
	| GT					{">"}
	| GE					{">="}
	| CONS					{"::"}
	| PLUS					{"+"}
	| MINUS					{"-"}
	| STAR					{"*"}
	| SLASH					{"/"}
	| PERCENT				{"%"}
	| LBRACKET; RBRACKET	{"[]"}
	| i = ID;				{i}

extends: EXTENDS; c = ID	{c}

uses:
	|												{[]}
	| USES; ts = separated_nonempty_list(COMMA, ID)	{ts}

else_if: ELSEIF; c = exp; THEN; t = block	{(c, t)}

else_:
	|					{[]}
	| ELSE; e = block	{[(ABool true, e)]}

any_assign:
	| a = assign	{a}
	| d = def		{d}

case: CASE; e = exp; THEN; b = block	{(e, b)}
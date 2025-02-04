%token NULL
%token <bool> BOOL
%token <float> NUMBER
%token <string> STRING
%token LBRACKET
%token RBRACKET
%token COMMA
%token LBRACE
%token RBRACE
%token COLON
%token FUN
%token LPAREN
%token RPAREN
%token END
%token <string> ID
%token DOT
%token IF
%token THEN
%token ELSE
%token LET
%token IN
%token EQUAL
%token DEF
%token CLASS
%token EXTENDS
%token USES
%token TRAIT
%token EOF
%start <Exp.t> prog
%%

prog: b = block; EOF	{b}

block: ss = stmt*	{EBlock ss}

stmt:
	| a = assign												{match a with (n, v) -> EAssign (n, v)}
	| o = exp; DOT; f = ID; EQUAL; v = exp						{EDotAssign (o, f, v)}
	| d = def													{match d with (n, ps, b) -> EDef (n, ps, b)}
	| CLASS; n = ID; e = extends?; u = uses?; ds = def*; END	{EClass (n, e, u, ds)}
	| TRAIT; n = ID; ams = ID*; ms = def*; END					{Exp.ETrait (n, ams, ms)}
	| e = exp													{e}

assign: n = ID; EQUAL; v = exp	{(n, v)}

def: DEF; n = ID; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{(n, ps, b)}

extends: EXTENDS; e = exp	{e}

uses: USES; e = exp	{e}

exp:
	| NULL																	{ENull}
	| b = BOOL																{EBool b}
	| n = NUMBER															{ENumber n}
	| s = STRING															{EString s}
	| LBRACKET; es = separated_list(COMMA, exp); RBRACKET					{EList es}
	| LBRACE; es = separated_list(COMMA, dict_entry); RBRACE				{EDict es}
	| FUN; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{EFun (ps, b)}
	| v = ID																{EVar v}
	| e = exp; DOT; f = ID													{EDot (e, f)}
	| f = exp; LPAREN; a = separated_list(COMMA, exp); RPAREN				{ECall (f, a)}
	| IF; c = exp; THEN; t = block; ELSE; e = block; END					{EIf (c, t, e)}
	| LET; a = separated_list(COMMA, assign); IN; b = block; END			{ELet (a, b)}

dict_entry: k = exp; COLON; v = exp	{(k, v)}
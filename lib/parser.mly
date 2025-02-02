%token NULL
%token <bool> BOOL
%token <float> NUMBER
%token <string> STRING
%token LBRACKET
%token RBRACKET
%token COMMA
%token FUN
%token LPAREN
%token RPAREN
%token END
%token <string> ID
%token DOT
%token IF
%token THEN
%token ELSE
%token DEF
%token EQUAL
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
	| n = ID; EQUAL; e = exp									{EAssign (n, e)}
	| d = def													{match d with (n, ps, b) -> EDef (n, ps, b)}
	| CLASS; n = ID; e = extends?; u = uses?; ds = def*; END	{EClass (n, e, u, ds)}
	| TRAIT; n = ID; ams = ID*; ms = def*; END					{Exp.ETrait (n, ams, ms)}
	| e = exp													{e}

def: DEF; n = ID; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{(n, ps, b)}

extends: EXTENDS; e = exp	{e}

uses: USES; e = exp	{e}

exp:
	| NULL																	{ENull}
	| b = BOOL																{EBool b}
	| n = NUMBER															{ENumber n}
	| s = STRING															{EString s}
	| LBRACKET; es = separated_list(COMMA, exp); RBRACKET					{EList es}
	| FUN; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{EFun (ps, b)}
	| v = ID																{EVar v}
	| e = exp; DOT; f = ID													{EDot (e, f)}
	| f = exp; LPAREN; a = separated_list(COMMA, exp); RPAREN				{ECall (f, a)}
	| IF; c = exp; THEN; t = block; ELSE; e = block; END					{EIf (c, t, e)}
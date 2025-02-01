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
%token ELSEIF
%token ELSE
%token WHILE
%token DO
%token FOR
%token IN
%token EQUAL
%token CLASS
%token EXTENDS
%token STATIC
%token EOF
%start <Exp.t> prog
%%

prog: b = block; EOF	{b}

block: ss = stmt*	{EBlock ss}

stmt:
	| WHILE; c = exp; DO; b = block; END											{EWhile (c, b)}
	| FOR; n = ID; IN; l = exp; DO; b = block; END									{EFor (n, l, b)}
	| n = ID; EQUAL; e = exp														{EAssign (n, e)}
	| FUN; n = ID; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{EFun (n, ps, b)}
	| CLASS; n = ID; e = extends?; ds = class_def*; END								{Exp.EClass (n, e, ds)}
	| e = exp																		{e}

exp:
	| NULL																	{ENull}
	| b = BOOL																{EBool b}
	| n = NUMBER															{ENumber n}
	| s = STRING															{EString s}
	| LBRACKET; es = separated_list(COMMA, exp); RBRACKET					{EList es}
	| FUN; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{ELambda (ps, b)}
	| v = ID																{EVar v}
	| e = exp; DOT; f = ID													{EDot (e, f)}
	| f = exp; LPAREN; a = separated_list(COMMA, exp); RPAREN				{ECall (f, a)}
	| IF; c = exp; THEN; t = block; es = elseif*; e = else_; END			{EIf (((c, t) :: es) @ e)}

elseif: ELSEIF; c = exp; THEN; t = block	{(c, t)}

else_:
	|					{[]}
	| ELSE; b = block	{[(EBool true, b)]}

extends: EXTENDS; e = exp	{e}

class_def:
	| n = ID; EQUAL; e = exp																{CAssign (n, e)}
	| FUN; n = ID; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END			{CFun (n, ps, b)}
	| STATIC; FUN; n = ID; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{CStaticFun (n, ps, b)}
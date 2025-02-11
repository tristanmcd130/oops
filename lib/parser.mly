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
%token STRUCT
%token TRAIT
%token IMPL
%token FOR
%token AND
%token OR
%token NOT
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
%token EOF
%left OR
%left AND
%left LT LE EQ NE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc NOT
%start <Exp.t> prog
%%

prog: b = block; EOF	{b}

block: ss = stmt*	{EBlock ss}

stmt:
	| a = assign									{match a with (n, v) -> EAssign (n, v)}
	| o = exp; DOT; f = ID; EQUAL; v = exp			{EDotAssign (o, f, v)}
	| d = def										{match d with (n, ps, b) -> EDef (n, ps, b)}
	| STRUCT; n = ID; fs = ID*; END					{EStruct (n, fs)}
	| TRAIT; n = ID; ams = ID*; ms = def*; END		{ETrait (n, ams, ms)}
	| IMPL; n = exp; ms = def*; END					{EImpl (None, n, ms)}
	| IMPL; t = exp; FOR; n = exp; ms = def*; END	{Exp.EImpl (Some t, n, ms)}
	| e = exp										{e}

assign: n = ID; EQUAL; v = exp	{(n, v)}

def: DEF; n = ID; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{(n, ps, b)}

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
	| o = unary_op; e = exp													{ECall (EDot (e, o), [])}
	| e1 = exp; o = binary_op; e2 = exp										{match o with "::" -> ECall (EDot (e2, o), [e1]) | _ -> ECall (EDot (e1, o), [e2])}
	| IF; c = exp; THEN; t = block; ELSE; e = block; END					{EIf (c, t, e)}
	| LET; a = separated_list(COMMA, assign); IN; b = block; END			{ELet (a, b)}

dict_entry: k = exp; COLON; v = exp	{(k, v)}

%inline unary_op:
	| NOT	{"not"}
	| MINUS	{"-"}

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
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
%token ELSEIF
%token BAR
%token ARROW
%token MATCH
%token LET
%token IN
%token TRY
%token CATCH
%token THROW
%token EQUAL
%token DEF
%token STRUCT
%token TRAIT
%token IMPL
%token FOR
%token MODULE
%token EXPORTS
%token IMPORT
%token AND
%token OR
%token NOT
%token PLUS
%token MINUS
%token UMINUS
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

block:
	|						{Block []}
	| s = stmt				{s}
	| s = stmt; ss = stmt+	{Block (s :: ss)}

stmt:
	| a = assign																{match a with (n, v) -> Assign (n, v)}
	| o = exp; DOT; f = ID; EQUAL; v = exp										{DotAssign (o, f, v)}
	| d = def																	{match d with (n, ps, b) -> Def (n, ps, b)}
	| STRUCT; n = ID; fs = ID*; END												{Struct (n, fs)}
	| TRAIT; n = ID; ams = ID*; ms = def*; END									{Trait (n, ams, ms)}
	| IMPL; t = exp?; FOR; n = exp; ms = def*; END								{Impl (t, n, ms)}
	| MODULE; n = ID; EXPORTS; es = separated_list(COMMA, ID); b = block; END	{Module (n, es, b)}
	| IMPORT; f = STRING														{Import f}
	| THROW; e = exp															{Exp.Throw e}
	| e = exp																	{e}

assign: p = exp; EQUAL; v = exp	{(p, v)}

def: DEF; n = fun_id; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{(n, ps, b)}

fun_id:
	| NOT		{"not"}
	| UMINUS	{"u-"}
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
	| i = ID	{i}

exp:
	| NULL																	{Null}
	| b = BOOL																{Bool b}
	| n = NUMBER															{Number n}
	| s = STRING															{String s}
	| LBRACKET; es = separated_list(COMMA, exp); RBRACKET					{List es}
	| LBRACE; es = separated_list(COMMA, dict_entry); RBRACE				{Dict es}
	| FUN; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{Fun (ps, b)}
	| v = ID																{Var v}
	| e = exp; DOT; f = fun_id												{Dot (e, f)}
	| f = exp; LPAREN; a = separated_list(COMMA, exp); RPAREN				{Call (f, a)}
	| o = unary_op; e = exp													{Call (Dot (e, o), [])}
	| e1 = exp; o = binary_op; e2 = exp										{match o with "::" -> Call (Dot (e2, o), [e1]) | _ -> Call (Dot (e1, o), [e2])}
	| IF; c = exp; THEN; t = block; es = elseif*; e = else_; END			{If (((c, t) :: es) @ [e])}
	| MATCH; e = exp; cs = case*; END										{Match (e, cs)}
	| LET; a = separated_list(COMMA, assign); IN; b = block; END			{Let (a, b)}
	| TRY; b = block; CATCH; cs = case*; END								{Try (b, cs)}
	| LPAREN; e = exp; RPAREN												{e}

dict_entry: k = exp; COLON; v = exp	{(k, v)}

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

elseif: ELSEIF; c = exp; THEN; t = block	{(c, t)}

else_:
	|					{(Bool true, Null)}
	| ELSE; b = block	{(Bool true, b)}

case: BAR; t = exp; ARROW; b = block	{(t, b)}
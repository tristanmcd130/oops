%token NULL
%token <bool> BOOL
%token <float> NUMBER
%token <string> STRING
%token LBRACKET
%token COMMA
%token RBRACKET
%token LBRACE
%token COLON
%token RBRACE
%token UMINUS
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
%token AND
%token OR
%token NOT
%token CONS
%token FUN
%token LPAREN
%token RPAREN
%token END
%token EQUAL
%token DEF
%token IF
%token THEN
%token ELSEIF
%token ELSE
%token LET
%token IN
%token STRUCT
%token IMPL
%token FOR
%token DOT
%token TRAIT
%token IMPORT
%token EXPORT
%token AS
%token THROW
%token <string> ID
%token EOF
%right CONS
%left OR
%left AND
%left LT LE EQ NE GT GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc NOT
%start <Ast.t> program
%%

program: b = block; EOF	{b}

block:
	|						{Ast.Block []}
	| s = stmt				{s}
	| s = stmt; ss = stmt+	{Block (s :: ss)}

stmt:
	| a = assign									{match a with (n, v) -> Assign (n, v)}
	| d = def										{match d with (n, ps, b) -> Assign (n, Fun (n, ps, b))}
	| STRUCT; n = ID; fs = ID*; END					{Assign (n, Struct (n, fs))}
	| IMPL; t = exp?; FOR; ty = exp; ms = def*;	END	{Impl (t, ty, ms)}
	| TRAIT; n = ID; rs = ID*; ps = def*; END		{Assign (n, Trait (n, rs, ps))}
	| IMPORT; s = STRING; f = for_?					{Import (s, f)}
	| EXPORT; es = separated_list(COMMA, ID)		{Export es}
	| THROW; e = exp								{Ast.Throw e}
	| e = exp										{e}

assign: n = ID; EQUAL; v = exp	{(n, v)}

def: DEF; n = fun_id; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{(n, ps, b)}

fun_id:
	| UMINUS	{"u-"}
	| NOT		{"not"}
	| PLUS		{"+"}
	| MINUS		{"-"}
	| STAR		{"*"}
	| SLASH		{"/"}
	| PERCENT	{"%"}
	| LT		{"<"}
	| LE		{"<="}
	| EQ		{"=="}
	| NE		{"!="}
	| GT		{">"}
	| GE		{">="}
	| AND		{"and"}
	| OR		{"or"}
	| CONS		{"::"}
	| n = ID	{n}

for_: FOR; ns = separated_list(COMMA, as_)	{ns}

as_:
	| n = ID				{(n, n)}
	| n1 = ID; AS; n2 = ID	{(n1, n2)}

exp:
	| NULL																	{Ast.Null}
	| b = BOOL																{Bool b}
	| n = NUMBER															{Number n}
	| s = STRING															{String s}
	| LBRACKET; l = separated_list(COMMA, exp); RBRACKET					{List l}
	| LBRACE; m = separated_list(COMMA, map_entry); RBRACE					{Map m}
	| n = ID																{Var n}
	| o = unary_op; e = exp													{Call (Dot (e, o), [])}
	| e1 = exp; o = binary_op; e2 = exp										{match o with "::" -> Call (Dot (e2, o), [e1]) | _ -> Call (Dot (e1, o), [e2])}
	| FUN; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{Fun ("", ps, b)}
	| f = exp; LPAREN; a = separated_list(COMMA, exp); RPAREN				{Call (f, a)}
	| IF; c = exp; THEN; t = block; es = elseif*; e = else_; END			{If ((c, t) :: es @ [e])}
	| LET; a = separated_list(COMMA, assign); IN; b = block; END			{Call (Fun ("", List.map fst a, b), List.map snd a)}
	| e = exp; DOT; f = ID													{Dot (e, f)}
	| LPAREN; e = exp; RPAREN												{e}

map_entry: k = exp; COLON; v = exp	{(k, v)}

%inline unary_op:
	| MINUS	{"u-"}
	| NOT	{"not"}

%inline binary_op:
	| PLUS		{"+"}
	| MINUS		{"-"}
	| STAR		{"*"}
	| SLASH		{"/"}
	| PERCENT	{"%"}
	| LT		{"<"}
	| LE		{"<="}
	| EQ		{"=="}
	| NE		{"!="}
	| GT		{">"}
	| GE		{">="}
	| AND		{"and"}
	| OR		{"or"}
	| CONS		{"::"}

elseif: ELSEIF; t = exp; THEN; b = block	{(t, b)}

else_:
	|					{(Ast.Bool true, Ast.Null)}
	| ELSE; e = block	{(Ast.Bool true, e)}
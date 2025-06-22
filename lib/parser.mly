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
	| a = assign																	{match a with (n, v) -> Assign (n, v)}
	| DEF; n = ID; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{Ast.Assign (n, Fun (ps, b))}
	| e = exp																		{e}

assign: n = ID; EQUAL; v = exp	{(n, v)}

exp:
	| NULL																	{Null}
	| b = BOOL																{Bool b}
	| n = NUMBER															{Number n}
	| s = STRING															{String s}
	| LBRACKET; l = separated_list(COMMA, exp); RBRACKET					{List l}
	| LBRACE; m = separated_list(COMMA, map_entry); RBRACE					{Map m}
	| n = ID																{Var n}
	| o = unary_op; e = exp													{Unary (o, e)}
	| e1 = exp; o = binary_op; e2 = exp										{Binary (e1, o, e2)}
	| FUN; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{Fun (ps, b)}
	| f = exp; LPAREN; a = separated_list(COMMA, exp); RPAREN				{Call (f, a)}
	| IF; c = exp; THEN; t = block; es = elseif*; e = else_; END			{Ast.If ((c, t) :: es @ [e])}
	| LET; a = separated_list(COMMA, assign); IN; b = block; END			{Call (Fun (List.map fst a, b), List.map snd a)}
	| LPAREN; e = exp; RPAREN												{e}

map_entry: k = exp; COLON; v = exp	{(k, v)}

%inline unary_op:
	| MINUS	{Ast.Negate}
	| NOT	{Ast.Not}

%inline binary_op:
	| MINUS		{Ast.Subtract}
	| STAR		{Ast.Multiply}
	| SLASH		{Ast.Divide}
	| PERCENT	{Ast.Modulo}
	| LT		{Ast.LT}
	| LE		{Ast.LE}
	| EQ		{Ast.EQ}
	| NE		{Ast.NE}
	| GT		{Ast.GT}
	| GE		{Ast.GE}
	| AND		{Ast.And}
	| OR		{Ast.Or}
	| PLUS		{Ast.Add}
	| CONS		{Ast.Cons}

elseif: ELSEIF; t = exp; THEN; b = block	{(t, b)}

else_:
	|					{(Ast.Bool true, Ast.Null)}
	| ELSE; e = block	{(Ast.Bool true, e)}
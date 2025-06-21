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
%token FUN
%token LPAREN
%token RPAREN
%token END
%token DEF
%token <string> ID
%token EQUAL
%token EOF
%left PLUS MINUS
%left STAR SLASH PERCENT
%start <Ast.t> program
%%

program: b = block; EOF	{b}

block:
	|						{Ast.Block []}
	| s = stmt				{s}
	| s = stmt; ss = stmt+	{Block (s :: ss)}

stmt:
	| n = ID; EQUAL; v = exp														{Assign (n, v)}
	| DEF; n = ID; LPAREN; ps = separated_list(COMMA, ID); RPAREN; b = block; END	{Ast.Assign (n, Fun (ps, b))}
	| e = exp																		{e}

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
	| f = exp; LPAREN; a = separated_list(COMMA, exp); RPAREN				{Ast.Call (f, a)}
	| LPAREN; e = exp; RPAREN												{e}

map_entry: k = exp; COLON; v = exp	{(k, v)}

%inline unary_op:
	| MINUS	{Ast.Negate}

%inline binary_op:
	// | LT		{LT}
	// | LE		{LE}
	// | EQ		{EQ}
	// | NE		{NE}
	// | GT		{GT}
	// | GE		{GE}
	| PLUS		{Ast.Add}
	| MINUS		{Ast.Subtract}
	| STAR		{Ast.Multiply}
	| SLASH		{Ast.Divide}
	| PERCENT	{Ast.Modulo}
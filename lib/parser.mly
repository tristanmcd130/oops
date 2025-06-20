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
%token EOF
%start <Ast.t> program
%%

program: b = block; EOF	{b}

block:
	|						{Ast.Block []}
	| e = exp				{e}
	| e = exp; es = exp+	{Block (e :: es)}

exp:
	| NULL													{Null}
	| b = BOOL												{Bool b}
	| n = NUMBER											{Number n}
	| s = STRING											{String s}
	| LBRACKET; l = separated_list(COMMA, exp); RBRACKET	{List l}
	| LBRACE; m = separated_list(COMMA, map_entry); RBRACE	{Ast.Map m}

map_entry: k = exp; COLON; v = exp	{(k, v)}
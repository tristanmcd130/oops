{
	open Lexing
	open Parser
	exception SyntaxError of string
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let num = '-'? (digit+ frac? | digit* frac) exp?
let ws = [' ' '\t']
let id = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule read =
	parse
	| ws		{read lexbuf}
	| '\n'		{new_line lexbuf; read lexbuf}
	| "null"	{NULL}
	| "true"	{TRUE}
	| "false"	{FALSE}
	| '.'		{DOT}
	| num		{NUMBER (float_of_string (Lexing.lexeme lexbuf))}
	| '"'		{read_string (Buffer.create 16) lexbuf}
	| "def"		{DEF}
	| "end"		{END}
	| "class"	{CLASS}
	| "extends"	{EXTENDS}
	| "uses"	{USES}
	| "trait"	{TRAIT}
	| "super"	{SUPER}
	| "if"		{IF}
	| "then"	{THEN}
	| "elseif"	{ELSEIF}
	| "else"	{ELSE}
	| "and"		{AND}
	| "or"		{OR}
	| "not"		{NOT}
	| id		{ID (Lexing.lexeme lexbuf)}
	| ','		{COMMA}
	| '('		{LPAREN}
	| ')'		{RPAREN}
	| '['		{LBRACKET}
	| ']'		{RBRACKET}
	| '{'		{LBRACE}
	| '}'		{RBRACE}
	| '+'		{PLUS}
	| '-'		{MINUS}
	| '*'		{STAR}
	| '/'		{SLASH}
	| '%'		{PERCENT}
	| "<"		{LT}
	| "<="		{LE}
	| "=="		{EQ}
	| "!="		{NE}
	| ">"		{GT}
	| ">="		{GE}
	| '='		{EQUAL}
	| "::"		{CONS}
	| ':'		{COLON}
	| "->"		{ARROW}
	| '\\'		{BACKSLASH}
	| '#'		{skip_comment lexbuf}
	| eof		{EOF}
	| _			{raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf))}
and skip_comment =
	parse
	| '\n'	{new_line lexbuf; read lexbuf}
	| eof	{EOF}
	| _		{skip_comment lexbuf}
and read_string buf =
	parse
	| '"'			{STRING (Buffer.contents buf)}
	| '\\' '"'		{Buffer.add_char buf '"'; read_string buf lexbuf}
	| '\\' '\\'		{Buffer.add_char buf '\\'; read_string buf lexbuf}
	| '\\' 'n'		{Buffer.add_char buf '\n'; read_string buf lexbuf}
	| '\\' 't'		{Buffer.add_char buf '\t'; read_string buf lexbuf}
	| [^ '"' '\\']+	{Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
	| eof			{raise (SyntaxError "String is not terminated")}
	| _				{raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf))}
{
	open Lexing
	open Parser
}

let ws = [' ' '\t']+
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let number = digit+ frac? exp?
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
	parse
	| ws		{read lexbuf}
	| '\n'		{new_line lexbuf; read lexbuf}
	| "null"	{NULL}
	| "true"	{BOOL true}
	| "false"	{BOOL false}
	| number	{NUMBER (float_of_string (Lexing.lexeme lexbuf))}
	| '"'		{read_string (Buffer.create 16) lexbuf}
	| '['		{LBRACKET}
	| ']'		{RBRACKET}
	| ','		{COMMA}
	| '{'		{LBRACE}
	| '}'		{RBRACE}
	| ':'		{COLON}
	| "fun"		{FUN}
	| '('		{LPAREN}
	| ')'		{RPAREN}
	| "end"		{END}
	| '.'		{DOT}
	| "if"		{IF}
	| "then"	{THEN}
	| "else"	{ELSE}
	| "let"		{LET}
	| "in"		{IN}
	| '='		{EQUAL}
	| "def"		{DEF}
	| "class"	{CLASS}
	| "extends"	{EXTENDS}
	| "uses"	{USES}
	| "trait"	{TRAIT}
	| id		{ID (Lexing.lexeme lexbuf)}
	| '#'		{skip_comment lexbuf}
	| eof		{EOF}
	| _			{failwith ("Illegal character: " ^ Lexing.lexeme lexbuf)}
and read_string buf =
	parse
	| '"'			{STRING (Buffer.contents buf)}
	| '\\' '\\'		{Buffer.add_char buf '\\'; read_string buf lexbuf}
	| '\\' 't'		{Buffer.add_char buf '\t'; read_string buf lexbuf}
	| '\\' 'n'		{Buffer.add_char buf '\n'; read_string buf lexbuf}
	| [^ '"' '\\']+	{Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf}
	| eof			{failwith "String not terminated"}
	| _				{failwith ("Illegal character in string: " ^ Lexing.lexeme lexbuf)}
and skip_comment =
	parse
	| '\n'	{new_line lexbuf; read lexbuf}
	| eof	{EOF}
	| _		{skip_comment lexbuf}
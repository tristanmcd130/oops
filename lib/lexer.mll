{
	open Lexing
	open Parser
}

let ws = [' ' '\t']+
let digit = ['0'-'9']
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let num = ['-' '+']? digit+ frac? exp?
let id = ['a'-'z' 'A'-'Z' '_'] ['0'-'9' 'a'-'z' 'A'-'Z' '_']*

rule read = parse
| ws		{read lexbuf}
| '\n'		{new_line lexbuf; read lexbuf}
| "null"	{NULL}
| "true"	{BOOL true}
| "false"	{BOOL false}
| num		{NUMBER (lexbuf |> lexeme |> float_of_string)}
| '"'		{read_string (Buffer.create 16) lexbuf}
| '['		{LBRACKET}
| ','		{COMMA}
| ']'		{RBRACKET}
| '{'		{LBRACE}
| ':'		{COLON}
| '}'		{RBRACE}
| '+'		{PLUS}
| '-'		{MINUS}
| '*'		{STAR}
| '/'		{SLASH}
| '%'		{PERCENT}
| '<'		{LT}
| "<="		{LE}
| "=="		{EQ}
| "!="		{NE}
| '>'		{GT}
| ">="		{GE}
| "and"		{AND}
| "or"		{OR}
| "not"		{NOT}
| "::"		{CONS}
| "fun"		{FUN}
| '('		{LPAREN}
| ')'		{RPAREN}
| "end"		{END}
| '='		{EQUAL}
| "def"		{DEF}
| "if"		{IF}
| "then"	{THEN}
| "elseif"	{ELSEIF}
| "else"	{ELSE}
| "let"		{LET}
| "in"		{IN}
| "struct"	{STRUCT}
| '.'		{DOT}
| "impl"	{IMPL}
| "for"		{FOR}
| "trait"	{TRAIT}
| id		{ID (lexbuf |> lexeme)}
| _			{failwith ("Unexpected character " ^ lexeme lexbuf)}
| eof		{EOF}
and read_string buf = parse
| '"'			{STRING (Buffer.contents buf)}
| "\\\\"		{Buffer.add_char buf '\\'; read_string buf lexbuf}
| "\\\""		{Buffer.add_char buf '"'; read_string buf lexbuf}
| "\\n"			{Buffer.add_char buf '\n'; read_string buf lexbuf}
| "\\t"			{Buffer.add_char buf '\t'; read_string buf lexbuf}
| [^ '"' '\\']+	{lexeme lexbuf |> Buffer.add_string buf; read_string buf lexbuf}
| _				{failwith ("Illegal string character " ^ lexeme lexbuf)}
| eof			{failwith "Unterminated string"}
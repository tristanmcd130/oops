{
	open Lexing
	open Parser
	exception SyntaxError of string
}

let ws = [' ' '\t']+
let digit = ['0'-'9']
let frac = '.' digit+
let exp = ['e' 'E'] ['-' '+']? digit+
let num = ['-' '+']? digit* frac? exp?

rule read = parse
| ws		{read lexbuf}
| '\n'		{new_line lexbuf; read lexbuf}
| "null"	{NULL}
| "true"	{BOOL true}
| "false"	{BOOL false}
| num		{NUMBER (lexbuf |> Lexing.lexeme |> float_of_string)}
| '"'		{read_string (Buffer.create 16) lexbuf}
| '['		{LBRACKET}
| ','		{COMMA}
| ']'		{RBRACKET}
| '{'		{LBRACE}
| ':'		{COLON}
| '}'		{RBRACE}
| _			{raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf))}
| eof		{EOF}
and read_string buf = parse
| '"'			{STRING (Buffer.contents buf)}
| "\\\\"		{Buffer.add_char buf '\\'; read_string buf lexbuf}
| "\\\""		{Buffer.add_char buf '"'; read_string buf lexbuf}
| "\\n"			{Buffer.add_char buf '\n'; read_string buf lexbuf}
| "\\t"			{Buffer.add_char buf '\t'; read_string buf lexbuf}
| [^ '"' '\\']+	{Lexing.lexeme lexbuf |> Buffer.add_string buf; read_string buf lexbuf}
| _				{raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf))}
| eof			{raise (SyntaxError "Unterminated string")}
grammar Oops;

program: block EOF;
block: stmt*;
stmt
	: assign		# assign_stmt
	| op_assign		# op_assign_stmt
	| dot_assign	# dot_assign_stmt
	| index_assign	# index_assign_stmt
	| nonlocal_		# nonlocal_stmt
	| super_		# super_stmt
	| exp args		# call_stmt
	| break_		# break_stmt
	| continue_		# continue_stmt
	| do			# do_stmt
	| while_		# while_stmt
	| if_			# if_stmt
	| for_			# for_stmt
	| switch		# switch_stmt
	| fun			# fun_stmt
	| class_		# class_stmt
	| return_		# return_stmt
	| import_		# import_stmt
	| try_			# try_stmt
	| throw			# throw_stmt
	;
assign: NAME '=' exp;
op_assign: NAME assign_op '=' exp;
assign_op: factor_op | term_op;
dot_assign: exp '.' NAME assign_op? '=' exp;
index_assign: exp '[' exp ']' assign_op? '=' exp;
nonlocal_: 'nonlocal' NAME assign_op? '=' exp;
super_: 'super' '.' NAME args;
args
	: '(' ')'
	| '(' exp (',' exp)* ')'
	| '(' splat_arg ')'
	| '(' (exp ',')+ splat_arg ')'
	;
splat_arg: '...' exp;
break_: 'break';
continue_: 'continue';
do: 'do' block 'end';
while_: 'while' exp 'do' block 'end';
if_: 'if' exp 'then' block else_if* else_? 'end';
else_if: 'else' 'if' exp 'then' block;
else_: 'else' block;
for_: 'for' NAME 'in' exp 'do' block 'end';
switch: 'switch' exp case* default? 'end';
case: 'case' exp (',' exp)* 'then' block;
default: 'default' block;
fun: 'fun' fun_name params block 'end';
fun_name: NAME | '[' ']' | '[' ']' '=' | '(' ')' | 'unot' | 'u-' | '*' | '/' | '%' | '+' | '-' | '<=' | '==' | '!=' | '>' | '>=' | 'and' | 'or';
params
	: '(' ')'
	| '(' NAME (',' NAME)* ')'
	| '(' assign (',' assign)* ')'
	| '(' splat_param ')'
	| '(' (NAME ',')+ assign (',' assign)* ')'
	| '(' (NAME ',')+ splat_param ')'
	| '(' (assign ',')+ splat_param ')'
	| '(' (NAME ',')+ (assign ',')+ splat_param ')'
	;
splat_param: '...' NAME;
class_: 'class' NAME extends? (fun | static_fun | assign)* 'end';
extends: 'extends' NAME;
static_fun: 'static' fun;
return_: 'return' exp?;
import_: 'import' string;
try_: 'try' block 'catch' NAME block 'end';
throw: 'throw' exp;
exp
	: NULL					# null
	| TRUE					# true
	| FALSE					# false
	| INT					# int
	| FLOAT					# float
	| string				# string_exp
	| list_					# list_exp
	| dict_					# dict_exp
	| lambda_				# lambda_exp
	| parens				# parens_exp
	| NAME					# var
	| exp '.' NAME			# dot
	| exp '[' exp ']'		# index
	| super_				# super_exp
	| exp args				# call
	| unary					# unary_exp
	| exp factor_op exp		# factor
	| exp term_op exp		# term
	| exp compare_op exp	# compare
	| exp 'and' exp			# and
	| exp 'or' exp			# or
	| exp '?' exp ':' exp	# ternary
	;
string: STRING;
list_: '[' (exp (',' exp)*)? ']';
dict_: '{' (dict_entry (',' dict_entry)*)? '}';
dict_entry: exp ':' exp;
lambda_: 'fun' params block 'end';
parens: '(' exp ')';
unary: unary_op exp;
unary_op: 'not' | '-';
factor_op: '*' | '/' | '%';
term_op: '+' | '-';
compare_op: '<' | '<=' | '==' | '!=' | '>' | '>=';
repl_exp: exp EOF;

WS: [ \t\n]+ -> channel(HIDDEN);
COMMENT: '#' ~[\n]* -> channel(HIDDEN);
NULL: 'null';
TRUE: 'true';
FALSE: 'false';
fragment DIGIT: [0-9];
INT: '-'? DIGIT+;
fragment EXP: [eE] INT;
FLOAT: '-'? (DIGIT+ ('.' DIGIT+)? | '.' DIGIT+) EXP?;
fragment HEX_DIGIT: [0-9a-fA-F];
fragment ESC_SEQ: '\\' ('\\' | '"' | 't' | 'n' | 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT);
STRING: '"' (ESC_SEQ | ~[\\"])* '"';
NAME: [_a-zA-Z][_a-zA-Z0-9]*;

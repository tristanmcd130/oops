from lark import Lark, Transformer
from evaluate import *

def parse(string):
	parser = Lark(
	r"""
	?start: exp*
	?exp: number
		| string
		| symbol
		| s_exp
		| quote
	!number: SIGNED_NUMBER
	!string: ESCAPED_STRING
	!symbol: /[^\s()\"';]+/
	s_exp: "(" exp* ")"
	quote: "'" exp
	
	%import common.WS
	%import common.SIGNED_NUMBER
	%import common.ESCAPED_STRING
	%ignore WS
	%ignore /;.*/
	""")
	class USmalltalkTransformer(Transformer):
		def start(self, items):
			return List([Symbol("begin")] + items)
		def number(self, items):
			return Number(float(items[0].value))
		def string(self, items):
			return String(items[0].value[1 : -1].encode("utf-8").decode("unicode_escape"))
		def symbol(self, items):
			return Symbol(items[0].value)
		def s_exp(self, items):
			return List(items)
		def quote(self, items):
			return List([Symbol("quote")] + items)
	result = USmalltalkTransformer().transform(parser.parse(string))
	# print(result)
	return result
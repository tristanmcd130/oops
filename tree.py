from dataclasses import dataclass

class AST:
	pass

class Stmt(AST):
	pass

class Exp(AST):
	pass

@dataclass
class Block(Stmt):
	body: list[Stmt]

@dataclass
class Assign(Stmt):
	name: str
	value: Exp

@dataclass
class OpAssign(Stmt):
	name: str
	op: str
	value: Exp

@dataclass
class DotAssign(Stmt):
	object: Exp
	field: str
	op: str
	value: Exp

@dataclass
class IndexAssign(Stmt):
	object: Exp
	index: Exp
	op: str
	value: Exp

@dataclass
class Nonlocal(Stmt):
	name: str
	op: str
	value: Exp

@dataclass
class Super(Stmt, Exp):
	method: str
	args: list[Exp]

@dataclass
class Op(Stmt, Exp):
	op: str
	args: list[Exp]

@dataclass
class SplatArg(Exp):
	value: Exp

@dataclass
class Break(Stmt):
	pass

@dataclass
class Continue(Stmt):
	pass

@dataclass
class Do(Stmt):
	body: Stmt

@dataclass
class While(Stmt):
	cond: Exp
	body: Stmt

@dataclass
class If(Stmt):
	conds: list[tuple[Exp, Stmt]]

@dataclass
class For(Stmt):
	var: str
	list: Exp
	body: Stmt

@dataclass
class Switch(Stmt):
	exp: Exp
	cases: list[tuple[list[Exp], Stmt]]
	default: Stmt

@dataclass
class Fun(Stmt):
	name: str
	params: list[str | Assign]
	body: Stmt

@dataclass
class Class(Stmt):
	name: str
	superclass: str
	methods: list[Fun]
	static_methods: list[Fun]
	static_fields: list[Assign]

@dataclass
class Return(Stmt):
	value: Exp

@dataclass
class Try(Stmt):
	try_body: Stmt
	name: str
	catch_body: Stmt

@dataclass
class Throw(Stmt):
	error: Exp

@dataclass
class Null(Exp):
	pass

@dataclass
class Import(Stmt):
	filename: str

@dataclass
class Bool(Exp):
	value: bool

@dataclass
class Int(Exp):
	value: int

@dataclass
class Float(Exp):
	value: float

@dataclass
class String(Exp):
	value: str

@dataclass
class List(Exp):
	values: list[Exp]

@dataclass
class Dict(Exp):
	items: list[tuple[Exp, Exp]]

@dataclass
class Lambda(Exp):
	params: list[str]
	body: Stmt

@dataclass
class Var(Exp):
	name: str

@dataclass
class Dot(Exp):
	exp: Exp
	field: str
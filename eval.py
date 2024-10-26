from parse import *

class Env:
	def __init__(self, bindings, parent = None):
		self.bindings = bindings
		self.parent = parent
	def __getitem__(self, name):
		if name in self.bindings:
			return self.bindings[name]
		if self.parent:
			return self.parent[name]
		raise NameError(f"{name} not defined")
	def __setitem__(self, name, value):
		self.bindings[name] = value
	def op_assign(self, name, op, value):
		if name in self.bindings:
			self.bindings[name] = do_op(op, self.bindings[name], value)
		elif self.parent:
			self.parent.op_assign(name, op, value)
		else:
			raise NameError(f"{name} not defined")
	def nonlocal_assign(self, name, op, value):
		if self.parent:
			self.parent._nonlocal_assign(name, op, value)
		else:
			raise NameError(f"{name} not defined")
	def _nonlocal_assign(self, name, op, value):
		if name in self.bindings:
			if op:
				self.bindings[name] = do_op(op, self.bindings[name], value)
			else:
				self.bindings[name] = value
		elif self.parent:
			self.parent._nonlocal_assign(name, op, value)
		else:
			raise NameError(f"{name} not defined")

class Function:
	def __init__(self, name, params, body, env):
		self.name = name
		self.params = params
		self.body = body
		self.env = env
	def assign_params(self, *args):
		args = list(args)
		result = {}
		for param in self.params:
			if isinstance(param, str):
				if param.startswith("..."):
					result[param[3 : ]] = args
					args = []
				elif len(args) == 0:
					raise TypeError(f"Too few arguments given to {self.name}")
				else:
					result[param] = args.pop(0)
			elif isinstance(param, tuple):
				if len(args) == 0:
					result[param[0]] = param[1]
				else:
					result[param[0]] = args.pop(0)
			else:
				raise ValueError(f"Invalid type of parameter: {param}")
		if len(args) > 0:
			raise TypeError(f"Too many arguments given to {self.name}")
		return result
	def __call__(self, *args):
		try:
			eval(self.body, Env(self.assign_params(*args), self.env))
		except ReturnException as e:
			return e.value

class BreakException(Exception):
	pass

class ContinueException(Exception):
	pass

class ReturnException(Exception):
	def __init__(self, value):
		self.value = value

class CustomException(Exception):
	def __init__(self, value):
		self.value = value

class Object:
	def __init__(self, class_, fields = {}):
		self.class_ = class_
		self.fields = fields
	def __getitem__(self, name):
		# print(f"GET ITEM {name} FROM {self.class_.name} OBJECT: {self.fields}")
		if name in self.fields:
			return self.fields[name]
		return self.get_method(name)
	def get_method(self, name, start_class = None):
		# print(f"GET METHOD {name} FROM {self.class_.name} OBJECT")
		if start_class == None:
			start_class = self.class_
		method = start_class._get_method(name)
		if isinstance(method, Function):
			return Function(method.name, method.params, method.body, Env({"self": self}, method.env))
		return lambda *args: method(self, *args)

class Class(Object):
	def __init__(self, name, superclass, metaclass, methods, static_fields):
		# print(f"CREATING CLASS {name}:\nSUPERCLASS: {superclass}\nMETACLASS: {metaclass}\nMETHODS: {methods}\nSTATIC FIELDS: {static_fields}\n")
		super().__init__(metaclass, static_fields)
		self.name = name
		self.superclass = superclass
		self.methods = methods
	def _get_method(self, name):
		# print(f"GET METHOD {name} FROM {self.name} CLASS")
		if name in self.methods:
			method = self.methods[name]
			if isinstance(method, Function):
				return Function(method.name, method.params, method.body, Env({"super": self.superclass}, method.env))
			return method
		if self.superclass:
			return self.superclass._get_method(name)
		raise NameError(f"Method {name} not defined")
	def __call__(self, *args):
		object = Object(self)
		object.get_method("init")(*args)
		return object

ObjectClass = Class("Object", None, Class("ObjectMetaclass", None, None, {}, {}), {
	"init": lambda self: None,
	"to_string": lambda self: f"<{self.class_.name} object>"
}, {})
ClassClass = Class("Class", ObjectClass, Class("ClassMetaclass", ObjectClass.class_, None, {}, {}), {
	"()": lambda self, *args: self(*args),
	"to_string": lambda self: f"<class {self.name}>"
}, {})
ObjectClass.class_.superclass = ClassClass
ObjectClass.class_.class_ = ClassClass
ClassClass.class_.class_ = ClassClass
def exception_init(self, msg):
	self.fields["msg"] = msg
ExceptionClass = Class("Exception", ObjectClass, Class("ExceptionMetaclass", ObjectClass.class_, ClassClass, {}, {}), {
	"init": exception_init,
}, {})
def list_iterator_init(self, list):
	self.fields["list"] = list
	self.fields["index"] = 0
	if len(list) == 0:
		self.fields["done"] = True
		self.fields["value"] = None
	else:
		self.fields["done"] = False
		self.fields["value"] = list[0]
def list_iterator_next(self):
	self.fields["index"] += 1
	if self.fields["index"] >= len(self.fields["list"]):
		self.fields["done"] = True
		self.fields["value"] = None
	else:
		self.fields["done"] = False
		self.fields["value"] = self.fields["list"][self.fields["index"]]
ListIteratorClass = Class("ListIterator", ObjectClass, Class("ListIteratorMetaclass", ObjectClass.class_, ClassClass, {}, {}), {
	"init": list_iterator_init,
	"next": list_iterator_next,
}, {})

def do_op(op, *args):
	if isinstance(args[0], Object):
		return args[0][op](*args[1 : ])
	match op:
		case "[]":
			try:
				return args[0][args[1]]
			except IndexError:
				raise CustomException(1)
		case "[]=":
			args[0][args[1]] = args[2]
		case "()":
			return args[0](*args[1 : ])
		case "unot":
			return not args[0]
		case "u-":
			return -args[0]
		case "*":
			return args[0] * args[1]
		case "/":
			return args[0] / args[1]
		case "%":
			return args[0] % args[1]
		case "+":
			return args[0] + args[1]
		case "-":
			return args[0] - args[1]
		case "<":
			return args[0] < args[1]
		case "<=":
			return args[0] <= args[1]
		case "==":
			return args[0] == args[1]
		case "!=":
			return args[0] != args[1]
		case ">":
			return args[0] > args[1]
		case ">=":
			return args[0] >= args[1]
		case "and":
			return bool(args[0] and args[1])
		case "or":
			return bool(args[0] or args[1])
		case "?:":
			if do_op("to_bool", args[0]):
				return args[1]
			return args[2]
		case "to_bool":
			return bool(args[0])
		case "to_string":
			if args[0] == None:
				return "null"
			if isinstance(args[0], bool):
				return str(args[0]).lower()
			if isinstance(args[0], list):
				return f"[{', '.join([do_op("to_string", value) for value in args[0]])}]"
			if isinstance(args[0], dict):
				return "{" + ", ".join([f"{do_op("to_string", key)}: {do_op("to_string", value)}" for (key, value) in args[0].items()]) + "}"
			if isinstance(args[0], Class):
				return f"<class {args[0].name}>"
			if isinstance(args[0], Function):
				if args[0].name == "<lambda>":
					return args[0].name
				return f"<function {args[0].name}>"
			if callable(args[0]):
				return "<primitive function>"
			return str(args[0])
		case "iterator":
			if isinstance(args[0], list | str):
				return ListIteratorClass(args[0])
			if isinstance(args[0], dict):
				return ListIteratorClass([list(item) for item in args[0].items()])
			raise ValueError(f"The only primitive types that have iterators are lists, strings, and dictionaries")
		case _:
			raise ValueError(f"Invalid operation {op}")

def eval(tree, env):
	match tree:
		case ast.Block(body):
			for stmt in body:
				eval(stmt, env)
		case ast.Assign(name, value):
			env[name] = eval(value, env)
		case ast.OpAssign(name, op, value):
			env.op_assign(name, op, eval(value, env))
		case ast.DotAssign(object, field, op, value):
			object = eval(object, env)
			if op:
				object.fields[field] = do_op(op, object.fields[field], eval(value, env))
			else:
				object.fields[field] = eval(value, env)
		case ast.IndexAssign(object, index, op, value):
			object = eval(object, env)
			if op:
				index = eval(index, env)
				object[index] = do_op(op, object[index], eval(value, env))
			else:
				do_op("[]=", object, eval(index, env), eval(value, env))
		case ast.Nonlocal(name, op, value):
			env.nonlocal_assign(name, op, eval(value, env))
		case ast.Super(method, args):
			return env["self"].get_method(method, env["super"])(*args)
		case ast.Op(op, args):
			if isinstance(args[-1], ast.SplatArg):
				iterator = do_op("iterator", eval(args.pop(-1).value, env))
				args = [eval(arg, env) for arg in args]
				while not do_op("to_bool", iterator["done"]):
					args.append(iterator["value"])
					iterator["next"]()
			else:
				args = [eval(arg, env) for arg in args]
			return do_op(op, *args)
		case ast.Break():
			raise BreakException()
		case ast.Continue():
			raise ContinueException()
		case ast.Do(body):
			eval(body, Env({}, env))
		case ast.While(cond, body):
			while do_op("to_bool",eval(cond, env)):
				try:
					eval(body, env)
				except BreakException:
					break
				except ContinueException:
					pass
		case ast.If(conds):
			for (cond, body) in conds:
				if do_op("to_bool", eval(cond, env)):
					eval(body, env)
					break
		case ast.For(var, list, body):
			iterator = do_op("iterator", eval(list, env))
			while not do_op("to_bool", iterator["done"]):
				try:
					env.bindings |= {var: iterator["value"]}
					eval(body, env)
				except BreakException:
					break
				except ContinueException:
					pass
				iterator["next"]()
		case ast.Fun(name, params, body):
			env[name] = Function(name, [(param.name, eval(param.value, env)) if isinstance(param, ast.Assign) else param for param in params], body, env)
		case ast.Class(name, superclass, methods, static_methods, static_fields):
			if superclass:
				superclass = env[superclass]
			else:
				superclass = ObjectClass
			metaclass = Class(name + "Metaclass", superclass.class_, ClassClass, {method.name: Function(f"{name}Metaclass.{method.name}", method.params, method.body, env) for method in static_methods}, {})
			env[name] = Class(name, superclass, metaclass, {method.name: Function(f"{name}.{method}", method.params, method.body, env) for method in methods}, {field.name: eval(field.value, env) for field in static_fields})
		case ast.Return(value):
			raise ReturnException(eval(value, env))
		case ast.Import(filename):
			with open(filename) as file:
				eval(parse(file.read()), env)
		case ast.Try(try_body, name, catch_body):
			try:
				eval(try_body, env)
			except CustomException as error:
				env |= {name: error.value}
				eval(catch_body, env)
		case ast.Throw(error):
			raise CustomException(eval(error, env))
		case ast.Null():
			return None
		case ast.Bool(value) | ast.Int(value) | ast.Float(value) | ast.String(value):
			return value
		case ast.List(values):
			return [eval(value, env) for value in values]
		case ast.Dict(items):
			return {eval(key, env): eval(value, env) for (key, value) in items}
		case ast.Lambda(params, body):
			return Function("<lambda>", params, body, env)
		case ast.Var(name):
			return env[name]
		case ast.Dot(exp, field):
			object = eval(exp, env)
			assert isinstance(object, Object), "Only objects support dot notation"
			return object[field]
		case exp:
			raise TypeError(f"What is {exp}?")
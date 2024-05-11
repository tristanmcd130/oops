class Env:
	def __init__(self, env, parent = None):
		self.env = env
		self.parent = parent
	def __getitem__(self, name):
		if name in self.env:
			return self.env[name]
		if self.parent:
			return self.parent[name]
		raise NameError(f"{name} not defined")
	def define(self, name, value):
		if name in self:
			raise NameError(f"{name} already defined, cannot redefine")
		self.env[name] = value
	def __setitem__(self, name, value):
		if name not in self:
			raise NameError(f"{name} not defined, cannot set")
		if name in self.env:
			self.env[name] = value
		else:
			self.parent[name] = value
	def __contains__(self, name):
		try:
			self[name]
			return True
		except NameError:
			return False
	def __repr__(self):
		return f"Env(env={self.env}, parent={self.parent})"

class Method: # these are NOT objects, there should be no way to create them or manipulate them outside of class definitions
	def __init__(self, params, body, env):
		self.params = params
		self.body = body
		self.env = env
	def __call__(self, receiver, *args):
		def static_env(receiver_class):
			return Env(receiver_class.fields, static_env(receiver_class.superclass) if receiver_class.superclass else self.env)
		return evaluate(self.body, Env(dict(zip(self.params, args)) | {"self": receiver}, Env(receiver.fields, static_env(receiver.class_))))

class Object:
	def __init__(self, class_, fields):
		self.class_ = class_
		self.fields = fields
	def __getitem__(self, name):
		return self.fields[name]
	def __setitem__(self, name, value):
		assert name in self.fields, f"Object has no field {name}"
		self.fields[name] = value
	def send(self, message, *args, start_class = None):
		if not start_class:
			start_class = self.class_
		return start_class[message](self, *args)

class Class(Object):
	def __init__(self, superclass, metaclass, fields, static_fields, methods):
		super().__init__(metaclass, static_fields)
		self.superclass = superclass
		self.instance_fields = fields
		self.methods = methods
	def __getitem__(self, name):
		if name in self.methods:
			method = self.methods[name]
			if type(method) == Method:
				return Method(method.params, method.body, Env({"super": self.superclass}, method.env))
			return method
		if self.superclass:
			return self.superclass[name]
		raise NameError(f"Class {self} does not respond to message {name}")

ClassClass = Class(None, Class(None, None, [], {}, {}), [], {}, {
	"superclass": lambda receiver: receiver.superclass,
	"to-string": lambda receiver: String("<class>")
})
ClassClass.class_.class_ = ClassClass

def new(receiver):
	new_object = Object(receiver, {})
	class_ = receiver
	while class_:
		new_object.fields |= {field: Nil() for field in class_.instance_fields}
		class_ = class_.superclass
	return new_object
ObjectClass = Class(None, Class(ClassClass, ClassClass, [], {}, {"new": new}), [], {}, {
	"print": lambda receiver: print(receiver.send("to-string").value),
	"to-string": lambda receiver: String("<object>"),
	"yourself": lambda receiver: receiver,
	"value:": lambda receiver, *args: receiver,
	"class": lambda receiver: receiver.class_,
})
ClassClass.superclass = ObjectClass
ClassClass.class_.superclass = ObjectClass.class_

NumberClass = Class(ObjectClass, Class(ObjectClass.class_, ClassClass, [], {}, {}), [], {}, {
	"+": lambda receiver, other: Number(receiver.value + other.value),
	"-": lambda receiver, other: Number(receiver.value - other.value),
	"*": lambda receiver, other: Number(receiver.value * other.value),
	"^": lambda receiver, other: Number(receiver.value ** other.value),
	"sqrt": lambda receiver: Number(receiver.value ** 0.5),
	"to-string": lambda receiver: String("%g" % receiver.value),
	"<=": lambda receiver, other: Boolean(receiver.value <= other.value),
})
class Number(Object):
	def __init__(self, value):
		super().__init__(NumberClass, {})
		self.value = value

StringClass = Class(ObjectClass, Class(ObjectClass.class_, ClassClass, [], {}, {}), [], {}, {
	"+": lambda receiver, other: String(receiver.value + other.value),
	"to-string": lambda receiver: receiver,
	"format:": lambda receiver, *args: String(receiver.value.format(*[arg.send("to-string").value for arg in args])),
})
class String(Object):
	def __init__(self, value):
		super().__init__(StringClass, {})
		self.value = value

LambdaClass = Class(ObjectClass, Class(ObjectClass.class_, ClassClass, [], {}, {}), [], {}, {
	"value:": lambda receiver, *args: receiver(*args),
})
class Lambda(Object):
	def __init__(self, params, body, env):
		super().__init__(LambdaClass, {})
		self.params = params
		self.body = body
		self.env = env
	def __call__(self, *args):
		return evaluate(self.body, Env(dict(zip(self.params, args)), self.env))

BooleanClass = Class(ObjectClass, Class(ObjectClass.class_, ClassClass, [], {}, {}), [], {}, {
	"if:else:": lambda receiver, if_body, else_body: if_body.send("value:") if receiver.value else else_body.send("value:"),
	"and:": lambda receiver, other: Boolean(receiver.value and other.value),
	"or:": lambda receiver, other: Boolean(receiver.value or other.value),
	"xor:": lambda receiver, other: Boolean(bool(receiver.value ^ other.value)),
	"not": lambda receiver: Boolean(not receiver.value),
	"to-string": lambda receiver: String(str(receiver.value).lower()),
})
class Boolean(Object):
	def __init__(self, value):
		super().__init__(BooleanClass, {})
		self.value = value

def cons(receiver, car):
	result = receiver.send("new")
	result["car"] = car
	result["cdr"] = receiver
	return result
def _to_string(receiver):
	car = receiver.send("car")
	car_string = car.send("to-string").value
	cdr = receiver.send("cdr")
	if cdr.class_ == NilClass:
		cdr_string = ""
	else:
		cdr_string = " " + cdr.send("_to-string").value
	return String(car_string + cdr_string)
ListClass = Class(ObjectClass, Class(ObjectClass.class_, ClassClass, [], {}, {
	"new:": lambda receiver, *args: Nil() if len(args) == 0 else receiver.send("cons:", args[0], receiver.send("new:", *args[1 : ])),
}), ["car", "cdr"], {}, {
	"car": lambda receiver: receiver["car"],
	"cdr": lambda receiver: receiver["cdr"],
	"cons:": cons,
	"to-string": lambda receiver: String(f"({receiver.send('_to-string').value})"),
	"_to-string": _to_string,
	"nth:": lambda receiver, n: receiver["car"] if n.value == 0 else receiver["cdr"].send("nth:", Number(n.value - 1)),
})
class List(Object):
	def __init__(self, value):
		if len(value) == 0:
			super().__init__(NilClass, {})
		else:
			super().__init__(ListClass, {"car": value[0], "cdr": List(value[1 : ])})
	def __len__(self):
		if self.class_ == NilClass:
			return 0
		return len(self.fields["cdr"]) + 1
	def __getitem__(self, key):
		if self.class_ == NilClass:
			raise IndexError(f"Cannot index nil")
		if type(key) == slice:
			if key.step != None:
				raise IndexError(f"List doesn't support step")
			start = 0 if key.start == None else key.start
			stop = len(self) if key.stop == None else key.stop
			result = []
			for i in range(start, stop):
				result.append(self[i])
			return List(result)
		if key == 0:
			return self.fields["car"]
		return self.fields["cdr"][key - 1]
	def __setitem__(self, key, value):
		if self.class_ == NilClass:
			raise IndexError(f"Cannot index nil")
		if key == 0:
			self.fields["car"] = value
		self.fields["cdr"][key - 1] = value
	def __iter__(self):
		class ListIterator:
			def __init__(self, list):
				self.list = list
				self.index = 0
			def __iter__(self):
				return self
			def __next__(self):
				if self.index >= len(self.list):
					raise StopIteration()
				item = self.list[self.index]
				self.index += 1
				return item
		return ListIterator(self)
	def __add__(self, other):
		if self.class_ == NilClass:
			return other
		if self.fields["cdr"].class_ == NilClass:
			result = self
			result.fields["cdr"] = other
			return result
		return self.fields["cdr"] + other

NilClass = Class(ObjectClass, Class(ObjectClass.class_, ClassClass, [], {}, {}), [], {}, {
	"to-string": lambda receiver: String("()"),
	"car": lambda receiver: None,
	"cdr": lambda receiver: None, # these should raise errors
})
def Nil():
	return NilClass.send("new")

SymbolClass = Class(ObjectClass, Class(ObjectClass.class_, ClassClass, [], {}, {}), [], {}, {
	"to-string": lambda receiver: String(receiver.value)
})
class Symbol(Object):
	def __init__(self, value):
		super().__init__(SymbolClass, {})
		self.value = value
	def __repr__(self):
		return self.value

tabs = 0
def evaluate(exp, env):
	global tabs
	# print(f"{chr(9) * tabs}EXP: {exp}\n{chr(9) * tabs}ENV: {env}")
	tabs += 1
	def _evaluate(exp, env):
		if exp.class_ == ListClass:
			if exp[0].class_ == SymbolClass:
				match exp[0].value:
					case "quote":
						return exp[1]
					case "begin":
						if len(exp) == 0:
							return Nil()
						return [evaluate(e, env) for e in exp[1 : ]][-1]
					case "define":
						name = exp[1].value
						if name in env:
							raise NameError(f"{name} already defined, cannot redefine")
						env.define(name, evaluate(exp[2], env))
						return env[name]
					case "set":
						name = exp[1].value
						if name not in env:
							raise NameError(f"{name} not defined, cannot set")
						env[name] = evaluate(exp[2], env)
						return env[name]
					case "let":
						let_env = env
						for binding in exp[1]:
							let_env = Env({binding[0].value: evaluate(binding[1], let_env)}, let_env)
						return evaluate(List([Symbol("begin")]) + exp[2 : ], let_env)
					case "lambda":
						return Lambda([param.value for param in exp[1]], exp[2 : ], env)
					case "class":
						superclass = evaluate(exp[1], env)
						fields = []
						static_fields = {}
						methods = {}
						static_methods = {}
						for slot in exp[2 : ]:
							match slot[0].value:
								case "fields":
									fields.extend([field.value for field in slot[1 : ]])
								case "static-fields":
									static_fields |= {field[0].value: evaluate(field[1], env) for field in slot[1 : ]}
								case "method":
									methods |= {slot[1].value: Method([param.value for param in slot[2]], List([Symbol("begin")]) + slot[3 : ], env)}
								case "static-method":
									static_methods |= {slot[1].value: Method([param.value for param in slot[2]], List([Symbol("begin")]) + slot[3 : ], env)}
								case _:
									raise SyntaxError(f"Unknown section {slot[0].value}")
						# print(f"CLASS DECLARATION:\n{superclass=}\n{fields=}\n{static_fields=}\n{methods=}\n{static_methods=}")
						return Class(superclass, Class(superclass.class_, ClassClass, [], {}, static_methods), fields, static_fields, methods)
					case "defun":
						name = exp[1].value
						if name in env:
							raise NameError(f"{name} already defined, cannot redefine")
						env.define(name, evaluate(Lambda([param.value for param in exp[2]], exp[3 : ], env), env))
						return env[name]
					case "defclass":
						name = exp[1].value
						if name in env:
							raise NameError(f"{name} already defined, cannot redefine")
						superclass = evaluate(exp[2], env)
						fields = []
						static_fields = {}
						methods = {}
						static_methods = {}
						for slot in exp[3 : ]:
							match slot[0].value:
								case "fields":
									fields.extend([field.value for field in slot[1 : ]])
								case "static-fields":
									static_fields |= {field[0].value: evaluate(field[1], env) for field in slot[1 : ]}
								case "method":
									methods |= {slot[1].value: Method([param.value for param in slot[2]], List([Symbol("begin")]) + slot[3 : ], env)}
								case "static-method":
									static_methods |= {slot[1].value: Method([param.value for param in slot[2]], List([Symbol("begin")]) + slot[3 : ], env)}
								case _:
									raise SyntaxError(f"Unknown section {slot[0].value}")
						# print(f"CLASS DECLARATION:\n{superclass=}\n{fields=}\n{static_fields=}\n{methods=}\n{static_methods=}")
						env.define(name, Class(superclass, Class(superclass.class_, ClassClass, [], {}, static_methods), fields, static_fields, methods))
						return env[name]
					case "super":
						return env["self"].send(exp[1].value, *[evaluate(arg, env) for arg in exp[2 : ]], start_class = env["super"])
					case _:
						pass
			# message send
			receiver = evaluate(exp[0], env)
			return receiver.send(exp[1].value, *[evaluate(arg, env) for arg in exp[2 : ]])
		if type(exp) == Symbol:
			return env[exp.value]
		return exp
	result = _evaluate(exp, env)
	tabs -= 1
	# print(f"{chr(9) * tabs}=> {result}")
	return result
import antlr4
from OopsLexer import OopsLexer
from OopsParser import OopsParser
from OopsVisitor import OopsVisitor
import tree as ast

class OopsTransformer(OopsVisitor):
	def visitProgram(self, ctx: OopsParser.ProgramContext):
		return self.visit(ctx.block())
	def visitBlock(self, ctx: OopsParser.BlockContext):
		result = [self.visit(stmt) for stmt in ctx.stmt()]
		if len(result) == 1:
			return result[0]
		return ast.Block(result)
	def visitAssign(self, ctx: OopsParser.AssignContext):
		return ast.Assign(ctx.NAME().getText(), self.visit(ctx.exp()))
	def visitOp_assign(self, ctx: OopsParser.Op_assignContext):
		return ast.OpAssign(ctx.NAME().getText(), ctx.assign_op().getText(), self.visit(ctx.exp()))
	def visitDot_assign(self, ctx: OopsParser.Dot_assignContext):
		return ast.DotAssign(self.visit(ctx.exp(0)), ctx.NAME().getText(), ctx.assign_op().getText() if ctx.assign_op() else "", self.visit(ctx.exp(1)))
	def visitIndex_assign(self, ctx: OopsParser.Index_assignContext):
		return ast.IndexAssign(self.visit(ctx.exp(0)), self.visit(ctx.exp(1)), ctx.assign_op().getText() if ctx.assign_op() else "", self.visit(ctx.exp(2)))
	def visitNonlocal_(self, ctx: OopsParser.Nonlocal_Context):
		return ast.Nonlocal(ctx.NAME().getText(), ctx.assign_op().getText() if ctx.assign_op() else "", self.visit(ctx.exp()))
	def visitSuper_(self, ctx: OopsParser.Super_Context):
		return ast.Super(ctx.NAME().getText(), self.visit(ctx.args()))
	def visitCall_stmt(self, ctx: OopsParser.Call_stmtContext):
		return ast.Op("()", [self.visit(ctx.exp())] + self.visit(ctx.args()))
	def visitArgs(self, ctx: OopsParser.ArgsContext):
		return [self.visit(arg) for arg in ctx.exp()] + ([self.visit(ctx.splat_arg())] if ctx.splat_arg() else [])
	def visitSplat_arg(self, ctx: OopsParser.Splat_argContext):
		return ast.SplatArg(self.visit(ctx.exp()))
	def visitBreak_(self, ctx: OopsParser.Break_Context):
		return ast.Break()
	def visitContinue_(self, ctx: OopsParser.Continue_Context):
		return ast.Continue()
	def visitDo(self, ctx: OopsParser.DoContext):
		return ast.Do(self.visit(ctx.block()))
	def visitWhile_(self, ctx: OopsParser.While_Context):
		return ast.While(self.visit(ctx.exp()), self.visit(ctx.block()))
	def visitIf_(self, ctx: OopsParser.If_Context):
		return ast.If([(self.visit(ctx.exp()), self.visit(ctx.block()))] + [self.visit(branch) for branch in ctx.else_if()] + [(ast.Bool(True), self.visit(ctx.else_()) if ctx.else_() else ast.Block([]))])
	def visitElse_if(self, ctx: OopsParser.Else_ifContext):
		return (self.visit(ctx.exp()), self.visit(ctx.block()))
	def visitElse_(self, ctx: OopsParser.Else_Context):
		return self.visit(ctx.block())
	def visitFor_(self, ctx: OopsParser.For_Context):
		return ast.For(ctx.NAME().getText(), self.visit(ctx.exp()), self.visit(ctx.block()))
	def visitSwitch(self, ctx: OopsParser.SwitchContext):
		return ast.Switch(self.visit(ctx.exp()), [self.visit(case) for case in ctx.case()], self.visit(ctx.default()) if ctx.default() else ast.Block([]))
	def visitCase(self, ctx: OopsParser.CaseContext):
		return ([self.visit(exp) for exp in ctx.exp()], self.visit(ctx.block()))
	def visitDefault(self, ctx: OopsParser.DefaultContext):
		return self.visit(ctx.block())
	def visitFun(self, ctx: OopsParser.FunContext):
		return ast.Fun(ctx.fun_name().getText(), self.visit(ctx.params()), self.visit(ctx.block()))
	def visitParams(self, ctx: OopsParser.ParamsContext):
		return [param.getText() for param in ctx.NAME()] + [self.visit(param) for param in ctx.assign()] + ([ctx.splat_param().getText()] if ctx.splat_param() else [])
	def visitClass_(self, ctx: OopsParser.Class_Context):
		return ast.Class(ctx.NAME().getText(), self.visit(ctx.extends()) if ctx.extends() else "", [self.visit(method) for method in ctx.fun()], [self.visit(method) for method in ctx.static_fun()], [self.visit(field) for field in ctx.assign()])
	def visitExtends(self, ctx: OopsParser.ExtendsContext):
		return ctx.NAME().getText()
	def visitStatic_fun(self, ctx: OopsParser.Static_funContext):
		return self.visit(ctx.fun())
	def visitReturn_(self, ctx: OopsParser.Return_Context):
		return ast.Return(self.visit(ctx.exp()) if ctx.exp() else ast.Null())
	def visitImport_(self, ctx: OopsParser.Import_Context):
		return ast.Import(self.visit(ctx.string()).value)
	def visitTry_(self, ctx: OopsParser.Try_Context):
		return ast.Try(self.visit(ctx.block(0)), ctx.NAME().getText(), self.visit(ctx.block(1)))
	def visitThrow(self, ctx: OopsParser.ThrowContext):
		return ast.Throw(self.visit(ctx.exp()))
	def visitNull(self, ctx: OopsParser.NullContext):
		return ast.Null()
	def visitTrue(self, ctx: OopsParser.TrueContext):
		return ast.Bool(True)
	def visitFalse(self, ctx: OopsParser.FalseContext):
		return ast.Bool(False)
	def visitInt(self, ctx: OopsParser.IntContext):
		return ast.Int(int(ctx.INT().getText()))
	def visitFloat(self, ctx: OopsParser.FloatContext):
		return ast.Float(float(ctx.FLOAT().getText()))
	def visitString(self, ctx: OopsParser.StringContext):
		return ast.String(ctx.STRING().getText()[1 : -1].encode("utf-8").decode("unicode_escape"))
	def visitList_(self, ctx: OopsParser.List_Context):
		return ast.List([self.visit(exp) for exp in ctx.exp()])
	def visitDict_(self, ctx: OopsParser.Dict_Context):
		return ast.Dict([self.visit(entry) for entry in ctx.dict_entry()])
	def visitDict_entry(self, ctx: OopsParser.Dict_entryContext):
		return (self.visit(ctx.exp(0)), self.visit(ctx.exp(1)))
	def visitLambda_(self, ctx: OopsParser.Lambda_Context):
		return ast.Lambda(self.visit(ctx.params()), self.visit(ctx.block()))
	def visitParens(self, ctx: OopsParser.ParensContext):
		return self.visit(ctx.exp())
	def visitVar(self, ctx: OopsParser.VarContext):
		return ast.Var(ctx.NAME().getText())
	def visitDot(self, ctx: OopsParser.DotContext):
		return ast.Dot(self.visit(ctx.exp()), ctx.NAME().getText())
	def visitIndex(self, ctx: OopsParser.IndexContext):
		return ast.Op("[]", [self.visit(ctx.exp(0)), self.visit(ctx.exp(1))])
	def visitCall(self, ctx: OopsParser.CallContext):
		return ast.Op("()", [self.visit(ctx.exp())] + self.visit(ctx.args()))
	def visitUnary(self, ctx: OopsParser.UnaryContext):
		return ast.Op("u" + ctx.unary_op().getText(), [self.visit(ctx.exp())])
	def visitFactor(self, ctx: OopsParser.FactorContext):
		return ast.Op(ctx.factor_op().getText(), [self.visit(ctx.exp(0)), self.visit(ctx.exp(1))])
	def visitTerm(self, ctx: OopsParser.TermContext):
		return ast.Op(ctx.term_op().getText(), [self.visit(ctx.exp(0)), self.visit(ctx.exp(1))])
	def visitCompare(self, ctx: OopsParser.CompareContext):
		return ast.Op(ctx.compare_op().getText(), [self.visit(ctx.exp(0)), self.visit(ctx.exp(1))])
	def visitAnd(self, ctx: OopsParser.AndContext):
		return ast.Op("and", [self.visit(ctx.exp(0)), self.visit(ctx.exp(1))])
	def visitOr(self, ctx: OopsParser.OrContext):
		return ast.Op("or", [self.visit(ctx.exp(0)), self.visit(ctx.exp(1))])
	def visitRange(self, ctx: OopsParser.RangeContext):
		return ast.Op("..", [self.visit(ctx.exp(0)), self.visit(ctx.exp(1))])
	def visitTernary(self, ctx: OopsParser.TernaryContext):
		return ast.Op("?:", [self.visit(ctx.exp(0)), self.visit(ctx.exp(1)), self.visit(ctx.exp(2))])
	def visitRepl_exp(self, ctx: OopsParser.Repl_expContext):
		return self.visit(ctx.exp())

class FunkErrorListener(antlr4.error.ErrorListener.ErrorListener):
	def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
		raise SyntaxError(f"Line {line}, column {column}: {msg}")

def parse(string, start_rule = "program"):
	input_stream = antlr4.InputStream(string)
	lexer = OopsLexer(input_stream)
	token_stream = antlr4.CommonTokenStream(lexer)
	token_stream.fill()
	parser = OopsParser(token_stream)
	parser.removeErrorListeners()
	parser.addErrorListener(FunkErrorListener())
	tree = getattr(parser, start_rule)()
	return OopsTransformer().visit(tree)
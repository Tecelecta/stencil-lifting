from fparser.two.Fortran2003 import *
from fparser.two.utils import *
import sdsl
import functools


class TranslateContext:
    def __init__(self):
        self.decl_dict = {}
        self.global_decl_dict = {}
        self.current_function = None
        self.default_intent_out = True
        self.is_local = False

    # ------------------------------------------------------------
    # basic
    # ------------------------------------------------------------
    def get_function_by_name(self, name):
        return eval("self.translate_%s" % name)

    def translate_AST_Class(self, ast, *args):
        name = type(ast).__name__
        if name[-5:] == "_List":
            return [self.get_function_by_name(type(child).__name__)(child, *args) for child in ast.children]
        else:
            return self.get_function_by_name(name)(ast, *args)

    def translate_Each_Children(self, ast, *args):
        return [self.translate_AST_Class(child, *args) for child in ast.children]

    def translate_Name(self, ast, *args):
        assert isinstance(ast, Name)
        return str(ast)

    # ------------------------------------------------------------
    # module
    # ------------------------------------------------------------
    def translate_Module(self, ast: Module) -> list[str]:
        assert isinstance(ast, Module)
        content = [self.translate_AST_Class(child) for child in ast.children[1:-1]]
        return functools.reduce(list.__add__, content, [])

    def translate_Module_Subprogram_Part(self, ast: Module_Subprogram_Part) -> list[str]:
        assert isinstance(ast, Module_Subprogram_Part)
        content = [self.translate_AST_Class(child) for child in ast.children[1:]]
        return functools.reduce(list.__add__, content, [])

    # ------------------------------------------------------------
    # routine
    # ------------------------------------------------------------
    def translate_Program(self, ast: Program) -> list[str]:
        assert isinstance(ast, Program)
        return sdsl.flatten_lines(self.translate_Each_Children(ast), -1)

    def translate_Subroutine_Subprogram(self, ast):
        assert isinstance(ast, Subroutine_Subprogram)
        self.is_local = True
        func_name, param_list = self.translate_Subroutine_Stmt(ast.children[0])
        func_decl = sdsl.FuncDecl(func_name, True, param_list, "subroutine")
        self.decl_dict = self.global_decl_dict.copy()
        self.default_intent_out = True
        local_decl = []
        body = []
        for child in ast.children[1:-1]:
            if isinstance(child, Specification_Part):
                local_decl = self.translate_Specification_Part_local(child, param_list)
            else:
                body += self.translate_AST_Class(child, param_list)
        body = local_decl + body
        s = func_decl.function_format(self.decl_dict, body)
        self.decl_dict = {}
        self.is_local = False
        return s

    def translate_Subroutine_Stmt(self, ast):
        assert isinstance(ast, Subroutine_Stmt)
        return str(ast.get_name()), [str(child) for child in ast.children[2].children]

    def translate_Function_Subprogram(self, ast):
        assert isinstance(ast, Function_Subprogram)
        self.is_local = True
        func_name, param_list = self.translate_Function_Stmt(ast.children[0])
        func_decl = sdsl.FuncDecl(func_name, True, param_list, "function")
        self.current_function = func_name
        self.global_decl_dict[func_name] = func_decl
        self.decl_dict = self.global_decl_dict.copy()
        self.default_intent_out = False
        local_decl = []
        body = []
        for child in ast.children[1:-1]:
            if isinstance(child, Specification_Part):
                local_decl = self.translate_Specification_Part_local(child, param_list)
            else:
                body += self.translate_AST_Class(child, param_list)
        body = local_decl + body
        s = func_decl.function_format(self.decl_dict, body)
        self.decl_dict = {}
        self.current_function = None
        self.is_local = False
        return s

    def translate_Function_Stmt(self, ast):
        assert isinstance(ast, Function_Stmt)
        return str(ast.get_name()), [str(child) for child in ast.children[2].children]

    def translate_Specification_Part(self, ast, *args):
        assert isinstance(ast, Specification_Part)
        if len(args) == 0:
            return self.translate_Specification_Part_global(ast)
        else:
            return self.translate_Specification_Part_local(ast, *args)

    def translate_Specification_Part_global(self, ast):
        for child in ast.children:
            if isinstance(child, Type_Declaration_Stmt):
                self.global_decl_dict.update(self.translate_Type_Declaration_Stmt(child))
            elif isinstance(child, Derived_Type_Def):
                type_decl = self.translate_Derived_Type_Def(child)
                self.global_decl_dict[type_decl.type_name] = type_decl
        global_decl = []
        for var_name, decl in self.global_decl_dict.items():
            if isinstance(decl, sdsl.VarDecl):
                global_decl.append(decl.variable_format())
            elif isinstance(decl, sdsl.TypeDecl):
                global_decl += decl.struct_format()
        return global_decl

    def translate_Specification_Part_local(self, ast, param_list):
        for child in ast.children:
            if isinstance(child, Type_Declaration_Stmt):
                self.decl_dict.update(self.translate_Type_Declaration_Stmt(child))
            elif isinstance(child, Derived_Type_Def):
                type_decl = self.translate_Derived_Type_Def(child)
                self.decl_dict[type_decl.type_name] = type_decl
        local_decl = []
        for var_name, decl in self.decl_dict.items():
            if not decl.is_local or var_name in param_list:
                continue
            decl = self.decl_dict[var_name]
            if isinstance(decl, sdsl.VarDecl):
                local_decl.append(decl.variable_format())
            elif isinstance(decl, sdsl.TypeDecl):
                local_decl += decl.struct_format()
        return local_decl

    # ------------------------------------------------------------
    # type
    # ------------------------------------------------------------
    def translate_Type_Declaration_Stmt(self, ast):
        assert isinstance(ast, Type_Declaration_Stmt)
        type_name = self.translate_AST_Class(ast.children[0])  # Declaration_Type_Spec
        attr_map = {}
        if ast.children[1]:
            self.translate_AST_Class(ast.children[1], attr_map)  # Attr_Spec_List
        array_dims = attr_map.get("dims", [])
        intent_out = attr_map.get("intent", self.current_function is None)
        entity_list = self.translate_AST_Class(ast.children[2], type_name, array_dims, intent_out)  # Entity_Decl_List
        decl_dict = {}
        for entity in entity_list:
            decl_dict[entity.var_name] = entity
        return decl_dict

    def translate_Dimension_Attr_Spec(self, ast, attr_map):
        assert isinstance(ast, Dimension_Attr_Spec)
        attr_map["dims"] = self.translate_Each_Children(ast.children[1]) if ast.children[1] else []

    def translate_Intent_Attr_Spec(self, ast, attr_map):
        assert isinstance(ast, Intent_Attr_Spec)
        attr_map["intent"] = str(ast.children[1]) != "IN"

    def translate_Intrinsic_Type_Spec(self, ast):
        assert isinstance(ast, Intrinsic_Type_Spec)
        return sdsl.Intrinsic_Type_Map[ast.children[0]]

    def translate_Declaration_Type_Spec(self, ast):
        assert isinstance(ast, Declaration_Type_Spec)
        return str(ast.children[1])  # Fortran要使用type关键字，DSL直接使用名字即可

    def translate_Entity_Decl(self, ast, type_name, array_dims, intent_out):
        assert isinstance(ast, Entity_Decl)
        if len(ast.children) == 4:
            if len(array_dims) == 0 and ast.children[1]:
                array_dims = self.translate_AST_Class(ast.children[1])  # Array_Spec
            elif ast.children[1]:
                raise "Mixing 2 types of array declaration"
            var_name = str(ast.get_name())
            init_val = self.translate_AST_Class(ast.children[3]) if ast.children[3] else None
            if isinstance(self.decl_dict.get(var_name, None), sdsl.FuncDecl):
                var_name += "_return"
            return sdsl.VarDecl(type_name, self.is_local, var_name, array_dims, intent_out, init_val)

    def translate_Explicit_Shape_Spec(self, ast):
        assert isinstance(ast, Explicit_Shape_Spec)
        assert ast.children[1]
        lower_bound = str(ast.children[0]) if ast.children[0] else "1"  # 注意不是从0开始！
        upper_bound = str(ast.children[1])
        return [lower_bound, upper_bound]

    def translate_Derived_Type_Def(self, ast):
        assert isinstance(ast, Derived_Type_Def)
        type_name = self.translate_Derived_Type_Stmt(ast.children[0])
        elem_list = []
        for child in ast.children:
            if isinstance(child, Component_Part):
                elem_list += self.translate_Component_Part(child)
        return sdsl.TypeDecl(type_name, self.is_local, elem_list)

    def translate_Derived_Type_Stmt(self, ast):
        assert isinstance(ast, Derived_Type_Stmt)
        return ast.get_start_name()

    def translate_Component_Part(self, ast):
        assert isinstance(ast, Component_Part)
        return functools.reduce(list.__add__, self.translate_Each_Children(ast), [])

    def translate_Data_Component_Def_Stmt(self, ast):
        assert isinstance(ast, Data_Component_Def_Stmt)
        elem_type = self.translate_AST_Class(ast.children[0])  # Declaration_Type_Spec
        attr_map = {}
        if ast.children[1]:
            self.translate_AST_Class(ast.children[1], attr_map)  # Attr_Spec_List
        array_dims = attr_map.get("dims", [])
        return self.translate_AST_Class(ast.children[2], elem_type, array_dims)  # Component_Decl_List

    def translate_Component_Decl(self, ast, elem_type, array_dims):
        assert isinstance(ast, Component_Decl)
        if len(ast.children) == 4:
            if len(array_dims) == 0 and ast.children[1]:
                array_dims = self.translate_AST_Class(ast.children[1])  # Array_Spec
            elif ast.children[1]:
                raise "Mixing 2 types of array declaration"
            elem_name = str(ast.children[0])
            init_val = self.translate_AST_Class(ast.children[3]) if ast.children[3] else None
            return sdsl.VarDecl(elem_type, self.is_local, elem_name, array_dims, None, init_val)

    def translate_Initialization(self, ast):
        assert isinstance(ast, Initialization)
        return self.translate_AST_Class(ast.children[1], False)

    def translate_Component_Initialization(self, ast):
        assert isinstance(ast, Component_Initialization)
        return self.translate_AST_Class(ast.children[1])

    # ------------------------------------------------------------
    # control flow
    # ------------------------------------------------------------
    def translate_Execution_Part(self, ast, param_list):
        assert isinstance(ast, Execution_Part)
        return functools.reduce(list.__add__, self.translate_Each_Children(ast), [])

    def translate_If_Stmt(self, ast):
        assert isinstance(ast, If_Stmt)
        cond = self.translate_AST_Class(ast.children[0], False)
        body = self.translate_AST_Class(ast.children[1])
        return ["if " + cond, body, "end if"]

    def translate_If_Construct(self, ast):
        assert isinstance(ast, If_Construct)
        return self.translate_Each_Children(ast)

    def translate_If_Then_Stmt(self, ast):
        assert isinstance(ast, If_Then_Stmt)
        return "if " + self.translate_AST_Class(ast.children[0], False)

    def translate_Else_If_Stmt(self, ast):
        assert isinstance(ast, Else_If_Stmt)
        return "else if " + self.translate_AST_Class(ast.children[0], False)

    def translate_Else_Stmt(self, ast):
        assert isinstance(ast, Else_Stmt)
        return "else"

    def translate_End_If_Stmt(self, ast):
        assert isinstance(ast, End_If_Stmt)
        return "end if"

    def translate_Block_Nonlabel_Do_Construct(self, ast):
        assert isinstance(ast, Block_Nonlabel_Do_Construct)
        return self.translate_Each_Children(ast)

    def translate_Nonlabel_Do_Stmt(self, ast):
        assert isinstance(ast, Nonlabel_Do_Stmt)
        return self.translate_Loop_Control(ast.children[1])

    def translate_End_Do_Stmt(self, ast):
        assert isinstance(ast, End_Do_Stmt)
        return "end do"

    def translate_Loop_Control(self, ast):
        assert isinstance(ast, Loop_Control)
        assert len(ast.children) == 3
        assert ast.children[0] is None and ast.children[2] is None
        counter = ast.children[1]
        return "do %s = %s" % (counter[0], ", ".join([self.translate_AST_Class(item, False) for item in counter[1]]))

    def translate_Return_Stmt(self, ast):
        assert isinstance(ast, Return_Stmt)
        if ast.children[0] is None:
            return []
        assert False  # TODO

    # ------------------------------------------------------------
    # statement
    # ------------------------------------------------------------
    def translate_Assignment_Stmt(self, ast):
        assert isinstance(ast, Assignment_Stmt)
        left = self.translate_AST_Class(ast.children[0], True)
        right = self.translate_AST_Class(ast.children[2], False)
        if isinstance(left, sdsl.FuncDecl):
            return left.function_format(self.decl_dict, ["%s_return = %s" % (left.func_name, right)])
        if left == self.current_function:
            left += "_return"
        if '[' in left or '.' in left or left in self.decl_dict:
            return ["%s = %s" % (left, right)]
        else:
            return ["var %s = %s" % (left, right)]

    def translate_Call_Stmt(self, ast):
        assert isinstance(ast, Call_Stmt)
        return ["// " + str(ast)]  # 忽略调用语句

    def translate_Write_Stmt(self, ast, *args):
        assert isinstance(ast, Write_Stmt)
        return ["// " + str(ast)]  # 忽略输出语句

    # ------------------------------------------------------------
    # expression
    # ------------------------------------------------------------
    def translate_Data_Ref(self, ast, is_left_value):
        assert isinstance(ast, Data_Ref)
        base = self.translate_AST_Class(ast.children[0], is_left_value)
        elem = self.translate_AST_Class(ast.children[1], False)
        return "%s.%s" % (base, elem)

    def translate_Part_Ref(self, ast, is_left_value):
        assert isinstance(ast, Part_Ref)
        base = self.translate_AST_Class(ast.children[0], is_left_value)
        index = self.translate_AST_Class(ast.children[1], False)
        base_decl = self.decl_dict.get(base, None)
        if isinstance(base_decl, sdsl.VarDecl) and len(base_decl.array_dims) > 0:
            return "%s[%s]" % (base, ", ".join(index))
        if not is_left_value:
            return "%s(%s)" % (base, ", ".join(index))
        # 匿名函数
        return sdsl.FuncDecl(base, self.is_local, index, "lambda")

    def translate_Parenthesis(self, ast, is_left_value):
        assert isinstance(ast, Parenthesis)
        return "(%s)" % self.translate_AST_Class(ast.children[1], is_left_value)

    def translate_Intrinsic_Function_Reference(self, ast, is_left_value):
        assert isinstance(ast, Intrinsic_Function_Reference)
        name = str(ast.children[0])
        argv = self.translate_AST_Class(ast.children[1], False)
        return sdsl.intrinsic_function(name, argv)

    def translate_UnaryOpBase(self, ast, is_left_value):
        assert isinstance(ast, UnaryOpBase)
        op = ast.children[0]
        right = self.translate_AST_Class(ast.children[1], False)
        return sdsl.unary_operator(op, right)

    def translate_BinaryOpBase(self, ast, is_left_value):
        assert isinstance(ast, BinaryOpBase)
        left = self.translate_AST_Class(ast.children[0], False)
        op = ast.children[1]
        right = self.translate_AST_Class(ast.children[2], False)
        return sdsl.binary_operator(left, op, right)

    translate_Mult_Operand = translate_BinaryOpBase
    translate_Add_Operand = translate_BinaryOpBase
    translate_Or_Operand = translate_BinaryOpBase
    translate_Equiv_Operand = translate_BinaryOpBase
    translate_Level_2_Expr = translate_BinaryOpBase
    translate_Level_2_Unary_Expr = translate_UnaryOpBase
    translate_Level_3_Expr = translate_BinaryOpBase
    translate_Level_4_Expr = translate_BinaryOpBase
    translate_And_Operand = translate_UnaryOpBase
    translate_Or_Operand = translate_BinaryOpBase
    translate_Equiv_Operand = translate_BinaryOpBase
    translate_Level_5_Expr = translate_BinaryOpBase
    translate_Expr = translate_BinaryOpBase

    # ------------------------------------------------------------
    # literal
    # ------------------------------------------------------------
    def translate_Int_Literal_Constant(self, ast, *args):
        assert isinstance(ast, Int_Literal_Constant)
        return str(ast.children[0])

    def translate_Real_Literal_Constant(self, ast, *args):
        assert isinstance(ast, Real_Literal_Constant)
        value = str(ast.children[0])
        index = value.find('D')
        if index > 0:
            value = value[:index]  # 去掉那些奇怪的后缀
        return str(float(value))

    def translate_Logical_Literal_Constant(self, ast, *args):
        assert isinstance(ast, Logical_Literal_Constant)
        return str(ast.children[0])[1:-1].lower()

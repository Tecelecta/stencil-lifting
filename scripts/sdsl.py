import functools


class Decl:
    def __init__(self, type_name, is_local):
        self.type_name = type_name
        self.is_local = is_local


class VarDecl(Decl):
    def __init__(self, type_name, is_local, var_name, array_dims, intent_out, init_val):
        Decl.__init__(self, type_name, is_local)
        self.var_name = var_name
        self.array_dims = array_dims
        self.intent_out = intent_out
        self.init_val = init_val

    def parameter_format(self):
        array_suffix = specified_length_array(self.array_dims) if len(self.array_dims) > 0 else ""
        return "%s%s %s" % (self.type_name, array_suffix, self.var_name)

    def variable_format(self):
        array_suffix = specified_length_array(self.array_dims) if len(self.array_dims) > 0 else ""
        return "var %s = %s%s(%s)" % (
            self.var_name, self.type_name, array_suffix, self.init_val if self.init_val else "")


class TypeDecl(Decl):
    def __init__(self, type_name, is_local, elem_list):
        Decl.__init__(self, type_name, is_local)
        self.elem_list = elem_list

    def struct_format(self):
        head = "struct " + self.type_name
        body = [elem.variable_format() for elem in self.elem_list]
        tail = "end struct"
        return [head, body, tail, ""]

class FuncDecl(Decl):
    def __init__(self, func_name, is_local, param_list, decl_kind):
        Decl.__init__(self, "func", is_local)
        self.func_name = func_name
        self.param_list = param_list
        self.decl_kind = decl_kind

    def function_head_format(self, decl_dict):
        ret_decl = []
        param_decl = []
        for param in self.param_list:
            decl = decl_dict.get(param, None)
            if decl:
                s = decl.parameter_format()
                if self.decl_kind != "lambda" and decl.intent_out:
                    ret_decl.append(s)
            else:
                s = "%s %s" % (get_implicit_type(param), param)
            param_decl.append(s)
        if self.decl_kind != "subroutine":
            ret_name = self.func_name + "_return"
            decl = decl_dict.get(ret_name, None)
            if decl:
                s = decl.parameter_format()
            else:
                s = "%s %s" % (get_implicit_type(param), ret_name)
            ret_decl.append(s)
        return "func %s = %s(%s)\n" % (", ".join(ret_decl), self.func_name, ", ".join(param_decl))

    def function_format(self, decl_dict, body):
        head = self.function_head_format(decl_dict)
        return [head, body, "end func", ""]


def array_bound_string(bound) -> str:
    try:
        return str(int(eval(bound)))
    except:
        return '?'


def undefined_length_array(dims: list) -> str:
    return "[%s]" % ", ".join(["?:?" for _ in dims])


def specified_length_array(dims: list) -> str:
    return "[%s]" % ", ".join([array_bound_string(dim[0]) + " : " + array_bound_string(dim[1]) for dim in dims])


def get_implicit_type(name: str) -> str:
    c = ord(name[0])
    return "Integer" if ord('I') <= c <= ord('N') else "Real"


def unary_operator(op: str, right: str):
    op = Operator_Map.get(op, op.lower())
    return "%s %s" % (op, right)


def binary_operator(left: str, op: str, right: str):
    op = Operator_Map.get(op, op.lower())
    return "%s %s %s" % (left, op, right)


def intrinsic_function(name: str, argv: list):
    return "%s(%s)" % (Intrinsic_Function_Map.get(name, name.lower()), ", ".join(argv))


def flatten_lines(src, tab_num):
    dst = []
    for item in src:
        if isinstance(item, str):
            item = '\t' * tab_num + item
            if not item.endswith('\n'):
                item += '\n'
            dst.append(item)
        elif isinstance(item, list):
            dst += flatten_lines(item, tab_num + 1)
    return dst


Intrinsic_Type_Map = {"INTEGER": "Integer", "REAL": "Real", "DOUBLE PRECISION": "Real", "LOGICAL": "Logic"}
Operator_Map = {".AND.": "&&", ".OR.": "||", ".NOT.": "!", ".EQV.": "==", ".NEQV.": "!=",
                ".GT.": ">", ".GE.": ">=", ".LT.": "<", ".LE.": "<=", ".EQ.": "==", ".NE.": "!="}
Intrinsic_Function_Map = {}

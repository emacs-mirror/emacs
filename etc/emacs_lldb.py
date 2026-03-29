# Copyright (C) 2022-2026 Free Software Foundation, Inc.
#
# This file is part of GNU Emacs.
#
# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

# Load this module in LLDB with
#
# (lldb) command script import emacs_lldb
#
# Available commands start with 'x' and can be seen with
#
# (lldb) help

import lldb


########################################################################
#                              Utilities
########################################################################

# Return the name of enumerator ENUM as a string.
def enumerator_name(enum):
    enumerators = enum.GetType().GetEnumMembers()
    for enum_member in enumerators:
        if enum.GetValueAsUnsigned() == enum_member.GetValueAsUnsigned():
            return enum_member.GetName()
    return None

# A class wrapping an SBValue for a Lisp_Object, providing convenience
# functions.
class Lisp_Object:
    # Map pvec_type enumerators to corresponding C types.
    pvec2type = {
        "PVEC_FRAME": "struct frame",
        "PVEC_WINDOW": "struct window",
        "PVEC_BIGNUM": "struct Lisp_Bignum",
        "PVEC_MARKER": "struct Lisp_Marker",
        "PVEC_OVERLAY": "struct Lisp_Overlay",
        "PVEC_FINALIZER": "struct Lisp_Finalizer",
        "PVEC_SYMBOL_WITH_POS": "struct Lisp_Symbol_With_Pos",
        "PVEC_MISC_PTR": "",
        "PVEC_USER_PTR": "struct Lisp_User_Ptr",
        "PVEC_PROCESS": "struct Lisp_Process",
        "PVEC_BOOL_VECTOR": "struct Lisp_Bool_Vector",
        "PVEC_BUFFER": "struct buffer",
        "PVEC_HASH_TABLE": "struct Lisp_Hash_Table",
        "PVEC_OBARRAY": "struct Lisp_Obarray",
        "PVEC_TERMINAL": "struct terminal",
        "PVEC_WINDOW_CONFIGURATION": "struct save_window_data",
        "PVEC_SUBR": "struct Lisp_Subr",
        "PVEC_OTHER": "void",
        "PVEC_XWIDGET": "void",
        "PVEC_XWIDGET_VIEW": "void",
        "PVEC_THREAD": "struct thread_state",
        "PVEC_MUTEX": "Lisp_Mutex",
        "PVEC_CONDVAR": "Lisp_CondVar",
        "PVEC_MODULE_FUNCTION": "struct Lisp_Module_Function",
        "PVEC_NATIVE_COMP_UNIT": "struct Lisp_Native_Comp_Unit",
        "PVEC_SQLITE": "struct Lisp_Sqlite",
        "PVEC_CLOSURE": "struct Lisp_Vector",
        "PVEC_CHAR_TABLE": "struct Lisp_Vector",
        "PVEC_SUB_CHAR_TABLE": "struct Lisp_Sub_Char_Table",
        "PVEC_RECORD": "struct Lisp_Vector",
        "PVEC_FONT": "struct font",
        "PVEC_NORMAL_VECTOR": "struct Lisp_Vector",
        "PVEC_TS_NODE": "struct Lisp_TS_Node",
        "PVEC_TS_PARSER": "struct Lisp_TS_Parser",
        "PVEC_TS_COMPILED_QUERY": "struct Lisp_TS_Query",
    }

    # Object construction/initialization.
    def __init__(self, lisp_obj):
        self.tagged = lisp_obj
        self.unsigned = None
        self.lisp_type = None
        self.pvec_type = None
        self.untagged = None
        self.init_unsigned()
        self.init_lisp_types()
        self.init_values()

    def init_unsigned(self):
        if self.tagged.GetType().GetTypeClass() == lldb.eTypeClassStruct:
            # Lisp_Object is actually a struct.
            lisp_word = self.tagged.GetValueForExpressionPath(".i")
            self.unsigned = lisp_word.GetValueAsUnsigned()
        else:
            self.unsigned = self.tagged.GetValueAsUnsigned()

    # Initialize self.lisp_type to the C Lisp_Type enumerator of the
    # Lisp_Object, as a string.  Initialize self.pvec_type likewise to
    # the pvec_type enumerator if the object is a vector-like, as a
    # string.
    def init_lisp_types(self):
        GCTYPEBITS = self.unsigned_const('GCTYPEBITS')
        self.lisp_type = self.tag_name(self.unsigned
                                       & ((1 << GCTYPEBITS) - 1))
        if self.lisp_type == "Lisp_Vectorlike":
            self.pvec_type = "PVEC_NORMAL_VECTOR"
            vector = self.get_lisp_pointer("struct Lisp_Vector")
            size = vector.GetValueForExpressionPath("->header.size")
            size = size.GetValueAsUnsigned()
            PSEUDOVECTOR_FLAG = self.unsigned_const('PSEUDOVECTOR_FLAG')
            if size & PSEUDOVECTOR_FLAG:
                PVEC_TYPE_MASK = self.unsigned_const(
                    'More_Lisp_Bits::PVEC_TYPE_MASK')
                PSEUDOVECTOR_AREA_BITS = self.unsigned_const(
                    'More_Lisp_Bits::PSEUDOVECTOR_AREA_BITS')
                pvec = (size & PVEC_TYPE_MASK) >> PSEUDOVECTOR_AREA_BITS
                self.pvec_type = self.pvec_name(pvec)

    # Initialize self.untagged according to lisp_type and pvec_type.
    def init_values(self):
        lt = self.lisp_type
        if lt == "Lisp_Symbol":
            offset = self.get_lisp_pointer("char").GetValueAsUnsigned()
            self.untagged = self.eval(
                f"(struct Lisp_Symbol *)((char *)&lispsym + {offset})")
        elif lt == "Lisp_String":
            self.untagged = self.get_lisp_pointer("struct Lisp_String")
        elif lt == "Lisp_Vectorlike":
            c_type = Lisp_Object.pvec2type[self.pvec_type]
            self.untagged = self.get_lisp_pointer(c_type)
        elif lt == "Lisp_Cons":
            self.untagged = self.get_lisp_pointer("struct Lisp_Cons")
        elif lt == "Lisp_Float":
            self.untagged = self.get_lisp_pointer("struct Lisp_Float")
        elif lt in ("Lisp_Int0", "Lisp_Int1"):
            GCTYPEBITS = self.unsigned_const('GCTYPEBITS')
            x = self.unsigned >> (GCTYPEBITS - 1)
            self.untagged = self.eval(f"(EMACS_INT){x})")
        elif lt == "Lisp_Type_Unused0":
            self.untagged = self.unsigned
        else:
            assert False, f"Unknown Lisp type {self.lisp_type}"

    # Get a numeric constant (unsigned).
    const_cache = {}
    def unsigned_const(self, name):
        val = self.const_cache.get(name)
        if val is None:
            frame = self.tagged.GetFrame()
            val = frame.EvaluateExpression(name).GetValueAsUnsigned()
            self.const_cache[name] = val
        return val

    # Get the name of a Lisp_Object tag value, like "Lisp_String".
    tag_cache = {}
    def tag_name(self, tag):
        name = self.tag_cache.get(tag)
        if name is None:
            frame = self.tagged.GetFrame()
            val = frame.EvaluateExpression(f"(enum Lisp_Type){tag}")
            name = enumerator_name(val)
            self.tag_cache[tag] = name
        return name

    # Get the name of a pseudovector type tag, like "PVEC_HASH_TABLE".
    pvec_cache = {}
    def pvec_name(self, pvec):
        name = self.pvec_cache.get(pvec)
        if name is None:
            frame = self.tagged.GetFrame()
            val = frame.EvaluateExpression(f"(enum pvec_type){pvec}")
            name = enumerator_name(val)
            self.pvec_cache[pvec] = name
        return name

    # Evaluate EXPR in the context of the current frame.
    eval_cache = {}
    def eval(self, expr):
        val = self.eval_cache.get(expr)
        if val is None:
            frame = self.tagged.GetFrame()
            val = frame.EvaluateExpression(expr)
            self.eval_cache[expr] = val
        return val

    # Return an SBValue for this object denoting a pointer of type
    # TYP*.
    lisp_ptr_cache = {}
    def get_lisp_pointer(self, typ):
        uns = self.unsigned
        ptr = self.lisp_ptr_cache.get((uns, typ))
        if ptr is None:
            VALMASK = self.unsigned_const('VALMASK')
            frame = self.tagged.GetFrame()
            ptr = frame.EvaluateExpression(
                f"({typ}*)(EMACS_INT){uns & VALMASK}")
            self.lisp_ptr_cache[(uns, typ)] = ptr
        return ptr

    # If this is a Lisp_String, return an SBValue for its string data.
    # Return None otherwise.
    def get_string_data(self):
        if self.lisp_type == "Lisp_String":
            return self.untagged.GetValueForExpressionPath("->u.s.data")
        return None

    # if this is a Lisp_Symbol, return an SBBalue for its name.
    # Return None otherwise.
    def get_symbol_name(self):
        if self.lisp_type == "Lisp_Symbol":
            name = self.untagged.GetValueForExpressionPath("->u.s.name")
            return Lisp_Object(name).get_string_data()
        return None

    # Return a summary string for this object.
    def summary(self):
        return str(self.untagged)


########################################################################
#                           LLDB Commands
########################################################################

def xbacktrace(debugger, command, ctx, result, internal_dict):
    """Print Emacs Lisp backtrace"""
    frame = ctx.GetFrame()
    n = frame.EvaluateExpression(
        "current_thread->m_specpdl_ptr - current_thread->m_specpdl")
    for i in reversed(range(0, n.GetValueAsUnsigned())):
        s = frame.EvaluateExpression(f"current_thread->m_specpdl[{i}]")
        kind = enumerator_name(s.GetChildMemberWithName("kind"))
        if kind == "SPECPDL_BACKTRACE":
            function = Lisp_Object(s.GetValueForExpressionPath(".bt.function"))
            if function.lisp_type == "Lisp_Symbol":
                sym_name = function.get_symbol_name()
                result.AppendMessage(str(sym_name))
            elif function.lisp_type == "Lisp_Vectorlike":
                result.AppendMessage(function.pvec_type)
            else:
                result.AppendMessage(function.lisp_type)

def xdebug_print(debugger, command, result, internal_dict):
    """Print Lisp_Objects using safe_debug_print()"""
    debugger.HandleCommand(f"expr safe_debug_print({command})")


########################################################################
#                             Formatters
########################################################################

def type_summary_Lisp_Object(obj, internal_dict):
    return Lisp_Object(obj).summary()

class Lisp_Object_Provider:
    """Synthetic children provider for Lisp_Objects.
    Supposedly only used by 'frame variable', where -P <n> can be used
    to specify a printing depth. """
    def __init__(self, valobj, internal_dict):
        self.valobj = valobj
        self.children = {}

    def update(self):
        lisp_obj = Lisp_Object(self.valobj)
        lisp_type = lisp_obj.lisp_type
        try:
            if lisp_type == "Lisp_Symbol":
                child = lisp_obj.get_symbol_name()
                self.children["name"] = child
            elif lisp_type == "Lisp_String":
                child = lisp_obj.get_string_data()
                self.children["data"] = child
            elif lisp_type == "Lisp_Cons":
                car = lisp_obj.untagged.GetValueForExpressionPath("->u.s.car")
                cdr = lisp_obj.untagged.GetValueForExpressionPath("->u.s.u.cdr")
                self.children["car"] = car
                self.children["cdr"] = cdr
            else:
                self.children["untagged"] = lisp_obj.untagged
        except:
            print(f"*** exception in child provider update for {lisp_type}")
            pass

    def num_children(self):
        return len(self.children)

    def get_child_index(self, name):
        index = 0
        for child_name, child in self.children:
            if child_name == name:
                return index
            index = index + 1
        return -1

    def get_child_at_index(self, index):
        key = list(self.children)[index]
        return self.children[key]


########################################################################
#                           Initialization
########################################################################

# Define Python FUNCTION as an LLDB command.
def define_command (debugger, function):
    lldb_command = function.__name__
    python_function = __name__ + "." + function.__name__
    interpreter = debugger.GetCommandInterpreter()
    def define(overwrite):
        res = lldb.SBCommandReturnObject()
        interpreter.HandleCommand(f"command script add "
                                  f"{overwrite} "
                                  f"--function {python_function} "
                                  f"{lldb_command}",
                                  res)
        return res.Succeeded()
    if not define("--overwrite"):
        define("")

# Define Python FUNCTION as an LLDB type summary provider for types
# matching REGEX.  Type summaries defined here are defined in the
# category Emacs, and can be seen with 'type summary list -w Emacs',
# and deleted in a similar way.
def define_type_summary(debugger, regex, function):
    python_function = __name__ + "." + function.__name__
    debugger.HandleCommand(f"type summary add --expand "
                           f"--cascade true "
                           f"--category Emacs "
                           f"--python-function {python_function} "
                           + regex)

# Define Python class CLS as a children provider for the types
# matching REFEXP.  Providers are defined in the category Emacs, and
# can be seen with 'type synthetic list -w Emacs', and deleted in a
# similar way.
def define_type_synthetic(debugger, regex, cls):
    python_class = __name__ + "." + cls.__name__
    debugger.HandleCommand(f"type synthetic add "
                           f"--category Emacs "
                           f"--python-class {python_class} "
                           + regex)

# Enable a given category of type summary providers.
def enable_type_category(debugger, category):
    debugger.HandleCommand(f"type category enable {category}")

# This function is called by LLDB to initialize the module.
def __lldb_init_module(debugger, internal_dict):
    define_command(debugger, xbacktrace)
    define_command(debugger, xdebug_print)
    define_type_summary(debugger, "Lisp_Object", type_summary_Lisp_Object)
    define_type_synthetic(debugger, "Lisp_Object", Lisp_Object_Provider)
    enable_type_category(debugger, "Emacs")
    print('Emacs debugging support has been installed.')

# end.

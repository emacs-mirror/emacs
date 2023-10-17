# Copyright (C) 2022-2023 Free Software Foundation, Inc.
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
        "PVEC_COMPILED": "struct Lisp_Vector",
        "PVEC_CHAR_TABLE": "struct Lisp_Vector",
        "PVEC_SUB_CHAR_TABLE": "void",
        "PVEC_RECORD": "struct Lisp_Vector",
        "PVEC_FONT": "struct font",
        "PVEC_NORMAL_VECTOR": "struct Lisp_Vector"
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
        t = self.eval(f"(enum Lisp_Type)"
                      f"((EMACS_INT) {self.unsigned} "
                      f"& (1 << GCTYPEBITS) - 1)")
        self.lisp_type = enumerator_name(t)
        if self.lisp_type == "Lisp_Vectorlike":
            self.pvec_type = "PVEC_NORMAL_VECTOR"
            vector = self.get_lisp_pointer("struct Lisp_Vector")
            size = vector.GetValueForExpressionPath("->header.size")
            size = size.GetValueAsUnsigned()
            pseudo = self.eval(f"{size} & PSEUDOVECTOR_FLAG")
            if pseudo.GetValueAsUnsigned() != 0:
                typ = self.eval(
                    f"(enum pvec_type) (({size} "
                    f"& More_Lisp_Bits::PVEC_TYPE_MASK) "
                    f">> More_Lisp_Bits::PSEUDOVECTOR_AREA_BITS)")
                self.pvec_type = enumerator_name(typ)

    # Initialize self.untagged according to lisp_type and pvec_type.
    def init_values(self):
        if self.lisp_type == "Lisp_Symbol":
            offset = self.get_lisp_pointer("char").GetValueAsUnsigned()
            self.untagged = self.eval(f"(struct Lisp_Symbol *)"
                                   f" ((char *) &lispsym + {offset})",
                                   True)
        elif self.lisp_type == "Lisp_String":
            self.untagged = self.get_lisp_pointer("struct Lisp_String", True)
        elif self.lisp_type == "Lisp_Vectorlike":
            c_type = Lisp_Object.pvec2type[self.pvec_type]
            self.untagged = self.get_lisp_pointer(c_type, True)
        elif self.lisp_type == "Lisp_Cons":
            self.untagged = self.get_lisp_pointer("struct Lisp_Cons", True)
        elif self.lisp_type == "Lisp_Float":
            self.untagged = self.get_lisp_pointer("struct Lisp_Float", True)
        elif self.lisp_type in ("Lisp_Int0", "Lisp_Int1"):
            self.untagged = self.eval(f"((EMACS_INT) {self.unsigned}) "
                                      f">> (GCTYPEBITS - 1)", True)
        elif self.lisp_type == "Lisp_Type_Unused0":
            self.untagged = self.unsigned
        else:
            assert False, f"Unknown Lisp type {self.lisp_type}"

    # Evaluate EXPR in the context of the current frame.
    def eval(self, expr, make_var=False):
        frame = self.tagged.GetFrame()
        if make_var:
            return frame.EvaluateExpression(expr)
        options = lldb.SBExpressionOptions()
        options.SetSuppressPersistentResult(True)
        return frame.EvaluateExpression(expr, options)

    # Return an SBValue for this object denoting a pointer of type
    # TYP*.
    def get_lisp_pointer(self, typ, make_var=False):
        return self.eval(f"({typ}*) (((EMACS_INT) "
                         f"{self.unsigned}) & VALMASK)",
                         make_var)

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

# According to SBCommanInterpreter.cpp, the return value of
# HandleCompletions is as follows:
#
# Index 1 to the end contain all the completions.
#
# At index 0:
#
# If all completions have a common prefix, this is the shortest
# completion, with the common prefix removed from it.
#
# If it is the completion for a whole word, a space is added at the
# end.
#
# So, the prefix is what could be added to make the command partially
# complete.
#
# If there is no common prefix, index 0 has an empty string "".

def xcomplete(debugger, command, result, internal_dict):
    """Print completions for COMMAND."""
    interpreter = debugger.GetCommandInterpreter()
    string_list = lldb.SBStringList()
    interpreter.HandleCompletion(command, len(command), len(command),
                                 -1, string_list)
    list = ""
    for i in range(string_list.GetSize()):
        list += '"' + string_list.GetStringAtIndex(i) + '" '
    result.AppendMessage("(" + list + ")")


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
    define_command(debugger, xcomplete)
    define_type_summary(debugger, "Lisp_Object", type_summary_Lisp_Object)
    define_type_synthetic(debugger, "Lisp_Object", Lisp_Object_Provider)
    enable_type_category(debugger, "Emacs")
    print('Emacs debugging support has been installed.')

# end.

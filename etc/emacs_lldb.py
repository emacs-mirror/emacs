# Copyright (C) 2022 Free Software Foundation, Inc.
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
#                              Utilties
########################################################################

# Return the Lisp_Type of Lisp_Object OBJ.
def get_lisp_type(obj):
    int_value = obj.GetValueAsUnsigned()
    return obj.GetFrame().EvaluateExpression(
        f"(enum Lisp_Type) ((EMACS_INT) {int_value} "
        "& (1 << GCTYPEBITS) - 1)")

# Return the Lisp_Type or pseudo-vector type of OBJ.
def get_lisp_type_or_vectorlike(obj):
    lisp_type = get_lisp_type(obj)
    if enumerator_name(lisp_type) == "Lisp_Vectorlike":
        vector = get_lisp_pointer(obj, "struct Lisp_Vector")
        header_size = vector.GetValueForExpressionPath(
            "->header.size").GetValueAsUnsigned()
        frame = obj.GetFrame()
        pseudo = frame.EvaluateExpression(
            f"{header_size} & PSEUDOVECTOR_FLAG")
        if pseudo.GetValueAsUnsigned() != 0:
            return frame.EvaluateExpression(
                f"(enum pvec_type) (({header_size} "
                "& More_Lisp_Bits::PVEC_TYPE_MASK) "
                ">> More_Lisp_Bits::PSEUDOVECTOR_AREA_BITS)")
        return frame.EvaluateExpression("pvec_type::PVEC_NORMAL_VECTOR")
    return lisp_type

# Return Lisp_Object OBJ as pointer to TYP *.
def get_lisp_pointer(obj, typ):
    return obj.GetFrame().EvaluateExpression(
        f"({typ}*) (((EMACS_INT) {obj.GetValueAsUnsigned()}) & VALMASK)")

# Return Lisp_Object OBJ as pointer to Lisp_Symbol.
def get_lisp_symbol(obj):
    ptr = get_lisp_pointer(obj, "char")
    offset = ptr.GetValueAsUnsigned()
    return obj.GetFrame().EvaluateExpression(
        f"(struct Lisp_Symbol *) ((char *) &lispsym + {offset})")

# Return Lisp_Object OBJ as pointer to Lisp_String
def get_lisp_string(obj):
    return get_lisp_pointer(obj, "struct Lisp_String")

# Return the string data of Lisp_Object OBJ which denotes a Lisp_String.
def get_lisp_string_data(obj):
    string = get_lisp_string(obj)
    return string.GetValueForExpressionPath("->u.s.data")

# Assuming OBJ denotes a Lisp_Symbol, return the name of the symbol.
def get_lisp_symbol_name(obj):
    sym = get_lisp_symbol(obj)
    name = sym.GetValueForExpressionPath("->u.s.name")
    return get_lisp_string_data(name)

# Return a string for the enuerator ENUM.
def enumerator_name(enum):
    enumerators = enum.GetType().GetEnumMembers()
    return enumerators[enum.GetValueAsUnsigned()].GetName()


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
            function = s.GetValueForExpressionPath(".bt.function")
            function_type = enumerator_name(get_lisp_type(function))
            if function_type == "Lisp_Symbol":
                sym_name = get_lisp_symbol_name(function)
                result.AppendMessage(str(sym_name))
            elif function_type == "Lisp_Vectorlike":
                subtype = get_lisp_type_or_vectorlike(function)
                result.AppendMessage(str(subtype))
            else:
                result.AppendMessage(function_type)

def xdebug_print(debugger, command, result, internal_dict):
    """Print Lisp_Objects using safe_debug_print()"""
    debugger.HandleCommand(f"expr safe_debug_print({command})")


########################################################################
#                             Formatters
########################################################################

# Return a type summary for Lisp_Objects.
def format_Lisp_Object(obj, internal_dict):
    lisp_type = get_lisp_type_or_vectorlike(obj)
    kind = enumerator_name(lisp_type)
    summary = "-> "
    if kind == "PVEC_FRAME":
        ptr = get_lisp_pointer(obj, "struct frame")
        summary += str(ptr)
    elif kind == "PVEC_WINDOW":
        ptr = get_lisp_pointer(obj, "struct window")
        summary += str(ptr)
    return summary


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

# Define Python FUNCTION as an LLDB type formatter.
def define_formatter(debugger, regex, function):
    python_function = __name__ + "." + function.__name__
    debugger.HandleCommand(f"type summary add "
                           f"--cascade true "
                           f'--regex "{regex}" '
                           f"--python-function {python_function}")

# This function is called by LLDB to initialize the module.
def __lldb_init_module(debugger, internal_dict):
    define_command(debugger, xbacktrace)
    define_command(debugger, xdebug_print)
    define_formatter(debugger, "Lisp_Object", format_Lisp_Object)
    print('Emacs debugging support has been installed.')

# end.

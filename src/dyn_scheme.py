import re
import math
import operator as op

from parsec import *

class Symbol:
    def __init__(self, s):
        self.str = s

    def __eq__(self, other):
        return self.str == other.str

    def __str__(self):
        return ('#{%s}' % self.str)

    def __repr__(self):
        return self.__str__()

# ignore cases.
@generate
def comment():
    yield string(';')
    yield many(none_of('\n'))
    # Optional not yet supported
    yield string('\n') ^ string('')

ignore = comment ^ spaces()

lparen = string('(')
rparen = string(')')


def escaped_char_parser(char, res):
    return string('\\' + char).result(res)

escaped_char = escaped_char_parser('a', '\a') ^ \
    escaped_char_parser('b', '\b') ^ \
    escaped_char_parser('t', '\t') ^ \
    escaped_char_parser('n', '\n') ^ \
    escaped_char_parser('r', '\r') ^ \
    escaped_char_parser('\"', '\"') ^ \
    escaped_char_parser('\\', '\\') ^ \
    escaped_char_parser('|', '|')
  
@generate
def number():
    n = yield regex(r'(\+|-)?(0|[1-9][0-9]*)([.][0-9]+)?([eE][+-]?[0-9]+)?')
    return float(n)

initial = regex(r'[a-zA-Z]') | one_of("!$%&*/:<=>?@^_~")
special_subsequent = one_of('+-.@')
sign_subsequent = initial ^ one_of('+-@')
subsequent = initial ^ regex(r'[0-9]') ^ special_subsequent

@generate
def normal_identifier():
    init = yield initial
    sub = yield many(subsequent)
    return init + ''.join(sub)

# See standard RSR7 chapter 7.
# This is the second row of 'peculiar indetifier'
@generate
def peculiar_identifier():
    sign = yield one_of('+-')
    sub = yield sign_subsequent
    sub_rest = yield many(sign_subsequent)

    return sign + sub + ''.join(sub_rest)
     
symbol = (one_of('+-') ^ normal_identifier ^ peculiar_identifier).parsecmap(Symbol)

escaped_or_char = escaped_char ^ none_of('"')

def list_to_string(lst):
    return ''.join(lst)

# Very inefficient, maybe optimize later
string_ = (string('"') >> many(escaped_or_char) << string('"')) \
        .parsecmap(list_to_string)

boolean = (string('#true') ^ string('#t')).result(True) ^ \
            (string('#false') ^ string('#f')).result(False)

# Please see the specification of RSR7, chapter 7.
# I try to use the same names as in the formal syntax.
simple_datum = boolean ^ number ^ string_ ^ symbol

@generate
def list_():
    yield lparen
    es = yield many(datum << ignore)
    yield rparen
    return es

@generate
def abbreviation():
    "Currently only supports quote '"
    yield string("'")
    e = yield datum
    return [Symbol('quote'), e]

compound_datum = list_ ^ abbreviation
datum = simple_datum ^ compound_datum

# With help of http://norvig.com/lispy.html
def standard_env():
    return {
        '+':op.add, '-':op.sub, '*':op.mul,
        '>':op.gt, '<':op.lt, '>=':op.ge, '<=':op.le, '=':op.eq,
    }

global_env = standard_env()

def eval(expr: str, env):
    if isinstance(expr, Symbol):
        return env[expr.str]
    elif not isinstance(expr, list):
        return expr
    else:
        proc = eval(expr[0], env)
        args = [eval(arg, env) for arg in expr[1:]]
        return proc(*args)

if __name__ == '__main__':
    while True:
        i = input("> ")
        print("Input: \t\t", i.__repr__())
        expr = datum.parse_strict(i)
        print("Parsed: \t", expr)
        print("Evaluated: \t", eval(expr, global_env))


import re
import sys
import math
import pprint
import operator as op
import collections.abc as cabc

from functools import reduce
from parsec import *

DEBUG = False

################### Data types

class Symbol:
    def __init__(self, s):
        self.str = s

    def __eq__(self, other):
        if(isinstance(other, Symbol)):
            return self.str == other.str
        else:
            return False
    
    def __str__(self):
        return self.str

class EmptyList:
    def __str__(self):
        return "()"

emptyList = EmptyList()

class Cons(cabc.Sequence):

    def __init__(self, a, b=None):
        if b == None:

            x = emptyList
            for el in reversed(list(a)):
                x = Cons(el, x)

            self.tup = x.tup
            self.is_list = True
        else:
            self.is_list = isinstance(b, Cons) and b.is_list
            self.tup = (a, b)
     
    def car(self):
        return self.tup[0]
    
    def cdr(self):
        return self.tup[1]
     
    def __iter__(self):
        pointer = self

        while True:
            if isinstance(pointer, Cons):
                yield pointer.car()
                pointer = pointer.cdr()
            elif pointer == emptyList:
                break
            else:
                yield pointer
                break
    
    def __len__(self):
        return reduce(lambda n,_: n + 1, self, 0)
     
    def __getitem__(self, key):
        return list(self)[key]
    
    def __str__(self):
        inner = list(map(str, self))
        
        if self.is_list:
            return "(" + " ".join(inner) + ")"
        else:
            return "(" + " ".join(inner[:-1]) + " . " + inner[-1] + ")" 
     
    def __eq__(self, other):
        if isinstance(other, Cons) or isinstance(other, list):
            return list(self) == list(other)
        else:
            return False
    
    # Eval all, return new Cons
    def eval_all(self, env):
        return Cons(map(lambda e: eval(env, e), self))

    # Eval all, return last
    def eval_all_ret_last(self, env):
        return reduce(lambda _, e: eval(env, e), self, None)

class Vector(list):
 
    def __str__(self):
        inner = map(str, self)
        return "#(" + " ".join(inner) + ")"

class String(str):

    def __str__(self):
        return self.__repr__()

# BultinFunction, DynFunction and Fexpr are callables
# When evaluated, they are passed an environment in which
# their 'body' should be evaluated (env) and an environment
# in which their parameters are evaluated (p_env).

class BuiltinFunction:
    def __init__(self, fn):
        self.fn = fn
    def __call__(self, env, p_env, args):
        return self.fn(*args.eval_all(p_env))
    def __str__(self):
        return "{BultinFunction %s}" % self.fn.__name__

# Dynamically scoped function
class DynFunction:
    def __init__(self, params, body):
        self.params, self.body = params, body
    def __call__(self, env, p_env, args):
        
        if isinstance(self.params, Cons):
            dct = dict(zip(map(str, self.params),
                           args.eval_all(p_env)))
            env.append(dct)
        elif isinstance(self.params, Symbol):
            env.append({self.params.str: args.eval_all(p_env)})
        else:
            raise ValueError('Fexpr: unknown parameter type')
        
        res = self.body.eval_all_ret_last(p_env)
        env.pop()
        return res
    
    def __str__(self):
        return "{DynamicFunction}"


# Dynamically scoped F expression
class Fexpr:
    def __init__(self, params, body):
        self.params, self.body = params, body
    def __call__(self, env, _, args):
        
        if isinstance(self.params, Cons):
            dct = dict(zip(map(str, self.params), args))
            env.append(dct)
        elif isinstance(self.params, Symbol):
            env.append({self.params.str: args})
        else:
            raise ValueError('Fexpr: unknown parameter type')

        res = self.body.eval_all_ret_last(env, self.body)
        env.pop()
         
        return res
    
    def __str__(self):
        return "{FunctionalExpression}"


# Control flow primitives

def eval_(env, p_env, args):
    return eval(env, eval(p_env, args[0]))

def fexpr(env, _, args):
    return Fexpr(args.car(), args.cdr())

def dyn_lambda(env, _, args):
    return DynFunction(args.car(), args.cdr())

def define(env, p_env, args):
    env[-1][args.car().str] = eval(p_env, args.cdr())

def if_(env, _, args):
    c = eval(env, cond)
    if eval(env, args[0]):
        return eval(env, args[1])
    else:
        return eval(env, args[2])

def equal(_, p_env, args):
    return env(p_env, args[0]) == env(p_env, args[1])


#################### Parsers

# ignore cases.
@generate
def comment():
    yield string(';')
    yield many(none_of('\n'))
    # Optional not yet supported
    yield string('\n') ^ string('')

ignore = comment ^ spaces()

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

# Inefficient, maybe optimize later
string_ = (string('"') >> many(escaped_or_char) << string('"')) \
        .parsecmap(lambda lst: String(''.join(lst)))

boolean = (string('#true') ^ string('#t')).result(True) ^ \
            (string('#false') ^ string('#f')).result(False)

# Please see the specification of RSR7, chapter 7.
# I try to use the same names as in the formal syntax.
simple_datum = boolean ^ number ^ string_ ^ symbol

@generate
def list_():
    yield string('(')
    es = yield many(datum << ignore)
    yield string(')')
    return Cons(es)

@generate
def vector():
    yield string('#(')
    es = yield many(datum << ignore)
    yield string(')')
    return Vector(es)

@generate
def abbreviation():
    "Currently only supports quote '"
    yield string("'")
    e = yield datum
    return Cons(Symbol('quote'), Cons(e, emptyList))

compound_datum = list_ | vector | abbreviation
datum = simple_datum ^ compound_datum
datums = many(ignore >> datum << ignore)

###################### Environment

# A dictionary in Python is a mapping from strings
# to values. An environment is a collection of these
# dictionaries. The dictionaries in an environment
# holds only valid Scheme types as values. The keys
# are normal Python strings but correspond to symbols
# in the interpreted Scheme language.

class Environment(Vector):

    def __init__(self, lst=None):
        if lst == None:
            super().__init__([{
                # Control flow primitives
                'eval': eval_,
                'fexpr': fexpr,
                'dyn-lambda': dyn_lambda,
                'define': define,
                'if': if_,
                'equal?': equal,
                 
                # Data structure primitives
                '+': BuiltinFunction(op.add),
                '-': BuiltinFunction(op.sub),
                '*': BuiltinFunction(op.mul),
                '>': BuiltinFunction(op.gt),
                '<': BuiltinFunction(op.lt),
                '>=': BuiltinFunction(op.ge),
                '<=': BuiltinFunction(op.le),
                '=': BuiltinFunction(op.eq),
                'car': BuiltinFunction(lambda x: x.car()),
                'cdr': BuiltinFunction(lambda x: x.cdr()),
                'string-append': BuiltinFunction(op.add),
                'write': BuiltinFunction(print)
            }])
        else: 
            super().__init__(lst)
    
    def value_of_symbol(self, symbol):
        key = symbol.str
        i = len(self) - 1
        while i >= 0:
            val = self[i].get(key)
            if val != None:
                return val
            i -= 1
        
        raise ValueError('Key %s could not be found in environment' % key)
     
    def __str__(self):
        keys = str(list(self[-1].keys()))
        return 'Env{' + str(len(self)) + ', ' + keys + '}'
    
global_env = Environment()

def eval(env, expr):
    res = None

    if isinstance(expr, Symbol):
        if expr.str == '*environment*':
            res = env
        else:
            res = env.value_of_symbol(expr)
    elif not isinstance(expr, Cons): # Constant literal
        res = expr 
    else: # A list
        fst = eval(env, expr[0])
         
        if isinstance(fst, Cons):
            callble = fst.car()
            new_env = fst.cdr()
            stack_trace.append(callble)
            res = fun(new_env, env, expr.cdr())
            stack_trace.pop()
        else:
            stack_trace.append(fst)
            res = fst(env, env, expr.cdr())
            stack_trace.pop()
    
    if(DEBUG and expr != res):
        print("==============================================")
        print('Evaluated: ', expr)
        print('Result: ', res)
        print("==============================================")
    
    return res


stack_trace = []

def print_stack_trace():
    if not DEBUG: return
    print("=================Stack trace==================")
    pprint.PrettyPrinter(indent=4).pprint(stack_trace)
    print("==============================================")

def eval_stack_trace(env, expr):
    try:
        return eval(env, expr)
    except:
        print_stack_trace()
        raise

def eval_string(env, str_):
    return eval_stack_trace(env, datum.parse_strict(str_))

if __name__ == '__main__':
    if len(sys.argv) > 1:
        file = sys.argv[1]
        with open(file, 'r') as fd:
            content = fd.read()
            exprs = datums.parse_strict(content)
            for e in exprs:
                eval_stack_trace(global_env, e)
    else: # REPL
        print('No file supplied, starting REPL')
        while True:
            stack_trace = []
            i = input("> ")
            print("Input: \t\t", i.__repr__())
            expr = datum.parse_strict(i)
            print("Parsed: \t", str(expr))
            print("Evaluated: \t", str(eval_stack_trace(global_env, expr)))

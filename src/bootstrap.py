import re
import sys
import math
import pprint
import operator as op

from parsec import *

DEBUG = False

################### Data types (symbol, function, fexpression)

class Symbol:
    def __init__(self, s):
        self.str = s

    def __eq__(self, other):
        if(isinstance(other, Symbol)):
            return self.str == other.str
        else:
            return False

    def __str__(self):
        return ('#{%s}' % self.str)

    def __repr__(self):
        return self.__str__()

# 'p_env' is the environment in which the
# arguments should be evaluated
class BuiltinFunction:
    def __init__(self, fn):
        self.fn = fn
    def __call__(self, env, p_env, *args):
        return self.fn(*[eval(p_env, a) for a in args]) 

# Dynamically scoped function
class DynFunction:
    def __init__(self, params, body):
        self.params, self.body = params, body
    def __call__(self, env, p_env, *args):
        
        if isinstance(self.params, list):
            dct = dict(zip(self.params, [eval(p_env, a) for a in args]))
            env.append(dct)
        elif isinstance(self.params, Symbol):
            env.append({self.params.str: [eval(p_env, a) for a in args]})
        else:
            raise ValueError('Fexpr: unknown parameter type')
        
        res = eval(env, self.body)
        env.pop()
        return res

# Dynamically scoped F expression
class Fexpr:
    def __init__(self, params, body):
        self.params, self.body = params, body
    def __call__(self, env, _, *args):
        
        if isinstance(self.params, list):
            dct = dict(zip(self.params, args))
            env.append(dct)
        elif isinstance(self.params, Symbol):
            env.append({self.params.str: args})
        else:
            raise ValueError('Fexpr: unknown parameter type')

        res = eval(env, self.body)
        env.pop()
        
        return res

# Special forms
def env_ref(env, p_env, nth):
    return env.get_reference(eval(p_env, nth))

def eval_(env, p_env, expr):
    return eval(env, eval(p_env, expr))

def fexpr(env, _, params, body):
    return Fexpr([p.str for p in params], body)

def dyn_lambda(env, _, params, body):
    return DynFunction([p.str for p in params], body)

def define(env, p_env, var, value):
    env[-1][var.str] = eval(p_env, value)

def if_(env, _, cond, then, else_):
    c = eval(env, cond)
    if c:
        return eval(env, then)
    else:
        return eval(env, else_)


#################### Parsers

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

# Inefficient, maybe optimize later
string_ = (string('"') >> many(escaped_or_char) << string('"')) \
        .parsecmap(lambda lst: ''.join(lst))

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
datums = many(ignore >> datum << ignore)

# With help of http://norvig.com/lispy.html

###################### Environment

# First some terminology:
# A dictionary in Python is a mapping from strings
# to values. An environment is a collection of these
# dictionaries. The dictionaries in an environment
# holds only valid Scheme types as values. The keys
# are normal Python strings but correspond to symbols
# in the interpreted Scheme language.

# We need a collection type which can do the following
# operations very fast:
# - Iterate in reverse over the environment
# - Make a reference to the environment... Note:
#   - the collection of dictionaries should be immutable
#     when pointed by such a reference
#   - the dictionaries itself can still be changed, the
#     reference should expose these changes
# - Add a dictionary to the end of the environment, while
#   making sure the current references satisfy the above constraints.
# - Remove a dictionary at the end of the environment, while
#   making sure the current references satisfy the above constraints.

# The easiest (but probably not the most performant) way
# of implementing this is to use a list as an environment.
# Every time we make a reference we simply copy this list.

class Environment(list):

    def __init__(self, lst=None):
        if lst == None:
            super(Environment, self).__init__([{
                '+': BuiltinFunction(op.add),
                '-': BuiltinFunction(op.sub),
                '*': BuiltinFunction(op.mul),
                '>': BuiltinFunction(op.gt),
                '<': BuiltinFunction(op.lt),
                '>=': BuiltinFunction(op.ge),
                '<=': BuiltinFunction(op.le),
                '=': BuiltinFunction(op.eq),
                'string-append': BuiltinFunction(op.add),
                'write': BuiltinFunction(print),
                'env-ref': env_ref,
                'eval': eval_,
                'fexpr': fexpr,
                'dyn-lambda': dyn_lambda,
                'define': define,
                'if': if_
            }])
        else: 
            super(Environment, self).__init__(lst)
    
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
    
    # Make a reference to the environment
    # chain. The reference starts nth dictionaries
    # back.
    def get_reference(self, nth_float):
        nth = int(nth_float)
        l = len(self)
        if nth < l:
            return Environment(self[0:l-nth])
        else:
            raise IndexError('Cannot make reference to environment: \
                    %d is out of range of environment' % nth)
    
global_env = Environment()

def eval(env, expr):
    res = None

    if isinstance(expr, Symbol):
        res = env.value_of_symbol(expr)
    elif not isinstance(expr, list): # Constant literal
        res = expr 
    elif isinstance(expr[0], Symbol):
        stack_trace.append(expr[0].str)
        res = eval(env, expr[0])(env, env, *expr[1:])
        stack_trace.pop()
    elif isinstance(expr[0], list):
        fn = eval(env, expr[0][0])
        new_env = eval(env, expr[0][1])
        args = [eval(env, a) for a in expr[1:]]
    else:
        call_position = eval(env, expr[0])
        if isinstance(fst, list):
            fun = call_position[0]
            new_env = call_position[1]
            fun(new_env, env, *expr[1:])
        else: # Assume it's a function
            stack_trace.append(fn)
            res = call_position(env, env, *expr[1:])
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
            print("Parsed: \t", expr)
            print("Evaluated: \t", eval_stack_trace(global_env, expr))



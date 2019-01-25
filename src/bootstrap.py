import re
import sys
import math
import pprint
import traceback
import os
import atexit
import readline
import operator as op
import collections.abc as cabc

from watchdog.observers import Observer
from watchdog.events import PatternMatchingEventHandler

from functools import reduce
from parsec import *

DEBUG = False
stack_trace = []

################### Data types

class Symbol:
    def __init__(self, s):
        self.str = s

    def __eq__(self, other):
        if(isinstance(other, Symbol)):
            return self.str == other.str
        else:
            return False
    
    def scheme_repr(self):
        return self.str

class EmptyList:
    def scheme_repr(self):
        return "()"
    
    def __iter__(self):
        return iter(())

emptyList = EmptyList()

class Cons(cabc.Sequence):

    def __init__(self, a, b):
        self.is_list = (isinstance(b, Cons) and b.is_list) or b == emptyList
        self.tup = (a, b)

    def from_iterator(it, return_list=True):
        x = emptyList
        l = list(it)
        i = len(l) - 1
        
        if len(l) <= 1 and not return_list:
            raise ValueError('Cannot return pair with less than two values')
        elif not return_list:
            x = Cons(l[i - 1], l[i])
            i -= 2
        
        while i >= 0:
            x = Cons(l[i], x)
            i = i - 1
        
        return x
    
    # Structure preserving map
    # If 'self' is a pair, a pair will be returned
    def map(self, fun):
        return Cons.from_iterator(self, fun, return_list=self.is_list)
     
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
    
    def scheme_repr(self):
        inner = list(map(scheme_repr, self))
        
        if self.is_list:
            return "(" + " ".join(inner) + ")"
        else:
            return "(" + " ".join(inner[:-1]) + " . " + inner[-1] + ")" 
     
    def __eq__(self, other):
        if isinstance(other, Cons) or isinstance(other, list):
            return list(self) == list(other)
        else:
            return False

# When SchemeCallable or PythonCallable are evaluated,
# they are passed an environment in which
# their 'body' should be evaluated (env) and an environment
# in which their parameters are evaluated (p_env).

class PythonCallable:
    def __init__(self, fn, evaluate):
        self.fn = fn
        self.evaluate = evaluate
        self.name = fn.__name__
    
    def __call__(self, env, p_env, args, evaluate=None):
        global stack_trace
        
        if evaluate == None: evaluate = self.evaluate
        if evaluate: args = eval_all(p_env, args)
         
        stack_trace.append(self.scheme_repr())
        res = self.fn(env, p_env, args)
        stack_trace.pop()
        return res
    
    def scheme_repr(self):
        return "{BultinFunction %s}" % self.name

# Dynamically scoped function
class SchemeCallable:
    def __init__(self, params, body, type):
        self.params, self.body, self.type, self.name = params, body, type, None
    def __call__(self, env, p_env, args, evaluate=None):
         
        if evaluate == None: evaluate = self.type == 'dyn-lambda'
       
        if evaluate:
            args = eval_all(p_env, args)

        if isinstance(self.params, Cons):
            dct = dict(zip(map(lambda sym: sym.str, self.params), args))
        elif isinstance(self.params, Symbol):
            dct = {self.params.str: args}
        elif self.params == emptyList:
            dct = {}
        else:
            raise ValueError('SchemeCallable: unknown parameter type')
         
        stack_trace.append(self.scheme_repr())
        res = eval_all_ret_last(Cons(dct, env), self.body)
        stack_trace.pop()
        return res
     
    def scheme_repr(self):
        if self.name != None:
            return "{%s %s}" % (self.type, self.name)
        else:
            return "{%s}" % (self.type)

def scheme_repr(val):
    ii = isinstance
    normal_fun = callable(val) and not (ii(val, SchemeCallable) or ii(val, PythonCallable))
    
    if ii(val, bool):
        return "#t" if val else "#f"
    elif ii(val, float) or ii(val, int):
        return str(val)
    elif ii(val, list):
        inner = map(scheme_repr, val)
        return "#(" + " ".join(inner) + ")"
    elif ii(val, str):
        return '"' + repr(val)[1:-1] + '"'
    elif ii(val, dict):
        return "{HashTable (%d)}" % len(val)
    elif val == None:
        return 'None'
    else:
        return val.scheme_repr()

#################### Parsers

# ignore cases.
@generate
def comment():
    yield string(';')
    yield many(none_of('\n'))
    # Optional not yet supported
    yield string('\n') ^ string('')

@generate
def block_comment():
    yield string('#|')
    count = 1
    while count > 0:
        p = yield (string('#|') ^ string('|#') ^ none_of(''))
        if p == '#|':
            count += 1
        elif p == '|#':
            count -= 1

ignore = many(block_comment ^ comment ^ space())

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
    yield string('(')
    es = yield many(ignore >> datum << ignore)
    yield string(')')
    return Cons.from_iterator(es)

@generate
def vector():
    yield string('#(')
    es = yield many(datum << ignore)
    yield string(')')
    return es

@generate
def abbreviation():
    prefix_to_sym = {
        "'": 'quote',
        '`': 'quasiquote',
        ',': 'unquote',
        ',@': 'unquote-splicing'
    }
    prefix = yield string("'") | string('`') | (string(',@') ^ string(','))
    e = yield datum
    return Cons(Symbol(prefix_to_sym[prefix]), Cons(e, emptyList))
    
compound_datum = list_ ^ vector ^ abbreviation
datum = simple_datum ^ compound_datum
datums = many(ignore >> datum << ignore)


################### Environment

def define(env, p_env, args):
    val = eval(p_env, args.cdr().car())
    name = args.car().str
    
    if isinstance(val, SchemeCallable) \
            or isinstance(val, PythonCallable):
        val.name = name
    
    env.car()[name] = val

def set(env, p_env, args):
    to_set = eval(p_env, args.cdr().car())
    key = args.car().str
    for ht in env:
        if key in ht:
            ht[key] = to_set
            return to_set
    raise ValueError('Key to set %s is not active in the current environment' % key)

def if_(env, _, args):
    cdr = args.cdr()
    cddr = cdr.cdr()
    if eval(env, args.car()):
        return eval(env, cdr.car())
    elif cddr != emptyList:
        return eval(env, cddr.car())
    else:
        return False

def quasiquote_cons(env, p_env, cons):

    for val in cons:
        if isinstance(val, Cons) and val.car() == Symbol('unquote-splicing'):
            yield from eval(env, val.cdr().car())
        else:
            yield quasiquote(env, p_env, val, is_list = False)

def quasiquote(env, p_env, args, is_list = True):
    a = args.car() if is_list else args

    if isinstance(a, Cons) and a.car() == Symbol('unquote'):
        return eval(env, a.cdr().car())
    elif isinstance(a, Cons):
        return Cons.from_iterator(
                quasiquote_cons(env, p_env, a),
                return_list=a.is_list)
    else:
        return a 

def let(env, p_env, args):
    dict = {}
     
    for (symbol, expr) in args.car():
        dict[symbol.str] = eval(p_env, expr)
    
    return eval_all_ret_last(Cons(dict, env), args.cdr())

def let_star(env, p_env, args):
    dict = {}
    p_env = Cons(dict, p_env)
    for (symbol, expr) in args.car():
        dict[symbol.str] = eval(p_env, expr)

    return eval_all_ret_last(Cons(dict, env), args.cdr())

def map_(env, p_env, args):
    f = args.car()
    lst = args.cdr().car()
    return lst.map(lambda e: f(env, p_env, e))

def write_(*args):
    print(*[a if isinstance(a, str) else scheme_repr(a) for a in args])

def new_global_env():
    PC = PythonCallable
    SC = SchemeCallable
    
    # Simple callable
    s_c = lambda fn: PythonCallable(lambda env, p_env, args: fn(*args), True)
     
    env = Cons({

    # Control flow primitives
    'eval': PC(lambda env, p_env, args: eval(env, args.car()), True),
    'fexpr': PC(lambda env, p_env, args: SC(args.car(), args.cdr(), 'fexpr'), False),
    'dyn-lambda': PC(lambda env, p_env, args: SC(args.car(), args.cdr(), 'dyn-lambda'), False),
    'define': PC(define, False),
    'set!': PC(set, False),
    'if': PC(if_, False),
    'equal?': s_c(op.eq),
    'exit': s_c(exit),
    
    'quasiquote': PC(quasiquote, False),
    'quote': PC(lambda env, p_env, args: args.car(), False),
    'apply': PC(lambda env, p_env, args: args.car()(env, p_env, args[1], evaluate=False), True),

    'let': PC(let, False),
    'let*': PC(let_star, False),
     
    # Data structure primitives

    # Number
    '+': s_c(lambda *args: reduce(op.add, args, 0)),
    '-': s_c(op.sub),
    '*': s_c(lambda *args: reduce(op.mul, args, 1)),
    '>': s_c(op.gt),
    '<': s_c(op.lt),
    '>=': s_c(op.ge),
    '<=': s_c(op.le),
    '=': s_c(op.eq),
    
    # Boolean
    'not': s_c(lambda b: not b),
    'boolean?': s_c(lambda b: isinstance(b, bool)),
    'and': s_c(lambda *b: reduce(op.and_, b, True)),
    'or': s_c(lambda *b: reduce(op.or_, b, False)),
    
    # Hash table
    'make-hash-table': s_c(lambda: {}),
    'hash-table?': s_c(lambda h: isinstance(h, dict)),
    'hash-table-keys': s_c(lambda h: Cons.from_iterator(h.keys())),
    'hash-table-values': s_c(lambda h: Cons.from_iterator(h.values())),
    'hash-table-ref': s_c(lambda h, k: h[k.str]),
    'hash-table-exists?': s_c(lambda h, k: k.str in h),
    'hash-table-set!': s_c(lambda h, k, v: h.__setitem__(k.str, v)),
    
    # List
    'pair?': s_c(lambda c: isinstance(c, Cons)),
    'cons': s_c(lambda a, b: Cons(a, b)),
    'car': s_c(lambda x: x.car()),
    'cdr': s_c(lambda x: x.cdr()),
    'null?': s_c(lambda l: l == emptyList),
    'list': s_c(lambda *elms: Cons.from_iterator(elms)),
    'length': s_c(lambda l: len(l)),
    'list->vector': s_c(lambda l: list(l)),
    'map': PC(map_, True),


    # Vector
    'vector?': s_c(lambda x: isinstance(x, list)),
    'make-vector': s_c(lambda x=None: []),
    'vector': s_c(lambda *it: list(it)),
    'vector-length': s_c(lambda v: len(v)),
    'vector-ref': s_c(lambda v, k: v[int(k)]),
    'vector-set!': s_c(lambda v, k, val: v.__setitem__(int(k), val)),
    'vector->list': s_c(lambda v: Cons.from_iterator(v)),
    'vector-append': s_c(lambda v, *args: v + list(args)),
    
    # String
    'string?': s_c(lambda s: isinstance(s, str)),
    'string-length': s_c(lambda s: len(s)),
    'string-ref': s_c(lambda s, k: s[int(k)]),
    'string-append': s_c(lambda *s: reduce(op.add, s, '')),
    'string-join': s_c(lambda sep, s: sep.join(s)),
    'string->symbol': s_c(lambda s: Symbol(s)),
    
    # Symbol
    'symbol->string': s_c(lambda sym: sym.str),
     
    'write': s_c(write_)
    }, emptyList)

    for key, val in env.car().items():
        val.name = key

    return env

def value_of_symbol(env, sym):
    while True:
        if env == emptyList:
            raise ValueError('Key %s could not be found in environment' % sym.str)
        else:
            dct = env.car()
            val = dct.get(sym.str)
            if val != None: return val
            env = env.cdr()

# Eval all, return new Cons
def eval_all(env, it):
    return Cons.from_iterator(map(lambda e: eval(env, e), it))

# Eval all, return 
def eval_all_ret_last(env, it):
    return reduce(lambda _, e: eval(env, e), it, None)

def eval(env, expr):
    res = None

    if isinstance(expr, Symbol):
        if expr.str == '*environment*':
            return env
        else:
            return value_of_symbol(env, expr)
    elif not isinstance(expr, Cons): # Constant literal
        return expr 
    else: # It's a list
        fst = eval(env, expr.car())
        
          
        if isinstance(fst, Cons):
            callble = fst.car()
            new_env = fst.cdr()
            return callble(new_env, env, expr.cdr())
        
        return fst(env, env, expr.cdr())

def eval_string(str_):
    return eval(new_global_env(), datum.parse_strict(str_))

def read_execute_file(env, file_name):
    global stack_trace
    try:
        with open(file_name, 'r') as fd:
            content = fd.read()
            exprs = datums.parse_strict(content)
            for e in exprs:
                eval(env, e)
    except Exception as e:
        print('EXCEPTION: \t', e)
        print('SCHEME TRACE: ', stack_trace)
        stack_trace = []
        raise

def setup_repl_history():
    try:
        histfile = os.path.join(os.path.expanduser('~'), '.pyka_history')
        readline.read_history_file(histfile)
        readline.set_history_length(1000)
    except IOError as e:
        print('Failed to read history file: ', e)
        pass
    
    atexit.register(readline.write_history_file, histfile)

# Read-Eval-Print
def rep(env):
    global stack_trace
    
    try:
        i = input("> ")
        print("Input: \t\t", i.__repr__())
        expr = datum.parse_strict(i)
        print("Parsed: \t", scheme_repr(expr))
        print("Evaluated: \t", scheme_repr(eval(env, expr)))
    except Exception as e:
        print('\nEXCEPTION: \t', e)
        print('SCHEME TRACE: \t', stack_trace)
        stack_trace = []
        last = traceback.format_tb(e.__traceback__)[-1]
        print('\n', last)
        env.car()['*trace*'] = traceback.format_exc()
        print('Use (write *trace*) to see entire Python stack trace')

def on_file_modify(file_name, callback):
    obs = Observer()
    handler = PatternMatchingEventHandler([file_name])
    handler.on_modified = lambda e: callback(e)
    obs.schedule(handler, os.path.dirname(file_name))
    obs.start()
 
if __name__ == '__main__':
    
    env = new_global_env()
    reload = False
    
    def reset():
        global env
        print('\nInput file has been changed. Reloading.')
        env = new_global_env()
        read_execute_file(env, sys.argv[1])
    
    if len(sys.argv) > 1:
         
        read_execute_file(env, sys.argv[1])
         
        def modified(e):
            global reload
            
            current_input = readline.get_line_buffer()
            if current_input == '':
                reset()
                print('> ', end='', flush=True)
            else:
                reload = True
         
        on_file_modify(sys.argv[1], modified)
     
    setup_repl_history() 
    
    while True:
        if reload:
            reset()
            reload = False
        rep(env)


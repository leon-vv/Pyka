import re
import sys
import math
import pprint
import traceback
import os
from io import TextIOWrapper
import atexit
import readline
import operator as op
import collections.abc as cabc

from watchdog.observers import Observer
from watchdog.events import PatternMatchingEventHandler

from functools import reduce
from parsec import *

# Todo: look into converting Python tail calls to
# iterations
sys.setrecursionlimit(2500)

################### Global variables

current_input_port = sys.stdin
current_error_port = sys.stderr
current_output_port = sys.stdout

################### Data types

class Symbol:
    def __init__(self, s, line=None):
        self.str = s
        if line != None: self.line = line
    
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
    def __len__(self):
        return 0
    def __iter__(self):
        return iter(())

emptyList = EmptyList()

class Cons(cabc.Sequence):

    def __init__(self, a, b, line=None):
        self.is_list = (isinstance(b, Cons) and b.is_list) or b == emptyList
        self.tup = (a, b)
        if line != None: self.line = None

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

    def __call__(self, env, p_env, args, evaluate=None):
        return self.car()(self.cdr(), p_env, args, evaluate) 
     
    # Structure preserving map
    # If 'self' is a pair, a pair will be returned
    def map(self, fun):
        return Cons.from_iterator(map(fun, self),
                return_list=self.is_list)
     
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
        if evaluate == None: evaluate = self.evaluate
        if evaluate: args = eval_all(env, args)
              
        return self.fn(env, args)
    
    def scheme_repr(self):
        return "{BultinFunction %s}" % self.name

# Dynamically scoped function
class SchemeCallable:
    def __init__(self, params, body, type):
        self.params, self.body, self.type, self.name = params, body, type, None
    def __call__(self, env, p_env, args, evaluate=None):
         
        if evaluate == None: evaluate = self.type == 'd-fun'
       
        if evaluate: args = eval_all(env, args)

        if isinstance(self.params, Cons):
            dct = {}
            args_pointer = args
            parms_pointer = self.params
            
            while isinstance(parms_pointer, Cons):
                if not isinstance(args_pointer, Cons):
                    raise ValueError('Not enough arguments supplied to SchemeCallable ' + self.name)

                dct[parms_pointer.car().str] = args_pointer.car()
                parms_pointer = parms_pointer.cdr()
                args_pointer = args_pointer.cdr()
             
            if parms_pointer != emptyList:
                dct[parms_pointer.str] = args_pointer
        
        elif isinstance(self.params, Symbol):
            dct = {self.params.str: args}
        elif self.params == emptyList:
            dct = {}
        else:
            raise ValueError('SchemeCallable: unknown parameter type')
         
        return eval_all_ret_last(Cons(dct, env), self.body)
     
    def scheme_repr(self):
        if self.name != None:
            return "{%s %s}" % (self.type, self.name)
        else:
            return "{%s}" % (self.type)

def scheme_repr(val):
    ii = isinstance
    
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
        if len(val) > 3:
          return "{HashTable (%d)}" % len(val)
        else:
          return str(val)
    elif val == None:
        return 'None'
    else:
        return val.scheme_repr()

#################### Parsers

@generate
def comment():
    yield string(';')
    yield many(none_of('\n'))
    # Optional not yet supported
    yield string('\n') ^ string('')

@generate
def block_comment(): # Properly nested (hence, 'count')
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

# See RSR7 small language specification
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

dot_subsequent = sign_subsequent | string('.')

@generate
def peculiar_identifier_dot():
    d = yield string('.')
    ds = yield dot_subsequent
    subs = yield many(subsequent)
    return d + ds + ''.join(subs)

@generate
def symbol():
    s = yield mark((one_of('+-') ^
        normal_identifier ^
        peculiar_identifier ^
        peculiar_identifier_dot))
    return Symbol(s[1], s[0][0])

escaped_or_char = escaped_char ^ none_of('"')

# Inefficient, maybe optimize later
string_ = (string('"') >> many(escaped_or_char) << string('"'))\
    .parsecmap(lambda s: ''.join(s))

boolean = (string('#true') ^ string('#t')).result(True) ^ \
            (string('#false') ^ string('#f')).result(False)

# Please see the specification of RSR7, chapter 7.
# I try to use the same names as in the formal syntax.
simple_datum = boolean ^ number ^ string_ ^ symbol

@generate
def list_():
    res = yield mark(string('('))
    line = res[0][0]
     
    datum_ignore = ignore >> datum << ignore
    es = yield many(datum_ignore)
    point_or_paren = yield (string(')') | string('.'))
    if point_or_paren == '.':
        last = yield datum_ignore
        yield string(')')
        es.append(last)
        c = Cons.from_iterator(es, return_list=False)
    else:
        c = Cons.from_iterator(es)

    c.line = line
    return c

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

# Print debug
def printd(val):
    print(scheme_repr(val))

def define(env, args):
    val = eval(env, args.cdr().car())
    name = args.car().str
    
    if isinstance(val, SchemeCallable) \
            or isinstance(val, PythonCallable):
        val.name = name # Better stack trace
     
    env.car()[name] = val

def undefine(env, args):
  name = args.car().str
  env.car().pop(name, None)

def set(env, args):
    to_set = eval(env, args.cdr().car())
    key = args.car().str
    for ht in env:
        if key in ht:
            ht[key] = to_set
            return to_set
    raise ValueError('Symbol to set %s is not active in the current environment' % key)

def if_(env, args):
    cdr = args.cdr()
    cddr = cdr.cdr()
    if eval(env, args.car()):
        return eval(env, cdr.car())
    elif cddr != emptyList:
        return eval(env, cddr.car())
    else:
        return False

def do(env, args):
    dct = {}
    
    for b in args.car():
        dct[b.car().str] = eval(env, b.cdr().car()) # init
    
    test = args.cdr().car()
    
    while not eval(Cons(dct, env), test.car()):
        for c in args.cdr().cdr():
            eval(Cons(dct, env), c)
         
        vals = []
        
        # Make step
        for b in args.car():
            if b.cdr().cdr() != emptyList:
                vals.append((b.car().str,
                             eval(Cons(dct, env), b[2])))
        
        # Update binding
        for name,val in vals: dct[name] = val
    
    return reduce(lambda _, e: eval(Cons(dct, env), e), test.cdr(), False)

def let(env, args):
    dict = {}
    
    for (symbol, expr) in args.car():
        dict[symbol.str] = eval(env, expr)
     
    return eval_all_ret_last(Cons(dict, env), args.cdr())

def let_star(env, args):
    dict = {}
    new_env = Cons(dict, env)
    for (symbol, expr) in args.car():
        dict[symbol.str] = eval(new_env, expr)

    return eval_all_ret_last(Cons(dict, env), args.cdr())

def map_(env, args):
    f = args.car()
    lst = args.cdr().car()
    if lst == emptyList: return emptyList
    
    return lst.map(lambda e: f(env, env, Cons(e, emptyList), evaluate=False))

def string_to_number(env, args):
  try:
    return float(args.car())
  except ValueError:
    return False

def call_with_port(env, port, proc):
    proc(env, Cons(port, emptyList))
    port.close()

def call_with_input_file(env, file_name, proc):
    proc = open(file_name, 'r')
    call_with_port(env, port, proc)

def call_with_output_file(env, file_name, proc):
    proc = open(file_name, 'w')
    call_with_port(env, port, proc)

def with_input_from_file(env, file_name, proc):
    global current_input_port
    tmp = current_input_port
    current_input_port = open(file_name, 'r')
    proc(env, p_env)
    current_input_port.close()
    current_input_port = tmp

def with_output_to_file(env, file_name, proc):
    global current_input_port
    tmp = current_input_port
    current_input_port = open(file_name, 'r')
    proc(env, p_env)
    current_input_port.close()
    current_input_port = tmp

def read_all(port=current_input_port):
    content = port.read()
    return Cons.from_iterator(datums.parse_strict(content))

def load(env, args):
    file_name = args.car()
    if args.cdr() != emptyList:
      env = args.cdr().car()
    read_execute_file(env, file_name)

def and_(env, args):
    if args == emptyList: return True
    first = eval(env, args.car())
    if first:
        return and_(env, args.cdr())
    else:
        return False

def or_(env, args):
    if args == emptyList: return False
    first = eval(env, args.car())
    if first:
        return True
    else:
        return or_(env, args.cdr())

# Simple implementation of continuations using Exceptions
# Note: this implementation does NOT support continuations
# that escape the call/cc function

class ResumeFromContinuation(Exception):
    def __init__(self, val, id_):
        self.val = val
        self.id = id_

continuation_id = 0

def call_cc(env, args):

    global continuation_id
    id_ = continuation_id
    continuation_id += 1
     
    def raise_(env, args=emptyList):
        val = args.car() if args != emptyList else False
        raise ResumeFromContinuation(val, id_)
      
    continuation = PythonCallable(raise_, True)
        
    try:
        return args.car()(env, Cons(continuation, emptyList))
    except ResumeFromContinuation as r:
        if r == id_:
            return r.val
        else:
            raise

def new_global_env():
    PC = PythonCallable
    SC = SchemeCallable
    
    # Simple callable
    fn = lambda fn: PythonCallable(lambda env, args: fn(*args), True)
     
    env = Cons({

    # Control flow primitives
    'eval': fn(lambda expr, env: eval(env, expr)),
    'd-fexpr': PC(lambda env, args: SC(args.car(), args.cdr(), 'd-fexpr'), False),
    'd-fun': PC(lambda env, args: SC(args.car(), args.cdr(), 'd-fun'), False),
    'define': PC(define, False),
    'undefine': PC(undefine, False),
    'if': PC(if_, False),
    'equal?': fn(op.eq),
    'exit': fn(exit),
    'call-with-current-continuation': PC(call_cc, True),
    'call/cc': PC(call_cc, True),
    'get-env': PC(lambda env, args: env, True),
    
    'do': PC(do, False),
    'apply': PC(lambda env, args: args.car()(env, env, args[1], evaluate=False), True),

    'let': PC(let, False),
    'let*': PC(let_star, False),

    'load': PC(load, True),
     
    # Data structure primitives

    # Procedures
    'procedure?': fn(lambda p: isinstance(p, PC) or isinstance(p, SC)),
    
    # Number
    'number?': fn(lambda n: isinstance(n, float)),
    '+': fn(lambda *args: reduce(op.add, args, 0)),
    '-': fn(op.sub),
    '*': fn(lambda *args: reduce(op.mul, args, 1)),
    '>': fn(op.gt),
    '<': fn(op.lt),
    '>=': fn(op.ge),
    '<=': fn(op.le),
    '=': fn(op.eq),
    
    # Boolean
    'not': fn(lambda b: not b),
    'boolean?': fn(lambda b: isinstance(b, bool)),
    'and': PC(and_, False),
    'or': PC(or_, False),
    
    # Hash table
    'make-hash-table': fn(lambda: {}),
    'hash-table?': fn(lambda h: isinstance(h, dict)),
    'hash-table-keys': fn(lambda h: Cons.from_iterator(h.keys())),
    'hash-table-values': fn(lambda h: Cons.from_iterator(h.values())),
    'hash-table-ref': fn(lambda h, k: h[k.str]),
    'hash-table-exists?': fn(lambda h, k: k.str in h),
    'hash-table-set!': fn(lambda h, k, v: h.__setitem__(k.str, v)),
    'hash-table-copy': fn(lambda h: dict(h)),
    
    # List
    'pair?': fn(lambda c: isinstance(c, Cons)),
    'list?': fn(lambda c: isinstance(c, Cons) and c.is_list),
    'cons': fn(lambda a, b: Cons(a, b)),
    'car': fn(lambda x: x.car()),
    'cdr': fn(lambda x: x.cdr()),
    'null?': fn(lambda l: l == emptyList),
    'list': fn(lambda *elms: Cons.from_iterator(elms)),
    'length': fn(lambda l: len(l)),
    'list->vector': fn(lambda l: list(l)),
    'map': PC(map_, True),
    
    # Vector
    'vector?': fn(lambda x: isinstance(x, list)),
    'make-vector': fn(lambda k=0,fill=0: [fill]*int(k)),
    'vector': fn(lambda *it: list(it)),
    'vector-length': fn(lambda v: len(v)),
    'vector-ref': fn(lambda v, k: v[int(k)]),
    'vector-set!': fn(lambda v, k, val: v.__setitem__(int(k), val)),
    'vector->list': fn(lambda v: Cons.from_iterator(v)),
    'vector-append': fn(lambda v, *args: v + list(args)),
    
    # String
    'string?': fn(lambda s: isinstance(s, str)),
    'make-string': fn(lambda k, char='~': char*int(k)),
    'string-length': fn(lambda s: len(s)),
    'string-ref': fn(lambda s, k: s[int(k)]),
    'string-append': fn(lambda *s: reduce(op.add, s, '')),
    'string-join': fn(lambda sep, s: sep.join(s)),
    'string->symbol': fn(lambda s: Symbol(s)),
    'any->string': fn(lambda v: scheme_repr(v)),
    'substring': fn(lambda s, start=0, end=None: s[int(start):None if end == None else int(end)]),
    'string->number': PC(string_to_number, True),
     
    # Symbol
    'symbol?': fn(lambda s: isinstance(s, Symbol)),
    'symbol->string': fn(lambda sym: sym.str),
     
    # Port
    'call-with-port': PC(call_with_port, True),
    'call-with-input-file': PC(call_with_input_file, True),
    'call-with-output-file': PC(call_with_output_file, True),
    'with-input-from-file': PC(with_input_from_file, True),
    'with-output-to-file': PC(with_output_to_file, True),
    'read-all': fn(read_all),

    'input-port?': fn(lambda p: isinstance(p, TextIOWrapper) and p.readable()),
    'output-port?': fn(lambda p: isinstance(p, TextIOWrapper) and p.writable()),
    'port?': fn(lambda p: isinstance(p, io.FileIO)),
    'input-port-open?': fn(lambda p: not p.closed),
    'output-port-open?': fn(lambda p: not p.closed),
    'current-input-port': fn(lambda: current_input_port),
    'current-output-port': fn(lambda: current_output_port),
    'current-error-port': fn(lambda: current_error_port),
    'open-input-file': fn(lambda name: open(name, 'r')),
    'open-output-file': fn(lambda name: open(name, 'w')),
    'close-port': fn(lambda p: p.close()),
     
    'write': fn(lambda val, port=current_output_port: port.write(scheme_repr(val))),
    'newline': fn(lambda port=current_output_port: port.write('\n')),
    'write-string': fn(lambda s, port=current_output_port: port.write(s))

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
            if sym.str in dct:
              val = dct.get(sym.str)
              if val != None: return val
              else: raise ValueError('Key %s is equal to None in environment' % sym.str)
            env = env.cdr()

def eval_all(env, it):
    return Cons.from_iterator(map(lambda e: eval(env, e), it))

def eval_all_ret_last(env, it):
    return reduce(lambda _, e: eval(env, e), it, None)

eval_stack = []

def eval(env, expr):
    eval_stack.append(expr)
    res = None 
    
    if isinstance(expr, Symbol):
        res = value_of_symbol(env, expr)
    elif not isinstance(expr, Cons): # Constant literal
        res = expr 
    else: # It's a list
        res = eval(env, expr.car())(env, env, expr.cdr())
    
    eval_stack.pop()
    return res

def eval_string(str_):
    return eval(new_global_env(), datum.parse_strict(str_))

def print_eval_stack():
    global eval_stack

    for expr in [eval_stack[0]] + eval_stack[-5:]:
        print('EVALUATING: \t', scheme_repr(expr))
        if hasattr(expr, 'line'):
            print('PARSED AT: \t line ' + str(expr.line) + '\n')
  
    eval_stack = []

def handle_exception(env, e):
    print('\nEXCEPTION: \t', e)
        
    print_eval_stack()
      
    last = traceback.format_tb(e.__traceback__)[-1]
    print('\n', last)
    env.car()['*trace*'] = traceback.format_exc()
    print('Use (write-string *trace*) to see entire Python stack trace')

def read_execute_file(env, file_name):
    try:
        with open(file_name, 'r') as fd:
            content = fd.read()
            exprs = datums.parse_strict(content)
            for e in exprs:
                eval(env, e)
    except Exception as e:
        handle_exception(env, e)

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
    try:
        i = input("> ")
        print("Input: \t\t", i.__repr__())
        expr = datum.parse_strict(i)
        print("Parsed: \t", scheme_repr(expr))
        print("Evaluated: \t", scheme_repr(eval(env, expr)))
    except Exception as e:
        handle_exception(env, e)

def on_file_modify(file_name, callback):
    obs = Observer()
    handler = PatternMatchingEventHandler([file_name])
    #handler.on_modified = lambda e: callback(e)
    obs.schedule(handler, os.path.dirname(file_name))
    obs.start()
 
if __name__ == '__main__':
    
    env = new_global_env()
    reload = False
    
    def reset():
        global env
        print('\nInput file changed. Reloading.')
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


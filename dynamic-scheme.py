from typing import cast, TypeVar, List, Tuple, Callable, Union, Any

# A parser is a function which:
# Accepts a string and a starting position.
# Returns a list of two tuples, the first element
# is the result of the parse operation. The second
# element is the new position in the string.

T = TypeVar('T')
J = TypeVar('J')
ParserResult = List[Tuple[T, int]]
Parser = Callable[[str, int], ParserResult[T]]


def valid_float_char(c: str) -> bool:
    return '0' <= c <= '9' or c in "+-e"

def any_parser(*parsers: Parser[T]) -> Parser[T]:

    def parser(s: str, pos: int):
        res = []
        for p in parsers:
            res.extend(p(s, pos))
        return res
    return parser

# ValueError exceptions are skipped
def map_parser_exception(f: Callable[[T], J], p: Parser[T]) -> Parser[J]:

    def parser(s: str, pos: int):
        new_result = []
        
        for r in p(s, pos):
            try:
               mapped = f(r[0])
               new_result.append( (mapped, r[1]) )
            except ValueError:
                pass
        
        return new_result
        
    return parser

# ValueError exceptions bubble up, unlinke map_parser_exception
def map_parser(f: Callable[[T], J], p: Parser[T]) -> Parser[J]:

    def parser(s: str, pos: int):
        return list(map(lambda r: (f(r[0]), r[1]), p(s, pos)))
    
    return parser

# The given parser should consume all input
def complete_parser(p: Parser[T]) -> Parser[T]:

    def parser(s: str, pos: int):
        res = p(s, pos)
        return list(filter(lambda r: r[1] == len(s), res))
    return parser

def filter_char_parser(f: Callable[[str], bool]) -> Parser[str]:

    def parser(s: str, pos: int):
        i = pos
        while i < len(s) and f(s[i]): i += 1
        
        if(i > pos): return [ (s[pos:i], i) ]
        else: return []
    return parser

# 'convert' is allowed to throw ValueError
def parser_from_characters(f: Callable[[str], bool], 
                           convert: Callable[[str], T]) -> Parser[T]:
    
    return map_parser_exception(lambda s: convert(s), filter_char_parser(f))

def pattern_parser(pattern: str) -> Parser[str]:

    def parser(s: str, p: int):
        if (s[p:]).startswith(pattern):
            return [(pattern, p + len(pattern))]
        else:
            return []
    return parser

# Parser any char except the given one
def except_parser(char: str) -> Parser[str]:
    def parser(s: str, pos: int):
        if pos < len(s) and s[pos] == char: return []
        elif pos < len(s): return [(s[pos], pos + 1)]
    return parser

def chain_parsers(*parsers: Parser[str]) -> Parser[str]:

    def parser(s: str, pos: int):

        res = parsers[0](s, pos)
        
        for p in parsers[1:]:
            new_res: List[Tuple[str, int]] = []

            for r in res:
                p = map_parser(
                        lambda s: r[0] + s,
                        p)
                
                new_res.extend(p(s, r[1]))
            
            res = new_res
         
        return res

    return parser


def prepend(s: str, r: ParserResult[str]) -> ParserResult[str]:
    return list(map(lambda r: (s + r[0], r[1]), r))

def repeat_parser(p: Parser[str]) -> Parser[str]:
    
    def parser(s: str, pos: int):
        res = p(s, pos)
        
        while True:
            new_res = []
            
            for r in res:
                to_extend = p(s, r[1])
                to_extend = prepend(r[0], to_extend)
                new_res.extend(to_extend)
        
            if len(new_res): res = new_res
            else: break
        
        return res
    
    return parser

    
# Try the first parser, if it fails, try the second
def if_else_parser(p1: Parser[T], p2: Parser[T]) -> Parser[T]:

    def parser(s: str, pos: int):
        res = p1(s, pos)
        if len(res): return res
        else: return p2(s, pos)

    return parser

def set_value(p: Parser[T], v: J) -> Parser[J]:
    return map_parser(lambda r: v, p)

def whatever_parser(s: str, pos: int) -> ParserResult[str]:
    if(pos < len(s)): return [(s[pos], pos + 1)]
    else: return []

def debug_parser(p: Parser[T]) -> Parser[T]:

    def parser(s: str, pos: int):
        r = p(s, pos)
        print("\n\n")
        print("Input: ", pos, " ", s)
        print("------------------")
        print("Output: ")
        print(r)
        return r
    
    return parser

escape_control_parser = any_parser(
    set_value(pattern_parser("\\a"), "\a"),
    set_value(pattern_parser("\\b"), "\b"),
    set_value(pattern_parser("\\t"), "\t"),
    set_value(pattern_parser("\\n"), "\n"),
    set_value(pattern_parser("\\r"), "\r"),
    set_value(pattern_parser("\\\""), "\""),
    set_value(pattern_parser("\\\\"), "\\"),
    set_value(pattern_parser("\|"), "|"))

# Recursive types not yet supported
Ast = Union[int, float, str, List[Any]]

string_parser: Parser[Ast] = cast(Parser[Ast],
    chain_parsers(
        pattern_parser("\""),
        repeat_parser(
            if_else_parser(escape_control_parser,
                           except_parser("\""))),
        pattern_parser("\"")))

float_parser: Parser[Ast] = parser_from_characters(valid_float_char, float)
int_parser: Parser[Ast] = parser_from_characters(lambda c: '0' <= c <= '9', int)


def eval(e: str) -> ParserResult[Ast]:
    parser: Parser[Ast] = complete_parser(
            any_parser(float_parser, int_parser, string_parser))
    return parser(e, 0)


while True:
    print(eval(input("> ")))






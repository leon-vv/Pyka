import unittest
import sys

sys.path.append('./src/')

from bootstrap import *

class SchemeTest(unittest.TestCase):

    def parser_test(self, parser, str_, val):
        self.assertEqual(parser.parse_strict(str_), val)

    def test_boolean(self):
        self.parser_test(boolean, '#t', True)
        self.parser_test(boolean, '#true', True)
        self.parser_test(boolean, '#f', False)
        self.parser_test(boolean, '#false', False)

    def test_number(self):
        self.parser_test(number, '+5e3', 5000)
        self.parser_test(number, '50', 50)

    def test_string(self):
        self.parser_test(string_, '"Abc"', 'Abc')

    def test_symbol(self):
        self.parser_test(symbol, 'abc', Symbol('abc'))

    def test_comment(self):
        self.parser_test(comment, ';  abc \n', None)
    
    def test_quote(self):
        self.parser_test(abbreviation,
                "'abc",
                [Symbol('quote'), Symbol('abc')])
        self.parser_test(abbreviation,
                "'(some \"list\")",
                [Symbol('quote'), [Symbol('some'), 'list']])
    
    def str_test(self, data, s):
        self.assertEqual(str(data), s)
    
    def test_str(self):
        self.str_test(Symbol('abc'), 'abc')
        self.str_test(String('abc'), "'abc'")
        self.str_test(emptyList, '()')
        self.str_test(Cons(1, 2), '(1 . 2)')
        self.str_test(Cons(1, emptyList), '(1)')
        self.str_test(Cons(1, Cons(2, emptyList)), '(1 2)')
        self.str_test(Vector([1,2]), '#(1 2)')
    
    def test_eval_all(self):
       self.assertEqual(
               eval_all(global_env,
                   datum.parse_strict('((+ 10 20) (+ 20 30))')),
               Cons(30.0, Cons(50.0, emptyList)))

    def eval_test(self, code, result):
        self.assertEqual(eval_string(global_env, code), result)

    def test_plus(self):
        self.eval_test('(+ 10 20)', 30)

    def test_dyn_lambda(self):
        self.eval_test('((dyn-lambda (x) (+ x x)) 10)', 20)

    def test_vararg(self):
        self.eval_test('((dyn-lambda x x) 1 2)', [1, 2])
 
if __name__ == '__main__':
    unittest.main()



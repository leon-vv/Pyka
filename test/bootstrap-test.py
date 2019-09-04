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
    
    def repr_test(self, data, s):
        self.assertEqual(scheme_repr(data), s)
    
    def test_str(self):
        self.repr_test(Symbol('abc'), 'abc')
        self.repr_test('abc', '"abc"')
        self.repr_test(emptyList, '()')
        self.repr_test(Cons(1, 2), '(1 . 2)')
        self.repr_test(Cons(1, emptyList), '(1)')
        self.repr_test(Cons(1, Cons(2, emptyList)), '(1 2)')
        self.repr_test([1,2], '#(1 2)')
    
    def test_eval_all(self):
       self.assertEqual(
               eval_all(new_global_env(),
                   datum.parse_strict('((+ 10 20) (+ 20 30))')),
               Cons(30.0, Cons(50.0, emptyList)))

    def eval_test(self, code, result):
        self.assertEqual(eval_string(code), result)

    def test_plus(self):
        self.eval_test('(+ 10 20)', 30)

    def test_dyn_lambda(self):
        self.eval_test('((d-fun (x) (+ x x)) 10)', 20)

    def test_vararg(self):
        self.eval_test('((d-fun x x) 1 2)', [1, 2])

    def test_apply(self):
        self.eval_test("(apply + (list 1 2 3))", 6)
        self.eval_test("(apply + (list))", 0)
        self.eval_test("(apply length (list (list 1 2 3)))", 3)

    def test_map(self):
        self.eval_test('(map (d-fun (x) (+ x 1)) (list 1 2 3))',
                Cons.from_iterator([2,3,4]))
        self.eval_test('(map length (list (list 1 2) (list 3) (list)))',
                Cons.from_iterator([2, 1, 0]))

    def test_or_and(self):
        self.eval_test('(and)', True)
        self.eval_test('(or)', False)
        self.eval_test('(and #f a)', False) # a not evaluated
        self.eval_test('(or #t a)', True)
     
    def test_curry(self):
        self.eval_test('(((curry curry (d-fun x x) 1 2) 3 4) 5 6)',
            Cons.from_iterator([1, 2, 3, 4, 5, 6])) 

    def test_list_tail(self):
        self.eval_test('(list-tail (list 1 2 3 4 5) 2)',
            Cons.from_iterator([3, 4, 5]))

    def test_from_iterator(self):
        self.assertEqual(Cons.from_iterator([1,2]),
                Cons(1, Cons(2, emptyList)))        

        self.assertEqual(Cons.from_iterator([]), emptyList)
        self.assertEqual(Cons.from_iterator([1]), Cons(1, emptyList))

        self.assertEqual(Cons.from_iterator([1,2,3], return_list=False),
                Cons(1, Cons(2, 3)))
        self.assertEqual(Cons.from_iterator([2, 10], return_list=False),
                Cons(2, 10))
 
if __name__ == '__main__':
    unittest.main()



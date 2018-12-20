import unittest
import sys

sys.path.append('./src/')

from dyn_scheme import *


class ParserTest(unittest.TestCase):

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

if __name__ == '__main__':
    unittest.main()



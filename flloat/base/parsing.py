from abc import ABC

import sys
from ply import lex
import ply.yacc as yacc

import os


class Lexer(ABC):

    @property
    def tokens(self):
        raise NotImplementedError

    # Define a rule so we can track line numbers
    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    # A string containing ignored characters (spaces and tabs)
    t_ignore = ' \t'

    # Error handling rule
    def t_error(self, t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    # Build the lexer
    def build(self, **kwargs):
        self.lexer = lex.lex(module=self, **kwargs)


class Parser(ABC):

    def __init__(self, name, tokens, lexer, precedence=None):
        self.name = name
        self.tokens = tokens
        if precedence:
            self.precedence = precedence
        self.lexer = lexer
        self.lexer.build()

        # Build the parser
        # outputdir = "flloat/base/tables/%s" % name
        self.parser = yacc.yacc(module=self)#, outputdir=outputdir)

    def __call__(self, s, **kwargs):
        return self.parser.parse(s, lexer=self.lexer.lexer)

    # Error rule for syntax errors
    def p_error(self, p):
        raise ValueError("Syntax error in input! %s" %str(p))

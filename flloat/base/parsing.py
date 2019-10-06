# -*- coding: utf-8 -*-
"""This module contains the definition of an abstract parser module, build with PLY."""

from abc import ABC, abstractmethod

import ply.yacc as yacc
from ply import lex


class Lexer(ABC):
    """This class defines an abstract lexer."""

    @property
    @abstractmethod
    def tokens(self):
        """Return the tokens."""

    def t_newline(self, t):
        """Define a rule so we can track line numbers."""
        r"\n+"
        t.lexer.lineno += len(t.value)

    # A string containing ignored characters (spaces and tabs)
    t_ignore = " \t"

    def t_error(self, t):
        """Handle the lexer error."""
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    def build(self, **kwargs):
        """Build the lexer."""
        self.lexer = lex.lex(module=self, **kwargs)


class Parser(ABC):
    """This class implements an abstract parser."""

    def __init__(self, name, tokens, lexer, precedence=None):
        """
        Initialize a parser.

        :param name: the name of the parser.
        :param tokens: the tokens of the parser.
        :param lexer: the lexer of the parser.
        :param precedence: the precedence rules.
        """
        self.name = name
        self.tokens = tokens
        if precedence:
            self.precedence = precedence
        self.lexer = lexer
        self.lexer.build()

        # Build the parser
        # outputdir = "flloat/base/tables/%s" % name
        self.parser = yacc.yacc(module=self)  # , outputdir=outputdir)

    def __call__(self, s, **kwargs):
        """
        Execute the parsing of a string.

        :param s: the string
        :param kwargs: optional arguments (depends on concrete classes.
        :return: the parsed logical formula.
        """
        return self.parser.parse(s, lexer=self.lexer.lexer)

    # Error rule for syntax errors
    def p_error(self, p):
        """Raise a parsing error."""
        raise ValueError("Syntax error in input! %s" % str(p))

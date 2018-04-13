from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.parsing import Lexer, Parser
from flloat.syntax.pl import PLNot, PLAtomic, PLOr, PLAnd, PLImplies, PLEquivalence, PLTrue, PLFalse


class PLLexer(Lexer):

    def __init__(self):
        super().__init__()

    reserved = {
        'true': 'TRUE',
        'false': 'FALSE',
    }

    # List of token names.   This is always required
    tokens = (
        'ATOM',
        'NOT',
        'AND',
        'OR',
        'IMPLIES',
        'EQUIVALENCE',
        'LPAREN',
        'RPAREN'
    ) + tuple(reserved.values())

    # Regular expression rules for simple tokens
    t_NOT          = r'~'
    t_AND          = r'&'
    t_OR           = r'\|'
    t_IMPLIES      = r'->'
    t_EQUIVALENCE  = r'<->'
    t_LPAREN       = r'\('
    t_RPAREN       = r'\)'


    def t_ATOM(self, t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = PLLexer.reserved.get(t.value, 'ATOM')  # Check for reserved words
        return t



# Yacc example
class PLParser(Parser):

    def __init__(self):
        lexer = PLLexer()
        precedence = (
            ('left', 'EQUIVALENCE'),
            ('left', 'IMPLIES'),
            ('left', 'OR'),
            ('left', 'AND'),
            ('right', 'NOT'),
        )
        super().__init__("pl", lexer.tokens, lexer, precedence)

    def p_formula_atom(self, p):
        """formula : ATOM
                   | TRUE
                   | FALSE"""
        if p[1]==Symbols.TRUE.value:
            p[0] = PLTrue()
        elif p[1]==Symbols.FALSE.value:
            p[0] = PLFalse()
        else:
            p[0] = PLAtomic(Symbol(p[1]))


    def p_formula_not(self, p):
        'formula : NOT formula'
        p[0] = PLNot(p[2])

    def p_formula_or(self, p):
        'formula : formula OR formula'
        p[0] = PLOr({p[1], p[3]})

    def p_formula_and(self, p):
        'formula : formula AND formula'
        p[0] = PLAnd({p[1], p[3]})

    def p_formula_implies(self, p):
        'formula : formula IMPLIES formula'
        p[0] = PLImplies([p[1], p[3]])

    def p_formula_equivalence(self, p):
        'formula : formula EQUIVALENCE formula'
        p[0] = PLEquivalence({p[1], p[3]})

    def p_factor_expr(self, p):
        'formula : LPAREN formula RPAREN'
        p[0] = p[2]

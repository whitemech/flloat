from flloat.base.Symbol import Symbol
from flloat.base.parsing import Lexer, Parser
from flloat.syntax.pl import PLNot, PLAtomic, PLOr, PLAnd, PLImplies, PLEquivalence


class PLLexer(Lexer):

    def __init__(self):
        super().__init__()

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
    )

    # Regular expression rules for simple tokens
    t_ATOM         = r'[a-zA-Z0-9]+'
    t_NOT          = r'~'
    t_AND          = r'&'
    t_OR           = r'\|'
    t_IMPLIES      = r'->'
    t_EQUIVALENCE  = r'<->'
    t_LPAREN       = r'\('
    t_RPAREN       = r'\)'


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
        'formula : ATOM'
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

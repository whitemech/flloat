from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.parsing import Lexer, Parser
from flloat.parser.pl import PLParser
from flloat.syntax.ldlf import LDLfLogicalTrue, LDLfLogicalFalse, LDLfNot, LDLfOr, LDLfEquivalence, LDLfImplies, \
    LDLfAnd, LDLfDiamond, LDLfBox, RegExpTest, RegExpStar, RegExpUnion, RegExpSequence, RegExpPropositional, LDLfEnd, \
    LDLfLast
from flloat.syntax.pl import PLNot, PLAtomic, PLOr, PLAnd, PLImplies, PLEquivalence, PLTrue, PLFalse


class LDLfLexer(Lexer):

    def __init__(self):
        super().__init__()

    reserved = {
        'true': 'TRUE',
        'false': 'FALSE',
        'tt': 'TT',
        'ff': 'FF',
        'end': 'END',
        'last': 'LAST',
    }

    # List of token names.   This is always required
    tokens = (
        'ATOM',
        'NOT',
        'AND',
        'OR',
        'IMPLIES',
        'EQUIVALENCE',
        'TEST',
        'SEQ',
        'UNION',
        'STAR',
        'LPAREN',
        'RPAREN',
        'BOXLSEPARATOR',
        'BOXRSEPARATOR',
        'DIAMONDLSEPARATOR',
        'DIAMONDRSEPARATOR',
    ) + tuple(reserved.values())

    # Regular expression rules for simple tokens
    t_NOT               = r'~'
    t_AND               = r'&'
    t_OR                = r'\|'
    t_IMPLIES           = r'->'
    t_EQUIVALENCE       = r'<->'
    t_TEST              = r'\?'
    t_SEQ               = r';'
    t_UNION             = r'\+'
    t_STAR              = r'\*'
    t_LPAREN            = r'\('
    t_RPAREN            = r'\)'
    t_BOXLSEPARATOR     = r'\['
    t_BOXRSEPARATOR     = r'\]'
    t_DIAMONDLSEPARATOR = r'<'
    t_DIAMONDRSEPARATOR = r'>'

    def t_ATOM(self, t):
        r'[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = LDLfLexer.reserved.get(t.value, 'ATOM')  # Check for reserved words
        return t


# Yacc example
class LDLfParser(Parser):

    def __init__(self):
        lexer = LDLfLexer()
        precedence = (
            ('left', 'EQUIVALENCE'),
            ('left', 'IMPLIES'),
            ('left', 'UNION'),
            ('left', 'SEQ'),
            ('left', 'STAR'),
            ('left', 'TEST'),
            ('left', 'OR'),
            ('left', 'AND'),
            ('right', 'DIAMONDLSEPARATOR', 'BOXLSEPARATOR'),
            ('left',  'DIAMONDRSEPARATOR', 'BOXRSEPARATOR'),
            ('right', 'NOT'),

        )
        super().__init__("ldlf", lexer.tokens, lexer, precedence)

        # self.pl_parser = PLParser()

    def p_temp_formula(self, p):
        """temp_formula : temp_formula EQUIVALENCE temp_formula
                        | temp_formula IMPLIES temp_formula
                        | temp_formula OR temp_formula
                        | temp_formula AND temp_formula
                        | BOXLSEPARATOR path BOXRSEPARATOR temp_formula
                        | DIAMONDLSEPARATOR path DIAMONDRSEPARATOR temp_formula
                        | NOT temp_formula
                        | TT
                        | FF
                        | END
                        | LAST"""
        if len(p) == 2:
            if p[1] == Symbols.LOGICAL_TRUE.value:
                p[0] = LDLfLogicalTrue()
            elif p[1] == Symbols.LOGICAL_FALSE.value:
                p[0] = LDLfLogicalFalse()
            elif p[1] == Symbols.END.value:
                p[0] = LDLfEnd()
            elif p[1] == Symbols.LAST.value:
                p[0] = LDLfLast()
            else:
                p[0] = LDLfDiamond(RegExpPropositional(p[1]), LDLfLogicalTrue())
        elif len(p) == 3:
            p[0] = LDLfNot(p[2])
        elif len(p) == 4:
            l, o, r = p[1:]
            if o == '<->':
                p[0] = LDLfEquivalence({l, r})
            elif o == '->':
                p[0] = LDLfImplies([l, r])
            elif o == '|':
                p[0] = LDLfOr({l, r})
            elif o == '&':
                p[0] = LDLfAnd({l, r})
            else:
                raise ValueError
        elif len(p) == 5:
            if p[1] == '[':
                p[0] = LDLfBox(p[2], p[4])
            elif p[1] == '<':
                p[0] = LDLfDiamond(p[2], p[4])
            else:
                raise ValueError
        else:
            raise ValueError

    # def p_formula_propositional(self, p):
    #     'formula : propositional'
    #     p[0] = LDLfDiamond(RegExpPropositional(p[1]), LDLfLogicalTrue())

    def p_path(self, p):
        """path : path UNION path
                | path SEQ path
                | path STAR
                | temp_formula TEST
                | propositional"""

        if len(p)==2:
            p[0] = RegExpPropositional(p[1])
        elif len(p)==3:
            if p[2]=="?":
                p[0] = RegExpTest(p[1])
            elif p[2] == "*":
                p[0] = RegExpStar(p[1])
            else:
                raise ValueError
        elif len(p)==4:
            if p[2]=="+":
                p[0] = RegExpUnion({p[1], p[3]})
            elif p[2] == ";":
                p[0] = RegExpSequence([p[1], p[3]])
            else:
                raise ValueError
        else:
            raise ValueError


    def p_propositional(self, p):
        """propositional : propositional EQUIVALENCE propositional
                         | propositional IMPLIES propositional
                         | propositional OR propositional
                         | propositional AND propositional
                         | NOT propositional
                         | FALSE
                         | TRUE
                         | ATOM"""
        if len(p)==4:
            if p[2] == "<->":
                p[0] = PLEquivalence({p[1], p[3]})
            elif p[2] == "->":
                p[0] = PLImplies([p[1], p[3]])
            elif p[2] == "|":
                p[0] = PLOr({p[1], p[3]})
            elif p[2] == "&":
                p[0] = PLAnd({p[1], p[3]})
            else:
                raise ValueError
            # else:
            #     p[0] = p[2]
        elif len(p)==3:
            p[0] = PLNot(p[2])
        elif len(p)==2:
            if p[1]==Symbols.TRUE.value:
                p[0] = PLTrue()
            elif p[1]==Symbols.FALSE.value:
                p[0] = PLFalse()
            else:
                p[0] = PLAtomic(Symbol(p[1]))
        else:
            raise ValueError

    def p_expr_paren(self, p):
        """temp_formula : LPAREN temp_formula RPAREN
        path            : LPAREN path RPAREN
        propositional   : LPAREN propositional RPAREN
        """
        p[0] = p[2]


    # def p_formula_atom(self, p):
    #     """formula : ATOM
    #                | TRUE
    #                | FALSE
    #                | TT
    #                | FF
    #                """
    #     if p[1]==Symbols.TRUE.value:
    #         p[0] = PLTrue()
    #     elif p[1]==Symbols.FALSE.value:
    #         p[0] = PLFalse()
    #     elif p[1] == Symbols.LOGICAL_TRUE.value:
    #         p[0] = LDLfLogicalTrue()
    #     elif p[1] == Symbols.LOGICAL_FALSE.value:
    #         p[0] = LDLfLogicalFalse()
    #     else:
    #         p[0] = PLAtomic(Symbol(p[1]))




if __name__ == '__main__':
    parser = LDLfParser()
    while True:
        try:
            s = input('calc > ')
        except EOFError:
            break
        if not s: continue
        result = parser(s)
        print(result)

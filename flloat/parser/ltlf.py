# -*- coding: utf-8 -*-
"""Implementation of the LTLf parser."""
from flloat.base.parsing import Lexer, Parser
from flloat.base.symbols import Symbols
from flloat.helpers import sym2regexp
from flloat.ltlf import (
    LTLfNext,
    LTLfNot,
    LTLfUntil,
    LTLfEquivalence,
    LTLfImplies,
    LTLfOr,
    LTLfAnd,
    LTLfEventually,
    LTLfAlways,
    LTLfAtomic,
    LTLfRelease,
    LTLfTrue,
    LTLfFalse,
    LTLfWeakNext,
    LTLfEnd,
)


class LTLfLexer(Lexer):
    """Implementation of the lexer for the LTLf parser."""

    reserved = {
        "true": "TRUE",
        "false": "FALSE",
        Symbols.NEXT.value: "NEXT",
        Symbols.WEAK_NEXT.value: "WEAK_NEXT",
        Symbols.UNTIL.value: "UNTIL",
        Symbols.EVENTUALLY.value: "EVENTUALLY",
        Symbols.ALWAYS.value: "ALWAYS",
        Symbols.RELEASE.value: "RELEASE",
        Symbols.END.value: "END",
    }

    # List of token names.   This is always required
    tokens = (
        "ATOM",
        "NOT",
        "AND",
        "OR",
        "IMPLIES",
        "EQUIVALENCE",
        "LPAREN",
        "RPAREN",
    ) + tuple(reserved.values())

    # Regular expression rules for simple tokens
    t_NOT = sym2regexp(Symbols.NOT)
    t_AND = sym2regexp(Symbols.AND)
    t_OR = sym2regexp(Symbols.OR)
    t_IMPLIES = sym2regexp(Symbols.IMPLIES)
    t_EQUIVALENCE = sym2regexp(Symbols.EQUIVALENCE)
    t_LPAREN = sym2regexp(Symbols.ROUND_BRACKET_LEFT)
    t_RPAREN = sym2regexp(Symbols.ROUND_BRACKET_RIGHT)
    t_NEXT = sym2regexp(Symbols.NEXT)
    t_UNTIL = sym2regexp(Symbols.UNTIL)
    t_EVENTUALLY = sym2regexp(Symbols.EVENTUALLY)
    t_ALWAYS = sym2regexp(Symbols.ALWAYS)
    t_RELEASE = sym2regexp(Symbols.RELEASE)
    t_END = sym2regexp(Symbols.END)

    def t_ATOM(self, t):
        r"""[a-zA-Z_][a-zA-Z_0-9]*"""
        t.type = LTLfLexer.reserved.get(t.value, "ATOM")  # Check for reserved words
        return t


class LTLfParser(Parser):
    """Implementation of the parser for the LTLf logic formalism."""

    def __init__(self):
        """Initialize the LTLf parser."""
        lexer = LTLfLexer()
        precedence = (
            ("left", "UNTIL", "EVENTUALLY", "ALWAYS", "RELEASE"),
            ("left", "EQUIVALENCE"),
            ("left", "IMPLIES"),
            ("left", "OR"),
            ("left", "AND"),
            ("right", "NEXT", "WEAK_NEXT"),
            ("right", "NOT"),
        )
        super().__init__("ltlf", lexer.tokens, lexer, precedence)

    def p_formula(self, p):  # NOQA
        """formula : formula EQUIVALENCE formula
                   | formula IMPLIES formula
                   | formula OR formula
                   | formula AND formula
                   | formula UNTIL formula
                   | formula RELEASE formula
                   | EVENTUALLY formula
                   | ALWAYS formula
                   | NEXT formula
                   | WEAK_NEXT formula
                   | NOT formula
                   | TRUE
                   | FALSE
                   | END
                   | ATOM
        """
        if len(p) == 2:
            if p[1] == Symbols.TRUE.value:
                p[0] = LTLfTrue()
            elif p[1] == Symbols.FALSE.value:
                p[0] = LTLfFalse()
            elif p[1] == Symbols.END.value:
                p[0] = LTLfEnd()
            else:
                p[0] = LTLfAtomic(p[1])
        elif len(p) == 3:
            if p[1] == Symbols.NEXT.value:
                p[0] = LTLfNext(p[2])
            elif p[1] == Symbols.WEAK_NEXT.value:
                p[0] = LTLfWeakNext(p[2])
            elif p[1] == Symbols.EVENTUALLY.value:
                p[0] = LTLfEventually(p[2])
            elif p[1] == Symbols.ALWAYS.value:
                p[0] = LTLfAlways(p[2])
            elif p[1] == Symbols.NOT.value:
                p[0] = LTLfNot(p[2])
        elif len(p) == 4:
            l, o, r = p[1:]
            if o == Symbols.EQUIVALENCE.value:
                p[0] = LTLfEquivalence([l, r])
            elif o == Symbols.IMPLIES.value:
                p[0] = LTLfImplies([l, r])
            elif o == Symbols.OR.value:
                p[0] = LTLfOr([l, r])
            elif o == Symbols.AND.value:
                p[0] = LTLfAnd([l, r])
            elif o == Symbols.UNTIL.value:
                p[0] = LTLfUntil([l, r])
            elif o == Symbols.RELEASE.value:
                p[0] = LTLfRelease([l, r])
            else:
                raise ValueError
        else:
            raise ValueError

    def p_expr_paren(self, p):
        """formula : LPAREN formula RPAREN"""
        p[0] = p[2]


if __name__ == "__main__":
    parser = LTLfParser()
    while True:
        try:
            s = input("parser > ")
        except EOFError:
            break
        if not s:
            continue
        result = parser(s)
        print(str(result))

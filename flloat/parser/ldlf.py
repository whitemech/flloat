# -*- coding: utf-8 -*-
"""Implementation of the LDLf parser."""
from pathlib import Path

from lark import Lark, Transformer, Token, Tree

from flloat.helpers import ParsingError
from flloat.ldlf import (
    LDLfLogicalTrue,
    LDLfLogicalFalse,
    LDLfNot,
    LDLfOr,
    LDLfEquivalence,
    LDLfImplies,
    LDLfAnd,
    LDLfDiamond,
    LDLfBox,
    RegExpTest,
    RegExpStar,
    RegExpUnion,
    RegExpSequence,
    RegExpPropositional,
    LDLfEnd,
    LDLfLast,
    LDLfPropositionalAtom,
)
from flloat.parser import CUR_DIR
from flloat.parser.pl import PLTransformer


class LDLfTransformer(Transformer):
    def __init__(self):
        super().__init__()
        self._pl_transformer = PLTransformer()

    def start(self, args):
        return args[0]

    def ldlf_formula(self, args):
        assert len(args) == 1
        return args[0]

    def ldlf_equivalence(self, args):
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LDLfEquivalence(subformulas)
        else:
            raise ParsingError

    def ldlf_implication(self, args):
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LDLfImplies(subformulas)
        else:
            raise ParsingError

    def ldlf_or(self, args):
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LDLfOr(subformulas)
        else:
            raise ParsingError

    def ldlf_and(self, args):
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return LDLfAnd(subformulas)
        else:
            raise ParsingError

    def ldlf_box(self, args):
        if len(args) == 1:
            return args[0]
        elif len(args) == 4:
            _, regex, _, formula = args
            return LDLfBox(regex, formula)
        else:
            raise ParsingError

    def ldlf_diamond(self, args):
        if len(args) == 1:
            return args[0]
        elif len(args) == 4:
            _, regex, _, formula = args
            return LDLfDiamond(regex, formula)
        else:
            raise ParsingError

    def ldlf_not(self, args):
        if len(args) == 1:
            return args[0]
        else:
            f = args[-1]
            for _ in args[:-1]:
                f = LDLfNot(f)
            return f

    def ldlf_wrapped(self, args):
        if len(args) == 1:
            return args[0]
        elif len(args) == 3:
            _, formula, _ = args
            return formula
        else:
            raise ParsingError

    def ldlf_atom(self, args):
        assert len(args) == 1
        formula = args[0]
        return formula

    def ldlf_tt(self, args):
        return LDLfLogicalTrue()

    def ldlf_ff(self, args):
        return LDLfLogicalFalse()

    def ldlf_last(self, args):
        return LDLfLast()

    def ldlf_end(self, args):
        return LDLfEnd()

    def ldlf_symbol(self, args):
        assert len(args) == 1
        tree = args[0]
        pl_transformer = PLTransformer()
        symbol = pl_transformer.transform(tree)
        return LDLfPropositionalAtom(symbol)

    def regular_expression(self, args):
        assert len(args) == 1
        return args[0]

    def regular_expression_union(self, args):
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return RegExpUnion(subformulas)
        else:
            raise ParsingError

    def regular_expression_sequence(self, args):
        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return RegExpSequence(subformulas)
        else:
            raise ParsingError

    def regular_expression_star(self, args):
        if len(args) == 1:
            return args[0]
        if len(args) == 2:
            l, _ = args
            return RegExpStar(l)
        elif len(args) == 3:
            _, formula, _ = args
            return formula
        elif len(args) == 4:
            _, formula, _, _ = args
            return RegExpStar(formula)
        else:
            raise ParsingError

    def regular_expression_test(self, args):
        if len(args) == 1:
            return args[0]
        elif len(args) == 2:
            formula, _ = args
            return RegExpTest(formula)
        elif len(args) == 3:
            _, formula, _ = args
            return formula
        elif len(args) == 4:
            _, formula, _, _ = args
            return RegExpTest(formula)
        else:
            raise ParsingError

    def regular_expression_propositional(self, args):
        assert len(args) == 1
        return RegExpPropositional(args[0])

    def propositional_formula(self, args):
        tree = args[0]

        def _replace(tree):
            if isinstance(tree, Token):
                tree.type = tree.type.replace("pl__", "")
            if isinstance(tree, Tree):
                tree.data = tree.data.replace("pl__", "")
                for t in tree.children:
                    _replace(t)

        _replace(tree)
        formula = self._pl_transformer.transform(tree)
        return formula


class LDLfParser:
    def __init__(self):
        self._parser = Lark(open(str(Path(CUR_DIR, "ldlf.lark"))), parser="lalr")
        self._transformer = LDLfTransformer()

    def __call__(self, text):
        tree = self._parser.parse(text)
        formula = self._transformer.transform(tree)
        return formula


if __name__ == "__main__":
    parser = LDLfParser()
    while True:
        try:
            s = input("ldlf> ")
            if not s:
                continue
            result = parser(s)
            print("result:", result, type(result))
        except EOFError:
            break
        except Exception as e:
            print(str(e))

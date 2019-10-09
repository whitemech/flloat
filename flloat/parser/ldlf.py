# -*- coding: utf-8 -*-
"""Implementation of the LDLf parser."""
import inspect
import os
from pathlib import Path

from lark import Lark, Transformer, Token, Tree

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
    LDLfAtomic, LDLfTrue, LDLfFalse)
from flloat.parser.pl import PLTransformer

CUR_DIR = os.path.dirname(inspect.getfile(inspect.currentframe()))


class LDLfTransformer(Transformer):

    def __init__(self):
        super().__init__()
        self._pl_transformer = PLTransformer()

    def start(self, args):
        return args[0]

    def ldlf_formula(self, args):
        assert len(args) == 1
        return args[0]

    def ldlf_wrapped(self, args):
        assert len(args) == 3
        return args[1]

    def ldlf_equivalence(self, args):
        assert len(args) == 3
        l, _, r = args
        return LDLfEquivalence([l, r])

    def ldlf_implication(self, args):
        assert len(args) == 3
        l, _, r = args
        return LDLfImplies([l, r])

    def ldlf_or(self, args):
        assert len(args) == 3
        l, _, r = args
        return LDLfOr([l, r])

    def ldlf_and(self, args):
        assert len(args) == 3
        l, _, r = args
        return LDLfAnd([l, r])

    def ldlf_box(self, args):
        assert len(args) == 4
        regexpr, expr = args[1], args[3]
        return LDLfBox(regexpr, expr)

    def ldlf_diamond(self, args):
        assert len(args) == 4
        regexpr, expr = args[1], args[3]
        return LDLfDiamond(regexpr, expr)

    def ldlf_not(self, args):
        assert len(args) == 2
        l, r = args
        return LDLfNot(r)

    def ldlf_atom(self, args):
        assert len(args) == 1
        formula = args[0]
        if isinstance(formula, (LDLfTrue, LDLfFalse, LDLfLogicalTrue, LDLfLogicalFalse, LDLfEnd, LDLfLast)):
            return formula
        elif isinstance(formula, str):
            return LDLfAtomic(formula)
        else:
            raise ValueError()

    def ldlf_tt(self, args):
        return LDLfLogicalTrue()

    def ldlf_ff(self, args):
        return LDLfLogicalFalse()

    def ldlf_last(self, args):
        return LDLfLast()

    def ldlf_end(self, args):
        return LDLfEnd()

    def ldlf_true(self, args):
        return LDLfTrue()

    def ldlf_false(self, args):
        return LDLfFalse()

    def ldlf_symbol(self, args):
        assert len(args) == 1
        tree = args[0]
        pl_transformer = PLTransformer()
        symbol = pl_transformer.transform(tree)
        return symbol

    def regular_expression(self, args):
        assert len(args) == 1
        return args[0]

    def wrapped_regular_expression(self, args):
        assert len(args) == 3
        return args[1]

    def regular_expression_union(self, args):
        assert len(args) == 3
        l, _, r = args
        return RegExpUnion([l, r])

    def regular_expression_sequence(self, args):
        assert len(args) == 3
        l, _, r = args
        return RegExpSequence([l, r])

    def regular_expression_test(self, args):
        assert len(args) == 2
        l, r = args
        return RegExpTest(l)

    def regular_expression_star(self, args):
        assert len(args) == 2
        l, r = args
        return RegExpStar(l)

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
        self._parser = Lark(open(str(Path(CUR_DIR, "ldlf.lark"))))
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

# -*- coding: utf-8 -*-
"""Implementation of the PL parser."""
import inspect
import os
from pathlib import Path

from lark import Lark, Transformer

from flloat.pl import (
    PLNot,
    PLAtomic,
    PLOr,
    PLAnd,
    PLImplies,
    PLEquivalence,
    PLTrue,
    PLFalse,
)

CUR_DIR = os.path.dirname(inspect.getfile(inspect.currentframe()))


class PLTransformer(Transformer):

    def start(self, args):
        return args[0]

    def propositional_formula(self, args):
        """Parse the propositional formula"""
        assert len(args) == 1
        return args[0]

    def wrapped_prop(self, args):
        assert len(args) == 3
        return args[1]

    def equivalence_prop(self, args):
        assert len(args) == 3
        l, _, r = args
        return PLEquivalence([l, r])

    def implication_prop(self, args):
        assert len(args) == 3
        l, _, r = args
        return PLImplies([l, r])

    def or_prop(self, args):
        assert len(args) == 3
        l, _, r = args
        return PLOr([l, r])

    def and_prop(self, args):
        assert len(args) == 3
        l, _, r = args
        return PLAnd([l, r])

    def not_prop(self, args):
        assert len(args) == 2
        _, r = args
        return PLNot(r)

    def atom_prop(self, args):
        assert len(args) == 1
        return args[0]

    def true_prop(self, args):
        assert len(args) == 1
        return PLTrue()

    def false_prop(self, args):
        assert len(args) == 1
        return PLFalse()

    def atom(self, args):
        assert len(args) == 1
        return PLAtomic(str(args[0]))

    def string(self, args):
        return "".join(args)


class PLParser:

    def __init__(self):
        self._transformer = PLTransformer()
        self._parser = Lark(open(str(Path(CUR_DIR, "pl.lark"))))

    def __call__(self, text):
        tree = self._parser.parse(text)
        formula = self._transformer.transform(tree)
        return formula



if __name__ == "__main__":
    parser = PLParser()
    while True:
        try:
            s = input("pl > ")
        except EOFError:
            break
        if not s:
            continue
        result = parser(s)
        print(result)

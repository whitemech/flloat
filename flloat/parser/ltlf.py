# -*- coding: utf-8 -*-
"""Implementation of the LTLf parser."""
import inspect
import os

from flloat.base.formulas import AtomicFormula

from flloat.parser.pl import PLTransformer

CUR_DIR = os.path.dirname(inspect.getfile(inspect.currentframe()))

# -*- coding: utf-8 -*-
"""Implementation of the PL parser."""
import inspect
import os
from pathlib import Path

from flloat.ltlf import LTLfEquivalence, LTLfImplies, LTLfOr, LTLfAnd, LTLfNot, LTLfUntil, LTLfRelease, LTLfAlways, \
    LTLfEventually, LTLfNext, LTLfWeakNext, LTLfTrue, LTLfAtomic, LTLfFalse
from lark import Lark, Transformer

from flloat.pl import (
    PLAtomic,
    PLTrue,
    PLFalse,
)

CUR_DIR = os.path.dirname(inspect.getfile(inspect.currentframe()))


class LTLfTransformer(Transformer):

    def __init__(self):
        super().__init__()
        self._pl_transformer = PLTransformer()

    def start(self, args):
        assert len(args) == 1
        return args[0]

    def ltlf_formula(self, args):
        assert len(args) == 1
        return args[0]

    def ltlf_wrapped(self, args):
        assert len(args) == 3
        return args[1]

    def ltlf_equivalence(self, args):
        assert len(args) == 3
        l, _, r = args
        return LTLfEquivalence([l, r])

    def ltlf_implication(self, args):
        assert len(args) == 3
        l, _, r = args
        return LTLfImplies([l, r])

    def ltlf_or(self, args):
        assert len(args) == 3
        l, _, r = args
        return LTLfOr([l, r])

    def ltlf_and(self, args):
        assert len(args) == 3
        l, _, r = args
        return LTLfAnd([l, r])

    def ltlf_until(self, args):
        assert len(args) == 3
        l, _, r = args
        return LTLfUntil([l, r])

    def ltlf_release(self, args):
        assert len(args) == 3
        l, _, r = args
        return LTLfRelease([l, r])

    def ltlf_always(self, args):
        assert len(args) == 2
        _, r = args
        return LTLfAlways(r)

    def ltlf_eventually(self, args):
        assert len(args) == 2
        _, r = args
        return LTLfEventually(r)

    def ltlf_next(self, args):
        assert len(args) == 2
        _, r = args
        return LTLfNext(r)

    def ltlf_weak_next(self, args):
        assert len(args) == 2
        _, r = args
        return LTLfWeakNext(r)

    def ltlf_not(self, args):
        assert len(args) == 2
        _, r = args
        return LTLfNot(r)

    def ltlf_atom(self, args):
        assert len(args) == 1
        formula = args[0]
        if isinstance(formula, LTLfTrue) or isinstance(formula, LTLfFalse):
            return formula
        elif isinstance(formula, str):
            return LTLfAtomic(formula)
        else:
            raise ValueError()

    def ltlf_true(self, args):
        return LTLfTrue()

    def ltlf_false(self, args):
        return LTLfFalse()

    def ltlf_symbol(self, args):
        assert len(args) == 1
        tree = args[0]
        pl_transformer = PLTransformer()
        symbol = pl_transformer.transform(tree)
        return symbol

class LTLfParser:

    def __init__(self):
        self._transformer = LTLfTransformer()
        self._parser = Lark(open(str(Path(CUR_DIR, "ltlf.lark"))))

    def __call__(self, text):
        tree = self._parser.parse(text)
        formula = self._transformer.transform(tree)
        return formula


if __name__ == "__main__":
    parser = LTLfParser()
    while True:
        try:
            s = input("ltlf > ")
        except EOFError:
            break
        if not s:
            continue
        result = parser(s)
        print(result)

if __name__ == "__main__":
    parser = LTLfParser()
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

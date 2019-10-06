# -*- coding: utf-8 -*-
"""Base classes for truth evaluations of logical formulas."""
from abc import ABC, abstractmethod

from flloat.base.formulas import UnaryOperator, BinaryOperator
from flloat.base.symbols import Symbols


class Truth(ABC):
    """Interface for formulas that support truth evaluation."""

    @abstractmethod
    def truth(self, *args) -> bool:
        """Return true if formula is true in a given interpretation."""


class NotTruth(UnaryOperator, Truth):
    """A 'not' formula that supports truth evaluation."""

    operator_symbol = Symbols.NOT.value

    def truth(self, *args):
        """Get the truth of the formula wrt. a model."""
        return not self.f.truth(*args)


class AndTruth(BinaryOperator, Truth):
    """An 'and' formula that supports truth evaluation."""

    operator_symbol = Symbols.AND.value

    def truth(self, *args):
        """Get the truth of the formula wrt. a model."""
        for f in self.formulas:
            if not f.truth(*args):
                return False
        return True
        # return all(f.truth(*args) for f in self.formulas)


class OrTruth(BinaryOperator, Truth):
    """An 'or' formula that supports truth evaluation."""

    operator_symbol = Symbols.OR.value

    def truth(self, *args):
        """Get the truth of the formula wrt. a model."""
        for f in self.formulas:
            if f.truth(*args):
                return True
        return False
        # return any(f.truth(*args) for f in self.formulas)


class ImpliesTruth(BinaryOperator, Truth):
    """An 'implies' formula that supports truth evaluation."""

    operator_symbol = Symbols.IMPLIES.value

    def truth(self, *args):
        """Get the truth of the formula wrt. a model."""
        # eq_formula = self._convert()
        # return eq_formula.truth(i)
        fs = self.formulas
        N = len(fs)

        def evaluation(x, y):
            return not x or y

        truth = evaluation(fs[0].truth(*args), fs[1].truth(*args))
        for idx in range(2, N):
            # if at some point the premise is False, then return True
            if not truth:
                return True
            else:
                truth = evaluation(truth, fs[idx].truth(*args))

        return truth


class EquivalenceTruth(BinaryOperator, Truth):
    """An 'equivalence' formula that supports truth evaluation."""

    operator_symbol = Symbols.EQUIVALENCE.value

    def truth(self, *args):
        """Get the truth of the formula wrt. a model."""
        fs = self.formulas
        t = [f.truth(*args) for f in fs]
        # either all true or all false
        return all(t) or not any(t)

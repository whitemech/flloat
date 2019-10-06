# -*- coding: utf-8 -*-
"""
This module contains definitions of interfaces for convertible formulas.

'Convertible' means that, given a logic formalism, the formula can be expressed in simpler terms.

E.g. the 'equivalence' formula can be expressed in terms of And and Not:

    A <-> B === (A & B) & (!A & !B)

Hence all 'equivalence' formulas should support such conversion.
"""
from abc import abstractmethod, ABC

from flloat.base.formulas import Formula, BinaryOperator, CommutativeBinaryOperator
from flloat.base.nnf import NNF
from flloat.base.truths import Truth, ImpliesTruth, EquivalenceTruth


class ConvertibleFormula(Formula):
    """An interface that defines a 'convert' method.

    The formula can be converted to simpler operators,
    depending on the logic formalism the formula belongs to.
    """

    @abstractmethod
    def convert(self):
        """Convert the formula."""


class BaseConvertibleFormula(ConvertibleFormula, Truth, NNF, ABC):
    """A base convertible formula that supports truth evaluation and NNF transformation."""

    def truth(self, *args) -> bool:
        """Get the truth of the formula wrt. a model."""
        return self.convert().truth(*args)

    def _to_nnf(self):
        return self.convert().to_nnf()

    def negate(self):
        """Negate the formula."""
        return self.convert().negate()


class ImpliesConvertible(ImpliesTruth, BaseConvertibleFormula, BinaryOperator):
    """A base convertible formula for formulas of type 'implies'."""

    @property
    @abstractmethod
    def And(self):
        """Get the 'and' formula."""

    @property
    @abstractmethod
    def Or(self):
        """Get the 'or' formula."""

    @property
    @abstractmethod
    def Not(self):
        """Get the 'not' formula."""

    def convert(self):
        """Convert the formula."""
        fs = self.formulas
        if len(fs) > 2:
            a, b = self.And(fs[:-1]), fs[-1]
        else:
            a, b = fs
        res = self.Or([self.Not(a), b])
        return res


class EquivalenceConvertible(
    EquivalenceTruth, BaseConvertibleFormula, CommutativeBinaryOperator, ABC
):
    """A base convertible formula for formulas of type 'equivalence'."""

    @property
    @abstractmethod
    def And(self):
        """Get the 'and' formula."""

    @property
    @abstractmethod
    def Or(self):
        """Get the 'or' formula."""

    @property
    @abstractmethod
    def Not(self):
        """Get the 'not' formula."""

    def convert(self):
        """Convert the formula."""
        fs = self.formulas
        pos = self.And(fs)
        neg = self.And([self.Not(f) for f in fs])

        res = self.Or([pos, neg])
        return res

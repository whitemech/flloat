# -*- coding: utf-8 -*-
"""Base classes for delta transformations."""

from abc import ABC, abstractmethod

from pythomata import PropInt

from flloat.base.convertible import (
    BaseConvertibleFormula,
    EquivalenceConvertible,
    ImpliesConvertible
)
from flloat.pl import PLFormula


class Delta(ABC):
    """Interface for formulas where the delta function is defined."""

    @abstractmethod
    def delta(self, i: PropInt, epsilon: bool = False) -> PLFormula:
        """
        Apply the delta function.

        :param i: the propositional interpretation.
        :param epsilon:
        :return:
        """


class DeltaConvertibleFormula(BaseConvertibleFormula, Delta, ABC):
    """Interface for 'equivalence' formulas delta-convertible."""

    def _delta(self, i: PropInt, epsilon=False) -> PLFormula:
        return self.convert().delta(i, epsilon)


class EquivalenceDeltaConvertible(EquivalenceConvertible, DeltaConvertibleFormula, ABC):
    """Interface for 'equivalence' formulas delta-convertible."""


class ImpliesDeltaConvertible(ImpliesConvertible, DeltaConvertibleFormula, ABC):
    """Interface for 'implies' formulas delta-convertible."""

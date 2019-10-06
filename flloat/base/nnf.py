# -*- coding: utf-8 -*-
"""Base classes for Negative Normal Form (NNF) transformations."""
from abc import ABC, abstractmethod
from functools import lru_cache
from typing import Type

from flloat.base.formulas import (
    UnaryOperator,
    CommutativeBinaryOperator,
    BinaryOperator,
)
from flloat.helpers import MAX_CACHE_SIZE


class NNF(ABC):
    """Interface for formulas that support NNF transformations."""

    @lru_cache(maxsize=MAX_CACHE_SIZE)
    def to_nnf(self):
        """Transform the formula in NNF."""
        return self._to_nnf()

    @abstractmethod
    def _to_nnf(self):
        """Actual implementation of the NNF transformation."""

    @abstractmethod
    def negate(self):
        """Negate the formula."""


class DualNNF(NNF, ABC):
    """A formula that has a negative dual formulas that supports the NNF."""

    @property
    def Dual(self) -> Type:
        """Get the 'dual' formula type for the concrete class."""

    @Dual.setter
    def Dual(self, x) -> None:
        """Set the 'dual' formula type for the concrete class."""
        self.Dual = x


class AtomicNNF(NNF, ABC):
    """An atomic formula that support the NNF."""

    @property
    def Not(self) -> Type:
        """Get the 'not' formula type for the concrete class."""

    @Not.setter
    def Not(self, x) -> None:
        """Set the 'not' formula type for the concrete class."""
        self.Not = x

    def _to_nnf(self):
        return self

    def negate(self):
        """Negate the formula."""
        return self.Not(self)


class NotNNF(UnaryOperator, NNF, ABC):
    """An unary formula that represents the 'not' wrt the NNF."""

    def _to_nnf(self):
        if isinstance(self.f, AtomicNNF):
            return self.f.negate()
        else:
            return self.f.negate().to_nnf()

    def negate(self):
        """Negate the formula."""
        return self.f


class DualUnaryOperatorNNF(UnaryOperator, DualNNF, ABC):
    """An unary formula that has a dual formula wrt the NNF."""

    @property
    def Not(self) -> Type:
        """Get the 'not' formula type for the concrete class."""

    @Not.setter
    def Not(self, x) -> None:
        """Set the 'not' formula type for the concrete class."""
        self.Not = x

    def _to_nnf(self):
        return type(self)(self.f.to_nnf())

    def negate(self):
        """Negate the formula."""
        return self.Dual(self.Not(self.f))


class DualBinaryOperatorNNF(BinaryOperator, DualNNF, ABC):
    """A binary formula that supports the NNF."""

    def _to_nnf(self):
        childs = [child.to_nnf() for child in self.formulas]
        return type(self)(childs).simplify()

    def negate(self):
        """Negate the formula."""
        childs = [child.negate() for child in self.formulas]
        return self.Dual(childs).simplify()


class DualCommutativeOperatorNNF(CommutativeBinaryOperator, DualBinaryOperatorNNF, ABC):
    """A binary formula that has a negative dual formula and supports the NNF."""

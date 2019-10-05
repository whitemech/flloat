# -*- coding: utf-8 -*-
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
    @lru_cache(maxsize=MAX_CACHE_SIZE)
    def to_nnf(self):
        return self._to_nnf()

    @abstractmethod
    def _to_nnf(self):
        """Actual implementation of the NNF transformation."""

    @abstractmethod
    def negate(self):
        """Negate the formula."""


class DualNNF(NNF, ABC):
    @property
    def Dual(self) -> Type:
        """The 'dual' formula type for the concrete class."""

    @Dual.setter
    def Dual(self, x):
        self.Dual = x


class AtomicNNF(NNF, ABC):
    @property
    def Not(self) -> Type:
        """The 'not' formula type for the concrete class."""

    @Not.setter
    def Not(self, x) -> None:
        self.Not = x

    def _to_nnf(self):
        return self

    def negate(self):
        return self.Not(self)


class NotNNF(UnaryOperator, NNF, ABC):
    def _to_nnf(self):
        if isinstance(self.f, AtomicNNF):
            return self.f.negate()
        else:
            return self.f.negate().to_nnf()

    def negate(self):
        return self.f


class DualUnaryOperatorNNF(UnaryOperator, DualNNF, ABC):
    @property
    def Not(self) -> Type:
        """The 'not' formula type for the concrete class."""

    @Not.setter
    def Not(self, x) -> None:
        self.Not = x

    def _to_nnf(self):
        return type(self)(self.f.to_nnf())

    def negate(self):
        return self.Dual(self.Not(self.f))


class DualBinaryOperatorNNF(BinaryOperator, DualNNF, ABC):
    def _to_nnf(self):
        childs = [child.to_nnf() for child in self.formulas]
        return type(self)(childs).simplify()

    def negate(self):
        childs = [child.negate() for child in self.formulas]
        return self.Dual(childs).simplify()


class DualCommutativeOperatorNNF(CommutativeBinaryOperator, DualBinaryOperatorNNF, ABC):
    pass

from abc import ABC, abstractmethod
from functools import lru_cache

from flloat.base.Formula import UnaryOperator, CommutativeBinaryOperator, AtomicFormula, BinaryOperator
from flloat.utils import MAX_CACHE_SIZE


class NNF(ABC):

    @lru_cache(maxsize=MAX_CACHE_SIZE)
    def to_nnf(self):
        return self._to_nnf()

    @abstractmethod
    def _to_nnf(self):
        raise NotImplementedError

    @abstractmethod
    def negate(self):
        raise NotImplementedError


class DualNNF(NNF):
    @property
    def Dual(self):
        raise NotImplementedError

    @Dual.setter
    def Dual(self, x):
        self.Dual = x

class NotNNF(UnaryOperator, NNF):
    def _to_nnf(self):
        if not isinstance(self.f, AtomicFormula):
            return self.f.negate().to_nnf()
        else:
            return self.f.negate()

    def negate(self):
        return self.f


class DualUnaryOperatorNNF(UnaryOperator, DualNNF):
    @property
    def Not(self):
        raise NotImplementedError

    @Not.setter
    def Not(self, x):
        self.Not = x

    def _to_nnf(self):
        return type(self)(self.f.to_nnf())

    def negate(self):
        return self.Dual(self.Not(self.f))

class DualBinaryOperatorNNF(BinaryOperator, DualNNF):

    def _to_nnf(self):
        childs = [child.to_nnf() for child in self.formulas]
        return type(self)(childs).simplify()

    def negate(self):
        childs = [child.negate().to_nnf() for child in self.formulas]
        return self.Dual(childs).simplify()


class DualCommutativeOperatorNNF(CommutativeBinaryOperator, DualBinaryOperatorNNF):
    pass
    # def _to_nnf(self):
    #     childs = [child.to_nnf() for child in self.members]
    #     return type(self)(childs).simplify()
    #
    # def negate(self):
    #     childs = [child.negate().to_nnf() for child in self.members]
    #     return self.Dual(childs)



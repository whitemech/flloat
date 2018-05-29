from abc import ABC, abstractmethod

from flloat.base.Formula import UnaryOperator, CommutativeBinaryOperator, AtomicFormula, BinaryOperator
from flloat.base.truths import NotTruth


class NNF(ABC):

    def __init__(self):
        pass
        # self.precomputed_nnf = None

    def to_nnf(self):
        # get the result already computed, if any
        # if self.precomputed_nnf is None:
        #     self.precomputed_nnf = self._to_nnf()
        # return self.precomputed_nnf
        return self._to_nnf()

    @abstractmethod
    def _to_nnf(self):
        raise NotImplementedError

    @abstractmethod
    def negate(self):
        raise NotImplementedError


class NotNNF(UnaryOperator, NNF):
    def _to_nnf(self):
        if not isinstance(self.f, AtomicFormula):
            return self.f.negate().to_nnf()
        else:
            return self

    def negate(self):
        return self.f

class DualNNF(NNF):
    @property
    def Dual(self):
        raise NotImplementedError

    @Dual.setter
    def Dual(self, x):
        self.Dual = x


class DualUnaryOperatorNNF(UnaryOperator, DualNNF):
    @property
    def Not(self):
        raise NotImplementedError

    @Not.setter
    def Not(self, x):
        self.Not= x

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
        return self.Dual(childs)


class DualCommutativeOperatorNNF(CommutativeBinaryOperator, DualBinaryOperatorNNF, DualNNF):
    pass


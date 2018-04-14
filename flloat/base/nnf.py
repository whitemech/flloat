from abc import ABC, abstractmethod

from flloat.base.Formula import UnaryOperator, CommutativeBinaryOperator, AtomicFormula
from flloat.base.truths import NotTruth


class NNF(ABC):
    @abstractmethod
    def to_nnf(self):
        raise NotImplementedError

    @abstractmethod
    def negate(self):
        raise NotImplementedError

# class DefaultNNF(NNF):
#     def to_nnf(self):
#         return self
#

class NotNNF(UnaryOperator, NNF):
    def to_nnf(self):
        if not isinstance(self.f, AtomicFormula):
            return self.f.negate().to_nnf()
        else:
            return self

    def negate(self):
        return self.f

# class AndNNF(CommutativeBinaryOperator, NNF):
#     @property
#     def OrClass(self):
#         raise NotImplementedError
#
#     def to_nnf(self):
#         childs = set([child.to_nnf() for child in self.formulas])
#         return type(self)(childs)
#
#     def negate(self):
#         childs = set([child.negate().to_nnf() for child in self.formulas])
#         return self.OrClass(childs)
#
# class OrNNF(CommutativeBinaryOperator, NNF):
#     @property
#     def AndClass(self):
#         raise NotImplementedError
#
# class ImpliesNNF(CommutativeBinaryOperator, NNF):
#     @property
#     def OrClass(self):
#         raise NotImplementedError
#
#     @property
#     def AndClass(self):
#         raise NotImplementedError
#

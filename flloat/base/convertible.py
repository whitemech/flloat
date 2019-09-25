from abc import abstractmethod, ABC

from flloat.base.formulas import Formula, BinaryOperator, CommutativeBinaryOperator
from flloat.base.nnf import NNF
from flloat.base.truths import Truth, ImpliesTruth, EquivalenceTruth


class ConvertibleFormula(Formula):
    @abstractmethod
    def convert(self):
        raise NotImplementedError


class BaseConvertibleFormula(ConvertibleFormula, Truth, NNF, ABC):
    def truth(self, *args):
        return self.convert().truth(*args)

    def _to_nnf(self):
        return self.convert().to_nnf()

    def negate(self):
        return self.convert().negate()


class ImpliesConvertible(ImpliesTruth, BaseConvertibleFormula, BinaryOperator):
    @property
    def And(self):
        raise NotImplementedError

    @property
    def Or(self):
        raise NotImplementedError

    @property
    def Not(self):
        raise NotImplementedError

    def convert(self):
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
    @property
    def And(self):
        raise NotImplementedError

    @property
    def Or(self):
        raise NotImplementedError

    @property
    def Not(self):
        raise NotImplementedError

    def convert(self):
        fs = self.formulas
        pos = self.And(fs)
        neg = self.And([self.Not(f) for f in fs])

        res = self.Or([pos, neg])
        return res

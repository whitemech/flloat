from abc import abstractmethod, ABC

from flloat.base.formulas import Formula, BinaryOperator, CommutativeBinaryOperator
from flloat.base.delta import Delta
from flloat.base.nnf import NNF
from flloat.base.truths import Truth, ImpliesTruth, EquivalenceTruth
from flloat.semantics.pl import PLInterpretation


class ConvertibleFormula(Formula):
    @abstractmethod
    def _convert(self):
        raise NotImplementedError


class BaseConvertibleFormula(ConvertibleFormula, Truth, NNF, ABC):
    def truth(self, *args):
        return self._convert().truth(*args)

    def _to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate()


class DeltaConvertibleFormula(BaseConvertibleFormula, Delta):

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self._convert().delta(i, epsilon)


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

    def _convert(self):
        fs = self.formulas
        if len(fs) > 2:
            a, b = self.And(fs[:-1]), fs[-1]
        else:
            a, b = fs
        res = self.Or([self.Not(a), b])
        return res


class ImpliesDeltaConvertible(ImpliesConvertible, DeltaConvertibleFormula, ABC):
    pass


class EquivalenceConvertible(EquivalenceTruth, BaseConvertibleFormula, CommutativeBinaryOperator, ABC):

    @property
    def And(self):
        raise NotImplementedError

    @property
    def Or(self):
        raise NotImplementedError

    @property
    def Not(self):
        raise NotImplementedError

    def _convert(self):
        fs = self.formulas
        pos = self.And(fs)
        neg = self.And([self.Not(f) for f in fs])

        res = self.Or([pos, neg])
        return res


class EquivalenceDeltaConvertible(EquivalenceConvertible, DeltaConvertibleFormula, ABC):
    pass

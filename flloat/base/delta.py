from abc import ABC, abstractmethod

from flloat.base.convertible import BaseConvertibleFormula, EquivalenceConvertible, ImpliesConvertible
from flloat.semantics.pl import PLInterpretation
from flloat.pl import PLFormula


class Delta(ABC):

    @abstractmethod
    def delta(self, i: PLInterpretation, epsilon: bool = False) -> PLFormula:
        """
        Apply the delta function.

        :param i: the propositional interpretation.
        :param epsilon:
        :return:
        """


class DeltaConvertibleFormula(BaseConvertibleFormula, Delta):

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self.convert().delta(i, epsilon)


class EquivalenceDeltaConvertible(EquivalenceConvertible, DeltaConvertibleFormula, ABC):
    pass


class ImpliesDeltaConvertible(ImpliesConvertible, DeltaConvertibleFormula, ABC):
    pass

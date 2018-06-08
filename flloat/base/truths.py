from abc import ABC, abstractmethod

from flloat.base.Formula import Formula, UnaryOperator, BinaryOperator
from flloat.base.Symbols import Symbols


class Truth(ABC):
    @abstractmethod
    def truth(self, *args) -> bool:
        raise NotImplementedError

class NotTruth(UnaryOperator, Truth):
    def __init__(self, f):
        UnaryOperator.__init__(self, f)

    operator_symbol = Symbols.NOT.value
    def truth(self, *args):
        return not self.f.truth(*args)


class AndTruth(BinaryOperator, Truth):
    operator_symbol = Symbols.AND.value
    def truth(self, *args):
        for f in self.formulas:
            if not f.truth(*args):
                return False
        return True
        # return all(f.truth(*args) for f in self.formulas)

class OrTruth(BinaryOperator, Truth):
    operator_symbol = Symbols.OR.value
    def truth(self, *args):
        for f in self.formulas:
            if f.truth(*args):
                return True
        return False
        # return any(f.truth(*args) for f in self.formulas)


class ImpliesTruth(BinaryOperator, Truth):
    operator_symbol = Symbols.IMPLIES.value


    def truth(self, *args):
        # eq_formula = self._convert()
        # return eq_formula.truth(i)
        fs = self.formulas
        N = len(fs)
        evaluation = lambda x, y: not x or y
        truth = evaluation(fs[0].truth(*args), fs[1].truth(*args))
        for idx in range(2, N):
            # if at some point the premise is False, then return True
            if not truth:
                return True
            else:
                truth = evaluation(truth, fs[idx].truth(*args))

        return truth

class EquivalenceTruth(BinaryOperator, Truth):
    operator_symbol = Symbols.EQUIVALENCE.value

    def truth(self, *args):
        fs = self.formulas
        N = len(fs)
        t = [f.truth(*args) for f in fs]
        # either all true or all false
        return all(t) or not any(t)

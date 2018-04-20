from abc import ABC, abstractmethod

from flloat.base.Formula import Formula, UnaryOperator, BinaryOperator
from flloat.base.Symbols import Symbols


class Truth(ABC):
    @abstractmethod
    def truth(self, *args) -> bool:
        raise NotImplementedError

class NotTruth(UnaryOperator, Truth):
    operator_symbol = Symbols.NOT.value
    def truth(self, *args):
        return not self.f.truth(*args)


class AndTruth(BinaryOperator, Truth):
    operator_symbol = Symbols.AND.value
    def truth(self, *args):
        return all(f.truth(*args) for f in self.formulas)

class OrTruth(BinaryOperator, Truth):
    operator_symbol = Symbols.OR.value
    def truth(self, *args):
        return any(f.truth(*args) for f in self.formulas)


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
            # if at some point the premise is False, then treturn True
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


# def __init__(self, formulas:OperatorChilds):
#     PLFormula.__init__(self)
#     BinaryOperator.__init__(self, formulas)
#
# # def _convert(self):
# #     fs = self.formulas
# #     N = len(fs)
# #     res = PLOr({PLNot(fs[0]), fs[1]})
# #     for i in range(2, N):
# #         res = PLOr({PLNot(res), fs[i]})
# #
# #     return res
#
# def truth(self, i:Interpretation):
#     # eq_formula = self._convert()
#     # return eq_formula.truth(i)
#     fs = self.formulas
#     N = len(fs)
#     evaluation = lambda x, y: not x or y
#     truth = evaluation(fs[0].truth(i), fs[1].truth(i))
#     for idx in range(2, N):
#         if not truth:
#             return True
#         else:
#             truth = evaluation(truth, fs[idx].truth(i))
#
#     return truth

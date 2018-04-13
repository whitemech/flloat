from typing import Iterable, Set

from flloat.base.Formula import Formula, CommutativeBinaryOperator, UnaryOperator, BinaryOperator, OperatorChilds, \
    CommOperatorChilds
from flloat.base.Interpretation import Interpretation
from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols


class PLFormula(Formula):
    pass


class PLCommBinaryOperator(PLFormula, CommutativeBinaryOperator):
    def __init__(self, formulas:CommOperatorChilds):
        PLFormula.__init__(self)
        CommutativeBinaryOperator.__init__(self, formulas)

    def __str__(self):
        return CommutativeBinaryOperator.__str__(self)

class PLAtomic(PLFormula):
    def __init__(self, s:Symbol):
        self.s = s

    def _members(self):
        return self.s

    def truth(self, i:Interpretation):
        return self.s in i

    def __str__(self):
        return str(self.s)


class PLTrue(PLAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.TRUE.value))

    def truth(self, i:Interpretation):
        return True

class PLFalse(PLAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.FALSE.value))

    def truth(self, i: Interpretation):
        return False



class PLNot(PLFormula, UnaryOperator):
    operator_symbol = "~"

    def __init__(self, f:Formula):
        PLFormula.__init__(self)
        UnaryOperator.__init__(self, f)

    def truth(self, i:Interpretation):
        return not self.f.truth(i)


class PLAnd(PLCommBinaryOperator):
    operator_symbol = "&"

    def truth(self, i:Interpretation):
        return all(f.truth(i) for f in self.formulas)


class PLOr(PLCommBinaryOperator):
    operator_symbol = "|"

    def truth(self, i:Interpretation):
        return any(f.truth(i) for f in self.formulas)


class PLImplies(PLFormula, BinaryOperator):
    operator_symbol = "->"

    def __init__(self, formulas:OperatorChilds):
        PLFormula.__init__(self)
        BinaryOperator.__init__(self, formulas)

    def _convert(self):
        fs = self.formulas
        N = len(fs)
        res = PLOr({PLNot(fs[0]), fs[1]})
        for i in range(2, N):
            res = PLOr({PLNot(res), fs[i]})

        return res

    def truth(self, i:Interpretation):
        # eq_formula = self._convert()
        # return eq_formula.truth(i)
        fs = self.formulas
        N = len(fs)
        evaluation = lambda x, y: not x or y
        truth = evaluation(fs[0].truth(i), fs[1].truth(i))
        for idx in range(2, N):
            if not truth:
                return True
            else:
                truth = evaluation(truth, fs[idx].truth(i))

        return truth


class PLEquivalence(PLCommBinaryOperator):
    operator_symbol = "<->"

    def truth(self, i: Interpretation):
        fs = self.formulas
        N = len(fs)
        t = [f.truth(i) for f in fs]
        # either all true or all false
        return all(t) or not any(t)



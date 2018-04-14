from abc import abstractmethod
from typing import Iterable, Set

from flloat.base.Formula import Formula, CommutativeBinaryOperator, UnaryOperator, BinaryOperator, OperatorChilds, \
    CommOperatorChilds, AtomicFormula
from flloat.base.nnf import NNF, NotNNF
from flloat.base.Interpretation import Interpretation
from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.truths import NotTruth, AndTruth, OrTruth, ImpliesTruth, EquivalenceTruth, Truth
from flloat.semantics.pl import PLInterpretation

class PLTruth(Truth):
    @abstractmethod
    def truth(self, i: PLInterpretation, *args):
        raise NotImplementedError

class PLFormula(Formula, Truth, NNF):
    pass

class PLCommBinaryOperator(PLFormula, CommutativeBinaryOperator):
    pass

class PLAtomic(PLFormula, AtomicFormula):
    def truth(self, i:PLInterpretation, *args):
        return self.s in i

    def to_nnf(self):
        return self

    def negate(self):
        return PLNot(self)

class PLTrue(PLAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.TRUE.value))

    def truth(self, i: PLInterpretation, *args):
        return True

    def negate(self):
        return PLFalse()

class PLFalse(PLAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.FALSE.value))

    def truth(self, i: PLInterpretation, *args):
        return False

    def negate(self):
        return PLTrue()


class PLNot(PLFormula, NotTruth, NotNNF):
    operator_symbol = "~"

class PLAnd(PLCommBinaryOperator, AndTruth):
    operator_symbol = "&"
    def to_nnf(self):
        childs = set([child.to_nnf() for child in self.formulas])
        return PLAnd(childs)

    def negate(self):
        childs = set([child.negate() for child in self.formulas])
        return PLOr(childs)

class PLOr(PLCommBinaryOperator, OrTruth):
    operator_symbol = "|"

    def to_nnf(self):
        childs = set([child.to_nnf() for child in self.formulas])
        return PLOr(childs)

    def negate(self):
        childs = set([child.negate() for child in self.formulas])
        return PLAnd(childs)


class PLImplies(PLFormula, ImpliesTruth):
    operator_symbol = "->"

    def _convert(self):
        fs = self.formulas
        a, b = PLAnd(set(fs[:-1])), fs[-1]
        res = PLOr({PLNot(a), b})
        return res

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate()

    def truth(self, *args):
        return self._convert().truth(*args)


class PLEquivalence(PLCommBinaryOperator, EquivalenceTruth):
    operator_symbol = "<->"

    def _convert(self):
        fs = self.formulas
        pos = PLAnd(set(fs))
        neg = PLAnd(set(PLNot(f) for f in fs))

        res = PLOr({pos, neg})
        return res

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate()

    def truth(self, *args):
        return self._convert().truth(*args)





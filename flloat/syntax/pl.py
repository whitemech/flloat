from abc import abstractmethod
from typing import Iterable, Set

from flloat.base.Formula import Formula, CommutativeBinaryOperator, UnaryOperator, BinaryOperator, OperatorChilds, \
    CommOperatorChilds, AtomicFormula
from flloat.base.nnf import NNF, NotNNF
from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.truths import NotTruth, AndTruth, OrTruth, ImpliesTruth, EquivalenceTruth, Truth
from flloat.semantics.pl import PLInterpretation
from flloat.utils import powerset


class PLTruth(Truth):
    @abstractmethod
    def truth(self, i: PLInterpretation, *args):
        raise NotImplementedError

class PLFormula(Formula, Truth, NNF):
    def all_models(self, alphabet: Set[Symbol]) -> Set[PLInterpretation]:
        """Find all the models of a given formula.
        Very trivial (and inefficient) algorithm: BRUTE FORCE on all the possible interpretations.
        """
        all_possible_interpretations = sorted(powerset(alphabet), key=len)
        models = set()
        for i in all_possible_interpretations:
            # compute current Interpretation, considering False
            # all propositional symbols not present in current interpretation
            current_interpretation = PLInterpretation(set(i))
            if self.truth(current_interpretation):
                models.add(current_interpretation)

        return models

    def minimal_models(self, alphabet: Set[Symbol]) -> Set[PLInterpretation]:
        """Find models of min size (i.e. the less number of proposition to True)."""
        models = self.all_models(alphabet)
        size2models = {}

        for m in models:
            size = len(m.true_propositions)
            if size not in size2models:
                size2models[size] = set()
            size2models[size].add(m)

        if not size2models:
            return set()
        else:
            return size2models[min(size2models.keys())]

    def __repr__(self):
        return self.__str__()

class PLBinaryOperator(PLFormula, BinaryOperator):
    pass

class PLCommBinaryOperator(PLBinaryOperator, CommutativeBinaryOperator):
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
    pass

class PLAnd(PLCommBinaryOperator, AndTruth):
    def to_nnf(self):
        childs = set([child.to_nnf() for child in self.formulas])
        return PLAnd(childs)

    def negate(self):
        childs = set([child.negate() for child in self.formulas])
        return PLOr(childs)

class PLOr(PLCommBinaryOperator, OrTruth):

    def to_nnf(self):
        childs = set([child.to_nnf() for child in self.formulas])
        return PLOr(childs)

    def negate(self):
        childs = set([child.negate() for child in self.formulas])
        return PLAnd(childs)


class PLImplies(PLBinaryOperator, ImpliesTruth):
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





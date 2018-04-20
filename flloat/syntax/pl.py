from abc import abstractmethod
from typing import Set

from flloat.base.Formula import Formula, CommutativeBinaryOperator, BinaryOperator, AtomicFormula
from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.convertible import ImpliesConvertible, EquivalenceConvertible
from flloat.base.nnf import NNF, NotNNF, DualBinaryOperatorNNF
from flloat.base.truths import NotTruth, AndTruth, OrTruth, Truth
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

    def find_labels(self):
        return {self.s}

class PLTrue(PLAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.TRUE.value))

    def truth(self, *args):
        return True

    def negate(self):
        return PLFalse()

    def find_labels(self):
        return set()

class PLFalse(PLAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.FALSE.value))

    def truth(self, *args):
        return False

    def negate(self):
        return PLTrue()

    def find_labels(self):
        return set()


class PLNot(PLFormula, NotTruth, NotNNF):
    pass


class PLOr(PLCommBinaryOperator, OrTruth, DualBinaryOperatorNNF):
    pass

class PLAnd(PLCommBinaryOperator, AndTruth, DualBinaryOperatorNNF):
    pass


class PLImplies(PLBinaryOperator, ImpliesConvertible):
    And = PLAnd
    Or = PLOr
    Not = PLNot

class PLEquivalence(PLCommBinaryOperator, EquivalenceConvertible):
    And = PLAnd
    Or = PLOr
    Not = PLNot


PLOr.Dual = PLAnd
PLAnd.Dual = PLOr

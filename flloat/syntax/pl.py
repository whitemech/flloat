from abc import abstractmethod
from typing import Set

from flloat.base.Formula import Formula, CommutativeBinaryOperator, BinaryOperator, AtomicFormula
from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.convertible import ImpliesConvertible, EquivalenceConvertible
from flloat.base.nnf import NNF, NotNNF, DualBinaryOperatorNNF, DualCommutativeOperatorNNF
from flloat.base.truths import NotTruth, AndTruth, OrTruth, Truth
from flloat.semantics.pl import PLInterpretation
from flloat.utils import powerset, _powerset


class PLTruth(Truth):
    @abstractmethod
    def truth(self, i: PLInterpretation, *args):
        raise NotImplementedError

class PLFormula(Formula, Truth, NNF):
    def __init__(self):
        Formula.__init__(self)
        NNF.__init__(self)

    def all_models(self, alphabet: Set[Symbol]) -> Set[PLInterpretation]:
        """Find all the models of a given formula.
        Very trivial (and inefficient) algorithm: BRUTE FORCE on all the possible interpretations.
        """
        all_possible_interpretations = _powerset(alphabet)
        for i in all_possible_interpretations:
            # compute current Interpretation, considering False
            # all propositional symbols not present in current interpretation
            current_interpretation = PLInterpretation(i)
            if self.truth(current_interpretation):
                yield current_interpretation


    def minimal_models(self, alphabet: Set[Symbol]) -> Set[PLInterpretation]:
        """Find models of min size (i.e. the less number of proposition to True)."""

        models = list(self.all_models(alphabet))

        minimal_models = set()
        for m in models:
            min_m = m
            for m1 in models:
                if min_m.true_propositions.issuperset(m1.true_propositions):
                    min_m = m1
            minimal_models.add(min_m)


        return minimal_models

    def __repr__(self):
        return self.__str__()

    def find_atomics(self):
        if hasattr(self, "atoms"):
            return getattr(self, "atoms")
        res = self._find_atomics()
        setattr(self, "atoms", res)
        return res

    @abstractmethod
    def _find_atomics(self):
        raise NotImplementedError

class PLBinaryOperator(PLFormula, BinaryOperator):
    def __init__(self, formulas):
        PLFormula.__init__(self)
        BinaryOperator.__init__(self, formulas)

    def _find_atomics(self):
        res = set()
        for subf in self.formulas:
            try:
                res = res.union(subf.find_atomics())
            except:
                res.add(subf)
        return res


class PLCommBinaryOperator(PLBinaryOperator, DualCommutativeOperatorNNF):
    def __init__(self, formulas):
        PLBinaryOperator.__init__(self, formulas)
        DualCommutativeOperatorNNF.__init__(self, formulas)


class PLAtomic(PLFormula, AtomicFormula):
    def __init__(self, s):
        PLFormula.__init__(self)
        AtomicFormula.__init__(self, s)

    def truth(self, i:PLInterpretation, *args):
        return self.s in i

    def _to_nnf(self):
        return self

    def negate(self):
        return PLNot(self)

    def find_labels(self):
        return {self.s}

    def _find_atomics(self):
        return {self}

class PLTrue(PLAtomic):
    def __init__(self):
        PLAtomic.__init__(self, Symbol(Symbols.TRUE.value))

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
    def __init__(self, f):
        PLFormula.__init__(self)
        NotTruth.__init__(self, f)

    def _find_atomics(self):
        try:
            return self.f.find_atomics()
        except:
            return self.f



class PLOr(PLCommBinaryOperator, OrTruth):
    pass

class PLAnd(PLCommBinaryOperator, AndTruth):
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

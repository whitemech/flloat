from abc import abstractmethod
from functools import lru_cache
from typing import Set


from flloat.base.Alphabet import _Alphabet, Alphabet
from flloat.base.Formula import Formula, CommutativeBinaryOperator, BinaryOperator, AtomicFormula
from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.convertible import ImpliesConvertible, EquivalenceConvertible
from flloat.base.nnf import NNF, NotNNF, DualBinaryOperatorNNF, DualCommutativeOperatorNNF
from flloat.base.truths import NotTruth, AndTruth, OrTruth, Truth
from flloat.semantics.pl import PLInterpretation
from flloat.utils import powerset, _powerset, MAX_CACHE_SIZE


class PLTruth(Truth):
    @abstractmethod
    def truth(self, i: PLInterpretation, *args):
        raise NotImplementedError

class PLFormula(Formula, PLTruth, NNF):
    def __init__(self):
        Formula.__init__(self)

        self._all_models = None
        self._minimal_models = None
        self._atoms = None

    def all_models(self, alphabet: _Alphabet) -> Set[PLInterpretation]:
        """Find all the possible interpretations given a set of symbols"""

        all_possible_interpretations = alphabet.powerset().symbols
        all_models = set()
        for i in all_possible_interpretations:
            # compute current Interpretation, considering False
            # all propositional symbols not present in current interpretation
            current_interpretation = PLInterpretation(i)
            if self.truth(current_interpretation):
                all_models.add(current_interpretation)

        self._all_models = all_models
        return all_models

    @lru_cache(maxsize=MAX_CACHE_SIZE)
    def minimal_models(self, alphabet: _Alphabet) -> Set[PLInterpretation]:
        """Find models of min size (i.e. the less number of proposition to True).
        Very trivial (and inefficient) algorithm: BRUTE FORCE on all the possible interpretations."""
        models = self.all_models(alphabet)

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
        if self._atoms is None:
            self._atoms = self._find_atomics()
        return self._atoms

    @abstractmethod
    def _find_atomics(self):
        raise NotImplementedError

class PLBinaryOperator(BinaryOperator, PLFormula):
    # def __init__(self, formulas):
    #     PLFormula.__init__(self)
    #     BinaryOperator.__init__(self, formulas)

    def _find_atomics(self):
        res = set()
        for subf in self.formulas:
            try:
                res = res.union(subf.find_atomics())
            except:
                res.add(subf)
        return res

class PLCommBinaryOperator(DualCommutativeOperatorNNF, PLFormula):
    # def __init__(self, formulas):
    #     PLFormula.__init__(self)
    #     DualCommutativeOperatorNNF.__init__(self, formulas)

    def _find_atomics(self):
        res = set()
        for subf in self.formulas:
            try:
                res = res.union(subf.find_atomics())
            except:
                res.add(subf)
        return res


class PLAtomic(AtomicFormula, PLFormula):
    # def __init__(self, s):
    #     PLFormula.__init__(self)
    #     AtomicFormula.__init__(self, s)

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
        PLAtomic.__init__(self, Symbol(Symbols.FALSE.value))

    def truth(self, *args):
        return False

    def negate(self):
        return PLTrue()

    def find_labels(self):
        return set()


class PLNot(NotTruth, PLFormula, NotNNF):

    def _find_atomics(self):
        # try:
        #     return self.f.find_atomics()
        # except:
        #     return self.f
        return self.f.find_atomics()


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

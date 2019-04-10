from abc import abstractmethod, ABC
from functools import lru_cache
from typing import Set


from flloat.base.convertible import ImpliesConvertible, EquivalenceConvertible
from flloat.base.formulas import Formula, BinaryOperator, AtomicFormula
from flloat.base.nnf import NNF, NotNNF, DualCommutativeOperatorNNF
from flloat.base.symbols import _Alphabet, Symbol, Symbols
from flloat.base.truths import NotTruth, AndTruth, OrTruth, Truth
from flloat.semantics.pl import PLInterpretation
from flloat.helpers import MAX_CACHE_SIZE


class PLTruth(Truth, ABC):

    @abstractmethod
    def truth(self, i: PLInterpretation, *args) -> bool:
        """
        Tell if the formula is true under a propositional interpretation.

        :param i: the propositional interpretation.
        :return: True if the formula is true under the interpretation i, False otherwise.
        """


class PLFormula(Formula, PLTruth, NNF):
    """A class to represent propositional formulas."""

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

    def find_atomics(self) -> Set[AtomicFormula]:
        """
        Find all the atomic formulas (i.e. the leaves in the syntax tree.) in the propositional formulas.
        :return: the set of  atomic formulas.
        """
        if self._atoms is None:
            self._atoms = self._find_atomics()
        return self._atoms

    @abstractmethod
    def _find_atomics(self) -> Set[AtomicFormula]:
        """Find all the atomic formulas in the propositional formulas."""


class PLBinaryOperator(BinaryOperator, PLFormula):
    """A class to represent propositional binary formulas."""

    def _find_atomics(self):
        res = set()
        for subf in self.formulas:
            try:
                res = res.union(subf.find_atomics())
            except:
                res.add(subf)
        return res


class PLCommBinaryOperator(DualCommutativeOperatorNNF, PLFormula):
    """A class to represent propositional binary formulas of a commutative operator."""

    def _find_atomics(self):
        res = set()
        for subf in self.formulas:
            try:
                res = res.union(subf.find_atomics())
            except:
                res.add(subf)
        return res


class PLAtomic(AtomicFormula, PLFormula):
    """A class to represent propositional atomic formulas."""

    def truth(self, i: PLInterpretation, *args) -> bool:
        return self.s in i

    def _to_nnf(self) -> 'PLAtomic':
        return self

    def negate(self) -> 'PLNot':
        return PLNot(self)

    def find_labels(self) -> Set[Symbol]:
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

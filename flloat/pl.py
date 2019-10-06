# -*- coding: utf-8 -*-
from abc import abstractmethod, ABC
from functools import lru_cache
from typing import Set

from flloat.base.convertible import ImpliesConvertible, EquivalenceConvertible
from flloat.base.formulas import Formula, BinaryOperator, AtomicFormula, UnaryOperator
from flloat.base.nnf import NNF, NotNNF, DualCommutativeOperatorNNF, AtomicNNF
from flloat.base.symbols import Alphabet, Symbol, Symbols
from flloat.base.truths import NotTruth, AndTruth, OrTruth, Truth
from flloat.helpers import MAX_CACHE_SIZE, _powerset
from flloat.semantics.pl import PLInterpretation


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

    def all_models(self, alphabet: Alphabet) -> Set[PLInterpretation]:
        """Find all the possible interpretations given a set of symbols"""

        all_possible_interpretations = _powerset(alphabet)
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
    def minimal_models(self, alphabet: Alphabet) -> Set[PLInterpretation]:
        """
        Find models of min size (i.e. the less number of proposition to True).

        Very trivial (and inefficient) algorithm: BRUTE FORCE on all the possible interpretations.
        """
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
        Find all the atomic formulas in the propositional formulas.

        That is, find the leaves in the syntax tree.

        :return: the set of  atomic formulas.
        """
        if self._atoms is None:
            self._atoms = self._find_atomics()
        return self._atoms

    @abstractmethod
    def _find_atomics(self) -> Set[AtomicFormula]:
        """Find all the atomic formulas in the propositional formulas."""


class PLAtomic(AtomicFormula, AtomicNNF, PLFormula):
    """A class to represent propositional atomic formulas."""

    def truth(self, i: PLInterpretation, *args) -> bool:
        return self.s in i

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return {self.s}

    def _find_atomics(self):
        return {self}


class PLUnaryOperator(UnaryOperator, PLFormula):
    """A class to represent propositional unary operators."""

    def _find_atomics(self):
        return self.f.find_atomics()


class PLBinaryOperator(BinaryOperator, PLFormula):
    """A class to represent propositional binary formulas."""

    def _find_atomics(self):
        res = set()
        for subf in self.formulas:
            res = res.union(subf.find_atomics())
        return res


class PLCommBinaryOperator(DualCommutativeOperatorNNF, PLBinaryOperator):
    """A class to represent propositional binary formulas of a commutative operator."""


class PLTrue(PLAtomic):
    """Propositional true."""

    def __init__(self):
        PLAtomic.__init__(self, Symbols.TRUE.value)

    def truth(self, *args) -> bool:
        return True

    def negate(self) -> "PLFalse":
        return PLFalse()

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return set()


class PLFalse(PLAtomic):
    """Propositional false."""

    def __init__(self):
        PLAtomic.__init__(self, Symbols.FALSE.value)

    def truth(self, *args) -> bool:
        return False

    def negate(self) -> "PLTrue":
        return PLTrue()

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return set()


class PLNot(PLUnaryOperator, NotNNF, NotTruth):
    """Propositional Not."""


class PLOr(PLCommBinaryOperator, OrTruth):
    """Propositional Or"""


class PLAnd(PLCommBinaryOperator, AndTruth):
    """Propositional And"""


class PLImplies(PLBinaryOperator, ImpliesConvertible):
    """Propositional Implication"""

    And = PLAnd
    Or = PLOr
    Not = PLNot


class PLEquivalence(PLCommBinaryOperator, EquivalenceConvertible):
    """Propositional Equivalence"""

    And = PLAnd
    Or = PLOr
    Not = PLNot


PLAtomic.Not = PLNot
PLOr.Dual = PLAnd
PLAnd.Dual = PLOr

# -*- coding: utf-8 -*-
from abc import abstractmethod, ABC
from typing import Set, Any, Dict, Optional

import sympy
from pythomata import PropositionalInterpretation as PropInt
from sympy.logic.boolalg import Boolean, BooleanTrue, BooleanFalse

from flloat.base.convertible import ImpliesConvertible, EquivalenceConvertible
from flloat.base.formulas import Formula, BinaryOperator, AtomicFormula, UnaryOperator
from flloat.base.nnf import NNF, NotNNF, DualCommutativeOperatorNNF, AtomicNNF
from flloat.base.symbols import Symbols, Symbol
from flloat.base.truths import NotTruth, AndTruth, OrTruth, Truth


class PLTruth(Truth, ABC):
    @abstractmethod
    def truth(self, i: PropInt, *args) -> bool:
        """
        Tell if the formula is true under a propositional interpretation.

        :param i: the propositional interpretation.
        :return: True if the formula is true under the interpretation i, False otherwise.
        """


class PLFormula(Formula, PLTruth, NNF):
    """A class to represent propositional formulas."""

    def __init__(self):
        Formula.__init__(self)
        self._atoms = None  # type: Optional[Set[PLAtomic]]

    def __repr__(self):
        return str(self)

    def find_atomics(self) -> Set["PLAtomic"]:
        """
        Find all the atomic formulas in the propositional formulas.

        That is, find the leaves in the syntax tree.

        :return: the set of  atomic formulas.
        """
        if self._atoms is None:
            self._atoms = self._find_atomics()
        return self._atoms

    @abstractmethod
    def _find_atomics(self) -> Set["PLAtomic"]:
        """Find all the atomic formulas in the propositional formulas."""


def to_sympy(
    formula: PLFormula, replace: Optional[Dict[Symbol, Symbol]] = None
) -> Boolean:
    """
    Translate a PLFormula object into a SymPy expression.

    :param formula: the formula to translate.
    :param replace: an optional mapping from symbols to replace to other replacement symbols.
    :return: the SymPy formula object equivalent to the formula.
    """
    if replace is None:
        replace = {}

    if isinstance(formula, PLTrue):
        return BooleanTrue()
    elif isinstance(formula, PLFalse):
        return BooleanFalse()
    elif isinstance(formula, PLAtomic):
        symbol = replace.get(formula.s, formula.s)
        return sympy.Symbol(symbol)
    elif isinstance(formula, PLNot):
        return sympy.Not(to_sympy(formula.f), replace=replace)
    elif isinstance(formula, PLOr):
        return sympy.simplify(
            sympy.Or(*map(to_sympy, formula.formulas)), replace=replace
        )
    elif isinstance(formula, PLAnd):
        return sympy.simplify(
            sympy.And(*map(to_sympy, formula.formulas)), replace=replace
        )
    elif isinstance(formula, PLImplies):
        if len(formula.formulas) == 2:
            return sympy.simplify(sympy.Implies(*formula.formulas), replace=replace)
        else:
            return sympy.simplify(
                sympy.Implies(
                    formula.formulas[0], to_sympy(PLImplies(formula.formulas[1:]))
                ),
                replace=replace,
            )
    elif isinstance(formula, PLEquivalence):
        return sympy.simplify(sympy.Equivalent(*formula.formulas), replace=replace)
    else:
        raise ValueError("Formula is not valid.")


class PLAtomic(AtomicFormula, AtomicNNF, PLFormula):
    """A class to represent propositional atomic formulas."""

    def truth(self, i: PropInt, *args) -> bool:
        return i.get(self.s, False)

    def find_labels(self) -> Set[Any]:
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

    def find_labels(self) -> Set[Any]:
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

    def find_labels(self) -> Set[Any]:
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

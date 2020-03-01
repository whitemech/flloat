# -*- coding: utf-8 -*-
import functools
from abc import abstractmethod, ABC
from typing import Set, Any, Dict, Optional

import sympy
from pythomata import (
    PropositionalInterpretation as PropInt,
    PropositionalInterpretation,
)
from sympy.logic.boolalg import Boolean, BooleanTrue, BooleanFalse

from flloat.base import (
    Formula,
    PropositionalTruth,
    AtomicFormula,
    BinaryOperator,
    UnaryOperator,
)
from flloat.symbols import Symbol, Symbols


class PLFormula(Formula, PropositionalTruth):
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

    @abstractmethod
    def negate(self) -> "PLFormula":
        """Negate the formula. Used by 'to_nnf'."""


def to_sympy(
    formula: Formula, replace: Optional[Dict[Symbol, sympy.Symbol]] = None
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
        return sympy.Not(to_sympy(formula.f, replace=replace))
    elif isinstance(formula, PLOr):
        return sympy.simplify(
            sympy.Or(*[to_sympy(f, replace=replace) for f in formula.formulas])
        )
    elif isinstance(formula, PLAnd):
        return sympy.simplify(
            sympy.And(*[to_sympy(f, replace=replace) for f in formula.formulas])
        )
    elif isinstance(formula, PLImplies):
        return sympy.simplify(
            sympy.Implies(*[to_sympy(f, replace=replace) for f in formula.formulas])
        )
    elif isinstance(formula, PLEquivalence):
        return sympy.simplify(
            sympy.Equivalent(*[to_sympy(f, replace=replace) for f in formula.formulas])
        )
    else:
        raise ValueError("Formula is not valid.")


class PLAtomic(AtomicFormula, PLFormula):
    """A class to represent propositional atomic formulas."""

    def truth(self, i: PropInt, *args) -> bool:
        return i.get(self.s, False)

    def find_labels(self) -> Set[Any]:
        """Return the set of symbols."""
        return {self.s}

    def _find_atomics(self):
        return {self}

    def negate(self) -> PLFormula:
        return PLNot(self)


class PLBinaryOperator(BinaryOperator[PLFormula], PLFormula, ABC):
    """An operator for Propositional Logic."""

    def to_nnf(self):
        return type(self)([f.to_nnf() for f in self.formulas])

    def _find_atomics(self) -> Set[PLAtomic]:
        return functools.reduce(
            set.union, [f.find_atomics() for f in self.formulas] # type: ignore
        )


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

    def to_nnf(self):
        """Convert to NNF."""
        return self


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

    def to_nnf(self):
        return self


class PLNot(UnaryOperator[PLFormula], PLFormula):
    """Propositional Not."""

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.NOT.value

    def truth(self, i: PropositionalInterpretation):
        return not self.f.truth(i)

    def to_nnf(self):
        if not isinstance(self.f, AtomicFormula):
            return self.f.negate().to_nnf()
        else:
            return self

    def negate(self) -> PLFormula:
        return self.f

    def _find_atomics(self) -> Set["PLAtomic"]:
        return self.f.find_atomics()


class PLOr(PLBinaryOperator):
    """Propositional Or"""

    @property
    def operator_symbol(self) -> Symbol:
        """Get the operator symbol."""
        return Symbols.OR.value

    def truth(self, i: PropositionalInterpretation):
        return any(f.truth(i) for f in self.formulas)

    def to_nnf(self):
        return PLOr([f.to_nnf() for f in self.formulas])

    def negate(self) -> PLFormula:
        return PLAnd([f.negate() for f in self.formulas])


class PLAnd(PLBinaryOperator):
    """Propositional And"""

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.AND.value

    def truth(self, i: PropositionalInterpretation):
        return all(f.truth(i) for f in self.formulas)

    def to_nnf(self):
        return PLAnd([f.to_nnf() for f in self.formulas])

    def negate(self) -> PLFormula:
        return PLOr([f.negate() for f in self.formulas])


class PLImplies(PLBinaryOperator):
    """Propositional Implication"""

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.IMPLIES.value

    def truth(self, i: PropositionalInterpretation):
        return self.to_nnf().truth(i)

    def negate(self) -> PLFormula:
        return self.to_nnf().negate()

    def to_nnf(self):
        first, second = self.formulas[0:2]
        final_formula = PLOr([PLNot(first).to_nnf(), second.to_nnf()])
        for subformula in self.formulas[2:]:
            final_formula = PLOr([PLNot(final_formula).to_nnf(), subformula.to_nnf()])
        return final_formula


class PLEquivalence(PLBinaryOperator):
    """Propositional Equivalence"""

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.EQUIVALENCE.value

    def truth(self, i: PropositionalInterpretation):
        return self.to_nnf().truth(i)

    def to_nnf(self):
        fs = self.formulas
        pos = PLAnd(fs)
        neg = PLAnd([PLNot(f) for f in fs])
        res = PLOr([pos, neg]).to_nnf()
        return res

    def negate(self) -> PLFormula:
        return self.to_nnf().negate()

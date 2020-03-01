# -*- coding: utf-8 -*-

"""Base classes for the implementation of a generic syntax tree."""
from abc import abstractmethod, ABC
from typing import Sequence, Set, Tuple

from pythomata import PropositionalInterpretation

from flloat.symbols import Symbol, Symbols
from flloat.helpers import Hashable

FiniteTrace = Sequence[PropositionalInterpretation]


class Formula(Hashable, ABC):
    """Abstract class for a formula."""

    @abstractmethod
    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""

    def simplify(self) -> "Formula":
        """Simplify the formula."""
        return self

    def to_nnf(self):
        """Transform the formula in NNF."""
        return self

    @abstractmethod
    def negate(self) -> "Formula":
        """Negate the formula. Used by 'to_nnf'."""


class AtomicFormula(Formula, ABC):
    """An abstract atomic formula."""

    def __init__(self, s: Symbol):
        """
        Inintialize the atomic formula.

        :param s: the atomic symbol.
        """
        super().__init__()
        self.s = s

    def _members(self):
        return self.s

    def __str__(self):
        """Get the string representation."""
        return str(self.s)

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return {self.s}


class Operator(Formula, ABC):
    """Implements an operator."""

    base_expression = (
        Symbols.ROUND_BRACKET_LEFT.value + "%s" + Symbols.ROUND_BRACKET_RIGHT.value
    )

    @property
    @abstractmethod
    def operator_symbol(self) -> Symbol:
        """Get the symbol of the operator."""


class UnaryOperator(Operator, ABC):
    """A class to represent unary operator."""

    def __init__(self, f: Formula):
        """
        Instantiate the unary operator over a formula.

        :param f: the formula on which the operator is applied.
        """
        super().__init__()
        self.f = f.simplify()

    def __str__(self):
        """Get the string representation."""
        return (
            str(self.operator_symbol)
            + Symbols.ROUND_BRACKET_LEFT.value
            + str(self.f)
            + Symbols.ROUND_BRACKET_RIGHT.value
        )

    def _members(self):
        return self.operator_symbol, self.f

    def __lt__(self, other):
        """Compare the formula with another formula."""
        return self.f.__lt__(other.f)

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return self.f.find_labels()


OperatorChildren = Sequence[Formula]


class BinaryOperator(Operator, ABC):
    """A generic binary formula."""

    def __init__(self, formulas: OperatorChildren):
        """
        Initialize the binary operator.

        :param formulas: the children formulas of the operator.
        """
        super().__init__()
        assert len(formulas) >= 2
        self.formulas = tuple(formulas)  # type: OperatorChildren

    def __str__(self):
        """Return the string representation."""
        return (
            "("
            + (" " + str(self.operator_symbol) + " ").join(map(str, self.formulas))
            + ")"
        )

    def _members(self) -> Tuple[Symbol, OperatorChildren]:
        return self.operator_symbol, self.formulas

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return set.union(*map(lambda f: f.find_labels(), self.formulas))


class PropositionalTruth:
    """Interface for propositional formulas."""

    @abstractmethod
    def truth(self, i: PropositionalInterpretation) -> bool:
        """
        Return the truth evaluation of the formula wrt the propositional interpretation.

        :param i: the propositional interpretation.
        :return: True if the formula is satisfied by the interpretation, False otherwise.
        """


class FiniteTraceTruth:
    """Interface for formulas that support the trace semantics."""

    @abstractmethod
    def truth(self, i: FiniteTrace, pos: int) -> bool:
        """
        Return the truth evaluation of the formula wrt the trace.

        :param i: the trace.
        :param pos: the position from where to start the evaluation
        :return: True if the formula is satisfied by the trace, False otherwise.
        """


class RegExpTruth:
    """Interface for regular expression semantics."""

    @abstractmethod
    def truth(self, tr: FiniteTrace, start: int = 0, end: int = 0) -> bool:
        """
        Return the truth evaluation of the regex wrt the trace.

        :param tr: the trace.
        :param start: the start index.
        :param end: the end index.
        :return: True if the regex is satisfied by the trace, False otherwise.
        """

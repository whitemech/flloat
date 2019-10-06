# -*- coding: utf-8 -*-

"""Base classes for the implementation of a generic syntax tree."""
from abc import abstractmethod, ABC
from typing import Sequence, Set, Tuple, cast

from flloat.base.symbols import Symbol, Symbols
from flloat.helpers import Hashable


class Formula(Hashable, ABC):
    """Abstract class for a formula."""

    @abstractmethod
    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""

    def simplify(self) -> 'Formula':
        """Simplify the formula."""
        return self


class AtomicFormula(Formula):
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
CommOperatorChildren = Set[Formula]


class BinaryOperator(Operator, ABC):
    """A generic binary formula."""

    def __init__(self, formulas: OperatorChildren):
        """
        Initialize the binary operator.

        :param formulas: the children formulas of the operator.
        """
        super().__init__()
        assert len(formulas) >= 2
        self.formulas = tuple(formulas)
        self.formulas = self._popup()

    def __str__(self):
        """Return the string representation."""
        return (
            "(" + (" " + str(self.operator_symbol) + " ").join(map(str, self.formulas)) + ")"
        )

    def _members(self) -> Tuple[Symbol, OperatorChildren]:
        return self.operator_symbol, self.formulas

    def _popup(self) -> OperatorChildren:
        """
        Refactor the binary formula.

        That is, find recursively commutative binary operator
        among child formulas and pop up them at the same level.
        """
        res = []
        for child in self.formulas:
            if type(child) == type(self):
                superchilds = cast(BinaryOperator, child).formulas
                res.extend(superchilds)
            else:
                res.append(child)
        return tuple(res)

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return set.union(*map(lambda f: f.find_labels(), self.formulas))


class CommutativeBinaryOperator(BinaryOperator, ABC):
    """A generic commutative binary formula."""

    def __init__(self, formulas: OperatorChildren, idempotence: bool = True):
        """
        Instantiate a commutative binary operator.

        :param formulas: a sequence of sub-formulas concatenated with the binary operator.
        :param idempotence: whether the binary operator satisfies
                          | the idempotence property (e.g. A & A === A)
        """
        super().__init__(formulas)
        self.idempotence = idempotence
        if idempotence:
            # order does not matter -> set operation
            # remove duplicates -> set operation
            self.formulas_set = frozenset(self.formulas)
            # unique representation -> sorting
            self.members = tuple(sorted(self.formulas_set, key=lambda x: hash(x)))

    def simplify(self) -> Formula:
        """
        Simplify the formula.

        :return: the simplified formula.
        """
        if self.idempotence:
            if len(self.formulas_set) == 1:
                return next(iter(self.formulas_set)).simplify()
            else:
                return type(self)(self.members)
        else:
            return self

    def _members(self):
        if self.idempotence:
            return self.operator_symbol, self.members
            # return (self.operator_symbol, self.formulas_set)
        else:
            return super()._members()

    def find_labels(self):
        """Return the set of symbols."""
        return set.union(*map(lambda f: f.find_labels(), self.formulas))

    def __str__(self):
        """Return the string representation."""
        if self.idempotence:
            return (
                "("
                + (" " + str(self.operator_symbol) + " ").join(map(str, self.members))
                + ")"
            )
        else:
            return super().__str__()

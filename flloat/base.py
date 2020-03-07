# -*- coding: utf-8 -*-

"""Base classes for the implementation of a generic syntax tree."""
from abc import abstractmethod, ABC
from typing import Sequence, Set, Tuple, TypeVar, Generic, cast, Union
import re

from pythomata import PropositionalInterpretation

from flloat.symbols import Symbols, OpSymbol
from flloat.helpers import Hashable

FiniteTrace = Sequence[PropositionalInterpretation]
AtomSymbol = Union["QuotedFormula", str]


class Formula(Hashable, ABC):
    """Abstract class for a formula."""

    @abstractmethod
    def find_labels(self) -> Set[AtomSymbol]:
        """Return the set of symbols."""

    def to_nnf(self) -> "Formula":
        """Transform the formula in NNF."""
        return self

    @abstractmethod
    def negate(self) -> "Formula":
        """Negate the formula. Used by 'to_nnf'."""


class AtomicFormula(Formula, ABC):
    """An abstract atomic formula.

    Both formulae and names can be used as atomic symbols.
    A name must be a string made of letters, numbers, underscores, or it must
    be a quoted string.
    """

    name_regex = re.compile(r'(\w+)|(".*")')

    def __init__(self, s: Union[AtomSymbol, Formula]):
        """Inintializes the atomic formula.

        :param s: the atomic symbol. Formulae are implicitly converted to
            quoted formulae.
        """
        super().__init__()

        # If formula
        if isinstance(s, Formula):
            self.s = QuotedFormula(s)  # type: AtomSymbol

        # If name
        else:
            self.s = str(s)
            if not self.name_regex.fullmatch(self.s):
                raise ValueError(
                    "The symbol name does not respect the naming convention."
                )

    def _members(self):
        return self.s

    def __str__(self):
        """Get the string representation."""
        return str(self.s)

    def find_labels(self) -> Set[AtomSymbol]:
        """Return the set of symbols."""
        return {self.s}


class QuotedFormula(Hashable):
    """This object is a constant representation of a formula.

    This can be used as a normal formula. Quoted formulas can also be used as
    hashable objects and for atomic symbols.
    """

    _mutable = ["_hash"]

    def __init__(self, f: Formula):
        """Initialize.

        :param f: formula to represent.
        """
        super().__init__()
        self.__dict__["_QuotedFormula__f"] = f
        self.__dict__["_QuotedFormula__str"] = '"' + str(f) + '"'

    def _members(self) -> Formula:
        return self.__f

    def __str__(self):
        """Quoted formula."""
        return self.__str

    def __repr__(self):
        """Quoted formula."""
        return str(self)

    def __getattr__(self, attrname):
        """Redirect to Formula."""
        return getattr(self.__f, attrname)

    def __setattr__(self, attr, value):
        """If immutable, raises an error."""
        if attr in self._mutable:
            self.__dict__[attr] = value
        else:
            raise AttributeError("Can't modify: immutable object.")

    def __delattr__(self, attr):
        """Raise an error, because del is not supported."""
        raise AttributeError("Can't modify: immutable object.")

    def __dir__(self):
        """Expose the same interface as wrapped."""
        members = set(dir(self.__f)).union(object.__dir__(self))
        return sorted(members)

    @property
    def wrapped(self) -> Formula:
        """Return the wrapped Formula."""
        return self.__f


class Operator(Formula, ABC):
    """Implements an operator."""

    base_expression = (
        Symbols.ROUND_BRACKET_LEFT.value + "%s" + Symbols.ROUND_BRACKET_RIGHT.value
    )

    @property
    @abstractmethod
    def operator_symbol(self) -> OpSymbol:
        """Get the symbol of the operator."""


T = TypeVar("T")
OperatorChildren = Sequence[T]


class UnaryOperator(Generic[T], Operator, ABC):
    """A class to represent unary operator."""

    def __init__(self, f: T):
        """
        Instantiate the unary operator over a formula.

        :param f: the formula on which the operator is applied.
        """
        super().__init__()
        self.f = f

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

    def find_labels(self) -> Set[AtomSymbol]:
        """Return the set of symbols."""
        return cast(Formula, self.f).find_labels()


class BinaryOperator(Generic[T], Operator, ABC):
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

    def _members(self) -> Tuple[OpSymbol, OperatorChildren]:
        return self.operator_symbol, self.formulas

    def find_labels(self) -> Set[AtomSymbol]:
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
    def truth(self, i: FiniteTrace, pos: int = 0) -> bool:
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

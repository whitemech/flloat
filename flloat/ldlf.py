# -*- coding: utf-8 -*-
from abc import abstractmethod, ABC
from typing import Set, cast, Sequence

from pythomata import PropositionalInterpretation

from flloat.base import (
    Formula,
    FiniteTraceTruth,
    UnaryOperator,
    BinaryOperator,
    RegExpTruth,
    AtomicFormula,
    FiniteTrace,
)
from flloat.delta import Delta
from flloat.flloat import to_automaton
from flloat.pl import PLFormula, PLTrue, PLFalse, PLAnd, PLOr, PLAtomic
from flloat.symbols import Symbol, Symbols


class LDLfFormula(Formula, FiniteTraceTruth, Delta, ABC):

    def to_nnf(self) -> "LDLfFormula":
        """Transform the formula in NNF."""
        return self

    @abstractmethod
    def negate(self) -> "LDLfFormula":
        """Negate the formula. Used by 'to_nnf'."""

    def delta(self, i: PropositionalInterpretation, epsilon=False) -> PLFormula:
        f = self.to_nnf()
        d = f._delta(i, epsilon=epsilon)
        if epsilon:
            # By definition, if epsilon=True, then the result must be either True or False.
            # The output is a Propositional Formula with only True or False as atomics.
            # Hence, we just evaluate the formula with the empty propositional interpretation.
            d = PLTrue() if d.truth({}) else PLFalse()
        return d

    @abstractmethod
    def _delta(self, i: PropositionalInterpretation, epsilon=False) -> "PLFormula":
        """apply delta function, assuming that 'self' is a LDLf formula in Negative Normal Form"""
        raise NotImplementedError

    def __repr__(self):
        return self.__str__()

    def to_automaton(self):
        return to_automaton(self)


class DeltaRegExp(ABC):
    @abstractmethod
    def delta_diamond(
        self, f: LDLfFormula, i: PropositionalInterpretation, epsilon: bool = False
    ):
        """
        Delta interface for regular expressions in the diamond operator.

        :param f: the LDLf formula.
        :param i: the propositional interpretation.
        :param epsilon: evaluate on empty trace or not.
        :return: a propositional formula.
        """

    @abstractmethod
    def delta_box(self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False):
        """
        Delta interface for regular expressions in the box operator.

        :param f: the LDLf formula.
        :param i: the propositional interpretation.
        :param epsilon: evaluate on empty trace or not.
        :return: a propositional formula.
        """


class RegExpFormula(Formula, RegExpTruth, DeltaRegExp, ABC):
    """Regular Expression formula."""

    def negate(self) -> Formula:
        """Negate should not be called for regular expressions."""
        raise ValueError


class LDLfUnaryOperator(UnaryOperator[LDLfFormula], LDLfFormula, ABC):
    def __init__(self, f: LDLfFormula):
        super().__init__(f)
        self.f = cast(LDLfFormula, self.f)


class LDLfBinaryOperator(BinaryOperator[LDLfFormula], LDLfFormula, ABC):
    """A binary operator for LDLf."""

    def __init__(self, formulas: Sequence[LDLfFormula]):
        super().__init__(formulas)
        self.formulas = cast(Sequence[LDLfFormula], self.formulas)


class LDLfTemporalFormula(LDLfFormula, ABC):
    @property
    @abstractmethod
    def temporal_brackets(self) -> str:
        """The bracket symbols for the temporal operator."""

    def __init__(self, r: RegExpFormula, f: LDLfFormula):
        super().__init__()
        self.r = r
        self.f = f

    def _members(self):
        return self.temporal_brackets, self.r, self.f

    def __str__(self):
        return (
            self.temporal_brackets[0]
            + str(self.r)
            + self.temporal_brackets[1]
            + "("
            + str(self.f)
            + ")"
        )

    def find_labels(self) -> Set[Symbol]:
        return self.f.find_labels().union(self.r.find_labels())

    def to_nnf(self):
        return type(self)(self.r.to_nnf(), self.f.to_nnf())


class LDLfPropositionalAtom(LDLfFormula):
    """An LDLf propositional formula.

    In LDLf with empty trace, this is equivalent to <phi>tt.
    """

    def __init__(self, s: Symbol):
        super().__init__()
        self.s = s

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.to_nnf()._delta(i, epsilon=epsilon)

    def truth(self, i: FiniteTrace, pos: int):
        return self.to_nnf().truth(i, pos)

    def _members(self):
        return self.to_nnf()._members()

    def to_nnf(self) -> LDLfFormula:
        return LDLfDiamond(RegExpPropositional(PLAtomic(self.s)), LDLfLogicalTrue())

    def negate(self) -> LDLfFormula:
        return self.to_nnf().negate()

    def find_labels(self) -> Set[Symbol]:
        return {self.s}

    def __str__(self):
        return "_".join(map(str, self._members()))


class LDLfLogicalTrue(AtomicFormula, LDLfFormula):
    def __init__(self):
        super().__init__(Symbols.LOGICAL_TRUE.value)

    def find_labels(self):
        return set()

    def truth(self, *args):
        return True

    def to_nnf(self):
        return self

    def negate(self):
        return LDLfLogicalFalse()

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return PLTrue()


class LDLfLogicalFalse(AtomicFormula, LDLfFormula):
    def __init__(self):
        super().__init__(Symbols.LOGICAL_FALSE.value)

    def find_labels(self):
        return set()

    def truth(self, *args):
        return False

    def to_nnf(self):
        return self

    def negate(self):
        return LDLfLogicalTrue()

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return PLFalse()


class LDLfNot(UnaryOperator, LDLfFormula):
    def __init__(self, f: LDLfFormula):
        super().__init__(f)
        self.f = cast(LDLfFormula, self.f)

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.NOT.value

    def negate(self) -> LDLfFormula:
        return self.f

    def truth(self, i: FiniteTrace, pos: int) -> bool:
        return not self.f.truth(i, pos)

    def to_nnf(self):
        if not isinstance(self.f, AtomicFormula):
            return self.f.negate().to_nnf()
        elif isinstance(self.f, (LDLfLogicalFalse, LDLfLogicalTrue)):
            return self.f.negate()
        else:
            return self

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        if epsilon:
            return PLFalse()

        result = self.f._delta(i, epsilon=epsilon)
        if result == PLTrue():
            return PLFalse()
        else:
            return PLTrue()


class LDLfAnd(LDLfBinaryOperator):
    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.AND.value

    def negate(self) -> LDLfFormula:
        return LDLfOr([f.negate() for f in self.formulas])

    def truth(self, i: FiniteTrace, pos: int) -> bool:
        return all(f.truth(i, pos) for f in self.formulas)

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return PLAnd([f._delta(i, epsilon) for f in self.formulas])


class LDLfOr(LDLfBinaryOperator):
    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.OR.value

    def negate(self) -> LDLfFormula:
        return LDLfOr([f.negate() for f in self.formulas])

    def truth(self, i: FiniteTrace, pos: int) -> bool:
        return any(f.truth(i, pos) for f in self.formulas)

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return PLOr([f._delta(i, epsilon) for f in self.formulas])


class LDLfImplies(LDLfBinaryOperator):
    """LDLf Implication"""

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.IMPLIES.value

    def truth(self, i: FiniteTrace, pos: int):
        return self.to_nnf().truth(i, pos)

    def negate(self) -> LDLfFormula:
        return self.to_nnf().negate()

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.to_nnf()._delta(i, epsilon=epsilon)

    def to_nnf(self) -> LDLfFormula:
        first, second = self.formulas[0:2]
        final_formula = LDLfOr([LDLfNot(first).to_nnf(), second.to_nnf()])
        for subformula in self.formulas[2:]:
            final_formula = LDLfOr(
                [LDLfNot(final_formula).to_nnf(), subformula.to_nnf()]
            )
        return final_formula


class LDLfEquivalence(LDLfBinaryOperator):
    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.EQUIVALENCE.value

    def truth(self, i: FiniteTrace, pos: int):
        return self.to_nnf().truth(i, pos)

    def to_nnf(self) -> LDLfFormula:
        fs = self.formulas
        pos = LDLfAnd(fs)
        neg = LDLfAnd([LDLfNot(f) for f in fs])
        res = LDLfOr([pos, neg]).to_nnf()
        return res

    def negate(self) -> LDLfFormula:
        return self.to_nnf().negate()

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.to_nnf()._delta(i, epsilon=epsilon)


class LDLfDiamond(LDLfTemporalFormula):
    @property
    def temporal_brackets(self) -> str:
        return (
            Symbols.EVENTUALLY_BRACKET_LEFT.value
            + Symbols.EVENTUALLY_BRACKET_RIGHT.value
        )

    def truth(self, tr: FiniteTrace, pos: int = 0):
        # last + 1 in order to include the last step
        return any(
            self.r.truth(tr, pos, j) and self.f.truth(tr, j)
            for j in range(pos, len(tr) + 1)
        )

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.r.delta_diamond(self.f, i, epsilon)

    def negate(self) -> LDLfFormula:
        return LDLfBox(self.r, self.f.negate())


class LDLfBox(LDLfTemporalFormula):
    @property
    def temporal_brackets(self) -> str:
        return Symbols.ALWAYS_BRACKET_LEFT.value + Symbols.ALWAYS_BRACKET_RIGHT.value

    def truth(self, tr: FiniteTrace, pos: int = 0):
        # last + 1 in order to include the last step
        return all(
            not (self.r.truth(tr, pos, j)) or self.f.truth(tr, j)
            for j in range(pos, len(tr) + 1)
        )

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.r.delta_box(self.f, i, epsilon)

    def negate(self) -> LDLfFormula:
        return LDLfDiamond(self.r, self.f.negate())


class RegExpPropositional(RegExpFormula):
    def __init__(self, pl_formula: PLFormula):
        RegExpFormula.__init__(self)
        self.pl_formula = pl_formula

    def truth(self, tr: FiniteTrace, start: int = 0, end: int = 0):
        return end == start + 1 and start < len(tr) and self.pl_formula.truth(tr[start])

    def _members(self):
        return RegExpPropositional, self.pl_formula

    def __str__(self):
        return str(self.pl_formula)

    def to_nnf(self):
        return RegExpPropositional(self.pl_formula.to_nnf())

    def negate(self):
        return RegExpPropositional(self.pl_formula.negate())

    def delta_diamond(
        self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False
    ):
        if epsilon:
            return PLFalse()
        if self.pl_formula.truth(i):
            return PLAtomic(_expand(f))
        else:
            return PLFalse()

    def delta_box(self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False):
        if epsilon:
            return PLTrue()
        if self.pl_formula.truth(i):
            return PLAtomic(_expand(f))
        else:
            return PLTrue()

    def find_labels(self):
        return self.pl_formula.find_labels()


class RegExpTest(UnaryOperator, RegExpFormula):
    def __init__(self, f: LDLfFormula):
        RegExpFormula.__init__(self)
        UnaryOperator.__init__(self, f)
        self.f = cast(LDLfFormula, self.f)

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.PATH_TEST.value

    def truth(self, tr: FiniteTrace, start: int = 0, end: int = 0):
        return start == end and self.f.truth(tr, start)

    def to_nnf(self):
        return RegExpTest(self.f.to_nnf())

    def delta_diamond(
        self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False
    ) -> PLFormula:
        return PLAnd([self.f._delta(i, epsilon), f._delta(i, epsilon)])  # type: ignore

    def delta_box(
        self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False
    ) -> PLFormula:
        return PLOr(
            [LDLfNot(self.f).to_nnf()._delta(i, epsilon), f._delta(i, epsilon)]
        )  # type: ignore

    def find_labels(self):
        return self.f.find_labels()

    def __str__(self):
        s = super().__str__()
        s = s[1:] + s[0]
        return s


class RegExpUnion(BinaryOperator, RegExpFormula):
    def __init__(self, formulas: Sequence[RegExpFormula]):
        super().__init__(formulas)
        self.formulas = cast(Sequence[RegExpFormula], self.formulas)

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.PATH_UNION.value

    def truth(self, tr: FiniteTrace, start: int = 0, end: int = 0):
        return any(f.truth(tr, start, end) for f in self.formulas)

    def to_nnf(self):
        return RegExpUnion([r.to_nnf() for r in self.formulas])

    def delta_diamond(
        self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False
    ):
        return PLOr([LDLfDiamond(r, f)._delta(i, epsilon) for r in self.formulas])

    def delta_box(self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False):
        return PLAnd([LDLfBox(r, f)._delta(i, epsilon) for r in self.formulas])


class RegExpSequence(BinaryOperator, RegExpFormula):
    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.PATH_SEQUENCE.value

    def __init__(self, formulas: Sequence[RegExpFormula]):
        super().__init__(formulas)
        self.formulas = cast(Sequence[RegExpFormula], self.formulas)

    def truth(self, tr: FiniteTrace, start: int = 0, end: int = 0):
        f1 = self.formulas[0]
        if len(self.formulas) == 2:
            f2 = self.formulas[1]
        else:
            f2 = RegExpSequence(self.formulas[1:])

        l = [
            (f1.truth(tr, start, k), f2.truth(tr, k, end))
            for k in range(start, end + 1)
        ]

        return any(
            f1.truth(tr, start, k) and f2.truth(tr, k, end)
            for k in range(start, end + 1)
        )

    def to_nnf(self):
        return RegExpSequence([r.to_nnf() for r in self.formulas])

    def delta_diamond(
        self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False
    ):
        res = LDLfDiamond(self.formulas[-1], f)
        for r in reversed(self.formulas[:-1]):
            res = LDLfDiamond(r, res)
        return res._delta(i, epsilon)

    def delta_box(self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False):
        res = LDLfBox(self.formulas[-1], f)
        for r in reversed(self.formulas[:-1]):
            res = LDLfBox(r, res)
        return res._delta(i, epsilon)


class RegExpStar(UnaryOperator, RegExpFormula):
    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.PATH_STAR.value

    def __init__(self, f: RegExpFormula):
        super().__init__(f)
        self.f = cast(RegExpFormula, self.f)

    def truth(self, tr: FiniteTrace, start: int = 0, end: int = 0):
        return start == end or any(
            self.f.truth(tr, start, k) and self.truth(tr, k, end)
            for k in range(start, end + 1)
        )

    def __str__(self):
        s = super().__str__()
        s = s[1:] + s[0]
        return s

    def to_nnf(self):
        return RegExpStar(self.f.to_nnf())

    def delta_diamond(
        self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False
    ):
        return PLOr(
            [
                f._delta(i, epsilon),
                LDLfDiamond(self.f, _FreezedFalse(LDLfDiamond(self, f)))._delta(
                    i, epsilon
                ),
            ]
        )

    def delta_box(self, f: LDLfFormula, i: PropositionalInterpretation, epsilon=False):
        return PLAnd(
            [
                f._delta(i, epsilon),
                LDLfBox(self.f, _FreezedTrue(LDLfBox(self, f)))._delta(i, epsilon),
            ]
        )


class LDLfEnd(LDLfFormula):
    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.to_nnf()._delta(i, epsilon=epsilon)

    def find_labels(self) -> Set[Symbol]:
        return self.to_nnf().find_labels()

    def truth(self, i: FiniteTrace, pos: int):
        return self.to_nnf().truth(i, pos)

    def _members(self):
        return (Symbols.END.value,)

    def to_nnf(self) -> LDLfFormula:
        return LDLfBox(RegExpPropositional(PLTrue()), LDLfLogicalFalse())

    def negate(self) -> LDLfFormula:
        return self.to_nnf().negate()

    def __str__(self):
        return "_".join(map(str, self._members()))


class LDLfLast(LDLfFormula):
    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.to_nnf()._delta(i, epsilon=epsilon)

    def find_labels(self) -> Set[Symbol]:
        return self.to_nnf().find_labels()

    def truth(self, i: FiniteTrace, pos: int):
        return self.to_nnf().truth(i, pos)

    def _members(self):
        return (Symbols.LAST.value,)

    def to_nnf(self) -> LDLfFormula:
        return LDLfDiamond(RegExpPropositional(PLTrue()), LDLfEnd().to_nnf())

    def negate(self) -> LDLfFormula:
        return self.to_nnf().negate()

    def __str__(self):
        return "_".join(map(str, self._members()))


class _FreezedFalse(LDLfFormula):
    def __init__(self, f: LDLfFormula):
        super().__init__()
        self.f = f

    def _members(self):
        return ("_F", self.f)

    def __str__(self):
        return "_".join(map(str, self._members()))

    def delta(self, i: PropositionalInterpretation, epsilon=False):
        return self._delta(i, epsilon)

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return PLFalse()

    def to_nnf(self):
        return self

    def negate(self):
        return _FreezedFalse(self.f)

    def find_labels(self):
        return super().find_labels()

    def truth(self, tr: FiniteTrace, pos: int):
        raise NotImplementedError


class _FreezedTrue(LDLfFormula):
    def __init__(self, f: LDLfFormula):
        super().__init__()
        self.f = f

    def _members(self):
        return ("_T", self.f)

    def __str__(self):
        return "_".join(map(str, self._members()))

    def delta(self, i: PropositionalInterpretation, epsilon=False):
        return self._delta(i, epsilon)

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return PLTrue()

    def to_nnf(self):
        return self

    def negate(self):
        return _FreezedFalse(self.f)

    def find_labels(self):
        return super().find_labels()

    def truth(self, tr: FiniteTrace, pos: int):
        raise NotImplementedError


def _expand(f: Formula):
    if isinstance(f, _FreezedFalse) or isinstance(f, _FreezedTrue):
        return _expand(f.f)
    # elif isinstance(f, LDLfLogicalTrue):
    #     return PLTrue()
    # elif isinstance(f, LDLfLogicalFalse):
    #     return PLFalse()
    elif isinstance(f, LDLfDiamond) or isinstance(f, LDLfBox):
        return type(f)(f.r, _expand(f.f))
    elif isinstance(f, BinaryOperator):
        return type(f)([_expand(subf) for subf in f.formulas])
    # elif isinstance(f, LDLfLogicalTrue):
    #     return PLTrue()
    # elif isinstance(f, LDLfLogicalFalse):
    #     return PLFalse()
    else:
        return f

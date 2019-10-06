# -*- coding: utf-8 -*-
"""
This module contains the implementation of Linear Temporal Logic on finite traces.

References:
    - Linear Temporal Logic and Linear Dynamic Logic on Finite Traces:
      https://www.cs.rice.edu/~vardi/papers/ijcai13.pdf
    - LTLfand LDLfSynthesis Under Partial Observability:
      http://www.diag.uniroma1.it/~degiacom/papers/2016/IJCAI16dv.pdf

"""
from abc import abstractmethod, ABC
from functools import lru_cache
from typing import Set

from flloat.base.convertible import ConvertibleFormula, BaseConvertibleFormula
from flloat.base.delta import (
    Delta,
    DeltaConvertibleFormula,
    EquivalenceDeltaConvertible,
    ImpliesDeltaConvertible,
)
from flloat.base.formulas import (
    Formula,
    CommutativeBinaryOperator,
    AtomicFormula,
    UnaryOperator,
)
from flloat.base.nnf import (
    NNF,
    NotNNF,
    DualBinaryOperatorNNF,
    DualUnaryOperatorNNF,
    AtomicNNF,
)
from flloat.base.symbols import Symbol, Symbols
from flloat.base.truths import Truth, NotTruth, OrTruth, AndTruth
from flloat.flloat import to_automaton
from flloat.helpers import MAX_CACHE_SIZE
from flloat.ldlf import (
    LDLfNot,
    LDLfAnd,
    LDLfOr,
    LDLfDiamond,
    RegExpPropositional,
    RegExpStar,
    RegExpSequence,
    RegExpTest,
    LDLfPropositional,
    LDLfEnd,
)
from flloat.pl import PLTrue, PLFalse, PLAnd, PLOr, PLAtomic, PLFormula
from flloat.semantics.pl import PLInterpretation
# from flloat.flloat import DFAOTF, to_automaton_
from flloat.semantics.traces import FiniteTrace, FiniteTraceTruth


class LTLfTruth(Truth):
    """Interface for"""

    @abstractmethod
    def truth(self, i: FiniteTrace, pos: int = 0):
        raise NotImplementedError


class LTLfFormula(Formula, LTLfTruth, NNF, Delta):
    @lru_cache(maxsize=MAX_CACHE_SIZE)
    def delta(self, i: PLInterpretation, epsilon=False) -> PLFormula:
        f = self.to_nnf()
        d = f._delta(i, epsilon)
        if epsilon:
            # By definition, if epsilon=True, then the result must be either PLTrue or PLFalse
            # Now, the output is a Propositional Formula with only PLTrue or PLFalse as atomics
            # Hence, we just evaluate the formula with a dummy PLInterpretation
            d = PLTrue() if d.truth(None) else PLFalse()
        return d

    @abstractmethod
    def _delta(self, i: PLInterpretation, epsilon=False):
        """apply delta function, assuming that 'self' is a LTLf formula in Negative Normal Form"""

    @abstractmethod
    def to_LDLf(self):
        """
        Tranform the formula into an equivalent LDLf formula.

        :return: an LDLf formula.
        """

    def __repr__(self):
        return self.__str__()

    def to_automaton(self, labels: Set[Symbol] = None):
        if labels is None:
            labels = self.find_labels()

        return to_automaton(self, labels)


class LTLfCommBinaryOperator(CommutativeBinaryOperator, LTLfFormula, ABC):
    pass


class LTLfTemporalFormula(LTLfFormula, FiniteTraceTruth, ABC):
    pass


class LTLfAtomic(AtomicFormula, AtomicNNF, LTLfFormula):
    def __init__(self, s: Symbol):
        AtomicFormula.__init__(self, s)

    def __str__(self):
        return AtomicFormula.__str__(self)

    def _members(self):
        return LTLfAtomic, self.s

    def _to_nnf(self):
        return self

    def negate(self):
        return LTLfNot(LTLfAtomic(self.s))

    def _delta(self, i: PLInterpretation, epsilon: bool = False):
        if epsilon:
            return PLFalse()
        return PLTrue() if PLAtomic(self.s).truth(i) else PLFalse()

    def truth(self, i: FiniteTrace, pos: int = 0):
        return PLAtomic(self.s).truth(i.get(pos))

    def find_labels(self) -> Set[Symbol]:
        return PLAtomic(self.s).find_labels()

    def to_LDLf(self):
        return LDLfPropositional(PLAtomic(self.s)).convert()


class LTLfTrue(LTLfAtomic):
    def __init__(self):
        super().__init__(Symbols.TRUE.value)

    def negate(self):
        return LTLfFalse()

    def _delta(self, i: PLInterpretation, epsilon: bool = False):
        if epsilon:
            return PLFalse()
        else:
            return PLTrue()

    def truth(self, i: FiniteTrace, pos: int = 0):
        return True

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return set()


class LTLfFalse(LTLfAtomic):
    def __init__(self):
        super().__init__(Symbols.FALSE.value)

    def negate(self):
        return LTLfTrue()

    def _delta(self, i: PLInterpretation, epsilon: bool = False):
        return PLFalse()

    def truth(self, i: FiniteTrace, pos: int = 0):
        return False

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return set()


class LTLfNot(NotTruth, LTLfFormula, NotNNF):
    def _delta(self, i: PLInterpretation, epsilon=False):
        if isinstance(self.f, LTLfAtomic) or isinstance(self.f, LTLfEnd):
            if epsilon:
                return PLFalse()
            else:
                return PLTrue() if self.f._delta(i, epsilon) == PLFalse() else PLFalse()
        else:
            # the formula must be in NNF form!!!
            raise Exception

    def to_LDLf(self):
        return LDLfNot(self.f.to_LDLf())


class LTLfAnd(LTLfCommBinaryOperator, AndTruth, DualBinaryOperatorNNF):
    def _delta(self, i: PLInterpretation, epsilon=False):
        return PLAnd([f._delta(i, epsilon) for f in self.formulas])

    def to_LDLf(self):
        return LDLfAnd([f.to_LDLf() for f in self.formulas])


class LTLfOr(LTLfCommBinaryOperator, OrTruth, DualBinaryOperatorNNF):
    def _delta(self, i: PLInterpretation, epsilon=False):
        return PLOr([f._delta(i, epsilon) for f in self.formulas])

    def to_LDLf(self):
        return LDLfOr([f.to_LDLf() for f in self.formulas])


class LTLfImplies(ImpliesDeltaConvertible, LTLfFormula):
    And = LTLfAnd
    Or = LTLfOr
    Not = LTLfNot

    def to_LDLf(self):
        return self.convert().to_LDLf()


class LTLfEquivalence(EquivalenceDeltaConvertible, LTLfCommBinaryOperator):
    And = LTLfAnd
    Or = LTLfOr
    Not = LTLfNot

    def to_LDLf(self):
        return self.convert().to_LDLf()


class LTLfNext(DualUnaryOperatorNNF, LTLfTemporalFormula):
    operator_symbol = "X"
    Not = LTLfNot

    def truth(self, i: FiniteTrace, pos: int = 0):
        return pos < i.last() and self.f.truth(i, pos + 1)

    def _delta(self, i: PLInterpretation, epsilon=False):
        if epsilon:
            return PLFalse()
        else:
            return PLAnd([PLAtomic(self.f), PLAtomic(LTLfNot(LTLfEnd()).to_nnf())])

    def to_LDLf(self):
        return LDLfDiamond(
            RegExpPropositional(PLTrue()),
            LDLfAnd([self.f.to_LDLf(), LDLfNot(LDLfEnd())]),
        )


class LTLfWeakNext(DualUnaryOperatorNNF, ConvertibleFormula, LTLfTemporalFormula):
    operator_symbol = Symbols.WEAK_NEXT.value
    Not = LTLfNot

    def convert(self):
        return LTLfNot(LTLfNext(LTLfNot(self.f)))

    def truth(self, i: FiniteTrace, pos: int = 0):
        return self.convert().truth(i, pos)

    def _delta(self, i: PLInterpretation, epsilon=False):
        if epsilon:
            return PLTrue()
        else:
            return PLOr([PLAtomic(self.f), PLAtomic(LTLfEnd().to_nnf())])

    def to_LDLf(self):
        return self.convert().to_LDLf()


class LTLfUntil(DualBinaryOperatorNNF, LTLfTemporalFormula):
    operator_symbol = "U"

    def _to_nnf(self):
        return LTLfUntil([f.to_nnf() for f in self.formulas])

    def negate(self):
        return LTLfRelease([LTLfNot(f) for f in self.formulas])

    def truth(self, i: FiniteTrace, pos: int = 0):
        f1 = self.formulas[0]
        f2 = (
            LTLfUntil(self.formulas[1:]) if len(self.formulas) > 2 else self.formulas[1]
        )

        return any(
            f2.truth(i, j) and all(f1.truth(i, k) for k in range(pos, j))
            for j in range(pos, i.last() + 1)
        )

    def _delta(self, i: PLInterpretation, epsilon: bool = False):
        if epsilon:
            return PLFalse()
        f1 = self.formulas[0]
        f2 = (
            LTLfUntil(self.formulas[1:]) if len(self.formulas) > 2 else self.formulas[1]
        )
        return PLOr(
            [
                f2._delta(i, epsilon),
                PLAnd([f1._delta(i, epsilon), LTLfNext(self)._delta(i, epsilon)]),
            ]
        )

    def to_LDLf(self):
        f1 = self.formulas[0].to_LDLf()
        f2 = (
            LTLfUntil(self.formulas[1:]).to_LDLf()
            if len(self.formulas) > 2
            else self.formulas[1].to_LDLf()
        )
        return LDLfDiamond(
            RegExpStar(RegExpSequence([RegExpTest(f1), RegExpPropositional(PLTrue())])),
            LDLfAnd([f2, LDLfNot(LDLfEnd())]),
        )


class LTLfEventually(UnaryOperator, DeltaConvertibleFormula, LTLfTemporalFormula):
    operator_symbol = "F"
    Not = LTLfNot

    def convert(self):
        return LTLfUntil([LTLfTrue(), self.f])

    def to_LDLf(self):
        return LDLfDiamond(
            RegExpStar(RegExpPropositional(PLTrue())),
            LDLfAnd([self.f.to_LDLf(), LDLfNot(LDLfEnd())]),
        )


class LTLfAlways(UnaryOperator, DeltaConvertibleFormula, LTLfTemporalFormula):
    operator_symbol = "G"
    Not = LTLfNot

    def convert(self):
        return LTLfNot(LTLfEventually(LTLfNot(self.f)).convert())

    def to_LDLf(self):
        return self.convert().to_LDLf()


class LTLfRelease(DualBinaryOperatorNNF, BaseConvertibleFormula, LTLfTemporalFormula):
    operator_symbol = "R"
    Dual = LTLfUntil

    def convert(self):
        return LTLfNot(LTLfUntil([LTLfNot(f) for f in self.formulas]))

    def _delta(self, i: PLInterpretation, epsilon=False):
        if epsilon:
            return PLTrue()
        f1 = self.formulas[0]
        f2 = (
            LTLfRelease(self.formulas[1:])
            if len(self.formulas) > 2
            else self.formulas[1]
        )
        return PLAnd(
            [
                f2._delta(i, epsilon),
                PLOr([f1._delta(i, epsilon), LTLfWeakNext(self)._delta(i, epsilon)]),
            ]
        )

    def to_LDLf(self):
        return self.convert().to_LDLf()


class LTLfEnd(DeltaConvertibleFormula, BaseConvertibleFormula, LTLfAtomic):
    def __init__(self):
        LTLfAtomic.__init__(self, Symbols.END.value)

    def _members(self):
        return (self.s,)

    def convert(self):
        return LTLfAlways(LTLfFalse()).to_nnf()

    def negate(self):
        return LTLfEventually(LTLfTrue()).to_nnf()

    def __str__(self):
        return "_".join(map(str, self._members()))


LTLfAtomic.Not = LTLfNot  # type: ignore

LTLfAnd.Dual = LTLfOr  # type: ignore
LTLfOr.Dual = LTLfAnd  # type: ignore

LTLfNext.Dual = LTLfWeakNext  # type: ignore
LTLfWeakNext.Dual = LTLfNext  # type: ignore

LTLfEventually.Dual = LTLfAlways  # type: ignore
LTLfAlways.Dual = LTLfEventually  # type: ignore

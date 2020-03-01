# -*- coding: utf-8 -*-
"""
This module contains the implementation of Linear Temporal Logic on finite traces.

References:
    - Linear Temporal Logic and Linear Dynamic Logic on Finite Traces:
      https://www.cs.rice.edu/~vardi/papers/ijcai13.pdf
    - LTLf and LDLfSynthesis Under Partial Observability:
      http://www.diag.uniroma1.it/~degiacom/papers/2016/IJCAI16dv.pdf

"""
from abc import abstractmethod, ABC
from typing import Set, Sequence, cast

from pythomata import PropositionalInterpretation

from flloat.base import FiniteTraceTruth, Formula, AtomicFormula, FiniteTrace, UnaryOperator, BinaryOperator
from flloat.delta import Delta
from flloat.symbols import Symbol, Symbols

from flloat.flloat import to_automaton

from flloat.pl import PLFalse, PLTrue, PLAtomic, PLOr, PLAnd, PLFormula


class LTLfFormula(Formula, FiniteTraceTruth, Delta, ABC):

    def delta(self, i: PropositionalInterpretation, epsilon=False) -> PLFormula:
        f = self.to_nnf()
        d = f._delta(i, epsilon=epsilon)
        if epsilon:
            # By definition, if epsilon=True, then the result must be either PLTrue or PLFalse
            # Now, the output is a Propositional Formula with only PLTrue or PLFalse as atomics
            # Hence, we just evaluate the formula with a dummy PropositionalInterpretation
            d = PLTrue() if d.truth({}) else PLFalse()
        return d

    @abstractmethod
    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        """apply delta function, assuming that 'self' is a LTLf formula in Negative Normal Form"""

    # @abstractmethod
    def to_ldlf(self):
        """
        Tranform the formula into an equivalent LDLf formula.

        :return: an LDLf formula.
        """

    def __repr__(self):
        return self.__str__()

    def to_automaton(self):
        return to_automaton(self)


class LTLfUnaryOperator(UnaryOperator, LTLfFormula, ABC):

    def __init__(self, f: LTLfFormula):
        super().__init__(f)
        self.f = cast(LTLfFormula, self.f)


class LTLfBinaryOperator(BinaryOperator, LTLfFormula, ABC):
    """A binary operator for LTLf."""

    def __init__(self, formulas: Sequence[LTLfFormula]):
        super().__init__(formulas)
        self.formulas = cast(Sequence[LTLfFormula], self.formulas)


class LTLfAtomic(AtomicFormula, LTLfFormula):

    def negate(self):
        return LTLfNot(self)

    def _delta(self, i: PropositionalInterpretation, epsilon: bool = False):
        if epsilon:
            return PLFalse()
        return PLTrue() if PLAtomic(self.s).truth(i) else PLFalse()

    def truth(self, i: FiniteTrace, pos: int = 0):
        if len(i) > 0:
            return PLAtomic(self.s).truth(i[pos])
        else:
            return False

    def find_labels(self) -> Set[Symbol]:
        return PLAtomic(self.s).find_labels()

    # def to_ldlf(self):
    #     return LDLfPropositional(PLAtomic(self.s)).convert()


class LTLfTrue(LTLfAtomic):
    def __init__(self):
        super().__init__(Symbols.TRUE.value)

    def _delta(self, i: PropositionalInterpretation, epsilon: bool = False):
        if epsilon:
            return PLFalse()
        else:
            return PLTrue()

    def truth(self, i: FiniteTrace, pos: int = 0):
        return len(i) > 0

    def negate(self):
        return LTLfFalse()

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return set()


class LTLfFalse(LTLfAtomic):
    def __init__(self):
        super().__init__(Symbols.FALSE.value)

    def negate(self):
        return LTLfTrue()

    def _delta(self, i: PropositionalInterpretation, epsilon: bool = False):
        return PLFalse()

    def truth(self, i: FiniteTrace, pos: int = 0):
        return False

    def find_labels(self) -> Set[Symbol]:
        """Return the set of symbols."""
        return set()


class LTLfNot(LTLfUnaryOperator):

    def __init__(self, f: LTLfFormula):
        super().__init__(f)

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.NOT.value

    def to_nnf(self):
        if not isinstance(self.f, AtomicFormula):
            return self.f.negate().to_nnf()
        else:
            return self

    def negate(self) -> LTLfFormula:
        return self.f

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        if isinstance(self.f, LTLfAtomic) or isinstance(self.f, LTLfEnd):
            if epsilon:
                return PLFalse()
            else:
                return PLTrue() if self.f._delta(i, epsilon) == PLFalse() else PLFalse()
        else:
            # the formula must be in NNF form!!!
            raise Exception

    def truth(self, i: FiniteTrace, pos: int = 0):
        if len(i) == 0:
            if isinstance(self.f, LTLfAtomic):
                return False
            else:
                return self.to_nnf().truth(i, pos)
        else:
            return not self.f.truth(i, pos)

    # def to_ldlf(self):
    #     return LDLfNot(self.f.to_ldlf())


class LTLfAnd(LTLfBinaryOperator):

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.AND.value

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return PLAnd([f._delta(i, epsilon) for f in self.formulas])

    def truth(self, i: FiniteTrace, pos: int):
        return all(f.truth(i, pos) for f in self.formulas)

    def to_nnf(self):
        return LTLfAnd([f.to_nnf() for f in self.formulas])

    def negate(self) -> LTLfFormula:
        return LTLfOr([f.negate() for f in self.formulas])

    # def to_ldlf(self):
    #     return LDLfAnd([f.to_ldlf() for f in self.formulas])


class LTLfOr(LTLfBinaryOperator):

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.OR.value

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return PLOr([f._delta(i, epsilon) for f in self.formulas])

    def truth(self, i: FiniteTrace, pos: int):
        return any(f.truth(i, pos) for f in self.formulas)

    def to_nnf(self):
        return LTLfOr([f.to_nnf() for f in self.formulas])

    def negate(self) -> LTLfFormula:
        return LTLfAnd([f.negate() for f in self.formulas])


class LTLfImplies(LTLfBinaryOperator):
    """LTLf Implication"""

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.IMPLIES.value

    def truth(self, i: FiniteTrace, pos: int):
        return self.to_nnf().truth(i, pos)

    def negate(self) -> LTLfFormula:
        return self.to_nnf().negate()

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.to_nnf()._delta(i, epsilon=epsilon)

    def to_nnf(self) -> LTLfFormula:
        first, second = self.formulas[0: 2]
        final_formula = LTLfOr([LTLfNot(first).to_nnf(), second.to_nnf()])
        for subformula in self.formulas[2:]:
            final_formula = LTLfOr([LTLfNot(final_formula).to_nnf(), subformula.to_nnf()])
        return final_formula


class LTLfEquivalence(LTLfBinaryOperator):

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.EQUIVALENCE.value

    def truth(self, i: FiniteTrace, pos: int):
        return self.to_nnf().truth(i, pos)

    def to_nnf(self) -> LTLfFormula:
        fs = self.formulas
        pos = LTLfAnd(fs)
        neg = LTLfAnd([LTLfNot(f) for f in fs])
        res = LTLfOr([pos, neg]).to_nnf()
        return res

    def negate(self) -> LTLfFormula:
        return self.to_nnf().negate()

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.to_nnf()._delta(i, epsilon=epsilon)


class LTLfNext(LTLfUnaryOperator):

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.NEXT.value

    def to_nnf(self) -> LTLfFormula:
        return LTLfNext(self.f.to_nnf())

    def negate(self) -> LTLfFormula:
        return LTLfWeakNext(self.f.negate())

    def truth(self, i: FiniteTrace, pos: int = 0):
        return pos < len(i) - 1 and self.f.truth(i, pos + 1)

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        if epsilon:
            return PLFalse()
        else:
            return PLAnd([PLAtomic(self.f), PLAtomic(LTLfNot(LTLfEnd()).to_nnf())])

    # def to_ldlf(self):
    #     return LDLfDiamond(
    #         RegExpPropositional(PLTrue()),
    #         LDLfAnd([self.f.to_ldlf(), LDLfNot(LDLfEnd())]),
    #     )


class LTLfWeakNext(LTLfUnaryOperator):

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.WEAK_NEXT.value

    def to_nnf(self) -> LTLfFormula:
        return LTLfWeakNext(self.f.to_nnf())

    def negate(self) -> LTLfFormula:
        return LTLfNext(self.f.negate())

    def truth(self, i: FiniteTrace, pos: int = 0):
        return not (pos < len(i) - 1) or self.f.truth(i, pos + 1)

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        if epsilon:
            return PLTrue()
        else:
            return PLOr([PLAtomic(self.f), PLAtomic(LTLfEnd().to_nnf())])

    # def to_ldlf(self):
    #     return self.convert().to_ldlf()


class LTLfUntil(LTLfBinaryOperator):

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.UNTIL.value

    def to_nnf(self):
        return LTLfUntil([f.to_nnf() for f in self.formulas])

    def negate(self):
        return LTLfRelease([f.negate() for f in self.formulas])

    def truth(self, i: FiniteTrace, pos: int = 0):
        f1 = self.formulas[0]
        f2 = (
            LTLfUntil(self.formulas[1:]) if len(self.formulas) > 2 else self.formulas[1]
        )

        return any(
            f2.truth(i, j) and all(f1.truth(i, k) for k in range(pos, j))
            for j in range(pos, len(i))
        )

    def _delta(self, i: PropositionalInterpretation, epsilon: bool = False):
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

    # def to_ldlf(self):
    #     f1 = self.formulas[0].to_ldlf()
    #     f2 = (
    #         LTLfUntil(self.formulas[1:]).to_ldlf()
    #         if len(self.formulas) > 2
    #         else self.formulas[1].to_ldlf()
    #     )
    #     return LDLfDiamond(
    #         RegExpStar(RegExpSequence([RegExpTest(f1), RegExpPropositional(PLTrue())])),
    #         LDLfAnd([f2, LDLfNot(LDLfEnd())]),
    #     )


class LTLfRelease(LTLfBinaryOperator):

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.RELEASE.value

    def to_nnf(self):
        return LTLfRelease([f.to_nnf() for f in self.formulas])

    def negate(self):
        return LTLfUntil([f.negate() for f in self.formulas])

    def truth(self, i: FiniteTrace, pos: int = 0):
        f1 = self.formulas[0]
        f2 = (
            LTLfRelease(self.formulas[1:])
            if len(self.formulas) > 2
            else self.formulas[1]
        )
        return all(
            f2.truth(i, j) or any(f1.truth(i, k) for k in range(pos, j))
            for j in range(pos, len(i))
        )

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
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


class LTLfEventually(LTLfUnaryOperator):

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.EVENTUALLY.value

    def to_nnf(self) -> LTLfFormula:
        return LTLfUntil([LTLfTrue(), self.f])

    def negate(self) -> LTLfFormula:
        return self.to_nnf().negate()

    def truth(self, i: FiniteTrace, pos: int):
        return self.to_nnf().truth(i, pos)

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.to_nnf()._delta(i, epsilon=epsilon)

    # def to_ldlf(self):
    #     return LDLfDiamond(
    #         RegExpStar(RegExpPropositional(PLTrue())),
    #         LDLfAnd([self.f.to_ldlf(), LDLfNot(LDLfEnd())]),
    #     )


class LTLfAlways(LTLfUnaryOperator):

    @property
    def operator_symbol(self) -> Symbol:
        return Symbols.EVENTUALLY.value

    def to_nnf(self) -> LTLfFormula:
        return LTLfRelease([LTLfFalse(), self.f.to_nnf()])

    def negate(self) -> LTLfFormula:
        return self.to_nnf().negate()

    def truth(self, i: FiniteTrace, pos: int):
        return self.to_nnf().truth(i, pos)

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.to_nnf()._delta(i, epsilon=epsilon)


class LTLfEnd(LTLfFormula):

    def _delta(self, i: PropositionalInterpretation, epsilon=False):
        return self.to_nnf()._delta(i, epsilon=epsilon)

    def find_labels(self) -> Set[Symbol]:
        return self.to_nnf().find_labels()

    def truth(self, i: FiniteTrace, pos: int):
        return self.to_nnf().truth(i, pos)

    def _members(self):
        return Symbols.END.value,

    def to_nnf(self) -> LTLfFormula:
        return LTLfAlways(LTLfFalse()).to_nnf()

    def negate(self) -> LTLfFormula:
        return self.to_nnf().negate()

    def __str__(self):
        return "_".join(map(str, self._members()))

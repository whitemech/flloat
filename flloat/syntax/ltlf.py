from abc import abstractmethod, ABC
from typing import Set

from flloat.base.Formula import Formula, CommutativeBinaryOperator, AtomicFormula, BinaryOperator, UnaryOperator
from flloat.base.Symbol import Symbol
from flloat.base.nnf import NNF, NotNNF
from flloat.base.truths import Truth, NotTruth, OrTruth, EquivalenceTruth, ImpliesTruth, AndTruth
from flloat.flloat import DFAOTF, to_automaton
from flloat.semantics.ldlf import FiniteTrace
from flloat.semantics.pl import PLInterpretation, PLTrueInterpretation
from flloat.syntax.ldlf import FiniteTraceTruth
from flloat.syntax.pl import PLTrue, PLFalse, PLAnd, PLOr, PLFormula, PLAtomic


class LTLfTruth(Truth):
    @abstractmethod
    def truth(self, i: FiniteTrace, pos: int):
        raise NotImplementedError

class Delta(ABC):
    @abstractmethod
    def delta(self, i: PLInterpretation, epsilon=False):
        raise NotImplementedError


class LTLfFormula(Formula, LTLfTruth, NNF, Delta):

    def delta(self, i: PLInterpretation, epsilon=False):
        f = self.to_nnf()
        d = f._delta(i, epsilon)
        if epsilon:
            # By definition, if epsilon=True, then the result must be either PLTrue or PLFalse
            # Now, the output is a Propositional Formula with only PLTrue or PLFalse as atomics
            # Hence, we just evaluate the formula with a dummy PLInterpretation
            d = PLTrue() if d.truth(PLTrueInterpretation()) else PLFalse()
        return d

    @abstractmethod
    def _delta(self, i: PLInterpretation, epsilon=False):
        """apply delta function, assuming that 'self' is a LDLf formula in Negative Normal Form"""
        raise NotImplementedError

    def __repr__(self):
        return self.__str__()

    def to_automaton(self, labels:Set[Symbol]=None, on_the_fly=False, determinize=False, minimize=True):
        if on_the_fly:
            return DFAOTF(self)
        else:
            return to_automaton(self, labels, determinize, minimize)


class LTLfCommBinaryOperator(LTLfFormula, CommutativeBinaryOperator):
    pass

class LTLfTemporalFormula(LTLfFormula, FiniteTraceTruth):
    pass


class LTLfAtomic(LTLfFormula, AtomicFormula):

    def __init__(self, a:PLAtomic):
        super().__init__(a.s)
        self.a = a

    def __str__(self):
        return AtomicFormula.__str__(self)


    def _members(self):
        return (LTLfAtomic, self.a)

    def to_nnf(self):
        return self.a.to_nnf()

    def negate(self):
        return self.a.negate()

    def _delta(self, i:PLInterpretation, epsilon=False):
        raise NotImplementedError

    def truth(self, i: FiniteTrace, pos: int):
        return self.a.truth(i.get(pos))

    def find_labels(self):
        return self.a.find_labels()


class LTLfTrue(LTLfAtomic):
    def __init__(self):
        super().__init__(PLTrue())


class LTLfFalse(LTLfAtomic):
    def __init__(self):
        super().__init__(PLFalse())


class LTLfNot(LTLfFormula, NotTruth, NotNNF):

    def to_nnf(self):
        return self.f.negate().to_nnf()

    def negate(self):
        return self.f

    def _delta(self, i: PLInterpretation, epsilon=False):
        # should never called, since it is called from NNF formulas
        raise Exception

class LTLfAnd(LTLfCommBinaryOperator, AndTruth):
    def to_nnf(self):
        childs = set([child.to_nnf() for child in self.formulas])
        return LTLfAnd(childs)

    def negate(self):
        childs = set([child.negate().to_nnf() for child in self.formulas])
        return LTLfOr(childs)

    def _delta(self, i:PLInterpretation, epsilon=False):
        return PLAnd({f._delta(i, epsilon) for f in self.formulas})


class LTLfOr(LTLfCommBinaryOperator, OrTruth):

    def to_nnf(self):
        childs = set([child.to_nnf() for child in self.formulas])
        return LTLfOr(childs)

    def negate(self):
        childs = set([child.negate().to_nnf() for child in self.formulas])
        return LTLfAnd(childs)

    def _delta(self, i:PLInterpretation, epsilon=False):
        return PLOr({f._delta(i, epsilon) for f in self.formulas})


class LTLfImplies(LTLfFormula, ImpliesTruth):

    def _convert(self):
        fs = self.formulas
        a, b = LTLfAnd(set(fs[:-1])), fs[-1]
        res = LTLfOr({LTLfNot(a), b})
        return res

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate().to_nnf()

    def truth(self, *args):
        return self._convert().truth(*args)

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self._convert()._delta(i, epsilon)


class LTLfEquivalence(LTLfCommBinaryOperator, EquivalenceTruth):

    def _convert(self):
        fs = self.formulas
        pos = LTLfAnd(set(fs))
        neg = LTLfOr(set(LTLfNot(f) for f in fs))

        res = LTLfOr({pos, neg})
        return res

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate()

    def truth(self, *args):
        return self._convert().truth(*args)

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self._convert()._delta(i, epsilon)


class LTLfNext(LTLfTemporalFormula, UnaryOperator):
    operator_symbol = "X"

    def to_nnf(self):
        return LTLfNext(self.f.to_nnf())

    def negate(self):
        return LTLfNext(LTLfNot(self.f))

    def truth(self, i: FiniteTrace, pos: int):
        return pos < i.last() and self.f.truth(i, pos + 1)

    def _delta(self, i: PLInterpretation, epsilon=False):
        pass


class LTLfWeakNext(LTLfTemporalFormula, UnaryOperator):
    operator_symbol = "WX"

    def _convert(self):
        return LTLfNot(LTLfNext(LTLfNot(self.f)))

    def negate(self):
        return self._convert().negate()

    def truth(self, *args):
        return self._convert().truth(*args)

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self._convert()._delta(i, epsilon)


class LTLfUntil(LTLfTemporalFormula, BinaryOperator):
    operator_symbol = "U"

    def to_nnf(self):
        return LTLfUntil([f.to_nnf() for f in self.formulas])

    def negate(self):
        return LTLfRelease([LTLfNot(f) for f in self.formulas])

    def truth(self, i: FiniteTrace, pos: int):
        f1 = self.formulas[0]
        f2 = LTLfUntil(self.formulas[1:]) if len(self.formulas)>2 else self.formulas[1]
        return any(f2.truth(i, j) and all(f1.truth(i, k) for k in range(pos, j)) for j in range(pos, i.last()+1))

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self._delta(i, epsilon)


class LTLfEventually(LTLfTemporalFormula, UnaryOperator):
    operator_symbol = "F"

    def _convert(self):
        return LTLfUntil([LTLfTrue(), self.f])

    def negate(self):
        return self._convert().negate()

    def truth(self, *args):
        return self._convert().truth(*args)

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self._convert()._delta(i, epsilon)



class LTLfAlways(LTLfTemporalFormula, UnaryOperator):
    operator_symbol = "G"

    def _convert(self):
        return LTLfUntil([LTLfTrue(), self.f])

    def negate(self):
        return self._convert().negate()

    def truth(self, *args):
        return self._convert().truth(*args)

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self._convert()._delta(i, epsilon)


class LTLfRelease(LTLfTemporalFormula, BinaryOperator):
    operator_symbol = "R"

    def _convert(self):
        return LTLfNot(LTLfUntil([LTLfNot(f) for f in self.formulas]))

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate()

    def truth(self, *args):
        return self._convert().truth(*args)

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self._convert().truth(i, epsilon)


from abc import abstractmethod, ABC
from typing import Set

from pythomata.base.NFA import NFA

from flloat.base.Formula import Formula, CommutativeBinaryOperator, UnaryOperator, BinaryOperator, OperatorChilds, \
    AtomicFormula
from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.nnf import NNF, NotNNF
from flloat.base.truths import NotTruth, AndTruth, OrTruth, ImpliesTruth, EquivalenceTruth, Truth
from flloat.flloat import to_automaton, DFAOTF
from flloat.semantics.ldlf import FiniteTrace
from flloat.semantics.pl import PLInterpretation, PLTrueInterpretation
from flloat.syntax.pl import PLFormula, PLTrue, PLFalse, PLAnd, PLOr


class LDLfTruth(Truth):
    @abstractmethod
    def truth(self, i: FiniteTrace, pos: int):
        raise NotImplementedError

class RegExpTruth(Truth):
    @abstractmethod
    def truth(self, tr: FiniteTrace, start: int, end: int):
        raise NotImplementedError

class Delta(ABC):
    @abstractmethod
    def delta(self, i:PLInterpretation, epsilon=False):
        raise NotImplementedError


class LDLfFormula(Formula, LDLfTruth, NNF, Delta):

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




class LDLfCommBinaryOperator(LDLfFormula, CommutativeBinaryOperator):
    pass

class DeltaRegExp(ABC):
    @abstractmethod
    def deltaDiamond(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        raise NotImplementedError

    @abstractmethod
    def deltaBox(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        raise NotImplementedError

class RegExpFormula(Formula, RegExpTruth, NNF, DeltaRegExp):
    # this should be never called. Just for override the inherited abstract method.
    def negate(self):
        raise NotImplementedError


class LDLfTemporalFormula(LDLfFormula):
    @property
    def temporal_brackets(self)->str:
        raise NotImplementedError

    def __init__(self, r:RegExpFormula, f:LDLfFormula):
        self.r = r
        self.f = f

    def _members(self):
        return (self.temporal_brackets, self.r, self.f)

    def __str__(self):
        return self.temporal_brackets[0] + str(self.r) + self.temporal_brackets[1] + "(" + str(self.f) + ")"

    def find_labels(self):
        return self.f.find_labels().union(self.r.find_labels())


class LDLfAtomic(LDLfFormula, AtomicFormula):
    def __str__(self):
        return AtomicFormula.__str__(self)

    def find_labels(self):
        return set()


class LDLfLogicalTrue(LDLfAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.LOGICAL_TRUE.value))

    def truth(self, *args):
        return True

    def to_nnf(self):
        return self

    def negate(self):
        return LDLfLogicalFalse()

    def _delta(self, i:PLInterpretation, epsilon=False):
        return PLTrue()


class LDLfLogicalFalse(LDLfAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.LOGICAL_FALSE.value))

    def truth(self, *args):
        return False

    def to_nnf(self):
        return self

    def negate(self):
        return LDLfLogicalTrue()

    def _delta(self, i:PLInterpretation, epsilon=False):
        return PLFalse()


class LDLfNot(LDLfFormula, NotTruth, NotNNF):

    def to_nnf(self):
        return self.f.negate().to_nnf()

    def negate(self):
        return self.f

    def _delta(self, i: PLInterpretation, epsilon=False):
        # should never called, since it is called from NNF formulas
        raise Exception


class LDLfAnd(LDLfCommBinaryOperator, AndTruth):
    def to_nnf(self):
        childs = set([child.to_nnf() for child in self.formulas])
        return LDLfAnd(childs)

    def negate(self):
        childs = set([child.negate().to_nnf() for child in self.formulas])
        return LDLfOr(childs)

    def _delta(self, i:PLInterpretation, epsilon=False):
        return PLAnd({f._delta(i, epsilon) for f in self.formulas})


class LDLfOr(LDLfCommBinaryOperator, OrTruth):

    def to_nnf(self):
        childs = set([child.to_nnf() for child in self.formulas])
        return LDLfOr(childs)

    def negate(self):
        childs = set([child.negate().to_nnf() for child in self.formulas])
        return LDLfAnd(childs)

    def _delta(self, i:PLInterpretation, epsilon=False):
        return PLOr({f._delta(i, epsilon) for f in self.formulas})


class LDLfImplies(LDLfFormula, ImpliesTruth):

    def _convert(self):
        fs = self.formulas
        a, b = LDLfAnd(set(fs[:-1])), fs[-1]
        res = LDLfOr({LDLfNot(a), b})
        return res

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate().to_nnf()

    def truth(self, *args):
        return self._convert().truth(*args)

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self._convert()._delta(i, epsilon)


class LDLfEquivalence(LDLfCommBinaryOperator, EquivalenceTruth):

    def _convert(self):
        fs = self.formulas
        pos = LDLfAnd(set(fs))
        neg = LDLfOr(set(LDLfNot(f) for f in fs))

        res = LDLfOr({pos, neg})
        return res

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate()

    def truth(self, *args):
        return self._convert().truth(*args)

    def _delta(self, i: PLInterpretation, epsilon=False):
        return self._convert()._delta(i, epsilon)


class LDLfDiamond(LDLfTemporalFormula, LDLfTruth):
    temporal_brackets = "<>"

    def truth(self, i: FiniteTrace, pos: int):
        return any(self.r.truth(i, pos, j) and self.f.truth(i, j) for j in range(pos, i.length()+1))
        # length + 1 in order to include the last step

    def to_nnf(self):
        return LDLfDiamond(self.r.to_nnf(), self.f.to_nnf())

    def negate(self):
        return LDLfBox(self.r, LDLfNot(self.f))

    def _delta(self, i:PLInterpretation, epsilon=False):
        return self.r.deltaDiamond(self.f, i, epsilon)


class LDLfBox(LDLfTemporalFormula):
    temporal_brackets = "[]"

    def truth(self, i: FiniteTrace, pos: int):
        return LDLfNot(LDLfDiamond(self.r, LDLfNot(self.f))).truth(i, pos)

    def to_nnf(self):
        return LDLfBox(self.r.to_nnf(), self.f.to_nnf())

    def negate(self):
        return LDLfDiamond(self.r, LDLfNot(self.f))

    def _delta(self, i:PLInterpretation, epsilon=False):
        return self.r.deltaBox(self.f, i, epsilon)


class RegExpPropositional(RegExpFormula, PLFormula):
    def __init__(self, pl_formula:PLFormula):
        self.pl_formula = pl_formula

    def truth(self, tr: FiniteTrace, start: int, end: int):
        return end == start + 1 \
                and start < tr.length() \
                and self.pl_formula.truth(tr.get(start))

    def _members(self):
        return (RegExpPropositional, self.pl_formula)

    def __str__(self):
        return str(self.pl_formula)

    def to_nnf(self):
        return RegExpPropositional(self.pl_formula.to_nnf())

    def negate(self):
        return RegExpPropositional(self.pl_formula.negate())

    def deltaDiamond(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        if epsilon:
            return PLFalse()
        if self.pl_formula.truth(i):
            return _expand(f)
        else:
            return PLFalse()

    def deltaBox(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        if epsilon:
            return PLTrue()
        if self.pl_formula.truth(i):
            return _expand(f)
        else:
            return PLTrue()

    def find_labels(self):
        return self.pl_formula.find_labels()


class RegExpTest(RegExpFormula, UnaryOperator):
    operator_symbol = "?"

    def __init__(self, f:LDLfFormula):
        super().__init__(f)

    def truth(self, tr: FiniteTrace, start: int, end: int):
        return start == end and self.f.truth(tr, start)

    def __str__(self):
        s = super().__str__()
        s = s[1:] + s[0]
        return s

    def to_nnf(self):
        return RegExpTest(self.f.to_nnf())

    def deltaDiamond(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        return PLAnd({self.f._delta(i, epsilon), f._delta(i, epsilon)})

    def deltaBox(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        return PLOr({LDLfNot(self.f).to_nnf()._delta(i, epsilon), f._delta(i, epsilon)})

    def find_labels(self):
        return self.f.find_labels()

class RegExpUnion(RegExpFormula, CommutativeBinaryOperator):
    operator_symbol = "+"

    def truth(self, tr: FiniteTrace, start: int, end: int):
        return any(f.truth(tr, start, end) for f in self.formulas)

    def to_nnf(self):
        return RegExpUnion({r.to_nnf() for r in self.formulas})

    def deltaDiamond(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        return PLOr({LDLfDiamond(r, f)._delta(i, epsilon) for r in self.formulas})

    def deltaBox(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        return PLAnd({LDLfBox(r, f)._delta(i, epsilon) for r in self.formulas})

class RegExpSequence(RegExpFormula, BinaryOperator):
    operator_symbol = ";"

    def __init__(self, formulas: OperatorChilds):
        RegExpFormula.__init__(self)
        BinaryOperator.__init__(self, formulas)

    def truth(self, tr: FiniteTrace, start: int, end: int):
        if len(self.formulas)==2:
            return any(self.formulas[0].truth(tr, start, k) and self.formulas[1].truth(tr, k, end) for k in range(start, end + 1))
        else:
            next_sequences = RegExpSequence.__init__(self, self.formulas[1:])
            return any(self.formulas[0].truth(tr, start, k) and next_sequences.truth(tr, k, end)
                       for k in range(start, end + 1))

    def to_nnf(self):
        return RegExpSequence([r.to_nnf() for r in self.formulas])

    def deltaDiamond(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        res = LDLfDiamond(self.formulas[-1], f)
        for r in reversed(self.formulas[:-1]):
            res = LDLfDiamond(r, res)
        return res._delta(i, epsilon)

    def deltaBox(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        res = LDLfBox(self.formulas[-1], f)
        for r in reversed(self.formulas[:-1]):
            res = LDLfBox(r, res)
        return res._delta(i, epsilon)


class RegExpStar(RegExpFormula, UnaryOperator):
    operator_symbol = "*"

    def truth(self, tr: FiniteTrace, start: int, end: int):
        return start == end \
            or any(self.f.truth(tr, start, k) and self.truth(tr, k, end)
                   for k in range(start, end + 1))

    def __str__(self):
        s = super().__str__()
        s = s[1:] + s[0]
        return s

    def to_nnf(self):
        return RegExpStar(self.f.to_nnf())

    def deltaDiamond(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        return PLOr({f._delta(i, epsilon), LDLfDiamond(self.f, F(LDLfDiamond(self, f)))._delta(i, epsilon)})

    def deltaBox(self, f:LDLfFormula, i: PLInterpretation, epsilon=False):
        return PLAnd({f._delta(i, epsilon), LDLfBox(self.f, T(LDLfBox(self, f)))._delta(i, epsilon)})


class LDLfPropositional(LDLfFormula):
    def __init__(self, pl_formula:PLFormula):
        self.pl_formula = pl_formula

    def _convert(self):
        return LDLfDiamond(RegExpPropositional(self.pl_formula), LDLfLogicalTrue())

    def truth(self, i: FiniteTrace, pos: int):
        return self._convert().truth(i, pos)

    def _members(self):
        return (LDLfPropositional, self.pl_formula)

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate()

    def _delta(self, i:PLInterpretation, epsilon=False):
        return self._convert()._delta(i, epsilon)

    def find_labels(self):
        return self.pl_formula.find_labels()



class LDLfEnd(LDLfAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.END.value))

    def _convert(self):
        return LDLfBox(RegExpPropositional(PLTrue()), LDLfLogicalFalse())

    def truth(self, *args):
        return self._convert().truth(*args)

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate()

    def _delta(self, i:PLInterpretation, epsilon=False):
        return self._convert()._delta(i, epsilon)



class LDLfLast(LDLfAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.LAST.value))

    def _convert(self):
        return LDLfDiamond(RegExpPropositional(PLTrue()), LDLfEnd()._convert())

    def truth(self, *args):
        return self._convert().truth(*args)

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate()

    def _delta(self, i:PLInterpretation, epsilon=False):
        return self._convert()._delta(i, epsilon)


class F(Formula, Delta):
    def __init__(self, f: Formula):
        self.f = f

    def _members(self):
        return ("F", self.f)

    def __str__(self):
        return "_".join(map(str, self._members()))

    def delta(self, i:PLInterpretation, epsilon=False):
        return PLFalse()

    def to_nnf(self):
        return self

    def find_labels(self):
        return super().find_labels()


class T(Formula, Delta):
    def __init__(self, f: Formula):
        self.f = f

    def _members(self):
        return ("T", self.f)

    def __str__(self):
        return "_".join(map(str, self._members()))

    def delta(self, i:PLInterpretation, epsilon=False):
        return PLTrue()

    def to_nnf(self):
        return self

    def find_labels(self):
        return super().find_labels()

def _expand(f:Formula):
    if isinstance(f, F) or isinstance(f, T):
        return _expand(f.f)
    # elif isinstance(f, LDLfLogicalTrue):
    #     return PLTrue()
    # elif isinstance(f, LDLfLogicalFalse):
    #     return PLFalse()
    elif isinstance(f, LDLfDiamond) or isinstance(f, LDLfBox):
        return type(f)(f.r, _expand(f.f))
    else:
        return f


from abc import abstractmethod, ABC

from flloat.base.Formula import Formula, CommutativeBinaryOperator, UnaryOperator, BinaryOperator, OperatorChilds, \
    AtomicFormula
from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.nnf import NNF, NotNNF
from flloat.base.truths import NotTruth, AndTruth, OrTruth, ImpliesTruth, EquivalenceTruth, Truth
from flloat.semantics.ldlf import FiniteTraceInterpretation
from flloat.semantics.pl import PLInterpretation
from flloat.syntax.pl import PLFormula, PLTrue


class LDLfTruth(Truth):
    @abstractmethod
    def truth(self, i: FiniteTraceInterpretation, pos: int):
        raise NotImplementedError

class RegExpTruth(Truth):
    @abstractmethod
    def truth(self, tr: FiniteTraceInterpretation, start: int, end: int):
        raise NotImplementedError

class Delta(ABC):
    @abstractmethod
    def delta(self, i:PLInterpretation, epsilon=False):
        raise NotImplementedError

class LDLfFormula(Formula, LDLfTruth, NNF, Delta):
    pass


class LDLfCommBinaryOperator(LDLfFormula, CommutativeBinaryOperator):
    pass

class RegExpFormula(Formula, RegExpTruth, NNF, Delta):
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
        return self.temporal_brackets[0] + str(self.r) + self.temporal_brackets[1] + str(self.f)


class LDLfAtomic(LDLfFormula, AtomicFormula):
    pass


class LDLfLogicalTrue(LDLfAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.LOGICAL_TRUE.value))

    def truth(self, *args):
        return True

    def to_nnf(self):
        return self

    def negate(self):
        return LDLfLogicalFalse()


class LDLfLogicalFalse(LDLfAtomic):
    def __init__(self):
        super().__init__(Symbol(Symbols.LOGICAL_FALSE.value))

    def truth(self, *args):
        return False

    def to_nnf(self):
        return self

    def negate(self):
        return LDLfLogicalTrue()


class LDLfNot(LDLfFormula, NotTruth, NotNNF):
    operator_symbol = "~"

    def to_nnf(self):
        return self.f.negate().to_nnf()

    def negate(self):
        return self.f


class LDLfAnd(LDLfCommBinaryOperator, AndTruth):
    operator_symbol = "&"

    def to_nnf(self):
        childs = set([child.to_nnf() for child in self.formulas])
        return LDLfAnd(childs)

    def negate(self):
        childs = set([child.negate().to_nnf() for child in self.formulas])
        return LDLfOr(childs)


class LDLfOr(LDLfCommBinaryOperator, OrTruth):
    operator_symbol = "|"

    def to_nnf(self):
        childs = set([child.to_nnf() for child in self.formulas])
        return LDLfOr(childs)

    def negate(self):
        childs = set([child.negate().to_nnf() for child in self.formulas])
        return LDLfAnd(childs)


class LDLfImplies(LDLfFormula, ImpliesTruth):
    operator_symbol = "->"
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


class LDLfEquivalence(LDLfCommBinaryOperator, EquivalenceTruth):
    operator_symbol = "<->"

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


class LDLfDiamond(LDLfTemporalFormula, LDLfTruth):
    temporal_brackets = "<>"

    def truth(self, i: FiniteTraceInterpretation, pos: int):
        return any(self.r.truth(i, pos, j) and self.f.truth(i, j) for j in range(pos, i.length()+1))
        # length + 1 in order to include the last step

    def to_nnf(self):
        return LDLfDiamond(self.r.to_nnf(), self.f.to_nnf())

    def negate(self):
        return LDLfBox(self.r, LDLfNot(self.f))


class LDLfBox(LDLfTemporalFormula):
    temporal_brackets = "[]"

    def truth(self, i: FiniteTraceInterpretation, pos: int):
        return LDLfNot(LDLfDiamond(self.r, LDLfNot(self.f))).truth(i, pos)

    def to_nnf(self):
        return LDLfBox(self.r.to_nnf(), self.f.to_nnf())

    def negate(self):
        return LDLfDiamond(self.r, LDLfNot(self.f))


class RegExpPropositional(RegExpFormula, PLFormula):
    def __init__(self, pl_formula:PLFormula):
        self.pl_formula = pl_formula

    def truth(self, tr: FiniteTraceInterpretation, start: int, end: int):
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


class RegExpTest(RegExpFormula, UnaryOperator):
    operator_symbol = "?"

    def __init__(self, f:LDLfFormula):
        super().__init__(f)

    def truth(self, tr: FiniteTraceInterpretation, start: int, end: int):
        return start == end and self.f.truth(tr, start)

    def __str__(self):
        s = super().__str__()
        s = s[1:] + s[0]
        return s

    def to_nnf(self):
        return RegExpTest(self.f.to_nnf())




class RegExpUnion(RegExpFormula, CommutativeBinaryOperator):
    operator_symbol = "+"

    def truth(self, tr: FiniteTraceInterpretation, start: int, end: int):
        return any(f.truth(tr, start, end) for f in self.formulas)

    def to_nnf(self):
        return RegExpUnion({r.to_nnf() for r in self.formulas})


class RegExpSequence(RegExpFormula, BinaryOperator):
    operator_symbol = ";"

    def __init__(self, formulas: OperatorChilds):
        RegExpFormula.__init__(self)
        BinaryOperator.__init__(self, formulas)

    def truth(self, tr: FiniteTraceInterpretation, start: int, end: int):
        if len(self.formulas)==2:
            return any(self.formulas[0].truth(tr, start, k) and self.formulas[1].truth(tr, k, end) for k in range(start, end + 1))
        else:
            next_sequences = RegExpSequence.__init__(self, self.formulas[1:])
            return any(self.formulas[0].truth(tr, start, k) and next_sequences.truth(tr, k, end)
                       for k in range(start, end + 1))

    def to_nnf(self):
        return RegExpSequence([r.to_nnf() for r in self.formulas])


class RegExpStar(RegExpFormula, UnaryOperator):
    operator_symbol = "*"

    def truth(self, tr: FiniteTraceInterpretation, start: int, end: int):
        return start == end \
            or any(self.f.truth(tr, start, k) and self.truth(tr, k, end)
                   for k in range(start, end + 1))

    def __str__(self):
        s = super().__str__()
        s = s[1:] + s[0]
        return s

    def to_nnf(self):
        return RegExpStar(self.f.to_nnf())


class LDLfPropositional(LDLfFormula):
    def __init__(self, pl_formula:PLFormula):
        self.pl_formula = pl_formula

    def _convert(self):
        return LDLfDiamond(RegExpPropositional(self.pl_formula), LDLfLogicalTrue())

    def truth(self, i: FiniteTraceInterpretation, pos: int):
        return self._convert().truth(i, pos)

    def _members(self):
        return (LDLfPropositional, self.pl_formula)

    def to_nnf(self):
        return self._convert().to_nnf()

    def negate(self):
        return self._convert().negate()




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

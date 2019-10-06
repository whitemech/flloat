# -*- coding: utf-8 -*-
from flloat.parser.ldlf import LDLfParser
from flloat.semantics.traces import FiniteTrace
from flloat.semantics.pl import PLInterpretation, PLFalseInterpretation
from flloat.ldlf import LDLfLogicalTrue, LDLfLogicalFalse, LDLfNot, LDLfAnd, LDLfPropositional, \
    RegExpPropositional, LDLfDiamond, LDLfEquivalence, LDLfBox, RegExpStar, LDLfOr, RegExpUnion, RegExpSequence, \
    LDLfEnd, RegExpTest, LDLfLast
from flloat.pl import PLAnd, PLAtomic, PLNot, PLEquivalence, PLFalse, PLTrue


def test_parser():
    parser = LDLfParser()
    sa, sb = "A", "B"
    a, b = PLAtomic(sa), PLAtomic(sb)

    tt = LDLfLogicalTrue()
    ff = LDLfLogicalFalse()
    true = PLTrue()
    false = PLFalse()
    r_true = RegExpPropositional(true)
    r_false = RegExpPropositional(false)

    assert tt == parser("tt")
    assert ff == parser("ff")
    assert LDLfDiamond(r_true, tt) == parser("<true>tt")
    assert LDLfDiamond(r_false, tt) == parser("<false>tt")
    assert parser("!!tt && <!A&B>tt") == LDLfAnd(
        [LDLfNot(tt), LDLfDiamond(RegExpPropositional(PLAnd([PLNot(a), b])), tt)])

    assert parser("[true*]([true]ff || <!A>tt || <(true)*><B>tt)") == \
           LDLfBox(RegExpStar(r_true),
                   LDLfOr([
                       LDLfBox(r_true, ff),
                       LDLfDiamond(RegExpPropositional(PLNot(a)), tt),
                       LDLfDiamond(RegExpStar(r_true), (LDLfDiamond(RegExpPropositional(b), tt)))
                   ])
                   )

    assert parser("[A&B&A]ff <--> <A&B&A>tt") == LDLfEquivalence([
        LDLfBox(RegExpPropositional(PLAnd([a, b, a])), ff),
        LDLfDiamond(RegExpPropositional(PLAnd([a, b, a])), tt),
    ])

    assert parser("<A+B>tt") == LDLfDiamond(RegExpUnion([RegExpPropositional(a), RegExpPropositional(b)]), tt)
    assert parser("<A;B>tt") == LDLfDiamond(RegExpSequence([RegExpPropositional(a), RegExpPropositional(b)]), tt)
    assert parser("<A+(B;A)>end") == LDLfDiamond(
        RegExpUnion([RegExpPropositional(a), RegExpSequence([RegExpPropositional(b), RegExpPropositional(a)])]),
        LDLfEnd()
    )

    assert parser("!!(<!(A<->D)+(B;C)*+(!!last)?>[(true)*]end)") == LDLfNot(
        LDLfDiamond(
            RegExpUnion([
                RegExpPropositional(PLNot(PLEquivalence([PLAtomic("A"), PLAtomic("D")]))),
                RegExpStar(RegExpSequence([
                    RegExpPropositional(PLAtomic("B")),
                    RegExpPropositional(PLAtomic("C")),
                ])),
                RegExpTest(LDLfNot(LDLfLast()))
            ]),
            LDLfBox(
                RegExpStar(RegExpPropositional(PLTrue())),
                LDLfEnd()
            )
        )
    )


def test_truth():
    sa, sb = "a", "b"
    a, b = PLAtomic(sa), PLAtomic(sb)

    i_ = PLFalseInterpretation()
    i_a = PLInterpretation({sa})
    i_b = PLInterpretation({sb})
    i_ab = PLInterpretation({sa, sb})

    tr_false_a_b_ab = FiniteTrace([
        i_,
        i_a,
        i_b,
        i_ab,
        i_
    ])

    tt = LDLfLogicalTrue()
    ff = LDLfLogicalFalse()

    assert tt.truth(tr_false_a_b_ab, 0)
    assert not ff.truth(tr_false_a_b_ab, 0)
    assert not LDLfNot(tt).truth(tr_false_a_b_ab, 0)
    assert LDLfNot(ff).truth(tr_false_a_b_ab, 0)
    assert LDLfAnd([LDLfPropositional(a), LDLfPropositional(b)]).truth(tr_false_a_b_ab, 3)
    assert not LDLfDiamond(RegExpPropositional(PLAnd([a, b])), tt).truth(tr_false_a_b_ab, 0)

    parser = LDLfParser()
    trace = FiniteTrace.from_symbol_sets([
        {},
        {"A"},
        {"A"},
        {"A", "B"},
        {}
    ])

    formula = "<true*;A&B>tt"
    parsed_formula = parser(formula)
    assert parsed_formula.truth(trace, 0)

    formula = "[(A+!B)*]<C>tt"
    parsed_formula = parser(formula)
    assert not parsed_formula.truth(trace, 1)

    formula = "<(<!C>tt)?><A>tt"
    parsed_formula = parser(formula)
    assert parsed_formula.truth(trace, 1)

    formula = "<!C+A>tt"
    parsed_formula = parser(formula)
    assert parsed_formula.truth(trace, 1)


def test_nnf():
    parser = LDLfParser()

    assert parser("!!tt").to_nnf() == LDLfLogicalFalse()
    assert parser("!!!!tt").to_nnf() == LDLfLogicalTrue()

    assert parser("!!(<!(A&B)>end)").to_nnf() == parser("[!A | !B]<true>tt")

    f = parser("!!(<!(A<->D)+(B;C)*+(!!last)?>[(true)*]end)")
    assert f.to_nnf() == parser("[(([true]<true>tt)? + ((B ; C))* + ((A | D) & (!(D) | !(A))))]<(true)*><true>tt")
    assert f.to_nnf() == f.to_nnf().to_nnf().to_nnf().to_nnf()


def test_delta():
    parser = LDLfParser()
    sa, sb, sc = "A", "B", "C"
    a, b, c = PLAtomic(sa), PLAtomic(sb), PLAtomic(sc)

    i_ = PLFalseInterpretation()
    i_a = PLInterpretation({sa})
    i_b = PLInterpretation({sb})
    i_ab = PLInterpretation({sa, sb})

    true = PLTrue()
    false = PLFalse()
    tt = LDLfLogicalTrue()
    ff = LDLfLogicalFalse()

    assert parser("<A>tt").delta(i_) == false
    assert parser("<A>tt").delta(i_a) == PLAtomic(tt)
    assert parser("<A>tt").delta(i_b) == false
    assert parser("<A>tt").delta(i_ab) == PLAtomic(tt)

    assert parser("[B]ff").delta(i_) == true
    assert parser("[B]ff").delta(i_a) == true
    assert parser("[B]ff").delta(i_b) == PLAtomic(ff)
    assert parser("[B]ff").delta(i_ab) == PLAtomic(ff)

    f = parser("!!(<!(A<->B)+(B;A)*+(!!last)?>[(true)*]end)")
    assert f.delta(i_) == f.to_nnf().delta(i_)
    assert f.delta(i_ab) == f.to_nnf().delta(i_ab)

    assert f.delta(i_, epsilon=True) == f.to_nnf().delta(i_, epsilon=True)
    assert f.delta(i_ab, epsilon=True) == f.to_nnf().delta(i_ab, epsilon=True)
    # with epsilon=True, the result is either PLTrue or PLFalse
    assert f.delta(i_, epsilon=True) in [PLTrue(), PLFalse()]


def test_find_labels():
    parser = LDLfParser()

    f = "< (!(A | B | C ))* ; (A | C) ; (!(A | B | C))* ; (B | C) ><true>tt"
    formula = parser(f)
    assert formula.find_labels() == {c for c in "ABC"}

    f = "(<((((<B>tt)?);true)*) ; ((<(A & B)>tt) ?)>tt)"
    formula = parser(f)
    assert formula.find_labels() == {c for c in "AB"}


class TestToAutomaton:

    @classmethod
    def setup_class(cls):
        cls.parser = LDLfParser()
        cls.i_ = PLInterpretation(set())
        cls.i_a = PLInterpretation({"A"})
        cls.i_b = PLInterpretation({"B"})
        cls.i_ab = PLInterpretation({"A", "B"})

    def test_diamond(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser("<A+!B>tt").to_automaton()

        assert not dfa.accepts([])
        assert dfa.accepts([i_, i_b])
        assert dfa.accepts([i_a])
        assert not dfa.accepts([i_b])
        assert dfa.accepts([i_a, i_, i_ab, i_b])
        assert not dfa.accepts([i_b, i_ab])

    def test_diamond_eventually(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser("<true*;B>tt").to_automaton(labels={"A", "B"})

        assert not dfa.accepts([])
        assert dfa.accepts([i_, i_b])
        assert not dfa.accepts([i_a])
        assert dfa.accepts([i_b])
        assert dfa.accepts([i_a, i_, i_ab, i_b])
        assert dfa.accepts([i_b, i_ab])

    def test_diamond_sequence(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser("< (!(A | B | C ))* ; (A | C) ; (!(A | B | C))* ; (B | C) ><true>tt").to_automaton()

        assert not dfa.accepts([])
        assert not dfa.accepts([i_, i_b])
        assert dfa.accepts([i_a, i_b, i_])
        assert dfa.accepts([i_, i_, i_, i_, i_a, i_, i_ab, i_, i_])
        assert not dfa.accepts([i_b, i_b])

    def test_diamond_test(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser("(<((((<B>tt)?);true)*) ; ((<(A & B)>tt) ?)>tt)").to_automaton()

        assert not dfa.accepts([])
        assert not dfa.accepts([i_b, i_b, i_b])
        assert dfa.accepts([i_b, i_b, i_ab])

    def test_diamond_and_box(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser("(<true>tt) && ([A]<B>tt)").to_automaton()

        assert not dfa.accepts([])
        assert dfa.accepts([i_b])
        assert dfa.accepts([i_])
        assert not dfa.accepts([i_a])
        assert not dfa.accepts([i_ab])
        assert dfa.accepts([i_ab, i_ab])
        assert dfa.accepts([i_a, i_b])

    def test_box_safety(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser("[true*](<A>tt --> <true*><B>tt)").to_automaton()

        assert dfa.accepts([])
        assert dfa.accepts([i_b])
        assert dfa.accepts([i_])
        assert not dfa.accepts([i_a])
        assert dfa.accepts([i_ab])
        assert dfa.accepts([i_ab, i_ab])
        assert dfa.accepts([i_a, i_b])
        assert not dfa.accepts([i_a, i_a])

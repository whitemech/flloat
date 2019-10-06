# -*- coding: utf-8 -*-
from flloat.parser.ltlf import LTLfParser
from flloat.semantics.traces import FiniteTrace
from flloat.semantics.pl import PLFalseInterpretation, PLInterpretation
from flloat.ltlf import LTLfAtomic, LTLfAnd, LTLfEquivalence, LTLfOr, LTLfNot, LTLfImplies, LTLfEventually, \
    LTLfAlways, LTLfUntil, LTLfRelease, LTLfNext, LTLfWeakNext, LTLfTrue, LTLfFalse
from flloat.pl import PLAtomic, PLTrue, PLFalse, PLAnd, PLOr


def test_parser():
    parser = LTLfParser()
    a, b, c = [LTLfAtomic(c) for c in "ABC"]

    assert parser("!A | B <-> !(A & !B) <-> A->B") == LTLfEquivalence([
        LTLfOr([LTLfNot(a), b]),
        LTLfNot(LTLfAnd([a, LTLfNot(b)])),
        LTLfImplies([a, b])
    ])

    assert parser("(X A) & (WX !B)") == LTLfAnd([
        LTLfNext(a),
        LTLfWeakNext(LTLfNot(b))
    ])

    assert parser("(F (A&B)) <-> !(G (!A | !B) )") == LTLfEquivalence([
        LTLfEventually(LTLfAnd([a, b])),
        LTLfNot(LTLfAlways(LTLfOr([LTLfNot(a), LTLfNot(b)])))
    ])

    assert parser("(A U B U C) <-> !(!A R !B R !C)") == LTLfEquivalence([
        LTLfUntil([a, b, c]),
        LTLfNot(LTLfRelease([LTLfNot(a), LTLfNot(b), LTLfNot(c)]))
    ])


class TestTruth:

    @classmethod
    def setup_class(cls):
        cls.parser = LTLfParser()

        cls.trace = FiniteTrace.from_symbol_sets([
            {"A"},
            {"A"},
            {"B"},
            {"B"},
            {"C"},
            {"C"},
        ])

    def test_truth_next(self):
        parser = self.parser
        t = self.trace

        assert parser("X A").truth(t, 0)
        assert not parser("X A").truth(t, 1)
        assert parser("X B").truth(t, 1)
        assert parser("X C").truth(t, 4)
        # at the last step, Next != WeakNext
        assert not parser("X C").truth(t, 5)

    def test_truth_weaknext(self):
        parser = self.parser
        t = self.trace

        assert not parser("WX A").truth(t, 1)
        assert parser("WX C").truth(t, 5)

    def test_until(self):
        parser = self.parser
        t = self.trace

        assert parser("A U B U C").truth(t, 0)
        assert parser("A U B U C").truth(t, 2)
        assert parser("A U B U C").truth(t, 4)
        assert not parser("A U B U C").truth(t, 10)

        assert not parser("A U C").truth(t, 0)
        assert not parser("C U B").truth(t, 0)

    def test_release(self):
        parser = self.parser
        t = self.trace

        assert not parser("(!A R !B R !C)").truth(t, 0)
        assert not parser("(!A R !B R !C)").truth(t, 2)
        assert not parser("(!A R !B R !C)").truth(t, 4)
        assert parser("(!A R !B R !C)").truth(t, 10)

    def test_eventually(self):
        parser = self.parser
        t = self.trace

        assert parser("F C & !A & !B").truth(t, 0)
        assert not parser("F A & B & C").truth(t, 0)
        assert parser("F G C").truth(t, 0)
        assert not parser("F G B").truth(t, 0)

    def test_always(self):
        parser = self.parser
        t = self.trace

        assert parser("G A | B | C").truth(t, 0)
        assert parser("G F (C & !A & !B)").truth(t, 0)
        assert not parser("G C").truth(t, 0)
        assert parser("G C").truth(t, 4)
        assert parser("G C").truth(t, 10)
        assert parser("G F C").truth(t, 0)


def test_nnf():
    parser = LTLfParser()
    a, b, c = [LTLfAtomic(c) for c in "ABC"]

    f = parser("!(A & !B)")
    assert f.to_nnf() == LTLfOr([LTLfNot(a), b])

    f = parser("!(!A | B)")
    assert f.to_nnf() == LTLfAnd([a, LTLfNot(b)])

    f = parser("!( (A->B) <-> (!A | B))")
    assert f.to_nnf() == LTLfAnd([
        LTLfAnd([a, LTLfNot(b)]),
        LTLfOr([LTLfNot(a), b]),
    ])

    # Next and Weak Next
    f = parser("!(X (A & B))")
    assert f.to_nnf() == LTLfWeakNext(LTLfOr([LTLfNot(a), LTLfNot(b)]))

    f = parser("!(WX (A & B))")
    assert f.to_nnf() == LTLfNext(LTLfOr([LTLfNot(a), LTLfNot(b)]))

    # Eventually and Always
    f = parser("!(F (A | B))")
    assert f.to_nnf() == LTLfAlways(LTLfAnd([LTLfNot(a), LTLfNot(b)])).to_nnf()

    f = parser("!(F (A | B))")
    assert f.to_nnf() == LTLfAlways(LTLfAnd([LTLfNot(a), LTLfNot(b)])).to_nnf()
    f = parser("!(G (A | B))")
    assert f.to_nnf() == LTLfEventually(LTLfAnd([LTLfNot(a), LTLfNot(b)])).to_nnf()

    # Until and Release
    f = parser("!(A U B)")
    assert f.to_nnf() == LTLfRelease([LTLfNot(a), LTLfNot(b)])
    f = parser("!(A R B)")
    assert f.to_nnf() == LTLfUntil([LTLfNot(a), LTLfNot(b)])


class TestDelta:

    @classmethod
    def setup_class(cls):
        cls.parser = LTLfParser()
        cls.i_, cls.i_a, cls.i_b, cls.i_ab = \
            PLFalseInterpretation(), PLInterpretation({"A"}), PLInterpretation({"B"}), PLInterpretation({"A", "B"})
        cls.true = PLTrue()
        cls.false = PLFalse()

    def test_atomic(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("A").delta(i_) == false
        assert parser("A").delta(i_a) == true
        assert parser("A").delta(i_b) == false
        assert parser("A").delta(i_ab) == true
        assert parser("A").delta(None, epsilon=True) == false

    def test_not(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("!A").delta(i_) == true
        assert parser("!A").delta(i_a) == false
        assert parser("!A").delta(i_b) == true
        assert parser("!A").delta(i_ab) == false
        assert parser("!A").delta(None, epsilon=True) == false

    def test_and(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("A & B").delta(i_) == PLAnd([false, false])
        assert parser("A & B").delta(i_a) == PLAnd([true, false])
        assert parser("A & B").delta(i_b) == PLAnd([false, true])
        assert parser("A & B").delta(i_ab) == PLAnd([true, true])
        assert parser("A & B").delta(i_, epsilon=True) == false

    def test_or(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("A | B").delta(i_) == PLOr([false, false])
        assert parser("A | B").delta(i_a) == PLOr([true, false])
        assert parser("A | B").delta(i_b) == PLOr([false, true])
        assert parser("A | B").delta(i_ab) == PLOr([true, true])
        assert parser("A | B").delta(i_, epsilon=True) == false

    def test_next(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("X A").delta(i_) == PLAnd(
            [PLAtomic(LTLfAtomic("A")), PLAtomic(LTLfEventually(LTLfTrue()).to_nnf())])
        assert parser("X A").delta(i_a) == PLAnd(
            [PLAtomic(LTLfAtomic("A")), PLAtomic(LTLfEventually(LTLfTrue()).to_nnf())])
        assert parser("X A").delta(i_b) == PLAnd(
            [PLAtomic(LTLfAtomic("A")), PLAtomic(LTLfEventually(LTLfTrue()).to_nnf())])
        assert parser("X A").delta(i_ab) == PLAnd(
            [PLAtomic(LTLfAtomic("A")), PLAtomic(LTLfEventually(LTLfTrue()).to_nnf())])
        assert parser("X A").delta(i_, epsilon=True) == false

    def test_until(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("A U B").delta(i_a) == PLOr([
            false,
            PLAnd([
                true,
                PLAtomic(LTLfUntil([LTLfAtomic("A"), LTLfAtomic("B")])),
                PLAtomic(LTLfEventually(LTLfTrue()).to_nnf())
            ])
        ])
        assert parser("A U B").delta(i_ab, epsilon=True) == false

    def test_release(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("A R B").delta(i_a) == PLAnd([
            false,
            PLOr([
                true,
                PLAtomic(LTLfRelease([LTLfAtomic("A"), LTLfAtomic("B")])),
                PLAtomic(LTLfAlways(LTLfFalse()).to_nnf())
            ])
        ])
        assert parser("A R B").delta(i_ab, epsilon=True) == true

    def test_eventually(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("F A").delta(i_a) == PLOr(
            [true, PLAnd([
                PLAtomic(LTLfEventually(LTLfTrue()).to_nnf()),
                true,
                PLAtomic(LTLfUntil([LTLfTrue(), LTLfAtomic("A")]))
            ])])
        assert parser("F A").delta(i_) == PLOr(
            [false, PLAnd([
                PLAtomic(LTLfEventually(LTLfTrue()).to_nnf()),
                true,
                PLAtomic(LTLfUntil([LTLfTrue(), LTLfAtomic("A")]))
            ])])
        assert parser("F A").delta(i_a, epsilon=True) == false
        assert parser("F A").delta(i_, epsilon=True) == false

    def test_always(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("G A").delta(i_a) == PLAnd(
            [true, PLOr([false, PLAtomic(LTLfAlways(LTLfFalse()).to_nnf()), PLAtomic(LTLfRelease([LTLfFalse(), LTLfAtomic("A")]))])])
        assert parser("G A").delta(i_a) == PLAnd(
            [true, PLOr([false, PLAtomic(LTLfAlways(LTLfFalse()).to_nnf()), PLAtomic(LTLfRelease([LTLfFalse(), LTLfAtomic("A")]))])])
        assert parser("G A").delta(i_a, epsilon=True) == true
        assert parser("G A").delta(i_, epsilon=True) == true


class TestToAutomaton:

    @classmethod
    def setup_class(cls):
        cls.parser = LTLfParser()
        cls.a, cls.b, cls.c = "A", "B", "C"
        cls.alphabet_abc = {cls.a, cls.b, cls.c}

        cls.i_ = PLInterpretation(set())
        cls.i_a = PLInterpretation({cls.a})
        cls.i_b = PLInterpretation({cls.b})
        cls.i_ab = PLInterpretation({cls.a, cls.b})

    def test_atomic(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        ltlf = parser("A")
        dfa = ltlf.to_automaton(labels={"A", "B"})

        assert not dfa.accepts([])
        assert not dfa.accepts([i_])
        assert dfa.accepts([i_a])
        assert not dfa.accepts([i_b])
        assert dfa.accepts([i_ab])
        assert not dfa.accepts([i_, i_])
        assert not dfa.accepts([i_, i_a])
        assert not dfa.accepts([i_, i_b])
        assert not dfa.accepts([i_, i_ab])
        assert dfa.accepts([i_a, i_])
        assert dfa.accepts([i_a, i_a])
        assert dfa.accepts([i_a, i_b])
        assert dfa.accepts([i_a, i_ab])
        assert not dfa.accepts([i_b, i_])
        assert not dfa.accepts([i_b, i_a])
        assert not dfa.accepts([i_b, i_b])
        assert not dfa.accepts([i_b, i_ab])
        assert dfa.accepts([i_ab, i_])
        assert dfa.accepts([i_ab, i_a])
        assert dfa.accepts([i_ab, i_b])
        assert dfa.accepts([i_ab, i_ab])

        assert dfa.accepts([i_a, i_, i_ab, i_b])
        assert not dfa.accepts([i_, i_ab])

    def test_next(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        ltlf = parser("X A")
        dfa = ltlf.to_automaton(labels={"A", "B"})

        assert not dfa.accepts([])
        assert not dfa.accepts([i_])
        assert not dfa.accepts([i_a])
        assert not dfa.accepts([i_b])
        assert not dfa.accepts([i_ab])
        assert not dfa.accepts([i_, i_])
        assert dfa.accepts([i_, i_a])
        assert not dfa.accepts([i_, i_b])
        assert dfa.accepts([i_, i_ab])
        assert not dfa.accepts([i_a, i_])
        assert dfa.accepts([i_a, i_a])
        assert not dfa.accepts([i_a, i_b])
        assert dfa.accepts([i_a, i_ab])
        assert not dfa.accepts([i_b, i_])
        assert dfa.accepts([i_b, i_a])
        assert not dfa.accepts([i_b, i_b])
        assert dfa.accepts([i_b, i_ab])
        assert not dfa.accepts([i_ab, i_])
        assert dfa.accepts([i_ab, i_a])
        assert not dfa.accepts([i_ab, i_b])
        assert dfa.accepts([i_ab, i_ab])

        assert not dfa.accepts([i_a, i_b, i_])
        assert not dfa.accepts([i_, i_, i_, i_, i_a, i_, i_ab, i_, i_])

    def test_weak_next(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        ltlf = parser("WX A")
        dfa = ltlf.to_automaton(labels={"A", "B"})

        assert dfa.accepts([])
        assert dfa.accepts([i_])
        assert dfa.accepts([i_a])
        assert dfa.accepts([i_b])
        assert dfa.accepts([i_ab])
        assert not dfa.accepts([i_, i_])
        assert dfa.accepts([i_, i_a])
        assert not dfa.accepts([i_, i_b])
        assert dfa.accepts([i_, i_ab])
        assert not dfa.accepts([i_a, i_])
        assert dfa.accepts([i_a, i_a])
        assert not dfa.accepts([i_a, i_b])
        assert dfa.accepts([i_a, i_ab])
        assert not dfa.accepts([i_b, i_])
        assert dfa.accepts([i_b, i_a])
        assert not dfa.accepts([i_b, i_b])
        assert dfa.accepts([i_b, i_ab])
        assert not dfa.accepts([i_ab, i_])
        assert dfa.accepts([i_ab, i_a])
        assert not dfa.accepts([i_ab, i_b])
        assert dfa.accepts([i_ab, i_ab])

        assert dfa.accepts([i_b])
        assert not dfa.accepts([i_b, i_b, i_b])
        assert dfa.accepts([i_b, i_a, i_ab])

    def test_until(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        ltlf = parser("A U B")
        dfa = ltlf.to_automaton(labels={"A", "B"})

        assert not dfa.accepts([])
        assert not dfa.accepts([i_])
        assert not dfa.accepts([i_a])
        assert dfa.accepts([i_b])
        assert dfa.accepts([i_ab])
        assert not dfa.accepts([i_, i_])
        assert not dfa.accepts([i_, i_a])
        assert not dfa.accepts([i_, i_b])
        assert not dfa.accepts([i_, i_ab])
        assert not dfa.accepts([i_a, i_])
        assert not dfa.accepts([i_a, i_a])
        assert dfa.accepts([i_a, i_b])
        assert dfa.accepts([i_a, i_ab])
        assert dfa.accepts([i_b, i_])
        assert dfa.accepts([i_b, i_a])
        assert dfa.accepts([i_b, i_b])
        assert dfa.accepts([i_b, i_ab])
        assert dfa.accepts([i_ab, i_])
        assert dfa.accepts([i_ab, i_a])
        assert dfa.accepts([i_ab, i_b])
        assert dfa.accepts([i_ab, i_ab])

    def test_release(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        ltlf = parser("!A R !B")
        dfa = ltlf.to_automaton(labels={"A", "B"})

        assert dfa.accepts([])
        assert dfa.accepts([i_])
        assert dfa.accepts([i_a])
        assert not dfa.accepts([i_b])
        assert not dfa.accepts([i_ab])
        assert dfa.accepts([i_, i_])
        assert dfa.accepts([i_, i_a])
        assert dfa.accepts([i_, i_b])
        assert dfa.accepts([i_, i_ab])
        assert dfa.accepts([i_a, i_])
        assert dfa.accepts([i_a, i_a])
        assert not dfa.accepts([i_a, i_b])
        assert not dfa.accepts([i_a, i_ab])
        assert not dfa.accepts([i_b, i_])
        assert not dfa.accepts([i_b, i_a])
        assert not dfa.accepts([i_b, i_b])
        assert not dfa.accepts([i_b, i_ab])
        assert not dfa.accepts([i_ab, i_])
        assert not dfa.accepts([i_ab, i_a])
        assert not dfa.accepts([i_ab, i_b])
        assert not dfa.accepts([i_ab, i_ab])

    def test_eventually(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        ltlf = parser("F A")
        dfa = ltlf.to_automaton(labels={"A", "B"})

        assert not dfa.accepts([])
        assert not dfa.accepts([i_])
        assert dfa.accepts([i_a])
        assert not dfa.accepts([i_b])
        assert dfa.accepts([i_ab])
        assert not dfa.accepts([i_, i_])
        assert dfa.accepts([i_, i_a])
        assert not dfa.accepts([i_, i_b])
        assert dfa.accepts([i_, i_ab])
        assert dfa.accepts([i_a, i_])
        assert dfa.accepts([i_a, i_a])
        assert dfa.accepts([i_a, i_b])
        assert dfa.accepts([i_a, i_ab])
        assert not dfa.accepts([i_b, i_])
        assert dfa.accepts([i_b, i_a])
        assert not dfa.accepts([i_b, i_b])
        assert dfa.accepts([i_b, i_ab])
        assert dfa.accepts([i_ab, i_])
        assert dfa.accepts([i_ab, i_a])
        assert dfa.accepts([i_ab, i_b])
        assert dfa.accepts([i_ab, i_ab])

        assert not dfa.accepts([i_b, i_b, i_b])
        assert dfa.accepts([i_b, i_a, i_ab])

    def test_always(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        ltlf = parser("G A")
        dfa = ltlf.to_automaton(labels={"A", "B"})

        assert dfa.accepts([])
        assert not dfa.accepts([i_])
        assert dfa.accepts([i_a])
        assert not dfa.accepts([i_b])
        assert dfa.accepts([i_ab])
        assert not dfa.accepts([i_, i_])
        assert not dfa.accepts([i_, i_a])
        assert not dfa.accepts([i_, i_b])
        assert not dfa.accepts([i_, i_ab])
        assert not dfa.accepts([i_a, i_])
        assert dfa.accepts([i_a, i_a])
        assert not dfa.accepts([i_a, i_b])
        assert dfa.accepts([i_a, i_ab])
        assert not dfa.accepts([i_b, i_])
        assert not dfa.accepts([i_b, i_a])
        assert not dfa.accepts([i_b, i_b])
        assert not dfa.accepts([i_b, i_ab])
        assert not dfa.accepts([i_ab, i_])
        assert dfa.accepts([i_ab, i_a])
        assert not dfa.accepts([i_ab, i_b])
        assert dfa.accepts([i_ab, i_ab])

        assert not dfa.accepts([i_b, i_b, i_b])
        assert not dfa.accepts([i_b, i_a, i_ab])
        assert dfa.accepts([i_a, i_a, i_ab])
        assert not dfa.accepts([i_a, i_a, i_ab, i_b])
        assert dfa.accepts([i_a, i_a, i_ab, i_a])

    def test_always_a_implies_next_b(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        ltlf = parser("G (A -> X(B) )")
        dfa = ltlf.to_automaton(labels={"A", "B"})

        assert dfa.accepts([])
        assert dfa.accepts([i_])
        assert not dfa.accepts([i_a])
        assert dfa.accepts([i_b])
        assert not dfa.accepts([i_ab])
        assert dfa.accepts([i_, i_])
        assert not dfa.accepts([i_, i_a])
        assert dfa.accepts([i_, i_b])
        assert not dfa.accepts([i_, i_ab])
        assert not dfa.accepts([i_a, i_])
        assert not dfa.accepts([i_a, i_a])
        assert dfa.accepts([i_a, i_b])
        assert not dfa.accepts([i_a, i_ab])
        assert dfa.accepts([i_b, i_])
        assert not dfa.accepts([i_b, i_a])
        assert dfa.accepts([i_b, i_b])
        assert not dfa.accepts([i_b, i_ab])
        assert not dfa.accepts([i_ab, i_])
        assert not dfa.accepts([i_ab, i_a])
        assert dfa.accepts([i_ab, i_b])
        assert not dfa.accepts([i_ab, i_ab])

        assert dfa.accepts([i_b, i_b, i_b])
        assert not dfa.accepts([i_b, i_a, i_ab])
        assert not dfa.accepts([i_a, i_a, i_ab])
        assert not dfa.accepts([i_a, i_a, i_ab, i_b])
        assert dfa.accepts([i_a, i_ab, i_ab, i_b])
        assert not dfa.accepts([i_a, i_ab, i_ab, i_])
        # very important
        assert dfa.accepts([i_a, i_ab, i_ab, i_b, i_])
        assert dfa.accepts([i_a, i_ab, i_ab, i_b, i_b])
        assert dfa.accepts([i_a, i_ab, i_b, i_ab, i_b])

    def test_always_a_implies_next_always_b(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        ltlf = parser("G (A -> X(B))")
        dfa = ltlf.to_automaton(labels={"A", "B"})

        assert dfa.accepts([])
        assert dfa.accepts([i_])
        assert not dfa.accepts([i_a])
        assert dfa.accepts([i_b])
        assert not dfa.accepts([i_ab])
        assert dfa.accepts([i_, i_])
        assert not dfa.accepts([i_, i_a])
        assert dfa.accepts([i_, i_b])
        assert not dfa.accepts([i_, i_ab])
        assert not dfa.accepts([i_a, i_])
        assert not dfa.accepts([i_a, i_a])
        assert dfa.accepts([i_a, i_b])
        assert not dfa.accepts([i_a, i_ab])
        assert dfa.accepts([i_b, i_])
        assert not dfa.accepts([i_b, i_a])
        assert dfa.accepts([i_b, i_b])
        assert not dfa.accepts([i_b, i_ab])
        assert not dfa.accepts([i_ab, i_])
        assert not dfa.accepts([i_ab, i_a])
        assert dfa.accepts([i_ab, i_b])
        assert not dfa.accepts([i_ab, i_ab])

        assert dfa.accepts([i_b, i_b, i_b])
        assert not dfa.accepts([i_b, i_a, i_ab])
        assert not dfa.accepts([i_a, i_a, i_ab])
        assert not dfa.accepts([i_a, i_a, i_ab, i_b])
        assert dfa.accepts([i_a, i_ab, i_ab, i_b])
        assert not dfa.accepts([i_a, i_ab, i_ab, i_])

        assert dfa.accepts([i_a, i_ab, i_ab, i_b, i_])
        assert dfa.accepts([i_a, i_ab, i_ab, i_b, i_b])
        assert dfa.accepts([i_a, i_ab, i_b, i_ab, i_b])

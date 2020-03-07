# -*- coding: utf-8 -*-
import pytest
import os
import lark
from hypothesis import given

from flloat.ltlf import (
    LTLfAtomic,
    LTLfAnd,
    LTLfEquivalence,
    LTLfOr,
    LTLfNot,
    LTLfImplies,
    LTLfEventually,
    LTLfAlways,
    LTLfUntil,
    LTLfRelease,
    LTLfNext,
    LTLfWeakNext,
    LTLfTrue,
    LTLfFalse,
)
from flloat.parser.ltlf import LTLfParser
from flloat.pl import PLAtomic, PLTrue, PLFalse, PLAnd, PLOr
from tests.conftest import ltlf_formulas
from tests.strategies import propositional_words
from .parsing import ParsingCheck

parser = LTLfParser()


def test_parser():
    parser = LTLfParser()
    a, b, c = [LTLfAtomic(c) for c in "abc"]

    assert parser("!a | b <-> !(a & !b) <-> a->b") == LTLfEquivalence(
        [
            LTLfOr([LTLfNot(a), b]),
            LTLfNot(LTLfAnd([a, LTLfNot(b)])),
            LTLfImplies([a, b]),
        ]
    )

    assert parser("(X a) & (WX !b)") == LTLfAnd([LTLfNext(a), LTLfWeakNext(LTLfNot(b))])

    assert parser("(F (a&b)) <-> !(G (!a | !b) )") == LTLfEquivalence(
        [
            LTLfEventually(LTLfAnd([a, b])),
            LTLfNot(LTLfAlways(LTLfOr([LTLfNot(a), LTLfNot(b)]))),
        ]
    )

    assert parser("(a U b U c) <-> !(!a R !b R !c)") == LTLfEquivalence(
        [
            LTLfUntil([a, b, c]),
            LTLfNot(LTLfRelease([LTLfNot(a), LTLfNot(b), LTLfNot(c)])),
        ]
    )


def test_names():

    good = ["a", "b", "name", "complex_name", "proposition10"]
    bad = ["Future", "X", "$", "", "40a", "niceName"]

    for name in good:
        str(LTLfAtomic(name)) == name

    for name in bad:
        with pytest.raises(ValueError):
            str(LTLfAtomic(name)) == name


class TestTruth:
    @classmethod
    def setup_class(cls):
        cls.parser = LTLfParser()

        cls.trace = [
            {"a": True},
            {"a": True},
            {"b": True},
            {"b": True},
            {"c": True},
            {"c": True},
        ]

    def test_truth_next(self):
        parser = self.parser
        t = self.trace

        assert parser("X a").truth(t, 0)
        assert not parser("X a").truth(t, 1)
        assert parser("X b").truth(t, 1)
        assert parser("X c").truth(t, 4)
        # at the last step, Next != WeakNext
        assert not parser("X c").truth(t, 5)

    def test_truth_weaknext(self):
        parser = self.parser
        t = self.trace

        assert not parser("WX a").truth(t, 1)
        assert parser("WX c").truth(t, 5)

    def test_until(self):
        parser = self.parser
        t = self.trace

        assert parser("a U b U c").truth(t, 0)
        assert parser("a U b U c").truth(t, 2)
        assert parser("a U b U c").truth(t, 4)
        assert not parser("a U b U c").truth(t, 10)

        assert not parser("a U c").truth(t, 0)
        assert not parser("c U b").truth(t, 0)

    def test_release(self):
        parser = self.parser
        t = self.trace

        assert not parser("(!a R !b R !c)").truth(t, 0)
        assert not parser("(!a R !b R !c)").truth(t, 2)
        assert not parser("(!a R !b R !c)").truth(t, 4)
        assert parser("(!a R !b R !c)").truth(t, 10)

    def test_eventually(self):
        parser = self.parser
        t = self.trace

        assert not parser("F c & !a & !b").truth(t, 0)
        assert not parser("F a & b & c").truth(t, 0)
        assert parser("F(G(c))").truth(t, 0)
        assert not parser("F(G(b))").truth(t, 0)

    def test_always(self):
        parser = self.parser
        t = self.trace

        assert not parser("G a | b | c").truth(t, 0)
        assert parser("G F (c & !a & !b)").truth(t, 0)
        assert not parser("G c").truth(t, 0)
        assert parser("G c").truth(t, 4)
        assert parser("G c").truth(t, 10)
        assert parser("G F c").truth(t, 0)


def test_nnf():
    parser = LTLfParser()
    a, b, c = [LTLfAtomic(c) for c in "abc"]

    f = parser("!(a & !b)")
    assert f.to_nnf() == LTLfOr([LTLfNot(a), b])

    f = parser("!(!a | b)")
    assert f.to_nnf() == LTLfAnd([a, LTLfNot(b)])

    f = parser("!(a <-> b)")
    assert f.to_nnf() == LTLfAnd([LTLfOr([LTLfNot(a), LTLfNot(b)]), LTLfOr([a, b])])

    # Next and Weak Next
    f = parser("!(X (a & b))")
    assert f.to_nnf() == LTLfWeakNext(LTLfOr([LTLfNot(a), LTLfNot(b)]))

    f = parser("!(WX (a & b))")
    assert f.to_nnf() == LTLfNext(LTLfOr([LTLfNot(a), LTLfNot(b)]))

    # Eventually and Always
    f = parser("!(F (a | b))")
    assert f.to_nnf() == LTLfAlways(LTLfAnd([LTLfNot(a), LTLfNot(b)])).to_nnf()

    # Until and Release
    f = parser("!(a U b)")
    assert f.to_nnf() == LTLfRelease([LTLfNot(a), LTLfNot(b)])
    f = parser("!(a R b)")
    assert f.to_nnf() == LTLfUntil([LTLfNot(a), LTLfNot(b)])

    f = parser("!(F (a | b))")
    assert f.to_nnf() == LTLfAlways(LTLfAnd([LTLfNot(a), LTLfNot(b)])).to_nnf()
    f = parser("!(G (a | b))")
    assert f.to_nnf() == LTLfEventually(LTLfAnd([LTLfNot(a), LTLfNot(b)])).to_nnf()


class TestDelta:
    @classmethod
    def setup_class(cls):
        cls.parser = LTLfParser()
        cls.i_, cls.i_a, cls.i_b, cls.i_ab = (
            {},
            {"a": True},
            {"b": True},
            {"a": True, "b": True},
        )
        cls.true = PLTrue()
        cls.false = PLFalse()

    def test_atomic(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("a").delta(i_) == false
        assert parser("a").delta(i_a) == true
        assert parser("a").delta(i_b) == false
        assert parser("a").delta(i_ab) == true
        assert parser("a").delta(None, epsilon=True) == false

    def test_not(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("!a").delta(i_) == true
        assert parser("!a").delta(i_a) == false
        assert parser("!a").delta(i_b) == true
        assert parser("!a").delta(i_ab) == false
        assert parser("!a").delta(None, epsilon=True) == false

    def test_and(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("a & b").delta(i_) == PLAnd([false, false])
        assert parser("a & b").delta(i_a) == PLAnd([true, false])
        assert parser("a & b").delta(i_b) == PLAnd([false, true])
        assert parser("a & b").delta(i_ab) == PLAnd([true, true])
        assert parser("a & b").delta(i_, epsilon=True) == false

    def test_or(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("a | b").delta(i_) == PLOr([false, false])
        assert parser("a | b").delta(i_a) == PLOr([true, false])
        assert parser("a | b").delta(i_b) == PLOr([false, true])
        assert parser("a | b").delta(i_ab) == PLOr([true, true])
        assert parser("a | b").delta(i_, epsilon=True) == false

    def test_next(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("X a").delta(i_) == PLAnd(
            [PLAtomic(LTLfAtomic("a")), PLAtomic(LTLfEventually(LTLfTrue()).to_nnf())]
        )
        assert parser("X a").delta(i_a) == PLAnd(
            [PLAtomic(LTLfAtomic("a")), PLAtomic(LTLfEventually(LTLfTrue()).to_nnf())]
        )
        assert parser("X a").delta(i_b) == PLAnd(
            [PLAtomic(LTLfAtomic("a")), PLAtomic(LTLfEventually(LTLfTrue()).to_nnf())]
        )
        assert parser("X a").delta(i_ab) == PLAnd(
            [PLAtomic(LTLfAtomic("a")), PLAtomic(LTLfEventually(LTLfTrue()).to_nnf())]
        )
        assert parser("X a").delta(i_, epsilon=True) == false

    def test_until(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("a U b").delta(i_a) == PLOr(
            [
                false,
                PLAnd(
                    [
                        true,
                        PLAnd(
                            [
                                PLAtomic(LTLfUntil([LTLfAtomic("a"), LTLfAtomic("b")])),
                                PLAtomic(LTLfEventually(LTLfTrue()).to_nnf()),
                            ]
                        ),
                    ]
                ),
            ]
        )
        assert parser("a U b").delta(i_ab, epsilon=True) == false

    def test_release(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("a R b").delta(i_a) == PLAnd(
            [
                false,
                PLOr(
                    [
                        true,
                        PLOr(
                            [
                                PLAtomic(
                                    LTLfRelease([LTLfAtomic("a"), LTLfAtomic("b")])
                                ),
                                PLAtomic(LTLfAlways(LTLfFalse()).to_nnf()),
                            ]
                        ),
                    ]
                ),
            ]
        )
        assert parser("a R b").delta(i_ab, epsilon=True) == true

    def test_eventually(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("F a").delta(i_a) == PLOr(
            [
                true,
                PLAnd(
                    [
                        true,
                        PLAnd(
                            [
                                PLAtomic(LTLfUntil([LTLfTrue(), LTLfAtomic("a")])),
                                PLAtomic(LTLfUntil([LTLfTrue(), LTLfTrue()])),
                            ]
                        ),
                    ]
                ),
            ]
        )
        assert parser("F a").delta(i_) == PLOr(
            [
                false,
                PLAnd(
                    [
                        true,
                        PLAnd(
                            [
                                PLAtomic(LTLfUntil([LTLfTrue(), LTLfAtomic("a")])),
                                PLAtomic(LTLfUntil([LTLfTrue(), LTLfTrue()])),
                            ]
                        ),
                    ]
                ),
            ]
        )
        assert parser("F a").delta(i_a, epsilon=True) == false
        assert parser("F a").delta(i_, epsilon=True) == false

    def test_always(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab
        true = self.true
        false = self.false

        assert parser("G a").delta(i_a) == PLAnd(
            [
                true,
                PLOr(
                    [
                        false,
                        PLOr(
                            [
                                PLAtomic(LTLfRelease([LTLfFalse(), LTLfAtomic("a")])),
                                PLAtomic(LTLfRelease([LTLfFalse(), LTLfFalse()])),
                            ]
                        ),
                    ]
                ),
            ]
        )
        assert parser("G a").delta(i_a) == PLAnd(
            [
                true,
                PLOr(
                    [
                        false,
                        PLOr(
                            [
                                PLAtomic(LTLfRelease([LTLfFalse(), LTLfAtomic("a")])),
                                PLAtomic(LTLfRelease([LTLfFalse(), LTLfFalse()])),
                            ]
                        ),
                    ]
                ),
            ]
        )
        assert parser("G a").delta(i_a, epsilon=True) == true
        assert parser("G a").delta(i_, epsilon=True) == true


class TestToAutomaton:
    @classmethod
    def setup_class(cls):
        cls.parser = LTLfParser()
        cls.a, cls.b, cls.c = "a", "b", "c"
        cls.alphabet_abc = {cls.a, cls.b, cls.c}

        cls.i_ = {}
        cls.i_a = {cls.a: True}
        cls.i_b = {cls.b: True}
        cls.i_ab = {cls.a: True, cls.b: True}

    def test_atomic(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        ltlf = parser("a")
        dfa = ltlf.to_automaton()

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

        ltlf = parser("X a")
        dfa = ltlf.to_automaton()

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

        ltlf = parser("WX a")
        dfa = ltlf.to_automaton()

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

        ltlf = parser("a U b")
        dfa = ltlf.to_automaton()

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

        ltlf = parser("!a R !b")
        dfa = ltlf.to_automaton()

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

        ltlf = parser("F a")
        dfa = ltlf.to_automaton()

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

        ltlf = parser("G a")
        dfa = ltlf.to_automaton()

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

        ltlf = parser("G (a -> X(b) )")
        dfa = ltlf.to_automaton()

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

        ltlf = parser("G (a -> X(b))")
        dfa = ltlf.to_automaton()

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

    def test_conditional_response(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser("G (a -> F(b))").to_automaton()

        assert dfa.accepts([])
        assert dfa.accepts([i_b])
        assert dfa.accepts([i_])
        assert not dfa.accepts([i_a])
        assert dfa.accepts([i_ab])
        assert dfa.accepts([i_ab, i_ab])
        assert dfa.accepts([i_a, i_b])
        assert not dfa.accepts([i_a, i_a])


@pytest.fixture(scope="session", params=ltlf_formulas)
def ltlf_formula_automa_pair(request):
    formula_obj = parser(request.param)
    automaton = formula_obj.to_automaton()
    return formula_obj, automaton


@pytest.fixture(scope="session", params=ltlf_formulas)
def ltlf_formula_nnf_pair(request):
    formula_obj = parser(request.param)
    nnf = formula_obj.to_nnf()
    return formula_obj, nnf


@given(propositional_words(["a", "b", "c"], min_size=0, max_size=5))
def test_nnf_equivalence(ltlf_formula_nnf_pair, word):
    """Test that a formula is equivalent to its NNF form."""
    formula, formula_nnf = ltlf_formula_nnf_pair
    assert formula.truth(word, 0) == formula_nnf.truth(word, 0)


@given(propositional_words(["a", "b", "c"], min_size=0, max_size=5))
def test_formula_automaton_equivalence(ltlf_formula_automa_pair, word):
    formula_obj, automaton = ltlf_formula_automa_pair
    assert formula_obj.truth(word, 0) == automaton.accepts(word)


@given(propositional_words(["a", "b", "c"], min_size=1, max_size=5))
def test_persistence_is_equivalent_to_response_on_nonempty_words(word):
    formula_1 = LTLfAlways(LTLfEventually(LTLfAtomic("a")))
    formula_2 = LTLfEventually(LTLfAlways(LTLfAtomic("a")))
    assert formula_1.truth(word, 0) == formula_2.truth(word, 0)


def test_persistence_and_response_on_empty_words():
    formula_1 = LTLfAlways(LTLfEventually(LTLfAtomic("a")))
    formula_2 = LTLfEventually(LTLfAlways(LTLfAtomic("a")))
    recurrence_truth = formula_1.truth([], 0)
    persistence_truth = formula_2.truth([], 0)
    assert recurrence_truth
    assert not persistence_truth


class TestParsingTree:
    @classmethod
    def setup_class(cls):

        # Path to grammar
        this_path = os.path.dirname(os.path.abspath(__file__))
        grammar_path = "../flloat/parser/ltlf.lark"
        grammar_path = os.path.join(this_path, *grammar_path.split("/"))

        cls.checker = ParsingCheck(grammar_path)

    def test_propositional(self):

        ok, err = self.checker.precedence_check("a & !b | c", list("|&a!bc"))
        assert ok, err

        ok, err = self.checker.precedence_check(
            "!a&(b->c)", "&,!,a,(,),->,b,c".split(",")
        )
        assert ok, err

    def test_unary(self):

        ok, err = self.checker.precedence_check("X X a", list("XXa"))
        assert ok, err

        ok, err = self.checker.precedence_check(
            "X(G faLse)", "X ( ) G faLse".split(" ")
        )
        assert ok, err

        ok, err = self.checker.precedence_check("X G a", list("XGa"))
        assert ok, err

        ok, err = self.checker.precedence_check("GX a", list("GXa"))
        assert ok, err

        ok, err = self.checker.precedence_check(
            "XGXFWX G prop0", "X G X F WX G prop0".split(" ")
        )
        assert ok, err

        ok, err = self.checker.precedence_check(
            "XXWX!(!WXGGG a)", "X X WX ! ( ) ! WX G G G a".split(" ")
        )
        assert ok, err

    def test_bad_termination(self):

        # Wrong termination or space
        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("!a&", list("!a&"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("!&b", list("!&b"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("a|b|", list("a|b|"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("G", list("G"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("(a)(", list("(a)("))

        with pytest.raises(lark.UnexpectedInput) as exc:
            self.checker.precedence_check("aUa", list("aUa"))

        with pytest.raises(lark.UnexpectedInput) as exc:
            self.checker.precedence_check("Xa", list("Xa"))

    def test_bad_names(self):

        # Invalid names
        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("G X G", list("GXG"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("Future", ["Future"])

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("X F", list("XF"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("!X", list("!X"))

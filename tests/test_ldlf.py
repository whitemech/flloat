# -*- coding: utf-8 -*-
import os

import lark
import pytest
from hypothesis import given, settings
from hypothesis.strategies import sampled_from

from flloat.ldlf import (
    LDLfLogicalTrue,
    LDLfLogicalFalse,
    LDLfNot,
    LDLfAnd,
    RegExpPropositional,
    LDLfDiamond,
    LDLfEquivalence,
    LDLfBox,
    RegExpStar,
    LDLfOr,
    RegExpUnion,
    RegExpSequence,
    LDLfEnd,
    RegExpTest,
    LDLfLast,
)
from flloat.parser.ldlf import LDLfParser
from flloat.pl import PLTrue, PLFalse, PLAnd, PLNot, PLAtomic, PLEquivalence
from .conftest import LDLfFixtures
from .strategies import propositional_words
from .parsing import ParsingCheck
from . import test_pl


parser = LDLfParser()


def test_parser():
    parser = LDLfParser()
    a, b = PLAtomic("A"), PLAtomic("B")

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
    assert parser("!tt & <!A&B>tt") == LDLfAnd(
        [LDLfNot(tt), LDLfDiamond(RegExpPropositional(PLAnd([PLNot(a), b])), tt)]
    )
    assert parser("[true*]([true]ff | <!A>tt | <(true)*><B>tt)") == LDLfBox(
        RegExpStar(r_true),
        LDLfOr(
            [
                LDLfBox(r_true, ff),
                LDLfDiamond(RegExpPropositional(PLNot(a)), tt),
                LDLfDiamond(
                    RegExpStar(r_true), (LDLfDiamond(RegExpPropositional(b), tt))
                ),
            ]
        ),
    )

    assert parser("[A&B&A]ff <-> <A&B&A>tt") == LDLfEquivalence(
        [
            LDLfBox(RegExpPropositional(PLAnd([a, b, a])), ff),
            LDLfDiamond(RegExpPropositional(PLAnd([a, b, a])), tt),
        ]
    )

    assert parser("<A+B>tt") == LDLfDiamond(
        RegExpUnion([RegExpPropositional(a), RegExpPropositional(b)]), tt
    )
    assert parser("<A;B>tt") == LDLfDiamond(
        RegExpSequence([RegExpPropositional(a), RegExpPropositional(b)]), tt
    )
    assert parser("<A+(B;A)>end") == LDLfDiamond(
        RegExpUnion(
            [
                RegExpPropositional(a),
                RegExpSequence([RegExpPropositional(b), RegExpPropositional(a)]),
            ]
        ),
        LDLfEnd(),
    )

    assert parser("!(<(!(A<->D))+((B;C)*)+(?!last)>[(true)*]end)") == LDLfNot(
        LDLfDiamond(
            RegExpUnion(
                [
                    RegExpPropositional(PLNot(PLEquivalence([a, PLAtomic("D")]))),
                    RegExpStar(
                        RegExpSequence(
                            [
                                RegExpPropositional(PLAtomic("B")),
                                RegExpPropositional(PLAtomic("C")),
                            ]
                        )
                    ),
                    RegExpTest(LDLfNot(LDLfLast())),
                ]
            ),
            LDLfBox(RegExpStar(RegExpPropositional(PLTrue())), LDLfEnd()),
        )
    )


class TestTruth:
    @classmethod
    def setup_class(cls):
        cls.parser = LDLfParser()
        cls.trace = [{}, {"A": True}, {"A": True}, {"A": True, "B": True}, {}]

    def test_1(self):
        sa, sb = "A", "B"
        a, b = PLAtomic(sa), PLAtomic(sb)

        i_ = {}
        i_a = {"A": True}
        i_b = {"B": True}
        i_ab = {"A": True, "B": True}

        tr_false_a_b_ab = [i_, i_a, i_b, i_ab, i_]

        tt = LDLfLogicalTrue()
        ff = LDLfLogicalFalse()

        assert tt.truth(tr_false_a_b_ab, 0)
        assert not ff.truth(tr_false_a_b_ab, 0)
        assert not LDLfNot(tt).truth(tr_false_a_b_ab, 0)
        assert LDLfNot(ff).truth(tr_false_a_b_ab, 0)
        assert LDLfDiamond(RegExpPropositional(a), LDLfLogicalTrue()).truth([i_a], 0)
        # assert LDLfAnd([LDLfPropositional(a), LDLfPropositional(b)]).truth(
        #     tr_false_a_b_ab, 3
        # )
        assert not LDLfDiamond(RegExpPropositional(PLAnd([a, b])), tt).truth(
            tr_false_a_b_ab, 0
        )

        trace = self.trace
        parser = self.parser

        formula = "<true*;A&B>tt"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 0)

        formula = "[(A+!B)*]<C>tt"
        parsed_formula = parser(formula)
        assert not parsed_formula.truth(trace, 1)

        formula = "<?(<!C>tt)><A>tt"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 1)

        formula = "<!C+A>tt"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 1)

        formula = "<!C+A>tt"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 1)

    def test_2(self):
        parser = self.parser
        trace = self.trace

        formula = "<!A>A"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 0)

        formula = "<!A; !B><A>(A & B)"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 0)

        formula = "<true*>(A&B&<true>(!A&!B))"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 0)

        formula = "[true*; B]!A"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 0)

        formula = "[true*]!C"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 0)

        formula = "!<true>A & !<true>true & <true>tt"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 4)

        formula = "<true>(!A & !true & tt) & !A & !B"
        parsed_formula = parser(formula)
        assert parsed_formula.truth(trace, 4)

        formula = "<true>true"
        parsed_formula = parser(formula)
        assert not parsed_formula.truth(trace, 4)

        formula = "true"
        parsed_formula = parser(formula)
        assert not parsed_formula.truth(trace, 5)


def test_nnf():
    parser = LDLfParser()

    assert parser("!tt").to_nnf() == LDLfLogicalFalse()
    assert parser("!!tt").to_nnf() == LDLfLogicalTrue()

    f = parser("!(<!(A&B)>end)").to_nnf()
    ff = parser("[!A | !B]<true>tt")
    assert f == ff
    assert parser("!(<!(A&B)>end)").to_nnf() == parser("[!A | !B]<true>tt")

    f = parser("!(<((!(A<->D))+((B;C)*)+(?(!last)))>[(true)*]end)")
    assert f.to_nnf() == f.to_nnf().to_nnf()


def test_delta():
    parser = LDLfParser()
    i_ = {}
    i_a = {"A": True}
    i_b = {"B": True}
    i_ab = {"A": True, "B": True}

    true = PLTrue()
    false = PLFalse()
    tt = PLAtomic(LDLfLogicalTrue())
    ff = PLAtomic(LDLfLogicalFalse())

    assert parser("<A>tt").delta(i_) == false
    assert parser("<A>tt").delta(i_a) == tt
    assert parser("<A>tt").delta(i_b) == false
    assert parser("<A>tt").delta(i_ab) == tt

    assert parser("[B]ff").delta(i_) == true
    assert parser("[B]ff").delta(i_a) == true
    assert parser("[B]ff").delta(i_b) == ff
    assert parser("[B]ff").delta(i_ab) == ff

    f = parser("!(<?(!last)>end)")
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

    f = "(<(((?(<B>tt));true)*) ; (? (<(A & B)>tt))>tt)"
    formula = parser(f)
    assert formula.find_labels() == {c for c in "AB"}


class TestToAutomaton:
    @classmethod
    def setup_class(cls):
        cls.parser = LDLfParser()
        cls.i_ = {}
        cls.i_a = {"A": True}
        cls.i_b = {"B": True}
        cls.i_ab = {"A": True, "B": True}

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

        dfa = parser("<true*;B>tt").to_automaton()

        assert not dfa.accepts([])
        assert dfa.accepts([i_, i_b])
        assert not dfa.accepts([i_a])
        assert dfa.accepts([i_b])
        assert dfa.accepts([i_a, i_, i_ab, i_b])
        assert dfa.accepts([i_b, i_ab])

    def test_diamond_sequence(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser(
            "< ((!(A | B | C ))*) ; (A | C) ; ((!(A | B | C))*) ; (B | C) ><true>tt"
        ).to_automaton()

        assert not dfa.accepts([])
        assert not dfa.accepts([i_, i_b])
        assert dfa.accepts([i_a, i_b, i_])
        assert dfa.accepts([i_, i_, i_, i_, i_a, i_, i_ab, i_, i_])
        assert not dfa.accepts([i_b, i_b])

    def test_diamond_test(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser("(<(((?(<B>tt));true)*) ; ( ?(<(A & B)>tt))>tt)").to_automaton()

        assert not dfa.accepts([])
        assert not dfa.accepts([i_b, i_b, i_b])
        assert dfa.accepts([i_b, i_b, i_ab])

    def test_diamond_and_box(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser("(<true>tt) & ([A]<B>tt)").to_automaton()

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

        dfa = parser("[true*](<A>tt -> <true*><B>tt)").to_automaton()

        assert dfa.accepts([])
        assert dfa.accepts([i_b])
        assert dfa.accepts([i_])
        assert not dfa.accepts([i_a])
        assert dfa.accepts([i_ab])
        assert dfa.accepts([i_ab, i_ab])
        assert dfa.accepts([i_a, i_b])
        assert not dfa.accepts([i_a, i_a])

    def test_convertible_atomics(self):
        parser = self.parser
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        dfa = parser("A").to_automaton()

        assert not dfa.accepts([i_])
        assert dfa.accepts([i_a])
        assert dfa.accepts([i_ab, i_])
        assert not dfa.accepts([])

        dfa = parser("A & B").to_automaton()

        assert not dfa.accepts([i_a])
        assert dfa.accepts([i_ab, i_])
        assert not dfa.accepts([])

        dfa = parser("<true>true").to_automaton()

        assert not dfa.accepts([i_a])
        assert dfa.accepts([i_ab, i_])
        assert not dfa.accepts([])


@pytest.fixture(scope="session", params=LDLfFixtures.ldlf_formulas)
def ldlf_formula_automa_pair(request):
    formula_obj = parser(request.param)
    automaton = formula_obj.to_automaton()
    return formula_obj, automaton


@pytest.fixture(scope="session", params=LDLfFixtures.ldlf_formulas)
def ldlf_formula_nnf_pair(request):
    formula_obj = parser(request.param)
    nnf = formula_obj.to_nnf()
    return formula_obj, nnf


@given(propositional_words(["a", "b", "c"], min_size=0, max_size=5))
def test_nnf_equivalence(ldlf_formula_nnf_pair, word):
    """Test that a formula is equivalent to its NNF form."""
    formula, formula_nnf = ldlf_formula_nnf_pair
    assert formula.truth(word, 0) == formula_nnf.truth(word, 0)


@pytest.mark.parametrize("ldlf_theorem", LDLfFixtures.ldlf_theorems)
@given(propositional_words(["a", "b", "c"], min_size=0, max_size=5))
def test_theorems(ldlf_theorem, word):
    """Test that the validity of theorems."""
    formula = parser(ldlf_theorem)
    assert formula.truth(word, 0)


@pytest.mark.parametrize("ldlf_theorem", LDLfFixtures.ldlf_advanced_theorems)
@given(
    propositional_words(["a", "b", "c"], min_size=0, max_size=5),
    sampled_from(LDLfFixtures.ldlf_formulas),
    sampled_from(LDLfFixtures.ldlf_formulas),
)
@settings(max_examples=500)
def test_advanced_theorems(ldlf_theorem, word, ldlf_formula_1, ldlf_formula_2):
    """Test that the validity of theorems."""
    concrete_theorem = ldlf_theorem.format(f1=ldlf_formula_1, f2=ldlf_formula_2)
    formula = parser(concrete_theorem)
    assert formula.truth(word, 0)


@given(propositional_words(["a", "b", "c"], min_size=0, max_size=5))
def test_formula_automaton_equivalence(ldlf_formula_automa_pair, word):
    formula_obj, automaton = ldlf_formula_automa_pair
    assert formula_obj.truth(word, 0) == automaton.accepts(word)


class TestParsingTree:
    @classmethod
    def setup_class(cls):

        # Path to grammar
        this_path = os.path.dirname(os.path.abspath(__file__))
        grammar_path = "../flloat/parser/ldlf.lark"
        grammar_path = os.path.join(this_path, *grammar_path.split("/"))

        cls.checker = ParsingCheck(grammar_path)

    def test_propositional(self):

        # LDLf must include PL
        test_pl.TestParsingTree.test_unary(self)
        test_pl.TestParsingTree.test_and_or(self)
        test_pl.TestParsingTree.test_implications(self)
        test_pl.TestParsingTree.test_misc(self)
        test_pl.TestParsingTree.test_bad_examples(self)

    def test_boxes_and_diamonds(self):

        ok, err = self.checker.precedence_check("<R>a", list("<>Ra"))
        assert ok, err

        ok, err = self.checker.precedence_check("[B][B]<A>a", list("[]B[]B<>Aa"))
        assert ok, err

    def test_regex(self):

        ok, err = self.checker.precedence_check("<a&b+c>a", list("<>+&abca"))
        assert ok, err

        ok, err = self.checker.precedence_check("[a; b; c|b]a", list("[];;ab|cba"))
        assert ok, err

        ok, err = self.checker.precedence_check(
            "[a&b->c; b+c]a", list("[]+;") + ["->"] + list("&abcbca")
        )
        assert ok, err

    def test_star(self):

        ok, err = self.checker.precedence_check("[a+b*]c", list("[]+a*bc"))
        assert ok, err

        ok, err = self.checker.precedence_check("[((a&b+p)*)]c", list("[]()*()+&abpc"))
        assert ok, err

        ok, err = self.checker.precedence_check("<(a*)*+c*>z", list("<>+*()*a*cz"))
        assert ok, err

    def test_test(self):

        ok, err = self.checker.precedence_check("[a; ?<a>b]c", list("[];a?<>abc"))
        assert ok, err

        ok, err = self.checker.precedence_check("[?[?<a>b]b*]c", list("[]*?[]?<>abbc"))
        assert ok, err

    def test_outer(self):

        ok, err = self.checker.precedence_check("a-><b>e&c", ["->"] + list("a&<>bec"))
        assert ok, err

        ok, err = self.checker.precedence_check(
            "a-><b>(e&c)", ["->"] + list("a<>b()&ec")
        )
        assert ok, err

    def test_special(self):

        # These can be confused with atoms. Print to see the difference
        ok, err = self.checker.precedence_check("<true>tt", "< > true tt".split(" "))
        assert ok, err

        ok, err = self.checker.precedence_check("<false>ff", "< > false ff".split(" "))
        assert ok, err

        ok, err = self.checker.precedence_check("last&end", "& last end".split(" "))
        assert ok, err

    def test_bad_termination(self):

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("!a&", list("!a&"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("a&!", list("&a!"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("(a)(", list("()a("))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("<R>", list("<>R"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("[R]", list("[]R"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("[R][R", list("[]R[R"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("[]a", list("[]a"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("[(a]b", list("[(a]b"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("[a]b*", list("[]a*b"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("[a**]b", list("[]**ab"))

        with pytest.raises(lark.UnexpectedInput):
            self.checker.precedence_check("[<a>c?]b", list("[]?<>acb"))

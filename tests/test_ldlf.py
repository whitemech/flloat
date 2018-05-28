from flloat.base.Symbol import Symbol
from flloat.parser.ldlf import LDLfParser
from flloat.semantics.ldlf import FiniteTrace
from flloat.semantics.pl import PLInterpretation, PLFalseInterpretation
from flloat.syntax.ldlf import LDLfLogicalTrue, LDLfLogicalFalse, LDLfNot, LDLfAnd, LDLfPropositional, \
    RegExpPropositional, LDLfDiamond, LDLfEquivalence, LDLfBox, RegExpStar, LDLfOr, RegExpUnion, RegExpSequence, \
    LDLfEnd, RegExpTest, LDLfLast
from flloat.syntax.pl import PLAnd, PLAtomic, PLNot, PLEquivalence, PLFalse, PLTrue


def test_parser():
    parser = LDLfParser()
    sa, sb = Symbol("A"), Symbol("B")
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
    assert parser("!tt & <!A&B>tt") == LDLfAnd([LDLfNot(tt), LDLfDiamond(RegExpPropositional(PLAnd([PLNot(a),b])), tt)])

    assert parser("[true*](([true]ff) | (<!A>tt) | (<(true)*>(<B>tt)))") ==\
        LDLfBox(RegExpStar(r_true),
            LDLfOr([
                LDLfBox(r_true, ff),
                LDLfDiamond(RegExpPropositional(PLNot(a)), tt),
                LDLfDiamond(RegExpStar(r_true), (LDLfDiamond(RegExpPropositional(b), tt)))
            ])
        )

    assert parser("[A&B&A]ff <-> <A&B&A>tt") == LDLfEquivalence([
        LDLfBox(RegExpPropositional(PLAnd([a, b, a])), ff),
        LDLfDiamond(RegExpPropositional(PLAnd([a,b,a])), tt),
    ])

    assert parser("<A+B>tt")  == LDLfDiamond(RegExpUnion([RegExpPropositional(a), RegExpPropositional(b)]), tt)
    assert parser("<A;B>tt") == LDLfDiamond(RegExpSequence([RegExpPropositional(a), RegExpPropositional(b)]), tt)
    assert parser("<A+(B;A)>end") == LDLfDiamond(
        RegExpUnion([RegExpPropositional(a), RegExpSequence([RegExpPropositional(b), RegExpPropositional(a)])]),
        LDLfEnd()
    )

    assert parser("!(<!(A<->D)+(B;C)*+(!last)?>[(true)*]end)") == LDLfNot(
        LDLfDiamond(
            RegExpUnion([
                RegExpPropositional(PLNot(PLEquivalence([PLAtomic(Symbol("A")), PLAtomic(Symbol("D"))]))),
                RegExpStar(RegExpSequence([
                    RegExpPropositional(PLAtomic(Symbol("B"))),
                    RegExpPropositional(PLAtomic(Symbol("C"))),
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
    sa, sb = Symbol("a"), Symbol("b")
    a, b =   PLAtomic(sa), PLAtomic(sb)

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

    assert      tt.truth(tr_false_a_b_ab, 0)
    assert not  ff.truth(tr_false_a_b_ab, 0)
    assert not  LDLfNot(tt).truth(tr_false_a_b_ab, 0)
    assert      LDLfNot(ff).truth(tr_false_a_b_ab, 0)
    assert      LDLfAnd([LDLfPropositional(a), LDLfPropositional(b)]).truth(tr_false_a_b_ab, 3)
    assert not  LDLfDiamond(RegExpPropositional(PLAnd([a, b])), tt).truth(tr_false_a_b_ab, 0)

    parser = LDLfParser()
    trace = FiniteTrace.fromStringSets([
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

    assert parser("!tt").to_nnf() == LDLfLogicalFalse()
    assert parser("!!tt").to_nnf() == LDLfLogicalTrue()

    assert parser("!(<!(A&B)>end)").to_nnf() == parser("[!A | !B]<true>tt")

    f = parser("!(<!(A<->D)+(B;C)*+(!last)?>[(true)*]end)")
    assert f.to_nnf() == parser("[(([true]<true>tt)? + ((B ; C))* + ((A | D) & (!(D) | !(A))))]<(true)*><true>tt")
    assert f.to_nnf() == f.to_nnf().to_nnf().to_nnf().to_nnf()

def test_delta():
    parser = LDLfParser()
    sa, sb, sc = Symbol("A"), Symbol("B"), Symbol("C")
    a, b, c = PLAtomic(sa), PLAtomic(sb), PLAtomic(sc)

    i_ = PLFalseInterpretation()
    i_a = PLInterpretation({sa})
    i_b = PLInterpretation({sb})
    i_ab = PLInterpretation({sa, sb})

    true = PLTrue()
    false = PLFalse()
    tt = LDLfLogicalTrue()
    ff = LDLfLogicalFalse()

    assert parser("<A>tt").delta(i_)   == false
    assert parser("<A>tt").delta(i_a)  == tt
    assert parser("<A>tt").delta(i_b)  == false
    assert parser("<A>tt").delta(i_ab) == tt

    assert parser("[B]ff").delta(i_)   == true
    assert parser("[B]ff").delta(i_a)  == true
    assert parser("[B]ff").delta(i_b)  == ff
    assert parser("[B]ff").delta(i_ab) == ff

    # TODO: many other cases!

    f = parser("!(<!(A<->B)+(B;A)*+(!last)?>[(true)*]end)")
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
    assert formula.find_labels() == {Symbol(c) for c in "ABC"}

    f = "(<((((<B>tt)?);true)*) ; ((<(A & B)>tt) ?)>tt)"
    formula = parser(f)
    assert formula.find_labels() == {Symbol(c) for c in "AB"}

def test_to_automaton():
    parser = LDLfParser()
    a, b, c = Symbol("A"), Symbol("B"), Symbol("C")
    alphabet_abc = {a, b, c}

    i_ = PLInterpretation(set())
    i_a = PLInterpretation({a})
    i_b = PLInterpretation({b})
    i_ab = PLInterpretation({a, b})

    def _dfa_test(parser, string_formula, alphabet, test_function):
        """temporary function to easily test both the full DFA and the on-the-fly DFA"""
        dfa = parser(string_formula).to_automaton(alphabet, determinize=True, minimize=True)
        test_function(dfa)
        dfa = parser(string_formula).to_automaton(alphabet, on_the_fly=True)
        test_function(dfa)

    ##################################################################################
    f = "<A+!B>tt"
    def test_f(dfa):
        assert not dfa.word_acceptance([])
        assert     dfa.word_acceptance([i_, i_b])
        assert     dfa.word_acceptance([i_a])
        assert not dfa.word_acceptance([i_b])
        assert     dfa.word_acceptance([i_a, i_, i_ab, i_b])
        assert not dfa.word_acceptance([i_b, i_ab])
    _dfa_test(parser, f, alphabet_abc, test_f)
    ##################################################################################

    ##################################################################################
    f = "<true*;B>tt"

    def test_f(dfa):
        assert not dfa.word_acceptance([])
        assert     dfa.word_acceptance([i_, i_b])
        assert not dfa.word_acceptance([i_a])
        assert     dfa.word_acceptance([i_b])
        assert     dfa.word_acceptance([i_a, i_, i_ab, i_b])
        assert     dfa.word_acceptance([i_b, i_ab])

    _dfa_test(parser, f, alphabet_abc, test_f)
    ##################################################################################

    ##################################################################################
    f = "< (!(A | B | C ))* ; (A | C) ; (!(A | B | C))* ; (B | C) ><true>tt"
    def test_f(dfa):
        assert not dfa.word_acceptance([])
        assert not dfa.word_acceptance([i_, i_b])
        assert dfa.word_acceptance([i_a, i_b, i_])
        assert dfa.word_acceptance([i_, i_, i_, i_, i_a, i_, i_ab, i_, i_])
        assert not dfa.word_acceptance([i_b, i_b])
    _dfa_test(parser, f, alphabet_abc, test_f)
    ##################################################################################

    ##################################################################################
    f = "(<((((<B>tt)?);true)*) ; ((<(A & B)>tt) ?)>tt)"
    def test_f(dfa):
        assert not dfa.word_acceptance([])
        assert not dfa.word_acceptance([i_b, i_b, i_b])
        assert dfa.word_acceptance([i_b, i_b, i_ab])
    _dfa_test(parser, f, alphabet_abc, test_f)
    ##################################################################################

    ##################################################################################
    f = "(<true>tt) & ([A]<B>tt)"
    def test_f(dfa):
        assert not dfa.word_acceptance([])
        assert dfa.word_acceptance([i_b])
        assert dfa.word_acceptance([i_])
        assert not dfa.word_acceptance([i_a])
        assert not dfa.word_acceptance([i_ab])
        assert dfa.word_acceptance([i_ab, i_ab])
        assert dfa.word_acceptance([i_a, i_b])
    _dfa_test(parser, f, alphabet_abc, test_f)
    ##################################################################################

    #################################################################################
    f = "[true*](<A>tt -> <true*><B>tt)"
    def test_f(dfa):
        assert dfa.word_acceptance([])
        assert dfa.word_acceptance([i_b])
        assert dfa.word_acceptance([i_])
        assert not dfa.word_acceptance([i_a])
        assert dfa.word_acceptance([i_ab])
        assert dfa.word_acceptance([i_ab, i_ab])
        assert dfa.word_acceptance([i_a, i_b])
        assert not dfa.word_acceptance([i_a, i_a])
    _dfa_test(parser, f, alphabet_abc, test_f)
    #################################################################################

    #################################################################################
    # f = "<true*;A & B;true*;C & B>tt & [((<true>tt)?;true)*](<B -> (A | C)>tt  | [true]ff)"
    #
    # def test_f(dfa):
    #     assert not dfa.word_acceptance([i_])
    #     assert not dfa.word_acceptance([i_b])
    #     assert     dfa.word_acceptance([i_ab])
    #     assert     dfa.word_acceptance([i_])
    #     assert not dfa.word_acceptance([i_a])
    #     assert     dfa.word_acceptance([i_ab])
    #     assert     dfa.word_acceptance([i_ab, i_ab])
    #     assert     dfa.word_acceptance([i_a, i_b])
    #     assert not dfa.word_acceptance([i_a, i_a])
    #
    # _dfa_test(parser, f, alphabet_abc, test_f)
    #################################################################################


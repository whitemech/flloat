from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.parser.ltlf import LTLfParser
from flloat.semantics.ldlf import FiniteTrace
from flloat.semantics.pl import PLFalseInterpretation, PLInterpretation
from flloat.syntax.ltlf import LTLfAtomic, LTLfAnd, LTLfEquivalence, LTLfOr, LTLfNot, LTLfImplies, LTLfEventually, \
    LTLfAlways, LTLfUntil, LTLfRelease, LTLfNext, LTLfWeakNext
from flloat.syntax.pl import PLAtomic, PLTrue, PLFalse, PLAnd, PLOr


def test_parser():
    parser = LTLfParser()

    a, b, c = [LTLfAtomic(PLAtomic(Symbol(c))) for c in "ABC"]

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



def test_truth():
    parser = LTLfParser()
    t = FiniteTrace.fromStringSets([
        {"A"},
        {"A"},
        {"B"},
        {"B"},
        {"C"},
        {"C"},
    ])

    # Next and Weak Next
    f = "X A"
    assert parser("X A").truth(t, 0)
    assert not parser("X A").truth(t, 1)
    assert not parser("WX A").truth(t, 1)
    assert parser("X B").truth(t, 1)
    assert parser("X C").truth(t, 4)
    # at the last step, Next != WeakNext
    assert not parser("X C").truth(t, 5)
    assert parser("WX C").truth(t, 5)

    # Until
    f = "A U B U C"
    assert parser(f).truth(t, 0)
    assert parser(f).truth(t, 2)
    assert parser(f).truth(t, 4)
    assert not parser(f).truth(t, 10)

    assert not parser("A U C").truth(t, 0)
    assert not parser("C U B").truth(t, 0)

    # Release - dual of Until
    f = "(!A R !B R !C)"
    assert not parser(f).truth(t, 0)
    assert not parser(f).truth(t, 2)
    assert not parser(f).truth(t, 4)
    assert     parser(f).truth(t, 10)

    assert not parser("A U C").truth(t, 0)
    assert not parser("C U B").truth(t, 0)

    # Eventually
    assert      parser("F C & !A & !B").truth(t, 0)
    assert not  parser("F A & B & C").truth(t, 0)
    assert      parser("F G C").truth(t, 0)
    assert not  parser("F G B").truth(t, 0)

    # Always
    assert parser("G A | B | C").truth(t, 0)
    assert parser("G F (C & !A & !B)").truth(t, 0)
    assert not parser("G C").truth(t, 0)
    assert parser("G C").truth(t, 4)
    assert parser("G C").truth(t, 10)
    assert parser("G F C").truth(t, 0)


def test_nnf():
    parser = LTLfParser()
    a, b, c = [LTLfAtomic(PLAtomic(Symbol(c))) for c in "ABC"]

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
    assert f.to_nnf() == LTLfAlways(LTLfAnd([LTLfNot(a), LTLfNot(b)]))

    f = parser("!(F (A | B))")
    assert f.to_nnf() == LTLfAlways(LTLfAnd([LTLfNot(a), LTLfNot(b)]))
    f = parser("!(G (A | B))")
    assert f.to_nnf() == LTLfEventually(LTLfAnd([LTLfNot(a), LTLfNot(b)]))

    # Until and Release
    f = parser("!(A U B)")
    assert f.to_nnf() == LTLfRelease([LTLfNot(a), LTLfNot(b)])
    f = parser("!(A R B)")
    assert f.to_nnf() == LTLfUntil([LTLfNot(a), LTLfNot(b)])


def _test_delta():
    parser = LTLfParser()
    sa, sb = Symbol("A"), Symbol("B")
    a, b = PLAtomic(sa), PLAtomic(sb)

    i_ = PLFalseInterpretation()
    i_a = PLInterpretation({sa})
    i_b = PLInterpretation({sb})
    i_ab = PLInterpretation({sa, sb})

    true = PLTrue()
    false = PLFalse()

    assert parser("A").delta(i_) == false
    assert parser("A").delta(i_a) == true
    assert parser("A").delta(i_b) == false
    assert parser("A").delta(i_ab) == true

    assert parser("!A").delta(i_) == true
    assert parser("!A").delta(i_a) == false
    assert parser("!A").delta(i_b) == true
    assert parser("!A").delta(i_ab) == false

    assert parser("A & B").delta(i_) ==   PLAnd([false, false])
    assert parser("A & B").delta(i_a) ==  PLAnd([true, false])
    assert parser("A & B").delta(i_b) ==  PLAnd([false, true])
    assert parser("A & B").delta(i_ab) == PLAnd([true, true])

    assert parser("A | B").delta(i_) ==   PLOr([false, false])
    assert parser("A | B").delta(i_a) ==  PLOr([true, false])
    assert parser("A | B").delta(i_b) ==  PLOr([false, true])
    assert parser("A | B").delta(i_ab) == PLOr([true, true])

    assert parser("X A").delta(i_)   ==   LTLfAtomic(a)
    assert parser("X A").delta(i_a)  ==   LTLfAtomic(a)
    assert parser("X A").delta(i_b)  ==   LTLfAtomic(a)
    assert parser("X A").delta(i_ab) ==   LTLfAtomic(a)
    assert parser("X A").delta(i_, epsilon=True) == false

    assert parser("F A").delta(i_a) ==    PLOr([true, LTLfEventually(LTLfAtomic(a))])
    assert parser("F A").delta(i_) ==     PLOr([false, LTLfEventually(LTLfAtomic(a))])
    assert parser("F A").delta(i_, epsilon=True) == false

    assert parser("G A").delta(i_a) ==    PLAnd([true, LTLfAlways(LTLfAtomic(a))])
    assert parser("G A").delta(i_a, epsilon=True) == true
    assert parser("G A").delta(i_,  epsilon=True) == true

    assert parser("A U B").delta(i_a) == PLOr([
        false,
        PLAnd([
            true,
            LTLfUntil([LTLfAtomic(a), LTLfAtomic(b)])
        ])
    ])

    assert parser("A R B").delta(i_a) == PLAnd([
        false,
        PLOr([
            true,
            LTLfRelease([LTLfAtomic(a), LTLfAtomic(b)])
        ])
    ])


def _dfa_test(name, parser, string_formula, alphabet, test_function):
    """temporary function to easily test both the full DFA and the on-the-fly DFA"""
    ltlf = parser(string_formula)
    ldlf = ltlf.to_LDLf()

    dfa = ltlf.to_automaton(alphabet, determinize=True, minimize=True)
    # dfa.to_dot("tests/automata/" + name + "_ltlf", title=name+"\n"+string_formula)
    test_function(dfa)
    dfa = ltlf.to_automaton(alphabet, on_the_fly=True)
    test_function(dfa)

    # LDLf equivalent
    # print(str(ltlf), str(ldlf.to_nnf()))
    dfa = ldlf.to_automaton(alphabet, determinize=True, minimize=True)
    # dfa.to_dot("tests/automata/" + name + "_ldlf", title=name+"\n"+str(ldlf.to_nnf()))
    test_function(dfa)
    dfa = ldlf.to_automaton(alphabet, on_the_fly=True)
    test_function(dfa)

def test_to_automaton():
    parser = LTLfParser()
    a, b, c = Symbol("A"), Symbol("B"), Symbol("C")
    alphabet_abc = {a, b, c}

    i_ = PLInterpretation(set())
    i_a = PLInterpretation({a})
    i_b = PLInterpretation({b})
    i_ab = PLInterpretation({a, b})

    ##################################################################################
    f = "A"
    name = "atomic"

    def test_f(dfa):
        assert not dfa.word_acceptance([])
        assert not dfa.word_acceptance([i_])
        assert     dfa.word_acceptance([i_a])
        assert not dfa.word_acceptance([i_b])
        assert     dfa.word_acceptance([i_ab])
        assert not dfa.word_acceptance([i_,   i_])
        assert not dfa.word_acceptance([i_,   i_a])
        assert not dfa.word_acceptance([i_,   i_b])
        assert not dfa.word_acceptance([i_,   i_ab])
        assert     dfa.word_acceptance([i_a,  i_])
        assert     dfa.word_acceptance([i_a,  i_a])
        assert     dfa.word_acceptance([i_a,  i_b])
        assert     dfa.word_acceptance([i_a,  i_ab])
        assert not dfa.word_acceptance([i_b,  i_])
        assert not dfa.word_acceptance([i_b,  i_a])
        assert not dfa.word_acceptance([i_b,  i_b])
        assert not dfa.word_acceptance([i_b,  i_ab])
        assert     dfa.word_acceptance([i_ab, i_])
        assert     dfa.word_acceptance([i_ab, i_a])
        assert     dfa.word_acceptance([i_ab, i_b])
        assert     dfa.word_acceptance([i_ab, i_ab])

        assert dfa.word_acceptance([i_a, i_, i_ab, i_b])
        assert not dfa.word_acceptance([i_, i_ab])

    _dfa_test(name, parser, f, alphabet_abc, test_f)
    ##################################################################################

    ##################################################################################
    f = "X A"
    name = "next"

    def test_f(dfa):
        assert not dfa.word_acceptance([])
        assert not dfa.word_acceptance([i_])
        assert not dfa.word_acceptance([i_a])
        assert not dfa.word_acceptance([i_b])
        assert not dfa.word_acceptance([i_ab])
        assert not dfa.word_acceptance([i_, i_])
        assert     dfa.word_acceptance([i_, i_a])
        assert not dfa.word_acceptance([i_, i_b])
        assert     dfa.word_acceptance([i_, i_ab])
        assert not dfa.word_acceptance([i_a, i_])
        assert     dfa.word_acceptance([i_a, i_a])
        assert not dfa.word_acceptance([i_a, i_b])
        assert     dfa.word_acceptance([i_a, i_ab])
        assert not dfa.word_acceptance([i_b, i_])
        assert     dfa.word_acceptance([i_b, i_a])
        assert not dfa.word_acceptance([i_b, i_b])
        assert     dfa.word_acceptance([i_b, i_ab])
        assert not dfa.word_acceptance([i_ab, i_])
        assert     dfa.word_acceptance([i_ab, i_a])
        assert not dfa.word_acceptance([i_ab, i_b])
        assert     dfa.word_acceptance([i_ab, i_ab])

        assert not dfa.word_acceptance([i_a, i_b, i_])
        assert not dfa.word_acceptance([i_, i_, i_, i_, i_a, i_, i_ab, i_, i_])

    _dfa_test(name, parser, f, alphabet_abc, test_f)
    ##################################################################################

    ##################################################################################
    f = "WX A"
    name = "weak_next"

    def test_f(dfa):
        assert     dfa.word_acceptance([])
        assert     dfa.word_acceptance([i_])
        assert     dfa.word_acceptance([i_a])
        assert     dfa.word_acceptance([i_b])
        assert     dfa.word_acceptance([i_ab])
        assert not dfa.word_acceptance([i_, i_])
        assert     dfa.word_acceptance([i_, i_a])
        assert not dfa.word_acceptance([i_, i_b])
        assert     dfa.word_acceptance([i_, i_ab])
        assert not dfa.word_acceptance([i_a, i_])
        assert     dfa.word_acceptance([i_a, i_a])
        assert not dfa.word_acceptance([i_a, i_b])
        assert     dfa.word_acceptance([i_a, i_ab])
        assert not dfa.word_acceptance([i_b, i_])
        assert     dfa.word_acceptance([i_b, i_a])
        assert not dfa.word_acceptance([i_b, i_b])
        assert     dfa.word_acceptance([i_b, i_ab])
        assert not dfa.word_acceptance([i_ab, i_])
        assert     dfa.word_acceptance([i_ab, i_a])
        assert not dfa.word_acceptance([i_ab, i_b])
        assert     dfa.word_acceptance([i_ab, i_ab])


        assert     dfa.word_acceptance([i_b])
        assert not dfa.word_acceptance([i_b, i_b, i_b])
        assert     dfa.word_acceptance([i_b, i_a, i_ab])

    # _dfa_test(name, parser, f, alphabet_abc, test_f)
    ##################################################################################

    ##################################################################################
    f = "A U B"
    name = "until"

    def test_f(dfa):
        assert not dfa.word_acceptance([])
        assert not dfa.word_acceptance([i_])
        assert not dfa.word_acceptance([i_a])
        assert     dfa.word_acceptance([i_b])
        assert     dfa.word_acceptance([i_ab])
        assert not dfa.word_acceptance([i_, i_])
        assert not dfa.word_acceptance([i_, i_a])
        assert not dfa.word_acceptance([i_, i_b])
        assert not dfa.word_acceptance([i_, i_ab])
        assert not dfa.word_acceptance([i_a, i_])
        assert not dfa.word_acceptance([i_a, i_a])
        assert     dfa.word_acceptance([i_a, i_b])
        assert     dfa.word_acceptance([i_a, i_ab])
        assert     dfa.word_acceptance([i_b, i_])
        assert     dfa.word_acceptance([i_b, i_a])
        assert     dfa.word_acceptance([i_b, i_b])
        assert     dfa.word_acceptance([i_b, i_ab])
        assert     dfa.word_acceptance([i_ab, i_])
        assert     dfa.word_acceptance([i_ab, i_a])
        assert     dfa.word_acceptance([i_ab, i_b])
        assert     dfa.word_acceptance([i_ab, i_ab])

    _dfa_test(name, parser, f, alphabet_abc, test_f)
    ##################################################################################

    #################################################################################
    f = "!A R !B"
    name = "release"

    def test_f(dfa):
        assert     dfa.word_acceptance([])
        assert     dfa.word_acceptance([i_])
        assert     dfa.word_acceptance([i_a])
        assert not dfa.word_acceptance([i_b])
        assert not dfa.word_acceptance([i_ab])
        assert     dfa.word_acceptance([i_, i_])
        assert     dfa.word_acceptance([i_, i_a])
        assert     dfa.word_acceptance([i_, i_b])
        assert     dfa.word_acceptance([i_, i_ab])
        assert     dfa.word_acceptance([i_a, i_])
        assert     dfa.word_acceptance([i_a, i_a])
        assert not dfa.word_acceptance([i_a, i_b])
        assert not dfa.word_acceptance([i_a, i_ab])
        assert not dfa.word_acceptance([i_b, i_])
        assert not dfa.word_acceptance([i_b, i_a])
        assert not dfa.word_acceptance([i_b, i_b])
        assert not dfa.word_acceptance([i_b, i_ab])
        assert not dfa.word_acceptance([i_ab, i_])
        assert not dfa.word_acceptance([i_ab, i_a])
        assert not dfa.word_acceptance([i_ab, i_b])
        assert not dfa.word_acceptance([i_ab, i_ab])

    _dfa_test(name, parser, f, alphabet_abc, test_f)
    #################################################################################

    ##################################################################################
    f = "F A"
    name = "eventually"

    def test_f(dfa):
        assert not dfa.word_acceptance([])
        assert not dfa.word_acceptance([i_])
        assert     dfa.word_acceptance([i_a])
        assert not dfa.word_acceptance([i_b])
        assert     dfa.word_acceptance([i_ab])
        assert not dfa.word_acceptance([i_, i_])
        assert     dfa.word_acceptance([i_, i_a])
        assert not dfa.word_acceptance([i_, i_b])
        assert     dfa.word_acceptance([i_, i_ab])
        assert     dfa.word_acceptance([i_a, i_])
        assert     dfa.word_acceptance([i_a, i_a])
        assert     dfa.word_acceptance([i_a, i_b])
        assert     dfa.word_acceptance([i_a, i_ab])
        assert not dfa.word_acceptance([i_b, i_])
        assert     dfa.word_acceptance([i_b, i_a])
        assert not dfa.word_acceptance([i_b, i_b])
        assert     dfa.word_acceptance([i_b, i_ab])
        assert     dfa.word_acceptance([i_ab, i_])
        assert     dfa.word_acceptance([i_ab, i_a])
        assert     dfa.word_acceptance([i_ab, i_b])
        assert     dfa.word_acceptance([i_ab, i_ab])

        assert not dfa.word_acceptance([i_b, i_b, i_b])
        assert     dfa.word_acceptance([i_b, i_a, i_ab])

    _dfa_test(name, parser, f, alphabet_abc, test_f)
    ##################################################################################

    ##################################################################################
    f = "G A"
    name = "always"

    def test_f(dfa):
        assert     dfa.word_acceptance([])
        assert not dfa.word_acceptance([i_])
        assert     dfa.word_acceptance([i_a])
        assert not dfa.word_acceptance([i_b])
        assert     dfa.word_acceptance([i_ab])
        assert not dfa.word_acceptance([i_, i_])
        assert not dfa.word_acceptance([i_, i_a])
        assert not dfa.word_acceptance([i_, i_b])
        assert not dfa.word_acceptance([i_, i_ab])
        assert not dfa.word_acceptance([i_a, i_])
        assert     dfa.word_acceptance([i_a, i_a])
        assert not dfa.word_acceptance([i_a, i_b])
        assert     dfa.word_acceptance([i_a, i_ab])
        assert not dfa.word_acceptance([i_b, i_])
        assert not dfa.word_acceptance([i_b, i_a])
        assert not dfa.word_acceptance([i_b, i_b])
        assert not dfa.word_acceptance([i_b, i_ab])
        assert not dfa.word_acceptance([i_ab, i_])
        assert     dfa.word_acceptance([i_ab, i_a])
        assert not dfa.word_acceptance([i_ab, i_b])
        assert     dfa.word_acceptance([i_ab, i_ab])

        assert not dfa.word_acceptance([i_b, i_b, i_b])
        assert not dfa.word_acceptance([i_b, i_a, i_ab])
        assert     dfa.word_acceptance([i_a, i_a, i_ab])
        assert not dfa.word_acceptance([i_a, i_a, i_ab, i_b])
        assert     dfa.word_acceptance([i_a, i_a, i_ab, i_a])

    _dfa_test(name, parser, f, alphabet_abc, test_f)
    ##################################################################################

    ##################################################################################
    f = "G (A -> X(B) )"
    name = "always implication and next"

    def test_f(dfa):
        assert     dfa.word_acceptance([])
        assert     dfa.word_acceptance([i_])
        assert not dfa.word_acceptance([i_a])
        assert     dfa.word_acceptance([i_b])
        assert not dfa.word_acceptance([i_ab])
        assert     dfa.word_acceptance([i_, i_])
        assert not dfa.word_acceptance([i_, i_a])
        assert     dfa.word_acceptance([i_, i_b])
        assert not dfa.word_acceptance([i_, i_ab])
        assert not dfa.word_acceptance([i_a, i_])
        assert not dfa.word_acceptance([i_a, i_a])
        assert     dfa.word_acceptance([i_a, i_b])
        assert not dfa.word_acceptance([i_a, i_ab])
        assert     dfa.word_acceptance([i_b, i_])
        assert not dfa.word_acceptance([i_b, i_a])
        assert     dfa.word_acceptance([i_b, i_b])
        assert not dfa.word_acceptance([i_b, i_ab])
        assert not dfa.word_acceptance([i_ab, i_])
        assert not dfa.word_acceptance([i_ab, i_a])
        assert     dfa.word_acceptance([i_ab, i_b])
        assert not dfa.word_acceptance([i_ab, i_ab])

        assert     dfa.word_acceptance([i_b, i_b, i_b])
        assert not dfa.word_acceptance([i_b, i_a, i_ab])
        assert not dfa.word_acceptance([i_a, i_a, i_ab])
        assert not dfa.word_acceptance([i_a, i_a, i_ab, i_b])
        assert     dfa.word_acceptance([i_a, i_ab, i_ab, i_b])
        assert not dfa.word_acceptance([i_a, i_ab, i_ab, i_])
        # very important
        assert     dfa.word_acceptance([i_a, i_ab, i_ab, i_b, i_])
        assert     dfa.word_acceptance([i_a, i_ab, i_ab, i_b, i_b])
        assert     dfa.word_acceptance([i_a, i_ab, i_b, i_ab, i_b])

    _dfa_test(name, parser, f, alphabet_abc, test_f)
    ##################################################################################

    ##################################################################################
    f = "G (A -> X(G(B)) )"
    name = "always implication next always"

    def test_f(dfa):
        assert     dfa.word_acceptance([])
        assert     dfa.word_acceptance([i_])
        assert not dfa.word_acceptance([i_a])
        assert     dfa.word_acceptance([i_b])
        assert not dfa.word_acceptance([i_ab])
        assert     dfa.word_acceptance([i_, i_])
        assert not dfa.word_acceptance([i_, i_a])
        assert     dfa.word_acceptance([i_, i_b])
        assert not dfa.word_acceptance([i_, i_ab])
        assert not dfa.word_acceptance([i_a, i_])
        assert not dfa.word_acceptance([i_a, i_a])
        assert     dfa.word_acceptance([i_a, i_b])
        assert not dfa.word_acceptance([i_a, i_ab])
        assert     dfa.word_acceptance([i_b, i_])
        assert not dfa.word_acceptance([i_b, i_a])
        assert     dfa.word_acceptance([i_b, i_b])
        assert not dfa.word_acceptance([i_b, i_ab])
        assert not dfa.word_acceptance([i_ab, i_])
        assert not dfa.word_acceptance([i_ab, i_a])
        assert     dfa.word_acceptance([i_ab, i_b])
        assert not dfa.word_acceptance([i_ab, i_ab])

        assert     dfa.word_acceptance([i_b, i_b, i_b])
        assert not dfa.word_acceptance([i_b, i_a, i_ab])
        assert not dfa.word_acceptance([i_a, i_a, i_ab])
        assert not dfa.word_acceptance([i_a, i_a, i_ab, i_b])
        assert     dfa.word_acceptance([i_a, i_ab, i_ab, i_b])
        assert not dfa.word_acceptance([i_a, i_ab, i_ab, i_])
        # very important
        assert not dfa.word_acceptance([i_a, i_ab, i_ab, i_b, i_])
        assert     dfa.word_acceptance([i_a, i_ab, i_ab, i_b, i_b])
        assert     dfa.word_acceptance([i_a, i_ab, i_b, i_ab, i_b])

    _dfa_test(name, parser, f, alphabet_abc, test_f)

# def test_sapientino_formula():
#     p = LTLfParser()
#
#     f = '''(!bip U ( (A | B | C) & bip))
#                 & (G (A & bip -> X(G (bip -> !A) ) ) )
#                 & (G (B & bip -> X(G (bip -> !B) ) ) )
#                 & (G (C & bip -> X(G (bip -> !C) ) ) )
#                 & (
#                       (G (A & bip -> X( !bip U ((B | C) & bip) ) ) )
#                     | (G (B & bip -> X( !bip U ((A | C) & bip) ) ) )
#                     | (G (C & bip -> X( !bip U ((A | B) & bip) ) ) )
#                 )'''
#
#     f = '''( !bip U A & bip )'''
#
#     a, b, c, bip = Symbol("A"), Symbol("B"), Symbol("C"), Symbol("bip")
#     alphabet_abc = {a, b, c, bip}
#
#     def tr(l):
#         return FiniteTrace.fromSymbolSets(l).trace
#
#
#     def test_sapientino(dfa):
#         dfa.to_dot("temptemptemp")
#         assert not dfa.word_acceptance([])
#         assert not dfa.word_acceptance(tr([{bip}]))
#         assert     dfa.word_acceptance(tr([{bip, a}]))
#         assert     dfa.word_acceptance(tr([{bip, b}]))
#         assert     dfa.word_acceptance(tr([{bip, c}]))
#         assert not dfa.word_acceptance(tr([{bip, a}, {bip}]))
#         assert not dfa.word_acceptance(tr([{bip, b}, {bip}]))
#         assert not dfa.word_acceptance(tr([{bip, c}, {bip}]))
#     _dfa_test(p, f, alphabet_abc, test_sapientino)

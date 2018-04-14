from flloat.base.Symbol import Symbol
from flloat.parser.ldlf import LDLfParser
from flloat.parser.pl import PLParser
from flloat.semantics.ldlf import FiniteTraceInterpretation
from flloat.semantics.pl import PLInterpretation, PLFalseInterpretation
from flloat.syntax.ldlf import LDLfLogicalTrue, LDLfLogicalFalse, LDLfNot, LDLfAnd, LDLfPropositional, \
    RegExpPropositional, LDLfDiamond, LDLfEquivalence, LDLfBox, RegExpStar, LDLfOr, RegExpUnion, RegExpSequence, LDLfEnd
from flloat.syntax.pl import PLAnd, PLAtomic, PLNot, PLEquivalence, PLOr, PLImplies, PLFalse, PLTrue


def test_truth():
    sa, sb = Symbol("a"), Symbol("b")
    a, b =   PLAtomic(sa), PLAtomic(sb)

    i_ = PLFalseInterpretation()
    i_a = PLInterpretation({sa})
    i_b = PLInterpretation({sb})
    i_ab = PLInterpretation({sa, sb})

    tr_false_a_b_ab = FiniteTraceInterpretation([
        i_,
        i_a,
        i_b,
        i_ab
    ])

    tt = LDLfLogicalTrue()
    ff = LDLfLogicalFalse()

    assert      tt.truth(tr_false_a_b_ab, 0)
    assert not  ff.truth(tr_false_a_b_ab, 0)
    assert not  LDLfNot(tt).truth(tr_false_a_b_ab, 0)
    assert      LDLfNot(ff).truth(tr_false_a_b_ab, 0)
    assert      LDLfAnd({LDLfPropositional(a), LDLfPropositional(b)}).truth(tr_false_a_b_ab, 3)
    assert not  LDLfDiamond(RegExpPropositional(PLAnd({a, b})), tt).truth(tr_false_a_b_ab, 0)


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
    assert parser("~tt & <~A&B>tt") == LDLfAnd({LDLfNot(tt), LDLfDiamond(RegExpPropositional(PLAnd({PLNot(a),b})), tt)})

    assert parser("[true*](([true]ff) | (<~A>tt) | (<(true)*>(<B>tt)))") ==\
        LDLfBox(RegExpStar(r_true),
            LDLfOr({
                LDLfBox(r_true, ff),
                LDLfDiamond(RegExpPropositional(PLNot(a)), tt),
                LDLfDiamond(RegExpStar(r_true), (LDLfDiamond(RegExpPropositional(b), tt)))
            })
        )

    assert parser("[A&B&A]ff <-> <A&B&A>tt") == LDLfEquivalence({
        LDLfBox(RegExpPropositional(PLAnd({a, b, a})), ff),
        LDLfDiamond(RegExpPropositional(PLAnd({a,b,a})), tt),
    })

    assert parser("<A+B>tt")  == LDLfDiamond(RegExpUnion({RegExpPropositional(a), RegExpPropositional(b)}), tt)
    assert parser("<A;B>tt") == LDLfDiamond(RegExpSequence([RegExpPropositional(a), RegExpPropositional(b)]), tt)
    assert parser("<A+(B;A)>end") == LDLfDiamond(
        RegExpUnion({RegExpPropositional(a), RegExpSequence([RegExpPropositional(b), RegExpPropositional(a)])}),
        LDLfEnd()
    )


def test_nnf():
    parser = LDLfParser()
    assert parser("~(<~(A&B)>end)").to_nnf() == parser("[~A | ~B]<true>tt")

    f = parser("~(<~(A<->D)+(B;C)*+(~last)?>[(true)*]end)")
    assert f.to_nnf() == parser("[(([true]<true>tt)? + ((B ; C))* + ((A | D) & (~(D) | ~(A))))]<(true)*><true>tt")


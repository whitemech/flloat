from flloat.base.Symbol import Symbol
from flloat.parser.pl import PLParser
from flloat.semantics.pl import PLInterpretation
from flloat.syntax.pl import PLAnd, PLAtomic, PLNot, PLEquivalence, PLOr, PLImplies, PLFalse, PLTrue


def test_truth():
    sa, sb = Symbol("a"), Symbol("b")
    a, b = PLAtomic(sa), PLAtomic(sb)
    i_ = PLInterpretation(set())
    i_a = PLInterpretation({sa})
    i_b = PLInterpretation({sb})
    i_ab = PLInterpretation({sa, sb})

    a_and_b = PLAnd({a, b})
    assert not a_and_b.truth(i_)
    assert not a_and_b.truth(i_a)
    assert not a_and_b.truth(i_b)
    assert     a_and_b.truth(i_ab)

    not_a_and_not_b = PLAnd({PLNot(a), PLNot(b)})
    assert      not_a_and_not_b.truth(i_)
    assert not  not_a_and_not_b.truth(i_a)
    assert not  not_a_and_not_b.truth(i_b)
    assert not  not_a_and_not_b.truth(i_ab)

    material_implication = PLEquivalence({
        PLOr({PLNot(a), b}),
        PLNot(PLAnd({a, PLNot(b)})),
        PLImplies([a, b])
    })

    # the equivalence is valid (i.e. satisfied for every interpretation)
    assert material_implication.truth(i_)
    assert material_implication.truth(i_a)
    assert material_implication.truth(i_b)
    assert material_implication.truth(i_ab)

    a_and_false_and_true = PLAnd({a, PLFalse(), PLTrue()})
    assert not a_and_false_and_true.truth(i_)
    assert not a_and_false_and_true.truth(i_a)
    assert not a_and_false_and_true.truth(i_b)
    assert not a_and_false_and_true.truth(i_ab)

    a_or_false_or_true = PLOr({a, PLFalse(), PLTrue()})
    assert a_or_false_or_true.truth(i_)
    assert a_or_false_or_true.truth(i_a)
    assert a_or_false_or_true.truth(i_b)
    assert a_or_false_or_true.truth(i_ab)


def test_parser():
    parser = PLParser()
    sa, sb = Symbol("A"), Symbol("B")
    a, b = PLAtomic(sa), PLAtomic(sb)

    a_and_b = parser("A & B")
    true_a_and_b = PLAnd({a, b})
    assert a_and_b == true_a_and_b

    material_implication = parser("!A | B <-> !(A & !B) <-> A->B")
    true_material_implication = PLEquivalence({
        PLOr({PLNot(a), b}),
        PLNot(PLAnd({a, PLNot(b)})),
        PLImplies([a, b])
    })

    assert material_implication == true_material_implication

    true_a_and_false_and_true = PLAnd({a, PLFalse(), PLTrue()})
    a_and_false_and_true = parser("A & false & true")

    assert a_and_false_and_true == true_a_and_false_and_true


def test_nnf():
    parser = PLParser()

    not_a_and_b = parser("!(A&B)")
    nnf_not_a_and_b = parser("!A | !B")
    assert not_a_and_b.to_nnf() == nnf_not_a_and_b
    assert nnf_not_a_and_b == nnf_not_a_and_b.to_nnf()

    material_implication = parser("!A | B <-> !(A & !B) <-> A->B")
    nnf_material_implication = parser("((!A | B) & (!A | B) & (!A | B)) | ((A & !B) & (A & !B) & (A & !B))")
    assert material_implication.to_nnf() == nnf_material_implication
    assert nnf_material_implication == nnf_material_implication.to_nnf()



# -*- coding: utf-8 -*-
from flloat.parser.pl import PLParser
from flloat.semantics.pl import PLInterpretation
from flloat.pl import PLAnd, PLAtomic, PLNot, PLEquivalence, PLOr, PLImplies, PLFalse, PLTrue


def test_parser():
    parser = PLParser()
    sa, sb = "A", "B"
    a, b = PLAtomic(sa), PLAtomic(sb)

    a_and_b = parser("A & B")
    true_a_and_b = PLAnd([a, b])
    assert a_and_b == true_a_and_b

    material_implication = parser("!A | B <-> !(A & !B) <-> A->B")
    true_material_implication = PLEquivalence([
        PLOr([PLNot(a), b]),
        PLNot(PLAnd([a, PLNot(b)])),
        PLImplies([a, b])
    ])

    assert material_implication == true_material_implication

    true_a_and_false_and_true = PLAnd([a, PLFalse(), PLTrue()])
    a_and_false_and_true = parser("A & false & true")

    assert a_and_false_and_true == true_a_and_false_and_true


class TestTruth:

    @classmethod
    def setup_class(cls):
        cls.sa, cls.sb = "a", "b"

        cls.i_ = PLInterpretation(set())
        cls.i_a = PLInterpretation({cls.sa})
        cls.i_b = PLInterpretation({cls.sb})
        cls.i_ab = PLInterpretation({cls.sa, cls.sb})

        cls.a, cls.b = PLAtomic(cls.sa), PLAtomic(cls.sb)

    def test_atomic(self):
        a, b = self.a, self.b
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        assert not a.truth(i_)
        assert a.truth(i_a)
        assert not a.truth(i_b)
        assert a.truth(i_ab)

        assert not b.truth(i_)
        assert not b.truth(i_a)
        assert b.truth(i_b)
        assert b.truth(i_ab)

    def test_and(self):
        a, b = self.a, self.b
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        a_and_b = PLAnd([a, b])
        assert not a_and_b.truth(i_)
        assert not a_and_b.truth(i_a)
        assert not a_and_b.truth(i_b)
        assert a_and_b.truth(i_ab)

        not_a_and_not_b = PLAnd([PLNot(a), PLNot(b)])
        assert not_a_and_not_b.truth(i_)
        assert not not_a_and_not_b.truth(i_a)
        assert not not_a_and_not_b.truth(i_b)
        assert not not_a_and_not_b.truth(i_ab)

    def test_misc(self):
        a, b = self.a, self.b
        i_, i_a, i_b, i_ab = self.i_, self.i_a, self.i_b, self.i_ab

        material_implication = PLEquivalence([
            PLOr([PLNot(a), b]),
            PLNot(PLAnd([a, PLNot(b)])),
            PLImplies([a, b])
        ])

        # the equivalence is valid (i.e. satisfied for every interpretation)
        assert material_implication.truth(i_)
        assert material_implication.truth(i_a)
        assert material_implication.truth(i_b)
        assert material_implication.truth(i_ab)

        a_and_false_and_true = PLAnd([a, PLFalse(), PLTrue()])
        assert not a_and_false_and_true.truth(i_)
        assert not a_and_false_and_true.truth(i_a)
        assert not a_and_false_and_true.truth(i_b)
        assert not a_and_false_and_true.truth(i_ab)

        a_or_false_or_true = PLOr([a, PLFalse(), PLTrue()])
        assert a_or_false_or_true.truth(i_)
        assert a_or_false_or_true.truth(i_a)
        assert a_or_false_or_true.truth(i_b)
        assert a_or_false_or_true.truth(i_ab)


def test_nnf():
    parser = PLParser()
    sa, sb = "A", "B"
    a, b = PLAtomic(sa), PLAtomic(sb)
    i_ = PLInterpretation(set())
    i_a = PLInterpretation({sa})
    i_b = PLInterpretation({sb})
    i_ab = PLInterpretation({sa, sb})

    not_a_and_b = parser("!(A&B)")
    nnf_not_a_and_b = parser("!A | !B")
    assert not_a_and_b.to_nnf() == nnf_not_a_and_b
    assert nnf_not_a_and_b == nnf_not_a_and_b.to_nnf()

    dup = parser("!(A | A)")
    nnf_dup = dup.to_nnf()
    assert nnf_dup == PLNot(a)

    material_implication = parser("!A | B <-> !(A & !B) <-> A->B")
    nnf_material_implication = parser("((!A | B) & (!A | B) & (!A | B)) | ((A & !B) & (A & !B) & (A & !B))")
    nnf_m = material_implication.to_nnf()
    assert nnf_m == nnf_material_implication.to_nnf()

    assert nnf_m.truth(i_) == material_implication.truth(i_) == nnf_material_implication.truth(i_) == True
    assert nnf_m.truth(i_a) == material_implication.truth(i_a) == nnf_material_implication.truth(i_a) == True
    assert nnf_m.truth(i_b) == material_implication.truth(i_b) == nnf_material_implication.truth(i_b) == True
    assert nnf_m.truth(i_ab) == material_implication.truth(i_ab) == nnf_material_implication.truth(i_ab) == True


def test_find_labels():
    parser = PLParser()

    # complete formula
    f = "!A | B <-> !(A & !B) <-> A->B"
    formula = parser(f)
    assert formula.find_labels() == {"A", "B"}

    # more than one character
    f = "!A & (!AB & !A0)"
    formula = parser(f)
    assert formula.find_labels() == {c for c in {"A", "AB", "A0"}}

"""Misc tests"""
from flloat.base.Symbol import Symbol
from flloat.semantics.ldlf import FiniteTrace
from flloat.syntax.ldlf import LDLfLogicalTrue


def test_example_readme():
    from flloat.parser.ldlf import LDLfParser

    tt = LDLfLogicalTrue()
    tt.find_labels()

    parser = LDLfParser()
    formula = "<true*; A & B>tt"
    parsed_formula = parser(formula)

    assert str(parsed_formula) == "<((true)* ; (B & A))>(tt)" or str(parsed_formula) == "<((true)* ; (A & B))>(tt)"
    assert parsed_formula.find_labels() == {Symbol(c) for c in "AB"}

    t1 = FiniteTrace.fromStringSets([
        {},
        {"A"},
        {"A"},
        {"A", "B"}
    ])
    assert parsed_formula.truth(t1, 0)

    t2 = FiniteTrace.fromStringSets([
        {},
        {"A"},
        {"B"}
    ])
    assert not parsed_formula.truth(t2, 0)

    dfa = parsed_formula.to_automaton(determinize=True)
    assert     dfa.word_acceptance(t1.trace)
    assert not dfa.word_acceptance(t2.trace)

    # dfa.to_dot("my_dfa")





"""Misc tests"""

def test_ldlf_example_readme():
    from flloat.parser.ldlf import LDLfParser
    from flloat.base.Symbol import Symbol
    from flloat.semantics.ldlf import FiniteTrace

    parser = LDLfParser()
    formula = "<true*; A & B>tt"
    parsed_formula = parser(formula)

    assert str(parsed_formula) == "<((true)* ; (B & A))>(tt)" or str(parsed_formula) == "<((true)* ; (A & B))>(tt)"
    assert parsed_formula.find_labels() == {Symbol(c) for c in "AB"}

    from flloat.semantics.ldlf import FiniteTrace

    t1 = FiniteTrace.fromStringSets([
        {},
        {"A"},
        {"A"},
        {"A", "B"},
        {}
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


def test_ltlf_example_readme():
    from flloat.parser.ltlf import LTLfParser
    from flloat.base.Symbol import Symbol
    from flloat.semantics.ldlf import FiniteTrace

    parser = LTLfParser()
    formula = "F (A & !B)"
    parsed_formula = parser(formula)

    t1 = FiniteTrace.fromStringSets([
        {},
        {"A"},
        {"A"},
        {"A", "B"}
    ])
    assert parsed_formula.truth(t1, 0)

    t2 = FiniteTrace.fromStringSets([
        {},
        {"A", "B"},
        {"B"}
    ])
    assert not parsed_formula.truth(t2, 0)

    dfa = parsed_formula.to_automaton(determinize=True)
    assert dfa.word_acceptance(t1.trace)
    assert not dfa.word_acceptance(t2.trace)




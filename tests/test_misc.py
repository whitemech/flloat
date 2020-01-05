# -*- coding: utf-8 -*-
"""Misc tests"""
import os


def test_ldlf_example_readme():
    from flloat.parser.ldlf import LDLfParser

    parser = LDLfParser()
    formula = "<true*; A & B>tt"
    parsed_formula = parser(formula)

    assert str(parsed_formula) == "<((true)* ; (B & A))>(tt)" or str(parsed_formula) == "<((true)* ; (A & B))>(tt)"
    assert parsed_formula.find_labels() == {c for c in "AB"}

    t1 = [
        {"A": False, "B": False},
        {"A": True, "B": False},
        {"A": True, "B": False},
        {"A": True, "B": True},
        {"A": False, "B": False},
    ]
    assert parsed_formula.truth(t1, 0)
    t2 = [
        {"A": False, "B": False},
        {"A": True, "B": False},
        {"A": False, "B": True},
    ]
    assert not parsed_formula.truth(t2, 0)

    dfa = parsed_formula.to_automaton()
    assert dfa.accepts(t1)
    assert not dfa.accepts(t2)


def test_ltlf_example_readme():
    from flloat.parser.ltlf import LTLfParser

    parser = LTLfParser()
    formula = "F (A & !B)"
    parsed_formula = parser(formula)

    t1 = [
        {"A": False, "B": False},
        {"A": True, "B": False},
        {"A": True, "B": False},
        {"A": True, "B": True},
        {"A": False, "B": False},
    ]
    assert parsed_formula.truth(t1, 0)
    t2 = [
        {"A": False, "B": False},
        {"A": True, "B": True},
        {"A": False, "B": True},
    ]
    assert not parsed_formula.truth(t2, 0)
    dfa = parsed_formula.to_automaton()
    assert dfa.accepts(t1)
    assert not dfa.accepts(t2)


def test_hash_consistency_after_pickling():
    from flloat.parser.ltlf import LTLfParser
    import pickle

    parser = LTLfParser()
    formula = "F (A & !B)"
    old_obj = parser(formula)

    h = hash(old_obj)
    pickle.dump(old_obj, open("temp", "wb"))
    new_obj = pickle.load(open("temp", "rb"))

    assert new_obj._hash is None
    assert h == hash(new_obj)

    os.remove("temp")

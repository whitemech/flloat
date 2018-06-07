import re
from itertools import chain, combinations
# import pythomata.base.DFA
# import pythomata.base.NFA
#
# # https://docs.python.org/3/library/itertools.html#recipes
# from pythogic.base.Alphabet import Alphabet
#
from typing import Set, FrozenSet

from flloat.base.Symbols import Symbols


def powerset(iterable) -> FrozenSet:
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    combs = _powerset(iterable)
    res = frozenset(frozenset(x) for x in combs)
    # res = map(frozenset, combs)
    return res

def _powerset(iterable):
    s = list(set(iterable))
    combs = chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))
    for c in combs:
        yield c

def sym2regexp(sym:Symbols):
    s = sym.value
    if s in r"|()+?*.[]":
        return r"\%s"%s
    else:
        return s

# def my_str(obj):
#     s = str(obj)
#     s = re.sub("frozenset\(\)", "{}", s)
#     s = re.sub("frozenset\(\{(.*)\}\)", "{\g<1>}", s)
#     return s
#
#
# def declare_dfa_transition(dfa:pythomata.base.DFA.DFA):
#     pass
#
#
#
# def _to_pythomata_nfa(nfa:dict) -> pythomata.base.NFA.NFA:
#     """Generate a nfa (according to Pythomata API)
#     given a NFA dictionary generated by
#     the method LDLf_EmptyTraces.to_nfa"""
#
#     res = pythomata.base.NFA.NFA.fromTransitions(Alphabet(nfa["alphabet"]), nfa["states"], nfa["initial_states"],
#                               nfa["accepting_states"], nfa["transitions"])
#
#     return res
#
#
# def _to_pythomata_dfa(nfa_dict:dict, minimize=True, trimming=True) -> pythomata.base.DFA.DFA:
#     """Generate a DFA (according to Pythomata API)
#         given a NFA dictionary generated by
#         the method LDLf_EmptyTraces.to_nfa"""
#
#     nfa = _to_pythomata_nfa(nfa_dict)
#
#     dfa = pythomata.base.NFA.NFA.determinize(nfa)
#
#     if minimize:
#         dfa = pythomata.base.DFA.DFA.minimize(dfa)
#
#     if trimming:
#         dfa = pythomata.base.DFA.DFA.trim(dfa)
#
#     return dfa
#
#
# def print_nfa(nfa:dict, name, path="./"):
#     """Converts the NFA into a PySimpleAutomata.NFA
#     Then draw the NFA in .svg format.
#     :param nfa:
#     """
#     res = _to_pythomata_nfa(nfa)
#     res.to_dot(path + "/" + name)
#
# def print_dfa(nfa:dict, name, path="./"):
#     """Converts the NFA into a PySimpleAutomata.DFA
#     Then draw the NFA in .svg format."""
#     res = _to_pythomata_dfa(nfa)
#
#     res.to_dot(path + "/" + name)
#
#
#
MAX_CACHE_SIZE = 1000

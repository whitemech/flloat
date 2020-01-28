# -*- coding: utf-8 -*-
from typing import Set, FrozenSet, Dict

import sympy
from pythomata import PropInt, SymbolicAutomaton, SimpleDFA
from pythomata.alphabets import MapAlphabet
from sympy.logic.boolalg import BooleanFalse, BooleanTrue

from flloat.base.formulas import Formula
from flloat.base.symbols import Symbol
from flloat.helpers import powerset, iter_powerset
from flloat.pl import PLFormula, PLAtomic, PLNot, PLAnd, PLOr, PLImplies, PLEquivalence, PLTrue, PLFalse, to_sympy


def find_atomics(formula: Formula) -> Set[PLAtomic]:
    """Finds all the atomic formulas"""
    res = set()
    if isinstance(formula, PLFormula):
        res = formula.find_atomics()
    else:
        res.add(PLAtomic(formula))
    return res


def _transform_delta(f: Formula, formula2AtomicFormula):
    """From a Propositional Formula to a Propositional Formula
    with non-propositional subformulas replaced with a "freezed" atomic formula."""
    t = type(f)
    if isinstance(f, PLNot):
        return PLNot(_transform_delta(f, formula2AtomicFormula))
    # elif isinstance(f, PLBinaryOperator): #PLAnd, PLOr, PLImplies, PLEquivalence
    elif isinstance(f, (PLAnd, PLOr, PLImplies, PLEquivalence)):
        return t([_transform_delta(subf, formula2AtomicFormula) for subf in f.formulas])
    elif t == PLTrue or t == PLFalse:
        return f
    else:
        return formula2AtomicFormula[f]


def _is_true(Q: FrozenSet[FrozenSet]):
    if frozenset() in Q:
        return True
    conj = [
        PLAnd([subf.s.delta(None, epsilon=True) for subf in q])
        if len(q) >= 2
        else next(iter(q)).s.delta(None, epsilon=True)
        if len(q) == 1
        else PLFalse()
        for q in Q
    ]
    if len(conj) == 0:
        return False
    else:
        conj = PLOr(conj) if len(conj) >= 2 else conj[0]

    result = conj.truth(None)
    return result


def _make_transition(Q: FrozenSet[FrozenSet[Symbol]], i: PropInt):
    actions_set = set(i.keys())
    new_macrostate = set()

    for q in Q:
        # delta function applied to every formula in the macro state Q
        delta_formulas = [f.s.delta(i) for f in q]

        # find atomics -> so also ldlf formulas
        # replace atomic with custom object
        # convert to sympy

        # find the list of atoms, which are "true" atoms
        # (i.e. propositional atoms) or LDLf formulas
        atomics = [s for subf in delta_formulas for s in find_atomics(subf)]

        atom2id = {v: str(k) for k, v in enumerate(atomics)}  # type: Dict[str, PLAtomic]
        id2atom = {v: k for k, v in atom2id.items()}  # type: Dict[PLAtomic, str]

        # "freeze" the found atoms as symbols and build a mapping from symbols to formulas
        symbol2formula = {
            atom2id[f] for f in atomics if f != PLTrue() and f != PLFalse()
        }

        # build a map from formula to a "freezed" propositional Atomic Formula
        formula2atomic_formulas = {
            f: PLAtomic(atom2id[f])
            if f != PLTrue() and f != PLFalse()  # and not isinstance(f, PLAtomic)
            else f
            for f in atomics
        }

        # the final list of Propositional Atomic Formulas,
        # one for each formula in the original macro state Q
        transformed_delta_formulas = [
            _transform_delta(f, formula2atomic_formulas) for f in delta_formulas
        ]

        # the empty conjunction stands for true
        if len(transformed_delta_formulas) == 0:
            conjunctions = PLTrue()
        elif len(transformed_delta_formulas) == 1:
            conjunctions = transformed_delta_formulas[0]
        else:
            conjunctions = PLAnd(transformed_delta_formulas)

        # the model in this case is the smallest set of symbols
        # s.t. the conjunction of "freezed" atomic formula is true.
        # alphabet = frozenset(symbol2formula)
        # models = frozenset(conjunctions.minimal_models(alphabet))

        formula = to_sympy(conjunctions, replace=atom2id)
        all_models = list(sympy.satisfiable(formula, all_models=True))
        if len(all_models) == 1 and all_models[0] == BooleanFalse():
            models = set()
        elif len(all_models) == 1 and all_models[0] == {True: True}:
            models = [{}]
        else:
            models = map(lambda x: {k for k, v in x.items() if v is True}, all_models)

        for min_model in models:
            q_prime = frozenset({id2atom[s] for s in map(str, min_model)})

            new_macrostate.add(q_prime)

    return frozenset(new_macrostate)


# class DFAOTF(Simulator):
#     """DFA on the fly"""
#
#     def step(self, s: Symbol) -> Any:
#         pass
#
#     def accepts(self, word: List[Symbol]) -> bool:
#         return self.word_acceptance(word)
#
#     def __init__(self, f):
#         self.f = f.to_nnf()
#         self.cur_state = frozenset([frozenset([self.f])])
#
#     def reset(self):
#         self.cur_state = frozenset([frozenset([self.f])])
#
#     def word_acceptance(self, action_set_list: List[PLInterpretation]):
#         self.reset()
#         for a in action_set_list:
#             self.make_transition(a)
#         return self.is_true()
#
#     def is_true(self):
#         return _is_true(self.cur_state)
#
#     def get_current_state(self):
#         return self.cur_state
#
#     def make_transition(self, i: PLInterpretation):
#         self.cur_state = _make_transition(self.cur_state, i)
#

def get_labels_from_macrostate(macrostate):
    """Get labels from macrostate."""
    labels = set()
    for states in macrostate:
        for state in states:
            labels = labels.union(state.s.find_labels())
    return labels


def to_automaton(f):
    f = f.to_nnf()
    initial_state = frozenset({frozenset({PLAtomic(f)})})
    states = {initial_state}
    final_states = set()
    transition_function = {}

    all_labels = f.find_labels()
    alphabet = powerset(all_labels)

    if f.delta({}, epsilon=True) == PLTrue():
        final_states.add(initial_state)

    visited = set()
    to_be_visited = {initial_state}

    while len(to_be_visited) != 0:

        for q in list(to_be_visited):
            to_be_visited.remove(q)
            for actions_set in alphabet:
                new_state = _make_transition(q, {label: True for label in actions_set})
                if new_state not in states:
                    states.add(new_state)
                    to_be_visited.add(new_state)

                transition_function.setdefault(q, {})[
                    actions_set
                ] = new_state

                if new_state not in visited:
                    visited.add(new_state)
                    if _is_true(new_state):
                        final_states.add(new_state)

    automaton = SymbolicAutomaton()
    state2idx = {}
    for state in states:
        state_idx = automaton.create_state()
        state2idx[state] = state_idx
        if state == initial_state:
            automaton.set_initial_state(state_idx, True)
        if state in final_states:
            automaton.set_final_state(state_idx, True)

    for source in transition_function:
        for symbol, destination in transition_function[source].items():
            source_idx = state2idx[source]
            dest_idx = state2idx[destination]
            pos_expr = sympy.And(*map(sympy.Symbol, symbol))
            neg_expr = sympy.And(*map(lambda x: sympy.Not(sympy.Symbol(x)), all_labels.difference(symbol)))
            automaton.add_transition(source_idx, sympy.And(pos_expr, neg_expr), dest_idx)

    dfa = automaton.determinize().minimize()
    return dfa

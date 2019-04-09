from typing import Set, List

from flloat.base.formulas import Formula
from flloat.base.symbols import Symbol
from pythomata.base.Alphabet import Alphabet as PythomataAlphabet
from pythomata.base.DFA import DFA
from pythomata.base.Simulator import Simulator

from flloat.base.symbols import Alphabet
from flloat.semantics.pl import PLInterpretation
from flloat.syntax.pl import PLFormula, PLAtomic, PLTrue, PLFalse, PLNot, PLAnd, PLOr, PLImplies, \
    PLEquivalence
from flloat.helpers import powerset


def find_atomics(formula: Formula) -> Set[PLAtomic]:
    """Finds all the atomic formulas"""
    f = formula
    res = set()
    if isinstance(formula, PLFormula):
        res = formula.find_atomics()
    # elif isinstance(f, PLNot):
    #     res = res.union(find_atomics(f.f))
    # elif isinstance(f, PLBinaryOperator):
    #     for subf in f.formulas:
    #         res = res.union(find_atomics(subf))
    else:
        res.add(f)
    return res


def _transform_delta(f:Formula, formula2AtomicFormula):
    """From a Propositional Formula to a Propositional Formula
    with non-propositional subformulas replaced with a "freezed" atomic formula."""
    t = type(f)
    if t == PLNot:
        return PLNot(_transform_delta(f, formula2AtomicFormula))
    # elif isinstance(f, PLBinaryOperator): #PLAnd, PLOr, PLImplies, PLEquivalence
    elif t == PLAnd or t == PLOr or t == PLImplies or t == PLEquivalence:
        return t([_transform_delta(subf, formula2AtomicFormula) for subf in f.formulas])
    elif t == PLTrue or t == PLFalse:
        return f
    else:
        return formula2AtomicFormula[f]


class DFAOTF(Simulator):
    """DFA on the fly"""

    def __init__(self, f):
        self.f = f.to_nnf()
        self.reset()

    def reset(self):
        self.cur_state = frozenset([frozenset([self.f])])

    def word_acceptance(self, action_set_list:List[PLInterpretation]):
        self.reset()
        for a in action_set_list:
            self.make_transition(a)
        return self.is_true()

    def is_true(self):
        return self._is_true(self.cur_state)

    @staticmethod
    def _is_true(Q):
        if frozenset() in Q:
            return True
        conj = [PLAnd([subf.delta(None, epsilon=True) for subf in q]) if len(q) >= 2 else
                next(iter(q)).delta(None, epsilon=True) if len(q) == 1 else
                PLFalse() for q in Q]
        if len(conj) == 0:
            return False
        else:
            conj = PLOr(conj) if len(conj) >= 2 else conj[0]
        return conj.truth(None)

    def get_current_state(self):
        return self.cur_state

    def make_transition(self, i:PLInterpretation):
        self.cur_state = self._make_transition(self.cur_state, i)

    @staticmethod
    def _make_transition(Q, i: PLInterpretation):
        actions_set = i.true_propositions
        new_macrostate = set()

        for q in Q:
            # delta function applied to every formula in the macro state Q
            delta_formulas = [f.delta(actions_set) for f in q]

            # find the list of atoms, which are "true" atoms (i.e. propositional atoms) or LDLf formulas
            atomics = [s for subf in delta_formulas for s in find_atomics(subf)]

            atom2id = {v: k for k, v in enumerate(atomics)}
            id2atom = {v: k for k, v in atom2id.items()}

            # "freeze" the found atoms as symbols and build a mapping from symbols to formulas
            symbol2formula = {atom2id[f] for f in atomics if f != PLTrue() and f != PLFalse()}

            # build a map from formula to a "freezed" propositional Atomic Formula
            formula2atomic_formulas = {
                f: PLAtomic(atom2id[f])
                if f != PLTrue() and f != PLFalse()  # and not isinstance(f, PLAtomic)
                else f for f in atomics
            }

            # the final list of Propositional Atomic Formulas, one for each formula in the original macro state Q
            transformed_delta_formulas = [_transform_delta(f, formula2atomic_formulas) for f in delta_formulas]

            # the empy conjunction stands for true
            if len(transformed_delta_formulas) == 0:
                conjunctions = PLTrue()
            elif len(transformed_delta_formulas) == 1:
                conjunctions = transformed_delta_formulas[0]
            else:
                conjunctions = PLAnd(transformed_delta_formulas)

            # the model in this case is the smallest set of symbols s.t. the conjunction of "freezed" atomic formula
            # is true.
            models = frozenset(conjunctions.minimal_models(Alphabet(symbol2formula)))

            for min_model in models:
                q_prime = frozenset({id2atom[s] for s in min_model.true_propositions})

                new_macrostate.add(q_prime)

        return frozenset(new_macrostate)


def to_automaton(f, labels:Set[Symbol]=None, minimize=True):

    initial_state = frozenset({frozenset({f.to_nnf()})})
    states = {initial_state}
    final_states = set()
    transition_function = {}

    # the alphabet is the powerset of the set of fluents
    alphabet = powerset(labels)

    if DFAOTF._is_true(initial_state):
        final_states.add(initial_state)

    visited = set()
    to_be_visited = {initial_state}

    while len(to_be_visited)!=0:

        for q in list(to_be_visited):
            to_be_visited.remove(q)
            for actions_set in alphabet:
                new_state = DFAOTF._make_transition(q, PLInterpretation(actions_set))
                if not new_state in states:
                    states.add(new_state)
                    to_be_visited.add(new_state)

                transition_function.setdefault(q, {})[PLInterpretation(actions_set)] = new_state

                if new_state not in visited:
                    visited.add(new_state)
                    if DFAOTF._is_true(new_state): final_states.add(new_state)

    new_alphabet = PythomataAlphabet({PLInterpretation(set(sym)) for sym in alphabet})
    dfa = DFA(new_alphabet, frozenset(states), initial_state, frozenset(final_states), transition_function)

    if minimize:
        dfa = dfa.minimize().trim()

    return dfa


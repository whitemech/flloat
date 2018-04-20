from typing import Set, List

from pythomata.base.Alphabet import Alphabet
from pythomata.base.NFA import NFA
from pythomata.base.Simulator import Simulator

from flloat.base.Formula import Formula
from flloat.base.Symbol import Symbol
from flloat.semantics.pl import PLInterpretation, PLTrueInterpretation, PLFalseInterpretation
from flloat.syntax.pl import PLFormula, PLAtomic, PLTrue, PLFalse, PLNot, PLBinaryOperator, PLAnd, PLOr
from flloat.utils import powerset

def find_atomics(formula: Formula) -> Set[PLAtomic]:
    """Finds all the atomic formulas"""
    f = formula
    res = set()
    if isinstance(f, PLAtomic):
        res.add(f)
    elif isinstance(f, PLNot):
        res = res.union(find_atomics(f.f))
    elif isinstance(f, PLBinaryOperator):
        for subf in f.formulas:
            res = res.union(find_atomics(subf))
    else:
        res.add(f)
    return res

def _tranform_delta(f:Formula, formula2AtomicFormula):
    """From a Propositional Formula to a Propositional Formula
    with non-propositional subformulas replaced with a "freezed" atomic formula."""
    if isinstance(f, PLNot):
        return PLNot(_tranform_delta(f, formula2AtomicFormula))
    elif isinstance(f, PLBinaryOperator): #PLAnd, PLOr, PLImplies, PLEquivalence
        return type(f)([_tranform_delta(subf, formula2AtomicFormula) for subf in f.formulas])
    elif isinstance(f, PLTrue) or isinstance(f, PLFalse):
        return f
    else:
        return formula2AtomicFormula[f]


def to_automaton(f, labels:Set[Symbol]=None, determinize=False, minimize=True):
    """
    From a LDLfFormula, build the automaton.
    :param f:               a LDLfFormula;
    :param labels:          a set of Symbol, the fluents of our domain. If None, retrieve them from the formula;
    :param determinize:     True if you need to determinize the NFA, obtaining a DFA;
    :param minimize:        True if you need to minimize the DFA (if determinize is False this flag has no effect.)
    :return:                a NFA or a DFA which accepts the same traces that makes the formula True.
    """

    nnf = f.to_nnf()

    if labels is None:
        # if the labels of the formula are not specified in input,
        # retrieve them from the formula
        labels = nnf.find_labels()

    # the alphabet is the powerset of the set of fluents
    alphabet = powerset(labels)
    initial_states = {frozenset([nnf])}
    final_states = {frozenset()}
    delta = set()

    d = f.delta(PLFalseInterpretation(), epsilon=True)
    if d.truth(d):
        final_states.add(frozenset([nnf]))

    states = {frozenset(), frozenset([nnf])}

    states_changed, delta_changed = True, True
    while states_changed or delta_changed:

        states_changed, delta_changed = False, False
        for actions_set in alphabet:
            states_list = list(states)
            for q in states_list:

                # delta function applied to every formula in the macro state Q
                delta_formulas = [f.delta(actions_set) for f in q]

                # find the list of atoms, which are "true" atoms (i.e. propositional atoms) or LDLf formulas
                atomics = [s for subf in delta_formulas for s in find_atomics(subf)]

                # "freeze" the found atoms as symbols and build a mapping from symbols to formulas
                symbol2formula = {Symbol(str(f)): f for f in atomics if f != PLTrue() and f != PLFalse()}

                # build a map from formula to a "freezed" propositional Atomic Formula
                formula2atomic_formulas = {
                    f: PLAtomic(Symbol(str(f)))
                    if f != PLTrue() and f != PLFalse()# and not isinstance(f, PLAtomic)
                    else f for f in atomics
                }

                # the final list of Propositional Atomic Formulas, one for each formula in the original macro state Q
                transformed_delta_formulas = [_tranform_delta(f, formula2atomic_formulas) for f in delta_formulas]

                # the empy conjunction stands for true
                if len(transformed_delta_formulas) == 0:
                    conjunctions = PLTrue()
                elif len(transformed_delta_formulas) == 1:
                    conjunctions = transformed_delta_formulas[0]
                else:
                    conjunctions = PLAnd(transformed_delta_formulas)

                # the model in this case is the smallest set of symbols s.t. the conjunction of "freezed" atomic formula
                # is true.
                models = frozenset(conjunctions.minimal_models(set(symbol2formula)))
                if len(models) == 0:
                    continue
                for min_model in models:
                    q_prime = frozenset(
                        {symbol2formula[s] for s in min_model.true_propositions})

                    len_before = len(states)
                    states.add(q_prime)
                    if len(states) == len_before + 1:
                        states_list.append(q_prime)
                        states_changed = True

                    len_before = len(delta)
                    delta.add((q, actions_set, q_prime))
                    if len(delta) == len_before + 1:
                        delta_changed = True

                    # check if q_prime should be added as final state
                    if len(q_prime) == 0:
                        final_states.add(q_prime)
                    else:
                        subf_deltas = [subf.delta(PLFalseInterpretation(), epsilon=True) for subf in q_prime]
                        if len(subf_deltas)==1:
                            q_prime_delta_conjunction = subf_deltas[0]
                        else:
                            q_prime_delta_conjunction = PLAnd(subf_deltas)

                        if q_prime_delta_conjunction.truth(PLFalseInterpretation()):
                            final_states.add(q_prime)


    alphabet = Alphabet({PLInterpretation(set(sym)) for sym in alphabet})
    delta = frozenset((i, PLInterpretation(set(a)), o) for i, a, o in delta)

    # alphabet = Alphabet({frozenset(sym) for sym in alphabet})
    # delta = frozenset(delta)

    nfa = NFA.fromTransitions(
        alphabet=alphabet,
        states=frozenset(states),
        initial_states=frozenset(initial_states),
        accepting_states=frozenset(final_states),
        transitions=delta
    )

    if determinize:
        dfa = nfa.determinize()
        if minimize:
            dfa = dfa.minimize().trim()
        return dfa
    else:
        return nfa


class DFAOTF(Simulator):
    """DFA on the fly"""

    def __init__(self, f):
        self.f = f
        self.reset()

    def reset(self):
        self.cur_state = frozenset([frozenset([self.f])])

    def word_acceptance(self, action_set_list:List[PLInterpretation]):
        self.reset()
        for a in action_set_list:
            self.make_transition(a)
        return self.is_true()

    def is_true(self):
        # TODO: check if it is right

        if frozenset() in self.cur_state:
            return True
        conj = [PLAnd([subf.delta(PLFalseInterpretation(), epsilon=True) for subf in q]) if len(q)>=2 else
                next(iter(q)).delta(PLFalseInterpretation(), epsilon=True) if len(q)==1 else
                PLFalse() for q in self.cur_state]
        if len(conj) == 0:
            return False
        else:
            conj = PLOr(conj) if len(conj) >= 2 else conj[0]
        return conj.truth(None)


    def make_transition(self, i:PLInterpretation):
        actions_set = i.true_propositions
        new_macrostate = set()
        for q in self.cur_state:
            # delta function applied to every formula in the macro state Q
            delta_formulas = [f.delta(actions_set) for f in q]

            # find the list of atoms, which are "true" atoms (i.e. propositional atoms) or LDLf formulas
            atomics = [s for subf in delta_formulas for s in find_atomics(subf)]

            # "freeze" the found atoms as symbols and build a mapping from symbols to formulas
            symbol2formula = {Symbol(str(f)): f for f in atomics if f != PLTrue() and f != PLFalse()}

            # build a map from formula to a "freezed" propositional Atomic Formula
            formula2atomic_formulas = {
                f: PLAtomic(Symbol(str(f)))
                if f != PLTrue() and f != PLFalse()  # and not isinstance(f, PLAtomic)
                else f for f in atomics
            }

            # the final list of Propositional Atomic Formulas, one for each formula in the original macro state Q
            transformed_delta_formulas = [_tranform_delta(f, formula2atomic_formulas) for f in delta_formulas]

            # the empy conjunction stands for true
            if len(transformed_delta_formulas) == 0:
                conjunctions = PLTrue()
            elif len(transformed_delta_formulas) == 1:
                conjunctions = transformed_delta_formulas[0]
            else:
                conjunctions = PLAnd(transformed_delta_formulas)

            # the model in this case is the smallest set of symbols s.t. the conjunction of "freezed" atomic formula
            # is true.
            models = frozenset(conjunctions.minimal_models(set(symbol2formula)))

            if len(models) == 0:
                continue
            for min_model in models:
                q_prime = frozenset({symbol2formula[s] for s in min_model.true_propositions})

                new_macrostate.add(q_prime)

        self.cur_state = frozenset(new_macrostate)

from typing import Set

from pythomata.base.Alphabet import Alphabet
from pythomata.base.NFA import NFA

from flloat.base.Formula import Formula
from flloat.base.Symbol import Symbol
from flloat.semantics.pl import PLInterpretation, PLTrueInterpretation
from flloat.syntax.pl import PLFormula, PLAtomic, PLTrue, PLFalse, PLNot, PLBinaryOperator, PLAnd
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
    if isinstance(f, PLNot):
        return PLNot(_tranform_delta(f, formula2AtomicFormula))
    elif isinstance(f, PLBinaryOperator):
        return type(f)([_tranform_delta(subf, formula2AtomicFormula) for subf in f.formulas])
    elif isinstance(f, PLTrue) or isinstance(f, PLFalse):
        return f
    else:
        return formula2AtomicFormula[f]


def to_nfa(f, labels:Set[Symbol]):

    nnf = f.to_nnf()

    alphabet = powerset(labels)
    initial_states = {frozenset([nnf])}
    final_states = {frozenset()}
    delta = set()

    d = f.delta(None, epsilon=True)
    if d.truth(d, PLTrueInterpretation()):
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
                else:
                    conjunctions = PLAnd(set(transformed_delta_formulas))

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
                        q_prime_delta_conjunction = PLAnd({
                            subf.delta(None, epsilon=True) for subf in q_prime
                        })
                        if q_prime_delta_conjunction.truth(PLTrueInterpretation()):
                            final_states.add(q_prime)


    alphabet = Alphabet({PLInterpretation(set(sym)) for sym in alphabet})
    delta = frozenset((i, PLInterpretation(set(a)), o) for i, a, o in delta)

    # alphabet = Alphabet({frozenset(sym) for sym in alphabet})
    # delta = frozenset(delta)

    return NFA.fromTransitions(
        alphabet=alphabet,
        states=frozenset(states),
        initial_states=frozenset(initial_states),
        accepting_states=frozenset(final_states),
        transitions=delta
    )


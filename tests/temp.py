"""a temporary python script for small tests"""
from flloat.base.Symbol import Symbol
from flloat.parser.ldlf import LDLfParser
from flloat.parser.ltlf import LTLfParser
from flloat.semantics.ldlf import FiniteTrace


def k(n):
    from flloat.base.Symbol import Symbol
    from flloat.parser.ldlf import LDLfParser
    parser = LDLfParser()
    cc = ["c%d" % d for d in range(n)]
    labels = [Symbol(d) for d in cc]
    f = "<%s>tt" % ";".join(cc)
    print(f)

    f = parser(f)
    nfa = f.to_automaton(labels)
    nfa.to_dot("temp_nfa.NFA")
    print("nfa")
    dfa = nfa.determinize()
    dfa.to_dot("temp_det.DFA")
    print("dfa")
    dfa = dfa.minimize().trim()
    dfa.to_dot("temp_min.DFA")
    print("dfatrim")

if __name__ == '__main__':
    # k(4)
    # p = LTLfParser()
    p = LDLfParser()
    a,b,c = [Symbol(c) for c in "ABC"]
    f = p("<A*; B>tt")
    f.to_automaton({a,b,c}, determinize=True).complete().to_dot("temp.DFA")
    f.to_automaton({a, b, c}).to_dot("temp.NFA")
    k = f.to_automaton({a,b,c}, on_the_fly=True)

    t_false = FiniteTrace.fromStringSets([
        {"A"},
        {"A"},
        {"C"},
        {"B"}
    ])

    t_true = FiniteTrace.fromStringSets([
        {"A"},
        {"A"},
        {"B"},
        {"B"}
    ])

    # k.word_acceptance(t_false.trace)
    # k.word_acceptance(t_true.trace)

    print(k.cur_state)
    for s in t_true.trace:
        k.make_transition(s)
        print(k.cur_state)

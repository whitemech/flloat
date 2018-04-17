"""a temporary python script for small tests"""

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
    k(4)

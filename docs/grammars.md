# Grammars

Here are reported the grammars used by the [Lark](https://github.com/lark-parser/lark) parser
to parse Propositional Logic, LTLf and LDLf.

## Propositional Logic

[pl.lark](../flloat/parser/pl.lark)
```
start: propositional_formula
propositional_formula: wrapped_prop
                     | equivalence_prop
                     | implication_prop
                     | or_prop
                     | and_prop
                     | not_prop
                     | atom_prop

wrapped_prop: LSEPARATOR propositional_formula RSEPARATOR
equivalence_prop.5: propositional_formula EQUIVALENCE propositional_formula
implication_prop.4: propositional_formula IMPLY propositional_formula
or_prop.3: propositional_formula OR propositional_formula
and_prop.2: propositional_formula AND propositional_formula
not_prop.1: NOT propositional_formula
atom_prop: atom
	     | true_prop
	     | false_prop
atom: string*
true_prop.1: TRUE
false_prop.1: FALSE

string: /\w/+
TRUE : "True" | "TRUE" | "true"
FALSE : "False" | "FALSE" | "false"

EQUIVALENCE : "<->"
IMPLY : "->"
OR  : "||"|"|"
AND : "&&"|"&"
NOT : "!"

LSEPARATOR : "("
RSEPARATOR : ")"

WHITESPACE: (" " | "\n")+
%ignore WHITESPACE

```

## LTLf
[ltlf.lark](../flloat/parser/ltlf.lark)
```
start: ltlf_formula
ltlf_formula: ltlf_wrapped
            | ltlf_equivalence
            | ltlf_implication
            | ltlf_or
            | ltlf_and
            | ltlf_until
            | ltlf_release
            | ltlf_always
            | ltlf_eventually
            | ltlf_next
            | ltlf_weak_next
            | ltlf_not
            | ltlf_atom

ltlf_wrapped:     LSEPARATOR ltlf_formula RSEPARATOR
ltlf_equivalence.8: ltlf_formula EQUIVALENCE ltlf_formula
ltlf_implication.7: ltlf_formula IMPLY ltlf_formula
ltlf_or.6:          ltlf_formula OR ltlf_formula
ltlf_and.5:         ltlf_formula AND ltlf_formula
ltlf_until.4:       ltlf_formula UNTIL ltlf_formula
ltlf_release.4:     ltlf_formula RELEASE ltlf_formula
ltlf_always.3:      ALWAYS ltlf_formula
ltlf_eventually.3:  EVENTUALLY ltlf_formula
ltlf_next.2:        NEXT ltlf_formula
ltlf_weak_next.2:   WEAK_NEXT ltlf_formula
ltlf_not.1:         NOT ltlf_formula
ltlf_atom:        ltlf_symbol
         |        ltlf_true
         |        ltlf_false

ltlf_true: true_prop
ltlf_false: false_prop
ltlf_symbol: string
UNTIL: "U"
RELEASE: "R"
ALWAYS: "G"
EVENTUALLY: "F"
NEXT: "X"
WEAK_NEXT: "WX"
END: "END"|"end"|"End"
LAST: "LAST"|"last"|"Last"

WHITESPACE: (" " | "\n")+
%ignore WHITESPACE
%import .pl.string -> string
%import .pl.true_prop -> true_prop
%import .pl.false_prop -> false_prop
%import .pl.NOT -> NOT
%import .pl.OR -> OR
%import .pl.AND -> AND
%import .pl.EQUIVALENCE -> EQUIVALENCE
%import .pl.IMPLY -> IMPLY
%import .pl.LSEPARATOR -> LSEPARATOR
%import .pl.RSEPARATOR -> RSEPARATOR

```

## LDLf
[ldlf.lark](../flloat/parser/ldlf.lark)

```
start: ldlf_formula
ldlf_formula:  ldlf_wrapped
             | ldlf_equivalence
             | ldlf_implication
             | ldlf_or
             | ldlf_and
             | ldlf_box
             | ldlf_diamond
             | ldlf_not
             | ldlf_atom


ldlf_wrapped: LSEPARATOR ldlf_formula RSEPARATOR
ldlf_equivalence.6: ldlf_formula EQUIVALENCE ldlf_formula
ldlf_implication.5: ldlf_formula IMPLY ldlf_formula
ldlf_or.4: ldlf_formula OR ldlf_formula
ldlf_and.3: ldlf_formula AND ldlf_formula
ldlf_box.2: BOXLSEPARATOR regular_expression BOXRSEPARATOR ldlf_formula
ldlf_diamond.2: DIAMONDLSEPARATOR regular_expression DIAMONDRSEPARATOR ldlf_formula
ldlf_not.1: NOT ldlf_formula
ldlf_atom:   ldlf_tt
           | ldlf_ff
           | ldlf_last
           | ldlf_end
           | ldlf_true
           | ldlf_false
           | ldlf_symbol

ldlf_tt: TT
ldlf_ff: FF
ldlf_last: LAST
ldlf_end: END
ldlf_true: true_prop
ldlf_false: false_prop
ldlf_symbol: string

regular_expression: wrapped_regular_expression
                  | regular_expression_union
                  | regular_expression_sequence
                  | regular_expression_test
                  | regular_expression_star
                  | regular_expression_propositional

wrapped_regular_expression: LSEPARATOR regular_expression RSEPARATOR
regular_expression_union.3: regular_expression UNION regular_expression
regular_expression_sequence.3: regular_expression SEQ regular_expression
regular_expression_test.2: ldlf_formula TEST
regular_expression_star.2: regular_expression STAR
regular_expression_propositional.1: propositional_formula


%import common.ESCAPED_STRING
WHITESPACE: (" " | "\n")+
BOXLSEPARATOR: "["
BOXRSEPARATOR: "]"
DIAMONDLSEPARATOR: "<"
DIAMONDRSEPARATOR: ">"
TT: "tt"
FF: "ff"
LAST: "last"
END: "end"
UNION: "+"
SEQ: ";"
TEST: "?"
STAR: "*"
%ignore WHITESPACE
%import .pl.start -> propositional_formula
%import .pl.string -> string
%import .pl.true_prop -> true_prop
%import .pl.false_prop -> false_prop
%import .pl.EQUIVALENCE -> EQUIVALENCE
%import .pl.IMPLY -> IMPLY
%import .pl.OR -> OR
%import .pl.AND -> AND
%import .pl.NOT -> NOT
%import .pl.LSEPARATOR -> LSEPARATOR
%import .pl.RSEPARATOR -> RSEPARATOR

```

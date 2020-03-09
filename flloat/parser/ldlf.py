# -*- coding: utf-8 -*-
"""Implementation of the LDLf parser."""
from pathlib import Path

from lark import Lark, Transformer, Token, Tree, ParseError

from flloat.helpers import ParsingError
from flloat.ldlf import (
    LDLfLogicalTrue,
    LDLfLogicalFalse,
    LDLfNot,
    LDLfOr,
    LDLfEquivalence,
    LDLfImplies,
    LDLfAnd,
    LDLfDiamond,
    LDLfBox,
    RegExpTest,
    RegExpStar,
    RegExpUnion,
    RegExpSequence,
    RegExpPropositional,
    LDLfEnd,
    LDLfLast,
    LDLfPropositionalAtom,
)
from flloat.parser import CUR_DIR
from flloat.parser.pl import PLTransformer


class LDLfTransformer(Transformer):
    def __init__(self):
        super().__init__()
        self._pl_transformer = PLTransformer()
        self._pl_imported = ("prop_atom", "propositional_formula")

    def __starred_binaryop(self, args, formula_type):
        """Process a binary operator with repetitions.

        This parses rules of the form:   rule -> a (OP b)*

        :param args: The parging Tree.
        :param formula_type: Constructor of the OP class. It must accept
            a list of arguments.
        :return: a Formula.
        """

        if len(args) == 1:
            return args[0]
        elif (len(args) - 1) % 2 == 0:
            subformulas = args[::2]
            return formula_type(subformulas)
        else:
            raise ParsingError

    def start(self, args):
        assert len(args) == 1
        return args[0]

    def ldlf_formula(self, args):
        assert len(args) == 1
        return args[0]

    def ldlf_equivalence(self, args):
        return self.__starred_binaryop(args, LDLfEquivalence)

    def ldlf_implication(self, args):
        return self.__starred_binaryop(args, LDLfImplies)

    def ldlf_or(self, args):
        return self.__starred_binaryop(args, LDLfOr)

    def ldlf_and(self, args):
        return self.__starred_binaryop(args, LDLfAnd)

    def ldlf_unaryop(self, args):
        assert len(args) == 1
        return args[0]

    def ldlf_box(self, args):
        assert len(args) == 4
        _, regex, _, formula = args
        return LDLfBox(regex, formula)

    def ldlf_diamond(self, args):
        assert len(args) == 4
        _, regex, _, formula = args
        return LDLfDiamond(regex, formula)

    def ldlf_not(self, args):
        assert len(args) == 2
        return LDLfNot(args[1])

    def ldlf_wrapped(self, args):
        if len(args) == 1:
            return args[0]
        elif len(args) == 3:
            _, formula, _ = args
            return formula
        else:
            raise ParsingError

    def ldlf_atom(self, args):
        assert len(args) == 1
        formula = args[0]
        return formula

    def ldlf_tt(self, args):
        return LDLfLogicalTrue()

    def ldlf_ff(self, args):
        return LDLfLogicalFalse()

    def ldlf_last(self, args):
        return LDLfLast()

    def ldlf_end(self, args):
        return LDLfEnd()

    def regular_expression(self, args):
        assert len(args) == 1
        return args[0]

    def re_union(self, args):
        return self.__starred_binaryop(args, RegExpUnion)

    def re_sequence(self, args):
        return self.__starred_binaryop(args, RegExpSequence)

    def re_star(self, args):
        if len(args) == 1:
            return args[0]
        elif len(args) == 2:
            l, _ = args
            return RegExpStar(l)
        else:
            raise ParsingError

    def re_test(self, args):
        if len(args) == 1:
            return args[0]
        elif len(args) == 2:
            _, formula = args
            return RegExpTest(formula)
        else:
            raise ParsingError

    def re_wrapped(self, args):
        if len(args) == 1:
            return args[0]
        elif len(args) == 3:
            _, formula, _ = args
            return formula
        else:
            raise ParsingError

    def re_propositional(self, args):
        assert len(args) == 1
        return RegExpPropositional(args[0])

    def __getattr__(self, attr: str):
        """Also parse propositional logic."""

        if attr.startswith("pl__"):
            return getattr(self._pl_transformer, attr[4:])
        elif attr in self._pl_imported:
            return getattr(self._pl_transformer, attr)
        elif attr.isupper():
            raise AttributeError("Terminals should not be parsed")
        else:
            raise ParseError("No transformation exists for rule", attr)


class LDLfParser:
    def __init__(self):
        self._parser = Lark(open(str(Path(CUR_DIR, "ldlf.lark"))), parser="lalr")
        self._transformer = LDLfTransformer()

    def __call__(self, text):
        tree = self._parser.parse(text)
        formula = self._transformer.transform(tree)
        return formula


if __name__ == "__main__":
    parser = LDLfParser()
    while True:
        try:
            s = input("ldlf> ")
            if not s:
                continue
            result = parser(s)
            print("result:", result, type(result))
        except EOFError:
            break
        except Exception as e:
            print(str(e))

from abc import ABC
from enum import Enum
from typing import FrozenSet, Set, Hashable

import flloat
from flloat.helpers import powerset, ObjFactory, ObjConstructor

Symbol = Hashable


class Symbols(Enum):
    NOT = "!"
    AND = "&"
    OR = "|"
    EQUAL = "="
    IMPLIES = "->"
    EQUIVALENCE = "<->"
    NEXT = "X"
    WEAK_NEXT = "WX"
    UNTIL = "U"
    RELEASE = "R"
    EVENTUALLY = "F"
    ALWAYS = "G"
    PATH_UNION = "+"
    PATH_SEQUENCE = ";"
    PATH_STAR = "*"
    PATH_TEST = "?"
    ROUND_BRACKET_LEFT = "("
    ROUND_BRACKET_RIGHT = ")"
    EVENTUALLY_BRACKET_LEFT = "<"
    EVENTUALLY_BRACKET_RIGHT = ">"
    ALWAYS_BRACKET_LEFT = "["
    ALWAYS_BRACKET_RIGHT = "]"
    LAST = "last"
    END = "end"
    LOGICAL_TRUE = "tt"
    LOGICAL_FALSE = "ff"
    CARET = "^"
    TRUE = "true"
    FALSE = "false"
    LTLf_LAST = "last"


ALL_SYMBOLS = {v.value for v in Symbols}  # type: Set[str]


class _Alphabet(flloat.helpers.Hashable):
    def __init__(self, symbols: FrozenSet[Symbol]):
        super().__init__()
        self.symbols = symbols
        self._powerset = None

    def _members(self):
        return self.symbols

    def powerset(self):
        if self._powerset is None:
            self._powerset = _Alphabet(powerset(self.symbols))
        return self._powerset


class AlphabetConstructor(ObjConstructor):
    def __call__(self, symbols: Set[Symbol]) -> _Alphabet:
        f_symbols = frozenset(symbols)
        return super().__call__(f_symbols)

    @classmethod
    def from_strings(cls, symbol_strings: Set[str]):
        f_symbols = frozenset(s for s in symbol_strings)
        return alphabet_factory.new(f_symbols)


alphabet_factory = ObjFactory(_Alphabet)
Alphabet = AlphabetConstructor(alphabet_factory)


class Interpretation(flloat.helpers.Hashable, ABC):
    pass

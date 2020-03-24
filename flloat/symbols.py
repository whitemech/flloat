# -*- coding: utf-8 -*-
"""This module contains the definition to deal with symbols."""
from enum import Enum
from typing import Set


OpSymbol = str


class Symbols(Enum):
    """A set of symbols that can be used in a logical formula."""

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


ALL_SYMBOLS = {v.value for v in Symbols}  # type: Set[str]

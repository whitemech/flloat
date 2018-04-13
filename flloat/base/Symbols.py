from enum import Enum


class Symbols(Enum):
    DUMMY_PROPOSITION = "dummy_proposition"
    NOT = "~"
    AND = "&"
    OR = "|"
    EXISTS = "∃"
    FORALL = "Ɐ"
    EQUAL = "="
    IMPLIES = ">>"
    EQUIVALENCE = "==="
    NEXT = "○"
    UNTIL = "U"
    EVENTUALLY = "◇"
    ALWAYS = "□"
    PATH_UNION = "+"
    PATH_SEQUENCE = ";"
    PATH_STAR = "*"
    PATH_TEST = "?"
    ROUND_BRACKET_LEFT = "("
    ROUND_BRACKET_RIGHT = ")"
    ANGLE_BRACKET_LEFT = "❬"
    ANGLE_BRACKET_RIGHT = "❭"
    FULLWIDTH_SQUARE_BRACKET_LEFT = "［"
    FULLWIDTH_SQUARE_BRACKET_RIGHT = "］"
    TOP = "⊤"
    BOTTOM = "⊥"
    LAST = "Last"
    END = "End"
    LOGICAL_TRUE = "⊤⊤"
    LOGICAL_FALSE = "⊥⊥"
    CARET = "^"


ALL_SYMBOLS = {v.value for v in Symbols}

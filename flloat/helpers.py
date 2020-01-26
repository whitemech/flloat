# -*- coding: utf-8 -*-
from abc import ABC, abstractmethod
from copy import copy
from itertools import chain, combinations
from typing import Iterable, Set, FrozenSet

from pythomata import PropInt
from sympy import Symbol
from sympy.logic.boolalg import Boolean, BooleanFalse, BooleanTrue


ParsingError = ValueError("Parsing error.")


class Hashable(ABC):
    """A base class to represent hashable objects."""

    def __init__(self):
        self._hash = None

    @abstractmethod
    def _members(self):
        raise NotImplementedError

    def __eq__(self, other):
        if type(other) is type(self):
            return self._members() == other._members()
        else:
            return False

    def __hash__(self):
        if self._hash is None:
            members = self._members()
            self._hash = hash(members)
        return self._hash

    def __getstate__(self):
        d = copy(self.__dict__)
        d.pop("_hash")
        return d

    def __setstate__(self, state):
        self.__dict__ = state
        self._hash = None


def powerset(s: Set) -> FrozenSet:
    """
    Compute the power set of a set.

    >>> sorted(map(tuple, powerset({1,2,3})))
    [(), (1,), (1, 2), (1, 2, 3), (1, 3), (2,), (2, 3), (3,)]


    :param s: the set of elements on which to compute the power set.
    :return: the power set of the set provided in input.
    """
    combs = iter_powerset(s)
    res = frozenset(frozenset(x) for x in combs)
    return res


def iter_powerset(s: Set) -> Iterable:
    """
    The generative version of the power set function.

    :param s: the set of ee
    :return:
    """
    s = list(s)
    combs = chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))
    for c in combs:
        yield c


def sym2regexp(sym):
    s = sym.value
    if s in r"|()+?*.[]":
        return r"\%s" % s
    else:
        return s


def evaluate(formula: Boolean, i: PropInt) -> bool:
    """
    Evaluate a SymPy boolean expression againts a propositional interpretation.

    The symbols not present in the propositional interpretation will be considered False.

    >>> from sympy.parsing.sympy_parser import parse_expr
    >>> evaluate(parse_expr("a & b"), {"a": True})
    False

    >>> evaluate(parse_expr("a | b"), {"b": True})
    True

    >>> evaluate(parse_expr("a"), {"b": True})
    False

    :param formula: a sympy.logic.boolalg.Boolean.
    :param i: a propositional interpretation,
              i.e. a mapping from symbol identifiers to True/False
    :return: True if the formula is true in the interpretation, False o/w.
    """
    return formula.subs(i).replace(Symbol, BooleanFalse) == BooleanTrue()

MAX_CACHE_SIZE = 1024

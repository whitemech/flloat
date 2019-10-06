# -*- coding: utf-8 -*-
"""This module implement a trace, i.e. a list of propositional interpretations.

It can be used as model for temporal logics like LTLf and LDLf.
"""
from abc import abstractmethod
from typing import List, Set

from flloat.base.symbols import Symbol
from flloat.base.truths import Truth
from flloat.helpers import Hashable
from flloat.semantics.pl import PLInterpretation


class FiniteTrace(Hashable):
    """Implementation of a finite trace."""

    def __init__(self, trace: List[PLInterpretation]):
        """Initialize a trace."""
        super().__init__()
        self.trace = trace

    def _members(self):
        return tuple(self.trace)

    @staticmethod
    def from_symbol_sets(l: List[Set[Symbol]]) -> 'FiniteTrace':
        """Return a trace from a list of sets of symbols."""
        return FiniteTrace([PLInterpretation(frozenset(s)) for s in l])

    def length(self) -> int:
        """Return the length of the trace."""
        return len(self.trace)

    def last(self):
        """Return the last index of the trace."""
        return len(self.trace) - 1

    def _position_is_legal(self, position: int):
        return position >= 0 and position <= self.last()

    def get(self, position: int) -> PLInterpretation:
        """
        Get the propositional interpretation at a certain position.

        :param position: the index of the list.
        :return: the propositional interpretation at that position.
        """
        assert self._position_is_legal(position)
        return self.trace[position]

    def segment(self, start: int, end: int) -> 'FiniteTrace':
        """Return the sub-trace between two indexes.

        :param start: the start index of the sub-trace.
        :param end: the end index of the sub-trace
        :return: the sub-trace.
        """
        if not self._position_is_legal(start) or not self._position_is_legal(end):
            raise ValueError("Start or end position are not valid")
        return FiniteTrace(self.trace[start:end])

    def __str__(self):
        """Get the string representation."""
        return (
            "Trace (length=%s)" % self.length()
            + "\n\t"
            + "\n\t".join(
                "%d: {" % i + ", ".join(map(str, sorted(e))) + "}"
                for i, e in enumerate(self.trace)
            )
        )


class FiniteTraceTruth(Truth):
    """Interface for formulas that support the trace semantics."""

    @abstractmethod
    def truth(self, i: FiniteTrace, pos: int):
        """
        Return the truth evaluation of the formula wrt the trace.

        :param i: the trace.
        :param pos: the position from where to start the evaluation
        :return: True if the formula is satisfied by the trace, False otherwise.
        """

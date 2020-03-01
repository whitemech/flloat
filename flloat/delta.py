# -*- coding: utf-8 -*-
"""Base interface for delta transformations."""

from abc import ABC, abstractmethod

from pythomata import PropositionalInterpretation

from flloat.pl import PLFormula


class Delta(ABC):
    """Interface for formulas where the delta function is defined."""

    @abstractmethod
    def delta(self, i: PropositionalInterpretation, epsilon: bool = False) -> PLFormula:
        """
        Apply the delta function.

        :param i: the propositional interpretation.
        :param epsilon: whether the evaluation is on the empty trace.
        :return: a PLFormula object.
        """

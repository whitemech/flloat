# -*- coding: utf-8 -*-
"""Base module that contains several interfaces."""
from abc import ABC, abstractmethod


class NNF(ABC):
    """Interface for formulas that support NNF transformations."""

    @abstractmethod
    def to_nnf(self):
        """Transform the formula in NNF."""

    # @abstractmethod
    # def negate(self):
    #     """Negate the formula."""

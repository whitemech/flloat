# -*- coding: utf-8 -*-
"""This module contains the semantics for Propositional Logic."""

from typing import FrozenSet, Set, Union

from flloat.base.symbols import Symbol
from flloat.helpers import Hashable


class PLInterpretation(Hashable):
    """Implementation of a propositional interpretation."""

    def __init__(self, true_propositions: Union[Set[Symbol], FrozenSet[Symbol]]):
        """
        Initialize a propositional interpretation.

        :param true_propositions: the true symbols.
        """
        super().__init__()
        self.true_propositions = frozenset(true_propositions)

    def _members(self):
        return tuple(sorted(self.true_propositions))

    def __lt__(self, other):
        """Compare with another object."""
        return self.true_propositions < other.true_propositions

    def __contains__(self, item: Symbol):
        """Check if the symbol is true in the interpretation."""
        return item in self.true_propositions

    def __iter__(self):
        """Iterate over the true propositions."""
        return self.true_propositions.__iter__()

    def __str__(self):
        """Get the string representation."""
        return "{" + ", ".join(map(str, self._members())) + "}"

    def __repr__(self):
        """Get an unambiguous string representation."""
        return self.__str__()


class PLTrueInterpretation(PLInterpretation):
    """Implement a 'true' propositional interpretation.

    This interpretation contains all the symbols, by definition.
    """

    def __init__(self):
        """Initialize a universal propositional interpretation."""
        super().__init__(frozenset())

    def _members(self):
        return PLTrueInterpretation

    def __lt__(self, other):
        """Compare with another object."""
        return False

    def __contains__(self, item):
        """Check if the symbol is true in the interpretation."""
        return True


class PLFalseInterpretation(PLInterpretation):
    """Implement a 'fasle' propositional interpretation.

    This interpretation does not contain any symbol, by definition.
    """

    def __init__(self):
        """Initialize an empty propositional interpretation."""
        super().__init__(frozenset())

    def __contains__(self, item):
        """Check if the symbol is true in the interpretation."""
        return False

    def __lt__(self, other):
        """Compare with another object."""
        return True

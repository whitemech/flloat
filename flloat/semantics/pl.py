from typing import FrozenSet, Set

from flloat.base.Interpretation import Interpretation
from flloat.base.Symbol import Symbol

class PLInterpretation(Interpretation):

    def __init__(self, true_propositions:Set[Symbol]):
        self.true_propositions = frozenset(true_propositions)

    def __eq__(self, other):
        if type(self) == type(other):
            return self.true_propositions == other.true_propositions
        else:
            return False

    def _members(self):
        return self.true_propositions

    def __hash__(self):
        return hash(self._members())

    def __contains__(self, item:Symbol):
        return item in self.true_propositions

    def __iter__(self):
        return self.true_propositions.__iter__()

    def __str__(self):
        return "{" + ", ".join(map(str,self.true_propositions)) + "}"

    def __repr__(self):
        return self.__str__()

class PLTrueInterpretation(PLInterpretation):
    def __init__(self):
        super().__init__(set())

    def _members(self):
        return PLTrueInterpretation

    def __contains__(self, item):
        return True

class PLFalseInterpretation(PLInterpretation):
    def __init__(self):
        super().__init__(set())

    def __contains__(self, item):
        return False


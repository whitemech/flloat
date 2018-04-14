from typing import FrozenSet, Set

from flloat.base.Interpretation import Interpretation
from flloat.base.Symbol import Symbol


class PLInterpretation(Interpretation):

    def __init__(self, true_propositions:Set[Symbol]):
        self.true_propositions = true_propositions

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

class PLTrueInterpretation(PLInterpretation):
    def _members(self):
        return PLTrueInterpretation

    def __contains__(self, item):
        return True

class PLFalseInterpretation(PLInterpretation):
    def __init__(self):
        super().__init__(set())

    def __contains__(self, item):
        return False

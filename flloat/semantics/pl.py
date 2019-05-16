from typing import FrozenSet, Set, Union

from flloat.base.symbols import Symbol
from flloat.helpers import Hashable


class PLInterpretation(Hashable):

    def __init__(self, true_propositions: Union[Set[Symbol], FrozenSet[Symbol]]):
        super().__init__()
        self.true_propositions = frozenset(true_propositions)

    def _members(self):
        # return tuple(sorted(self.true_propositions, key=lambda x: x.name))
        return tuple(sorted(self.true_propositions))

    def __contains__(self, item: Symbol):
        return item in self.true_propositions

    def __iter__(self):
        return self.true_propositions.__iter__()

    def __str__(self):
        return "{" + ", ".join(map(str, self._members())) + "}"

    def __repr__(self):
        return self.__str__()


class PLTrueInterpretation(PLInterpretation):
    def __init__(self):
        super().__init__(frozenset())

    def _members(self):
        return PLTrueInterpretation

    def __contains__(self, item):
        return True


class PLFalseInterpretation(PLInterpretation):
    def __init__(self):
        super().__init__(frozenset())

    def __contains__(self, item):
        return False


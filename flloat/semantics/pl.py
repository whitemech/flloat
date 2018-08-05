from typing import FrozenSet, Set, List

from flloat.base.Interpretation import Interpretation
from flloat.base.Symbol import Symbol
from flloat.utils import ObjFactory, ObjConstructor


class _PLInterpretation(Interpretation):

    def __init__(self, true_propositions:FrozenSet[Symbol]):
        super().__init__()
        self.true_propositions = frozenset(true_propositions)

    def _members(self):
        # return tuple(sorted(self.true_propositions, key=lambda x: x.name))
        return self.true_propositions

    def __contains__(self, item:Symbol):
        return item in self.true_propositions

    def __iter__(self):
        return self.true_propositions.__iter__()

    def __str__(self):
        return "{" + ", ".join(map(str,self._members())) + "}"

    def __repr__(self):
        return self.__str__()

class PLTrueInterpretation(_PLInterpretation):
    def __init__(self):
        super().__init__(frozenset())

    def _members(self):
        return PLTrueInterpretation

    def __contains__(self, item):
        return True

class PLFalseInterpretation(_PLInterpretation):
    def __init__(self):
        super().__init__(frozenset())

    def __contains__(self, item):
        return False

class _PLInterpretationConstructor(ObjConstructor):
    def __call__(self, true_propositions:Set[Symbol]):
        f_sym = frozenset(true_propositions)
        return super().__call__(f_sym)

    def fromStrings(self, strings:List[str]):
        return self(set(Symbol(s) for s in strings))



plinterpretation_factory = ObjFactory(_PLInterpretation)
PLInterpretation = _PLInterpretationConstructor(plinterpretation_factory)

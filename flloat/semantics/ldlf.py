from abc import abstractmethod
from typing import List, Set

from flloat.base.Interpretation import Interpretation
from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.truths import Truth
from flloat.semantics.pl import PLInterpretation



class FiniteTrace(Interpretation):
    def __init__(self, trace: List[PLInterpretation], with_last=False):
        self.trace = trace

        if with_last:
            # Add '_Last' proposition at the last step
            # must be used for LTLf Formula
            last = self.trace[-1]
            new_last = PLInterpretation(last.true_propositions.union({Symbol(Symbols.LTLf_LAST.value)}))
            self.trace[-1] = new_last

    @staticmethod
    def fromSymbolSets(l:List[Set[Symbol]], **kwargs):
        return FiniteTrace([PLInterpretation(s) for s in l], **kwargs)

    @staticmethod
    def fromStringSets(l:List[Set[str]], **kwargs):
        return FiniteTrace([PLInterpretation({Symbol(string) for string in s}) for s in l], **kwargs)

    def length(self):
        return len(self.trace)

    def last(self):
        return len(self.trace)-1

    def _position_is_legal(self, position:int):
        return position>=0 and position <= self.last()

    def get(self, position:int) -> PLInterpretation:
        assert self._position_is_legal(position)
        return self.trace[position]

    def segment(self, start:int, end:int) :
        if not self._position_is_legal(start) or not self._position_is_legal(end):
            raise ValueError("Start or end position are not valid")
        return FiniteTrace(self.trace[start: end])

    def __str__(self):
        return "Trace (length=%s)" %self.length() + "\n\t" + \
               "\n\t".join("%d: {"%i + ", ".join(map(str,sorted(e))) + "}" for i, e in enumerate(self.trace))


class FiniteTraceTruth(Truth):
    @abstractmethod
    def truth(self, i: FiniteTrace, pos: int):
        raise NotImplementedError

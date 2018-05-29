from abc import ABC, abstractmethod

from flloat.semantics.pl import PLInterpretation


class Delta(ABC):

    def __init__(self):
        # self._computed_delta = None
        pass

    @abstractmethod
    def delta(self, i:PLInterpretation, epsilon=False):
        raise NotImplementedError


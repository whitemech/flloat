from abc import ABC, abstractmethod

from flloat.semantics.pl import PLInterpretation


class Delta(ABC):

    @abstractmethod
    def delta(self, i:PLInterpretation, epsilon=False):
        raise NotImplementedError

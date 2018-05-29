import hashlib
from abc import ABC, abstractmethod

class Hashable(ABC):

    def __init__(self):
        self._hash = None


    @abstractmethod
    def _members(self):
        raise NotImplementedError

    def __eq__(self, other):
        if type(other) is type(self):
            return self._members() == other._members()
        else:
            return False

    def __hash__(self):
        # if self._hash is None:
        #     self._hash = hash(self._members())
        # return self._hash
        return hash(self._members())

from abc import ABC, abstractmethod
from copy import copy

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
        if self._hash is None:
            self._hash = hash(self._members())
        return self._hash

    def __getstate__(self):
        d = copy(self.__dict__)
        d.pop("_hash")
        return d

    def __setstate__(self, state):
        self.__dict__ = state
        self._hash = None

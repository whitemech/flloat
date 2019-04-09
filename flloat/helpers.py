# import pythomata.base.DFA
# import pythomata.base.NFA


from abc import ABC, abstractmethod
from copy import copy
from itertools import chain, combinations
from typing import FrozenSet


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


def powerset(iterable) -> FrozenSet:
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    combs = _powerset(iterable)
    res = frozenset(frozenset(x) for x in combs)
    # res = map(frozenset, combs)
    return res


def _powerset(iterable):
    s = list(set(iterable))
    combs = chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))
    for c in combs:
        yield c


def sym2regexp(sym):
    s = sym.value
    if s in r"|()+?*.[]":
        return r"\%s"%s
    else:
        return s


MAX_CACHE_SIZE = 1024


class ObjFactory(object):
    def __init__(self, cls):
        self.cls = cls
        self.objects = {}

    def new(self, *args, **kwargs):
        old = self.objects.get(args, None)
        if old is not None:
            return old
        else:
            new = self.cls(*args, **kwargs)
            # notice: we do not save kwargs, so it works only with positional arguments.
            self.objects[args] = new
            return new


class ObjConstructor(object):
    def __init__(self, obj_factory:ObjFactory):
        self.obj_factory = obj_factory

    def __call__(self, *args, **kwargs):
        return self.obj_factory.new(*args, **kwargs)

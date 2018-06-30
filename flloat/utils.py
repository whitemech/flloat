from itertools import chain, combinations
# import pythomata.base.DFA
# import pythomata.base.NFA
#
# # https://docs.python.org/3/library/itertools.html#recipes
# from pythogic.base.Alphabet_ import Alphabet_
#
from typing import Set, FrozenSet

from flloat.base.Symbols import Symbols


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

def sym2regexp(sym:Symbols):
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


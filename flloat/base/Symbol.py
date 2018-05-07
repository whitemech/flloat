# -*- coding: utf-8 -*-
from flloat.base.Symbols import Symbols, ALL_SYMBOLS
from flloat.base.hashable import Hashable


class Symbol(Hashable):
    """A class to represent a symbol (actually, a wrap for a string)"""
    def __init__(self, name):
        super().__init__()
        self.name = name

    def _members(self):
        return (self.name)

    def __str__(self):
        return self.name

    def __repr__(self):
        # return ", ".join(map(str,self._members()))
        return str(self.name)

    def __lt__(self, other):
        return self.name.__lt__(other.name)



class FunctionSymbol(Symbol):
    """A class to represent a function symbol ("""
    def __init__(self, name: str, arity: int):
        super().__init__(name)
        self.arity = arity

    def __str__(self):
        return self.name + Symbols.CARET.value + str(self.arity)

    def _members(self):
        return (self.name, self.arity)


class ConstantSymbol(FunctionSymbol):
    def __init__(self, name:str):
        super().__init__(name, 0)


class PredicateSymbol(Symbol):
    def __init__(self, name: str, arity):
        super().__init__(name)
        self.arity = arity

    def __str__(self):
        return self.name + "^" + str(self.arity)

    def __repr__(self):
        return self.name + "^" + str(self.arity)

    def _members(self):
        return (self.name, self.arity)


class TrueSymbol(Symbol):
    def __init__(self):
        super().__init__(Symbols.TOP.value)

class FalseSymbol(Symbol):
    def __init__(self):
        super().__init__(Symbols.BOTTOM.value)

class LastSymbol(Symbol):
    def __init__(self):
        super().__init__(Symbols.LAST.value)

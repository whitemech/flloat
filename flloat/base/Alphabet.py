from typing import Set, FrozenSet

from flloat.base.Symbol import Symbol
from flloat.base.hashable import Hashable
from flloat.utils import _powerset, powerset, ObjFactory, ObjConstructor

class _Alphabet(Hashable):
    def __init__(self, symbols: FrozenSet[Symbol]):
        super().__init__()
        self.symbols = symbols
        self._powerset = None

    def _members(self):
        return self.symbols

    def powerset(self):
        if self._powerset is None:
            self._powerset = _Alphabet(powerset(self.symbols))
        return self._powerset


class AlphabetConstructor(ObjConstructor):
    def __call__(self, symbols: Set[Symbol]) -> _Alphabet:
        f_symbols = frozenset(symbols)
        return super().__call__(f_symbols)

    @classmethod
    def fromStrings(cls, symbol_strings: Set[str]):
        f_symbols = frozenset(Symbol(s) for s in symbol_strings)
        return alphabet_factory.new(f_symbols)

alphabet_factory = ObjFactory(_Alphabet)
Alphabet = AlphabetConstructor(alphabet_factory)

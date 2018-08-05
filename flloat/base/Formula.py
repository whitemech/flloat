from abc import abstractmethod, ABC
from typing import Sequence, Set

from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols
from flloat.base.hashable import Hashable


class Formula(Hashable):
    def __init__(self):
        super().__init__()

    @abstractmethod
    def find_labels(self) -> Set[Symbol]:
        return set()

    def simplify(self):
        return self

class AtomicFormula(Formula):
    def __init__(self, s:Symbol):
        super().__init__()
        self.s = s

    def _members(self):
        return self.s

    def __str__(self):
        return str(self.s)

    def find_labels(self):
        return {self.s}

class Operator(Formula):
    base_expression = Symbols.ROUND_BRACKET_LEFT.value + "%s" + Symbols.ROUND_BRACKET_RIGHT.value

    @property
    def operator_symbol(self) -> str:
        raise NotImplementedError


class UnaryOperator(Operator):
    def __init__(self, f: Formula):
        super().__init__()
        self.f = f.simplify()

    def __str__(self):
        return self.operator_symbol + Symbols.ROUND_BRACKET_LEFT.value + str(self.f) + Symbols.ROUND_BRACKET_RIGHT.value

    def _members(self):
        return (self.operator_symbol, self.f)

    def __lt__(self, other):
        return self.f.__lt__(other.f)

    def find_labels(self):
        return self.f.find_labels()


OperatorChilds = Sequence[Formula]
CommOperatorChilds = Set[Formula]


class BinaryOperator(Operator):
    """A generic binary formula"""


    def __init__(self, formulas:OperatorChilds):
        super().__init__()
        assert len(formulas) >= 2
        self.formulas = tuple(formulas)
        self.formulas = self._popup()

    def __str__(self):
        return "(" + (" "+self.operator_symbol+" ").join(map(str,self.formulas)) + ")"

    def _members(self):
        return (self.operator_symbol, self.formulas)

    def _popup(self):
        """recursively find commutative binary operator
        among child formulas and pop up them at the same level"""
        res = ()
        for child in self.formulas:
            if type(child) == type(self):
                superchilds = child.formulas
                res += superchilds
            else:
                res += (child, )
        return tuple(res)

    def find_labels(self):
        return set.union(*map(lambda f: f.find_labels(), self.formulas))

class CommutativeBinaryOperator(BinaryOperator):
    """A generic commutative binary formula"""


    def __init__(self, formulas:OperatorChilds, idempotence=True):
        # Assuming idempotence: e.g. A & A === A
        super().__init__(formulas)
        self.idempotence = idempotence
        if idempotence:
            # order does not matter -> set operation
            # remove duplicates -> set operation
            self.formulas_set = frozenset(self.formulas)
            # unique representation -> sorting
            self.members = tuple(sorted(self.formulas_set, key=lambda x: hash(x)))

    def simplify(self):
        if self.idempotence:
            if len(self.formulas_set) == 1:
                return next(iter(self.formulas_set)).simplify()
            else:
                return type(self)(self.members)
        else:
            return self


    def _members(self):
        if self.idempotence:
            return (self.operator_symbol, self.members)
            # return (self.operator_symbol, self.formulas_set)
        else:
            return super()._members()

    def find_labels(self):
        return set.union(*map(lambda f: f.find_labels(), self.formulas))

    def __str__(self):
        if self.idempotence:
            return "(" + (" "+self.operator_symbol+" ").join(map(str,self.members)) + ")"
        else:
            return super().__str__()

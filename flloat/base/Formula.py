from abc import abstractmethod, ABC
from typing import Sequence, FrozenSet, Set

from flloat.base.Interpretation import Interpretation
from flloat.base.Symbols import Symbols


class Formula(ABC):

    @abstractmethod
    def _members(self):
        raise NotImplementedError

    def __eq__(self, other):
        if type(other) is type(self):
            a = self._members()
            b = other._members()
            return self._members() == other._members()
        else:
            return False

    def __hash__(self):
        return hash(self._members())


    @abstractmethod
    def truth(self, i:Interpretation) -> bool:
        raise NotImplementedError


class Operator(Formula):
    base_expression = Symbols.ROUND_BRACKET_LEFT.value + "%s" + Symbols.ROUND_BRACKET_RIGHT.value

    @property
    def operator_symbol(self) -> str:
        raise NotImplementedError


class UnaryOperator(Operator):
    def __init__(self, f: Formula):
        self.f = f

    def __str__(self):
        return self.operator_symbol + Symbols.ROUND_BRACKET_LEFT.value + str(self.f) + Symbols.ROUND_BRACKET_RIGHT.value

    def _members(self):
        return self.operator_symbol, self.f

    def __lt__(self, other):
        return self.f.__lt__(other.f)


OperatorChilds = Sequence[Formula]
CommOperatorChilds = Set[Formula]


class BinaryOperator(Operator):
    """A generic binary formula"""

    def __init__(self, formulas:OperatorChilds):
        assert len(formulas) >= 2
        self.formulas = tuple(formulas)

    def __str__(self):
        return "(" + (" "+self.operator_symbol+" ").join(map(str,self.formulas)) + ")"

    def _members(self):
        return (self.operator_symbol, ) + tuple(self.formulas)



class CommutativeBinaryOperator(BinaryOperator):
    """A generic commutative binary formula"""

    def __init__(self, formulas:CommOperatorChilds):
        # Assuming idempotence: e.g. A & A === A
        if len(formulas)<2:
            f = formulas.pop()
            fs = [f, f]
        else:
            fs = formulas
        super().__init__(tuple(fs))
        self.formulas = self._popup()

    def _popup(self):
        """recursively find commutative binary operator
        among child formulas and pop up them at the same level"""
        res = ()
        for child in self.formulas:
            if type(child) == type(self):
                superchilds = child._popup()
                res += superchilds
            else:
                res += (child, )
        return tuple(res)

    def _members(self):
        return (self.operator_symbol, frozenset(self.formulas))



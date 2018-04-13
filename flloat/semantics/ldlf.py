from flloat.base.Formula import Formula
from flloat.base.Symbol import Symbol
from flloat.base.Symbols import Symbols


class LDLfFormula(Formula):
    pass

class LDLfLogicalTrue(LDLfFormula):

    def _members(self):
        return (Symbol(Symbols.LOGICAL_TRUE.value))


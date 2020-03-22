# FLLOAT


[![](https://img.shields.io/pypi/v/flloat.svg)](https://pypi.python.org/pypi/flloat)
![build](https://github.com/whitemech/flloat/workflows/FLLOAT%20Continuous%20Integration%20pipeline./badge.svg)
[![codecov](https://codecov.io/gh/whitemech/flloat/branch/master/graph/badge.svg)](https://codecov.io/gh/whitemech/flloat)
[![](https://img.shields.io/badge/docs-mkdocs-9cf)](https://www.mkdocs.org/)
[![](https://img.shields.io/badge/status-development-orange.svg)](https://img.shields.io/badge/status-development-orange.svg)
[![](https://img.shields.io/badge/flake8-checked-blueviolet)](https://img.shields.io/badge/flake8-checked-blueviolet)
[![](https://img.shields.io/badge/mypy-checked-blue)](https://img.shields.io/badge/mypy-checked-blue)
[![](https://img.shields.io/badge/license-Apache%202-lightgrey)](https://img.shields.io/badge/license-Apache%202-lightgrey)
[![](https://zenodo.org/badge/DOI/10.5281/zenodo.2577006.svg)](https://doi.org/10.5281/zenodo.2577006)

A Python implementation of the [FLLOAT](https://github.com/RiccardoDeMasellis/FLLOAT.git) library.

## Links

- GitHub: [https://github.com/whitemech/flloat](https://github.com/whitemech/flloat)
- PyPI: [https://pypi.org/project/flloat/](https://pypi.org/project/flloat/)
- Documentation: [https://whitemech.github.io/flloat](https://whitemech.github.io/flloat)
- Changelog: [https://whitemech.github.io/flloat/release-history/](https://whitemech.github.io/flloat/release-history/)
- Issue Tracker:[https://github.com/whitemech/flloat/issues](https://github.com/whitemech/flloat/issues)
- Download: [https://pypi.org/project/flloat/#files](https://pypi.org/project/flloat/#files)

## Install

- from [PyPI](https://pypi.org/project/flloat/):
```
pip install flloat
```
- or, from source (`master` branch):
```
pip install git+https://github.com/whitemech/flloat.git
```

- or, clone the repository and install:
```
git clone htts://github.com/whitemech/flloat.git
cd flloat
pip install .
```
## How to use

* Parse a LDLf formula:

```python
from flloat.parser.ldlf import LDLfParser

parser = LDLfParser()
formula_string = "<true*; A & B>tt"
formula = parser(formula_string)        # returns a LDLfFormula

print(formula)                          # prints "<((true)* ; (A & B))>(tt)"
print(formula.find_labels())            # prints {A, B}
```

*  Evaluate it over finite traces:

```python
t1 = [
    {"A": False, "B": False},
    {"A": True, "B": False},
    {"A": True, "B": False},
    {"A": True, "B": True},
    {"A": False, "B": False},
]
formula.truth(t1, 0)  # True
```

* Transform it into an automaton (``pythomata.SymbolicAutomaton`` object):

```python
dfa = formula.to_automaton()
assert dfa.accepts(t1)

# print the automaton
graph = dfa.to_graphviz()
graph.render("./my-automaton")  # requires Graphviz installed on your system.
```

Notice: `to_dot` requires [Graphviz](https://graphviz.gitlab.io/download/).
For info about how to use a `pythomata.DFA` please look at the [Pythomata docs](https://github.com/whitemech/pythomata).

* The same for a LTLf formula:

```python
from flloat.parser.ltlf import LTLfParser

# parse the formula
parser = LTLfParser()
formula = "F (A & !B)"
parsed_formula = parser(formula)

# evaluate over finite traces
t1 = [
    {"A": False, "B": False},
    {"A": True, "B": False},
    {"A": True, "B": False},
    {"A": True, "B": True},
    {"A": False, "B": False},
]
assert parsed_formula.truth(t1, 0)
t2 = [
    {"A": False, "B": False},
    {"A": True, "B": True},
    {"A": False, "B": True},
]
assert not parsed_formula.truth(t2, 0)

# from LTLf formula to DFA
dfa = parsed_formula.to_automaton()
assert dfa.accepts(t1)
assert not dfa.accepts(t2)
```

## Features

* Syntax, semantics and parsing support for the following formal languages:
    * Propositional Logic;
    * Linear Temporal Logic on Finite Traces
    * Linear Dynamic Logic on Finite Traces;

* Conversion from LTLf/LDLf formula to DFA

## Tests

To run the tests:

    tox

To run only the code tests:

    tox -e py37

To run only the code style checks:

    tox -e flake8

## Docs

To build the docs:


    mkdocs build


To view documentation in a browser


    mkdocs serve


and then go to [http://localhost:8000](http://localhost:8000)


## License

FLLOAT is released under the GNU Lesser General Public License v3.0 or later (LGPLv3+).

Copyright 2018-2020 WhiteMech


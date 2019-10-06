# FLLOAT


[![](https://img.shields.io/pypi/v/flloat.svg)](https://pypi.python.org/pypi/flloat)
[![](https://img.shields.io/travis/marcofavorito/flloat.svg)](https://travis-ci.org/marcofavorito/flloat)
[![](https://img.shields.io/pypi/pyversions/flloat.svg)](https://pypi.python.org/pypi/flloat)
[![](https://img.shields.io/badge/docs-mkdocs-9cf)](https://www.mkdocs.org/)
[![](https://img.shields.io/badge/status-development-orange.svg)](https://img.shields.io/badge/status-development-orange.svg)
[![](https://coveralls.io/repos/github/marcofavorito/flloat/badge.svg?branch=master)](https://coveralls.io/github/marcofavorito/flloat?branch=master)
[![](https://img.shields.io/badge/flake8-checked-blueviolet)](https://img.shields.io/badge/flake8-checked-blueviolet)
[![](https://img.shields.io/badge/mypy-checked-blue)](https://img.shields.io/badge/mypy-checked-blue)
[![](https://img.shields.io/badge/license-Apache%202-lightgrey)](https://img.shields.io/badge/license-Apache%202-lightgrey)
[![](https://zenodo.org/badge/DOI/10.5281/zenodo.2577006.svg)](https://doi.org/10.5281/zenodo.2577006)

A Python implementation of the [FLLOAT](https://github.com/RiccardoDeMasellis/FLLOAT.git) library.


* Free software: Apache 2.0 license
* Documentation: https://marcofavorito.github.io/flloat/

## Dependencies

The package depends on [Pythomata](https://marcofavorito.github.io/pythomata). Please follow the install instruction 
to get all the needed dependencies. 

## Install

- from [PyPI](https://pypi.org/project/flloat/):

      pip install flloat

- or, from source (`master` branch):

      pip install git+https://github.com/marcofavorito/flloat.git


- or, clone the repository and install:

      git clone htts://github.com/marcofavorito/flloat.git
      cd flloat
      pip install .

        pip install .

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

    from flloat.semantics.traces import FiniteTrace

    t1 = FiniteTrace.from_symbol_sets([
        {},
        {"A"},
        {"A"},
        {"A", "B"},
        {}
    ])
    formula.truth(t1, 0)  # True

```

* Transform it into an automaton (``pythomata.DFA`` object):

```python

    dfa = formula.to_automaton()

    # print the automaton
    dfa.to_dot("./automaton.DFA")

```

Notice: `to_dot` requires [Graphviz](https://graphviz.gitlab.io/download/).
For info about how to use a `pythomata.DFA` please look at the [Pythomata docs](https://github.com/marcofavorito/pythomata).

* The same for a LTLf formula:

```python

    from flloat.parser.ltlf import LTLfParser
    from flloat.semantics.traces import FiniteTrace

    # parse the formula
    parser = LTLfParser()
    formula_string = "F (A & !B)"
    formula = parser(formula_string)

    # evaluate over finite traces
    t1 = FiniteTrace.from_symbol_sets([
        {},
        {"A"},
        {"A"},
        {"A", "B"}
    ])
    assert formula.truth(t1, 0)

    # from LTLf formula to DFA
    dfa = formula.to_automaton()
    assert dfa.accepts(t1.trace)
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

Copyright 2018-2019 Marco Favorito


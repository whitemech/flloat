======
FLLOAT
======


.. image:: https://img.shields.io/pypi/v/flloat.svg
        :target: https://pypi.python.org/pypi/flloat

.. image:: https://img.shields.io/pypi/pyversions/flloat.svg
        :target: https://pypi.python.org/pypi/flloat

.. image:: https://img.shields.io/travis/MarcoFavorito/flloat.svg
        :target: https://travis-ci.org/MarcoFavorito/flloat

.. image:: https://readthedocs.org/projects/flloat/badge/?version=latest
        :target: https://flloat.readthedocs.io/en/latest/?badge=latest
        :alt: Documentation Status

.. image:: https://codecov.io/gh/MarcoFavorito/flloat/branch/master/graph/badge.svg
        :alt: Codecov coverage
        :target: https://codecov.io/gh/MarcoFavorito/flloat/branch/master/graph/badge.svg

.. image:: https://badges.gitter.im/rltg_flloat/Lobby.svg
     :alt: Join the chat at https://gitter.im/rltg_flloat/Lobby
     :target: https://gitter.im/rltg_flloat/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge


A Python implementation of the `FLLOAT`_ library.

.. _FLLOAT: https://github.com/RiccardoDeMasellis/FLLOAT.git


* Free software: MIT license
* Documentation: https://flloat.readthedocs.io.

Install
--------

From PyPI:

::

    pip install flloat

From repo (e.g. from branch develop):

::

    pip install git+https://github.com/MarcoFavorito/flloat@develop#egg=flloat


You might need to complete some extra step. Please check the following installation guides:

* `pythomata <https://github.com/MarcoFavorito/pythomata#install>`_
* `Graphviz <https://graphviz.gitlab.io/download/>`_

How to use
-----------

* Parse a LDLf formula:

.. code-block:: python

    from flloat.parser.ldlf import LDLfParser

    parser = LDLfParser()
    formula = "<true*; A & B>tt"
    parsed_formula = parser(formula)        # returns a LDLfFormula

    print(parsed_formula)                   # prints "<((true)* ; (A & B))>(tt)"
    print(parsed_formula.find_labels())     # prints {A, B}


*  Evaluate it over finite traces:

.. code-block:: python

    from flloat.semantics.ldlf import FiniteTrace

    t1 = FiniteTrace.fromStringSets([
        {},
        {"A"},
        {"A"},
        {"A", "B"},
        {}
    ])
    parsed_formula.truth(t1, 0)  # True


* Transform it into an automaton (``pythomata.DFA`` object):

.. code-block:: python

    dfa = parsed_formula.to_automaton(determinize=True)

    # print the automaton
    dfa.to_dot("./automaton.DFA")

Notice: ``to_dot`` requires `Graphviz <https://graphviz.gitlab.io/download/>`_.
For info about how to use a ``pythomata.DFA`` please look at the `docs <https://github.com/MarcoFavorito/pythomata>`_.

* The same for a LTLf formula:

.. code-block:: python

    from flloat.parser.ltlf import LTLfParser
    from flloat.base.Symbol import Symbol
    from flloat.semantics.ldlf import FiniteTrace

    # parse the formula
    parser = LTLfParser()
    formula = "F (A & !B)"
    parsed_formula = parser(formula)

    # evaluate over finite traces
    t1 = FiniteTrace.fromStringSets([
        {},
        {"A"},
        {"A"},
        {"A", "B"}
    ])
    assert parsed_formula.truth(t1, 0)

    # from LTLf formula to DFA
    dfa = parsed_formula.to_automaton(determinize=True)
    assert dfa.word_acceptance(t1.trace)

Features
--------

* Syntax, semantics and parsing support for the following formal languages:
    * Propositional Logic;
    * Linear Temporal Logic on Finite Traces
    * Linear Dynamic Logic on Finite Traces;

* Conversion from LTLf/LDLf formula to NFA, DFA and DFA on-the-fly

Credits
-------

This package was created with Cookiecutter_ and the `audreyr/cookiecutter-pypackage`_ project template.

.. _Cookiecutter: https://github.com/audreyr/cookiecutter
.. _`audreyr/cookiecutter-pypackage`: https://github.com/audreyr/cookiecutter-pypackage

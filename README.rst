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
--------

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
        {"A", "B"}
    ])
    parsed_formula.truth(trace, 0)  # True


* Transform it into an automaton (``pythomata.DFA`` object):

.. code-block:: python

    dfa = parsed_formula.to_automaton(determinize=True)

    # print the automaton
    dfa.to_dot("./automaton.DFA")

Notice: ``to_dot`` requires `Graphviz <https://graphviz.gitlab.io/download/>`_.
For info about how to use a ``pythomata.DFA`` please look at the `docs <https://github.com/MarcoFavorito/pythomata>`_.

Features
--------

* Syntax, semantics and parsing support for the following formal languages:

    * Propositional Logic;
    * Linear Dynamic Logic on Finite Traces;

* Conversion from LDLf formula to NFA, DFA and DFA on-the-fly

Credits
-------

This package was created with Cookiecutter_ and the `audreyr/cookiecutter-pypackage`_ project template.

.. _Cookiecutter: https://github.com/audreyr/cookiecutter
.. _`audreyr/cookiecutter-pypackage`: https://github.com/audreyr/cookiecutter-pypackage

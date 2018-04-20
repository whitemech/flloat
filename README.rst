======
FLLOAT
======


.. image:: https://img.shields.io/pypi/v/flloat.svg
        :target: https://pypi.python.org/pypi/flloat

.. image:: https://img.shields.io/travis/MarcoFavorito/flloat.svg
        :target: https://travis-ci.org/MarcoFavorito/flloat

.. image:: https://readthedocs.org/projects/flloat/badge/?version=latest
        :target: https://flloat.readthedocs.io/en/latest/?badge=latest
        :alt: Documentation Status


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

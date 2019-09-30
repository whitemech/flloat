#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""The setup script."""
import os

from setuptools import setup, find_packages

with open('README.rst') as readme_file:
    readme = readme_file.read()

with open('HISTORY.rst') as history_file:
    history = history_file.read()

here = os.path.abspath(os.path.dirname(__file__))
about = {}
with open(os.path.join(here, 'flloat', '__version__.py'), 'r') as f:
    exec(f.read(), about)


setup(
    name=about['__title__'],
    description=about['__description__'],
    version=about['__version__'],
    author=about['__author__'],
    url=about['__url__'],
    author_email=about["__author_email__"],
    long_description=readme + '\n\n' + history,
    classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: Apache License',
        'Natural Language :: English',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
    ],
    install_requires=["ply", "pythomata"],
    license=about["__license__"],
    include_package_data=True,
    keywords='flloat',
    packages=find_packages(include=['flloat*']),
    test_suite='tests',
    tests_require=["pytest"],
    zip_safe=False,
)

#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""The setup script."""

from setuptools import setup, find_packages

with open('README.rst') as readme_file:
    readme = readme_file.read()

with open('HISTORY.rst') as history_file:
    history = history_file.read()

requirements = ['ply', 'pythomata']

setup_requirements = ['pytest-runner', ]

test_requirements = ['pytest', ]

setup(
    author="Marco Favorito",
    author_email='marco.favorito@gmail.com',
    classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Natural Language :: English',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
    ],
    description="A Python implementation of the FLLOAT library. link: https://github.com/RiccardoDeMasellis/FLLOAT.git",
    install_requires=requirements,
    license="MIT license",
    long_description=readme + '\n\n' + history,
    include_package_data=True,
    keywords='flloat',
    name='flloat',
    packages=find_packages(include=['flloat*']),
    setup_requires=setup_requirements,
    test_suite='tests',
    tests_require=test_requirements,
    url='https://github.com/MarcoFavorito/flloat',
    version='0.1.3post6',
    zip_safe=False,
)

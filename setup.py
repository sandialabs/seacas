#!/usr/bin/env python

from setuptools import setup, Extension
from glob import glob
from exo_build_ext import cmake_build_ext

s = setup(name='exodus',
      description='A python wrapper of some of the exodus library',
      version='1.21.1',
      url='https://github.com/sandialabs/seacas',
      py_modules=['exodus3'],
      package_dir={"":"build/packages/seacas/scripts"},
      cmdclass = {'build_ext': cmake_build_ext},
      ext_modules = [Extension("exodus", [""])]
)

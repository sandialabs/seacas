import contextlib
import shutil
from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext
from pathlib import Path
import subprocess
import sys
import os

class CMakeExtension(Extension):
    def __init__(self, name, cmake_lists_dir='.', **kwargs):
        Extension.__init__(self, name, sources=[], **kwargs)
        self.cmake_lists_dir = os.path.abspath(cmake_lists_dir)

class cmake_build_ext(build_ext):
    def build_extensions(self):
        try:
            out = subprocess.check_output(['cmake', '--version'])
        except OSError as e:
            raise RuntimeError('Cannot find CMake executable') from e

        for ext in self.extensions:

            extdir = os.path.abspath(os.path.dirname(self.get_ext_fullpath(ext.name)))

            cmake_args = [
                '-DCMAKE_BUILD_TYPE=release',
                f'-DCMAKE_LIBRARY_OUTPUT_DIRECTORY={extdir}',
                f'-DCMAKE_ARCHIVE_OUTPUT_DIRECTORY={self.build_temp}',
                f'-DPYTHON_EXECUTABLE={sys.executable}'
            ]


            if not os.path.exists(self.build_temp):
                os.makedirs(self.build_temp)

            # Config
            build_dir = os.path.join(os.getcwd(), "build")
            env = dict(os.environ)
            env["JOBS"] = str(os.cpu_count())
            env["MPI"] = "YES"
            subprocess.check_call(['./install-tpl.sh'] + cmake_args, env=env)
            with contextlib.suppress(Exception):
                os.mkdir("build")
            subprocess.check_call(['../cmake-exodus'] + cmake_args, cwd=build_dir, env=env)

            # Build
            subprocess.check_call(['cmake', '--build', '.'], cwd=build_dir, env=env)
            subprocess.check_call(['cmake', '--install', '.'], cwd=build_dir, env=env)
            # for sofile in list(Path('lib/').glob('*.so*')):
                # shutil.copy(str(sofile), extdir)

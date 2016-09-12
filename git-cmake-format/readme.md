# git-cmake-format

This project aims to provide a quick and easy way to integrate clang-format into
your CMake project hosted in a git repository, it consists of three elements.

* `CMakeLists.txt` provides the custom `format` target
* `git-pre-commit-hook` blocks commits of unformatted C/C++ files
* `git-cmake-format.py` is called by the `format` target or the `pre-commit`
  hook, it queries git for edited files, then block the commit or formats the
  sources

## Dependencies

There are three dependencies:

* `git`
* `python` 2.7+
* `clang-format`

## Usage

To make use of this project you can either add it as a submodule to your
existing project, or copy the files into your repository. Now add the following
to your `CMakeLists.txt`.
```
add_subdirectory(/path/to/git-cmake-format)
```

Next you can generate your build system, assuming the required dependencies are
available on your path.

```
cd build
cmake ..
```

The installation of the `pre-commit` hook is done at CMake time during the
generation of the build system, if you have followed these steps it is already
installed at `/path/to/your/project/.git/hooks/pre-commit`.

### Options

It is possible to specify the path to any of the executables this project
depends upon using the following CMake variables.

* `GCF_GIT_PATH:STRING=/path/to/git`
* `GCF_PYTHON_PATH:STRING=/path/to/python`
* `GCF_CLANGFORMAT_PATH:STRING=/path/to/clang-format` defaults to `clang-format`

It is also possible to set the `-style=` command line argument for
`clang-format` with the following options, the default is `file`

* `GCF_CLANGFORMAT_STYLE:STRING=WebKit`

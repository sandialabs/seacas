A capability in testing is to build an "on-node parallel" version of
SEACAS using the Kokkos package. This includes a "Kokkos-aware" version of
the Ioss library. The modifications to the build process described in the
README file are shown below:

## Additional Libraries

You will need the following library if you want to use Kokkos with CUDA as the backend:

* [CUDA](#CUDA)

#### CUDA

  * CUDA is already installed on many platforms. In many cases, typing something like `module load cuda` should be sufficient. Loading the module would typically set environment variables, such as CUDA_PATH.
  * If installing yourself, see www.nvidia.com/object/cuda_home_new.html

## Configure, Build, and Install SEACAS
Build as described in README-PARALLEL.md if using MPI or as described in README.md
otherwise.  If you are using the `cmake-config-kokkos` script, change `MPI` to
`ON` or `OFF` as appropriate, change `CUDA` to `ON` or `OFF`
as appropriate, and then continue.

If using your own cmake script or directly calling cmake, be sure to do the following.

* Be sure the Kokkos package is enabled.

* If using OpenMP as a Kokkos backend, be sure to enable OpenMP.
If using CUDA as a backend, OpenMP can still be enabled.

```
-D SEACASProj_ENABLE_OpenMP:Bool=ON
```

* If using CUDA as a Kokkos backend,
   a. Enable CUDA and set the CUDA root directory.
```
-D TPL_ENABLE_CUDA:Bool=ON
-D CUDA_TOOLKIT_ROOT_DIR:Path=${CUDA_PATH}
```
   b. Some envionment variables need to be set. These can be set in your configure script, but then be sure to source the script rather than just running it so that your environment is correct when running `make`.
```
export OMPI_CXX=<SEACAS-source-directory>/packages/kokkos/config/nvcc_wrapper
export CUDA_MANAGED_FORCE_DEVICE_ALLOC=1
```




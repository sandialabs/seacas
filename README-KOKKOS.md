A capability in testing is to build an "on-node parallel" version of
SEACAS using the Kokkos package. This includes a "Kokkos-aware" version of
the Ioss library. The modifications to the build process described in the
README file are shown below:

## Additional Libraries

You will need the following library if you want to use Kokkos with CUDA as the backend:

* [CUDA](#cuda)

### CUDA

  * CUDA is already installed on many platforms. In many cases, typing something like `module load cuda` should be sufficient. Loading the module would typically set environment variables, such as CUDA_ROOT.
  * If installing yourself, see www.nvidia.com/object/cuda_home_new.html

## Configure, Build, and Install SEACAS
Build as described in README-PARALLEL.md if using MPI or as described in README.md
otherwise.  If you are using the `cmake-config-kokkos` script, change `MPI` to
`ON` or `OFF` as appropriate, change `CUDA` to `ON` or `OFF`
as appropriate, and then source the configure script.

```bash
source cmake-config-kokkos
```

If using your own cmake script or directly calling cmake, be sure to do the following.

* Enable the Kokkos package.

* If using OpenMP as the Kokkos backend, enable OpenMP and disable Pthread.
If using CUDA as the backend, OpenMP can still be enabled.

```bash
-D SEACASProj_ENABLE_OpenMP:Bool=ON
-D TPL_ENABLE_Pthread:Bool=OFF
```

* If using CUDA as the Kokkos backend, enable CUDA, set the CUDA root directory, and disable Pthread.
```bash
-D TPL_ENABLE_CUDA:Bool=ON
-D CUDA_TOOLKIT_ROOT_DIR:Path=${CUDA_PATH}
-D TPL_ENABLE_Pthread:Bool=OFF
```

* If using CUDA as the Kokkos backend, some envionment variables need to be set. These can be set in your configure script, but then be sure to source the script rather than just running it so that your environment is correct when running `make`.
  ```bash
  export OMPI_CXX=<SEACAS-source-directory>/packages/kokkos/config/nvcc_wrapper
  export CUDA_MANAGED_FORCE_DEVICE_ALLOC=1
  ```

Finally, build and install

```bash
make
make install
```

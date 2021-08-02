# @HEADER
# ************************************************************************
#
#            Trilinos: An Object-Oriented Solver Framework
#                 Copyright (2001) Sandia Corporation
#
#
# Copyright (2001) Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000, there is a non-exclusive license for use of this
# work by or on behalf of the U.S. Government.  Export of this program
# may require a license from the United States Government.
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the Corporation nor the names of the
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY SANDIA CORPORATION "AS IS" AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
# PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL SANDIA CORPORATION OR THE
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# NOTICE:  The United States Government is granted for itself and others
# acting on its behalf a paid-up, nonexclusive, irrevocable worldwide
# license in this data to reproduce, prepare derivative works, and
# perform publicly and display publicly.  Beginning five (5) years from
# July 25, 2001, the United States Government is granted for itself and
# others acting on its behalf a paid-up, nonexclusive, irrevocable
# worldwide license in this data to reproduce, prepare derivative works,
# distribute copies to the public, perform publicly and display
# publicly, and to permit others to do so.
#
# NEITHER THE UNITED STATES GOVERNMENT, NOR THE UNITED STATES DEPARTMENT
# OF ENERGY, NOR SANDIA CORPORATION, NOR ANY OF THEIR EMPLOYEES, MAKES
# ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY LEGAL LIABILITY OR
# RESPONSIBILITY FOR THE ACCURACY, COMPLETENESS, OR USEFULNESS OF ANY
# INFORMATION, APPARATUS, PRODUCT, OR PROCESS DISCLOSED, OR REPRESENTS
# THAT ITS USE WOULD NOT INFRINGE PRIVATELY OWNED RIGHTS.
#
# ************************************************************************
# @HEADER

# First, set up the variables for the (backward-compatible) TriBITS way of
# finding GTest.  These are used in case FIND_PACKAGE(GTest ...) is
# not called or does not find GTest.  Also, these variables need to be
# non-null in order to trigger the right behavior in the function
# TRIBITS_TPL_FIND_INCLUDE_DIRS_AND_LIBRARIES().
set(REQUIRED_HEADERS gtest.h)
set(REQUIRED_LIBS_NAMES gtest)

# Second, search for GTest components (if allowed) using the standard
# FIND_PACKAGE(GTest ...).
tribits_tpl_allow_pre_find_package(GTest  GTest_ALLOW_PREFIND)
if (GTest_ALLOW_PREFIND)
  message("-- Using FIND_PACKAGE(GTest ...) ...") 
  find_package(GTest CONFIG)
  if (GTest_FOUND)
    get_target_property(GTest_INCLUDE_DIRS GTest::gtest INTERFACE_INCLUDE_DIRECTORIES)
    get_target_property(GTest_CONFIGURATION GTest::gtest IMPORTED_CONFIGURATIONS)
    get_target_property(GTest_LIBRARIES GTest::gtest IMPORTED_LOCATION_${GTest_CONFIGURATION})
    # Tell TriBITS that we found GTest and there no need to look any further!
    set(TPL_GTest_INCLUDE_DIRS ${GTest_INCLUDE_DIRS} CACHE PATH "...")
    set(TPL_GTest_LIBRARIES ${GTest_LIBRARIES} CACHE FILEPATH "...")
    set(TPL_GTest_LIBRARY_DIRS ${GTest_LIBRARY_DIRS} CACHE PATH "...")
  endif()
endif()

# Third, call `tribits_tpl_find_include_dirs_and_libraries()`
tribits_tpl_find_include_dirs_and_libraries( GTest
  REQUIRED_HEADERS ${REQUIRED_HEADERS}
  REQUIRED_LIBS_NAMES ${REQUIRED_LIBS_NAMES}
  )
# NOTE: If `find_package(GTest ...)` was called and successfully found
# GTest, then `tribits_tpl_find_include_dirs_and_libraries()` will use the
# already-set variables and just print them out.  This is the final "hook"
# into the TriBITS TPL system.

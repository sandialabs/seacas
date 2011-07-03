divert(-1)
# Copyright (c) 1998 Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000 with Sandia Corporation, the U.S. Governement
# retains certain rights in this software.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
# 
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.  
# 
#     * Neither the name of Sandia Corporation nor the names of its
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 


# Name of system platform (for use in comments)
define(`M4__SYSTEM', MICROSOFT)

# Includes needed at the top of a file of C to be called from FORTRAN
define(`M4__STRING_DESCRIPTOR_INCLUDES',
`extern long fslen(int); /* returns declared fortran string length of nth arg */
')

# transformation from fortran name to name of C module
define(`NAMEF',`$1`'define(`STR_COUNT',1)')# for microsoft, just use same name

# transformation from string name to corresponding argument name
define(`STRINGF',`$1')

# extra arguments, if any, for string length
define(`STRINGX',`')

# declaration to be used for argument name descriptor
define(`STRINGD',`
    char *$1;	`$2'')

# declarations and initializations of canonical local variables
define(`STRINGL',`
    int $1`'`len' = fslen(STR_COUNT`'define(`STR_COUNT',incr(STR_COUNT)));') # all strings must be null-terminated

# FORTRAN declaration for a long integer (e.g. integer*4 for Microsoft)
define(`LONG_INT',`integer*4')

# FORTRAN declaration for a short integer (e.g. integer*2)
define(`SHORT_INT',`integer*2')

# FORTRAN declaration for an integer byte (e.g. integer*1 or byte)
define(`BYTE_INT',`integer*1')

# FORTRAN declaration for double precision (e.g. real for a Cray)
define(`DOUBLE_PRECISION',`double precision')

# FORTRAN syntax for including a file
define(`F_INCLUDE',`$`include': "$1"')

# include declaring C interfaces, needed in FORTRAN when calling C, e.g.
# Microsoft FORTRAN needs this
define(`M4__C_INTERFACE_DECLARATIONS',
`F_INCLUDE(msoft.int)
')

divert(0)dnl

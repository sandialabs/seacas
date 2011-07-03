divert(-1)dnl
# Copyright (c) 2005 Sandia Corporation. Under the terms of Contract
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

#
# diversion 1 is for collecting formal arguments
# diversion 2 is for extra formal arguments for string lengths
# diversion 3 is for formal argument declarations
# diversion 4 is for extra local variables derived from formal arguments
#
define(`STRING',`divert(1)ifdef(`INIT',,`, ')STRINGF(`$1')`'undefine(`INIT')`'STRINGX(`$1')`'divert(3)`'STRINGD(`$1',`$2')`'divert(4)`'STRINGL(`$1')`'divert(0)')dnl
define(`INTSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    int		*$1;	`$2'divert(0)')dnl
define(`LONGSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    long		*$1;	`$2'divert(0)')dnl
define(`FLOATSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    float	*$1;	`$2'divert(0)')dnl
define(`DOUBLESTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    double	*$1;	`$2'divert(0)')dnl
define(`REALSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    real	*$1;	`$2'divert(0)')dnl
#
# The following is for a pointer to a single character, not a Fortran 
# character variable
#
define(`CHARSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    char	*$1;	`$2'divert(0)')dnl
define(`VOIDSTAR',`divert(1)ifdef(`INIT',,`, ')$1`'undefine(`INIT')divert(3)
    void	*$1;	`$2'divert(0)')dnl
define(`M4__PROTO',`define(`INIT',1)$2`'NAMEF($1)(undivert(1)undivert(2))undivert(3)')
define(`M4__LOCALS',`undivert(4)')

# Note: override the following default definitions in OS.m4, where necessary

# Includes needed at the top of a file of C to be called from FORTRAN,
# e.g. "#include descrip" for VMS
define(`M4__STRING_DESCRIPTOR_INCLUDES',`')

# Special #defines needed for this FORTRAN, e.g. FORTRAN_HAS_NO_SHORT 
define(`M4__FORTRAN_DEFINES',`')

# FORTRAN syntax for including a file, e.g. `$include: "filename"' for msoft
define(`M4__RIGHT_QUOTE',')
define(`F_INCLUDE',`      `include' M4__RIGHT_QUOTE`'$1`'M4__RIGHT_QUOTE')

# include declaring C interfaces, needed in FORTRAN when calling C, e.g.
# Microsoft FORTRAN needs to include msoft.int
define(`M4__C_INTERFACE_DECLARATIONS',`')
divert(0)dnl

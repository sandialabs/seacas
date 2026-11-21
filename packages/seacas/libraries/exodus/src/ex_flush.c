/*
 * Copyright(C) 1999-2020, 2024 National Technology & Engineering Solutions
 * of Sandia, LLC (NTESS).  Under the terms of Contract DE-NA0003525 with
 * NTESS, the U.S. Government retains certain rights in this software.
 *
 * See packages/seacas/LICENSE for details
 */

#include "exodusII.h"     // for ex_err, etc
#include "exodusII_int.h" // for EX_FATAL, EX_NOERR

/*!
\ingroup Utilities

The function ex_flush() completes any output that the library is still
holding.  It is only meaningful when #EX_OPT_NONBLOCKING has been enabled on
the file, in which case variable writes are posted to PnetCDF and completed
later; ex_flush() is what completes them.  On every other kind of file it
succeeds and does nothing.

Call it at a natural boundary -- typically after writing all variables for a
time step -- to bound how much output the library buffers.  ex_update() and
ex_close() flush automatically, so an explicit call is never required for
correctness.

\note This is a collective call on a parallel file.  Every process that has
the file open must call it, including processes that wrote nothing, because
the underlying completion is itself collective.

\return In case of an error, ex_flush() returns a negative number; a warning
        will return a positive number.

 \param exoid  exodus file ID returned from a previous call to ex_create(),
               ex_open(), ex_create_par() or ex_open_par().
*/
int ex_flush(int exoid)
{
  EX_FUNC_ENTER();

  if (exi_check_valid_file_id(exoid, __func__) == EX_FATAL) {
    EX_FUNC_LEAVE(EX_FATAL);
  }

  EX_FUNC_LEAVE(exi_nb_flush(exoid));
}

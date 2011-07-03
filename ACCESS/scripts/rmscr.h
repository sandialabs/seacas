/*
 * rmscr.h - remove scratch files after running code.
 * $Id: rmscr.h,v 1.1 2008/10/31 05:19:57 gdsjaar Exp $
*/

/*
  The design of the file will be the following
  
  #if defined(TRANSLATOR)
  ...
  #endif

  #if defined(GRAPHICS)
  ...
  #endif

  #if define(ANALYSIS)
  ...
  #endif
*/

#if defined (TRANSLATOR)
XCOMM
XCOMM Remove temporary files
XCOMM

XCOMM Possible scratch files to delete
if (-e ${TEMP}/ex2ex1v2.$$) rm -f ${TEMP}/ex2ex1v2.$$
if (-e ${TEMP}/ex1ex2v2.$$) rm -f ${TEMP}/ex1ex2v2.$$

#if defined(PATEXO)
XCOMM patexo scratch files
if (-e ftn11) then
	rm -f ftn*
endif
#endif /* PATEXO */

#endif /* TRANSLATOR */


/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(GRAPHICS)
XCOMM
XCOMM Remove temporary files
XCOMM

if (-e STKFNT) rm -f STKFNT
if (-e SSRFNT) rm -f SSRFNT
if (-e ROMFNT) rm -f ROMFNT

#if defined(RUN1)
if (-e ${TEMP}/Blot.90.$$) rm -f ${TEMP}/Blot.90.$$
#endif

#if defined(RUN2)
XCOMM Must be split, FOR001 not always defined
if (${aprepro} == "file") then 
	rm -f ${FOR001}
endif
#endif

#endif /* GRAPHICS */

/***************************************************************/
/***************************************************************/
/***************************************************************/

#if defined(ANALYSIS)

XCOMM Possible scratch files to delete
if (-e ${codename}.g.$$) rm -f ${codename}.g.$$
#if defined(MERLIN2)
if (-e ${codename}.e.$$) rm -f ${codename}.e.$$
if (-e Merlin10.$$) rm -f Merlin10.$$
if (-e Merlin11.$$) rm -f Merlin11.$$
#endif
#if defined(COYOTE)
if (-e ${codename}.e.$$) rm -f ${codename}.e.$$
if (-e ${codename}.rsin.$$) rm -f ${codename}.rsin.$$
#endif
if (-e ${TEMP}/aprepro.$$) rm -f ${TEMP}/aprepro.$$

#endif

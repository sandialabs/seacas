!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Zoltan Library for Parallel Applications                                   !
! For more info, see the README file in the top-level Zoltan directory.      ! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  CVS File Information :
!     $RCSfile: farg_nas.f,v $
!     $Author: gdsjaar $
!     $Date: 2006/08/10 22:26:59 $
!     $Revision: 1.1 $
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Command line argument functions for NASoftware FortranPlus 2.0

      integer function mpir_iargc()
      use nas_system
      mpir_iargc = iargc()
      return
      end

      subroutine mpir_getarg( i, s )
      use nas_system
      integer       i
      character*(*) s
      call getarg(i,s)
      return
      end

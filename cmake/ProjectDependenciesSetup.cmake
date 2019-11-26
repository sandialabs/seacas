function(disable_if_no_fortran package)
IF (NOT ${PROJECT_NAME}_ENABLE_Fortran)
  IF (${PROJECT_NAME}_ENABLE_SEACAS${package})
    MESSAGE("-- "
      "WARNING: Setting ${PROJECT_NAME}_ENABLE_SEACAS${package}=OFF"
      " even though it was set to ${${PROJECT_NAME}_ENABLE_SEACAS${package}}"
      " because ${PROJECT_NAME}_ENABLE_Fortran=OFF!"
      )
  ELSEIF("${${PROJECT_NAME}_ENABLE_SEACAS${package}}" STREQUAL "")
     MESSAGE("-- "
      "NOTE: Setting ${PROJECT_NAME}_ENABLE_SEACAS${package}=OFF"
      " because ${PROJECT_NAME}_ENABLE_Fortran=OFF!"
       )
  ENDIF()
  SET(${PROJECT_NAME}_ENABLE_SEACAS${package} OFF PARENT_SCOPE)
ENDIF()
endfunction()

disable_if_no_fortran(Mapvarlib)
disable_if_no_fortran(Exodus_for)
disable_if_no_fortran(ExoIIv2for32)
disable_if_no_fortran(Supes)
disable_if_no_fortran(Suplib)
disable_if_no_fortran(PLT)
disable_if_no_fortran(Blot)
disable_if_no_fortran(Fastq)
disable_if_no_fortran(SVDI)
disable_if_no_fortran(Algebra)
disable_if_no_fortran(Exotxt)
disable_if_no_fortran(Gjoin)
disable_if_no_fortran(Gen3D)
disable_if_no_fortran(Genshell)
disable_if_no_fortran(Grepos)
disable_if_no_fortran(Explore)
disable_if_no_fortran(Mapvar)
disable_if_no_fortran(Mapvar-kd)
disable_if_no_fortran(Numbers)
disable_if_no_fortran(Txtexo)
disable_if_no_fortran(Ex2ex1v2)
disable_if_no_fortran(Ex1ex2v2)

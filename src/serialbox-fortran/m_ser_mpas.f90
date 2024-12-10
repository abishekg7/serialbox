!------------------------------------------------------------*- Fortran -*-----
!
!                              S E R I A L B O X
!
! This file is distributed under terms of BSD license.
! See LICENSE.txt for more information.
!
!------------------------------------------------------------------------------
!
!+ This module contains the FortranTestGenerator (FTG) frontend of Serialbox.
!+ For FTG see https://github.com/fortesg/fortrantestgenerator
!
!------------------------------------------------------------------------------

MODULE m_ser_mpas

!------------------------------------------------------------------------------
!
! Description:
!
!   This module contains simplified wrapper subroutines for the Fortran interface
!   of Serialbox (m_serialize.f90) to be used by the FortranTestGenerator
!   (https://github.com/fortesg/fortrantestgenerator), plus additional subroutines
!   for allocating array variables based on the stored sizes and bounds.
!
! Current Code Owner: Christian Hovy, Universitaet Hamburg
!  email:  hovy@informatik.uni-hamburg.de
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!============================================================================

USE iso_c_binding
USE m_serialize

IMPLICIT NONE

PUBLIC :: ignore_bullshit, ignore_bullshit_max_dim_size, ignore_bullshit_allow_negative_indices, ignore_not_existing, &
          mpas_set_serializer, mpas_get_serializer, mpas_destroy_serializer, mpas_print_serializer_debuginfo, &
          mpas_set_savepoint, mpas_get_savepoint, mpas_destroy_savepoint, &
          mpas_add_serializer_metainfo, mpas_get_serializer_metainfo,  mpas_add_field_metainfo, mpas_get_field_metainfo, &
          mpas_add_savepoint_metainfo, mpas_get_savepoint_metainfo, &
          mpas_field_exists, mpas_get_size, mpas_get_bounds, mpas_register_only, mpas_write, mpas_read, &
          mpas_allocate_pointer, mpas_allocate_allocatable, mpas_allocate_and_read_pointer, mpas_allocate_and_read_allocatable

PRIVATE

CHARACTER(LEN=*), PARAMETER :: module_name = 'm_ser_mpas'

INTERFACE mpas_set_serializer
  MODULE PROCEDURE &
    mpas_set_serializer_create, &
    mpas_set_serializer_existing
END INTERFACE

INTERFACE mpas_set_savepoint
  MODULE PROCEDURE &
    mpas_set_savepoint_create, &
    mpas_set_savepoint_existing
END INTERFACE

INTERFACE mpas_add_serializer_metainfo
  MODULE PROCEDURE &
    mpas_add_serializer_metainfo_b, &
    mpas_add_serializer_metainfo_i, &
    mpas_add_serializer_metainfo_l, &
    mpas_add_serializer_metainfo_f, &
    mpas_add_serializer_metainfo_d, &
    mpas_add_serializer_metainfo_s
END INTERFACE

INTERFACE mpas_get_serializer_metainfo
  MODULE PROCEDURE &
    mpas_get_serializer_metainfo_b, &
    mpas_get_serializer_metainfo_i, &
    mpas_get_serializer_metainfo_l, &
    mpas_get_serializer_metainfo_f, &
    mpas_get_serializer_metainfo_d
END INTERFACE

INTERFACE mpas_add_field_metainfo
  MODULE PROCEDURE &
    mpas_add_field_metainfo_b, &
    mpas_add_field_metainfo_i, &
    mpas_add_field_metainfo_l, &
    mpas_add_field_metainfo_f, &
    mpas_add_field_metainfo_d, &
    mpas_add_field_metainfo_s
END INTERFACE

INTERFACE mpas_get_field_metainfo
  MODULE PROCEDURE &
    mpas_get_field_metainfo_b, &
    mpas_get_field_metainfo_i, &
    mpas_get_field_metainfo_l, &
    mpas_get_field_metainfo_f, &
    mpas_get_field_metainfo_d
END INTERFACE

INTERFACE mpas_add_savepoint_metainfo
  MODULE PROCEDURE &
    mpas_add_savepoint_metainfo_b, &
    mpas_add_savepoint_metainfo_i, &
    mpas_add_savepoint_metainfo_l, &
    mpas_add_savepoint_metainfo_f, &
    mpas_add_savepoint_metainfo_d, &
    mpas_add_savepoint_metainfo_s
END INTERFACE

INTERFACE mpas_get_savepoint_metainfo
  MODULE PROCEDURE &
    mpas_get_savepoint_metainfo_b, &
    mpas_get_savepoint_metainfo_i, &
    mpas_get_savepoint_metainfo_l, &
    mpas_get_savepoint_metainfo_f, &
    mpas_get_savepoint_metainfo_d
END INTERFACE

INTERFACE mpas_write
  MODULE PROCEDURE &
    mpas_write_string, &
    mpas_write_logical_0d, &
    mpas_write_logical_1d, &
    mpas_write_logical_2d, &
    mpas_write_logical_3d, &
    mpas_write_logical_4d, &
    mpas_write_bool_0d, &
    mpas_write_bool_1d, &
    mpas_write_bool_2d, &
    mpas_write_bool_3d, &
    mpas_write_bool_4d, &
    mpas_write_int_0d, &
    mpas_write_int_1d, &
    mpas_write_int_2d, &
    mpas_write_int_3d, &
    mpas_write_int_4d, &
    mpas_write_long_0d, &
    mpas_write_long_1d, &
    mpas_write_long_2d, &
    mpas_write_long_3d, &
    mpas_write_long_4d, &
    mpas_write_float_0d, &
    mpas_write_float_1d, &
    mpas_write_float_2d, &
    mpas_write_float_3d, &
    mpas_write_float_4d, &
    mpas_write_double_0d, &
    mpas_write_double_1d, &
    mpas_write_double_2d, &
    mpas_write_double_3d, &
    mpas_write_double_4d
END INTERFACE

INTERFACE mpas_read
  MODULE PROCEDURE &
    mpas_read_string, &
    mpas_read_logical_0d, &
    mpas_read_logical_1d, &
    mpas_read_logical_2d, &
    mpas_read_logical_3d, &
    mpas_read_logical_4d, &
    mpas_read_bool_0d, &
    mpas_read_bool_1d, &
    mpas_read_bool_2d, &
    mpas_read_bool_3d, &
    mpas_read_bool_4d, &
    mpas_read_int_0d, &
    mpas_read_int_1d, &
    mpas_read_int_2d, &
    mpas_read_int_3d, &
    mpas_read_int_4d, &
    mpas_read_long_0d, &
    mpas_read_long_1d, &
    mpas_read_long_2d, &
    mpas_read_long_3d, &
    mpas_read_long_4d, &
    mpas_read_float_0d, &
    mpas_read_float_1d, &
    mpas_read_float_2d, &
    mpas_read_float_3d, &
    mpas_read_float_4d, &
    mpas_read_double_0d, &
    mpas_read_double_1d, &
    mpas_read_double_2d, &
    mpas_read_double_3d, &
    mpas_read_double_4d
END INTERFACE

INTERFACE mpas_allocate_pointer
  MODULE PROCEDURE &
    mpas_allocate_pointer_logical_0d, &
    mpas_allocate_pointer_logical_1d, &
    mpas_allocate_pointer_logical_2d, &
    mpas_allocate_pointer_logical_3d, &
    mpas_allocate_pointer_logical_4d, &
    mpas_allocate_pointer_bool_0d, &
    mpas_allocate_pointer_bool_1d, &
    mpas_allocate_pointer_bool_2d, &
    mpas_allocate_pointer_bool_3d, &
    mpas_allocate_pointer_bool_4d, &
    mpas_allocate_pointer_int_0d, &
    mpas_allocate_pointer_int_1d, &
    mpas_allocate_pointer_int_2d, &
    mpas_allocate_pointer_int_3d, &
    mpas_allocate_pointer_int_4d, &
    mpas_allocate_pointer_long_0d, &
    mpas_allocate_pointer_long_1d, &
    mpas_allocate_pointer_long_2d, &
    mpas_allocate_pointer_long_3d, &
    mpas_allocate_pointer_long_4d, &
    mpas_allocate_pointer_float_0d, &
    mpas_allocate_pointer_float_1d, &
    mpas_allocate_pointer_float_2d, &
    mpas_allocate_pointer_float_3d, &
    mpas_allocate_pointer_float_4d, &
    mpas_allocate_pointer_double_0d, &
    mpas_allocate_pointer_double_1d, &
    mpas_allocate_pointer_double_2d, &
    mpas_allocate_pointer_double_3d, &
    mpas_allocate_pointer_double_4d
END INTERFACE mpas_allocate_pointer

INTERFACE mpas_allocate_allocatable
  MODULE PROCEDURE &
    mpas_allocate_allocatable_logical_0d, &
    mpas_allocate_allocatable_logical_1d, &
    mpas_allocate_allocatable_logical_2d, &
    mpas_allocate_allocatable_logical_3d, &
    mpas_allocate_allocatable_logical_4d, &
    mpas_allocate_allocatable_bool_0d, &
    mpas_allocate_allocatable_bool_1d, &
    mpas_allocate_allocatable_bool_2d, &
    mpas_allocate_allocatable_bool_3d, &
    mpas_allocate_allocatable_bool_4d, &
    mpas_allocate_allocatable_int_0d, &
    mpas_allocate_allocatable_int_1d, &
    mpas_allocate_allocatable_int_2d, &
    mpas_allocate_allocatable_int_3d, &
    mpas_allocate_allocatable_int_4d, &
    mpas_allocate_allocatable_long_0d, &
    mpas_allocate_allocatable_long_1d, &
    mpas_allocate_allocatable_long_2d, &
    mpas_allocate_allocatable_long_3d, &
    mpas_allocate_allocatable_long_4d, &
    mpas_allocate_allocatable_float_0d, &
    mpas_allocate_allocatable_float_1d, &
    mpas_allocate_allocatable_float_2d, &
    mpas_allocate_allocatable_float_3d, &
    mpas_allocate_allocatable_float_4d, &
    mpas_allocate_allocatable_double_0d, &
    mpas_allocate_allocatable_double_1d, &
    mpas_allocate_allocatable_double_2d, &
    mpas_allocate_allocatable_double_3d, &
    mpas_allocate_allocatable_double_4d
END INTERFACE mpas_allocate_allocatable

INTERFACE mpas_allocate_and_read_pointer
  MODULE PROCEDURE &
    mpas_allocate_and_read_pointer_logical_0d, &
    mpas_allocate_and_read_pointer_logical_1d, &
    mpas_allocate_and_read_pointer_logical_2d, &
    mpas_allocate_and_read_pointer_logical_3d, &
    mpas_allocate_and_read_pointer_logical_4d, &
    mpas_allocate_and_read_pointer_bool_0d, &
    mpas_allocate_and_read_pointer_bool_1d, &
    mpas_allocate_and_read_pointer_bool_2d, &
    mpas_allocate_and_read_pointer_bool_3d, &
    mpas_allocate_and_read_pointer_bool_4d, &
    mpas_allocate_and_read_pointer_int_0d, &
    mpas_allocate_and_read_pointer_int_1d, &
    mpas_allocate_and_read_pointer_int_2d, &
    mpas_allocate_and_read_pointer_int_3d, &
    mpas_allocate_and_read_pointer_int_4d, &
    mpas_allocate_and_read_pointer_long_0d, &
    mpas_allocate_and_read_pointer_long_1d, &
    mpas_allocate_and_read_pointer_long_2d, &
    mpas_allocate_and_read_pointer_long_3d, &
    mpas_allocate_and_read_pointer_long_4d, &
    mpas_allocate_and_read_pointer_float_0d, &
    mpas_allocate_and_read_pointer_float_1d, &
    mpas_allocate_and_read_pointer_float_2d, &
    mpas_allocate_and_read_pointer_float_3d, &
    mpas_allocate_and_read_pointer_float_4d, &
    mpas_allocate_and_read_pointer_double_0d, &
    mpas_allocate_and_read_pointer_double_1d, &
    mpas_allocate_and_read_pointer_double_2d, &
    mpas_allocate_and_read_pointer_double_3d, &
    mpas_allocate_and_read_pointer_double_4d
END INTERFACE mpas_allocate_and_read_pointer

INTERFACE mpas_allocate_and_read_allocatable
  MODULE PROCEDURE &
    mpas_allocate_and_read_allocatable_logical_0d, &
    mpas_allocate_and_read_allocatable_logical_1d, &
    mpas_allocate_and_read_allocatable_logical_2d, &
    mpas_allocate_and_read_allocatable_logical_3d, &
    mpas_allocate_and_read_allocatable_logical_4d, &
    mpas_allocate_and_read_allocatable_bool_0d, &
    mpas_allocate_and_read_allocatable_bool_1d, &
    mpas_allocate_and_read_allocatable_bool_2d, &
    mpas_allocate_and_read_allocatable_bool_3d, &
    mpas_allocate_and_read_allocatable_bool_4d, &
    mpas_allocate_and_read_allocatable_int_0d, &
    mpas_allocate_and_read_allocatable_int_1d, &
    mpas_allocate_and_read_allocatable_int_2d, &
    mpas_allocate_and_read_allocatable_int_3d, &
    mpas_allocate_and_read_allocatable_int_4d, &
    mpas_allocate_and_read_allocatable_long_0d, &
    mpas_allocate_and_read_allocatable_long_1d, &
    mpas_allocate_and_read_allocatable_long_2d, &
    mpas_allocate_and_read_allocatable_long_3d, &
    mpas_allocate_and_read_allocatable_long_4d, &
    mpas_allocate_and_read_allocatable_float_0d, &
    mpas_allocate_and_read_allocatable_float_1d, &
    mpas_allocate_and_read_allocatable_float_2d, &
    mpas_allocate_and_read_allocatable_float_3d, &
    mpas_allocate_and_read_allocatable_float_4d, &
    mpas_allocate_and_read_allocatable_double_0d, &
    mpas_allocate_and_read_allocatable_double_1d, &
    mpas_allocate_and_read_allocatable_double_2d, &
    mpas_allocate_and_read_allocatable_double_3d, &
    mpas_allocate_and_read_allocatable_double_4d
END INTERFACE mpas_allocate_and_read_allocatable

LOGICAL :: ignore_bullshit = .TRUE.
INTEGER :: ignore_bullshit_max_dim_size = 999999999
LOGICAL :: ignore_bullshit_allow_negative_indices = .TRUE.
LOGICAL :: ignore_not_existing = .TRUE.

TYPE(t_serializer), POINTER :: serializer => NULL()
TYPE(t_savepoint),  POINTER :: savepoint  => NULL()

CONTAINS

!=============================================================================
!=============================================================================

SUBROUTINE mpas_set_serializer_create(directory, prefix, mode, opt_archive)

  CHARACTER(LEN=*), INTENT(IN)           :: directory, prefix
  CHARACTER, INTENT(IN)                  :: mode
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: opt_archive

  TYPE(t_serializer), POINTER :: new_serializer

  ALLOCATE(new_serializer)
  CALL fs_create_serializer(directory, prefix, mode, new_serializer, opt_archive)
  serializer => new_serializer

END SUBROUTINE mpas_set_serializer_create


SUBROUTINE mpas_set_serializer_existing(new_serializer)

  TYPE(t_serializer), INTENT(IN), TARGET :: new_serializer

  serializer => new_serializer

END SUBROUTINE mpas_set_serializer_existing


SUBROUTINE mpas_destroy_serializer()

  IF (ASSOCIATED(serializer)) THEN
    CALL fs_destroy_serializer(serializer)
    serializer => NULL()
  END IF

END SUBROUTINE mpas_destroy_serializer


FUNCTION mpas_get_serializer()

  TYPE(t_serializer), POINTER :: mpas_get_serializer

  IF (.NOT. ASSOCIATED(serializer)) THEN
    WRITE(*,*) TRIM(module_name)//" - ERROR: No serializer. Call mpas_set_serializer() first!"
    STOP
  ELSE
    mpas_get_serializer => serializer
  END IF

END FUNCTION mpas_get_serializer

SUBROUTINE mpas_print_serializer_debuginfo()
  CALL fs_print_debuginfo(serializer)
END SUBROUTINE mpas_print_serializer_debuginfo

!=============================================================================
!=============================================================================

SUBROUTINE mpas_set_savepoint_create(name)

  CHARACTER(LEN=*), INTENT(IN) :: name

  TYPE(t_savepoint), POINTER :: new_savepoint

  ALLOCATE(new_savepoint)
  CALL fs_create_savepoint(name, new_savepoint)
  savepoint => new_savepoint

END SUBROUTINE mpas_set_savepoint_create


SUBROUTINE mpas_set_savepoint_existing(new_savepoint)

  TYPE(t_savepoint), INTENT(IN), TARGET :: new_savepoint

  savepoint => new_savepoint

END SUBROUTINE mpas_set_savepoint_existing


SUBROUTINE mpas_destroy_savepoint()

  IF (ASSOCIATED(savepoint)) THEN
    CALL fs_destroy_savepoint(savepoint)
    savepoint => NULL()
  END IF

END SUBROUTINE mpas_destroy_savepoint


TYPE(t_savepoint) FUNCTION mpas_get_savepoint()

  IF (.NOT. ASSOCIATED(savepoint)) THEN
    WRITE(*,*) TRIM(module_name)//" - ERROR: No savepoint. Call mpas_set_savepoint() first!"
    STOP
  ELSE
    mpas_get_savepoint = savepoint
  END IF

END FUNCTION mpas_get_savepoint

!=============================================================================
!=============================================================================

LOGICAL FUNCTION mpas_field_exists(fieldname)

  CHARACTER(LEN=*), INTENT(IN) :: fieldname

  mpas_field_exists = fs_field_exists(serializer,  fieldname)

END FUNCTION mpas_field_exists

!=============================================================================
!=============================================================================

FUNCTION mpas_get_size(fieldname)

  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, DIMENSION(4)        :: mpas_get_size

  mpas_get_size = fs_get_size(serializer,  fieldname)

END FUNCTION mpas_get_size

FUNCTION mpas_get_bounds(fieldname)

  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, DIMENSION(8)        :: mpas_get_bounds

  mpas_get_bounds = fs_get_halos(serializer,  fieldname)

END FUNCTION mpas_get_bounds

!=============================================================================
!=============================================================================

SUBROUTINE mpas_add_serializer_metainfo_b(key, val)
  CHARACTER(LEN=*), INTENT(IN) :: key
  LOGICAL, INTENT(IN)          :: val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_add_serializer_metainfo_b

SUBROUTINE mpas_add_serializer_metainfo_i(key, val)
  CHARACTER(LEN=*), INTENT(IN)    :: key
  INTEGER(KIND=C_INT), INTENT(IN) :: val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_add_serializer_metainfo_i

SUBROUTINE mpas_add_serializer_metainfo_l(key, val)
  CHARACTER(LEN=*), INTENT(IN)     :: key
  INTEGER(KIND=C_LONG), INTENT(IN) :: val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_add_serializer_metainfo_l

SUBROUTINE mpas_add_serializer_metainfo_f(key, val)
  CHARACTER(LEN=*), INTENT(IN)   :: key
  REAL(KIND=C_FLOAT), INTENT(IN) :: val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_add_serializer_metainfo_f

SUBROUTINE mpas_add_serializer_metainfo_d(key, val)
  CHARACTER(LEN=*), INTENT(IN)    :: key
  REAL(KIND=C_DOUBLE), INTENT(IN) :: val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_add_serializer_metainfo_d

SUBROUTINE mpas_add_serializer_metainfo_s(key, val)
  CHARACTER(LEN=*), INTENT(IN) :: key
  CHARACTER(LEN=*), INTENT(IN) :: val
  CALL fs_add_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_add_serializer_metainfo_s

!=============================================================================
!=============================================================================

SUBROUTINE mpas_get_serializer_metainfo_b(key, val)
  CHARACTER(LEN=*), INTENT(IN) :: key
  LOGICAL, INTENT(OUT)         :: val
  CALL fs_get_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_get_serializer_metainfo_b

SUBROUTINE mpas_get_serializer_metainfo_i(key, val)
  CHARACTER(LEN=*), INTENT(IN)     :: key
  INTEGER(KIND=C_INT), INTENT(OUT) :: val
  CALL fs_get_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_get_serializer_metainfo_i

SUBROUTINE mpas_get_serializer_metainfo_l(key, val)
  CHARACTER(LEN=*), INTENT(IN)      :: key
  INTEGER(KIND=C_LONG), INTENT(OUT) :: val
  CALL fs_get_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_get_serializer_metainfo_l

SUBROUTINE mpas_get_serializer_metainfo_f(key, val)
  CHARACTER(LEN=*), INTENT(IN)    :: key
  REAL(KIND=C_FLOAT), INTENT(OUT) :: val
  CALL fs_get_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_get_serializer_metainfo_f

SUBROUTINE mpas_get_serializer_metainfo_d(key, val)
  CHARACTER(LEN=*), INTENT(IN)     :: key
  REAL(KIND=C_DOUBLE), INTENT(OUT) :: val
  CALL fs_get_serializer_metainfo(serializer, key, val)
END SUBROUTINE mpas_get_serializer_metainfo_d


!=============================================================================
!=============================================================================

SUBROUTINE mpas_add_field_metainfo_b(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname, key
  LOGICAL, INTENT(IN)          :: val
  CALL fs_add_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_add_field_metainfo_b

SUBROUTINE mpas_add_field_metainfo_i(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname, key
  INTEGER(KIND=C_INT), INTENT(IN) :: val
  CALL fs_add_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_add_field_metainfo_i

SUBROUTINE mpas_add_field_metainfo_l(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname, key
  INTEGER(KIND=C_LONG), INTENT(IN) :: val
  CALL fs_add_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_add_field_metainfo_l

SUBROUTINE mpas_add_field_metainfo_f(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN)   :: fieldname, key
  REAL(KIND=C_FLOAT), INTENT(IN) :: val
  CALL fs_add_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_add_field_metainfo_f

SUBROUTINE mpas_add_field_metainfo_d(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname, key
  REAL(KIND=C_DOUBLE), INTENT(IN) :: val
  CALL fs_add_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_add_field_metainfo_d

SUBROUTINE mpas_add_field_metainfo_s(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname, key
  CHARACTER(LEN=*), INTENT(IN) :: val
  CALL fs_add_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_add_field_metainfo_s

!=============================================================================
!=============================================================================

SUBROUTINE mpas_get_field_metainfo_b(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname, key
  LOGICAL, INTENT(OUT)         :: val
  CALL fs_get_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_get_field_metainfo_b

SUBROUTINE mpas_get_field_metainfo_i(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname, key
  INTEGER(KIND=C_INT), INTENT(OUT) :: val
  CALL fs_get_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_get_field_metainfo_i

SUBROUTINE mpas_get_field_metainfo_l(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname, key
  INTEGER(KIND=C_LONG), INTENT(OUT) :: val
  CALL fs_get_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_get_field_metainfo_l

SUBROUTINE mpas_get_field_metainfo_f(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname, key
  REAL(KIND=C_FLOAT), INTENT(OUT) :: val
  CALL fs_get_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_get_field_metainfo_f

SUBROUTINE mpas_get_field_metainfo_d(fieldname, key, val)
  CHARACTER(LEN=*), INTENT(IN)     :: fieldname, key
  REAL(KIND=C_DOUBLE), INTENT(OUT) :: val
  CALL fs_get_field_metainfo(serializer, fieldname, key, val)
END SUBROUTINE mpas_get_field_metainfo_d

!=============================================================================
!=============================================================================

SUBROUTINE mpas_add_savepoint_metainfo_b(key, val)
  CHARACTER(LEN=*), INTENT(IN) :: key
  LOGICAL, INTENT(IN)          :: val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_add_savepoint_metainfo_b

SUBROUTINE mpas_add_savepoint_metainfo_i(key, val)
  CHARACTER(LEN=*), INTENT(IN)    :: key
  INTEGER(KIND=C_INT), INTENT(IN) :: val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_add_savepoint_metainfo_i

SUBROUTINE mpas_add_savepoint_metainfo_l(key, val)
  CHARACTER(LEN=*), INTENT(IN)     :: key
  INTEGER(KIND=C_LONG), INTENT(IN) :: val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_add_savepoint_metainfo_l

SUBROUTINE mpas_add_savepoint_metainfo_f(key, val)
  CHARACTER(LEN=*), INTENT(IN)   :: key
  REAL(KIND=C_FLOAT), INTENT(IN) :: val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_add_savepoint_metainfo_f

SUBROUTINE mpas_add_savepoint_metainfo_d(key, val)
  CHARACTER(LEN=*), INTENT(IN)    :: key
  REAL(KIND=C_DOUBLE), INTENT(IN) :: val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_add_savepoint_metainfo_d

SUBROUTINE mpas_add_savepoint_metainfo_s(key, val)
  CHARACTER(LEN=*), INTENT(IN) :: key
  CHARACTER(LEN=*), INTENT(IN) :: val
  CALL fs_add_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_add_savepoint_metainfo_s

!=============================================================================
!=============================================================================

SUBROUTINE mpas_get_savepoint_metainfo_b(key, val)
  CHARACTER(LEN=*), INTENT(IN) :: key
  LOGICAL, INTENT(OUT)         :: val
  CALL fs_get_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_get_savepoint_metainfo_b

SUBROUTINE mpas_get_savepoint_metainfo_i(key, val)
  CHARACTER(LEN=*), INTENT(IN)     :: key
  INTEGER(KIND=C_INT), INTENT(OUT) :: val
  CALL fs_get_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_get_savepoint_metainfo_i

SUBROUTINE mpas_get_savepoint_metainfo_l(key, val)
  CHARACTER(LEN=*), INTENT(IN)      :: key
  INTEGER(KIND=C_LONG), INTENT(OUT) :: val
  CALL fs_get_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_get_savepoint_metainfo_l

SUBROUTINE mpas_get_savepoint_metainfo_f(key, val)
  CHARACTER(LEN=*), INTENT(IN)    :: key
  REAL(KIND=C_FLOAT), INTENT(OUT) :: val
  CALL fs_get_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_get_savepoint_metainfo_f

SUBROUTINE mpas_get_savepoint_metainfo_d(key, val)
  CHARACTER(LEN=*), INTENT(IN)     :: key
  REAL(KIND=C_DOUBLE), INTENT(OUT) :: val
  CALL fs_get_savepoint_metainfo(savepoint, key, val)
END SUBROUTINE mpas_get_savepoint_metainfo_d

!=============================================================================
!=============================================================================

SUBROUTINE mpas_register_only(fieldname, typename, lbounds, ubounds, cptr)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: typename
  INTEGER, INTENT(IN), OPTIONAL          :: lbounds(:), ubounds(:)
  TYPE(C_PTR), INTENT(IN), OPTIONAL      :: cptr

  LOGICAL       :: bullshit
  CHARACTER(16) :: loc
  INTEGER       :: sizes(4), bounds(8), i

  sizes  = (/ 1, 0, 0, 0 /)
  bounds = (/ 0, 0, 0, 0, 0, 0, 0, 0 /)

  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = PRESENT(lbounds) .NEQV. PRESENT(ubounds)
    IF (.NOT. bullshit .AND. PRESENT(lbounds)) THEN
      bullshit = SIZE(lbounds) /= SIZE(ubounds)
      IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
        bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
      END IF
      IF (.NOT. bullshit) THEN
        DO i = 1, SIZE(lbounds)
          sizes(i) = ubounds(i) - lbounds(i) + 1
          bounds(i * 2 - 1) = lbounds(i)
          bounds(i * 2) = ubounds(i)
        END DO
        bullshit = ANY(sizes > ignore_bullshit_max_dim_size)
      END IF
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_register_field(serializer, fieldname, 'int', 4, sizes(1), sizes(2), sizes(3), sizes(4), &
                           bounds(1), bounds(2), bounds(3), bounds(4), bounds(5), bounds(6), bounds(7), bounds(8))
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .TRUE.)
    IF (PRESENT(typename)) THEN
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:type', TRIM(typename))
    END IF
    IF (PRESENT(cptr)) THEN
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(mpas_loc_hex(cptr)))
    END IF
  END IF

END SUBROUTINE mpas_register_only

SUBROUTINE mpas_register_only_internal(fieldname, data_type, bytes_per_element, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  CHARACTER(LEN=*), INTENT(IN) :: data_type
  INTEGER, INTENT(IN)          :: bytes_per_element, lbounds(:), ubounds(:)

  INTEGER :: sizes(4), bounds(8), i

  sizes  = (/ 1, 0, 0, 0 /)
  bounds = (/ 0, 0, 0, 0, 0, 0, 0, 0 /)

  DO i = 1, SIZE(lbounds)
    sizes(i) = ubounds(i) - lbounds(i) + 1
    bounds(i * 2 - 1) = lbounds(i)
    bounds(i * 2) = ubounds(i)
  END DO

  CALL fs_register_field(serializer, fieldname, data_type, bytes_per_element, sizes(1), sizes(2), sizes(3), sizes(4), &
                         bounds(1), bounds(2), bounds(3), bounds(4), bounds(5), bounds(6), bounds(7), bounds(8))
  CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .TRUE.)

END SUBROUTINE mpas_register_only_internal

!=============================================================================
!=============================================================================

FUNCTION mpas_loc(field)

  TYPE(C_PTR), INTENT(in)  :: field
  INTEGER(KIND=C_INTPTR_T) :: mpas_loc

  INTERFACE
     SUBROUTINE mpas_loc_(field, loc) &
          BIND(c, name='serialboxFortranLoc')
       USE, INTRINSIC                   :: iso_c_binding
       TYPE(C_PTR), INTENT(IN), VALUE   :: field
       INTEGER(C_INTPTR_T), INTENT(OUT) :: loc
     END SUBROUTINE mpas_loc_
  END INTERFACE

  CALL mpas_loc_(field, mpas_loc)

END FUNCTION mpas_loc

FUNCTION mpas_loc_hex(field)

  TYPE(C_PTR), INTENT(in) :: field
  CHARACTER(16)           :: mpas_loc_hex

  WRITE (mpas_loc_hex,'(Z16)') mpas_loc(field)

END FUNCTION mpas_loc_hex

!=============================================================================
!=============================================================================

SUBROUTINE mpas_write_string(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)         :: fieldname
  CHARACTER(LEN=*), INTENT(IN), TARGET :: field

  CHARACTER(LEN=LEN(field)), POINTER :: padd
  LOGICAL                            :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_string

SUBROUTINE mpas_write_logical_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(IN), TARGET  :: field

  LOGICAL, POINTER :: padd
  LOGICAL          :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_logical_0d

SUBROUTINE mpas_write_logical_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(IN), TARGET  :: field(:)
  INTEGER, INTENT(IN)          :: lbounds(1), ubounds(1)

  LOGICAL, POINTER :: padd(:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'bool', fs_boolsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_logical_1d

SUBROUTINE mpas_write_logical_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(IN), TARGET  :: field(:,:)
  INTEGER, INTENT(IN)          :: lbounds(2), ubounds(2)

  LOGICAL, POINTER :: padd(:,:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'bool', fs_boolsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_logical_2d

SUBROUTINE mpas_write_logical_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(IN), TARGET  :: field(:,:,:)
  INTEGER, INTENT(IN)          :: lbounds(3), ubounds(3)

  LOGICAL, POINTER :: padd(:,:,:)
  LOGICAL          :: bullshit
  CHARACTER(16)    :: loc

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'bool', fs_boolsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_logical_3d

SUBROUTINE mpas_write_logical_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(IN), TARGET  :: field(:,:,:,:)
  INTEGER, INTENT(IN)          :: lbounds(4), ubounds(4)

  LOGICAL, POINTER :: padd(:,:,:,:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'bool', fs_boolsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_logical_4d

SUBROUTINE mpas_write_bool_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field

  LOGICAL(KIND=C_BOOL), POINTER :: padd
  LOGICAL                       :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_bool_0d

SUBROUTINE mpas_write_bool_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN)                      :: lbounds(1), ubounds(1)

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'bool', fs_boolsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_bool_1d

SUBROUTINE mpas_write_bool_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN)                      :: lbounds(2), ubounds(2)

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'bool', fs_boolsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_bool_2d

SUBROUTINE mpas_write_bool_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN)                      :: lbounds(3), ubounds(3)

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:,:)
  LOGICAL                       :: bullshit
  CHARACTER(16)                 :: loc

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'bool', fs_boolsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_bool_3d

SUBROUTINE mpas_write_bool_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN)                      :: lbounds(4), ubounds(4)

  LOGICAL(KIND=C_BOOL), POINTER :: padd(:,:,:,:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'bool', fs_boolsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_bool_4d

SUBROUTINE mpas_write_int_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(IN), TARGET  :: field

  INTEGER, POINTER :: padd
  LOGICAL          :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_int_0d

SUBROUTINE mpas_write_int_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(IN), TARGET  :: field(:)
  INTEGER, INTENT(IN)          :: lbounds(1), ubounds(1)

  INTEGER, POINTER :: padd(:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'int', fs_intsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_int_1d

SUBROUTINE mpas_write_int_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(IN), TARGET  :: field(:,:)
  INTEGER, INTENT(IN)          :: lbounds(2), ubounds(2)

  INTEGER, POINTER :: padd(:,:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'int', fs_intsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_int_2d

SUBROUTINE mpas_write_int_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(IN), TARGET  :: field(:,:,:)
  INTEGER, INTENT(IN)          :: lbounds(3), ubounds(3)

  INTEGER, POINTER :: padd(:,:,:)
  LOGICAL          :: bullshit
  CHARACTER(16)    :: loc

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'int', fs_intsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_int_3d

SUBROUTINE mpas_write_int_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(IN), TARGET  :: field(:,:,:,:)
  INTEGER, INTENT(IN)          :: lbounds(4), ubounds(4)

  INTEGER, POINTER :: padd(:,:,:,:)
  LOGICAL          :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'int', fs_intsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_int_4d

SUBROUTINE mpas_write_long_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field

  INTEGER(KIND=C_LONG), POINTER :: padd
  LOGICAL                       :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_long_0d

SUBROUTINE mpas_write_long_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN)                      :: lbounds(1), ubounds(1)

  INTEGER(KIND=C_LONG), POINTER :: padd(:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'long', fs_longsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_long_1d

SUBROUTINE mpas_write_long_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN)                      :: lbounds(2), ubounds(2)

  INTEGER(KIND=C_LONG), POINTER :: padd(:,:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'long', fs_longsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_long_2d

SUBROUTINE mpas_write_long_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN)                      :: lbounds(3), ubounds(3)

  INTEGER(KIND=C_LONG), POINTER :: padd(:,:,:)
  LOGICAL                       :: bullshit
  CHARACTER(16)                 :: loc

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'long', fs_longsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_long_3d

SUBROUTINE mpas_write_long_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN)                      :: lbounds(4), ubounds(4)

  INTEGER(KIND=C_LONG), POINTER :: padd(:,:,:,:)
  LOGICAL                       :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'long', fs_longsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_long_4d

SUBROUTINE mpas_write_float_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field

  REAL(KIND=C_FLOAT), POINTER :: padd
  LOGICAL                     :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_float_0d

SUBROUTINE mpas_write_float_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN)                    :: lbounds(1), ubounds(1)

  REAL(KIND=C_FLOAT), POINTER :: padd(:)
  LOGICAL                     :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'float', fs_floatsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_float_1d

SUBROUTINE mpas_write_float_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN)                    :: lbounds(2), ubounds(2)

  REAL(KIND=C_FLOAT), POINTER :: padd(:,:)
  LOGICAL                     :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'float', fs_floatsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_float_2d

SUBROUTINE mpas_write_float_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN)                    :: lbounds(3), ubounds(3)

  REAL(KIND=C_FLOAT), POINTER :: padd(:,:,:)
  LOGICAL                     :: bullshit
  CHARACTER(16)               :: loc

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'float', fs_floatsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_float_3d

SUBROUTINE mpas_write_float_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN)                    :: lbounds(4), ubounds(4)

  REAL(KIND=C_FLOAT), POINTER :: padd(:,:,:,:)
  LOGICAL                     :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'float', fs_floatsize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_float_4d

SUBROUTINE mpas_write_double_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field

  REAL(KIND=C_DOUBLE), POINTER :: padd
  LOGICAL                      :: bullshit

  padd => field
  bullshit = .FALSE.
  IF (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd)
  END IF

  IF (.NOT. bullshit) THEN
    CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_double_0d

SUBROUTINE mpas_write_double_1d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:)
  INTEGER, INTENT(IN)                     :: lbounds(1), ubounds(1)

  REAL(KIND=C_DOUBLE), POINTER :: padd(:)
  LOGICAL                      :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. SIZE(field, 1) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = lbounds(1) < 0 .OR. ubounds(1) < 0
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'double', fs_doublesize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_double_1d

SUBROUTINE mpas_write_double_2d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:,:)
  INTEGER, INTENT(IN)                     :: lbounds(2), ubounds(2)

  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:)
  LOGICAL                      :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'double', fs_doublesize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_double_2d

SUBROUTINE mpas_write_double_3d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:,:,:)
  INTEGER, INTENT(IN)                     :: lbounds(3), ubounds(3)

  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:,:)
  LOGICAL                      :: bullshit
  CHARACTER(16)                :: loc

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'double', fs_doublesize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_double_3d

SUBROUTINE mpas_write_double_4d(fieldname, field, lbounds, ubounds)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(IN), TARGET :: field(:,:,:,:)
  INTEGER, INTENT(IN)                     :: lbounds(4), ubounds(4)

  REAL(KIND=C_DOUBLE), POINTER :: padd(:,:,:,:)
  LOGICAL                      :: bullshit

  padd=>field
  bullshit = .FALSE.
  if (ignore_bullshit) THEN
    bullshit = .NOT. ASSOCIATED(padd) .OR. &
               SIZE(field, 1) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 2) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 3) > ignore_bullshit_max_dim_size .OR. &
               SIZE(field, 4) > ignore_bullshit_max_dim_size
    IF (.NOT. bullshit .AND. .NOT. ignore_bullshit_allow_negative_indices) THEN
      bullshit = ANY(lbounds < 0) .OR. ANY(ubounds < 0)
    END IF
  END IF

  IF (.NOT. bullshit) THEN
    IF (SIZE(field) > 0) THEN
      CALL fs_write_field(mpas_get_serializer(), mpas_get_savepoint(), fieldname, field, lbounds, ubounds)
      CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:registered_only', .FALSE.)
    ELSE
      CALL mpas_register_only_internal(fieldname, 'double', fs_doublesize(), lbounds, ubounds)
    END IF
    CALL mpas_add_field_metainfo(TRIM(fieldname), 'ftg:loc', TRIM(ADJUSTL(mpas_loc_hex(C_LOC(field)))))
  END IF

END SUBROUTINE mpas_write_double_4d

!=============================================================================
!=============================================================================

SUBROUTINE mpas_read_string(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)          :: fieldname
  CHARACTER(LEN=*), INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL            :: rperturb
  LOGICAL                               :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_string

SUBROUTINE mpas_read_logical_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL   :: rperturb
  LOGICAL                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_logical_0d

SUBROUTINE mpas_read_logical_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL   :: rperturb
  LOGICAL                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_logical_1d

SUBROUTINE mpas_read_logical_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL   :: rperturb
  LOGICAL                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_logical_2d

SUBROUTINE mpas_read_logical_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL   :: rperturb
  LOGICAL                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_logical_3d

SUBROUTINE mpas_read_logical_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  LOGICAL, INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL   :: rperturb
  LOGICAL                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_logical_4d

SUBROUTINE mpas_read_bool_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL                :: rperturb
  LOGICAL                                   :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_bool_0d

SUBROUTINE mpas_read_bool_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL                :: rperturb
  LOGICAL                                   :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_bool_1d

SUBROUTINE mpas_read_bool_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                :: rperturb
  LOGICAL                                   :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_bool_2d

SUBROUTINE mpas_read_bool_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                :: rperturb
  LOGICAL                                   :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_bool_3d

SUBROUTINE mpas_read_bool_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                :: rperturb
  LOGICAL                                   :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_bool_4d

SUBROUTINE mpas_read_int_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL   :: rperturb
  LOGICAL                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_int_0d

SUBROUTINE mpas_read_int_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL   :: rperturb
  LOGICAL                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_int_1d

SUBROUTINE mpas_read_int_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL   :: rperturb
  LOGICAL                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_int_2d

SUBROUTINE mpas_read_int_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL   :: rperturb
  LOGICAL                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_int_3d

SUBROUTINE mpas_read_int_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN) :: fieldname
  INTEGER, INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL   :: rperturb
  LOGICAL                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_int_4d

SUBROUTINE mpas_read_long_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL                :: rperturb
  LOGICAL                                   :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_long_0d

SUBROUTINE mpas_read_long_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL                :: rperturb
  LOGICAL                                   :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_long_1d

SUBROUTINE mpas_read_long_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                :: rperturb
  LOGICAL                                   :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_long_2d

SUBROUTINE mpas_read_long_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                :: rperturb
  LOGICAL                                   :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_long_3d

SUBROUTINE mpas_read_long_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                :: rperturb
  LOGICAL                                   :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_long_4d

SUBROUTINE mpas_read_float_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL              :: rperturb
  LOGICAL                                 :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_float_0d

SUBROUTINE mpas_read_float_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL              :: rperturb
  LOGICAL                                 :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_float_1d

SUBROUTINE mpas_read_float_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL              :: rperturb
  LOGICAL                                 :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    !CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    !IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    !END IF
  END IF
END SUBROUTINE mpas_read_float_2d

SUBROUTINE mpas_read_float_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL              :: rperturb
  LOGICAL                                 :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    !CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    !IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    !END IF
  END IF
END SUBROUTINE mpas_read_float_3d

SUBROUTINE mpas_read_float_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)            :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL              :: rperturb
  LOGICAL                                 :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_float_4d

SUBROUTINE mpas_read_double_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field
  REAL, INTENT(IN), OPTIONAL               :: rperturb
  LOGICAL                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_double_0d

SUBROUTINE mpas_read_double_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb
  LOGICAL                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_double_1d

SUBROUTINE mpas_read_double_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb
  LOGICAL                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_double_2d

SUBROUTINE mpas_read_double_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb
  LOGICAL                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_double_3d

SUBROUTINE mpas_read_double_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL               :: rperturb
  LOGICAL                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_get_field_metainfo(fieldname, 'ftg:registered_only', registered_only)
    IF (.NOT. registered_only) THEN
      CALL fs_read_field(serializer, savepoint, fieldname, field, rperturb)
    END IF
  END IF
END SUBROUTINE mpas_read_double_4d

!=============================================================================
!=============================================================================

SUBROUTINE mpas_allocate_pointer_logical_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname
  LOGICAL, INTENT(OUT), POINTER :: field
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_pointer_logical_0d

SUBROUTINE mpas_allocate_pointer_logical_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname
  LOGICAL, INTENT(OUT), POINTER :: field(:)
  INTEGER, DIMENSION(8)         :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_pointer_logical_1d

SUBROUTINE mpas_allocate_pointer_logical_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname
  LOGICAL, INTENT(OUT), POINTER :: field(:,:)
  INTEGER, DIMENSION(8)         :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_pointer_logical_2d

SUBROUTINE mpas_allocate_pointer_logical_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname
  LOGICAL, INTENT(OUT), POINTER :: field(:,:,:)
  INTEGER, DIMENSION(8)         :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_pointer_logical_3d

SUBROUTINE mpas_allocate_pointer_logical_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname
  LOGICAL, INTENT(OUT), POINTER :: field(:,:,:,:)
  INTEGER, DIMENSION(8)         :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_pointer_logical_4d

SUBROUTINE mpas_allocate_pointer_bool_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), POINTER :: field
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_pointer_bool_0d

SUBROUTINE mpas_allocate_pointer_bool_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), POINTER :: field(:)
  INTEGER, DIMENSION(8)                      :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_pointer_bool_1d

SUBROUTINE mpas_allocate_pointer_bool_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), POINTER :: field(:,:)
  INTEGER, DIMENSION(8)                      :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_pointer_bool_2d

SUBROUTINE mpas_allocate_pointer_bool_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), POINTER :: field(:,:,:)
  INTEGER, DIMENSION(8)                      :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_pointer_bool_3d

SUBROUTINE mpas_allocate_pointer_bool_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), POINTER :: field(:,:,:,:)
  INTEGER, DIMENSION(8)                      :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_pointer_bool_4d

SUBROUTINE mpas_allocate_pointer_int_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname
  INTEGER, INTENT(OUT), POINTER :: field
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_pointer_int_0d

SUBROUTINE mpas_allocate_pointer_int_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname
  INTEGER, INTENT(OUT), POINTER :: field(:)
  INTEGER, DIMENSION(8)         :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_pointer_int_1d

SUBROUTINE mpas_allocate_pointer_int_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname
  INTEGER, INTENT(OUT), POINTER :: field(:,:)
  INTEGER, DIMENSION(8)         :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_pointer_int_2d

SUBROUTINE mpas_allocate_pointer_int_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname
  INTEGER, INTENT(OUT), POINTER :: field(:,:,:)
  INTEGER, DIMENSION(8)         :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_pointer_int_3d

SUBROUTINE mpas_allocate_pointer_int_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)  :: fieldname
  INTEGER, INTENT(OUT), POINTER :: field(:,:,:,:)
  INTEGER, DIMENSION(8)         :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_pointer_int_4d

SUBROUTINE mpas_allocate_pointer_long_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), POINTER :: field
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_pointer_long_0d

SUBROUTINE mpas_allocate_pointer_long_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), POINTER :: field(:)
  INTEGER, DIMENSION(8)                      :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_pointer_long_1d

SUBROUTINE mpas_allocate_pointer_long_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), POINTER :: field(:,:)
  INTEGER, DIMENSION(8)                      :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_pointer_long_2d

SUBROUTINE mpas_allocate_pointer_long_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), POINTER :: field(:,:,:)
  INTEGER, DIMENSION(8)                      :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_pointer_long_3d

SUBROUTINE mpas_allocate_pointer_long_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), POINTER :: field(:,:,:,:)
  INTEGER, DIMENSION(8)                      :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_pointer_long_4d

SUBROUTINE mpas_allocate_pointer_float_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), POINTER :: field
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_pointer_float_0d

SUBROUTINE mpas_allocate_pointer_float_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), POINTER :: field(:)
  INTEGER, DIMENSION(8)                    :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_pointer_float_1d

SUBROUTINE mpas_allocate_pointer_float_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), POINTER :: field(:,:)
  INTEGER, DIMENSION(8)                    :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_pointer_float_2d

SUBROUTINE mpas_allocate_pointer_float_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), POINTER :: field(:,:,:)
  INTEGER, DIMENSION(8)                    :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_pointer_float_3d

SUBROUTINE mpas_allocate_pointer_float_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)             :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), POINTER :: field(:,:,:,:)
  INTEGER, DIMENSION(8)                    :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_pointer_float_4d

SUBROUTINE mpas_allocate_pointer_double_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), POINTER :: field
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_pointer_double_0d

SUBROUTINE mpas_allocate_pointer_double_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), POINTER :: field(:)
  INTEGER, DIMENSION(8)                     :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_pointer_double_1d

SUBROUTINE mpas_allocate_pointer_double_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), POINTER :: field(:,:)
  INTEGER, DIMENSION(8)                     :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_pointer_double_2d

SUBROUTINE mpas_allocate_pointer_double_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), POINTER :: field(:,:,:)
  INTEGER, DIMENSION(8)                     :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_pointer_double_3d

SUBROUTINE mpas_allocate_pointer_double_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)              :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), POINTER :: field(:,:,:,:)
  INTEGER, DIMENSION(8)                     :: bounds
  
  NULLIFY(field)
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_pointer_double_4d

!=============================================================================
!=============================================================================

SUBROUTINE mpas_allocate_allocatable_logical_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname
  LOGICAL, INTENT(OUT), ALLOCATABLE :: field

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_allocatable_logical_0d

SUBROUTINE mpas_allocate_allocatable_logical_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname
  LOGICAL, INTENT(OUT), ALLOCATABLE :: field(:)
  INTEGER, DIMENSION(8)             :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_logical_1d

SUBROUTINE mpas_allocate_allocatable_logical_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname
  LOGICAL, INTENT(OUT), ALLOCATABLE :: field(:,:)
  INTEGER, DIMENSION(8)             :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_logical_2d

SUBROUTINE mpas_allocate_allocatable_logical_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname
  LOGICAL, INTENT(OUT), ALLOCATABLE :: field(:,:,:)
  INTEGER, DIMENSION(8)             :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_logical_3d

SUBROUTINE mpas_allocate_allocatable_logical_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname
  LOGICAL, INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)
  INTEGER, DIMENSION(8)             :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_logical_4d

SUBROUTINE mpas_allocate_allocatable_bool_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                   :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), ALLOCATABLE :: field

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_allocatable_bool_0d

SUBROUTINE mpas_allocate_allocatable_bool_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                   :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), ALLOCATABLE :: field(:)
  INTEGER, DIMENSION(8)                          :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_bool_1d

SUBROUTINE mpas_allocate_allocatable_bool_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                   :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), ALLOCATABLE :: field(:,:)
  INTEGER, DIMENSION(8)                          :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_bool_2d

SUBROUTINE mpas_allocate_allocatable_bool_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                   :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), ALLOCATABLE :: field(:,:,:)
  INTEGER, DIMENSION(8)                          :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_bool_3d

SUBROUTINE mpas_allocate_allocatable_bool_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                   :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)
  INTEGER, DIMENSION(8)                          :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_bool_4d

SUBROUTINE mpas_allocate_allocatable_int_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname
  INTEGER, INTENT(OUT), ALLOCATABLE :: field

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_allocatable_int_0d

SUBROUTINE mpas_allocate_allocatable_int_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname
  INTEGER, INTENT(OUT), ALLOCATABLE :: field(:)
  INTEGER, DIMENSION(8)             :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_int_1d

SUBROUTINE mpas_allocate_allocatable_int_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname
  INTEGER, INTENT(OUT), ALLOCATABLE :: field(:,:)
  INTEGER, DIMENSION(8)             :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_int_2d

SUBROUTINE mpas_allocate_allocatable_int_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname
  INTEGER, INTENT(OUT), ALLOCATABLE :: field(:,:,:)
  INTEGER, DIMENSION(8)             :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_int_3d

SUBROUTINE mpas_allocate_allocatable_int_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)      :: fieldname
  INTEGER, INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)
  INTEGER, DIMENSION(8)             :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_int_4d

SUBROUTINE mpas_allocate_allocatable_long_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                   :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), ALLOCATABLE :: field

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_allocatable_long_0d

SUBROUTINE mpas_allocate_allocatable_long_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                   :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), ALLOCATABLE :: field(:)
  INTEGER, DIMENSION(8)                          :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_long_1d

SUBROUTINE mpas_allocate_allocatable_long_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                   :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), ALLOCATABLE :: field(:,:)
  INTEGER, DIMENSION(8)                          :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_long_2d

SUBROUTINE mpas_allocate_allocatable_long_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                   :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), ALLOCATABLE :: field(:,:,:)
  INTEGER, DIMENSION(8)                          :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_long_3d

SUBROUTINE mpas_allocate_allocatable_long_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                   :: fieldname
  INTEGER(KIND=C_LONG), INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)
  INTEGER, DIMENSION(8)                          :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_long_4d

SUBROUTINE mpas_allocate_allocatable_float_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), ALLOCATABLE :: field

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_allocatable_float_0d

SUBROUTINE mpas_allocate_allocatable_float_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), ALLOCATABLE :: field(:)
  INTEGER, DIMENSION(8)                        :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_float_1d

SUBROUTINE mpas_allocate_allocatable_float_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), ALLOCATABLE :: field(:,:)
  INTEGER, DIMENSION(4)                        :: mysize

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    mysize = mpas_get_size(fieldname)
    WRITE (*,'(A,I8,A,I8,A,I8,A,I8)') " ALLOCATING ",mysize(1)," ",mysize(2)," ",mysize(3)," ",mysize(4)

    !ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
    ALLOCATE(field(1:mysize(1),1:mysize(2)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_float_2d

SUBROUTINE mpas_allocate_allocatable_float_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), ALLOCATABLE :: field(:,:,:)
  INTEGER, DIMENSION(8)                        :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_float_3d

SUBROUTINE mpas_allocate_allocatable_float_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  REAL(KIND=C_FLOAT), INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)
  INTEGER, DIMENSION(8)                        :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_float_4d

SUBROUTINE mpas_allocate_allocatable_double_0d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                  :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), ALLOCATABLE :: field

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    ALLOCATE(field)
  END IF

END SUBROUTINE mpas_allocate_allocatable_double_0d

SUBROUTINE mpas_allocate_allocatable_double_1d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                  :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), ALLOCATABLE :: field(:)
  INTEGER, DIMENSION(8)                         :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_double_1d

SUBROUTINE mpas_allocate_allocatable_double_2d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                  :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), ALLOCATABLE :: field(:,:)
  INTEGER, DIMENSION(8)                         :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_double_2d

SUBROUTINE mpas_allocate_allocatable_double_3d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                  :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), ALLOCATABLE :: field(:,:,:)
  INTEGER, DIMENSION(8)                         :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_double_3d

SUBROUTINE mpas_allocate_allocatable_double_4d(fieldname, field)
  CHARACTER(LEN=*), INTENT(IN)                  :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(OUT), ALLOCATABLE :: field(:,:,:,:)
  INTEGER, DIMENSION(8)                         :: bounds

  IF (ALLOCATED(field)) THEN
    DEALLOCATE(field)
  END IF
  IF (mpas_field_exists(fieldname)) THEN
    bounds = mpas_get_bounds(fieldname)
    ALLOCATE(field(bounds(1):bounds(2),bounds(3):bounds(4),bounds(5):bounds(6),bounds(7):bounds(8)))
  END IF

END SUBROUTINE mpas_allocate_allocatable_double_4d

!=============================================================================
!=============================================================================

SUBROUTINE mpas_allocate_and_read_pointer_logical_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname
  LOGICAL, INTENT(INOUT), POINTER :: field
  REAL, INTENT(IN), OPTIONAL      :: rperturb
  LOGICAL                         :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_logical_0d

SUBROUTINE mpas_allocate_and_read_pointer_logical_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname
  LOGICAL, INTENT(INOUT), POINTER :: field(:)
  REAL, INTENT(IN), OPTIONAL      :: rperturb
  LOGICAL                         :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_logical_1d

SUBROUTINE mpas_allocate_and_read_pointer_logical_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname
  LOGICAL, INTENT(INOUT), POINTER :: field(:,:)
  REAL, INTENT(IN), OPTIONAL      :: rperturb
  LOGICAL                         :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_logical_2d

SUBROUTINE mpas_allocate_and_read_pointer_logical_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname
  LOGICAL, INTENT(INOUT), POINTER :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL      :: rperturb
  LOGICAL                         :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_logical_3d

SUBROUTINE mpas_allocate_and_read_pointer_logical_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname
  LOGICAL, INTENT(INOUT), POINTER :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL      :: rperturb
  LOGICAL                         :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_logical_4d

SUBROUTINE mpas_allocate_and_read_pointer_bool_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(INOUT), POINTER :: field
  REAL, INTENT(IN), OPTIONAL                   :: rperturb
  LOGICAL                                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_bool_0d

SUBROUTINE mpas_allocate_and_read_pointer_bool_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(INOUT), POINTER :: field(:)
  REAL, INTENT(IN), OPTIONAL                   :: rperturb
  LOGICAL                                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_bool_1d

SUBROUTINE mpas_allocate_and_read_pointer_bool_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(INOUT), POINTER :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                   :: rperturb
  LOGICAL                                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_bool_2d

SUBROUTINE mpas_allocate_and_read_pointer_bool_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(INOUT), POINTER :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                   :: rperturb
  LOGICAL                                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_bool_3d

SUBROUTINE mpas_allocate_and_read_pointer_bool_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(INOUT), POINTER :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                   :: rperturb
  LOGICAL                                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_bool_4d

SUBROUTINE mpas_allocate_and_read_pointer_int_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname
  INTEGER, INTENT(INOUT), POINTER :: field
  REAL, INTENT(IN), OPTIONAL      :: rperturb
  LOGICAL                         :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_int_0d

SUBROUTINE mpas_allocate_and_read_pointer_int_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname
  INTEGER, INTENT(INOUT), POINTER :: field(:)
  REAL, INTENT(IN), OPTIONAL      :: rperturb
  LOGICAL                         :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_int_1d

SUBROUTINE mpas_allocate_and_read_pointer_int_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname
  INTEGER, INTENT(INOUT), POINTER :: field(:,:)
  REAL, INTENT(IN), OPTIONAL      :: rperturb
  LOGICAL                         :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_int_2d

SUBROUTINE mpas_allocate_and_read_pointer_int_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname
  INTEGER, INTENT(INOUT), POINTER :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL      :: rperturb
  LOGICAL                         :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_int_3d

SUBROUTINE mpas_allocate_and_read_pointer_int_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)    :: fieldname
  INTEGER, INTENT(INOUT), POINTER :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL      :: rperturb
  LOGICAL                         :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_int_4d

SUBROUTINE mpas_allocate_and_read_pointer_long_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  INTEGER(KIND=C_LONG), INTENT(INOUT), POINTER :: field
  REAL, INTENT(IN), OPTIONAL                   :: rperturb
  LOGICAL                                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_long_0d

SUBROUTINE mpas_allocate_and_read_pointer_long_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  INTEGER(KIND=C_LONG), INTENT(INOUT), POINTER :: field(:)
  REAL, INTENT(IN), OPTIONAL                   :: rperturb
  LOGICAL                                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_long_1d

SUBROUTINE mpas_allocate_and_read_pointer_long_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  INTEGER(KIND=C_LONG), INTENT(INOUT), POINTER :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                   :: rperturb
  LOGICAL                                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_long_2d

SUBROUTINE mpas_allocate_and_read_pointer_long_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  INTEGER(KIND=C_LONG), INTENT(INOUT), POINTER :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                   :: rperturb
  LOGICAL                                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_long_3d

SUBROUTINE mpas_allocate_and_read_pointer_long_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                 :: fieldname
  INTEGER(KIND=C_LONG), INTENT(INOUT), POINTER :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                   :: rperturb
  LOGICAL                                      :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_long_4d

SUBROUTINE mpas_allocate_and_read_pointer_float_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  REAL(KIND=C_FLOAT), INTENT(INOUT), POINTER :: field
  REAL, INTENT(IN), OPTIONAL                 :: rperturb
  LOGICAL                                    :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_float_0d

SUBROUTINE mpas_allocate_and_read_pointer_float_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  REAL(KIND=C_FLOAT), INTENT(INOUT), POINTER :: field(:)
  REAL, INTENT(IN), OPTIONAL                 :: rperturb
  LOGICAL                                    :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_float_1d

SUBROUTINE mpas_allocate_and_read_pointer_float_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  REAL(KIND=C_FLOAT), INTENT(INOUT), POINTER :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                 :: rperturb
  LOGICAL                                    :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_float_2d

SUBROUTINE mpas_allocate_and_read_pointer_float_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  REAL(KIND=C_FLOAT), INTENT(INOUT), POINTER :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                 :: rperturb
  LOGICAL                                    :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_float_3d

SUBROUTINE mpas_allocate_and_read_pointer_float_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)               :: fieldname
  REAL(KIND=C_FLOAT), INTENT(INOUT), POINTER :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                 :: rperturb
  LOGICAL                                    :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_float_4d

SUBROUTINE mpas_allocate_and_read_pointer_double_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(INOUT), POINTER :: field
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_double_0d

SUBROUTINE mpas_allocate_and_read_pointer_double_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(INOUT), POINTER :: field(:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_double_1d

SUBROUTINE mpas_allocate_and_read_pointer_double_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(INOUT), POINTER :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_double_2d

SUBROUTINE mpas_allocate_and_read_pointer_double_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(INOUT), POINTER :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_double_3d

SUBROUTINE mpas_allocate_and_read_pointer_double_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(INOUT), POINTER :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_pointer(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_pointer_double_4d

!=============================================================================
!=============================================================================

SUBROUTINE mpas_allocate_and_read_allocatable_logical_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL, INTENT(INOUT), ALLOCATABLE, TARGET :: field
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_logical_0d

SUBROUTINE mpas_allocate_and_read_allocatable_logical_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL, INTENT(INOUT), ALLOCATABLE, TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_logical_1d

SUBROUTINE mpas_allocate_and_read_allocatable_logical_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL, INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_logical_2d

SUBROUTINE mpas_allocate_and_read_allocatable_logical_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL, INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_logical_3d

SUBROUTINE mpas_allocate_and_read_allocatable_logical_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  LOGICAL, INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_logical_4d

SUBROUTINE mpas_allocate_and_read_allocatable_bool_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(INOUT), ALLOCATABLE, TARGET :: field
  REAL, INTENT(IN), OPTIONAL                               :: rperturb
  LOGICAL                                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_bool_0d

SUBROUTINE mpas_allocate_and_read_allocatable_bool_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL                               :: rperturb
  LOGICAL                                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_bool_1d

SUBROUTINE mpas_allocate_and_read_allocatable_bool_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                               :: rperturb
  LOGICAL                                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_bool_2d

SUBROUTINE mpas_allocate_and_read_allocatable_bool_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                               :: rperturb
  LOGICAL                                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_bool_3d

SUBROUTINE mpas_allocate_and_read_allocatable_bool_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                             :: fieldname
  LOGICAL(KIND=C_BOOL), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                               :: rperturb
  LOGICAL                                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_bool_4d

SUBROUTINE mpas_allocate_and_read_allocatable_int_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  INTEGER, INTENT(INOUT), ALLOCATABLE, TARGET :: field
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_int_0d

SUBROUTINE mpas_allocate_and_read_allocatable_int_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  INTEGER, INTENT(INOUT), ALLOCATABLE, TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_int_1d

SUBROUTINE mpas_allocate_and_read_allocatable_int_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  INTEGER, INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_int_2d

SUBROUTINE mpas_allocate_and_read_allocatable_int_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  INTEGER, INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_int_3d

SUBROUTINE mpas_allocate_and_read_allocatable_int_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                :: fieldname
  INTEGER, INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                  :: rperturb
  LOGICAL                                     :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_int_4d

SUBROUTINE mpas_allocate_and_read_allocatable_long_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(INOUT), ALLOCATABLE, TARGET :: field
  REAL, INTENT(IN), OPTIONAL                               :: rperturb
  LOGICAL                                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_long_0d

SUBROUTINE mpas_allocate_and_read_allocatable_long_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL                               :: rperturb
  LOGICAL                                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_long_1d

SUBROUTINE mpas_allocate_and_read_allocatable_long_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                               :: rperturb
  LOGICAL                                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_long_2d

SUBROUTINE mpas_allocate_and_read_allocatable_long_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                               :: rperturb
  LOGICAL                                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_long_3d

SUBROUTINE mpas_allocate_and_read_allocatable_long_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                             :: fieldname
  INTEGER(KIND=C_LONG), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                               :: rperturb
  LOGICAL                                                  :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_long_4d

SUBROUTINE mpas_allocate_and_read_allocatable_float_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(INOUT), ALLOCATABLE, TARGET :: field
  REAL, INTENT(IN), OPTIONAL                             :: rperturb
  LOGICAL                                                :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_float_0d

SUBROUTINE mpas_allocate_and_read_allocatable_float_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL                             :: rperturb
  LOGICAL                                                :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_float_1d

SUBROUTINE mpas_allocate_and_read_allocatable_float_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                             :: rperturb
  LOGICAL                                                :: registered_only

  WRITE (*,'(A)') " point D"
  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    WRITE (*,'(A)') " point E"
    CALL mpas_read(fieldname, field, rperturb)
    WRITE (*,'(A)') " point E2"
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_float_2d

SUBROUTINE mpas_allocate_and_read_allocatable_float_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                             :: rperturb
  LOGICAL                                                :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_float_3d

SUBROUTINE mpas_allocate_and_read_allocatable_float_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                           :: fieldname
  REAL(KIND=C_FLOAT), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                             :: rperturb
  LOGICAL                                                :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_float_4d

SUBROUTINE mpas_allocate_and_read_allocatable_double_0d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(INOUT), ALLOCATABLE, TARGET :: field
  REAL, INTENT(IN), OPTIONAL                              :: rperturb
  LOGICAL                                                 :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_double_0d

SUBROUTINE mpas_allocate_and_read_allocatable_double_1d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:)
  REAL, INTENT(IN), OPTIONAL                              :: rperturb
  LOGICAL                                                 :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_double_1d

SUBROUTINE mpas_allocate_and_read_allocatable_double_2d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:)
  REAL, INTENT(IN), OPTIONAL                              :: rperturb
  LOGICAL                                                 :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_double_2d

SUBROUTINE mpas_allocate_and_read_allocatable_double_3d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:)
  REAL, INTENT(IN), OPTIONAL                              :: rperturb
  LOGICAL                                                 :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_double_3d

SUBROUTINE mpas_allocate_and_read_allocatable_double_4d(fieldname, field, rperturb)
  CHARACTER(LEN=*), INTENT(IN)                            :: fieldname
  REAL(KIND=C_DOUBLE), INTENT(INOUT), ALLOCATABLE, TARGET :: field(:,:,:,:)
  REAL, INTENT(IN), OPTIONAL                              :: rperturb
  LOGICAL                                                 :: registered_only

  IF (.NOT. ignore_not_existing .OR. mpas_field_exists(fieldname)) THEN
    CALL mpas_allocate_allocatable(fieldname, field)
    CALL mpas_read(fieldname, field, rperturb)
  END IF
END SUBROUTINE mpas_allocate_and_read_allocatable_double_4d

END MODULE m_ser_mpas

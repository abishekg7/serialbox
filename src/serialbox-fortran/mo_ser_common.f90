! This is a modified version of the original file mo_ser_common.F90
! from the ICON model. The original file is distributed under the
! following license: BSD-3-Clause
!
! ---------------------------------------------------------------
! Copyright (C) 2004-2024, DWD, MPI-M, DKRZ, KIT, ETH, MeteoSwiss
! Contact information: icon-model.org
!
! See AUTHORS.TXT for a list of authors
! See LICENSES/ for license information
! RKINDDX-License-Identifier: BSD-3-Clause
! ---------------------------------------------------------------

! Common serialization routines using Serialbox2

MODULE mo_ser_common

      USE m_serialize,  ONLY: fs_write_field, &
                              fs_read_field
      USE utils_ppser,  ONLY: ppser_savepoint, &
                              ppser_serializer, &
                              ppser_serializer_ref, &
                              ppser_zrperturb
    
      IMPLICIT NONE
    
      PUBLIC :: init, ser_component, t_ser_options, open_compare_file, close_compare_file
    
      PRIVATE
    
      LOGICAL :: linitialize = .TRUE.

      integer, parameter :: RKIND  = selected_real_kind(6)
      integer, parameter :: DKIND  = selected_real_kind(12)
      integer, parameter :: ser_nreport = 10
      REAL(DKIND) :: ser_nfail = 1.0_DKIND
    
      INTEGER :: unit_sum = 0 ! file unit for summary txt
      INTEGER :: unit_long = 0 ! file unit for detailed report
    
      REAL(DKIND) :: eps_r = EPSILON(1._DKIND)
      REAL(RKIND) :: eps_s = EPSILON(1._RKIND)
    
      INTERFACE compare
        MODULE PROCEDURE compare_r_0d
        MODULE PROCEDURE compare_r_1d
        MODULE PROCEDURE compare_r_2d
        MODULE PROCEDURE compare_r_3d
        MODULE PROCEDURE compare_r_4d
        MODULE PROCEDURE compare_s_0d
        MODULE PROCEDURE compare_s_1d
        MODULE PROCEDURE compare_s_2d
        MODULE PROCEDURE compare_s_3d
        MODULE PROCEDURE compare_s_4d
        MODULE PROCEDURE compare_i_0d
        MODULE PROCEDURE compare_i_1d
        MODULE PROCEDURE compare_i_2d
        MODULE PROCEDURE compare_i_3d
        MODULE PROCEDURE compare_i_4d
        MODULE PROCEDURE compare_l_0d
        MODULE PROCEDURE compare_l_1d
        MODULE PROCEDURE compare_l_2d
        MODULE PROCEDURE compare_l_3d
        MODULE PROCEDURE compare_l_4d
      END INTERFACE compare
    
      INTERFACE is_close
        MODULE PROCEDURE is_close_r
        MODULE PROCEDURE is_close_r2
        MODULE PROCEDURE is_close_s2
        MODULE PROCEDURE is_close_s
        MODULE PROCEDURE is_close_i
      END INTERFACE is_close
          
    
      TYPE :: t_ser_options
        INTEGER :: abs_threshold = 12 ! absolute threshold
        INTEGER :: rel_threshold = 12 ! relative threshold
        LOGICAL :: lopenacc = .FALSE. ! Update data on GPU from CPU if .TRUE.,
                                      ! used in read and perturb modes. Usually,
                                      ! lopenacc has to be set .TRUE. if
                                      ! avariable is present on GPU.
        INTEGER :: ser_mode = -1 ! write(0), read(1), read perturbed(2), or compare(3)
        INTEGER :: domain = -1 ! domain index to identify field
      END TYPE t_ser_options
    
      CONTAINS
    
      SUBROUTINE init(suffix)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: suffix
        REAL(KIND=DKIND) :: rprecision
        rprecision = 10.0**(-PRECISION(1.0))
    
        !$ser verbatim    IF (linitialize) THEN
        !$ser init directory='./ser_data' &
        !$ser&     prefix='reference_'//TRIM(suffix) &
        !$ser&     mpi_rank=get_my_mpi_work_id() &
        !$ser&     rprecision=rprecision &
        !$ser&     rperturb=1.0e-14_DKIND
    
        !!$ser init directory='./ser_data' &
        !!$ser&     prefix='current_'//TRIM(suffix) &
        !!$ser&     prefix_ref='reference_'//TRIM(suffix) &
        !!$ser&     mpi_rank=get_my_mpi_work_id() &
        !!$ser&     rprecision=rprecision &
        !!$ser&     rperturb=1.0e-14_DKIND
    
    
        !$ser verbatim     linitialize = .FALSE.
        !$ser verbatim     END IF
    
      END SUBROUTINE init
    
      SUBROUTINE open_compare_file(compare_file_name)
        CHARACTER(len=*) :: compare_file_name
        OPEN( newunit=unit_sum, file=TRIM(compare_file_name)//"_sum.txt", action="WRITE", status="REPLACE")
        OPEN( newunit=unit_long, file=TRIM(compare_file_name)//".txt", action="WRITE", status="REPLACE")
        WRITE(unit_sum, "(A, T40,A7, T50,A7, T60,A7, T70,A7, T80,A7)") "field", "rel", "abs", "%", "nfail", "ntot"
      END SUBROUTINE open_compare_file
    
      SUBROUTINE close_compare_file
        CLOSE( unit=unit_sum )
        CLOSE( unit=unit_long )
      END SUBROUTINE close_compare_file

      SUBROUTINE is_close_r2(ref, cur, atol, rtol, rel_diff, abs_diff, out)
        REAL(DKIND), INTENT(IN) :: ref, cur
        REAL(DKIND), INTENT(IN) :: atol, rtol
        REAL(DKIND), INTENT(OUT) :: rel_diff, abs_diff
        LOGICAL, INTENT(OUT) :: out
    
        REAL(DKIND) :: maxval
        
    
        abs_diff = ABS(cur - ref)
        maxval = MAX(ABS(cur), ABS(ref))
    
        ! compute relative difference for report
        IF(maxval <  eps_r) THEN
          rel_diff = 0
        ELSE
          rel_diff = abs_diff / maxval
        END IF
    
        ! threshold computed as in python's math.isclose
        out = abs_diff <= MAX(rtol*maxval, atol)
    
      END SUBROUTINE is_close_r2
    
      SUBROUTINE is_close_r(ref, cur, abs_threshold, rel_threshold, rel_diff, abs_diff, out)
        REAL(DKIND), INTENT(IN) :: ref, cur
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
        REAL(DKIND), INTENT(OUT) :: rel_diff, abs_diff
        LOGICAL, INTENT(OUT) :: out
    
        REAL(DKIND) :: maxval, at, rt
    
        abs_diff = ABS(cur - ref)
        maxval = MAX(ABS(cur), ABS(ref))
    
        ! compute relative difference for report
        IF(maxval <  eps_r) THEN
          rel_diff = 0
        ELSE
          rel_diff = abs_diff / maxval
        END IF
    
        ! thresholds given as negative exponents
        rt = 10._DKIND**(-rel_threshold)
        at = 10._DKIND**(-abs_threshold)
    
        ! threshold computed as in python's math.isclose
        out = abs_diff <= MAX(rt*maxval, at)
    
      END SUBROUTINE is_close_r
    
      SUBROUTINE is_close_s2(ref, cur, atol, rtol, rel_diff, abs_diff, out)
        REAL(RKIND), INTENT(IN) :: ref, cur
        REAL(RKIND), INTENT(IN) :: atol, rtol
        REAL(RKIND), INTENT(OUT) :: rel_diff, abs_diff
        LOGICAL, INTENT(OUT) :: out
    
        REAL(RKIND) :: maxval
    
        abs_diff = ABS(cur - ref)
        maxval = MAX(ABS(cur), ABS(ref))
    
        ! compute relative difference for report
        IF(maxval < eps_s) THEN
          rel_diff = 0
        ELSE
          rel_diff = abs_diff / maxval
        END IF
    
        ! threshold computed as in python's math.isclose
        out = abs_diff <= MAX(rtol*maxval, atol)
    
      END SUBROUTINE is_close_s2


      SUBROUTINE is_close_s(ref, cur, abs_threshold, rel_threshold, rel_diff, abs_diff, out)
        REAL(RKIND), INTENT(IN) :: ref, cur
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
        REAL(RKIND), INTENT(OUT) :: rel_diff, abs_diff
        LOGICAL, INTENT(OUT) :: out
    
        REAL(RKIND) :: maxval, at, rt
    
        abs_diff = ABS(cur - ref)
        maxval = MAX(ABS(cur), ABS(ref))
    
        ! compute relative difference for report
        IF(maxval < eps_s) THEN
          rel_diff = 0
        ELSE
          rel_diff = abs_diff / maxval
        END IF
    
        ! thresholds given as negative exponents
        rt = 10._RKIND**(-rel_threshold)
        at = 10._RKIND**(-abs_threshold)
    
        ! threshold computed as in python's math.isclose
        out = abs_diff <= MAX(rt*maxval, at)
    
      END SUBROUTINE is_close_s
    
      SUBROUTINE is_close_i(ref, cur, abs_threshold, rel_threshold, rel_diff, abs_diff, out)
        INTEGER, INTENT(IN) :: ref, cur
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
        INTEGER, INTENT(OUT) :: abs_diff
        REAL(RKIND), INTENT(OUT) :: rel_diff
        LOGICAL, INTENT(OUT) :: out
    
        INTEGER  :: maxval
        REAL(RKIND) :: at, rt
    
        abs_diff = ABS(cur - ref)
        maxval = MAX(ABS(cur), ABS(ref))
    
        ! compute relative difference for report
        IF(maxval ==  0) THEN
          rel_diff = 0
        ELSE
          rel_diff = REAL(abs_diff, RKIND) / REAL(maxval, RKIND)
        END IF
    
        ! thresholds given as negative exponents
        rt = 10._RKIND**(-rel_threshold)
        at = 10._RKIND**(-abs_threshold)
    
        ! threshold computed as in python's math.isclose
        out = abs_diff <= MAX(rt*maxval, at)
    
      END SUBROUTINE is_close_i
    
      SUBROUTINE report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, n_tot)
        CHARACTER(len=*), INTENT(IN) :: name
        REAL(DKIND), DIMENSION(:), INTENT(IN) :: report_rel_diff, report_abs_diff, report_cur, report_ref
        CHARACTER(len=*), DIMENSION(:), INTENT(IN) :: report_idx
        INTEGER, INTENT(IN) :: n_fail, n_tot
        REAL(DKIND) :: q
        INTEGER :: z
    
        IF(name(1:8) == '__TEST__') THEN
          IF(name(9:15) == 'error__') THEN
            ! internal test, fail expected (n_fail==n_tot)
            IF(n_fail /= n_tot) THEN
              WRITE(unit_sum, "(A, T40,A,I5,I5)") TRIM(name), " Internal test failed."
            ENDIF
          ELSEIF(name(9:17) == 'noerror__') THEN
            ! internal test, no error expected (n_fail==0)
            IF(n_fail /= 0) THEN
              WRITE(unit_sum, "(A, T40,A)") TRIM(name), " Internal test failed. (E.g. ACC update failed.)"
            ENDIF
          ELSE
            WRITE(unit_sum, "(A, T40,A)") TRIM(name), " Unexpected internal test."
          ENDIF
          RETURN
        ENDIF
    
        q = REAL(n_fail, DKIND) / REAL(n_tot, DKIND) * 100
    
        IF(q > ser_nfail) THEN
          WRITE(unit_sum, "(A, T40,E7.1E2, T50,E7.1E2, T60,F7.3, T70,I7, T80,I7)") TRIM(name), report_rel_diff(1), report_abs_diff(1), q, n_fail, n_tot
          WRITE(unit_long, "(A, A, I7, A, I7, A, F7.3, A)") TRIM(name), ": ", n_fail, " out of ", n_tot, " elements are off (", q, " %)"
          WRITE(unit_long, "(T10,A, T30,A, T50,A, T70,A, T90,A)") "rel diff", "abs diff", "current", "reference", "index"
          DO z=1, MIN(n_fail, SIZE(report_rel_diff, 1))
            WRITE(unit_long, "(T10,E14.8E2, T30,E14.8E2, T50,E14.8E2, T70,E14.8E2, T90,A)") report_rel_diff(z), report_abs_diff(z), report_cur(z), report_ref(z), TRIM(report_idx(z))
          END DO
        ENDIF
      END SUBROUTINE report
    
      SUBROUTINE compare_r_0d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        REAL(DKIND), INTENT(IN), TARGET :: cur
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        REAL(DKIND), TARGET :: ref, cur_cpy, rel_diff, abs_diff
        REAL(DKIND) :: report_rel_diff(1), report_abs_diff(1), report_cur(1), report_ref(1)
        CHARACTER(len=60) :: report_idx(1)
        LOGICAL :: out
        INTEGER :: n_fail
    
        n_fail = 0
        
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        
        call is_close(ref, cur, abs_threshold, rel_threshold, rel_diff, abs_diff, out)
        IF (.NOT. out) THEN
          n_fail = 1
        ENDIF
        
    
        IF (n_fail > 0) THEN
          cur_cpy = cur
          report_idx(1) = "(REAL(DKIND) scalar)"
          report_abs_diff(1) = abs_diff
          report_rel_diff(1) = rel_diff
          report_cur(1) = cur_cpy
          report_ref(1) = ref
        END IF
    
        
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, 1)
    
      END SUBROUTINE compare_r_0d
    
      SUBROUTINE compare_r_1d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        REAL(DKIND), INTENT(IN) :: cur(:)
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        REAL(DKIND), DIMENSION(size(cur, 1)) :: ref, cur_cpy, rel_diff, abs_diff
        LOGICAL :: mask(size(cur, 1))
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(1)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)

        DO i=1,size(cur, 1)
          call is_close(ref(i), cur(i), abs_threshold, rel_threshold, rel_diff(i), abs_diff(i), out)
          IF (.NOT. out) THEN
            n_fail = n_fail + 1
          ENDIF
        END DO
    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN
          mask = .TRUE.
          cur_cpy = cur
          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            WRITE(report_idx(z), "(A,I6,A)") "(", i, ")"
            report_abs_diff(z) = abs_diff(i)
            report_rel_diff(z) = rel_diff(i)
            report_cur(z) = cur_cpy(i)
            report_ref(z) = ref(i)
            mask(i) = .FALSE.
          END DO
        END IF
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_r_1d
    
      SUBROUTINE compare_r_2d(name, cur, atol, rtol)
        CHARACTER(LEN=*), INTENT(IN) :: name
        REAL(DKIND), INTENT(IN) :: cur(:,:)
        REAL(DKIND), INTENT(IN), OPTIONAL :: atol, rtol
    
        REAL(DKIND), DIMENSION(size(cur, 1), size(cur, 2)) :: ref, cur_cpy, rel_diff, abs_diff
        LOGICAL :: mask(size(cur, 1), size(cur, 2))
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(2)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        
        

        DO i=1,size(cur, 1)
          DO j=1,size(cur, 2)
            call is_close(ref(i,j), cur(i,j), atol, rtol, rel_diff(i,j), abs_diff(i,j), out)
            IF (.NOT. out) THEN
              n_fail = n_fail + 1
            ENDIF
          END DO
        END DO
    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN
          
          mask = .TRUE.
          
          cur_cpy = cur
          
          
          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            j = idx(2)
            WRITE(report_idx(z), "(A,I6,A,I6,A)") "(", i, ",", j, ")"
            report_abs_diff(z) = abs_diff(i,j)
            report_rel_diff(z) = rel_diff(i,j)
            report_cur(z) = cur_cpy(i,j)
            report_ref(z) = ref(i,j)
            mask(i,j) = .FALSE.
          END DO
        END IF
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_r_2d
    
      SUBROUTINE compare_r_3d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        REAL(DKIND), INTENT(IN) :: cur(:,:,:)
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        REAL(DKIND), DIMENSION(size(cur, 1), size(cur, 2), size(cur, 3)) :: ref, cur_cpy, rel_diff, abs_diff
        LOGICAL :: mask(size(cur, 1), size(cur, 2), size(cur, 3))
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(3)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, k, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        DO i=1,size(cur, 1)
          DO j=1,size(cur, 2)
            DO k=1,size(cur, 3)
              call is_close(ref(i,j,k), cur(i,j,k), abs_threshold, rel_threshold, rel_diff(i,j,k), abs_diff(i,j,k), out)
              IF (.NOT. out) THEN
                n_fail = n_fail + 1
              ENDIF
            END DO
          END DO
        END DO
    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN

          mask = .TRUE.

          cur_cpy = cur

          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            j = idx(2)
            k = idx(3)
            WRITE(report_idx(z), "(A,I6,A,I6,A,I6,A)") "(", i, ",", j, ",", k, ")"
            report_abs_diff(z) = abs_diff(i,j,k)
            report_rel_diff(z) = rel_diff(i,j,k)
            report_cur(z) = cur_cpy(i,j,k)
            report_ref(z) = ref(i,j,k)
            mask(i,j,k) = .FALSE.
          END DO
        END IF
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_r_3d
    
      SUBROUTINE compare_r_4d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        REAL(DKIND), INTENT(IN) :: cur(:,:,:,:)
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        REAL(DKIND), DIMENSION(size(cur, 1), size(cur, 2), size(cur, 3), size(cur, 4)) :: ref, cur_cpy, rel_diff, abs_diff
        LOGICAL :: mask(size(cur, 1), size(cur, 2), size(cur, 3), size(cur, 4))
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(4)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, k, l, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)

        DO i=1,size(cur, 1)
          DO j=1,size(cur, 2)
            DO k=1,size(cur, 3)
              DO l=1,size(cur, 4)
                call is_close(ref(i,j,k,l), cur(i,j,k,l), abs_threshold, rel_threshold, rel_diff(i,j,k,l), abs_diff(i,j,k,l), out)
                IF (.NOT. out) THEN
                  n_fail = n_fail + 1
                ENDIF
              END DO
            END DO
          END DO
        END DO
    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN
          mask = .TRUE.
          cur_cpy = cur

          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            j = idx(2)
            k = idx(3)
            l = idx(4)
            WRITE(report_idx(z), "(A,I6,A,I6,A,I6,A,I6,A)") "(", i, ",", j, ",", k, ",", l, ")"
            report_abs_diff(z) = abs_diff(i,j,k,l)
            report_rel_diff(z) = rel_diff(i,j,k,l)
            report_cur(z) = cur_cpy(i,j,k,l)
            report_ref(z) = ref(i,j,k,l)
            mask(i,j,k,l) = .FALSE.
          END DO
        END IF
    
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_r_4d
    
      SUBROUTINE compare_s_0d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
    
        REAL(RKIND), INTENT(IN), TARGET :: cur
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        REAL(RKIND), TARGET :: ref, cur_cpy, rel_diff, abs_diff
        REAL(DKIND) :: report_rel_diff(1), report_abs_diff(1), report_cur(1), report_ref(1)
        CHARACTER(len=60) :: report_idx(1)
        LOGICAL :: out
        INTEGER :: n_fail
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        call is_close(ref, cur, abs_threshold, rel_threshold, rel_diff, abs_diff, out)
        IF (.NOT. out) THEN
          n_fail = 1
        ENDIF
    
        IF (n_fail > 0) THEN
          cur_cpy = cur

          report_idx(1) = "(REAL(RKIND) scalar)"
          report_abs_diff(1) = REAL(abs_diff, DKIND)
          report_rel_diff(1) = REAL(rel_diff, DKIND)
          report_cur(1) = cur_cpy
          report_ref(1) = ref
        END IF
    
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, 1)
    
      END SUBROUTINE compare_s_0d
    
      SUBROUTINE compare_s_1d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        REAL(RKIND), INTENT(IN) :: cur(:)
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        REAL(RKIND), DIMENSION(size(cur, 1)) :: ref, cur_cpy, rel_diff, abs_diff
        LOGICAL :: mask(size(cur, 1))
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(1)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        DO i=1,size(cur, 1)
          call is_close(ref(i), cur(i), abs_threshold, rel_threshold, rel_diff(i), abs_diff(i), out)
          IF (.NOT. out) THEN
            n_fail = n_fail + 1
          ENDIF
        END DO
    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN

          mask = .TRUE.

          cur_cpy = cur

          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            WRITE(report_idx(z), "(A,I6,A)") "(", i, ")"
            report_abs_diff(z) = abs_diff(i)
            report_rel_diff(z) = rel_diff(i)
            report_cur(z) = cur_cpy(i)
            report_ref(z) = ref(i)
            mask(i) = .FALSE.
          END DO
        END IF
    
 
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_s_1d
    
      SUBROUTINE compare_s_2d(name, cur, atol, rtol)
        CHARACTER(LEN=*), INTENT(IN) :: name
        REAL(RKIND), INTENT(IN) :: cur(:,:)
        REAL(RKIND), INTENT(IN), OPTIONAL :: atol, rtol
    
        REAL(RKIND), DIMENSION(size(cur, 1), size(cur, 2)) :: ref, cur_cpy, rel_diff, abs_diff
        LOGICAL :: mask(size(cur, 1), size(cur, 2))
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(2)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
       
        DO i=1,size(cur, 1)
          DO j=1,size(cur, 2)
            call is_close(ref(i,j), cur(i,j), atol, rtol, rel_diff(i,j), abs_diff(i,j), out)
            IF (.NOT. out) THEN
              n_fail = n_fail + 1
            ENDIF
          END DO
        END DO
        
        ! compute additional info on CPU
        IF (n_fail > 0) THEN
          
          mask = .TRUE.
          
          cur_cpy = cur
          
          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            j = idx(2)
            WRITE(report_idx(z), "(A,I6,A,I6,A)") "(", i, ",", j, ")"
            report_abs_diff(z) = abs_diff(i,j)
            report_rel_diff(z) = rel_diff(i,j)
            report_cur(z) = cur_cpy(i,j)
            report_ref(z) = ref(i,j)
            mask(i,j) = .FALSE.
          END DO
        END IF
  
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_s_2d
    
      SUBROUTINE compare_s_3d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        REAL(RKIND), INTENT(IN) :: cur(:,:,:)
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        REAL(RKIND), DIMENSION(size(cur, 1), size(cur, 2), size(cur, 3)) :: ref, cur_cpy, rel_diff, abs_diff
        LOGICAL :: mask(size(cur, 1), size(cur, 2), size(cur, 3))
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(3)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, k, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        
        DO i=1,size(cur, 1)
          DO j=1,size(cur, 2)
            DO k=1,size(cur, 3)
              call is_close(ref(i,j,k), cur(i,j,k), abs_threshold, rel_threshold, rel_diff(i,j,k), abs_diff(i,j,k), out)
              IF (.NOT. out) THEN
                n_fail = n_fail + 1
              ENDIF
            END DO
          END DO
        END DO
        
    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN
        
          mask = .TRUE.
        
          cur_cpy = cur
        
          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            j = idx(2)
            k = idx(3)
            WRITE(report_idx(z), "(A,I6,A,I6,A,I6,A)") "(", i, ",", j, ",", k, ")"
            report_abs_diff(z) = abs_diff(i,j,k)
            report_rel_diff(z) = rel_diff(i,j,k)
            report_cur(z) = cur_cpy(i,j,k)
            report_ref(z) = ref(i,j,k)
            mask(i,j,k) = .FALSE.
          END DO
        END IF
    
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_s_3d
    
      SUBROUTINE compare_s_4d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        REAL(RKIND), INTENT(IN) :: cur(:,:,:,:)
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        REAL(RKIND), DIMENSION(size(cur, 1), size(cur, 2), size(cur, 3), size(cur, 4)) :: ref, cur_cpy, rel_diff, abs_diff
        LOGICAL :: mask(size(cur, 1), size(cur, 2), size(cur, 3), size(cur, 4))
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(4)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, k, l, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)

        DO i=1,size(cur, 1)
          DO j=1,size(cur, 2)
            DO k=1,size(cur, 3)
              DO l=1,size(cur, 4)
                call is_close(ref(i,j,k,l), cur(i,j,k,l), abs_threshold, rel_threshold, rel_diff(i,j,k,l), abs_diff(i,j,k,l), out)
                IF (.NOT. out) THEN
                  n_fail = n_fail + 1
                ENDIF
              END DO
            END DO
          END DO
        END DO
    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN
          mask = .TRUE.
          cur_cpy = cur

          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            j = idx(2)
            k = idx(3)
            l = idx(4)
            WRITE(report_idx(z), "(A,I6,A,I6,A,I6,A,I6,A)") "(", i, ",", j, ",", k, ",", l, ")"
            report_abs_diff(z) = abs_diff(i,j,k,l)
            report_rel_diff(z) = rel_diff(i,j,k,l)
            report_cur(z) = cur_cpy(i,j,k,l)
            report_ref(z) = ref(i,j,k,l)
            mask(i,j,k,l) = .FALSE.
          END DO
        END IF
    
  
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_s_4d
    
      SUBROUTINE compare_i_0d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        INTEGER, INTENT(IN), TARGET :: cur
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        INTEGER, TARGET :: ref, cur_cpy, abs_diff
        REAL(RKIND), TARGET :: rel_diff
        REAL(DKIND) :: report_rel_diff(1)
        INTEGER :: report_abs_diff(1), report_cur(1), report_ref(1)
        CHARACTER(len=60) :: report_idx(1)
        LOGICAL :: out
        INTEGER :: n_fail
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)

        call is_close(ref, cur, abs_threshold, rel_threshold, rel_diff, abs_diff, out)
        IF (.NOT. out) THEN
          n_fail = 1
        ENDIF
    
        IF (n_fail > 0) THEN

          cur_cpy = cur

          report_idx(1) = "(INTEGER scalar)"
          report_abs_diff(1) = abs_diff
          report_rel_diff(1) = rel_diff
          report_cur(1) = cur_cpy
          report_ref(1) = ref
        END IF
    
    
        call report(name, report_rel_diff, REAL(report_abs_diff, DKIND), REAL(report_cur, DKIND), REAL(report_ref, DKIND), report_idx, n_fail, 1)
    
      END SUBROUTINE compare_i_0d
    
      SUBROUTINE compare_i_1d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        INTEGER, INTENT(IN) :: cur(:)
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        INTEGER, DIMENSION(size(cur, 1)) :: ref, cur_cpy, abs_diff
        REAL(RKIND), DIMENSION(size(cur, 1)) :: rel_diff
        LOGICAL :: mask(size(cur, 1))
        INTEGER :: report_cur(ser_nreport), report_ref(ser_nreport), report_abs_diff(ser_nreport)
        REAL(DKIND) :: report_rel_diff(ser_nreport)
        INTEGER :: idx(1)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)

        DO i=1,size(cur, 1)
          call is_close(ref(i), cur(i), abs_threshold, rel_threshold, rel_diff(i), abs_diff(i), out)
          IF (.NOT. out) THEN
            n_fail = n_fail + 1
          ENDIF
        END DO
    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN
          mask = .TRUE.

          cur_cpy = cur

          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            WRITE(report_idx(z), "(A,I6,A)") "(", i, ")"
            report_abs_diff(z) = abs_diff(i)
            report_rel_diff(z) = rel_diff(i)
            report_cur(z) = cur_cpy(i)
            report_ref(z) = ref(i)
            mask(i) = .FALSE.
          END DO
        END IF
    
    
        call report(name, report_rel_diff, REAL(report_abs_diff, DKIND), REAL(report_cur, DKIND), REAL(report_ref, DKIND), report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_i_1d
    
      SUBROUTINE compare_i_2d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        INTEGER, INTENT(IN) :: cur(:,:)
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        INTEGER, DIMENSION(size(cur, 1), size(cur, 2)) :: ref, cur_cpy, abs_diff
        REAL(RKIND), DIMENSION(size(cur, 1), size(cur, 2)) :: rel_diff
        LOGICAL :: mask(size(cur, 1), size(cur, 2))
        INTEGER :: report_cur(ser_nreport), report_ref(ser_nreport), report_abs_diff(ser_nreport)
        REAL(DKIND) :: report_rel_diff(ser_nreport)
        INTEGER :: idx(2)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)

        DO i=1,size(cur, 1)
          DO j=1,size(cur, 2)
            call is_close(ref(i,j), cur(i,j), abs_threshold, rel_threshold, rel_diff(i,j), abs_diff(i,j), out)
            IF (.NOT. out) THEN
              n_fail = n_fail + 1
            ENDIF
          END DO
        END DO

    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN

          mask = .TRUE.

          cur_cpy = cur

          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            j = idx(2)
            WRITE(report_idx(z), "(A,I6,A,I6,A)") "(", i, ",", j, ")"
            report_abs_diff(z) = abs_diff(i,j)
            report_rel_diff(z) = rel_diff(i,j)
            report_cur(z) = cur_cpy(i,j)
            report_ref(z) = ref(i,j)
            mask(i,j) = .FALSE.
          END DO
        END IF
        
        call report(name, report_rel_diff, REAL(report_abs_diff, DKIND), REAL(report_cur, DKIND), REAL(report_ref, DKIND), report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_i_2d
    
      SUBROUTINE compare_i_3d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        INTEGER, INTENT(IN) :: cur(:,:,:)
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        INTEGER, DIMENSION(size(cur, 1), size(cur, 2), size(cur, 3)) :: ref, cur_cpy, abs_diff
        REAL(RKIND), DIMENSION(size(cur, 1), size(cur, 2), size(cur, 3)) :: rel_diff
        LOGICAL :: mask(size(cur, 1), size(cur, 2), size(cur, 3))
        INTEGER :: report_cur(ser_nreport), report_ref(ser_nreport), report_abs_diff(ser_nreport)
        REAL(DKIND) :: report_rel_diff(ser_nreport)
        INTEGER :: idx(3)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, k, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        DO i=1,size(cur, 1)
          DO j=1,size(cur, 2)
            DO k=1,size(cur, 3)
              call is_close(ref(i,j,k), cur(i,j,k), abs_threshold, rel_threshold, rel_diff(i,j,k), abs_diff(i,j,k), out)
              IF (.NOT. out) THEN
                n_fail = n_fail + 1
              ENDIF
            END DO
          END DO
        END DO
    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN

          mask = .TRUE.
          cur_cpy = cur
          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            j = idx(2)
            k = idx(3)
            WRITE(report_idx(z), "(A,I6,A,I6,A,I6,A)") "(", i, ",", j, ",", k, ")"
            report_abs_diff(z) = abs_diff(i,j,k)
            report_rel_diff(z) = rel_diff(i,j,k)
            report_cur(z) = cur_cpy(i,j,k)
            report_ref(z) = ref(i,j,k)
            mask(i,j,k) = .FALSE.
          END DO
        END IF
    
        call report(name, report_rel_diff, REAL(report_abs_diff, DKIND), REAL(report_cur, DKIND), REAL(report_ref, DKIND), report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_i_3d
    
      SUBROUTINE compare_i_4d(name, cur, lopenacc, abs_threshold, rel_threshold)
        CHARACTER(LEN=*), INTENT(IN) :: name
        INTEGER, INTENT(IN) :: cur(:,:,:,:)
        LOGICAL, INTENT(IN) :: lopenacc
        INTEGER, INTENT(IN) :: abs_threshold, rel_threshold
    
        INTEGER, DIMENSION(size(cur, 1), size(cur, 2), size(cur, 3), size(cur, 4)) :: ref, cur_cpy, abs_diff
        REAL(RKIND), DIMENSION(size(cur, 1), size(cur, 2), size(cur, 3), size(cur, 4)) :: rel_diff
        LOGICAL :: mask(size(cur, 1), size(cur, 2), size(cur, 3), size(cur, 4))
        INTEGER :: report_cur(ser_nreport), report_ref(ser_nreport), report_abs_diff(ser_nreport)
        REAL(DKIND) :: report_rel_diff(ser_nreport)
        INTEGER :: idx(4)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, k, l, z
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        DO i=1,size(cur, 1)
          DO j=1,size(cur, 2)
            DO k=1,size(cur, 3)
              DO l=1,size(cur, 4)
                call is_close(ref(i,j,k,l), cur(i,j,k,l), abs_threshold, rel_threshold, rel_diff(i,j,k,l), abs_diff(i,j,k,l), out)
                IF (.NOT. out) THEN
                  n_fail = n_fail + 1
                ENDIF
              END DO
            END DO
          END DO
        END DO
    
        ! compute additional info on CPU
        IF (n_fail > 0) THEN
          mask = .TRUE.
          cur_cpy = cur
          DO z=1,ser_nreport
            idx(:) = MAXLOC(rel_diff, mask)
            i = idx(1)
            j = idx(2)
            k = idx(3)
            l = idx(4)
            WRITE(report_idx(z), "(A,I6,A,I6,A,I6,A,I6,A)") "(", i, ",", j, ",", k, ",", l, ")"
            report_abs_diff(z) = abs_diff(i,j,k,l)
            report_rel_diff(z) = rel_diff(i,j,k,l)
            report_cur(z) = cur_cpy(i,j,k,l)
            report_ref(z) = ref(i,j,k,l)
            mask(i,j,k,l) = .FALSE.
          END DO
        END IF
    
        call report(name, report_rel_diff, REAL(report_abs_diff, DKIND), REAL(report_cur, DKIND), REAL(report_ref, DKIND), report_idx, n_fail, size(cur))
    
      END SUBROUTINE compare_i_4d
    
      SUBROUTINE compare_l_0d(name, cur, lopenacc)
        CHARACTER(LEN=*), INTENT(IN) :: name
        LOGICAL, INTENT(IN), TARGET :: cur
        LOGICAL, INTENT(IN) :: lopenacc
    
        LOGICAL, TARGET :: ref, cur_cpy
        REAL(DKIND) :: report_rel_diff(1), report_abs_diff(1), report_cur(1), report_ref(1)
        CHARACTER(len=60) :: report_idx(1)
        LOGICAL :: out
        INTEGER :: n_fail
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        IF (ref .NEQV. cur) THEN
          n_fail = 1
        ENDIF
    
        IF (n_fail > 0) THEN
          cur_cpy = cur
          report_idx(1) = "(LOGICAL scalar)"
          report_abs_diff(1) = 1._DKIND
          report_rel_diff(1) = 1._DKIND
          report_cur(1) = MERGE(1._DKIND, 0._DKIND, cur_cpy)
          report_ref(1) = MERGE(1._DKIND, 0._DKIND, ref)
        END IF
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, 1)
    
      END SUBROUTINE compare_l_0d
    
      SUBROUTINE compare_l_1d(name, cur, lopenacc)
        CHARACTER(LEN=*), INTENT(IN) :: name
        LOGICAL, INTENT(IN) :: cur(:)
        LOGICAL, INTENT(IN) :: lopenacc
    
        LOGICAL, DIMENSION(size(cur, 1)), TARGET :: ref, cur_cpy
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(1)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        cur_cpy = cur
    
        DO i=1,size(cur, 1)
          IF (ref(i) .NEQV. cur_cpy(i)) THEN
            n_fail = n_fail + 1
            IF (n_fail <= ser_nreport) THEN
              WRITE(report_idx(n_fail), "(A,I6,A)") "(", i, ")"
              report_abs_diff(n_fail) = 1._DKIND
              report_rel_diff(n_fail) = 1._DKIND
              report_cur(n_fail) = MERGE(1._DKIND, 0._DKIND, cur_cpy(i))
              report_ref(n_fail) = MERGE(1._DKIND, 0._DKIND, ref(i))
            ENDIF
          ENDIF
        END DO
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
      END SUBROUTINE compare_l_1d
    
      SUBROUTINE compare_l_2d(name, cur, lopenacc)
        CHARACTER(LEN=*), INTENT(IN) :: name
        LOGICAL, INTENT(IN) :: cur(:,:)
        LOGICAL, INTENT(IN) :: lopenacc
    
        LOGICAL, DIMENSION(size(cur, 1), size(cur, 2)), TARGET :: ref, cur_cpy
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(2)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        cur_cpy = cur
    
        DO j=1,size(cur, 2)
          DO i=1,size(cur, 1)
            IF (ref(i,j) .NEQV. cur_cpy(i,j)) THEN
              n_fail = n_fail + 1
              IF (n_fail <= ser_nreport) THEN
                WRITE(report_idx(n_fail), "(A,I6,',',I6,A)") "(", i, j, ")"
                report_abs_diff(n_fail) = 1._DKIND
                report_rel_diff(n_fail) = 1._DKIND
                report_cur(n_fail) = MERGE(1._DKIND, 0._DKIND, cur_cpy(i,j))
                report_ref(n_fail) = MERGE(1._DKIND, 0._DKIND, ref(i,j))
              ENDIF
            ENDIF
          END DO
        END DO
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
      END SUBROUTINE compare_l_2d
    
      SUBROUTINE compare_l_3d(name, cur, lopenacc)
        CHARACTER(LEN=*), INTENT(IN) :: name
        LOGICAL, INTENT(IN) :: cur(:,:,:)
        LOGICAL, INTENT(IN) :: lopenacc
    
        LOGICAL, DIMENSION(size(cur, 1), size(cur, 2), size(cur, 3)), TARGET :: ref, cur_cpy
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(3)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, k
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        cur_cpy = cur
    
        DO k=1,size(cur, 3)
          DO j=1,size(cur, 2)
            DO i=1,size(cur, 1)
              IF (ref(i,j,k) .NEQV. cur_cpy(i,j,k)) THEN
                n_fail = n_fail + 1
                IF (n_fail <= ser_nreport) THEN
                  WRITE(report_idx(n_fail), "(A,I6,',',I6,',',I6,A)") "(", i, j, k, ")"
                  report_abs_diff(n_fail) = 1._DKIND
                  report_rel_diff(n_fail) = 1._DKIND
                  report_cur(n_fail) = MERGE(1._DKIND, 0._DKIND, cur_cpy(i,j,k))
                  report_ref(n_fail) = MERGE(1._DKIND, 0._DKIND, ref(i,j,k))
                ENDIF
              ENDIF
            END DO
          END DO
        END DO
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
      END SUBROUTINE compare_l_3d
    
      SUBROUTINE compare_l_4d(name, cur, lopenacc)
        CHARACTER(LEN=*), INTENT(IN) :: name
        LOGICAL, INTENT(IN) :: cur(:,:,:,:)
        LOGICAL, INTENT(IN) :: lopenacc
    
        LOGICAL, DIMENSION(size(cur, 1), size(cur, 2), size(cur, 3), size(cur, 4)), TARGET :: ref, cur_cpy
        REAL(DKIND) :: report_rel_diff(ser_nreport), report_abs_diff(ser_nreport), report_cur(ser_nreport), report_ref(ser_nreport)
        INTEGER :: idx(4)
        CHARACTER(len=60) :: report_idx(ser_nreport)
        LOGICAL :: out
        INTEGER :: n_fail
        INTEGER :: i, j, k, l
    
        n_fail = 0
    
        call fs_read_field(ppser_serializer_ref, ppser_savepoint, TRIM(name), ref)
        cur_cpy = cur
    
        DO l=1,size(cur, 4)
          DO k=1,size(cur, 3)
            DO j=1,size(cur, 2)
              DO i=1,size(cur, 1)
                IF (ref(i,j,k,l) .NEQV. cur_cpy(i,j,k,l)) THEN
                  n_fail = n_fail + 1
                  IF (n_fail <= ser_nreport) THEN
                    WRITE(report_idx(n_fail), "(A,I6,',',I6,',',I6,',',I6,A)") "(", i, j, k, l, ")"
                    report_abs_diff(n_fail) = 1._DKIND
                    report_rel_diff(n_fail) = 1._DKIND
                    report_cur(n_fail) = MERGE(1._DKIND, 0._DKIND, cur_cpy(i,j,k,l))
                    report_ref(n_fail) = MERGE(1._DKIND, 0._DKIND, ref(i,j,k,l))
                  ENDIF
                ENDIF
              END DO
            END DO
          END DO
        END DO
    
        call report(name, report_rel_diff, report_abs_diff, report_cur, report_ref, report_idx, n_fail, size(cur))
      END SUBROUTINE compare_l_4d
    
    END MODULE mo_ser_common

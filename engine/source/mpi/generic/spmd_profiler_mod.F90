!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    spmd_profiler_mod           spmd_profiler_mod.F90
!||
!||    Fortran interface to the C++ SPMD profiler (spmd_profiler.cpp).
!||
!||    Profiling is controlled at runtime via spmd_profiling_enabled.
!||    Call spmd_profiler_init(rank) to enable, or set
!||    spmd_profiling_enabled = .true. directly.
!||    All subroutines are no-ops when profiling is disabled (~1 ns overhead).
!||
!||    THREADING: the profiler is NOT thread-safe (unsynchronized global
!||    state in spmd_profiler.cpp).  spmd_profile_begin/spmd_profile_end —
!||    and any SPMD wrapper call while profiling is enabled — must be made
!||    outside of OpenMP parallel regions, or by a single task only.
!||
!||    Typical use (before MPI_Finalize):
!||
!||      use spmd_mod
!||      call spmd_profiler_init(rank)  ! enables profiling
!||      ...
!||      call spmd_profiler_flush()     ! writes spmd_timeline_rank_NNNNN.spmd
!||      call MPI_Finalize(ierr)
!||====================================================================
      module spmd_profiler_mod
        use, intrinsic :: iso_c_binding
        implicit none




        !> Runtime profiling flag — set to .true. to enable profiling
        logical, public, save :: spmd_profiling_enabled = .false.

        ! Profiling section tags for use with spmd_profile_begin/end
        integer, parameter, public :: PROF_CONTSORT = -3002
        integer, parameter, public :: PROF_ELEMENT  = -3003
        integer, parameter, public :: PROF_KIN      = -3004
        integer, parameter, public :: PROF_INTEG    = -3005
        integer, parameter, public :: PROF_P0       = -3006
        integer, parameter, public :: PROF_IO       = -3007
        integer, parameter, public :: PROF_CONTFOR  = -3008
        integer, parameter, public :: PROF_ASM      = -3009
        integer, parameter, public :: PROF_EXFOR    = -3010
        integer, parameter, public :: PROF_CONTACT  = -3011
        integer, parameter, public :: PROF_OUTPUT   = -3012
        integer, parameter, public :: PROF_RESTART  = -3013
        integer, parameter, public :: PROF_CONTPOST = -3014
        integer, parameter, public :: PROF_RBODY    = -3015
        integer, parameter, public :: PROF_DTSTEP   = -3016
        integer, parameter, public :: PROF_ACCEL    = -3017
        integer, parameter, public :: PROF_INIT     = -3018
        integer, parameter, public :: PROF_ALEFOR   = -3019
        integer, parameter, public :: PROF_PREELEM  = -3020
        integer, parameter, public :: PROF_SPH_NLOC = -3021
        integer, parameter, public :: PROF_VELUPD   = -3022
        integer, parameter, public :: PROF_POSTOUT  = -3023
        integer, parameter, public :: PROF_ACC      = -3024
        integer, parameter, public :: PROF_ALE      = -3025
        integer, parameter, public :: PROF_ANIM     = -3026
        integer, parameter, public :: PROF_BCS      = -3027
        integer, parameter, public :: PROF_KINCOND  = -3028
        integer, parameter, public :: PROF_RBY      = -3029
        integer, parameter, public :: PROF_SENSOR   = -3030
        integer, parameter, public :: PROF_SPH      = -3031

        !> C++ back-end — declared private so callers use the Fortran wrappers.
        private :: spmd_profiler_init_c, spmd_profiler_flush_c
        private :: spmd_profiler_section_begin_c, spmd_profiler_section_end_c

        interface
          subroutine spmd_profiler_init_c(rank) &
            bind(c, name="spmd_profiler_init")
            import :: c_int
            integer(c_int), intent(in) :: rank
          end subroutine spmd_profiler_init_c

          subroutine spmd_profiler_flush_c() &
            bind(c, name="spmd_profiler_flush")
          end subroutine spmd_profiler_flush_c

          subroutine spmd_profiler_section_begin_c(tag, name, name_len) &
            bind(c, name="spmd_profiler_section_begin")
            import :: c_int, c_char
            integer(c_int), intent(in) :: tag
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), intent(in) :: name_len
          end subroutine spmd_profiler_section_begin_c

          subroutine spmd_profiler_section_end_c(tag) &
            bind(c, name="spmd_profiler_section_end")
            import :: c_int
            integer(c_int), intent(in) :: tag
          end subroutine spmd_profiler_section_end_c
        end interface

      contains

! ======================================================================================================================
!! \brief Initialise the profiler, set the rank, and enable profiling.
        subroutine spmd_profiler_init(rank)
          implicit none
          integer, intent(in) :: rank
          integer(c_int) :: rank_c

          rank_c = int(rank, c_int)
          call spmd_profiler_init_c(rank_c)
          spmd_profiling_enabled = .true.
        end subroutine spmd_profiler_init

! ======================================================================================================================
!! \brief Write the collected timeline and clear the in-memory buffer.
!!        Must be called BEFORE MPI_Finalize.
        subroutine spmd_profiler_flush()
          implicit none

          if (.not. spmd_profiling_enabled) return
          call spmd_profiler_flush_c()
        end subroutine spmd_profiler_flush

! ======================================================================================================================
!! \brief Begin a named user section for profiling.
!! \details If a section is already active, it is auto-closed first.
!!          User sections are suspended by MPI calls (spmd_in/spmd_out) and
!!          automatically resumed after the MPI call completes.
!!          Use tags <= -3000 to avoid collision with MPI tags.
!!          Must be called outside of OpenMP parallel regions, or by a
!!          single task only (the profiler is not thread-safe).
!CONTSORT      -3002
!ELEMENT       -3003
!KIN           -3004
!INTEG         -3005
!P0            -3006
!IO            -3007
!CONTFOR       -3008
!ASM           -3009
!EXFOR         -3010

        subroutine spmd_profile_begin(tag, name)
          implicit none
          integer, intent(in) :: tag
          character(len=*), intent(in), optional :: name
          integer(c_int) :: tag_c, name_len_c
          character(kind=c_char), dimension(65) :: name_c
          character(len=32) :: local_name
          integer :: i, n

          if (.not. spmd_profiling_enabled) return
          tag_c = int(tag, c_int)

          if (present(name)) then
            local_name = name
          else
            select case (tag)
             case (PROF_CONTSORT); local_name = "CONTSORT"
             case (PROF_ELEMENT);  local_name = "ELEMENT"
             case (PROF_KIN);      local_name = "KIN"
             case (PROF_INTEG);    local_name = "INTEG"
             case (PROF_P0);       local_name = "P0"
             case (PROF_IO);       local_name = "IO"
             case (PROF_CONTFOR);  local_name = "CONTFOR"
             case (PROF_ASM);      local_name = "ASM"
             case (PROF_EXFOR);    local_name = "EXFOR"
             case (PROF_CONTACT);  local_name = "CONTACT"
             case (PROF_OUTPUT);   local_name = "OUTPUT"
             case (PROF_RESTART);  local_name = "RESTART"
             case (PROF_CONTPOST); local_name = "CONTPOST"
             case (PROF_RBODY);    local_name = "RBODY"
             case (PROF_DTSTEP);   local_name = "DTSTEP"
             case (PROF_ACCEL);    local_name = "ACCEL"
             case (PROF_INIT);     local_name = "INIT"
             case (PROF_ALEFOR);   local_name = "ALEFOR"
             case (PROF_PREELEM);  local_name = "PREELEM"
             case (PROF_SPH_NLOC); local_name = "SPH_NLOC"
             case (PROF_VELUPD);   local_name = "VELUPD"
             case (PROF_POSTOUT);  local_name = "POSTOUT"
             case (PROF_ACC);      local_name = "ACC"
             case (PROF_ALE);      local_name = "ALE"
             case (PROF_ANIM);     local_name = "ANIM"
             case (PROF_BCS);      local_name = "BCS"
             case (PROF_KINCOND);  local_name = "KINCOND"
             case (PROF_RBY);      local_name = "RBY"
             case (PROF_SENSOR);   local_name = "SENSOR"
             case (PROF_SPH);      local_name = "SPH"
             case default; local_name = " "
            end select
          end if

          n = min(len_trim(local_name), 64)
          if (n > 0) then
            do i = 1, n
              name_c(i) = local_name(i:i)
            end do
            name_c(n+1) = c_null_char
            name_len_c = int(n, c_int)
          else
            name_c(1) = c_null_char
            name_len_c = 0_c_int
          end if
          call spmd_profiler_section_begin_c(tag_c, name_c, name_len_c)
        end subroutine spmd_profile_begin

! ======================================================================================================================
!! \brief End the active user section.
!! \details Emits the final segment. No-op if no section is active.
        subroutine spmd_profile_end(tag)
          implicit none
          integer, intent(in) :: tag
          integer(c_int) :: tag_c

          if (.not. spmd_profiling_enabled) return
          tag_c = int(tag, c_int)
          call spmd_profiler_section_end_c(tag_c)
        end subroutine spmd_profile_end

      end module spmd_profiler_mod

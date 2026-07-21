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
!||    Usage
!||    -----
!||    Compile with  -DSPMD_PROFILE  to activate profiling.
!||    Without the flag, all subroutines are compiled as no-ops so that
!||    application code does not need any #ifdef guards at call sites.
!||
!||    Typical use (before MPI_Finalize):
!||
!||      use spmd_mod   ! or: use spmd_profiler_mod directly
!||      ...
!||      call spmd_profiler_flush()   ! writes spmd_timeline_rank_NNNNN.json
!||      call MPI_Finalize(ierr)
!||
!||    For explicit initialisation (optional — profiler auto-inits):
!||
!||      call spmd_profiler_init(rank)
!||====================================================================
      module spmd_profiler_mod
        use, intrinsic :: iso_c_binding
        implicit none

#ifdef SPMD_PROFILE
        !> C++ back-end — declared private so callers use the Fortran wrappers.
        private :: spmd_profiler_init_c, spmd_profiler_flush_c

        interface
          subroutine spmd_profiler_init_c(rank) &
            bind(c, name="spmd_profiler_init")
            import :: c_int
            integer(c_int), intent(in) :: rank
          end subroutine spmd_profiler_init_c

          subroutine spmd_profiler_flush_c() &
            bind(c, name="spmd_profiler_flush")
          end subroutine spmd_profiler_flush_c
        end interface
#endif

      contains

!||====================================================================
!||    spmd_profiler_init   spmd_profiler_mod.F90
!||
!||    Initialise the profiler and set the rank used in the output file
!||    name.  Optional: the profiler auto-initialises on the first
!||    spmd_profiler_record_in call by querying MPI_COMM_WORLD.
!||====================================================================
        subroutine spmd_profiler_init(rank)
          implicit none
          integer, intent(in) :: rank
#ifdef SPMD_PROFILE
          integer(c_int) :: rank_c
          rank_c = int(rank, c_int)
          call spmd_profiler_init_c(rank_c)
#endif
        end subroutine spmd_profiler_init

!||====================================================================
!||    spmd_profiler_flush   spmd_profiler_mod.F90
!||
!||    Write the collected timeline to  spmd_timeline_rank_NNNNN.json
!||    and clear the in-memory buffer.
!||
!||    Must be called BEFORE MPI_Finalize (MPI_Wtime is no longer valid
!||    after finalization).
!||====================================================================
        subroutine spmd_profiler_flush()
          implicit none
#ifdef SPMD_PROFILE
          call spmd_profiler_flush_c()
#endif
        end subroutine spmd_profiler_flush

      end module spmd_profiler_mod

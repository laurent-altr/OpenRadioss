!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
        module cppsort_mod
          use iso_c_binding
          interface
            subroutine stlsort_int(n,a) bind(C,name="stlsort_int")
              use iso_c_binding
              integer(c_int) :: n
              integer(c_int) :: a(n)
            end subroutine stlsort_int
            subroutine stlsort_float(n,a) bind(C,name="stlsort_float")
              use iso_c_binding
              integer(c_int) :: n
              real(c_float) :: a(n)
            end subroutine stlsort_float
            subroutine stlsort_double(n,a) bind(C,name="stlsort_double")
              use iso_c_binding
              integer(c_int) :: n
              real(c_double) :: a(n)
            end subroutine stlsort_double
          end interface
          interface cppsort
             module procedure cppsort_int
             module procedure cppsort_real
             module procedure cppsort_double
          end interface
          contains
            subroutine cppsort_int(n, a)
              implicit none
              integer :: n
              integer :: a(n)
              call stlsort_int(n, a)
            end subroutine cppsort_int
            subroutine cppsort_real(n, a)
              implicit none
              integer :: n
              real :: a(n)
              call stlsort_float(n, a)
            end subroutine cppsort_real
            subroutine cppsort_double(n, a)
              implicit none
              integer :: n
              real(kind=8) :: a(n)
              call stlsort_double(n, a)
            end subroutine cppsort_double
        end module cppsort_mod

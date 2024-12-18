!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      module parith_on_mod
#include "my_real.inc"

        type pon_ 
            integer :: SFSKY !< second dimension of FSKY 
            integer :: SADSKY !< size of ADSKY
            my_real, dimension(:,:), allocatable :: FSKY !< 8xSFSKY array of the skyline Forces
            my_real, dimension(:), allocatable :: FSKYM !< mass (solid only?) 
            my_real, dimension(:), allocatable :: FTHESKY !<        
            my_real, dimension(:), allocatable :: CONDNSKY !< 
            my_real, dimension(:), allocatable :: FSKYD !< sph ?
            integer, dimension(:), allocatable :: ADSKY !< pointers to FSKY
             !spmd:
            integer, dimension(:), allocatable :: IADRCP !< reception of forces
            integer, dimension(:), allocatable :: IRECVP !< reception of forces
            integer, dimension(:), allocatable :: IADSDP !< send forces
            integer, dimension(:), allocatable :: ISENDP !< send forces

        end type pon_
      end module parith_on_mod

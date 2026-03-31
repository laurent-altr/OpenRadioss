!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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


      module gpu_pref
        use iso_c_binding
        implicit none

!!       interface set_prefloc
!!         module procedure set_prefloc_1d,   &
!!                          set_prefloc_1d_i, &
!!                          set_prefloc_2d,   &
!!                          set_prefloc_2d_i
!!       end interface set_prefloc

        integer(kind=C_INT32_T), private :: col(7) = [ &
        & int(Z'0000ff00',kind=C_INT32_T), &
        & int(Z'000000ff',kind=C_INT32_T), &
        & int(Z'00ffff00',kind=C_INT32_T), &
        & int(Z'00ff00ff',kind=C_INT32_T), &
        & int(Z'0000ffff',kind=C_INT32_T), &
        & int(Z'00ff0000',kind=C_INT32_T), &
        & int(Z'00ffffff',kind=C_INT32_T) ]
        character,private,target :: tempName(256)

        type, bind(C):: nvtxEventAttributes
          integer(C_INT16_T):: version=1
          integer(C_INT16_T):: size=48 !
          integer(C_INT):: category=0
          integer(C_INT):: colorType=1 ! NVTX_COLOR_ARGB = 1
          integer(C_INT):: color
          integer(C_INT):: payloadType=0 ! NVTX_PAYLOAD_UNKNOWN = 0
          integer(C_INT):: reserved0
          integer(C_INT64_T):: payload   ! union uint,int,double
          integer(C_INT):: messageType=1  ! NVTX_MESSAGE_TYPE_ASCII     = 1
          type(C_PTR):: message  ! ascii char
        end type

#ifdef _OPENACC
        interface nvtxRangePush
          ! push range with custom label and standard color
          subroutine nvtxRangePushA(name) bind(C, name='nvtxRangePushA')
            use iso_c_binding
            character(kind=C_CHAR) :: name(256)
          end subroutine

          ! push range with custom label and custom color
          subroutine nvtxRangePushEx(event) bind(C, name='nvtxRangePushEx')
            use iso_c_binding
            import:: nvtxEventAttributes
            type(nvtxEventAttributes):: event
          end subroutine
        end interface

        interface nvtxRangePop
          subroutine nvtxRangePop() bind(C, name='nvtxRangePop')
          end subroutine
        end interface
#endif


      contains

        subroutine nvtxStartRange(name,id)
          character(kind=c_char,len=*) :: name
          integer, optional:: id
          type(nvtxEventAttributes):: event
          character(kind=c_char,len=256) :: trimmed_name
          integer:: i

          trimmed_name=trim(name)//c_null_char

          ! move scalar trimmed_name into character array tempName
          do i=1,LEN(trim(name)) + 1
            tempName(i) = trimmed_name(i:i)
          enddo


          if ( .not. present(id)) then
#ifdef _OPENACC
            call nvtxRangePush(tempName)
#endif
          else
            event%color=col(mod(id,7)+1)
            event%message=c_loc(tempName)
#ifdef _OPENACC
            call nvtxRangePushEx(event)
#endif
          end if
        end subroutine

        subroutine nvtxEndRange
#ifdef _OPENACC
          call nvtxRangePop
#endif
        end subroutine
!!  !> @brief No-op stub for 1-D real array.
!!  !> @details cudaMemAdvise hints are not needed when using explicit
!!  !>          OpenACC data directives (!$ACC DATA CREATE / CAPTURE).
!!  !>          Kept as no-op to preserve call sites.
!!  !> @param[inout] arr_1d  Allocatable 1-D real array
        !!  SUBROUTINE set_prefloc_1d(arr_1d)
        !!  use precision_mod, only : WP
        !!  IMPLICIT NONE
        !!  real(WP), dimension(:), allocatable, intent(inout) :: arr_1d
        !!  ! no-op: device data management handled by !$ACC DATA directives
        !!  END SUBROUTINE

!!  !> @brief No-op stub for 1-D integer array.
!!  !> @param[inout] arr_1d  Allocatable 1-D integer array
        !!  SUBROUTINE set_prefloc_1d_i(arr_1d)
        !!  IMPLICIT NONE
        !!  integer, dimension(:), allocatable, intent(inout) :: arr_1d
        !!  ! no-op: device data management handled by !$ACC DATA directives
        !!  END SUBROUTINE

!!  !> @brief No-op stub for 2-D real array.
!!  !> @param[inout] arr_1d  Allocatable 2-D real array
        !!  SUBROUTINE set_prefloc_2d(arr_1d)
        !!  use precision_mod, only : WP
        !!  IMPLICIT NONE
        !!  real(WP), dimension(:,:), allocatable, intent(inout) :: arr_1d
        !!  ! no-op: device data management handled by !$ACC DATA directives
        !!  END SUBROUTINE

!!  !> @brief No-op stub for 2-D integer array.
!!  !> @param[inout] arr_1d  Allocatable 2-D integer array
        !!  SUBROUTINE set_prefloc_2d_i(arr_1d)
        !!  IMPLICIT NONE
        !!  integer, dimension(:,:), allocatable, intent(inout) :: arr_1d
        !!  ! no-op: device data management handled by !$ACC DATA directives
        !!  END SUBROUTINE


      END MODULE gpu_pref

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
         module spmd_inter_window_update
          implicit none
          private
          public :: spmd_inter_window_update_intra 
          public :: spmd_inter_window_update_inter

         contains

         subroutine spmd_inter_window_update_intra(inter_win, nodes, intbuf_tab, ipari,npari,ispmd)
          use inter_shm_mod 
          use nodal_arrays_mod
          use intbufdef_mod
          use iso_c_binding
          implicit none
#ifdef MPI
#include  "mpif.h"
#else
#define MPI_ADDRESS_KIND 8
#endif

! input arguments
          integer, intent(in) :: ispmd
          integer, intent(in) :: npari
          type(inter_win_), intent(inout) :: inter_win
          type(nodal_arrays_), intent(in) :: nodes
          type(intbuf_struct_), intent(in) :: intbuf_tab
          integer, dimension(npari), intent(in) :: ipari
! local variables
          integer :: mpi_err
          integer(MPI_ADDRESS_KIND) :: j,i,ibegin,iend
          double precision, dimension(:), pointer, volatile :: shared_data
          integer :: igap, intth, intfric, ityp, itied, nmn, nsn, inacti, ifq
          integer :: lr, li           
          integer :: ielec_offset, temp_offset, areas_offset, gap_s_offset, gap_sl_offset, ipartfrics_offset
          integer :: ispmd_offset, itab_offset, IKINET_offset, i_offset 
          integer(MPI_ADDRESS_KIND) :: buf64
#ifdef MPI
          igap = ipari(21)
          intth = ipari(47)
          intfric = ipari(72)
          ityp = ipari(7)
          itied = ipari(85)
          nmn = ipari(6)
          nsn = ipari(5)
          inacti = ipari(22)
          ifq =ipari(31)

          if(inter_win%COMM_INTRA_NODE== MPI_COMM_NULL) return

          shared_data => inter_win%shared_data
          call MPI_Barrier(inter_win%COMM_INTRA_NODE, mpi_err)
          ! Barrier
          ! Update shared memory
          ibegin = inter_win%addresses(inter_win%rank_inter+1,1)
          iend = inter_win%addresses(inter_win%rank_inter+1,2)
          ielec_offset = 0
          temp_offset = 0
          areas_offset = 0 
          gap_s_offset = 0 
          gap_sl_offset =0
          ipartfrics_offset = 0

          lr = 8
          if(igap==1 .or. igap==2)then
            gap_s_offset = lr + 1
            lr = lr + 1
          elseif(igap==3)then 
            gap_s_offset = lr + 1
            gap_sl_offset = lr + 2
            lr = lr + 2
          endif
          if(intth>0)then
             temp_offset = lr + 1
             areas_offset = lr + 2
             lr = lr + 2
          endif
          ispmd_offset = lr + 1
          itab_offset = lr + 2
          IKINET_offset = lr + 3
          i_offset = lr + 4
          ! =====
          li = lr +4
          if(intth>0)then
             ielec_offset = li + 1
             li = li + 1
          endif
          if(intfric>0)then
             ipartfrics_offset = li + 1
             li = li + 1
          endiF              

          lr = 0
          do i =1, nsn
            j = intbuf_tab%nsv(i)
            if(nodes%weight(j) == 0) cycle
!           write(800+ispmd,*) 'Node ',j
!           write(800+ispmd,*) 'X(1) ',nodes%X(1,j),ibegin-1+lr+1
!           write(800+ispmd,*) 'X(2) ',nodes%X(2,j)
!           write(800+ispmd,*) 'X(3) ',nodes%X(3,j)
!           write(800+ispmd,*) 'V(1) ',nodes%V(1,j)
!           write(800+ispmd,*) 'V(2) ',nodes%V(2,j)
!           write(800+ispmd,*) 'V(3) ',nodes%V(3,j)
!           write(800+ispmd,*) 'MS ',nodes%MS(j)
!           write(800+ispmd,*) 'STFNS ',INTBUF_TAB%STFNS(I)
!           if(igap==1 .or. igap==2)then
!               write(800+ispmd,*) 'gap_s ',intbuf_tab%gap_s(i)
!           elseif(igap==3)then
!                 write(800+ispmd,*) 'gap_s ',intbuf_tab%gap_s(i)
!                 write(800+ispmd,*) 'gap_sl ',intbuf_tab%gap_sl(i)
!           endif
            shared_data(ibegin-1+lr+1) = nodes%X(1,j)
            shared_data(ibegin-1+lr+2) = nodes%X(2,j)
            shared_data(ibegin-1+lr+3) = nodes%X(3,j)
            shared_data(ibegin-1+lr+4) = nodes%V(1,j)
            shared_data(ibegin-1+lr+5) = nodes%V(2,j)
            shared_data(ibegin-1+lr+6) = nodes%V(3,j)
            shared_data(ibegin-1+lr+7) = nodes%MS(j)
            shared_data(ibegin-1+lr+8) = INTBUF_TAB%STFNS(I)          
            if(igap==1 .or. igap==2)then
                shared_data(ibegin-1+lr+gap_s_offset) = intbuf_tab%gap_s(i)
            elseif(igap==3)then 
                shared_data(ibegin-1+lr+gap_s_offset)= intbuf_tab%gap_s(i)
                shared_data(ibegin-1+lr+gap_sl_offset)=intbuf_tab%gap_sl(i)
            endif
            if(intth>0)then
               shared_data(ibegin-1+lr+temp_offset) = 0!nodes%temp(j)
               shared_data(ibegin-1+lr+areas_offset) =0! intbuf_tab%areas(i)
            endif
            buf64 = ispmd
            shared_data(ibegin-1+lr+ispmd_offset) = transfer(buf64, 0.0d0)
            buf64 = nodes%ITAB(j)
            shared_data(ibegin-1+lr+itab_offset) = transfer(buf64, 0.0d0)        
            buf64 = nodes%KINET(j)
            shared_data(ibegin-1+lr+IKINET_offset) = transfer(buf64, 0.0d0)
            shared_data(ibegin-1+lr+i_offset) = transfer(i, 0.0d0)                              
             if(intth>0)then
               shared_data(ibegin-1+lr+ielec_offset) =0! intbuf_tab%ielec(i)
             endif
             if(intfric>0)then
                buf64 = intbuf_tab%ipartfrics(i)
                shared_data(ibegin-1+lr+ipartfrics_offset) = transfer(buf64,0.0d0)
             endif 
             lr = lr + inter_win%rsiz + inter_win%isiz
          enddo
       !  inter_win%ielec_offset = ielec_offset
       !  inter_win%temp_offset = temp_offset
       !  inter_win%areas_offset = areas_offset 
       !  inter_win%gap_s_offset = gap_s_offset 
       !  inter_win%gap_sl_offset = gap_sl_offset
       !  inter_win%ipartfrics_offset = ipartfrics_offset
       !  inter_win%x_offset = 1
       !  inter_win%v_offset = 4
       !  inter_win%ms_offset = 7
       !  inter_win%stfns_offset = 8
       !  inter_win%ispmd_offset = ispmd_offset
       !  inter_win%itab_offset = itab_offset
       !  inter_win%ikinet_offset = IKINET_offset
       !  inter_win%i_offset = i_offset

#endif
        end subroutine spmd_inter_window_update_intra 

        subroutine print_inter_window(inter_win)
          use inter_shm_mod 
          use iso_c_binding
          implicit none
#ifdef MPI
#include  "mpif.h"
#else
#define MPI_ADDRESS_KIND 8
#endif
! input arguments
          type(inter_win_), intent(inout) :: inter_win
! local variables
          integer :: mpi_err
          integer(MPI_ADDRESS_KIND) :: i, ibegin,iend,j,k
      integer :: size_shared_data
      integer :: ispmd
      double precision,  dimension(:,:), pointer :: shared_real
      integer(kind=8), dimension(:,:), pointer :: shared_int 

      size_shared_data = (inter_win%RSIZ + inter_WIN%ISIZ)!
      call c_f_pointer(inter_win%shared_base, shared_int,[ size_shared_data, inter_win%nsn_global ] )
      call c_f_pointer(inter_win%shared_base, shared_real,[ size_shared_data, inter_win%nsn_global ] )

         ibegin = 0
         ispmd = inter_win%rank_inter
         k = 1
         do i = 1, inter_win%nsn_global
            write(700+ispmd,*) 'Node ',i
            do j = 1, size_shared_data
               write(700+ispmd,*) K,shared_real(j,i),shared_int(j,i) 
               k = k + 1
            end do
            ibegin = ibegin + size_shared_data
         end do


        end subroutine

        subroutine spmd_inter_window_update_inter(inter_win)
          use inter_shm_mod 
          use iso_c_binding
          implicit none
#ifdef MPI
#include  "mpif.h"
#else
#define MPI_ADDRESS_KIND 8
#endif

! input arguments
          type(inter_win_), intent(inout) :: inter_win
! local variables
          integer :: mpi_err
          integer(MPI_ADDRESS_KIND) :: i, ibegin,iend
          double precision, dimension(:), pointer, volatile :: shared_data
#ifdef MPI
          shared_data => inter_win%shared_data
          if(inter_win%rank_intra_node == 0) then
            call MPI_Allgatherv(MPI_IN_PLACE, 0, MPI_DATATYPE_NULL, &
              shared_data, inter_win%recvcounts, inter_win%displs, MPI_DOUBLE_PRECISION, &
              inter_win%COMM_INTER_NODE, mpi_err)
          end if
#endif
        !call print_inter_window(inter_win)
        end subroutine spmd_inter_window_update_inter 

        end module spmd_inter_window_update

 
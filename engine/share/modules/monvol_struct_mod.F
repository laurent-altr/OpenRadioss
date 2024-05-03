Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    monvol_struct_mod                ../starter/share/modules1/monvol_struct_mod.F
      !||--- called by ------------------------------------------------------
      !||    aleno                            ../starter/source/airbag/fvmbag1.F
      !||    aletg                            ../starter/source/airbag/fvmbag1.F
      !||    applysort2fvm                    ../starter/source/airbag/fvmesh0.F
      !||    ddsplit                          ../starter/source/restart/ddsplit/ddsplit.F
      !||    deallocate_igrsurf_split         ../starter/source/spmd/deallocate_igrsurf_split.F
      !||    domdec2                          ../starter/source/spmd/domdec2.F
      !||    dometis                          ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    fillcne                          ../starter/source/spmd/domdec2.F
      !||    fvbag_vertex                     ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    fvbric1                          ../starter/source/airbag/fvbric1.F
      !||    fvdim                            ../starter/source/airbag/fvmesh.F
      !||    fvmesh0                          ../starter/source/airbag/fvmesh0.F
      !||    fvnodi                           ../starter/source/airbag/fvmbag1.F
      !||    fvtgi                            ../starter/source/airbag/fvmbag1.F
      !||    hm_read_monvol_type1             ../starter/source/airbag/hm_read_monvol_type1.F
      !||    hm_read_monvol_type10            ../starter/source/airbag/hm_read_monvol_type10.F
      !||    hm_read_monvol_type11            ../starter/source/airbag/hm_read_monvol_type11.F
      !||    hm_read_monvol_type2             ../starter/source/airbag/hm_read_monvol_type2.F
      !||    hm_read_monvol_type3             ../starter/source/airbag/hm_read_monvol_type3.F
      !||    hm_read_monvol_type4             ../starter/source/airbag/hm_read_monvol_type4.F
      !||    hm_read_monvol_type5             ../starter/source/airbag/hm_read_monvol_type5.F
      !||    hm_read_monvol_type6             ../starter/source/airbag/hm_read_monvol_type6.F
      !||    hm_read_monvol_type7             ../starter/source/airbag/hm_read_monvol_type7.F
      !||    hm_read_monvol_type8             ../starter/source/airbag/hm_read_monvol_type8.F
      !||    hm_read_monvol_type9             ../starter/source/airbag/hm_read_monvol_type9.F
      !||    hm_read_thgrou                   ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_thgrki_vent                   ../starter/source/output/th/hm_thgrki_vent.F
      !||    hypermesh_tetra                  ../starter/stub/fvmbags_stub.F
      !||    igrsurf_split                    ../starter/source/spmd/igrsurf_split.F
      !||    init_monvol                      ../starter/source/airbag/init_monvol.F
      !||    lectur                           ../starter/source/starter/lectur.F
      !||    monvol_build_edges               ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_check_delete_duplicated   ../starter/source/airbag/monvol_check_delete_duplicated.F
      !||    monvol_orient_surf               ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_reverse_normals           ../starter/share/modules1/monvol_struct_mod.F
      !||    monvol_triangulate_surface       ../starter/source/airbag/monvol_triangulate_surface.F
      !||    read_monvol                      ../starter/source/airbag/read_monvol.F
      !||    st_qaprint_driver                ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_monvol                ../starter/source/output/qaprint/st_qaprint_monvol.F
      !||    w_monvol                         ../starter/source/restart/ddsplit/w_monvol.F
      !||====================================================================
      MODULE MONVOL_STRUCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include "my_real.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
      TYPE MONVOL_STRUCT_
!     Automatic surface hole filling
      INTEGER :: NB_FILL_TRI
      INTEGER, DIMENSION(:), ALLOCATABLE :: FILL_TRI
      END TYPE MONVOL_STRUCT_
      
      CONTAINS
      !||====================================================================
      !||    monvol_deallocate   ../starter/share/modules1/monvol_struct_mod.F
      !||--- called by ------------------------------------------------------
      !||    lectur              ../starter/source/starter/lectur.F
      !||====================================================================
      SUBROUTINE MONVOL_DEALLOCATE(NVOLU, T_MONVOL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"    
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------   
      INTEGER, INTENT(IN) :: NVOLU
      TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(INOUT) :: T_MONVOL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II
      
      DO II = 1, NVOLU
         IF (ALLOCATED(T_MONVOL(II)%FILL_TRI)) DEALLOCATE(T_MONVOL(II)%FILL_TRI)
      ENDDO
      END SUBROUTINE MONVOL_DEALLOCATE
      END

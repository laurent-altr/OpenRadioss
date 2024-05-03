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
      !||    intbuf_fric_mod                 ../starter/share/modules1/intbuf_fric_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit                         ../starter/source/restart/ddsplit/ddsplit.F
      !||    hm_read_friction_models         ../starter/source/interfaces/friction/reader/hm_read_friction_models.F
      !||    hm_read_friction_orientations   ../starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
      !||    hm_read_inter_struct            ../starter/source/interfaces/reader/hm_read_inter_struct.F
      !||    hm_read_inter_type07            ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type11            ../starter/source/interfaces/int11/hm_read_inter_type11.F
      !||    hm_read_inter_type24            ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_inter_type25            ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||    hm_read_interfaces              ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    i11sti3                         ../starter/source/interfaces/inter3d1/i11sti3.F
      !||    i24gapm                         ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i24sti3                         ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i25gapm                         ../starter/source/interfaces/inter3d1/i25sti3.F
      !||    i25sti3                         ../starter/source/interfaces/inter3d1/i25sti3.F
      !||    i25sti_edg                      ../starter/source/interfaces/inter3d1/i25sti_edg.F
      !||    i7sti3                          ../starter/source/interfaces/inter3d1/i7sti3.F
      !||    inint3                          ../starter/source/interfaces/inter3d1/inint3.F
      !||    inintr                          ../starter/source/interfaces/interf1/inintr.F
      !||    inintr_orthdirfric              ../starter/source/interfaces/interf1/inintr_orthdirfric.F
      !||    intbuf_fric_copy                ../starter/source/interfaces/interf1/intbuf_fric_copy.F
      !||    intbuf_fric_ini_starter         ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||    intbuf_ini_starter              ../starter/source/interfaces/intbuf/intbuf_ini_starter.F
      !||    inter_dcod_friction             ../starter/source/interfaces/reader/inter_dcod_friction.F
      !||    intfric_wresti                  ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||    intfric_wrestr                  ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||    lecint                          ../starter/source/interfaces/interf1/lecint.F
      !||    lectur                          ../starter/source/starter/lectur.F
      !||    split_interfaces                ../starter/source/restart/ddsplit/split_interfaces.F
      !||    st_qaprint_driver               ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_friction             ../starter/source/output/qaprint/st_qaprint_friction.F
      !||    triintfric                      ../starter/source/interfaces/interf1/trintfric.F
      !||====================================================================
      MODULE INTBUF_FRIC_MOD
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------

C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      TYPE INTBUF_FRIC_STRUCT_  
      INTEGER ::    NSETPRTS       ! number of couple of parts 
      INTEGER ::    FRICMOD        ! the friction model
      INTEGER ::    FRICFORM        ! the friction formulation ( viscous or incremental)
      INTEGER ::    IFFILTER       !Friction filtering flag 
      INTEGER ::    S_TABPARTS_FRIC  ! number of parts  
      INTEGER ::    IORTHFRIC          ! Flag for orthotropic friction 0 isotropic / 1 orthotropic like main material / 2 orthotropic and orthotropy axes are defined in friction model
      my_real ::    XFILTR_FRIC        !Filtering coefficient

      INTEGER, DIMENSION(:), ALLOCATABLE :: TABCOUPLEPARTS_FRIC    ! Table of couple of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: TABPARTS_FRIC          ! Table of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: ADPARTS_FRIC           ! Table of adress of couple of parts
      INTEGER, DIMENSION(:), ALLOCATABLE :: IFRICORTH              ! Table of Orthotropic type of couple of parts 
      my_real, DIMENSION(:), ALLOCATABLE :: TABCOEF_FRIC           ! Table of friction coefficients

      END TYPE INTBUF_FRIC_STRUCT_

      END MODULE INTBUF_FRIC_MOD

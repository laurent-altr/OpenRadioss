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
      !||    restmod                     ../starter/share/modules1/restart_mod.F
      !||--- called by ------------------------------------------------------
      !||    contrl                      ../starter/source/starter/contrl.F
      !||    leclas                      ../starter/source/loads/laser/leclas.F
      !||    lectur                      ../starter/source/starter/lectur.F
      !||    new_link                    ../starter/source/coupling/rad2rad/new_link.F
      !||    r2r_check                   ../starter/source/coupling/rad2rad/r2r_check.F
      !||    r2r_check_seg               ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_clean_inter             ../starter/source/coupling/rad2rad/r2r_clean_inter.F
      !||    r2r_count                   ../starter/source/coupling/rad2rad/r2r_count.F
      !||    r2r_domdec                  ../starter/source/coupling/rad2rad/r2r_domdec.F
      !||    r2r_group                   ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_input                   ../starter/source/coupling/rad2rad/r2r_input.F
      !||    r2r_monvol                  ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec                  ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec_name             ../starter/source/coupling/rad2rad/r2r_prelec_name.F
      !||    r2r_split                   ../starter/source/coupling/rad2rad/r2r_split.F
      !||    r2r_void                    ../starter/source/coupling/rad2rad/r2r_void.F
      !||    r2r_void_1d                 ../starter/source/coupling/rad2rad/r2r_void.F
      !||    read_ebcs                   ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||    set_user_window_nodes       ../starter/source/user_interface/user_windows_tools.F
      !||    split_ebcs                  ../starter/source/boundary_conditions/ebcs/split_ebcs.F
      !||    st_qaprint_ebcs             ../starter/source/output/qaprint/st_qaprint_ebcs.F
      !||    tag_elem_void_r2r           ../starter/source/coupling/rad2rad/tagelem_r2r.F
      !||    tag_elem_void_r2r_lin       ../starter/source/coupling/rad2rad/tagelem_r2r.F
      !||    tagnod_r2r_s                ../starter/source/coupling/rad2rad/tagnod_r2r.F
      !||    uelt_spmd_additional_node   ../starter/source/user_interface/uaccess.F
      !||    upgrade_remnode2            ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      MODULE RESTMOD
      USE CLUSTER_MOD
      USE SPHBOX
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
#include "my_real.inc"

        INTEGER , DIMENSION(:), ALLOCATABLE :: 
     .      ICODE,   ISKEW,    ISKWN,   IFRAME,  NETH,  
     .      IBCSLAG,
     .      IPART,   NOM_OPT, NPC,      IXTG,    IXTG1,
     .      IXS,     IXQ,     IXC,      IXT,     IXP,     IXR,
     .      ITAB,    ITABM1,  GJBUFI,   
     .      IFILL,   IMS,     ISUBS,
     .      KXX,     IXX,      KXSP,    IXSP,    NOD2SP,
     .      ISPSYM,  ISPCOND, ISPHIO,   LPRTSPH, LONFSPH, IBCL,
     .      IBFV,    IECRAN,  ILAS,     LACCELM, 
     .      NNLINK,  LNLINK,  IPARG,   DD_IAD,  IGRV, 
     .      IEXLNK,  IBVEL,    LBVEL,   IACTIV,  IBMPC, 
     .      KINET,   IPARI,   NPRW,    LPRW,    
     .      ICONX,   LGAUGE,
     .      MADPRT,  MADSH4,  MADSH3,   MADSOL,  MADNOD,  MADFAIL, 
     .      NPBY,    NPBYL,   LPBY,     LPBYL,   LRIVET,  NSTRF,
     .      LJOINT,  NODPOR,  MONVOL,   LAGBUF,  
     .      IPART_STATE,IRBYM ,LNRBYM, WEIGHT_RM,ICODRBYM,
     .      IRBE3 ,LRBE3,INOD_PXFEM,IEL_PXFEM,
     .      IADC_PXFEM, ADSKY_PXFEM, MADIDX,MADCLNOD,NOM_SECT,
     .      IRBE2    ,LRBE2, PROCNE_PXFEM, IADSDP_PXFEM,
     .      IADRCP_PXFEM     ,ITHVAR,ICFIELD,ILOADP,INOD_CRKXFEM,
     .      IEL_CRKXFEM,IADC_CRKXFEM, ADSKY_CRKXFEM,PROCNE_CRKXFEM,
     .      CNE_CRKXFEM,IADSDP_CRKXFEM,IADRCP_CRKXFEM,IBUFSSG_IO,
     .      IBC_PLY,IBORDNODE,IEDGESH,XNODANIMEL,NODEDGE,IEDGE,
     .      CRKNODIAD,DD_R2R_ELEM,KXIG3D,IXIG3D,
     .      MAIN_PROC ,TAG_SKINS6,IBCSCYC,LBCSCYC,
     .      INTERLOADP,KLOADPINTER,LOADPINTER
            !rank (1 to NSPMD) of the processor that is the main of the node 
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_GLOBAL_VOIS,FACE_VOIS

      INTEGER, DIMENSION(:), ALLOCATABLE :: IBFTEMP, IBCV, IBCR, IBFFLUX

        INTEGER , DIMENSION(:), ALLOCATABLE :: 
     .      IAD_ELEM,  IAD_RBY,  IAD_RBY2, IAD_I2M, 
     .      IADCJ,     IAD_RBM,  IAD_RBM2, IAD_SEC,    IAD_CUT,   FR_ELEM, 
     .      FR_RBY,    FR_WALL,  FR_RBY2,  FR_I2M,     FR_MV,    FR_LL, 
     .      FR_CJ,     FR_RBM,   FR_RBM2,  FR_SEC,     FR_CUT,    RG_CUT, 
     .      FR_MAD,    FR_I18,   WEIGHT,   NEWFRONT,   NODGLOB,   NBRCVOIS, 
     .      LNRCVOIS,  NBSDVOIS, LNSDVOIS, NERCVOIS,   LERCVOIS,  NESDVOIS, 
     .      LESDVOIS,  NPSEGCOM, LSEGCOM,  NPORGEO, 
     .      LNODPOR,   ICODT,    ICODR,    ADSKY,      PROCNE,
     .      ADDCNI2,   PROCNI2,  IADSDP,   IADRCP,     IADS,      IADWAL, 
     .      IADRBK,    IADI2,    IADMV2,   IADMV3,     IADLL,     IADRBM, 
     .      IADI18,    ILINK,    FR_RL,    LLINK,      IADRL,     LINALE,
     .      NEFLSW,    NNFLSW,   ICUT,     LGRAV,      FASOLFR,   DD_R2R,
     .      FR_LAGF,   LLAGF,    IGEO,     IPM,        ICONTACT,  ISKWP,
     .      NSKWP,     ISENSP,   NSENSP,   IACCP,      NACCP,
     .                                                 IAD_RBE3M, FR_RBE3M,
     .      FR_RBE3MP ,FR_RBYM,  FR_RBYM2, IAD_RBYM   ,IAD_RBYM2, IADRBMK,
     .      ICODE_PLY ,ICODT_PLY,ISKEW_PLY,IAD_RBE2   ,FR_RBE2,
     .      LCFIELD   ,LLOADP,
     .      IAD_EDGE  ,FR_EDGE,  FR_NBEDGE,IGAUP      ,NGAUP,     NODLEVXF,WEIGHT_MD,
     .      NODGLOBXFE,ELCUTC,   NODENR,   KXFENOD2ELC,ENRTAG,
     .      TABSENSOR ,ADDCSRECT,FR_NOR,   IAD_FRNOR  , PROCNOR,    
     .      FR_EDG,    IAD_FREDG,IDRAPE,   NATIV0_SMS , SEGQUADFR,ISKWP_L, NE_NERCVOIS,  
     .      NE_LERCVOIS,  NE_NESDVOIS, NE_LESDVOIS

        my_real , 
     .    DIMENSION(:), ALLOCATABLE :: 
     .      X,       D,       V,       VR,      DR,      THKE,
     .      DAMPR,   DAMP,    MS,      IN,      TF,      PM, 
     .      SKEW,    XFRAME,  GEO,     EANI,    BUFMAT,  BUFGEO,
     .      BUFSF,   RBMPC,   GJBUFR,  W,       VEUL,    FILL, 
     .      DFILL,   ALPH,    WB,      DSAVE,   ASAVE, 
     .      SPBUF,
     .      VSPHIO,  SPHVELN, MSNF,    FORC    ,
     .      VEL,     FSAV,    FZERO,   XLAS,   ACCELM, 
     .      FBVEL,   GRAV,    FR_WAVE, PARTS0,
     .      ELBUF,   
     .      RWBUF,   RWSAV,   RBY,     RBYL,    
     .      RIVET,   SECBUF,  VOLMON,  LAMBDA,  WA,      FV, 
     .      A,       AR,      STIFN,   VISCN,   STIFR,   PARTSAV,
     .      FSKY,    FSKYM,   FSKYD,   UWA,     VAL2,    PHI,
     .      R,       ESTIF,   CRFLSW,  FLSW,    FANI,
     .      XCUT,    ANIN,    TANI,    SECFCUM, WASPH, 
     .      W16,     DRETRIO, LBVRS,   PV,      RCONX  , RCONTACT,
     .      ACONTACT,PCONTACT,
     .      FACTIV,           RBYM,    FRBE3 ,
     .      MS_PLY,  ZI_PLY,  MS0,     ADMSMS,  DMELC,   DMELTG,
     .      MSSA,    DMELS,   MSTR,    DMELTR,  MSP,     DMELP,
     .      MSRT,    DMELRT,  RES_SMS, PHIE,    MSF,
     .      CFIELD,  MSZ2,    DIAG_SMS,LOADP,   DMINT2,
     .      POR,     GAUGE,   WAGAP   ,RTHBUF,  KNOT,
     .      WIGE,    RDRAPE,  TAB_MAT, MS_2D ,KNOTLOCPC,KNOTLOCEL,
     .      FCONT_MAX,IN0  ,  DGAPLOADINT,DPL0CLD,VEL0CLD
        
      my_real, 
     .    DIMENSION(:, :), ALLOCATABLE :: XCELL
      my_real, 
     .    DIMENSION(:, :, :), ALLOCATABLE :: XFACE

      my_real , 
     .    DIMENSION(:), ALLOCATABLE :: FCONV, FBFTEMP, FRADIA, FBFFLUX

      TYPE (CLUSTER_) ,DIMENSION(:), ALLOCATABLE :: CLUSTER
C Double Precision array FBSAV6 (sensor 11 or 12 and parith/on)
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: FBSAV6
      TYPE (SPSYM_STRUCT)  :: XSPSYM,VSPSYM,WSMCOMP
      TYPE (FAILWAVE_STR_) :: FAILWAVE 
      TYPE (NLOCAL_STR_)   :: NLOC_DMG 
C Mat+ Prop timers (DDW/MONON)
      INTEGER, DIMENSION(:), ALLOCATABLE :: POIN_UMP
      INTEGER, DIMENSION(:,:), ALLOCATABLE ::TAB_UMP
      my_real, DIMENSION(:,:), ALLOCATABLE :: CPUTIME_MP_GLOB
      my_real, DIMENSION(:), ALLOCATABLE :: CPUTIME_MP
c      my_real, DIMENSION(:,:), ALLOCATABLE :: PSKID
C=======================================================================
C     Specific Adaptive Meshing :
C=======================================================================
C     SH4TREE(KSHTREE,NUMELC), SH3TREE(KSHTREE,NUMELTG)
C     1: father
C     2: 1st son (2nd son = 1st + 1, 3rd son = 1st + 2, 4th son = 1st + 3)
C     3:   = LEVEL SI ACTIVE
C         = -(LEVEL+1) SINON
C     4: group Number
C-----------------------------------------------
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: SH4TREE,SH3TREE
C-----------------------------------------------
C     IPADMESH(1,IP) = initial level of elements
C     PADMESH(1,IP) = cos(angle criteria)
C     PADMESH (2,IP) = criteria for error on thickness
C-----------------------------------------------
      INTEGER, DIMENSION(:,:),ALLOCATABLE :: IPADMESH
      my_real, 
     .         DIMENSION(:,:),ALLOCATABLE :: PADMESH
C-----------------------------------------------
      INTEGER, DIMENSION(:),ALLOCATABLE :: SH4TRIM, SH3TRIM
C-----------------------------------------------
C     MSC(N) = mass of shell element N
C     INC(N) = inertia of shell element N
C-----------------------------------------------
      my_real, 
     .         DIMENSION(:),ALLOCATABLE :: MSC,MSTG,INC,INTG
      my_real, 
     .         DIMENSION(:,:),ALLOCATABLE :: PTG
C-----------------------------------------------
C     Condensation
C-----------------------------------------------
      my_real, 
     .         DIMENSION(:),ALLOCATABLE :: MSCND,INCND
C-----------------------------------------------
C     Thermic
C-----------------------------------------------
      my_real, 
     .         DIMENSION(:),ALLOCATABLE :: MCPC, MCPTG
C=======================================================================
      DOUBLE PRECISION,
     .         DIMENSION(:), ALLOCATABLE :: DDP

      END MODULE RESTMOD

      !||====================================================================
      !||    intbufmod              ../starter/share/modules1/restart_mod.F
      !||--- called by ------------------------------------------------------
      !||    inintr                 ../starter/source/interfaces/interf1/inintr.F
      !||    lectur                 ../starter/source/starter/lectur.F
      !||    upgrade_remnode        ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||    upgrade_remnode2       ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||    upgrade_remnode_e2s    ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||    upgrade_remnode_edg    ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||    upgrade_remnode_edg2   ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||====================================================================
      MODULE INTBUFMOD
#include "my_real.inc"
        INTEGER , DIMENSION(:), ALLOCATABLE :: INBUF
        TARGET :: INBUF
       my_real,
     .         DIMENSION(:),ALLOCATABLE :: BUFIN
        TARGET :: BUFIN
      END MODULE INTBUFMOD
C=======================================================================
      !||====================================================================
      !||    intbuf_mod      ../engine/share/modules/restart_mod.F
      !||--- called by ------------------------------------------------------
      !||    radioss2        ../engine/source/engine/radioss2.F
      !||    rdresb          ../engine/source/output/restart/rdresb.F
      !||    restalloc       ../engine/source/output/restart/arralloc.F
      !||--- uses       -----------------------------------------------------
      !||    int8_mod        ../common_source/modules/interfaces/int8_mod.F
      !||    intbufdef_mod   ../common_source/modules/intbufdef_mod.F
      !||====================================================================
      MODULE INTBUF_MOD

        USE INTBUFDEF_MOD      
        USE INT8_MOD
        TYPE(INTBUF_STRUCT_),TARGET,DIMENSION(:),ALLOCATABLE :: INTBUF_TAB
        TYPE(INT8_STRUCT_),TARGET,DIMENSION(:),ALLOCATABLE :: T8

      END MODULE INTBUF_MOD
C=======================================================================
      !||====================================================================
      !||    fskyi_mod              ../engine/share/modules/restart_mod.F
      !||--- called by ------------------------------------------------------
      !||    reallocate_i_skyline   ../engine/source/system/reallocate_skyline.F
      !||    resol                  ../engine/source/engine/resol.F
      !||    restalloc              ../engine/source/output/restart/arralloc.F
      !||    spmd_exch2_a_pon       ../engine/source/mpi/forces/spmd_exch2_a_pon.F
      !||    spmd_i7fcom_pon        ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||====================================================================
      MODULE FSKYI_MOD
        my_real,
     .  DIMENSION(:,:),ALLOCATABLE :: FSKYI,TEMP_FSKYI
        INTEGER,DIMENSION(:),ALLOCATABLE :: ISKY,TEMP_ISKY
      END MODULE FSKYI_MOD
      !||====================================================================
      !||    intbuffric_mod    ../engine/share/modules/restart_mod.F
      !||--- called by ------------------------------------------------------
      !||    radioss2          ../engine/source/engine/radioss2.F
      !||    rdresb            ../engine/source/output/restart/rdresb.F
      !||    restalloc         ../engine/source/output/restart/arralloc.F
      !||--- uses       -----------------------------------------------------
      !||    intbuf_fric_mod   ../engine/share/modules/intbuf_fric_mod.F
      !||====================================================================
      MODULE INTBUFFRIC_MOD

        USE INTBUF_FRIC_MOD                     

        TYPE(INTBUF_FRIC_STRUCT_),TARGET,DIMENSION(:),ALLOCATABLE :: INTBUF_FRIC_TAB

      END MODULE INTBUFFRIC_MOD

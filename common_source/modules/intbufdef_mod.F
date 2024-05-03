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
      !||    intbufdef_mod                    ../common_source/modules/intbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    alefvm_main                      ../engine/source/ale/alefvm/alefvm_main.F
      !||    alemain                          ../engine/source/ale/alemain.F
      !||    alewdx                           ../engine/source/ale/grid/alewdx.F
      !||    build_csrect                     ../starter/source/model/mesh/build_cnel.F
      !||    c_front                          ../starter/source/restart/ddsplit/c_front.F
      !||    check_edge_state                 ../engine/source/interfaces/interf/check_edge_state.F
      !||    check_nodal_state                ../engine/source/interfaces/interf/check_nodal_state.F
      !||    check_remote_surface_state       ../engine/source/interfaces/interf/check_remote_surface_state.F
      !||    check_surface_state              ../engine/source/interfaces/interf/check_surface_state.F
      !||    chkstfn3n                        ../engine/source/interfaces/interf/chkstfn3.F
      !||    chkstifn                         ../engine/source/interfaces/inter2d/chkstifn.F
      !||    chktyp2                          ../starter/source/interfaces/interf1/chktyp2.F
      !||    cndmasi2_dim                     ../engine/source/elements/solid/solide10/s10cndf.F
      !||    cndmasi2_ini                     ../engine/source/elements/solid/solide10/s10cndf.F
      !||    copy_intbuf_tab                  ../common_source/interf/copy_intbuf_tab.F
      !||    copy_ival                        ../starter/source/restart/ddsplit/inter_tools.F
      !||    copy_ival_igeo                   ../starter/source/restart/ddsplit/inter_tools.F
      !||    copy_node_nodloc                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    copy_rval                        ../starter/source/restart/ddsplit/inter_tools.F
      !||    cp_impbuf                        ../engine/source/implicit/produt_v.F
      !||    ddsplit                          ../starter/source/restart/ddsplit/ddsplit.F
      !||    deplafakeige                     ../engine/source/assembly/deplafakeige.F
      !||    diag_int                         ../engine/source/mpi/implicit/imp_fri.F
      !||    dim_glob_k                       ../engine/source/implicit/ind_glob_k.F
      !||    dim_int7                         ../engine/source/implicit/ind_glob_k.F
      !||    dim_int_k                        ../engine/source/implicit/ind_glob_k.F
      !||    dim_kine_i                       ../engine/source/implicit/ind_glob_k.F
      !||    dim_kine_p                       ../engine/source/implicit/ind_glob_k.F
      !||    dim_kine_s                       ../engine/source/implicit/ind_glob_k.F
      !||    dim_kine_t                       ../engine/source/implicit/ind_glob_k.F
      !||    dim_kinefr                       ../engine/source/mpi/implicit/imp_fri.F
      !||    dim_kinfrk                       ../engine/source/mpi/implicit/imp_fri.F
      !||    dim_kinkn                        ../engine/source/implicit/imp_int_k.F
      !||    dim_kinmax                       ../engine/source/implicit/ind_glob_k.F
      !||    dim_kinmv                        ../engine/source/airbag/monv_imp0.F
      !||    dim_ndof_i                       ../engine/source/implicit/ind_glob_k.F
      !||    dim_ndof_ii                      ../engine/source/implicit/ind_glob_k.F
      !||    domdec2                          ../starter/source/spmd/domdec2.F
      !||    eig                              ../engine/stub/eig.F
      !||    eig1                             ../engine/stub/eig1.F
      !||    eigcond                          ../engine/stub/eigcond.F
      !||    eigp                             ../engine/stub/eigp.F
      !||    fictivmassigeo                   ../starter/source/groups/ssurftagigeo.F
      !||    fill_intercep                    ../starter/source/spmd/node/ddtools.F
      !||    fillcni2                         ../starter/source/spmd/domdec2.F
      !||    filter_node_nodloc               ../starter/source/restart/ddsplit/inter_tools.F
      !||    find_edge_from_remote_proc       ../engine/source/interfaces/interf/find_edge_from_remote_proc.F
      !||    find_edge_inter                  ../engine/source/interfaces/interf/find_edge_inter.F
      !||    find_surface_from_remote_proc    ../engine/source/interfaces/interf/find_surface_from_remote_proc.F
      !||    find_surface_inter               ../engine/source/interfaces/interf/find_surface_inter.F
      !||    flush_remnode_array              ../starter/source/interfaces/inter3d1/flush_remnode_array.F
      !||    fr_a2bd                          ../engine/source/mpi/implicit/imp_fri.F
      !||    fr_matv                          ../engine/source/mpi/implicit/imp_fri.F
      !||    fr_u2dd                          ../engine/source/mpi/implicit/imp_fri.F
      !||    fxbtagn                          ../starter/source/constraints/fxbody/fxbtagn.F
      !||    getnddli_g                       ../engine/source/mpi/implicit/imp_fri.F
      !||    hm_read_inter_type21             ../starter/source/interfaces/int21/hm_read_inter_type21.F
      !||    hm_read_inter_type25             ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||    i10fku3                          ../engine/source/interfaces/int10/i10ke3.F
      !||    i10forcf3                        ../engine/source/interfaces/int10/i10ke3.F
      !||    i10ke3                           ../engine/source/interfaces/int10/i10ke3.F
      !||    i10main_opt_tri                  ../engine/source/interfaces/intsort/i10opt_opt_tri.F
      !||    i10main_tri                      ../engine/source/interfaces/intsort/i10main_tri.F
      !||    i10mainf                         ../engine/source/interfaces/int10/i10mainf.F
      !||    i11edge                          ../starter/source/interfaces/inter3d1/i11edge.F
      !||    i11fku3                          ../engine/source/interfaces/int11/i11ke3.F
      !||    i11forcf3                        ../engine/source/interfaces/int11/i11ke3.F
      !||    i11ke3                           ../engine/source/interfaces/int11/i11ke3.F
      !||    i11main_crit_tri                 ../engine/source/interfaces/intsort/i11main_crit_tri.F
      !||    i11main_opt_tri                  ../engine/source/interfaces/intsort/i11main_opt_tri.F
      !||    i11main_tri                      ../engine/source/interfaces/intsort/i11main_tri.F
      !||    i11mainf                         ../engine/source/interfaces/int11/i11mainf.F
      !||    i14cmp                           ../engine/source/interfaces/int14/i14cmp.F
      !||    i14ist                           ../engine/source/interfaces/int14/i14ist.F
      !||    i14wfs                           ../engine/source/interfaces/int14/i14wfs.F
      !||    i15cmp                           ../engine/source/interfaces/int15/i15cmp.F
      !||    i16main                          ../engine/source/interfaces/int16/i16main.F
      !||    i17main                          ../engine/source/interfaces/int17/i17main.F
      !||    i17main_crit_tri                 ../engine/source/interfaces/int17/i17main_pena.F
      !||    i17main_pena                     ../engine/source/interfaces/int17/i17main_pena.F
      !||    i17main_tri                      ../engine/source/interfaces/int17/i17main_pena.F
      !||    i18main_kine_1                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_2                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_f                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_i                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_s                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_v                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i20ini3                          ../starter/source/interfaces/inter3d1/i20ini3.F
      !||    i20main_crit_tri                 ../engine/source/interfaces/intsort/i20main_crit_tri.F
      !||    i20main_opt_tri                  ../engine/source/interfaces/intsort/i20main_opt_tri.F
      !||    i20main_tri                      ../engine/source/interfaces/intsort/i20main_tri.F
      !||    i20mainf                         ../engine/source/interfaces/int20/i20mainf.F
      !||    i20sta                           ../starter/source/interfaces/inter3d1/inintr2.F
      !||    i20stifn                         ../starter/source/interfaces/inter3d1/i20stifn.F
      !||    i21_icrit                        ../engine/source/interfaces/intsort/i21_icrit.F
      !||    i21main_crit_tri                 ../engine/source/interfaces/intsort/i21main_crit_tri.F
      !||    i21main_gap                      ../engine/source/interfaces/int21/i21main_gap.F
      !||    i21main_opt_tri                  ../engine/source/interfaces/intsort/i21main_opt_tri.F
      !||    i21main_tri                      ../engine/source/interfaces/intsort/i21main_tri.F
      !||    i21mainf                         ../engine/source/interfaces/int21/i21mainf.F
      !||    i21reset                         ../engine/source/interfaces/int21/i21reset.F
      !||    i22main_tri                      ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i22mainf                         ../engine/source/interfaces/int22/i22mainf.F
      !||    i23main_opt_tri                  ../engine/source/interfaces/intsort/i23main_opt_tri.F
      !||    i23main_tri                      ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i23mainf                         ../engine/source/interfaces/int23/i23mainf.F
      !||    i24e2e_fictive_nodes_update      ../engine/source/interfaces/int24/i24for3e.F
      !||    i24ke3                           ../engine/source/interfaces/int24/i24ke3.F
      !||    i24main_crit_tri                 ../engine/source/interfaces/intsort/i24main_crit_tri.F
      !||    i24main_opt_tri                  ../engine/source/interfaces/intsort/i24main_opt_tri.F
      !||    i24main_tri                      ../engine/source/interfaces/intsort/i24main_tri.F
      !||    i24mainf                         ../engine/source/interfaces/int24/i24main.F
      !||    i24nitschfor3                    ../engine/source/interfaces/int24/i24nitschfor3.F
      !||    i24pxfem                         ../engine/source/interfaces/int24/i24pxfem.F
      !||    i24setnodes                      ../starter/source/interfaces/inter3d1/i24setnodes.F
      !||    i24stsecnd                       ../starter/source/interfaces/inter3d1/i24stslav.F
      !||    i24xvfic_upd                     ../engine/source/interfaces/int24/i24for3e.F
      !||    i25comp_1                        ../engine/source/interfaces/int25/i25comp_1.F
      !||    i25comp_2                        ../engine/source/interfaces/int25/i25comp_2.F
      !||    i25irtlm                         ../engine/source/interfaces/int25/i25irtlm.F
      !||    i25main_crit_tri                 ../engine/source/interfaces/intsort/i25main_crit_tri.F
      !||    i25main_free                     ../engine/source/interfaces/intsort/i25main_free.F
      !||    i25main_gap                      ../engine/source/interfaces/int25/i25main_gap.F
      !||    i25main_norm                     ../engine/source/interfaces/int25/i25main_norm.F
      !||    i25main_opt_tri                  ../engine/source/interfaces/intsort/i25main_opt_tri.F
      !||    i25main_slid                     ../engine/source/interfaces/int25/i25main_slid.F
      !||    i25main_tri                      ../engine/source/interfaces/intsort/i25main_tri.F
      !||    i25maind_2                       ../engine/source/interfaces/int25/i25maind_2.F
      !||    i25mainf                         ../engine/source/interfaces/int25/i25mainf.F
      !||    i25prep_nindex                   ../engine/source/interfaces/int25/i25slid.F
      !||    i25prep_send                     ../engine/source/interfaces/int25/i25slid.F
      !||    i25prep_sizbufs                  ../engine/source/interfaces/int25/i25slid.F
      !||    i25sors                          ../starter/source/interfaces/inter3d1/i25sors.F
      !||    i25stsecnd                       ../starter/source/interfaces/inter3d1/i25stslav.F
      !||    i25tagn                          ../engine/source/interfaces/int25/i25norm.F
      !||    i2_dtn                           ../starter/source/interfaces/inter3d1/i2_dtn.F
      !||    i2_dtn_27                        ../starter/source/interfaces/inter3d1/i2_dtn_27.F
      !||    i2_dtn_28                        ../starter/source/interfaces/inter3d1/i2_dtn_28.F
      !||    i2_imp0                          ../engine/source/interfaces/interf/i2_imp0.F
      !||    i2_imp1                          ../engine/source/interfaces/interf/i2_imp1.F
      !||    i2_impd                          ../engine/source/interfaces/interf/i2_impd.F
      !||    i2_impi                          ../engine/source/interfaces/interf/i2_imp0.F
      !||    i2_impm                          ../engine/source/interfaces/interf/i2_imp1.F
      !||    i2_impr1                         ../engine/source/interfaces/interf/i2_imp1.F
      !||    i2_impr2                         ../engine/source/interfaces/interf/i2_imp1.F
      !||    i2main                           ../starter/source/interfaces/interf1/i2master.F
      !||    i2tid3                           ../starter/source/interfaces/inter3d1/i2tid3.F
      !||    i2vit27                          ../engine/source/interfaces/interf/i2vit27.F
      !||    i2vit28                          ../engine/source/interfaces/interf/i2vit28.F
      !||    i5ke3                            ../engine/source/interfaces/inter3d/i5ke3.F
      !||    i6main                           ../engine/source/interfaces/inter3d/i6main.F
      !||    i7fku3                           ../engine/source/interfaces/int07/i7ke3.F
      !||    i7forcf3                         ../engine/source/interfaces/int07/i7ke3.F
      !||    i7ke3                            ../engine/source/interfaces/int07/i7ke3.F
      !||    i7main_crit_tri                  ../engine/source/interfaces/intsort/i7main_crit_tri.F
      !||    i7main_lmult                     ../engine/source/interfaces/int07/i7main_lmult.F
      !||    i7main_opt_tri                   ../engine/source/interfaces/intsort/i7main_opt_tri.F
      !||    i7main_tri                       ../engine/source/interfaces/intsort/i7main_tri.F
      !||    i7mainf                          ../engine/source/interfaces/int07/i7mainf.F
      !||    i7mainfr                         ../engine/source/interfaces/int07/i7ke3.F
      !||    i7remnode                        ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    i7stsecnd                        ../starter/source/interfaces/inter3d1/i7stslav.F
      !||    i9main2                          ../engine/source/interfaces/int09/i9main2.F
      !||    i9main3                          ../engine/source/interfaces/int09/i9main3.F
      !||    i9wale                           ../engine/source/interfaces/int09/i9wale.F
      !||    id_mvini                         ../engine/source/airbag/monv_imp0.F
      !||    iddl_int                         ../engine/source/mpi/implicit/imp_fri.F
      !||    iddl_mint                        ../engine/source/implicit/imp_int_k.F
      !||    idel_int                         ../engine/source/implicit/ind_glob_k.F
      !||    ielof2                           ../engine/source/interfaces/interf/ielof2.F
      !||    imp3_a2b                         ../engine/source/airbag/monv_imp0.F
      !||    imp3_u2x                         ../engine/source/airbag/monv_imp0.F
      !||    imp_buck                         ../engine/source/implicit/imp_buck.F
      !||    imp_chkm                         ../engine/source/implicit/imp_solv.F
      !||    imp_compab                       ../engine/source/implicit/imp_solv.F
      !||    imp_compabp                      ../engine/source/implicit/imp_solv.F
      !||    imp_diags                        ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_diagsn                       ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_dtkin                        ../engine/source/implicit/imp_int_k.F
      !||    imp_dykv                         ../engine/source/implicit/imp_dyna.F
      !||    imp_dykv0                        ../engine/source/implicit/imp_dyna.F
      !||    imp_fr7i                         ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_frfv                         ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_fri                          ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_frkd                         ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_frki                         ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_frsn                         ../engine/source/mpi/implicit/imp_fri.F
      !||    imp_i10mainf                     ../engine/source/interfaces/int10/i10ke3.F
      !||    imp_i11mainf                     ../engine/source/interfaces/int11/i11ke3.F
      !||    imp_i7mainf                      ../engine/source/interfaces/int07/i7ke3.F
      !||    imp_icomcrit                     ../engine/source/implicit/imp_int_k.F
      !||    imp_inisi                        ../engine/source/implicit/imp_pcg.F
      !||    imp_inist                        ../engine/source/implicit/imp_pcg.F
      !||    imp_int_k                        ../engine/source/implicit/imp_int_k.F
      !||    imp_intbuf                       ../engine/share/modules/imp_mod_def.F
      !||    imp_intdt                        ../engine/source/implicit/imp_int_k.F
      !||    imp_intfr                        ../engine/source/implicit/imp_solv.F
      !||    imp_inttd0                       ../engine/source/implicit/imp_int_k.F
      !||    imp_k_eig                        ../engine/stub/imp_k_eig.F
      !||    imp_lanzp                        ../engine/source/implicit/imp_lanz.F
      !||    imp_pcgh                         ../engine/source/implicit/imp_pcg.F
      !||    imp_ppcgh                        ../engine/source/implicit/imp_pcg.F
      !||    imp_sol_init                     ../engine/source/implicit/imp_sol_init.F
      !||    imp_solv                         ../engine/source/implicit/imp_solv.F
      !||    imp_tripi                        ../engine/source/implicit/imp_int_k.F
      !||    imp_updst                        ../engine/source/implicit/imp_pcg.F
      !||    imp_updv2                        ../engine/source/implicit/imp_pcg.F
      !||    ind_frkd                         ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_glob_k                       ../engine/source/implicit/ind_glob_k.F
      !||    ind_int_k                        ../engine/source/implicit/ind_glob_k.F
      !||    ind_kine_i                       ../engine/source/implicit/ind_glob_k.F
      !||    ind_kine_k                       ../engine/source/implicit/ind_glob_k.F
      !||    ind_kinefr                       ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_kinfrk                       ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_dd0                          ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_ddfv                         ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_dofspc                       ../engine/source/implicit/upd_glob_k.F
      !||    ini_kinkn                        ../engine/source/implicit/imp_int_k.F
      !||    ini_kinmv                        ../engine/source/airbag/monv_imp0.F
      !||    iniend                           ../starter/source/interfaces/inter3d1/iniend.F
      !||    iniend2d                         ../starter/source/interfaces/inter3d1/iniend.F
      !||    inint2                           ../starter/source/interfaces/inter2d1/inint2.F
      !||    inint3                           ../starter/source/interfaces/inter3d1/inint3.F
      !||    inint3_thkvar                    ../starter/source/interfaces/inter3d1/inint3_thkvar.F
      !||    inintmass                        ../starter/source/interfaces/inter3d1/inintmass.F
      !||    inintr                           ../starter/source/interfaces/interf1/inintr.F
      !||    inintr1                          ../starter/source/interfaces/interf1/inintr1.F
      !||    inintr2                          ../starter/source/interfaces/inter3d1/inintr2.F
      !||    inintr_orthdirfric               ../starter/source/interfaces/interf1/inintr_orthdirfric.F
      !||    inintr_thkvar                    ../starter/source/interfaces/interf1/inintr_thkvar.F
      !||    inintsub                         ../starter/source/interfaces/interf1/inintsub.F
      !||    inintsub_11                      ../starter/source/output/subinterface/inintsub_11.F
      !||    inintsub_25                      ../starter/source/output/subinterface/inintsub_25.F
      !||    inintsub_7                       ../starter/source/output/subinterface/inintsub_7.F
      !||    init_i25_edge                    ../engine/source/interfaces/int25/init_i25_edge.F
      !||    init_interf_sorting_strategy     ../engine/source/interfaces/init_interf_sorting_strategy.F
      !||    init_nodal_state                 ../engine/source/interfaces/interf/init_nodal_state.F
      !||    initia                           ../starter/source/elements/initia/initia.F
      !||    int12w                           ../engine/source/ale/inter/int12w.F
      !||    int18_law151_nsv_shift           ../engine/source/interfaces/int18/int18_law151_nsv_shift.F
      !||    int2_imp2                        ../engine/source/interfaces/interf/i2_imp2.F
      !||    int2cy_chk                       ../starter/source/constraints/general/bcs/lecbcscyc.F
      !||    int2modif_nd                     ../starter/source/elements/solid/solide10/dim_s10edg.F
      !||    int2poff                         ../engine/source/interfaces/interf/int2poff.F
      !||    int2poffh                        ../engine/source/interfaces/interf/int2poff.F
      !||    int2rupt                         ../engine/source/interfaces/interf/int2rupt.F
      !||    int8_ini                         ../starter/source/interfaces/intbuf/intbuf_ini_starter.F
      !||    int_fku3                         ../engine/source/implicit/imp_int_k.F
      !||    int_matv                         ../engine/source/implicit/imp_int_k.F
      !||    int_matvp                        ../engine/source/implicit/imp_int_k.F
      !||    intal1                           ../engine/source/ale/inter/intal1.F
      !||    intal2                           ../engine/source/ale/inter/intal2.F
      !||    intal3                           ../engine/source/ale/inter/intal3.F
      !||    intal4                           ../engine/source/ale/inter/intal4.F
      !||    intbuf_ini                       ../common_source/interf/intbuf_ini.F
      !||    intbuf_ini_starter               ../starter/source/interfaces/intbuf/intbuf_ini_starter.F
      !||    intbuf_mod                       ../engine/share/modules/restart_mod.F
      !||    intbuf_tab_c_ini                 ../common_source/interf/copy_intbuf_tab.F
      !||    intcrit                          ../engine/source/interfaces/intsort/intcrit.F
      !||    inter_check_sort                 ../engine/source/interfaces/generic/inter_check_sort.F
      !||    inter_color_coarse_voxel         ../engine/source/interfaces/generic/inter_color_coarse_voxel.F
      !||    inter_color_voxel                ../engine/source/interfaces/generic/inter_color_voxel.F
      !||    inter_count_node_curv            ../engine/source/interfaces/generic/inter_count_node_curv.F
      !||    inter_deallocate_wait            ../engine/source/interfaces/generic/inter_deallocate_wait.F
      !||    inter_minmax_node                ../engine/source/interfaces/generic/inter_minmax_node.F
      !||    inter_offset_itag                ../starter/source/elements/shell/shell_offset/inter_offset_itag.F90
      !||    inter_prepare_sort               ../engine/source/interfaces/generic/inter_prepare_sort.F
      !||    inter_sort                       ../engine/source/interfaces/generic/inter_sort.F
      !||    inter_sort_07                    ../engine/source/interfaces/int07/inter_sort_07.F
      !||    inter_struct_init                ../engine/source/interfaces/generic/inter_struct_init.F
      !||    inter_trc_7                      ../engine/source/interfaces/int07/inter_trc_7.F
      !||    inter_voxel_creation             ../engine/source/interfaces/generic/inter_voxel_creation.F
      !||    intfop1                          ../engine/source/interfaces/interf/intfop1.F
      !||    intfop2                          ../engine/source/interfaces/interf/intfop2.F
      !||    intfop8                          ../engine/source/interfaces/interf/intfop8.F
      !||    intmass_update                   ../engine/source/interfaces/interf/intmass_update.F
      !||    intti0                           ../engine/source/interfaces/interf/intti0.F
      !||    intti1                           ../engine/source/interfaces/interf/intti1.F
      !||    intti12a                         ../engine/source/interfaces/interf/intti12.F
      !||    intti12f                         ../engine/source/interfaces/interf/intti12.F
      !||    intti12v                         ../engine/source/interfaces/interf/intti12.F
      !||    intti2                           ../engine/source/interfaces/interf/intti2.F
      !||    intti2f                          ../engine/source/interfaces/interf/intti2f.F
      !||    intti2v                          ../engine/source/interfaces/interf/intti2v.F
      !||    inttri                           ../engine/source/interfaces/intsort/inttri.F
      !||    intvo2                           ../engine/source/interfaces/inter2d/intvo2.F
      !||    intvo3                           ../engine/source/interfaces/inter3d/intvo3.F
      !||    intvo8                           ../engine/source/interfaces/inter3d/intvo8.F
      !||    ipari_l_ini                      ../starter/source/restart/ddsplit/ipari_l_ini.F
      !||    itagsl12                         ../starter/source/interfaces/inter3d1/inintr2.F
      !||    itagsl2                          ../starter/source/interfaces/inter3d1/itagsl2.F
      !||    iwcontdd_type24                  ../starter/source/spmd/domain_decomposition/iwcontdd_type24.F
      !||    iwcontdd_type25                  ../starter/source/spmd/domain_decomposition/iwcontdd_type25.F
      !||    kin_kml                          ../engine/source/mpi/implicit/imp_fri.F
      !||    kin_knl                          ../engine/source/implicit/imp_int_k.F
      !||    kin_ksl                          ../engine/source/mpi/implicit/imp_fri.F
      !||    lag_i2main                       ../engine/source/tools/lagmul/lag_i2main.F
      !||    lag_mult                         ../engine/source/tools/lagmul/lag_mult.F
      !||    lag_multp                        ../engine/source/tools/lagmul/lag_mult.F
      !||    lagm_ini                         ../starter/source/tools/lagmul/lagm_ini.F
      !||    lecins                           ../starter/source/interfaces/interf1/lecins.F
      !||    lectur                           ../engine/source/input/lectur.F
      !||    lgmini_i2                        ../starter/source/tools/lagmul/lgmini_i2.F
      !||    lgmini_i7                        ../starter/source/tools/lagmul/lgmini_i7.F
      !||    lin_solv                         ../engine/source/implicit/lin_solv.F
      !||    lin_solvh0                       ../engine/source/implicit/lin_solv.F
      !||    lin_solvh1                       ../engine/source/implicit/lin_solv.F
      !||    lin_solvhm                       ../engine/source/implicit/lin_solv.F
      !||    lin_solvih2                      ../engine/source/implicit/lin_solv.F
      !||    ltag_i2main                      ../engine/source/tools/lagmul/lag_ntag.F
      !||    mav_lt2                          ../engine/source/implicit/produt_v.F
      !||    mav_lth                          ../engine/source/implicit/produt_v.F
      !||    mav_lth0                         ../engine/source/implicit/produt_v.F
      !||    mav_ltp                          ../engine/source/implicit/produt_v.F
      !||    mergemod                         ../starter/share/modules1/merge_mod.F
      !||    mmav_lth                         ../engine/source/implicit/produt_v.F
      !||    monv_diag                        ../engine/source/airbag/monv_imp0.F
      !||    monv_imp                         ../engine/source/airbag/monv_imp0.F
      !||    monv_m3                          ../engine/source/airbag/monv_imp0.F
      !||    monv_prem                        ../engine/source/airbag/monv_imp0.F
      !||    mpp_init                         ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    mv_matv                          ../engine/source/airbag/monv_imp0.F
      !||    nddli_ns                         ../engine/source/mpi/implicit/imp_fri.F
      !||    nl_solv                          ../engine/source/implicit/nl_solv.F
      !||    pre_i2                           ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    prepare_int25                    ../starter/source/model/mesh/build_cnel.F
      !||    prepare_split_cand               ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_cand_i20_edge      ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_cand_i21           ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_cand_i25_edge      ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i11                ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i17                ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i2                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i20                ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i21                ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i24                ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i25                ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i25e2e             ../starter/source/spmd/prepare_split_i25e2e.F
      !||    prepare_split_i7                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i8                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    prepare_split_i9                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    prescrint                        ../starter/source/interfaces/interf1/prescrint.F
      !||    print_stif                       ../engine/source/implicit/imp_solv.F
      !||    printime_interf                  ../engine/source/system/timer_interf.F
      !||    r2r_clean_inter                  ../starter/source/coupling/rad2rad/r2r_clean_inter.F
      !||    rdcomi                           ../engine/source/output/restart/rdcomm.F
      !||    recukin                          ../engine/source/implicit/recudis.F
      !||    remn_i2_edg                      ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2_edgop                    ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2op                        ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2op_edg25                  ../starter/source/interfaces/int25/i25remlin.F
      !||    remn_self24                      ../starter/source/interfaces/inter3d1/remn_self24.F
      !||    rer02                            ../engine/source/implicit/upd_glob_k.F
      !||    rer_int_v                        ../engine/source/implicit/upd_glob_k.F
      !||    reset_gap                        ../starter/source/interfaces/interf1/reset_gap.F
      !||    resol                            ../engine/source/engine/resol.F
      !||    resol_head                       ../engine/source/engine/resol_head.F
      !||    resol_init                       ../engine/source/engine/resol_init.F
      !||    ri2_int24p_ini                   ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    s10cndi2_ini                     ../engine/source/elements/solid/solide10/s10cndf.F
      !||    scrint                           ../starter/source/interfaces/interf1/scrint.F
      !||    secnd_surface_on_domain          ../starter/source/interfaces/inter3d1/i24setnodes.F
      !||    set_front8                       ../starter/source/spmd/node/ddtools.F
      !||    set_intercep                     ../starter/source/spmd/node/ddtools.F
      !||    sms_build_mat_2                  ../engine/source/ams/sms_build_mat_2.F
      !||    sms_ini_int                      ../engine/source/ams/sms_init.F
      !||    sms_ini_jad_1                    ../engine/source/ams/sms_init.F
      !||    sms_ini_jad_2                    ../engine/source/ams/sms_init.F
      !||    sms_ini_jad_3                    ../engine/source/ams/sms_init.F
      !||    sms_ini_kdi                      ../engine/source/ams/sms_init.F
      !||    sms_ini_kin_1                    ../engine/source/ams/sms_init.F
      !||    sortie_main                      ../engine/source/output/sortie_main.F
      !||    split_2ry_cand_ival_i21          ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_adskyn_25                  ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i11                   ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i20                   ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i20_edge              ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i24                   ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i25                   ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i25_edge              ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_i7                    ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_ival                  ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_ival_i21              ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_cand_rval                  ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_interfaces                 ../starter/source/restart/ddsplit/split_interfaces.F
      !||    split_isegpt_ival                ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_lbound_i25                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_ledge_i25                  ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_nisub_i25                  ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_nisub_i7                   ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_node_ival                  ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_node_ival2                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_node_ival_i24              ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_node_ival_i25              ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_node_nodloc                ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_node_nodloc_p0             ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_node_rval                  ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_remnode_i11                ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_remnode_i24                ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_remnode_i25                ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_remnode_i25_e2s            ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_remnode_i25_edge           ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_remnode_i7                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_seg_ival2                  ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_seg_ival_i20               ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_seg_ival_i20_2             ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_seg_nodloc                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_seg_nodloc_i24             ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_seg_rval_i20               ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_seg_segloc                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_segedge_nodloc_i24         ../starter/source/restart/ddsplit/inter_tools.F
      !||    split_xsav                       ../starter/source/restart/ddsplit/inter_tools.F
      !||    spmd_cell_list_exchange          ../engine/source/mpi/interfaces/spmd_cell_list_exchange.F
      !||    spmd_cell_size_exchange          ../engine/source/mpi/interfaces/spmd_cell_size_exchange.F
      !||    spmd_check_tag                   ../engine/source/mpi/ams/spmd_check_tag.F
      !||    spmd_exch_da20                   ../engine/source/mpi/interfaces/spmd_exch_da20.F
      !||    spmd_exch_deleted_surf_edge      ../engine/source/mpi/interfaces/spmd_exch_deleted_surf_edge.F
      !||    spmd_exch_efric                  ../engine/source/mpi/interfaces/spmd_exch_efric.F
      !||    spmd_exch_i24                    ../engine/source/mpi/interfaces/spmd_exch_i24.F
      !||    spmd_exch_i25                    ../engine/source/mpi/interfaces/spmd_exch_i25.F
      !||    spmd_exch_idel_seglo             ../engine/source/mpi/interfaces/spmd_exch_idel_seglo.F
      !||    spmd_exch_inter_18               ../engine/source/mpi/interfaces/spmd_exch_inter_18.F
      !||    spmd_exch_nor                    ../engine/source/mpi/interfaces/spmd_exch_nor.F
      !||    spmd_exch_press                  ../engine/source/mpi/interfaces/spmd_exch_press.F
      !||    spmd_exch_smst2                  ../engine/source/mpi/ams/spmd_exch_smst2.F
      !||    spmd_exch_sorting_efric          ../engine/source/mpi/interfaces/spmd_exch_sorting_efric.F
      !||    spmd_glob_min5                   ../engine/source/mpi/generic/spmd_glob_min5.F
      !||    spmd_i18kine_com_a               ../engine/source/mpi/interfaces/spmd_i18kine_com_a.F
      !||    spmd_i18kine_com_acc             ../engine/source/mpi/interfaces/spmd_i18kine_com_acc.F
      !||    spmd_i18kine_com_ms              ../engine/source/mpi/interfaces/spmd_i18kine_com_ms.F
      !||    spmd_i18kine_com_v               ../engine/source/mpi/interfaces/spmd_i18kine_com_v.F
      !||    spmd_i18kine_pene_com_poff       ../engine/source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
      !||    spmd_i21fthecom                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i21tempcom                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i24_prepare                 ../engine/source/interfaces/int24/i24_prepare.F
      !||    spmd_i25_prepare                 ../engine/source/interfaces/int25/i25_prepare.F
      !||    spmd_i25_slide_gat               ../engine/source/mpi/interfaces/spmd_i25slide.F
      !||    spmd_i25front_init               ../engine/source/mpi/interfaces/spmd_i25front.F
      !||    spmd_i25front_nor                ../engine/source/mpi/interfaces/spmd_i25front.F
      !||    spmd_i7fcom_poff                 ../engine/source/mpi/forces/spmd_i7fcom_poff.F
      !||    spmd_i7fcom_pon                  ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||    spmd_i7itied_cand                ../engine/source/mpi/interfaces/spmd_i7itied_cand.F
      !||    spmd_i7xvcom2                    ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
      !||    spmd_ifront                      ../engine/source/mpi/interfaces/spmd_ifront.F
      !||    spmd_ifront_stamp                ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_int18_law151_pon            ../engine/source/mpi/forces/spmd_int18_law151_pon.F
      !||    spmd_savefi                      ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_split_comm_inter            ../engine/source/mpi/interfaces/spmd_split_comm_inter.F
      !||    spmd_wait_nb                     ../engine/source/mpi/interfaces/spmd_wait_nb.F
      !||    st_qaprint_driver                ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_interfaces            ../starter/source/output/qaprint/st_qaprint_interfaces.F
      !||    ud_int5                          ../engine/source/implicit/upd_glob_k.F
      !||    upd_fr                           ../engine/source/mpi/implicit/imp_fri.F
      !||    upd_glob_k                       ../engine/source/implicit/upd_glob_k.F
      !||    upd_int_k                        ../engine/source/implicit/upd_glob_k.F
      !||    upd_kml                          ../engine/source/mpi/implicit/imp_fri.F
      !||    upd_ksl                          ../engine/source/mpi/implicit/imp_fri.F
      !||    upd_rhs                          ../engine/source/implicit/upd_glob_k.F
      !||    upd_rhs_fr                       ../engine/source/implicit/imp_solv.F
      !||    update_weight_inter_type_24_25   ../starter/source/spmd/domain_decomposition/update_weight_inter_type_24_25.F
      !||    updk_mv                          ../engine/source/airbag/monv_imp0.F
      !||    upgrade_cand_opt                 ../common_source/interf/upgrade_multimp.F
      !||    upgrade_lcand_e2s                ../common_source/interf/upgrade_multimp.F
      !||    upgrade_lcand_edg                ../common_source/interf/upgrade_multimp.F
      !||    upgrade_multimp                  ../common_source/interf/upgrade_multimp.F
      !||    upgrade_remnode                  ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||    upgrade_remnode2                 ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||    upgrade_remnode_e2s              ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||    upgrade_remnode_edg              ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||    upgrade_remnode_edg2             ../starter/source/interfaces/interf1/upgrade_remnode.F
      !||    w_fi                             ../starter/source/restart/ddsplit/w_fi.F
      !||    w_front                          ../starter/source/restart/ddsplit/w_front.F
      !||    w_intbuf_size                    ../starter/source/restart/ddsplit/inter_tools.F
      !||    w_pon                            ../starter/source/restart/ddsplit/w_pon.F
      !||    w_type8                          ../starter/source/restart/ddsplit/split_interfaces.F
      !||    wrcomi                           ../engine/source/output/restart/wrcomm.F
      !||    wrcomip                          ../starter/source/restart/ddsplit/wrcommp.F
      !||    write_intbuf                     ../engine/source/output/restart/write_intbuf.F
      !||    wrrestp                          ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    int8_mod                         ../common_source/modules/interfaces/int8_mod.F
      !||    metric_mod                       ../common_source/modules/metric_mod.F
      !||====================================================================
      MODULE INTBUFDEF_MOD
C----------------------------------------------------------------------- 
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE INT8_MOD
      USE METRIC_MOD


#include "my_real.inc"      
c
C=======================================================================
      TYPE INTBUF_STRUCT_
C=================================================  
c define typeINTBUF_STRUCT_ for new interface buffer INTBUF_TAB
c module is organized as following :
c   - define integer arrays sizes
c   - define float arrays sizes
c   - define integer arrays
c   - define float arrays

C=======================================================================
c DEFINE SIZES (integers arrays)
C=======================================================================
        integer ::   S_IRECTS     !  4*NRTS     :IRECTS:CONNECTIVITES FACES SECONDS     :1,2,3,4
        integer ::   S_IRECTM     !  4*NRTM_FE+4*9*NRTM_IGE :IRECTM:CONNECTIVITES FACES MAIN  :1,2,3,4,5,6,7,8,20,24
        integer ::   S_NSV        !  NSN        :NSV   :NOEUDS SECONDS                  :1,2,3,4,  7 
        integer ::   S_MSR        !  NMN+16*NRTM_IGE  :MSR   :NOEUDS MAINS                   :1,2,3,4
        integer ::   S_IRTLM      !  NSN        :FACE MAIN LA PLUS PROCHE    :1,2,3,4   
        integer ::   S_IRUPT      !  NSN        :IRUPT :Flag rupture                     :2
        integer ::   S_INORM      !  NSN        :INORM  :main orientation flag: type2 avec rupture
        integer ::   S_IELEC      !  (type20)
        integer ::   S_IELES      !  NRTS       :IELES :ELEMENT SECND                    :              9
        integer ::   S_LISUB      !  NISUB      :LISUB :LISTE DES SOUS-INTERFACES        :     7
        integer ::   S_TYPSUB     !  NISUB      :TYPSUB :TYPE OF SUBINTERFACES           :     25
        integer ::   S_ADDSUBS    !  NSN+1      :ADDSUBS:ADRESSE DS ZONE DE SS INTERF. COTE SECOND. :     7, 10, 24, 25  
        integer ::   S_ADDSUBM    !  NRTM+1     :ADDSUBM:ADRESSE DS ZONE DE SS INTERF. COTE MAIN  :     7, 10, 24, 25
        integer ::   S_LISUBS     !  NISUBS     :LISUBS :ZONE DES SS INTERF. COTE SECOND.           :     7, 10, 24, 25
        integer ::   S_LISUBM     !  NISUBM     :LISUBM :ZONE DES SS INTERF. COTE MAIN            :     7, 10, 24, 25
        integer ::   S_INFLG_SUBS !  NISUBS     :INFLG_SUBS: ZONE D APPARTENANCE A S1/S2 DES SS INTERF. COTE SECOND. : 25
        integer ::   S_INFLG_SUBM !  NISUBS     :INFLG_SUBS: ZONE D APPARTENANCE A S1/S2 DES SS INTERF.COTE MAIN   : 25
        integer ::   S_MSEGTYP    !  NRTM       :MSEGTYP: element type                   :      2,3,5,7,10,21,23; used only in Starter
        integer ::   S_CAND_E     !  MULTIMP*NSN:CAND_E:FACETTES CANDIDATES              :7,10,11,20   
        integer ::   S_CAND_N     !  MULTIMP*NSN:CAND_N:NOEUDS CANDIDATS                 :7,10,11,20  
        integer ::   S_I_STOK     !             : II_STOK                                : 7    10,11,20,24
        integer ::   S_I_STOK_E   !             : II_STOK_E                              : 7    10,11,20,24
        integer ::   S_IFPEN      !  MULTIMP*NSN:IFPEN :FLAG PENETR. (filtr frottement)  :           7
        integer ::   S_KREMNODE   !  NRTM+1     :KREMNODE:ADRESS NODES REMOVED FROM CONTACT IN REMNODE ARRAY :      7
        integer ::   S_REMNODE    !  NREMNODE   :REMNODE: NODES REMOVED FROM CONTACT     :      7
        integer ::   S_ADCCM      !  NRTM       :ADDCM :adresse dans CHAINE du 1er main:                  11
        integer ::   S_CHAIN      !  2*MULTIMP*NSN:CHAINE(1,adds) : bord second.     :                  11
!type20
        integer ::   S_DAANC6     !  18*2*NLN   :DAANC6:contient 18*NLN double precision : 20       
        integer ::   S_NBINFLG    !  NBINFLG    :NLN    :Flags binaire noeuds            NBIN:      20, 24, 25
        integer ::   S_MBINFLG    !  MBINFLG    :NRTM  :Flag multiusage facettes        :      20, 24, 25
        integer ::   S_EBINFLG    !  EBINFLG    :NEDGE :Flag multiusage edges           :      25
        integer ::   S_NLG        !  NLN        :NLG   :LOCAL TO GLOBAL NODE             :      20
        integer ::   S_ISLINS     !  2*NLINS    :ISLINS:SURFACE SECOND. et COTE          :      20
        integer ::   S_ISLINM     !  2*NLINM    :ISLINM:SURFACE MAIN et COTE           :      20
        integer ::   S_IXLINS     !  2*NLINS    :IXLINS:CONNECTIVITES BORDS SECONDS     :      20
        integer ::   S_IXLINM     !  2*NLINM    :IXLINM:CONNECTIVITES BORDS MAINS      :      20
        integer ::   S_NSVL       !  NSNE       :NSVL  :NOEUDS SECONDS DES LIGNES       :      20
        integer ::   S_MSRL       !  NMNE       :MSRL  :NOEUDS MAINS                   :      20
        integer ::   S_LCAND_N    !  MULTIMP*NMNE:LCAND_N:LIGNES MAINS CANDIDATES      :      20
        integer ::   S_LCAND_S    !  MULTIMP*NSNE:LCAND_N:LIGNES SECONDS CANDIDATES     :      20
        integer ::   S_ADCCM20    !  NLINM      :ADDCM :adresse dans CHAINE du 1er main:      20
        integer ::   S_CHAIN20    !  2*MULTIMP*NSNE:CHAINE(1,adds) : bord second.        :      20            11                                  !                       :CHAINE(2,adds) : adresse suivante       :      20  
!type1
        integer ::   S_ILOCS      !  NSN        :ILOCS :NOEUD MAIN LE PLUS PROCHE      :1, ,3,4
        integer ::   S_NSEGM      !  1+NMN      :NSEGM :ADRESSE DU VECTEUR LMSR (MAIN) :1, ,3,4,5
        integer ::   S_NRT        !  NRT        :LMSR  :FACES CONNECTEES AU NOEUD MAIN :1, ,3,4,5
!type2
        integer ::   S_MSEGTYP2   !  :NRTM      :MSEGTYP: main segment type sol/shell  :2
        integer ::   S_CSTS_BIS   !  :NRTM      :MSEGTYP: main segment type sol/shell  :2
!type3
        integer ::   S_IRTLS      !  NMN        :IRTLS :FACE SECOND. LA PLUS PROCHE      :    3,4
        integer ::   S_ILOCM      !  NMN        :ILOCM :NOEUD SECOND. LE PLUS PROCHE     :    3,4
        integer ::   S_IRTLOM     !  NSN        :IRTLOM:ANC. FACE MAIN. LA PLUS PROCHE  :    3,4
        integer ::   S_IRTLOS     !  NMN        :IRTLOS:ANC. FACE SECND. LA PLUS PROCHE  :    3,4
        integer ::   S_NSEGS      !  1+NSN      :NSEGS :ADRESSE DU VECTEUR LNSV (SECOND):    3,4,5
        integer ::   S_LNSV       !  NRT        :LMSR  :FACES CONNECTEES AU NOEUD MAIN :1, ,3,4,5
        integer ::   S_LMSR       !  NRT        :LMSR  :FACES CONNECTEES AU NOEUD MAIN :1, ,3,4,5
!type4
        integer ::   S_IELEM      !  NRTM       :IELEM :ELEMENT main                   :              9
!type12
        integer ::   S_FCOUNT     !  NSN: 12
!type14
        integer ::   S_KSURF      !  
        integer ::   S_IMPACT     !  NSN 
!type21
        integer ::   S_MSR21      !  NMNG    :MSR21: MSR global     :      21
        integer ::   S_MNDD       
        integer ::   S_MSR_L      
!type24
        integer ::   S_MVOISIN    !  4*NRTM     :MVOISIN: facettes mains voisines:(at least two commun nodes) : 24
        integer ::   S_NVOISIN    !  2*4*NRTM   :NVOISIN: Noeuds mains voisines      :24
        integer ::   S_MSEGLO     !  NRTM       :MSEGLO: global segment number (spmd)    :24, 25
        integer ::   S_MSEGTYP24  !  NRTM       :MSEGTYP: shell segment type             :24
!type25
        integer ::   S_EVOISIN    !  4*NRTM            :EVOISIN: No edge voisine
        integer ::   S_ADMSR      !  4*NRTM            :ADMSR  : adresse des normales aux noeuds mains : 25
        integer ::   S_LEDGE      !  4*NEDGE           :LEDGE  : description des edges : 25
        integer ::   S_LBOUND     !  NADMSR            :LBOUND : Index des sommets sur les aretes libres : 25
        integer ::   S_ACTNOR     !  NADMSR            :ACTNOR : tag des normales actives (0/1) : 25
        integer ::   S_FARM       !  4*MULTIMP*NSN     :FARM   :Flag In/out wrt 4 sub-triangles : 25
        integer ::   S_ADSKYN     !  4*NRTM+1          :ADSKYN : skyline pour assemblage parith/on des normales : 25
        integer ::   S_IADNOR     !  4*NRTM            :IADNOR : skyline pour assemblage parith/on des normales : 25
        integer ::   S_ISLIDE     !  4*NSN             :ISLIDE : sommets (0  4) sur lesquels le noeud secnd glisse : 25
        integer ::   S_KNOR2MSR   !  NADMSR+1          :KNOR2MSR: adress of connected segments to normals in NOR2MSR : 25
        integer ::   S_NOR2MSR    !  ...               :NOR2MSR : connected segments to normals in NOR2MSR : 25
        integer ::   S_CAND_OPT_N !  MULTIMP*NSN       :CAND_OPT_N : Candidats apres optimisation <=> nd second. (i25optcd.F)
        integer ::   S_CAND_OPT_E !  MULTIMP*NSN       :CAND_OPT_E : Candidats apres optimisation <=> segment main (i25optcd.F)
        integer ::   S_IF_ADH     !  NSN               :IF_ADH: if adhesion spring exists (1) or not (0)  :  25
        integer ::   S_CANDM_E2E  !  MULTIMP*NCONTE    :CANDM_E2E : Main line : Aretes candidates mains (shell & beams))
        integer ::   S_CANDS_E2E  !  MULTIMP*NCONTE    :CANDS_E2E : Secnd line : Aretes candidates secnds 
        integer ::   S_CANDM_E2S  !  MULTIMP*NCONTE    :CANDM_E2S : Main segment : Facettes candidates mains (solid edges)
        integer ::   S_CANDS_E2S  !  MULTIMP*NCONTE    :CANDS_E2S : Secnd line : Aretes candidates secnds 
        integer ::   S_CANDL_MAX 
        integer ::   S_CANDS_MAX 
        integer ::   S_ADDSUBE    !  NEDGE+1     :ADDSUBES:ADRESSE DS ZONE EDGES DE SS INTERF. COTE SECOND.             : 25   
        integer ::   S_LISUBE     !  NISUBE      :LISUBE  :ZONE EDGES DES SS INTERF. COTE SECOND.                       : 25
        integer ::   S_INFLG_SUBE !  NISUBE      :INFLG_SUBE :: ZONE D APPARTENANCE A S1/S2 DES SS INTERF.COTE MAIN   : 25
C-------S_ISEADD,S_ISEDGE,S_CAND_T will be cleaned after, for the moment with size 0
        integer ::   S_ISEADD     !  ISEADD     :SECND EDGE ADDRESS                      :      24
        integer ::   S_ISEDGE     !  L24ADD     :ISEDGE:SECND EDGES NODES AND FLAGS      :      24
        integer ::   S_CAND_T     !  MULTIMP*NSN:CAND_T:Candidate type                   :      24
c            ! =0 node to surface candidate
c            ! =1 node or edge to surface candidate
c            ! =2 edge to surface candidate
        integer ::   S_ISEG_PXFEM !  ISEG_PXFEM:       :      24
        integer ::   S_ISEG_PLY   !  ISEG_PLY          :      24

        integer ::   S_ICONT_I    !  NSN not detected pene_ini <0 :   24
C---- edge        
        integer ::   S_IRTSE      !  (5,NRTSE)
        integer ::   S_IS2SE      !  (2,NSNE)
        integer ::   S_IS2PT      !  (NSNE)
        integer ::   S_ISPT2      !  (NSN) - tag for irtlm on Fictive node
        integer ::   S_ISEGPT     !  (NSN)
        integer ::   S_IS2ID      !  (NSNE)
        integer ::   S_IFPEN_E    !  (MULTIMPE*NCONTE) : filter candidat friction       :25
        integer ::   S_IFPEN_E2S  !  (MULTIMPE*NCONTE) : filter candidat friction       :25
C---- ige
        integer ::   S_NIGE
C---- INTER FRICTION PARTS
        integer ::   S_IPARTFRICS!  NSN       :IPARTFRICS : number of part of secnd nodes 
        integer ::   S_IPARTFRICM!  NRTM      :IPARTFRICM : number of part of main segments
        integer ::   S_IPARTFRIC_E!  NEDGE    :IPARTFRIC_E : number of part of edge int25
C       Orthotropic friction 
        integer ::   S_IREP_FRICM !  NRTM      :IREP_FRIC : Orthotropic system formulation flag for reference vector for friction (same as IORTH/IREP for prop) 
C---- Irem Gap option for inter type25
        integer ::   S_KREMNOR!  NSN+1    :KREMNOR : skyline tab for main segment adress in REMNOR tab
        integer ::   S_REMNOR!   REMNODE  :REMNOR  : tab for main segment forbidden for each secnd node
        integer ::   S_KREMNODE_EDG   !  NEDGE+1    :KREMNODE_EDG:ADRESS LINES REMOVED FROM CONTACT IN REMNODE ARRAY :      25
        integer ::   S_REMNODE_EDG    !  NREMNODE_EDG   :REMNODE_EDG: LINES REMOVED FROM CONTACT     :      25
        integer ::   S_KREMNODE_E2S   !  NRTM+1    :KREMNODE_EDG:ADRESS LINES REMOVED FROM CONTACT IN REMNODE ARRAY :      25
        integer ::   S_REMNODE_E2S    !  NREMNODE_E2S   :REMNODE_EDG: LINES REMOVED FROM CONTACT     :      25
C----Nitsche Method
        integer ::   S_IELNRTS!  NRTS      :IELNRTS : number of secnd element 
        integer ::   S_ADRECTS!  4*NRTS    :ADRECTS : Adress of each secnd node of the segment in IXS/IXS10/IXS20/IXS16 
        integer ::   S_FACNRTS!  NRTS      :FACNRTS : Corresponding facet in element IELNRTS 
        integer ::   S_E2S_ACTNOR!
C=======================================================================
c DEFINE SIZES (float arrays)
C=======================================================================
        integer ::   S_STFAC        !  1          :STFAC :FACTEUR DE RIGIDITE D'INTERFACE(MANAGER)
        integer ::   S_VARIABLES    !   +1, ...
        integer ::   S_CSTS         !  2*NSN      :CSTS  :S ET T DES NOEUDS SECONDS       :   ,3,4,    
        integer ::   S_DPARA        !  7*NSN      :DPARA :DET,B1,B2,B3,C1,C2,C3            :  2
        integer ::   S_NMAS         !  NMN        :MMAS  :MASSE NOEUDS MAINS             :  2
        integer ::   S_SMAS         !  NSN      :SMAS : MASSE ET INERTIE DES NOEUDS SECND :   2  
        integer ::   S_SINER        !  NSN      :SINER : MASSE ET INERTIE DES NOEUDS SECND :   2
        integer ::   S_AREAS2       !  NSN       AREA       : secnd area                  :  int2 with rupture  
        integer ::   S_UVAR         !  NSN*NUVAR UVAR       : user buffer for secnds      :  int2 with rupture  
        integer ::   S_XM0          !  NSN*3     XM0(x,y,z) : secnd-main initial distance: int2 with rupture
        integer ::   S_SPENALTY     !  NSN      :
        integer ::   S_STFR_PENALTY !  NSN      :S
        integer ::   S_SKEW         !  ILEV=25  : NSN*3     XM0(x,y,z) : secnd-main initial distance: int2 with rupture
        integer ::   S_DSM          !  NSN*3     DSM(x,y,z) : secnd-main displacement    :  int2 with rupture
        integer ::   S_FSM          !  NSN*3     FSM(x,y,z) : secnd-main force           :  int2 with rupture
        integer ::   S_RUPT         !  6         RUPT       : penality/rupture parameters  :  int2 with rupture
        integer ::   S_FINI         !  ILEV=25   :3*NSN  :  int2 with rupture
        integer ::   S_STFNS        !  NSN        :STFNS :RIGIDITE NOEUDS SECONDS         :   ,3,4,    7
        integer ::   S_STFM         !  NRTM       :STFM  :RIGIDITE FACES MAIN        :   ,3,4,    7  10,11
        integer ::   S_STFS         !  NRTS       :STFS  :RIGIDITE FACES SECONDS          :   ,3,4          11
        integer ::   S_PENIM        !  2*NRTM     :PENIM :pene initiale max / main       :     20           11
        integer ::   S_PENIS        !  2*NRTS     :PENIS :pene initiale max / secnd        :                  11
        integer ::   S_GAP_M        !  NRTM       :GAP_M :GAP FACES MAIN             :(7)10,11,24
        integer ::   S_GAP_S        !  NSN        :GAP_S :GAP NOEUDS SECONDS              :(7)10,20
        integer ::   S_XSAV         !  3*MIN(NUMNOD,NSN+NMN):CRIT  :X(XYZ)                   :            7
        integer ::   S_CRIT         !  12         :CRIT  :MIN et MAX DEPLACEMENT (SPMD)    :            7 (10,11)
        integer ::   S_FRIC_P       !  10         :FRIC_P:FRICTION PARAMETERS              :        5,  7     24
        integer ::   S_XFILTR       !  1          :XFILTR:COEFF FILTRAGE FROTTEMENT     )  :        5,  7     24
        integer ::   S_AREAS        !  NSN       AREA       : secnd area                  :  7
        integer ::   S_AREAM        !  NRTM      AREA       : main area                  :  11
        integer ::   S_GAP_SL       !  NSN        :GAP_S_L :
        integer ::   S_GAP_ML       !  NRTM       :GAP_M_L :           
        integer ::   S_CAND_P       !  MULTIMP*NSN:CAND_P:PENETRATION INITIALE             :           (7)   
        integer ::   S_FTSAVX       !  MULTIMP*NSN:FTSAVX :    SAUVEGARDE FILTRAGE         :            7, 8  24
        integer ::   S_FTSAVY       !  MULTIMP*NSN:FTSAVY :    SAUVEGARDE FILTRAGE         :            7, 8  24
        integer ::   S_FTSAVZ       !  MULTIMP*NSN:FTSAVZ :    SAUVEGARDE FILTRAGE         :            7, 8  24
        integer ::   S_FTSAVX_E   !  MULTIMP*NCONTE    :FTSAVX_E : Friction for saving     :            25
        integer ::   S_FTSAVY_E   !  MULTIMP*NCONTE    :FTSAVX_E : Friction for saving     :            25 
        integer ::   S_FTSAVZ_E   !  MULTIMP*NCONTE    :FTSAVX_E : Friction for saving     :            25 
        integer ::   S_FTSAVX_E2S !  MULTIMP*NCONTE    :FTSAVX_E2S : Friction for saving   :            25 
        integer ::   S_FTSAVY_E2S !  MULTIMP*NCONTE    :FTSAVX_E2S : Friction for saving   :            25 
        integer ::   S_FTSAVZ_E2S !  MULTIMP*NCONTE    :FTSAVX_E2S : Friction for saving   :            25 
C---- ige
        integer ::   S_RIGE          
        integer ::   S_XIGE          
        integer ::   S_VIGE 
        integer ::   S_MASSIGE         
!type10
        integer ::   S_CAND_F       !  6*4*NSN    :CAND_F:ANCIENNE FORCES LOCALES +H1 A H3 :               10    
!type11
        integer ::   S_CAND_MAX 
!type20
        integer ::   S_CAND_FX      !  
        integer ::   S_CAND_FY      !  
        integer ::   S_CAND_FZ      !  
        integer ::   S_XA           !  3*NSN;3*NLN:XA    :XA DES NOEUDS                    :       20
        integer ::   S_VA           !  3*NSN;3*NLN:VA    :VA DES NOEUDS                    :       2
        integer ::   S_STFA         !  
        integer ::   S_PENIA        !  5*NLN      :PENIA :pene initiale points d'ancrages  :     20 
        integer ::   S_ALPHAK       !  3*NLN      :ALPHAK:reduction de rigidite d'ancrages :     20
        integer ::   S_GAP_SH       !  NRTM       :GAP_SH:Shift de gap pour gap nul solides:     20
        integer ::   S_AVX_ANCR     !  9*NLN :A-V-X-ANCR :Accel Vit deplacement POINT D'ANCRAGE: 20
        integer ::   S_CRITX        !  
        integer ::   S_GAP_ME       !  
        integer ::   S_STF          !  NLINM      :STFM  :RIGIDITE FACES MAIN        :       20
        integer ::   S_PENIME       !  2*NLINM    :PENIME:pene initiale max / edge main       :       20
        integer ::   S_GAP_SE       !   
        integer ::   S_PENISE       !  2*NLINS    :PENISE:pene initiale max / edge secnd   :       20
        integer ::   S_STFNE        !  NSNE       :STFNE :RIGIDITE NOEUDS SECONDS         :       20
!type1
        integer ::   S_N            !  3*NSN      :N     :NORMALE                          :1,
!type3,4,5,9
        integer ::   S_CSTM         !  2*NMN      :CSTM  :S ET T DES NOEUDS MAINS        :   ,3,4,
        integer ::   S_STFNM        !  NMN        :STFNM :RIGIDITE NOEUDS MAINS          :   ,3,4
        integer ::   S_FRICOS       !  3*NSN      :FRICOS:FORCE DE FRICTION(SECOND)       :   ,3,4,    
        integer ::   S_FRICOM       !  3*NMN      :FRICOM:FORCE DE FRICTION(MAIN)        :   ,3,4,   
        integer ::   S_FTSAV        !  3*NSN      :FTSAV :     SAUVEGARDE FILTRAGE         :        5         
!type6
        integer ::   S_FCONT        !  1          :FCONT :                                 :          6  
        integer ::   S_FS           !  3*NSN      :FS    :SAUVEGARDE DE LA PENETRATION     :          6
        integer ::   S_FM           !  3*NSN      :FM    :SAUVEGARDE DE LA PENETRATION     :          6
        integer ::   S_RMAS         !  2          :RMAS  :MASSE DES RIGID BODY             :          6
        integer ::   S_ANSMX0       
!type 8
        integer ::   S_T8          !   SIZE OF T8 Structure  (1 if Type8, 0 elsewhere)            :          8 
        integer ::   S_GAPN         !   NRTM
        integer ::   S_STF8         !   NRTM

!type 9
        integer ::   S_EE           !  EE    :ENERGIE DE FROTTEMENT            : 9

!type14
        integer ::   S_CIMP         !  3*NSN    :
        integer ::   S_NIMP         !  3*NSN    :
!type15
        integer ::   S_IOLD         !  
        integer ::   S_HOLD         !  
        integer ::   S_NOLD         !  
        integer ::   S_DOLD         !  
!type17
        integer ::   S_KS           !  
        integer ::   S_KM           !  
        integer ::   S_FROTS        !  
        integer ::   S_FROTM        !  
!type21
        integer ::   S_NOD_NORMAL   !  
        integer ::   S_RCURV        !  
        integer ::   S_ANGLM        !  
        integer ::   S_FROT_P       !  
        integer ::   S_ALPHA0       !  
        integer ::   S_AS           !  
        integer ::   S_BS           !  
        integer ::   S_THKNOD0      !  
!type22
!type23
!type24
        integer ::   S_GAPN_M       !  NMN        :GAPN_M:Nodal main gap                 :  24, 25 
        integer ::   S_SECND_FR      !  6*NSN      :SECND_FR:new(1:3) and old(4:6) friction F:  24  
        integer ::   S_PENE_OLD     !  NSN*5      :PENE_OLD:Old penetration,PEN_ini(Inacti=5):       24
        integer ::   S_STIF_OLD     !  NSN*2      :STIF_OLD:Old stifness                   :       24
        integer ::   S_TIME_S       !  NSN        :TIME_S :   impact TIME                  :  24
        integer ::   S_GAP_NM       !  12*NRTM          
C---------to be cleaned later
        integer ::   S_EDGE8L2      !  NSN        :EDGE8L2: max half edge length on secnd node: 24
        integer ::   S_NOD_2RY_LGTH !  NSN        :NODAL SECND LENGTH (TO CAL. Kg)         :     24  
        integer ::   S_NOD_MAS_LGTH !  NMN        :NODAL main LENGTH (TO CAL. Kg)        :     24  
        integer ::   S_GAP_N0       !  12*NRTM        :
        integer ::   S_DGAP_NM      !  4*NRTM        :
        integer ::   S_DGAP_M       !  NRTM        :
        integer ::   S_DELTA_PMAX_DGAP !  1        :I
        integer ::   S_XFIC         !  3*NSNE
        integer ::   S_VFIC         !  3*NSNE
        integer ::   S_MSFIC        !  NSNE

!type25
        integer ::   S_EDGE_BISECTOR       !  3*4*NRTM
        integer ::   S_PENM         !  4*MULTIMP*NSN      :PENM:Penetrations wrt 4 sub-triangles :  25 
        integer ::   S_DISTM        !  MULTIMP*NSN        :DISTM:True distance to segment        :  25 
        integer ::   S_LBM          !  MULTIMP*NSN        :LBM:Coordinate LB wrt 4 triangles     :  25 
        integer ::   S_LCM          !  MULTIMP*NSN        :LCM:Coordinate LC wrt 4 triangles     :  25 
        integer ::   S_VTX_BISECTOR !  3*2*NADMSR         :                                      :  25
        integer ::   S_CAND_PS      !  4*MULTIMPS*NCONTE:CAND_PS:PENETRATION INITIALE vs Solid Main Edges: 25
        integer ::   S_GAPE         !  NEDGE 
        integer ::   S_GAP_E_L      !  NEDGE 
        integer ::   S_STFE         !  NEDGE 
        integer ::   S_GAPMSAV      ! NRTM
        integer ::   S_E2S_NOD_NORMAL ! 3*NADMSR

        integer :: NUMBER_EDGE_TYPE1
        integer :: NUMBER_EDGE_TYPE1_0
C       Orthotropic friction 
        integer ::   S_DIR_FRICM !  2*NRTM         : orhotropic directions for friction 
C   Stiffness based on mass and time step
        integer ::   S_STIFMSDT_S       ! NSN : nodal mass on secondary node *STFACM
        integer ::   S_STIFMSDT_M       ! NRTM : main mass *STFACM
        integer ::   S_STIFMSDT_EDG     ! NEDGE : edge mass *STFACM
C
C=======================================================================
c DEFINE ARRAYS (integers arrays)
C=======================================================================
        integer, DIMENSION(:) , POINTER ::  IRECTS    
        integer, DIMENSION(:) , POINTER ::  IRECTM    
        integer, DIMENSION(:) , POINTER ::  NSV       
        integer, DIMENSION(:) , POINTER ::  MSR       
        integer, DIMENSION(:) , POINTER ::  IRTLM     
        integer, DIMENSION(:) , POINTER ::  IRUPT     
        integer, DIMENSION(:) , POINTER ::  INORM     
        integer, DIMENSION(:) , POINTER ::  IELEC     
        integer, DIMENSION(:) , POINTER ::  IELES     
        integer, DIMENSION(:) , POINTER ::  LISUB  
        integer, DIMENSION(:) , POINTER ::  TYPSUB        
        integer, DIMENSION(:) , POINTER ::  ADDSUBS   
        integer, DIMENSION(:) , POINTER ::  ADDSUBM   
        integer, DIMENSION(:) , POINTER ::  LISUBS    
        integer, DIMENSION(:) , POINTER ::  LISUBM    
        integer, DIMENSION(:) , POINTER ::  INFLG_SUBS
        integer, DIMENSION(:) , POINTER ::  INFLG_SUBM
        integer, DIMENSION(:) , POINTER ::  ADDSUBE   
        integer, DIMENSION(:) , POINTER ::  LISUBE    
        integer, DIMENSION(:) , POINTER ::  INFLG_SUBE
        integer, DIMENSION(:) , POINTER ::  MSEGTYP   
        integer, DIMENSION(:) , POINTER ::  CAND_E    
        integer, DIMENSION(:) , POINTER ::  CAND_N    
        integer, DIMENSION(:) , POINTER ::  I_STOK    
        integer, DIMENSION(:) , POINTER ::  I_STOK_E  
        integer, DIMENSION(:) , POINTER ::  IFPEN     
        integer, DIMENSION(:) , POINTER ::  KREMNODE  
        integer, DIMENSION(:) , POINTER ::  REMNODE   
        integer, DIMENSION(:) , POINTER ::  ADCCM     
        integer, DIMENSION(:) , POINTER ::  CHAIN     
        integer, DIMENSION(:) , POINTER ::  NIGE
!type20
        integer, DIMENSION(:) , POINTER ::  NBINFLG   
        integer, DIMENSION(:) , POINTER ::  MBINFLG   
        integer, DIMENSION(:) , POINTER ::  EBINFLG   
        integer, DIMENSION(:) , POINTER ::  NLG       
        integer, DIMENSION(:) , POINTER ::  DAANC6    
        integer, DIMENSION(:) , POINTER ::  ISLINS    
        integer, DIMENSION(:) , POINTER ::  ISLINM    
        integer, DIMENSION(:) , POINTER ::  IXLINS    
        integer, DIMENSION(:) , POINTER ::  IXLINM    
        integer, DIMENSION(:) , POINTER ::  NSVL      
        integer, DIMENSION(:) , POINTER ::  MSRL      
        integer, DIMENSION(:) , POINTER ::  LCAND_N   
        integer, DIMENSION(:) , POINTER ::  LCAND_S   
        integer, DIMENSION(:) , POINTER ::  ADCCM20   
        integer, DIMENSION(:) , POINTER ::  CHAIN20   
!type1
        integer, DIMENSION(:) , POINTER ::  ILOCS     
        integer, DIMENSION(:) , POINTER ::  NSEGM     
        integer, DIMENSION(:) , POINTER ::  NRT       
!type2
        integer, DIMENSION(:) , POINTER ::  MSEGTYP2
!type3
        integer, DIMENSION(:) , POINTER ::  IRTLS     
        integer, DIMENSION(:) , POINTER ::  ILOCM     
        integer, DIMENSION(:) , POINTER ::  IRTLOM    
        integer, DIMENSION(:) , POINTER ::  IRTLOS    
        integer, DIMENSION(:) , POINTER ::  NSEGS     
        integer, DIMENSION(:) , POINTER ::  LNSV      
        integer, DIMENSION(:) , POINTER ::  LMSR      
!type4
        integer, DIMENSION(:) , POINTER ::  IELEM     
!type12
        integer, DIMENSION(:) , POINTER ::  FCOUNT    
!type14
        integer, DIMENSION(:) , POINTER ::  KSURF     
        integer, DIMENSION(:) , POINTER ::  IMPACT    

!type21
        integer, DIMENSION(:) , POINTER ::  MSR21     
        integer, DIMENSION(:) , POINTER ::  MNDD      
        integer, DIMENSION(:) , POINTER ::  MSR_L     
!type24
        integer, DIMENSION(:) , POINTER ::  MVOISIN   
        integer, DIMENSION(:) , POINTER ::  NVOISIN   
        integer, DIMENSION(:) , POINTER ::  MSEGLO    
        integer, DIMENSION(:) , POINTER ::  MSEGTYP24 
C--------to be cleaned later        
        integer, DIMENSION(:) , POINTER ::  ISEADD    
        integer, DIMENSION(:) , POINTER ::  ISEDGE    
        integer, DIMENSION(:) , POINTER ::  CAND_T    
        integer, DIMENSION(:) , POINTER ::  ISEG_PXFEM
        integer, DIMENSION(:) , POINTER ::  ISEG_PLY  
        integer, DIMENSION(:) , POINTER ::  ICONT_I   
        integer, DIMENSION(:) , POINTER ::  IRTSE     
        integer, DIMENSION(:) , POINTER ::  IS2SE     
        integer, DIMENSION(:) , POINTER ::  IS2PT     
        integer, DIMENSION(:) , POINTER ::  ISPT2     
        integer, DIMENSION(:) , POINTER ::  ISEGPT     
        integer, DIMENSION(:) , POINTER ::  IS2ID      ! Global ID Fictive Nodes   
!type25
        integer, DIMENSION(:) , POINTER ::  NSV_ON_PMAIN     !  
        integer, DIMENSION(:) , POINTER ::  EVOISIN   !  
        integer, DIMENSION(:) , POINTER ::  ADMSR     !  
        integer, DIMENSION(:) , POINTER ::  LEDGE     !  
        integer, DIMENSION(:) , POINTER ::  LBOUND    !  
        integer, DIMENSION(:) , POINTER ::  FREE_IRECT_ID!
        integer, DIMENSION(:) , POINTER ::  ACTNOR    !  
        integer, DIMENSION(:) , POINTER ::  FARM      !  
        integer, DIMENSION(:) , POINTER ::  ADSKYN    !  
        integer, DIMENSION(:) , POINTER ::  IADNOR    !  
        integer, DIMENSION(:) , POINTER ::  ISLIDE    !  
        integer, DIMENSION(:) , POINTER ::  KNOR2MSR  !  
        integer, DIMENSION(:) , POINTER ::  NOR2MSR   !  
        integer, DIMENSION(:) , POINTER ::  CAND_OPT_N!  
        integer, DIMENSION(:) , POINTER ::  CAND_OPT_E!
        integer, DIMENSION(:) , POINTER ::  IF_ADH    ! Type25 and interface adhesion
        integer, DIMENSION(:) , POINTER ::  CANDM_E2E  
        integer, DIMENSION(:) , POINTER ::  CANDS_E2E
        integer, DIMENSION(:) , POINTER ::  CANDM_E2S  
        integer, DIMENSION(:) , POINTER ::  CANDS_E2S
        integer, DIMENSION(:) , POINTER ::  IFPEN_E   
        integer, DIMENSION(:) , POINTER ::  IFPEN_E2S      
        integer, DIMENSION(:), ALLOCATABLE :: EDGE_TYPE1
        integer, DIMENSION(:), ALLOCATABLE :: EDGE_TYPE1_0
C---- INTER FRICTION PARTS
        integer, DIMENSION(:) , POINTER ::  IPARTFRICS 
        integer, DIMENSION(:) , POINTER ::  IPARTFRICM
        integer, DIMENSION(:) , POINTER ::  IPARTFRIC_E
C      Orthotropic friction
        integer, DIMENSION(:) , POINTER ::  IREP_FRICM        
C---- Irem Gap option for interface type 25
        integer, DIMENSION(:) , POINTER ::  KREMNOR
        integer, DIMENSION(:) , POINTER ::  REMNOR 
        integer, DIMENSION(:) , POINTER ::  KREMNODE_EDG  
        integer, DIMENSION(:) , POINTER ::  REMNODE _EDG 
        integer, DIMENSION(:) , POINTER ::  KREMNODE_E2S  
        integer, DIMENSION(:) , POINTER ::  REMNODE _E2S 
C----Nitsche Method
        integer, DIMENSION(:) , POINTER ::  IELNRTS
        integer, DIMENSION(:) , POINTER ::  ADRECTS 
        integer, DIMENSION(:) , POINTER ::  FACNRTS

        integer, DIMENSION(:) , POINTER ::  E2S_ACTNOR

C=======================================================================
c DEFINE ARRAYS (float arrays)
C=======================================================================
        my_real, DIMENSION(:) , POINTER ::   STFAC        
        my_real, DIMENSION(:) , POINTER ::   VARIABLES    
        my_real, DIMENSION(:) , POINTER ::   CSTS         
        my_real, DIMENSION(:) , POINTER ::   DPARA        
        my_real, DIMENSION(:) , POINTER ::   NMAS         
        my_real, DIMENSION(:) , POINTER ::   SMAS         
        my_real, DIMENSION(:) , POINTER ::   SINER        
        my_real, DIMENSION(:) , POINTER ::   AREAS2       
        my_real, DIMENSION(:) , POINTER ::   UVAR         
        my_real, DIMENSION(:) , POINTER ::   XM0          
        my_real, DIMENSION(:) , POINTER ::   SPENALTY     
        my_real, DIMENSION(:) , POINTER ::   STFR_PENALTY 
        my_real, DIMENSION(:) , POINTER ::   SKEW         
        my_real, DIMENSION(:) , POINTER ::   DSM          
        my_real, DIMENSION(:) , POINTER ::   FSM          
        my_real, DIMENSION(:) , POINTER ::   RUPT         
        my_real, DIMENSION(:) , POINTER ::   FINI         
        my_real, DIMENSION(:) , POINTER ::   STFNS        
        my_real, DIMENSION(:) , POINTER ::   STFM         
        my_real, DIMENSION(:) , POINTER ::   STFS         
        my_real, DIMENSION(:) , POINTER ::   PENIM        
        my_real, DIMENSION(:) , POINTER ::   PENIS        
        my_real, DIMENSION(:) , POINTER ::   GAP_M        
        my_real, DIMENSION(:) , POINTER ::   GAP_S        
        my_real, DIMENSION(:) , POINTER ::   XSAV         
        my_real, DIMENSION(:) , POINTER ::   CRIT         
        my_real, DIMENSION(:) , POINTER ::   FRIC_P       
        my_real, DIMENSION(:) , POINTER ::   XFILTR       
        my_real, DIMENSION(:) , POINTER ::   AREAS        
        my_real, DIMENSION(:) , POINTER ::   AREAM        
        my_real, DIMENSION(:) , POINTER ::   GAP_SL       
        my_real, DIMENSION(:) , POINTER ::   GAP_ML       
        my_real, DIMENSION(:) , POINTER ::   CAND_P       
        my_real, DIMENSION(:) , POINTER ::   FTSAVX       
        my_real, DIMENSION(:) , POINTER ::   FTSAVY       
        my_real, DIMENSION(:) , POINTER ::   FTSAVZ  
        my_real, DIMENSION(:) , POINTER ::   FTSAVX_E      
        my_real, DIMENSION(:) , POINTER ::   FTSAVY_E       
        my_real, DIMENSION(:) , POINTER ::   FTSAVZ_E
        my_real, DIMENSION(:) , POINTER ::   FTSAVX_E2S       
        my_real, DIMENSION(:) , POINTER ::   FTSAVY_E2S       
        my_real, DIMENSION(:) , POINTER ::   FTSAVZ_E2S       
C---- ige
        my_real, DIMENSION(:) , POINTER ::   RIGE
        my_real, DIMENSION(:) , POINTER ::   XIGE
        my_real, DIMENSION(:) , POINTER ::   VIGE
        my_real, DIMENSION(:) , POINTER ::   MASSIGE
!type10
        my_real, DIMENSION(:) , POINTER ::   CAND_F    
!type20
        my_real, DIMENSION(:) , POINTER ::   CAND_FX   
        my_real, DIMENSION(:) , POINTER ::   CAND_FY   
        my_real, DIMENSION(:) , POINTER ::   CAND_FZ   
        my_real, DIMENSION(:) , POINTER ::   XA        
        my_real, DIMENSION(:) , POINTER ::   VA        
        my_real, DIMENSION(:) , POINTER ::   STFA      
        my_real, DIMENSION(:) , POINTER ::   PENIA     
        my_real, DIMENSION(:) , POINTER ::   ALPHAK    
        my_real, DIMENSION(:) , POINTER ::   GAP_SH    
        my_real, DIMENSION(:) , POINTER ::   AVX_ANCR  
        my_real, DIMENSION(:) , POINTER ::   CRITX     
        my_real, DIMENSION(:) , POINTER ::   GAP_ME    
        my_real, DIMENSION(:) , POINTER ::   STF       
        my_real, DIMENSION(:) , POINTER ::   PENIME    
        my_real, DIMENSION(:) , POINTER ::   GAP_SE    
        my_real, DIMENSION(:) , POINTER ::   PENISE    
        my_real, DIMENSION(:) , POINTER ::   STFNE     
!type1
        my_real, DIMENSION(:) , POINTER ::   N       
!type3,4,9
        my_real, DIMENSION(:) , POINTER ::   CSTM    
        my_real, DIMENSION(:) , POINTER ::   STFNM   
        my_real, DIMENSION(:) , POINTER ::   FRICOS  
        my_real, DIMENSION(:) , POINTER ::   FRICOM  
        my_real, DIMENSION(:) , POINTER ::   FTSAV   
!type6
        my_real, DIMENSION(:) , POINTER ::   FCONT   
        my_real, DIMENSION(:) , POINTER ::   FS      
        my_real, DIMENSION(:) , POINTER ::   FM      
        my_real, DIMENSION(:) , POINTER ::   RMAS    
        my_real, DIMENSION(:) , POINTER ::   ANSMX0  
!type 8
        my_real, DIMENSION(:) , POINTER ::   GAPN    
        my_real, DIMENSION(:) , POINTER ::   STF8    
        my_real, DIMENSION(:) , POINTER ::   EE      
!type14
        my_real, DIMENSION(:) , POINTER ::   CIMP   
        my_real, DIMENSION(:) , POINTER ::   NIMP   

!type15
        my_real, DIMENSION(:) , POINTER ::   IOLD   
        my_real, DIMENSION(:) , POINTER ::   HOLD   
        my_real, DIMENSION(:) , POINTER ::   NOLD   
        my_real, DIMENSION(:) , POINTER ::   DOLD   
!type17
        my_real, DIMENSION(:) , POINTER ::   KS     
        my_real, DIMENSION(:) , POINTER ::   KM     
        my_real, DIMENSION(:) , POINTER ::   FROTS  
        my_real, DIMENSION(:) , POINTER ::   FROTM  

!type21
        my_real, DIMENSION(:) , POINTER ::   NOD_NORMAL 
        my_real, DIMENSION(:) , POINTER ::   RCURV      
        my_real, DIMENSION(:) , POINTER ::   ANGLM      
        my_real, DIMENSION(:) , POINTER ::   FROT_P     
        my_real, DIMENSION(:) , POINTER ::   ALPHA0     
        my_real, DIMENSION(:) , POINTER ::   AS         
        my_real, DIMENSION(:) , POINTER ::   BS         
        my_real, DIMENSION(:) , POINTER ::   THKNOD0    
!type24
        my_real, DIMENSION(:) , POINTER ::   GAPN_M   
        my_real, DIMENSION(:) , POINTER ::   SECND_FR  
        my_real, DIMENSION(:) , POINTER ::   PENE_OLD 
        my_real, DIMENSION(:) , POINTER ::   STIF_OLD 
        my_real, DIMENSION(:) , POINTER ::   TIME_S   
        my_real, DIMENSION(:) , POINTER ::   GAP_NM   
C-------to be cleaned later        
        my_real, DIMENSION(:) , POINTER ::   EDGE8L2  
        my_real, DIMENSION(:) , POINTER ::   NOD_2RY_LGTH 
        my_real, DIMENSION(:) , POINTER ::   NOD_MAS_LGTH 
        my_real, DIMENSION(:) , POINTER ::   GAP_N0  
        my_real, DIMENSION(:) , POINTER ::   DGAP_NM 
        my_real, DIMENSION(:) , POINTER ::   DGAP_M  
        my_real, DIMENSION(:) , POINTER ::   DELTA_PMAX_DGAP  
        my_real, DIMENSION(:) , POINTER ::   XFIC     
        my_real, DIMENSION(:) , POINTER ::   VFIC     
        my_real, DIMENSION(:) , POINTER ::   MSFIC     
!type25
        REAL(4), DIMENSION(:) , POINTER ::   EDGE_BISECTOR  !
        my_real, DIMENSION(:) , POINTER ::   PENM    !
        my_real, DIMENSION(:) , POINTER ::   DISTM   !
        my_real, DIMENSION(:) , POINTER ::   LBM     !
        my_real, DIMENSION(:) , POINTER ::   LCM     !
        REAL(4), DIMENSION(:) , POINTER ::   VTX_BISECTOR ! 
        my_real, DIMENSION(:) , POINTER ::   CAND_PS
        my_real, DIMENSION(:) , POINTER ::   GAPE
        my_real, DIMENSION(:) , POINTER ::   GAP_E_L
        my_real, DIMENSION(:) , POINTER ::   STFE
        my_real, DIMENSION(:) , POINTER ::   GAPMSAV
        REAL(4), DIMENSION(:) , POINTER ::   E2S_NOD_NORMAL  !

        my_real, DIMENSION(:) , POINTER ::   STIFMSDT_S
        my_real, DIMENSION(:) , POINTER ::   STIFMSDT_M
        my_real, DIMENSION(:) , POINTER ::   STIFMSDT_EDG
        INTEGER :: NRTM_FREE

C MPI Communicators
        INTEGER :: MPI_COMM
        INTEGER :: RANK
        INTEGER :: NSPMD
C
        INTEGER :: NB_INTERNAL_EDGES        ! number of edges internal to the domain
        INTEGER :: NB_BOUNDARY_EDGES_LOCAL  ! boundary edges treated by current domain 
        INTEGER :: NB_BOUNDARY_EDGES_REMOTE ! boundary edges treated by the other domain 


!type2
        my_real, DIMENSION(:) , POINTER ::   CSTS_BIS
C      Orthotropic friction
        my_real, DIMENSION(:) , POINTER ::   DIR_FRICM
C
        TYPE(INT8_STRUCT_)             ::   T8
        TYPE(METRIC_STRUCT_)           ::   METRIC

C=======================================================================
      END TYPE INTBUF_STRUCT_
C=======================================================================

c INTBUF_SIZE array maximum length defined as parameter
c (maximum number of different arrays composing INTBUF_TAB structure)
      INTEGER, PARAMETER :: L_INTBUF_SIZE_MAX = 516
      INTEGER INTER_ITHKNOD !Flag to fill THKNOD array  (enabled with Ithick parameter from interface type 25 or 21)


c
c---------------
      END MODULE INTBUFDEF_MOD

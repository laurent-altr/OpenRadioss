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
      !||    elbufdef_mod                        ../common_source/modules/mat_elem/elbufdef_mod.F
      !||--- called by ------------------------------------------------------
      !||    a22conv3                            ../engine/source/ale/alefvm/cut_cells/a22conv3.F
      !||    aconve                              ../engine/source/ale/aconve.F
      !||    admdiv                              ../engine/source/model/remesh/admdiv.F
      !||    admerr                              ../engine/source/model/remesh/admerr.F
      !||    admgvid                             ../engine/source/model/remesh/admgvid.F
      !||    admini                              ../engine/source/model/remesh/admini.F
      !||    admmap3                             ../engine/source/model/remesh/admmap3.F
      !||    admmap4                             ../engine/source/model/remesh/admmap4.F
      !||    admregul                            ../engine/source/model/remesh/admregul.F
      !||    admthke                             ../engine/source/model/remesh/admthke.F
      !||    aeturb                              ../engine/source/ale/turbulence/aeturb.F
      !||    aflux0                              ../engine/source/ale/aflux0.F
      !||    aflux3_int22_fvm                    ../engine/source/ale/alefvm/cut_cells/aflux3_int22_fvm.F
      !||    afluxt                              ../engine/source/ale/ale51/afluxt.F
      !||    agauge                              ../engine/source/ale/agauge.F
      !||    agrad0                              ../engine/source/ale/agrad0.F
      !||    airbagb1                            ../engine/source/airbag/airbagb1.F
      !||    akturb                              ../engine/source/ale/turbulence/akturb.F
      !||    ale51_antidiff3_int22               ../engine/source/ale/alefvm/cut_cells/ale51_antidiff3_int22.F
      !||    ale51_finish                        ../engine/source/ale/ale51/ale51_finish.F
      !||    ale51_gradient_reconstruction       ../engine/source/ale/alemuscl/ale51_gradient_reconstruction.F
      !||    ale51_gradient_reconstruction2      ../engine/source/ale/alemuscl/ale51_gradient_reconstruction2.F
      !||    ale51_init                          ../engine/source/ale/ale51/ale51_init.F
      !||    ale51_upwind3_int22                 ../engine/source/ale/alefvm/cut_cells/ale51_upwind3_int22.F
      !||    aleflow                             ../engine/source/ale/porous/aleflow.F
      !||    alefvm_main                         ../engine/source/ale/alefvm/alefvm_main.F
      !||    alefvm_stress_int22                 ../engine/source/ale/alefvm/alefvm_stress_int22.F
      !||    alethe                              ../engine/source/ale/alethe.F
      !||    alew6                               ../engine/source/ale/grid/alew6.F
      !||    alewdx                              ../engine/source/ale/grid/alewdx.F
      !||    alloc_elbuf_imp                     ../engine/source/elements/elbuf/alloc_elbuf_imp.F
      !||    allocbuf_auto                       ../engine/source/elements/elbuf/allocbuf_auto.F
      !||    anim_nodal_p_elems                  ../engine/source/output/anim/generate/anim_nodal_p_elems.F
      !||    anim_nodal_ssp_elems                ../engine/source/output/anim/generate/anim_nodal_ssp_elems.F90
      !||    animig3d                            ../engine/source/output/anim/generate/animig3d.F
      !||    animx                               ../engine/source/output/anim/generate/animx.F
      !||    anioff0                             ../engine/source/output/anim/generate/anioff0.F
      !||    anioffc                             ../engine/source/output/anim/generate/anioffc.F
      !||    anioffc_crk                         ../engine/source/output/anim/generate/anioffc_crk.F
      !||    anioffc_ply                         ../engine/source/output/anim/generate/anioffc_ply.F
      !||    aniofff                             ../engine/source/output/anim/generate/aniofff.F
      !||    anioffs                             ../engine/source/output/anim/generate/anioff6.F
      !||    aniskew                             ../engine/source/output/anim/generate/aniskew.F
      !||    arezon                              ../engine/source/ale/arezon.F
      !||    atherm                              ../engine/source/ale/atherm.F
      !||    bforc2                              ../engine/source/ale/bimat/bforc2.F
      !||    binit2                              ../starter/source/ale/bimat/binit2.F
      !||    bsigini                             ../starter/source/elements/beam/bsigini.F
      !||    bulkfakeigeo3                       ../starter/source/elements/ige3d/bulkfakeigeo3.F
      !||    buserini                            ../starter/source/elements/beam/buserini.F
      !||    c3coork3                            ../engine/source/elements/sh3n/coque3n/c3coork3.F
      !||    c3epsini                            ../starter/source/elements/sh3n/coque3n/c3epsini.F
      !||    c3evec3                             ../engine/source/elements/sh3n/coque3n/c3evec3.F
      !||    c3fint_reg                          ../engine/source/elements/sh3n/coque3n/c3fint_reg.F
      !||    c3fint_reg_ini                      ../starter/source/elements/sh3n/coque3n/c3fint_reg_ini.F
      !||    c3inmas                             ../starter/source/elements/sh3n/coque3n/c3inmas.F
      !||    c3ke3                               ../engine/source/elements/sh3n/coque3n/c3ke3.F
      !||    c_seatbelts                         ../starter/source/restart/ddsplit/c_seatbelts.F
      !||    c_tf_ne                             ../engine/source/output/sty/c_tf_ne.F
      !||    cbacoor                             ../engine/source/elements/shell/coqueba/cbacoor.F
      !||    cbacoork                            ../engine/source/elements/shell/coqueba/cbacoork.F
      !||    cbacoort                            ../engine/source/elements/shell/coqueba/cbacoor.F
      !||    cbafint_reg                         ../engine/source/elements/shell/coqueba/cbafint_reg.F
      !||    cbafint_reg_ini                     ../starter/source/elements/shell/coqueba/cbafint_reg_ini.F
      !||    cbainit3                            ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cbake3                              ../engine/source/elements/shell/coqueba/cbake3.F
      !||    cbal58warp                          ../engine/source/elements/shell/coqueba/cbawarpoff.F
      !||    cbapinchproj                        ../engine/source/elements/shell/coqueba/cbapinchproj.F
      !||    cbufxfe                             ../starter/source/elements/xfem/cbufxfe.F
      !||    cdk6coor3                           ../engine/source/elements/sh3n/coquedk6/cdk6coor3.F
      !||    cdk6fint_reg                        ../engine/source/elements/sh3n/coquedk6/cdk6fint_reg.F
      !||    cdkcoor3                            ../engine/source/elements/sh3n/coquedk/cdkcoor3.F
      !||    cdkepsini                           ../starter/source/elements/sh3n/coquedk/cdkepsini.F
      !||    cdkfint_reg                         ../engine/source/elements/sh3n/coquedk/cdkfint_reg.F
      !||    cdkfint_reg_ini                     ../starter/source/elements/sh3n/coquedk/cdkfint_reg_ini.F
      !||    cdkinit3                            ../starter/source/elements/sh3n/coquedk/cdkinit3.F
      !||    cepsini                             ../starter/source/elements/shell/coque/cepsini.F
      !||    cevec3                              ../engine/source/elements/shell/coque/cevec3.F
      !||    cfailini                            ../starter/source/elements/shell/coque/cfailini.F
      !||    cfailini4                           ../starter/source/elements/shell/coque/cfailini.F
      !||    cfint_reg                           ../engine/source/elements/shell/coque/cfint_reg.F
      !||    cfint_reg_ini                       ../starter/source/elements/shell/coque/cfint_reg_ini.F
      !||    cgshell3                            ../engine/source/implicit/cgshell.F
      !||    cgshell4                            ../engine/source/implicit/cgshell.F
      !||    check_ale_comm                      ../engine/source/ale/check_ale_comm.F
      !||    check_mat_elem_prop_compatibility   ../starter/source/materials/mat/check_mat_elem_prop_compatibility.F
      !||    chk_dttsh                           ../starter/source/elements/thickshell/solidec/scdtchk3.F
      !||    chkstfn3n                           ../engine/source/interfaces/interf/chkstfn3.F
      !||    cinit3                              ../starter/source/elements/shell/coque/cinit3.F
      !||    cinmas                              ../starter/source/elements/shell/coque/cinmas.F
      !||    clusterf                            ../engine/source/output/cluster/clusterf.F
      !||    cm27in3                             ../starter/source/materials/mat/mat027/cm27in3.F
      !||    cm35in3                             ../starter/source/materials/mat/mat035/cm35in3.F
      !||    cmain3pinch                         ../engine/source/elements/shell/coqueba/cmain3pinch.F
      !||    cmaini3                             ../starter/source/elements/sh3n/coquedk/cmaini3.F
      !||    cmatc3                              ../engine/source/elements/shell/coqueba/cmatc3.F
      !||    cmatini                             ../starter/source/materials/mat_share/cmatini.F
      !||    cmatini4                            ../starter/source/materials/mat_share/cmatini4.F
      !||    cmlawi                              ../starter/source/elements/shell/coque/cepsini.F
      !||    cncoefort                           ../engine/source/elements/sh3n/coquedk/cncoef3.F
      !||    cnepsini                            ../starter/source/elements/shell/coqueba/cnepsini.F
      !||    cnloc_mat104_ini                    ../starter/source/materials/mat/mat104/cnloc_mat104_ini.F
      !||    cnloc_matini                        ../starter/source/materials/mat_share/cnloc_matini.F
      !||    cnvec3                              ../engine/source/elements/shell/coque/cnvec3.F
      !||    convecoff                           ../engine/source/constraints/thermic/convecoff.F
      !||    copy_elbuf                          ../engine/source/elements/elbuf/copy_elbuf.F
      !||    copy_elbuf_1                        ../engine/source/elements/elbuf/copy_elbuf_1.F
      !||    cortdir3                            ../engine/source/elements/shell/coque/cortdir3.F
      !||    corth3                              ../starter/source/elements/shell/coque/corth3.F
      !||    corthdir                            ../starter/source/elements/shell/coque/corthdir.F
      !||    corthini                            ../starter/source/elements/shell/coque/corthini.F
      !||    count_arsz_ct                       ../engine/source/output/sty/outp_c_t.F
      !||    count_arsz_st                       ../engine/source/output/sty/outp_s_t.F
      !||    cp_impbuf                           ../engine/source/implicit/produt_v.F
      !||    crklayer4n_adv                      ../engine/source/elements/xfem/crklayer4n_adv.F
      !||    crklayer4n_ini                      ../engine/source/elements/xfem/crklayer4n_ini.F
      !||    crkoffc                             ../engine/source/elements/xfem/precrklay.F
      !||    crkofftg                            ../engine/source/elements/xfem/precrklay.F
      !||    csigini                             ../starter/source/elements/shell/coque/csigini.F
      !||    csigini4                            ../starter/source/elements/shell/coqueba/scigini4.F
      !||    cuserini                            ../starter/source/elements/shell/coque/cuserini.F
      !||    cuserini4                           ../starter/source/elements/shell/coqueba/cuserini4.F
      !||    cutfunce                            ../engine/source/tools/sect/cutfunce.F
      !||    czcoork3                            ../engine/source/elements/shell/coquez/czcoork3.F
      !||    czcorc1                             ../engine/source/elements/shell/coquez/czcorc.F
      !||    czcorcht                            ../engine/source/elements/shell/coquez/czcorc.F
      !||    czcorct                             ../engine/source/elements/shell/coquez/czcorc.F
      !||    czke3                               ../engine/source/elements/shell/coquez/czke3.F
      !||    czkel3                              ../engine/source/elements/shell/coquez/czkel3.F
      !||    deallocate_elbuf                    ../starter/source/elements/elbuf_init/deallocate_buffer.F
      !||    deallocate_one_element_group        ../starter/source/elements/elbuf_init/deallocate_one_element_group.F
      !||    deltax22                            ../engine/source/interfaces/int22/deltax22.F
      !||    desacti                             ../engine/source/elements/desacti.F
      !||    dfunc0                              ../engine/source/output/anim/generate/dfunc0.F
      !||    dfuncc                              ../engine/source/output/anim/generate/dfuncc.F
      !||    dfuncc_crk                          ../engine/source/output/anim/generate/dfuncc_crk.F
      !||    dfuncc_ply                          ../engine/source/output/anim/generate/dfuncc_ply.F
      !||    dfuncf                              ../engine/source/output/anim/generate/dfuncf.F
      !||    dfuncs                              ../engine/source/output/anim/generate/dfunc6.F
      !||    dfungps1                            ../engine/source/output/anim/generate/dfuncf.F
      !||    dfungps2                            ../engine/source/output/anim/generate/dfuncf.F
      !||    dim_elemax                          ../engine/source/implicit/ind_glob_k.F
      !||    dim_elems1                          ../engine/source/implicit/ind_glob_k.F
      !||    dim_elems2                          ../engine/source/implicit/ind_glob_k.F
      !||    dim_elems3                          ../engine/source/implicit/ind_glob_k.F
      !||    dim_elems4                          ../engine/source/implicit/ind_glob_k.F
      !||    dim_elemsp                          ../engine/source/implicit/ind_glob_k.F
      !||    dim_fr_k                            ../engine/source/mpi/implicit/imp_fri.F
      !||    dim_glob_k                          ../engine/source/implicit/ind_glob_k.F
      !||    dim_kinmax                          ../engine/source/implicit/ind_glob_k.F
      !||    dim_tshedg                          ../engine/source/elements/thickshell/solidec/dim_tshedg.F
      !||    dmasani0                            ../engine/source/output/anim/generate/dmasani0.F
      !||    dmasanic                            ../engine/source/output/anim/generate/dmasanic.F
      !||    dmasanif                            ../engine/source/output/anim/generate/dmasanif.F
      !||    dmasanis                            ../engine/source/output/anim/generate/dmasani6.F
      !||    dtmain                              ../starter/source/materials/time_step/dtmain.F
      !||    dynain_c_strag                      ../engine/source/output/dynain/dynain_c_strag.F
      !||    dynain_c_strsg                      ../engine/source/output/dynain/dynain_c_strsg.F
      !||    dynain_shel_mp                      ../engine/source/output/dynain/dynain_shel_mp.F
      !||    dynain_shel_spmd                    ../engine/source/output/dynain/dynain_shel_spmd.F
      !||    dynain_size_c                       ../engine/source/output/dynain/dynain_size.F
      !||    ebcs0                               ../engine/source/boundary_conditions/ebcs/ebcs0.F
      !||    ebcs10                              ../engine/source/boundary_conditions/ebcs/ebcs10.F
      !||    ebcs_main                           ../engine/source/boundary_conditions/ebcs/ebcs_main.F
      !||    eflux3_int22_fvm                    ../engine/source/ale/alefvm/cut_cells/eflux3_int22_fvm.F
      !||    eig                                 ../engine/stub/eig.F
      !||    eig1                                ../engine/stub/eig1.F
      !||    eigcond                             ../engine/stub/eigcond.F
      !||    eigoff                              ../engine/source/output/anim/generate/eigoff.F
      !||    eigp                                ../engine/stub/eigp.F
      !||    eloff                               ../engine/source/elements/eloff.F
      !||    enrichc_ini                         ../engine/source/elements/xfem/enrichc_ini.F
      !||    enrichtg_ini                        ../engine/source/elements/xfem/enrichtg_ini.F
      !||    err_thk                             ../engine/source/elements/shell/err_thk.F
      !||    fail_gene1_s                        ../engine/source/materials/fail/gene1/fail_gene1_s.F
      !||    failini                             ../starter/source/elements/solid/solide/failini.F
      !||    forints                             ../engine/source/elements/forints.F
      !||    fv_up_switch                        ../engine/source/airbag/fv_up_switch.F
      !||    fvbag0                              ../engine/source/airbag/fvbag0.F
      !||    fvbag1                              ../engine/source/airbag/fvbag1.F
      !||    fvbag2                              ../engine/source/airbag/fvbag2.F
      !||    fvvent0                             ../engine/source/airbag/fvvent0.F
      !||    fxbodfp1                            ../engine/source/constraints/fxbody/fxbodfp.F
      !||    fxbsgmaj                            ../engine/source/constraints/fxbody/fxbsgmaj.F
      !||    fxbyfor                             ../engine/source/constraints/fxbody/fxbyfor.F
      !||    fxbypid                             ../engine/source/constraints/fxbody/fxbypid.F
      !||    genani                              ../engine/source/output/anim/generate/genani.F
      !||    genani1                             ../starter/source/output/anim/genani1.F
      !||    gendynain                           ../engine/source/output/dynain/gendynain.F
      !||    genh3d                              ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    genoutp                             ../engine/source/output/sty/genoutp.F
      !||    get_nodal_ipart                     ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    get_q4lsys                          ../engine/source/output/sta/sta_c_get_q4lsys.F
      !||    get_t3lsys                          ../engine/source/output/sta/sta_c_get_t3lsys.F
      !||    get_unique_main_cell                ../engine/source/interfaces/int22/get_unique_master_cell.F
      !||    gps_solid                           ../engine/source/output/outmaxsubr.F
      !||    gpsstrain_skin                      ../engine/source/output/anim/generate/tensgpstrain.F
      !||    gpstra_solid                        ../engine/source/output/outmaxsubr.F
      !||    h3d_fld_strain                      ../engine/source/output/h3d/h3d_results/h3d_fld_strain.F
      !||    h3d_fld_tsh                         ../engine/source/output/h3d/h3d_results/h3d_fld_tsh.F
      !||    h3d_nodal_scalar                    ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
      !||    h3d_nodal_tensor                    ../engine/source/output/h3d/h3d_results/h3d_nodal_tensor.F
      !||    h3d_nodal_vector                    ../engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
      !||    h3d_oned_off                        ../engine/source/output/h3d/spmd/spmd_h3d_oned_off.F
      !||    h3d_oned_scalar                     ../engine/source/output/h3d/h3d_results/h3d_oned_scalar.F
      !||    h3d_oned_tensor                     ../engine/source/output/h3d/h3d_results/h3d_oned_tensor.F
      !||    h3d_oned_vector                     ../engine/source/output/h3d/h3d_results/h3d_oned_vector.F
      !||    h3d_quad_off                        ../engine/source/output/h3d/spmd/spmd_h3d_quad_off.F
      !||    h3d_quad_scalar                     ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||    h3d_quad_tensor                     ../engine/source/output/h3d/h3d_results/h3d_quad_tensor.F
      !||    h3d_quad_vector                     ../engine/source/output/h3d/h3d_results/h3d_quad_vector.F
      !||    h3d_shell_off                       ../engine/source/output/h3d/spmd/spmd_h3d_shell_off.F
      !||    h3d_shell_scalar                    ../engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
      !||    h3d_shell_scalar_1                  ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
      !||    h3d_shell_tensor                    ../engine/source/output/h3d/h3d_results/h3d_shell_tensor.F
      !||    h3d_shell_vector                    ../engine/source/output/h3d/h3d_results/h3d_shell_vector.F
      !||    h3d_shell_vector_1                  ../engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
      !||    h3d_skin_ixskin                     ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_skin_off                        ../engine/source/output/h3d/h3d_results/h3d_skin_off.F
      !||    h3d_skin_tensor                     ../engine/source/output/h3d/h3d_results/h3d_skin_tensor.F
      !||    h3d_sol_skin_ixskin                 ../engine/source/output/h3d/h3d_results/h3d_sol_skin_ixskin.F
      !||    h3d_sol_skin_tensor                 ../engine/source/output/h3d/h3d_results/h3d_sol_skin_tensor.F
      !||    h3d_solid_off                       ../engine/source/output/h3d/spmd/spmd_h3d_solid_off.F
      !||    h3d_solid_scalar                    ../engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
      !||    h3d_solid_tensor                    ../engine/source/output/h3d/h3d_results/h3d_solid_tensor.F
      !||    h3d_solid_tensor_1                  ../engine/source/output/h3d/h3d_results/h3d_solid_tensor_1.F
      !||    h3d_solid_vector                    ../engine/source/output/h3d/h3d_results/h3d_solid_vector.F
      !||    h3d_sph_off                         ../engine/source/output/h3d/spmd/spmd_h3d_sph_off.F
      !||    h3d_sph_scalar                      ../engine/source/output/h3d/h3d_results/h3d_sph_scalar.F
      !||    h3d_sph_tensor                      ../engine/source/output/h3d/h3d_results/h3d_sph_tensor.F
      !||    h3d_velvecc22                       ../engine/source/output/h3d/h3d_results/h3d_velvecc22.F
      !||    h3d_velvecz22                       ../engine/source/output/h3d/h3d_results/h3d_velvecz22.F
      !||    hist2                               ../engine/source/output/th/hist2.F
      !||    i18for3                             ../engine/source/interfaces/int18/i18for3.F
      !||    i22for3                             ../engine/source/interfaces/int22/i22for3.F
      !||    i22mainf                            ../engine/source/interfaces/int22/i22mainf.F
      !||    i22subvol                           ../engine/source/interfaces/int22/i22subvol.F
      !||    i7mainf                             ../engine/source/interfaces/int07/i7mainf.F
      !||    i9grd2                              ../engine/source/interfaces/int09/i9grd2.F
      !||    i9grd3                              ../engine/source/interfaces/int09/i9grd3.F
      !||    i9wal2                              ../engine/source/interfaces/int09/i9wal2.F
      !||    i9wal3                              ../engine/source/interfaces/int09/i9wal3.F
      !||    i9wale                              ../engine/source/interfaces/int09/i9wale.F
      !||    ig3dinit3                           ../starter/source/elements/ige3d/ig3dinit3.F
      !||    imp_buck                            ../engine/source/implicit/imp_buck.F
      !||    imp_chkm                            ../engine/source/implicit/imp_solv.F
      !||    imp_glob_k                          ../engine/source/implicit/imp_glob_k.F
      !||    imp_glob_k0                         ../engine/source/implicit/imp_glob_k.F
      !||    imp_glob_khp                        ../engine/source/implicit/imp_glob_k.F
      !||    imp_init                            ../engine/source/implicit/imp_init.F
      !||    imp_k_eig                           ../engine/stub/imp_k_eig.F
      !||    imp_sol_init                        ../engine/source/implicit/imp_sol_init.F
      !||    imp_solv                            ../engine/source/implicit/imp_solv.F
      !||    ind_fr_k                            ../engine/source/mpi/implicit/imp_fri.F
      !||    ind_glob_k                          ../engine/source/implicit/ind_glob_k.F
      !||    ind_tshedg                          ../engine/source/elements/thickshell/solidec/ind_tshedg.F
      !||    ini_fr_k                            ../engine/source/mpi/implicit/imp_fri.F
      !||    ini_inimap1d                        ../starter/source/initial_conditions/inimap/ini_inimap1d.F
      !||    ini_inimap2d                        ../starter/stub/ini_inimap2d.F
      !||    ini_outmax_auto                     ../starter/source/elements/elbuf_init/ini_outmax_auto.F
      !||    ini_seatbelt                        ../starter/source/tools/seatbelts/ini_seatbelt.F
      !||    ini_tmax                            ../engine/source/output/ini_outmax.F
      !||    inicrkfill                          ../starter/source/elements/xfem/inicrkfill.F
      !||    iniebcs_dp                          ../starter/source/boundary_conditions/ebcs/iniebcs_dp.F
      !||    iniebcs_nrf_tcar                    ../starter/source/boundary_conditions/ebcs/iniebcs_nrf_tcar.F
      !||    iniebcsp                            ../starter/source/boundary_conditions/ebcs/iniebcsp.F
      !||    iniebcsp0                           ../starter/source/boundary_conditions/ebcs/iniebcsp0.F
      !||    inifill                             ../starter/source/initial_conditions/inivol/inifill.F
      !||    inigrav_eos                         ../starter/source/initial_conditions/inigrav/inigrav_eos.F
      !||    inigrav_load                        ../starter/source/initial_conditions/inigrav/inigrav_load.F
      !||    inigrav_m37                         ../starter/source/initial_conditions/inigrav/inigrav_m37.F
      !||    inigrav_m51                         ../starter/source/initial_conditions/inigrav/inigrav_m51.F
      !||    inintr_thkvar                       ../starter/source/interfaces/interf1/inintr_thkvar.F
      !||    iniphase                            ../starter/source/initial_conditions/inivol/iniphase.F
      !||    inirig_mat                          ../starter/source/elements/initia/inirig_mat.F
      !||    init_th                             ../engine/source/output/th/init_th.F
      !||    init_th0                            ../engine/source/output/th/init_th0.F
      !||    initvars_auto                       ../starter/source/elements/elbuf_init/initvars_auto.F
      !||    inivoid                             ../starter/source/elements/initia/inivoid.F
      !||    inivol_set                          ../starter/source/initial_conditions/inivol/inivol_set.F
      !||    inixfem                             ../engine/source/elements/xfem/inixfem.F
      !||    int18_law151_init                   ../engine/source/interfaces/int18/int18_law151_init.F
      !||    int18_law151_update                 ../engine/source/interfaces/int18/int18_law151_update.F
      !||    intal3                              ../engine/source/ale/inter/intal3.F
      !||    inter_sh_offset_dim                 ../engine/source/interfaces/shell_offset/inter_offset_dim.F90
      !||    inter_sh_offset_ini                 ../engine/source/interfaces/shell_offset/inter_offset_ini.F90
      !||    intfop2                             ../engine/source/interfaces/interf/intfop2.F
      !||    inttri                              ../engine/source/interfaces/intsort/inttri.F
      !||    joint_block_stiffness               ../engine/source/elements/joint/joint_block_stiffness.F
      !||    joint_elem_timestep                 ../engine/source/elements/joint/joint_elem_timestep.F
      !||    kine_seatbelt_force                 ../engine/source/tools/seatbelts/kine_seatbelt_force.F
      !||    kine_seatbelt_vel                   ../engine/source/tools/seatbelts/kine_seatbelt_vel.F
      !||    ktbuf_ini                           ../engine/source/implicit/imp_init.F
      !||    laser1                              ../engine/source/loads/laser/laser1.F
      !||    laser2                              ../engine/source/loads/laser/laser2.F
      !||    laser3                              ../engine/source/loads/laser/laser2.F
      !||    layini                              ../engine/source/elements/shell/coque/layini.F
      !||    layini1                             ../starter/source/elements/shell/coqueba/layini1.F
      !||    layini_xfe                          ../starter/source/elements/xfem/cbufxfe.F
      !||    lech3d                              ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    lectur                              ../engine/source/input/lectur.F
      !||    link_with_unique_main_cell          ../engine/source/interfaces/int22/link_with_unique_master_cell.F
      !||    lslocal                             ../starter/source/elements/xfem/lslocal.F
      !||    m11law                              ../engine/source/materials/mat/mat011/m11law.F
      !||    m11vs2                              ../engine/source/materials/mat/mat011/m11vs2.F
      !||    m11vs3                              ../engine/source/materials/mat/mat011/m11vs3.F
      !||    m1law8                              ../engine/source/materials/mat/mat001/m1law8.F
      !||    m20dcod                             ../starter/source/system/fsdcod.F
      !||    m24law                              ../engine/source/materials/mat/mat024/m24law.F
      !||    m2law8                              ../engine/source/materials/mat/mat002/m2law8.F
      !||    m2lawpi                             ../engine/source/materials/mat/mat002/m2lawpi.F
      !||    m37init                             ../starter/source/materials/mat/mat037/m37init.F
      !||    m3law8                              ../engine/source/materials/mat/mat003/m3law8.F
      !||    m51init                             ../starter/source/materials/mat/mat051/m51init.F
      !||    m51vois2                            ../engine/source/materials/mat/mat051/m51vois2.F
      !||    m51vois3                            ../engine/source/materials/mat/mat051/m51vois3.F
      !||    mat_elem_mod                        ../common_source/modules/mat_elem/mat_elem_mod.F
      !||    material_flow                       ../engine/source/tools/seatbelts/material_flow.F
      !||    matini                              ../starter/source/materials/mat_share/matini.F
      !||    mdama24                             ../engine/source/elements/solid/solidez/mdama24.F
      !||    meos8                               ../engine/source/materials/mat_share/meos8.F
      !||    monvol0                             ../engine/source/airbag/monvol0.F
      !||    mulaw_ib                            ../engine/source/elements/beam/mulaw_ib.F
      !||    mulawglc                            ../engine/source/materials/mat_share/mulawglc.F
      !||    mulawglcpinch                       ../engine/source/elements/shell/coqueba/mulawglcpinch.F
      !||    multi_buf2var                       ../engine/source/multifluid/multi_buf2var.F
      !||    multi_compute_dt                    ../engine/source/multifluid/multi_compute_dt.F
      !||    multi_computevolume                 ../engine/source/multifluid/multi_computevolume.F
      !||    multi_evolve_global                 ../engine/source/multifluid/multi_evolve_global.F
      !||    multi_evolve_partial                ../engine/source/multifluid/multi_evolve_partial.F
      !||    multi_fluxes_computation            ../engine/source/multifluid/multi_fluxes_computation.F
      !||    multi_fvm2fem                       ../engine/source/multifluid/multi_fvm2fem.F
      !||    multi_fvm_mod                       ../common_source/modules/ale/multi_fvm_mod.F
      !||    multi_globalize                     ../engine/source/multifluid/multi_globalize.F
      !||    multi_i18_force_poff                ../engine/source/interfaces/int18/multi_i18_force_poff.F
      !||    multi_muscl_fluxes_computation      ../engine/source/multifluid/multi_muscl_fluxes_computation.F
      !||    multi_muscl_gradients               ../engine/source/multifluid/multi_muscl_gradients.F
      !||    multi_pressure_equilibrium          ../engine/source/multifluid/multi_pressure_equilibrium.F
      !||    multi_timeevolution                 ../engine/source/multifluid/multi_timeevolution.F
      !||    multi_update_global                 ../engine/source/multifluid/multi_update_global.F
      !||    multi_update_partial                ../engine/source/multifluid/multi_update_partial.F
      !||    multi_var2buf                       ../engine/source/multifluid/multi_var2buf.F
      !||    multifluid_global_tdet              ../starter/source/multifluid/multifluid_global_tdet.F
      !||    multifluid_init2                    ../starter/source/multifluid/multifluid_init2.F
      !||    multifluid_init2t                   ../starter/source/multifluid/multifluid_init2t.F
      !||    multifluid_init3                    ../starter/source/multifluid/multifluid_init3.F
      !||    multifluid_init3t                   ../starter/source/multifluid/multifluid_init3t.F
      !||    nloc_count_solnod                   ../engine/source/elements/solid/solide/nloc_count_solnod.F90
      !||    nloc_dmg_init                       ../starter/source/materials/fail/nloc_dmg_init.F
      !||    nlocal_init_sta                     ../starter/source/materials/fail/nlocal_init_sta.F
      !||    nodal_schlieren                     ../engine/source/output/anim/generate/nodal_schlieren.F
      !||    nodald                              ../engine/source/output/anim/generate/nodald.F
      !||    nodaldt                             ../engine/source/output/anim/generate/nodaldt.F
      !||    nodalp                              ../engine/source/output/anim/generate/nodalp.F
      !||    nodalssp                            ../engine/source/output/anim/generate/nodalssp.F
      !||    nodalt                              ../engine/source/output/anim/generate/nodalt.F
      !||    nodalvfrac                          ../engine/source/output/anim/generate/nodalvfrac.F
      !||    nodalvol                            ../engine/source/output/anim/generate/nodalvol.F
      !||    nodalzvol                           ../engine/source/output/anim/generate/nodalzvol.F
      !||    noise                               ../engine/source/general_controls/computation/noise.F
      !||    ns_fvm_diffusion                    ../engine/source/multifluid/ns_fvm_diffusion.F
      !||    outp_arsz_ct                        ../engine/source/mpi/interfaces/spmd_outp.F
      !||    outp_arsz_st                        ../engine/source/mpi/interfaces/spmd_outp.F
      !||    outp_c_s                            ../engine/source/output/sty/outp_c_s.F
      !||    outp_c_t                            ../engine/source/output/sty/outp_c_t.F
      !||    outp_c_tf                           ../engine/source/output/sty/outp_c_t.F
      !||    outp_r_s                            ../engine/source/output/sty/outp_r_s.F
      !||    outp_r_t                            ../engine/source/output/sty/outp_r_t.F
      !||    outp_s_s                            ../engine/source/output/sty/outp_s_s.F
      !||    outp_s_t                            ../engine/source/output/sty/outp_s_t.F
      !||    outp_s_tt                           ../engine/source/output/sty/outp_s_t.F
      !||    outp_sp_s                           ../engine/source/output/sty/outp_sp_s.F
      !||    outp_sp_t                           ../engine/source/output/sty/outp_sp_t.F
      !||    outp_sp_tt                          ../engine/source/output/sty/outp_sp_t.F
      !||    output_div_u                        ../engine/source/output/anim/generate/output_div_u.F
      !||    output_schlieren                    ../engine/source/output/anim/generate/output_schlieren.F
      !||    parsorc                             ../engine/source/output/anim/generate/parsorc.F
      !||    parsorf                             ../engine/source/output/anim/generate/parsorf.F
      !||    pinit3                              ../starter/source/elements/beam/pinit3.F
      !||    pke3                                ../engine/source/elements/beam/pke3.F
      !||    pnoise                              ../engine/source/general_controls/computation/pnoise.F
      !||    porfor5                             ../engine/source/airbag/porfor5.F
      !||    porform5                            ../engine/source/airbag/porfor5.F
      !||    preinicrk3n                         ../starter/source/elements/xfem/preinicrk3N.F
      !||    preinicrk4n                         ../starter/source/elements/xfem/preinicrk4N.F
      !||    prelecflow                          ../engine/source/elements/solid/solide/prelecflow.F
      !||    projecig3d                          ../engine/source/elements/ige3d/projecig3d.F
      !||    q4init2                             ../starter/source/elements/solid_2d/quad4/q4init2.F
      !||    q4ke2                               ../engine/source/elements/solid_2d/quad4/q4ke2.F
      !||    qinit2                              ../starter/source/elements/solid_2d/quad/qinit2.F
      !||    r12ke3                              ../engine/source/elements/spring/r12ke3.F
      !||    r13ke3                              ../engine/source/elements/spring/r13ke3.F
      !||    r23forc3                            ../engine/source/elements/spring/r23forc3.F
      !||    r23law108                           ../engine/source/elements/spring/r23law108.F
      !||    r23law113                           ../engine/source/elements/spring/r23law113.F
      !||    r23law114                           ../engine/source/elements/spring/r23law114.F
      !||    r4ke3                               ../engine/source/elements/spring/r4ke3.F
      !||    r8ke3                               ../engine/source/elements/spring/r8ke3.F
      !||    radiatoff                           ../engine/source/constraints/thermic/radiatoff.F
      !||    rbyonf                              ../engine/source/constraints/general/rbody/rbyonf.F
      !||    rbypid                              ../engine/source/constraints/general/rbody/rbypid.F
      !||    rbysens                             ../engine/source/constraints/general/rbody/rbyonf.F
      !||    resol_init                          ../engine/source/engine/resol_init.F
      !||    rforc3                              ../engine/source/elements/spring/rforc3.F
      !||    rgwal1                              ../engine/source/ale/grid/rgwal1.F
      !||    rgwat2                              ../engine/source/interfaces/int09/rgwat2.F
      !||    rgwat3                              ../engine/source/interfaces/int09/rgwat3.F
      !||    rgwath                              ../engine/source/interfaces/int09/rgwath.F
      !||    rinit3                              ../starter/source/elements/spring/rinit3.F
      !||    ruser32ke3                          ../engine/source/elements/spring/ruser32ke3.F
      !||    s10deri3                            ../engine/source/elements/solid/solide10/s10deri3.F
      !||    s10derit3                           ../engine/source/elements/solid/solide10/s10derit3.F
      !||    s10derito3                          ../engine/source/elements/solid/solide10/s10derito3.F
      !||    s10init3                            ../starter/source/elements/solid/solide10/s10init3.F
      !||    s10jaci3                            ../starter/source/elements/solid/solide10/s10jaci3.F
      !||    s10ke3                              ../engine/source/elements/solid/solide10/s10ke3.F
      !||    s10upd11t12                         ../engine/source/elements/solid/solide10/s10upd11t12.F
      !||    s10volnodt3                         ../engine/source/elements/solid/solide4_sfem/s10volnodt3.F
      !||    s16init3                            ../starter/source/elements/thickshell/solide16/s16init3.F
      !||    s16sigp3                            ../engine/source/elements/thickshell/solide16/s16sigp3.F
      !||    s20init3                            ../starter/source/elements/solid/solide20/s20init3.F
      !||    s20ke3                              ../engine/source/elements/solid/solide20/s20ke3.F
      !||    s4alesfem                           ../engine/source/elements/solid/solide4_sfem/s4alesfem.F
      !||    s4init3                             ../starter/source/elements/solid/solide4/s4init3.F
      !||    s4ke3                               ../engine/source/elements/solid/solide4/s4ke3.F
      !||    s4lagsfem                           ../engine/source/elements/solid/solide4_sfem/s4lagsfem.F
      !||    s6cfint_reg                         ../engine/source/elements/thickshell/solide6c/s6cfint_reg.F
      !||    s6cinit3                            ../starter/source/elements/thickshell/solide6c/s6cinit3.F
      !||    s6cke3                              ../engine/source/elements/thickshell/solide6c/s6cke3.F
      !||    s8_is17jac_i                        ../engine/source/elements/solid/solide8e/s8_is17jac_i.F
      !||    s8cfint_reg                         ../engine/source/elements/thickshell/solide8c/s8cfint_reg.F
      !||    s8cinit3                            ../starter/source/elements/thickshell/solide8c/s8cinit3.F
      !||    s8cke3                              ../engine/source/elements/thickshell/solide8c/s8cke3.F
      !||    s8e_pij                             ../starter/source/elements/solid/solide8z/s8zderi3.F
      !||    s8e_sigp                            ../engine/source/elements/solid/solide8e/s8e_sig.F
      !||    s8fint3                             ../engine/source/elements/solid/solide8/s8fint3.F
      !||    s8fupd11t12                         ../engine/source/elements/solid/solide8e/s8fupd11t12.F
      !||    s8ske3                              ../engine/source/elements/solid/solide8s/s8ske3.F
      !||    s8zinit3                            ../starter/source/elements/solid/solide8z/s8zinit3.F
      !||    s8zke3                              ../engine/source/elements/solid/solide8z/s8zke3.F
      !||    s_user                              ../engine/source/output/sty/s_user.F
      !||    scaleini                            ../starter/source/elements/initia/scaleini.F
      !||    scfint_reg                          ../engine/source/elements/thickshell/solidec/scfint_reg.F
      !||    schlieren_buffer_gathering          ../engine/source/output/anim/generate/schlieren_buffer_gathering.F
      !||    scinit3                             ../starter/source/elements/thickshell/solidec/scinit3.F
      !||    sconnect_off                        ../engine/source/elements/solid/sconnect/sconnect_off.F
      !||    seatbelt_reduction_factor           ../engine/source/tools/seatbelts/seatbelt_reduction_factor.F
      !||    seggetv                             ../engine/source/interfaces/interf/seggetv.F
      !||    seteloff                            ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    sfint_reg                           ../engine/source/elements/solid/solide/sfint_reg.F
      !||    shell_local_frame                   ../engine/source/output/dynain/shell_rota.F
      !||    shell_offset_ini                    ../starter/source/elements/shell/shell_offset/shell_offset_ini.F90
      !||    shell_rota                          ../engine/source/output/dynain/shell_rota.F
      !||    sigeps02g                           ../engine/source/materials/mat/mat002/sigeps02g.F
      !||    sigeps104                           ../engine/source/materials/mat/mat104/sigeps104.F
      !||    sigeps104c                          ../engine/source/materials/mat/mat104/sigeps104c.F
      !||    sigeps105                           ../engine/source/materials/mat/mat105/sigeps105.F
      !||    sigeps107                           ../engine/source/materials/mat/mat107/sigeps107.F
      !||    sigeps107c                          ../engine/source/materials/mat/mat107/sigeps107c.F
      !||    sigeps112                           ../engine/source/materials/mat/mat112/sigeps112.F
      !||    sigeps112c                          ../engine/source/materials/mat/mat112/sigeps112c.F
      !||    sigeps121                           ../engine/source/materials/mat/mat121/sigeps121.F
      !||    sigeps121c                          ../engine/source/materials/mat/mat121/sigeps121c.F
      !||    sigeps122                           ../engine/source/materials/mat/mat122/sigeps122.F
      !||    sigeps122c                          ../engine/source/materials/mat/mat122/sigeps122c.F
      !||    sigeps22g                           ../engine/source/materials/mat/mat022/sigeps22g.F
      !||    sigeps37_single_cell                ../engine/source/interfaces/int22/sigeps37_single_cell.F
      !||    sigeps51                            ../engine/source/materials/mat/mat051/sigeps51.F
      !||    sigeps97                            ../engine/source/materials/mat/mat097/sigeps97.F
      !||    sigin3b                             ../starter/source/elements/solid/solid8p/sigin3b.F
      !||    sigrota                             ../engine/source/output/anim/generate/sigrota.F
      !||    sigrota_xfe                         ../engine/source/output/anim/generate/sigrota_xfe.F
      !||    sini43                              ../starter/source/elements/solid/sconnect/sini43.F
      !||    sinit22_fvm                         ../engine/source/interfaces/int22/sinit22_fvm.F
      !||    sinit3                              ../starter/source/elements/solid/solide/sinit3.F
      !||    sms_auto_dt                         ../starter/source/ams/sms_auto_dt.F
      !||    sms_build_mat_2                     ../engine/source/ams/sms_build_mat_2.F
      !||    soltosph_on1                        ../engine/source/elements/sph/soltosph_on1.F
      !||    soltosph_on12                       ../engine/source/elements/sph/soltosph_on1.F
      !||    soltosph_on2                        ../engine/source/elements/sph/soltosph_on2.F
      !||    soltospha                           ../engine/source/elements/sph/soltospha.F
      !||    soltosphf                           ../engine/source/elements/sph/soltosph.F
      !||    soltosphp                           ../engine/source/elements/sph/soltosph.F
      !||    spbrm_pre                           ../engine/source/implicit/imp_solv.F
      !||    spechan                             ../engine/source/elements/sph/spechan.F
      !||    spgauge                             ../engine/source/elements/sph/spgauge.F
      !||    sphprep                             ../engine/source/elements/sph/sphprep.F
      !||    spinit3                             ../starter/source/elements/sph/spinit3.F
      !||    splissv                             ../engine/source/elements/sph/splissv.F
      !||    spmd_fvb_switch                     ../engine/source/mpi/airbags/spmd_fvb_switch.F
      !||    spmd_l11vois                        ../engine/source/mpi/fluid/spmd_cfd.F
      !||    spmd_l51vois                        ../engine/source/mpi/fluid/spmd_cfd.F
      !||    sponfprs                            ../engine/source/elements/sph/sponfprs.F
      !||    sponof1                             ../engine/source/elements/sph/sponof1.F
      !||    sponof2                             ../engine/source/elements/sph/sponof2.F
      !||    spwfvis                             ../engine/source/elements/sph/spwfvis.F
      !||    stat_beam_mp                        ../engine/source/output/sta/stat_beam_mp.F
      !||    stat_beam_spmd                      ../engine/source/output/sta/stat_beam_spmd.F
      !||    stat_brick_mp                       ../engine/source/output/sta/stat_brick_mp.F
      !||    stat_brick_spmd                     ../engine/source/output/sta/stat_brick_spmd.F
      !||    stat_c_auxf                         ../engine/source/output/sta/stat_c_auxf.F
      !||    stat_c_epspf                        ../engine/source/output/sta/stat_c_epspf.F
      !||    stat_c_off                          ../engine/source/output/sta/stat_c_off.F
      !||    stat_c_orth_loc                     ../engine/source/output/sta/stat_c_orth_loc.F
      !||    stat_c_straf                        ../engine/source/output/sta/stat_c_straf.F
      !||    stat_c_strafg                       ../engine/source/output/sta/stat_c_strafg.F
      !||    stat_c_strsf                        ../engine/source/output/sta/stat_c_strsf.F
      !||    stat_c_strsfg                       ../engine/source/output/sta/stat_c_strsfg.F
      !||    stat_c_thk                          ../engine/source/output/sta/stat_c_thk.F
      !||    stat_inimap1d_file_spmd             ../engine/source/output/sta/stat_inimap1d_file_spmd.F
      !||    stat_inimap1d_spmd                  ../engine/source/output/sta/stat_inimap1d_spmd.F
      !||    stat_inimap2d_file_spmd             ../engine/source/output/sta/stat_inimap2d_file_spmd.F
      !||    stat_inimap2d_spmd                  ../engine/source/output/sta/stat_inimap2d_spmd.F
      !||    stat_p_aux                          ../engine/source/output/sta/stat_p_aux.F
      !||    stat_p_full                         ../engine/source/output/sta/stat_p_full.F
      !||    stat_quad_mp                        ../engine/source/output/sta/stat_quad_mp.F
      !||    stat_quad_spmd                      ../engine/source/output/sta/stat_quad_spmd.F
      !||    stat_r_full                         ../engine/source/output/sta/stat_r_full.F
      !||    stat_s_auxf                         ../engine/source/output/sta/stat_s_auxf.F
      !||    stat_s_eref                         ../engine/source/output/sta/stat_s_eref.F
      !||    stat_s_ortho                        ../engine/source/output/sta/stat_s_ortho.F
      !||    stat_s_straf                        ../engine/source/output/sta/stat_s_straf.F
      !||    stat_s_strsf                        ../engine/source/output/sta/stat_s_strsf.F
      !||    stat_shel_mp                        ../engine/source/output/sta/stat_shel_mp.F
      !||    stat_shel_spmd                      ../engine/source/output/sta/stat_shel_spmd.F
      !||    stat_size_c                         ../engine/source/output/sta/stat_size.F
      !||    stat_spring_mp                      ../engine/source/output/sta/stat_spring_mp.F
      !||    stat_spring_spmd                    ../engine/source/output/sta/stat_spring_spmd.F
      !||    stat_t_full                         ../engine/source/output/sta/stat_t_full.F
      !||    stat_truss_mp                       ../engine/source/output/sta/stat_truss_mp.F
      !||    stat_truss_spmd                     ../engine/source/output/sta/stat_truss_spmd.F
      !||    strn_tenscor3                       ../engine/source/output/h3d/h3d_results/h3d_strn_tenscor3.F
      !||    strs_tenscor3                       ../engine/source/output/h3d/h3d_results/strs_tenscor3.F
      !||    suinit3                             ../starter/source/elements/elbuf_init/suinit3.F
      !||    switch_to_dtnoda                    ../engine/source/time_step/switch_to_dtnoda.F
      !||    szhour3                             ../engine/source/elements/solid/solidez/szhour3.F
      !||    szhour3_or                          ../engine/source/elements/solid/solidez/szhour3_or.F
      !||    tagoff3n                            ../engine/source/interfaces/interf/chkstfn3.F
      !||    tencgps1                            ../engine/source/output/anim/generate/tensorc.F
      !||    tencgps2                            ../engine/source/output/anim/generate/tensorc.F
      !||    tensgps1                            ../engine/source/output/anim/generate/tensor6.F
      !||    tensgps2                            ../engine/source/output/anim/generate/tensor6.F
      !||    tensgps3                            ../engine/source/output/anim/generate/tensor6.F
      !||    tensgps_skin                        ../engine/source/output/anim/generate/tensor6.F
      !||    tensgpstrain                        ../engine/source/output/anim/generate/tensgpstrain.F
      !||    tensor0                             ../engine/source/output/anim/generate/tensor0.F
      !||    tensorc                             ../engine/source/output/anim/generate/tensorc.F
      !||    tensorc_crk                         ../engine/source/output/anim/generate/tensorc_crk.F
      !||    tensorc_ply                         ../engine/source/output/anim/generate/tensorc_ply.F
      !||    tensors                             ../engine/source/output/anim/generate/tensor6.F
      !||    tforc3                              ../engine/source/elements/truss/tforc3.F
      !||    thcluster                           ../engine/source/output/th/thcluster.F
      !||    thcoq                               ../engine/source/output/th/thcoq.F
      !||    thermexpc                           ../engine/source/materials/mat_share/thermexpc.F
      !||    thermexppi                          ../engine/source/elements/beam/thermexpp.F
      !||    thick_ilev                          ../starter/source/elements/xfem/thick_ilev.F
      !||    thickvar                            ../engine/source/elements/shell/coque/thickvar.F
      !||    thnst                               ../engine/source/output/th/thnst.F
      !||    thpout                              ../engine/source/output/th/thpout.F
      !||    thquad                              ../engine/source/output/th/thquad.F
      !||    thres                               ../engine/source/output/th/thres.F
      !||    thres_count                         ../engine/source/output/th/thres_count.F
      !||    thsol                               ../engine/source/output/th/thsol.F
      !||    thsph                               ../engine/source/output/th/thsph.F
      !||    thtrus                              ../engine/source/output/th/thtrus.F
      !||    tinit3                              ../starter/source/elements/truss/tinit3.F
      !||    tke3                                ../engine/source/elements/truss/tke3.F
      !||    tm_dmg_shells                       ../engine/source/output/outmaxsubr.F
      !||    tm_dmg_solid                        ../engine/source/output/outmaxsubr.F
      !||    tm_dmgl25_shell                     ../engine/source/output/outmaxsubr.F
      !||    tm_seq_shell                        ../engine/source/output/outmaxsubr.F
      !||    tm_seq_solid                        ../engine/source/output/outmaxsubr.F
      !||    tm_sig_shell                        ../engine/source/output/outmaxsubr.F
      !||    tm_sig_solid                        ../engine/source/output/outmaxsubr.F
      !||    tm_stra_shell                       ../engine/source/output/outmaxsubr.F
      !||    tm_stra_solid                       ../engine/source/output/outmaxsubr.F
      !||    upd_tmax                            ../engine/source/output/upd_outmax.F
      !||    update_slipring                     ../engine/source/tools/seatbelts/update_slipring.F
      !||    upenric3_n3                         ../engine/source/elements/xfem/upenric3_nx.F
      !||    upenric3_n4                         ../engine/source/elements/xfem/upenric3_nx.F
      !||    upoffc                              ../engine/source/elements/xfem/upoffc.F
      !||    upofftg                             ../engine/source/elements/xfem/upofftg.F
      !||    upxfem1                             ../engine/source/elements/xfem/upxfem1.F
      !||    upxfem_tagxp                        ../engine/source/elements/xfem/upxfem_tagxp.F
      !||    velvecc22                           ../engine/source/output/anim/generate/velvec.F
      !||    velvecz22                           ../engine/source/output/anim/generate/velvecz22.F
      !||    voln22                              ../engine/source/interfaces/int22/voln22.F
      !||    volpvgb                             ../engine/source/airbag/volpvg.F
      !||    w_elbuf_str                         ../engine/source/elements/elbuf/w_elbuf_str.F
      !||    write_buf_law51                     ../engine/source/materials/mat/mat051/write_buf_law51.F
      !||    write_cut_cell_buffer               ../engine/source/interfaces/int22/write_cut_cell_buffer.F
      !||    xfeoff                              ../engine/source/elements/xfem/xfeoff.F
      !||    xforc3                              ../engine/source/elements/xelem/xforc3.F
      !||    xinit3                              ../starter/source/elements/xelem/xinit3.F
      !||    zerovars_auto                       ../starter/source/elements/elbuf_init/zerovars_auto.F
      !||====================================================================
      MODULE ELBUFDEF_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c define type ELBUF_STRUCT_ for element buffer structure array
c=======================================================================      
c
      TYPE G_BUFEL_      ! material and property variables (mean values for each element)
        integer  NVAR_GLOB   
        integer  G_NOFF   
        integer  G_IERR
        integer  G_OFF    
        integer  G_GAMA   
        integer  G_SMSTR  
        integer  G_HOURG  
        integer  G_BFRAC  
        integer  G_EINT   
        integer  G_EINS   
        integer  G_RHO    
        integer  G_QVIS   
        integer  G_DELTAX 
        integer  G_VOL    
        integer  G_EPSD   
        integer  G_EPSQ
        integer  G_PLA    
        integer  G_TEMP   
        integer  G_TB     
        integer  G_RK     
        integer  G_RE     
        integer  G_SIG    
        integer  G_FOR    
        integer  G_MOM    
        integer  G_THK    
        integer  G_TAG22
        integer  G_STRA  
        integer  G_SIGI   
        integer  G_DMG   
        integer  G_FORPG    
        integer  G_MOMPG
        integer  G_GAMA_R   
C
        integer  G_FORPGPINCH  
        integer  G_MOMPGPINCH 
        integer  G_EPGPINCHXZ
        integer  G_EPGPINCHYZ 
        integer  G_EPGPINCHZZ
C    
        integer  G_STRPG
        integer  G_UELR
        integer  G_UELR1
        integer  G_DAMDL
        integer  G_FORTH    
        integer  G_EINTTH    
        integer  G_FILL
        integer  G_SEQ
        integer  G_STRW  
        integer  G_STRWPG  
        integer  G_THK_I    
        integer  G_JAC_I   
        integer  G_DT
        integer  G_ISMS 
        integer  G_STRHG
        integer  G_BPRELD        ! bolt preloading
        integer  G_ABURN
        integer  G_MU
        integer  G_PLANL
        integer  G_EPSDNL
        integer  G_TEMPG
        integer  G_COR_NF        ! Nodal forces for corotational formulation
        integer  G_COR_FR        ! Local frame for corotational formulation
        integer  G_COR_XR        ! Reference local coordinates for corotational formulation
        integer  G_MAXFRAC 
        integer  G_MAXEPS 
        INTEGER  G_BETAORTH
        integer  G_AMU
        INTEGER  G_TSAIWU
        INTEGER  G_DMGSCL
        INTEGER  G_SH_IOFFSET
c---
C    - 1D - elem (TRUSS, BEAM, SPRING) 
        integer  G_AREA
        integer  G_SKEW
        integer  G_LENGTH
        integer  G_TOTDEPL
        integer  G_TOTROT
        integer  G_FOREP
        integer  G_MOMEP
        integer  G_DEP_IN_TENS
        integer  G_DEP_IN_COMP
        integer  G_ROT_IN_TENS
        integer  G_ROT_IN_COMP
        integer  G_POSX
        integer  G_POSY
        integer  G_POSZ
        integer  G_POSXX
        integer  G_POSYY
        integer  G_POSZZ
        integer  G_YIELD
        integer  G_LENGTH_ERR
        integer  G_DV
        integer  G_DFS
        integer  G_SKEW_ERR
        integer  G_E6
        integer  G_RUPTCRIT
        integer  G_MASS
        integer  G_V_REPCVT
        integer  G_VR_REPCVT
        integer  G_NUVAR
        integer  G_NUVARN
        integer  G_DEFINI
        integer  G_FORINI
        integer  G_INIFRIC
        integer  G_ETOTSH
        integer  G_SKEW_ID
C
C    -  for seatbelt elements
        integer  G_SLIPRING_ID
        integer  G_SLIPRING_FRAM_ID
        integer  G_SLIPRING_STRAND
        integer  G_RETRACTOR_ID
        integer  G_RINGSLIP
        integer  G_ADD_NODE
        integer  G_UPDATE
        integer  G_FRAM_FACTOR
        integer  G_INTVAR
c---
        integer  G_DT_PITER
c---
        integer  G_IDT_TSH
c-------  max_historic variables     
        integer  G_TM_YIELD
        integer  G_TM_SEQ
        integer  G_TM_EINT
        integer  G_TM_DMG
        integer  G_TM_SIG  ! max(p1)&min(p3) 3 for 2D, 6 for 3D
        integer  G_TM_STRA ! max(p1)&min(p3)
c---
        integer, DIMENSION(:) , POINTER ::   NOFF  
        integer, DIMENSION(:) , POINTER ::   IERR  
        my_real, DIMENSION(:) , POINTER ::   OFF   
        my_real, DIMENSION(:) , POINTER ::   GAMA  
        DOUBLE PRECISION, DIMENSION(:) , POINTER ::   SMSTR 
        my_real, DIMENSION(:) , POINTER ::   HOURG 
        my_real, DIMENSION(:) , POINTER ::   BFRAC    
        my_real, DIMENSION(:) , POINTER ::   EINT  
        my_real, DIMENSION(:) , POINTER ::   EINS  
        my_real, DIMENSION(:) , POINTER ::   RHO   
        my_real, DIMENSION(:) , POINTER ::   QVIS       
        my_real, DIMENSION(:) , POINTER ::   DELTAX                 
        my_real, DIMENSION(:) , POINTER ::   VOL                            
        my_real, DIMENSION(:) , POINTER ::   EPSD  
        my_real, DIMENSION(:) , POINTER ::   EPSQ
        my_real, DIMENSION(:) , POINTER ::   PLA   
        my_real, DIMENSION(:) , POINTER ::   TEMP  
        my_real, DIMENSION(:) , POINTER ::   TB    
        my_real, DIMENSION(:) , POINTER ::   RK    
        my_real, DIMENSION(:) , POINTER ::   RE    
        my_real, DIMENSION(:) , POINTER ::   SIG                          
        my_real, DIMENSION(:) , POINTER ::   FOR                        
        my_real, DIMENSION(:) , POINTER ::   MOM                        
        my_real, DIMENSION(:) , POINTER ::   THK                        
        my_real, DIMENSION(:) , POINTER ::   TAG22                    
        my_real, DIMENSION(:) , POINTER ::   STRA                     
        my_real, DIMENSION(:) , POINTER ::   SIGI                     
        my_real, DIMENSION(:) , POINTER ::   DMG                     
        my_real, DIMENSION(:) , POINTER ::   FORPG   ! mean gauss point value    
        my_real, DIMENSION(:) , POINTER ::   MOMPG
        my_real, DIMENSION(:) , POINTER ::   GAMA_R  ! Co-rotational local sys  
C
        my_real, DIMENSION(:) , POINTER ::   FORPGPINCH 
        my_real, DIMENSION(:) , POINTER ::   MOMPGPINCH
        my_real, DIMENSION(:) , POINTER ::   EPGPINCHXZ
        my_real, DIMENSION(:) , POINTER ::   EPGPINCHYZ
        my_real, DIMENSION(:) , POINTER ::   EPGPINCHZZ
C                        
        my_real, DIMENSION(:) , POINTER ::   STRPG                  
        my_real, DIMENSION(:) , POINTER ::   TEMPG   
        my_real, DIMENSION(:) , POINTER ::   UELR   !  failure global variable
        my_real, DIMENSION(:) , POINTER ::   UELR1  !  failure global variable
        my_real, DIMENSION(:) , POINTER ::   DAMDL  !  delamination failure (law25)
        my_real, DIMENSION(:) , POINTER ::   FORTH
        my_real, DIMENSION(:) , POINTER ::   EINTTH                                                
        my_real, DIMENSION(:) , POINTER ::   FILL
        my_real, DIMENSION(:) , POINTER ::   SEQ
        my_real, DIMENSION(:) , POINTER ::   THK_I   !---- add for Ismstr=10 shell (XFEM not done) STRWPG total anti-symme curvature                         
        my_real, DIMENSION(:) , POINTER ::   STRW 
        my_real, DIMENSION(:) , POINTER ::   STRWPG
        my_real, DIMENSION(:) , POINTER ::   JAC_I   !--------inversed [J]
        my_real, DIMENSION(:) , POINTER ::   DT
        my_real, DIMENSION(:) , POINTER ::   ABURN 
        my_real, DIMENSION(:) , POINTER ::   MU 
        integer, DIMENSION(:) , POINTER ::   ISMS 
        integer, DIMENSION(:) , POINTER ::   SH_IOFFSET 
        my_real, DIMENSION(:) , POINTER ::   BPRELD  ! Bolt preloading 
        my_real, DIMENSION(:) , POINTER ::   COR_NF  ! Corotational nodal forces 
        my_real, DIMENSION(:) , POINTER ::   COR_FR  ! Corotational frame 
        my_real, DIMENSION(:) , POINTER ::   COR_XR  ! Corotational reference coordinates 
        my_real, DIMENSION(:) , POINTER ::   MAXFRAC
        my_real, DIMENSION(:) , POINTER ::   MAXEPS 
        my_real, DIMENSION(:) , POINTER ::   BETAORTH 
        my_real, DIMENSION(:) , POINTER ::   AMU
c---
C    - 1D - elem (TRUSS, BEAM, SPRING)
        my_real, DIMENSION(:) , POINTER ::   AREA
        my_real, DIMENSION(:) , POINTER ::   SKEW
        my_real, DIMENSION(:) , POINTER ::   LENGTH
        my_real, DIMENSION(:) , POINTER ::   TOTDEPL
        my_real, DIMENSION(:) , POINTER ::   TOTROT
        my_real, DIMENSION(:) , POINTER ::   FOREP
        my_real, DIMENSION(:) , POINTER ::   MOMEP
        my_real, DIMENSION(:) , POINTER ::   DEP_IN_TENS
        my_real, DIMENSION(:) , POINTER ::   DEP_IN_COMP
        my_real, DIMENSION(:) , POINTER ::   ROT_IN_TENS
        my_real, DIMENSION(:) , POINTER ::   ROT_IN_COMP
        my_real, DIMENSION(:) , POINTER ::   POSX
        my_real, DIMENSION(:) , POINTER ::   POSY
        my_real, DIMENSION(:) , POINTER ::   POSZ
        my_real, DIMENSION(:) , POINTER ::   POSXX
        my_real, DIMENSION(:) , POINTER ::   POSYY
        my_real, DIMENSION(:) , POINTER ::   POSZZ
        my_real, DIMENSION(:) , POINTER ::   YIELD
        my_real, DIMENSION(:) , POINTER ::   LENGTH_ERR
        my_real, DIMENSION(:) , POINTER ::   DV
        my_real, DIMENSION(:) , POINTER ::   DFS
        my_real, DIMENSION(:) , POINTER ::   SKEW_ERR
        my_real, DIMENSION(:) , POINTER ::   E6
        my_real, DIMENSION(:) , POINTER ::   RUPTCRIT
        my_real, DIMENSION(:) , POINTER ::   MASS
        my_real, DIMENSION(:) , POINTER ::   V_REPCVT
        my_real, DIMENSION(:) , POINTER ::   VR_REPCVT
        my_real, DIMENSION(:) , POINTER ::   VAR
        my_real, DIMENSION(:) , POINTER ::   VARN
        my_real, DIMENSION(:) , POINTER ::   DEFINI
        my_real, DIMENSION(:) , POINTER ::   FORINI
        my_real, DIMENSION(:) , POINTER ::   INIFRIC
        my_real, DIMENSION(:) , POINTER ::   STRHG
        my_real, DIMENSION(:) , POINTER ::   ETOTSH
        integer, DIMENSION(:) , POINTER ::   SKEW_ID
        TYPE (FAIL_LOC_) , DIMENSION(:) , POINTER ::   FAIL
C
C    -  for seatbelt elements
        integer, DIMENSION(:) , POINTER ::   SLIPRING_ID
        integer, DIMENSION(:) , POINTER ::   SLIPRING_FRAM_ID
        integer, DIMENSION(:) , POINTER ::   SLIPRING_STRAND
        integer, DIMENSION(:) , POINTER ::   RETRACTOR_ID
        my_real, DIMENSION(:) , POINTER ::   RINGSLIP
        integer, DIMENSION(:) , POINTER ::   ADD_NODE
        integer, DIMENSION(:) , POINTER ::   UPDATE
        my_real, DIMENSION(:) , POINTER ::   FRAM_FACTOR
        my_real, DIMENSION(:) , POINTER ::   INTVAR
c---
        my_real, DIMENSION(:) , POINTER ::   DT_PITER ! Tetra10 Iterative Power for Time Step Computation
        integer, DIMENSION(:) , POINTER ::   IDT_TSH  
c-------  max_historic variables     
        my_real, DIMENSION(:) , POINTER ::   TM_YIELD   
        my_real, DIMENSION(:) , POINTER ::   TM_SEQ   
        my_real, DIMENSION(:) , POINTER ::   TM_EINT   
        my_real, DIMENSION(:) , POINTER ::   TM_DMG   
        my_real, DIMENSION(:) , POINTER ::   TM_SIG1
        my_real, DIMENSION(:) , POINTER ::   TM_STRA1
        my_real, DIMENSION(:) , POINTER ::   TM_SIG3
        my_real, DIMENSION(:) , POINTER ::   TM_STRA3
c---  work array
        my_real, DIMENSION(:) , POINTER ::   TM_PSIG
        my_real, DIMENSION(:) , POINTER ::   TM_PSTRA
c---
      END TYPE G_BUFEL_


      TYPE L_BUFEL_      ! element variables per integration point
        integer  MLAW    ! material law type          
        integer  LawID   ! material law ID    
        my_real, DIMENSION(:) , POINTER ::   OFF    
        my_real, DIMENSION(:) , POINTER ::   GAMA   
        my_real, DIMENSION(:) , POINTER ::   STRA   
        my_real, DIMENSION(:) , POINTER ::   FRAC          
        my_real, DIMENSION(:) , POINTER ::   BFRAC
        my_real, DIMENSION(:) , POINTER ::   EINT   
        my_real, DIMENSION(:) , POINTER ::   EINS   
        my_real, DIMENSION(:) , POINTER ::   RHO    
        my_real, DIMENSION(:) , POINTER ::   DP_DRHO
        my_real, DIMENSION(:) , POINTER ::   QVIS   
        my_real, DIMENSION(:) , POINTER ::   DELTAX 
        my_real, DIMENSION(:) , POINTER ::   VOL    
        my_real, DIMENSION(:) , POINTER ::   EPSA   
        my_real, DIMENSION(:) , POINTER ::   EPSD   
        my_real, DIMENSION(:) , POINTER ::   EPSQ   
        my_real, DIMENSION(:) , POINTER ::   EPSF   
        my_real, DIMENSION(:) , POINTER ::   PLA    
        my_real, DIMENSION(:) , POINTER ::   TEMP   
        my_real, DIMENSION(:) , POINTER ::   TB     
        my_real, DIMENSION(:) , POINTER ::   RK     
        my_real, DIMENSION(:) , POINTER ::   RE         
        my_real, DIMENSION(:) , POINTER ::   VK     
        my_real, DIMENSION(:) , POINTER ::   SF     
        my_real, DIMENSION(:) , POINTER ::   ROB    
        my_real, DIMENSION(:) , POINTER ::   DAM    
        my_real, DIMENSION(:) , POINTER ::   DSUM   
        my_real, DIMENSION(:) , POINTER ::   DGLO   
        my_real, DIMENSION(:) , POINTER ::   CRAK   
        my_real, DIMENSION(:) , POINTER ::   ANG    
        my_real, DIMENSION(:) , POINTER ::   EPE    
        my_real, DIMENSION(:) , POINTER ::   EPC    
        my_real, DIMENSION(:) , POINTER ::   XST    
        my_real, DIMENSION(:) , POINTER ::   SSP    
        my_real, DIMENSION(:) , POINTER ::   Z      
        my_real, DIMENSION(:) , POINTER ::   VISC   
        my_real, DIMENSION(:) , POINTER ::   SIGL   
        my_real, DIMENSION(:) , POINTER ::   SIGV   
        my_real, DIMENSION(:) , POINTER ::   SIGA   
        my_real, DIMENSION(:) , POINTER ::   SIGB   
        my_real, DIMENSION(:) , POINTER ::   SIGC   
        my_real, DIMENSION(:) , POINTER ::   SIGD   
        my_real, DIMENSION(:) , POINTER ::   SIGF   
        my_real, DIMENSION(:) , POINTER ::   SIG    
        my_real, DIMENSION(:) , POINTER ::   SIGPLY    
        my_real, DIMENSION(:) , POINTER ::   FOR    
        my_real, DIMENSION(:) , POINTER ::   MOM
        my_real, DIMENSION(:) , POINTER ::   THK    
        DOUBLE PRECISION, DIMENSION(:) , POINTER ::   SMSTR    
        my_real, DIMENSION(:) , POINTER ::   DMG 
        my_real, DIMENSION(:) , POINTER ::   FORTH
        my_real, DIMENSION(:) , POINTER ::   EINTTH    
        my_real, DIMENSION(:) , POINTER ::   SEQ
        my_real, DIMENSION(:) , POINTER ::   JAC_I    
        my_real, DIMENSION(:) , POINTER ::   FAC_YLD  
        my_real, DIMENSION(:) , POINTER ::   ABURN
        my_real, DIMENSION(:) , POINTER ::   MU
        my_real, DIMENSION(:) , POINTER ::   PIJ   !--------[NI,J] for Imstr10
        DOUBLE PRECISION, DIMENSION(:) , POINTER ::   VOL0DP
        my_real, DIMENSION(:) , POINTER ::   PLANL
        my_real, DIMENSION(:) , POINTER ::   EPSDNL            
        my_real, DIMENSION(:) , POINTER ::   DMGSCL
        my_real, DIMENSION(:) , POINTER ::   TSAIWU
      END TYPE L_BUFEL_                             

      TYPE BUF_PROP_
        my_real, DIMENSION(:)  , POINTER ::  VAR
        my_real, DIMENSION(:)  , POINTER ::  VARN
      END TYPE BUF_PROP_

!--------------------------------------------------------------------------------      
!     Non-local buffer for regularization in the shell thickness
      TYPE BUF_NLOC_
        my_real, DIMENSION(:,:), POINTER :: MASSTH ! Embedded wire nodal masses
        my_real, DIMENSION(:,:), POINTER :: UNLTH  ! Non-local cumulated variable at nodes
        my_real, DIMENSION(:,:), POINTER :: VNLTH  ! Non-local velocities
        my_real, DIMENSION(:,:), POINTER :: FNLTH  ! Non-local forces
      END TYPE BUF_NLOC_
!     Non-local buffer for regularization in the thickshell thickness
      TYPE BUF_NLOCTS_
        my_real, DIMENSION(:,:), POINTER :: MASSTH ! Embedded wire nodal masses
        my_real, DIMENSION(:,:), POINTER :: UNLTH  ! Non-local cumulated variable at nodes
        my_real, DIMENSION(:,:), POINTER :: VNLTH  ! Non-local velocities
        my_real, DIMENSION(:,:), POINTER :: FNLTH  ! Non-local forces
      END TYPE BUF_NLOCTS_
!     Non-local buffer for brick elements geometry configuration
      TYPE BUF_NLOCS_
        INTEGER, DIMENSION(:)  , ALLOCATABLE :: NL_ISOLNOD ! Number of effective nodes (NEL)
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: NL_SOLNOD  ! Identifiers of effectives nodes (8,NEL)
      END TYPE BUF_NLOCS_
!--------------------------------------------------------------------------------

      TYPE BUF_EOS_
        my_real, DIMENSION(:)  , POINTER ::  VAR 
      END TYPE BUF_EOS_

      TYPE BUF_PORO_
        my_real, DIMENSION(:)  , POINTER ::  VAR 
      END TYPE BUF_PORO_

      TYPE BUF_VISC_
c        integer  ILAW    ! type de loi de viscosite
c        integer  NVAR
        my_real, DIMENSION(:)  , POINTER ::  VAR 
      END TYPE BUF_VISC_

      TYPE BUF_XFEM_       ! Buffer des elements XFEM crees par la fissuration
c-------  layer variables     
        integer  LY_SMSTR
        integer  LY_HOURG
        my_real, DIMENSION(:) , POINTER ::   DMG
        my_real, DIMENSION(:) , POINTER ::   GAMA
        my_real, DIMENSION(:) , POINTER ::   DIRA
        my_real, DIMENSION(:) , POINTER ::   DIRB
        my_real, DIMENSION(:) , POINTER ::   PLAPT
        my_real, DIMENSION(:) , POINTER ::   SIGPT
        my_real, DIMENSION(:) , POINTER ::   SMSTR
        my_real, DIMENSION(:) , POINTER ::   HOURG
        TYPE (G_BUFEL_)                                :: XGBUF   ! global variables
        TYPE (L_BUFEL_)  , DIMENSION(:,:,:)  , POINTER :: XLBUF   ! local variables (nptr,npts,nptt)
        TYPE (BUF_MAT_)  , DIMENSION(:,:,:)  , POINTER :: XMAT    ! material buffer
        TYPE (BUF_FAIL_) , DIMENSION(:,:,:)  , POINTER :: XFAIL   ! failure models
      END TYPE BUF_XFEM_
 
      TYPE FAIL_LOC_
        integer  ILAWF    ! type de loi de rupture
        integer  IDFAIL
        integer  NVAR
        integer  LF_DAM
        integer  LF_DAMMX
        integer  LF_DAMINI
        integer  LF_TDEL
        integer  LF_INDX
        integer  LF_OFF
        integer, DIMENSION(:)  , POINTER ::  INDX
        integer, DIMENSION(:)  , POINTER ::  OFF
        my_real, DIMENSION(:)  , POINTER ::  DAM
        my_real, DIMENSION(:)  , POINTER ::  VAR 
        my_real, DIMENSION(:)  , POINTER ::  DAMMX 
        my_real, DIMENSION(:)  , POINTER ::  DAMINI
        my_real, DIMENSION(:)  , POINTER ::  TDEL
      END TYPE FAIL_LOC_

      TYPE BUF_FAIL_
        TYPE(FAIL_LOC_), DIMENSION(:)  , POINTER ::  FLOC 
      END TYPE BUF_FAIL_

      TYPE BUF_MAT_
        my_real, DIMENSION(:)  , POINTER ::  VAR 
        INTEGER, DIMENSION(:)  , POINTER ::  VARTMP
      END TYPE BUF_MAT_
!     
      TYPE L_BUFEL_DIR_      ! element variables per slice in each layer
        my_real, DIMENSION(:) , POINTER ::   DIRA
        my_real, DIMENSION(:) , POINTER ::   DIRB
      END TYPE L_BUFEL_DIR_                             

      TYPE BUF_LAY_
        integer  ILAW     
        integer  IMAT 
        integer  IEOS
        integer  IVISC
        integer  IPORO
        integer  NFAIL
        integer  NVAR_MAT
        integer  NVAR_EOS        
        integer  NVARTMP
        integer  NVAR_VISC
        integer  NVAR_LAY   ! max nb of layer variables = 9
        integer  NVAR_LOC   ! max nb of local variables in LBUF = 51 (below)
        integer  NPTT       ! nb of integration points through layer (PID_51)
c-------
        integer  LY_DMG  
        integer  LY_GAMA   
        integer  LY_DIRA   
        integer  LY_DIRB   
        integer  LY_CRKDIR   
        integer  LY_PLAPT  ! mean plastic strain value between gauss points 
        integer  LY_SIGPT  ! mean stress value between gauss points
        integer  LY_HOURG
        integer  LY_UELR
        integer  LY_UELR1
        integer  LY_OFFPG
        integer  LY_OFF
c-------
        integer  L_OFF    
        integer  L_GAMA   
        integer  L_STRA   
        integer  L_FRAC   
        integer  L_BFRAC
        integer  L_EINT   
        integer  L_EINS   
        integer  L_RHO    
        integer  L_DP_DRHO
        integer  L_QVIS   
        integer  L_DELTAX 
        integer  L_VOL                        
        integer  L_EPSA   
        integer  L_EPSD                    
        integer  L_EPSQ   
        integer  L_EPSF   
        integer  L_PLA             
        integer  L_TEMP   
        integer  L_TB     
        integer  L_RK     
        integer  L_RE     
        integer  L_VK     
        integer  L_SF     
        integer  L_ROB    
        integer  L_DAM    
        integer  L_DSUM   
        integer  L_DGLO   
        integer  L_CRAK   
        integer  L_ANG    
        integer  L_EPE    
        integer  L_EPC    
        integer  L_XST         
        integer  L_SSP    
        integer  L_Z          
        integer  L_VISC   
        integer  L_SIGL   
        integer  L_SIGV   
        integer  L_SIGA   
        integer  L_SIGB   
        integer  L_SIGC   
        integer  L_SIGD   
        integer  L_SIGF   
        integer  L_SIG    
        integer  L_SIGPLY ! PLYXFEM
        integer  L_FOR    
        integer  L_MOM
        integer  L_THK 
        integer  L_SMSTR      
        integer  L_DMG  
        integer  L_FORTH
        integer  L_EINTTH  
        integer  L_SEQ
        integer  L_JAC_I
        integer  L_FAC_YLD
        integer  L_ABURN
        integer  L_MU
        integer  L_PIJ
        integer  L_VOL0DP
        integer  L_PLANL
        integer  L_EPSDNL
        integer  L_DMGSCL
        INTEGER  L_TSAIWU
c-------  layer variables     
        my_real, DIMENSION(:) , POINTER ::   DMG
        my_real, DIMENSION(:) , POINTER ::   GAMA
        my_real, DIMENSION(:) , POINTER ::   DIRA
        my_real, DIMENSION(:) , POINTER ::   DIRB
        my_real, DIMENSION(:) , POINTER ::   CRKDIR
        my_real, DIMENSION(:) , POINTER ::   PLAPT
        my_real, DIMENSION(:) , POINTER ::   SIGPT
        my_real, DIMENSION(:) , POINTER ::   HOURG
        my_real, DIMENSION(:) , POINTER ::   UELR   !  failure layer variable
        my_real, DIMENSION(:) , POINTER ::   UELR1  !  failure layer variable
        INTEGER, DIMENSION(:) , POINTER ::   OFFPG  !  failure of Gauss point
        INTEGER, DIMENSION(:) , POINTER ::   OFF    !  layer failure flag
c-------       
        TYPE (L_BUFEL_)  , DIMENSION(:,:,:)  , POINTER :: LBUF   ! local variables - per integration point
        TYPE (BUF_MAT_)  , DIMENSION(:,:,:)  , POINTER :: MAT    ! material buffer - per integration point
        TYPE (BUF_FAIL_) , DIMENSION(:,:,:)  , POINTER :: FAIL  
        TYPE (BUF_PROP_) , DIMENSION(:,:,:)  , POINTER :: PROP 
        TYPE (BUF_EOS_)  , DIMENSION(:,:,:)  , POINTER :: EOS  
        TYPE (BUF_VISC_) , DIMENSION(:,:,:)  , POINTER :: VISC  
        TYPE (BUF_PORO_) , DIMENSION(:,:,:)  , POINTER :: PORO  
        TYPE (BUF_XFEM_) , DIMENSION(:)      , POINTER :: XFEM        ! XFEM (NXEL)
        TYPE (L_BUFEL_DIR_) , DIMENSION(:)  , POINTER :: LBUF_DIR   ! Local direction by int point in the tickness for slice)
      END TYPE BUF_LAY_
c
c--------------------       
c 
      TYPE BUF_INTLOC_      ! element variables per integration point
        my_real, DIMENSION(:) , POINTER ::   EPS   ! (length=3)
        my_real, DIMENSION(:) , POINTER ::   SIG   ! (length=3)
      END TYPE BUF_INTLOC_                             

      TYPE BUF_INTLAY_
        integer  ILAW       ! inter ply material law type     
        integer  IMAT       ! inter ply material number
        integer  NFAIL
        integer  NVAR_MAT   ! number of user variables (UVAR) in the material buffer
        integer  NVARTMP    ! number of temp storage variables (VARTMP) in material laws
c------ interlayer variables par couche   (length=1) 
        my_real, DIMENSION(:) , POINTER ::   EINT         
        my_real, DIMENSION(:) , POINTER ::   COUNT
      
        TYPE (BUF_INTLOC_) , DIMENSION(:,:) , POINTER :: ILBUF
        TYPE (BUF_MAT_)    , DIMENSION(:,:) , POINTER :: MAT
        TYPE (BUF_FAIL_)   , DIMENSION(:,:) , POINTER :: FAIL  
      END TYPE BUF_INTLAY_
c--------------------       
 
      TYPE ELBUF_STRUCT_
        integer     :: IGTYP    
        integer     :: NEL      
        integer     :: NLAY     
        integer     :: NINTLAY  
        integer     :: NPTR     
        integer     :: NPTS     
        integer     :: NPTT   
        integer     :: IXFEM
        integer     :: NXEL       ! number of xfem parts created after element crack
        integer     :: IDRAPE
       
        TYPE (G_BUFEL_)                              :: GBUF   ! global variables - mean element values
        TYPE (BUF_LAY_)   , DIMENSION(:)   , POINTER :: BUFLY  ! BUFLY(NLAY) layer variables 
        TYPE (BUF_INTLAY_), DIMENSION(:)   , POINTER :: INTLAY ! inter-layer (NLAY-1)
        TYPE (BUF_XFEM_)  , DIMENSION(:)   , POINTER :: XFEM   ! XFEM (NXEL)
        TYPE (BUF_NLOC_)  , DIMENSION(:,:) , POINTER :: NLOC   ! Non-local thickness specific structure for shells
        TYPE (BUF_NLOCTS_), DIMENSION(:,:) , POINTER :: NLOCTS ! Non-local thickness specific structure for thickshells
        TYPE (BUF_NLOCS_)                            :: NLOCS  ! Non-local structure of brick element geometry configuration
      END TYPE ELBUF_STRUCT_
c
c---------------
      END MODULE ELBUFDEF_MOD

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
!||====================================================================
!||    my_alloc_mod                       ../common_source/tools/memory/my_alloc.F90
!||--- called by ------------------------------------------------------
!||    add_mass_stat                      ../starter/source/tools/admas/add_mass_stat.F
!||    admdiv                             ../engine/source/model/remesh/admdiv.F
!||    admfor0                            ../engine/source/model/remesh/admfor0.F
!||    admordr                            ../engine/source/model/remesh/admordr.F
!||    admregul                           ../engine/source/model/remesh/admregul.F
!||    allocate_nodal_arrays              ../common_source/modules/nodal_arrays.F90
!||    allocate_rbe3                      ../common_source/modules/constraints/rbe3_mod.F90
!||    allocate_rbe3pen                   ../common_source/modules/constraints/rbe3_mod.F90
!||    allocate_rwall                     ../common_source/modules/constraints/rwall_mod.F90
!||    allocate_rwall_pen                 ../common_source/modules/constraints/rwall_mod.F90
!||    allocate_sfem                      ../common_source/modules/elements/sfem_mod.F90
!||    allocate_sph_work                  ../common_source/modules/mat_elem/sph_work.F90
!||    allocbuf_auto                      ../engine/source/elements/elbuf/allocbuf_auto.F
!||    anioff0                            ../engine/source/output/anim/generate/anioff0.F
!||    anioffc                            ../engine/source/output/anim/generate/anioffc.F
!||    anioffc_crk                        ../engine/source/output/anim/generate/anioffc_crk.F
!||    anioffc_ply                        ../engine/source/output/anim/generate/anioffc_ply.F
!||    aniofff                            ../engine/source/output/anim/generate/aniofff.F
!||    anioffs                            ../engine/source/output/anim/generate/anioff6.F
!||    assadd2                            ../engine/source/assembly/assadd2.F
!||    boxtagn                            ../starter/source/model/box/bigbox.F
!||    c3grhead                           ../starter/source/elements/sh3n/coque3n/c3grhead.F
!||    c3grtails                          ../starter/source/elements/sh3n/coque3n/c3grtails.F
!||    c3init3                            ../starter/source/elements/sh3n/coque3n/c3init3.F
!||    cbainit3                           ../starter/source/elements/shell/coqueba/cbainit3.F
!||    cgrhead                            ../starter/source/elements/shell/coque/cgrhead.F
!||    cgrtails                           ../starter/source/elements/shell/coque/cgrtails.F
!||    chk_dttsh                          ../starter/source/elements/thickshell/solidec/scdtchk3.F
!||    chkmsr3n                           ../engine/source/interfaces/interf/chkstfn3.F
!||    cinit3                             ../starter/source/elements/shell/coque/cinit3.F
!||    compute_voxel_dimensions           ../engine/source/interfaces/intsort/voxel_dimensions.F90
!||    create_ellipse_clause              ../starter/source/model/sets/create_ellipse_clause.F
!||    create_line_from_element           ../starter/source/model/sets/create_line_from_element.F
!||    create_line_from_surface_all       ../starter/source/model/sets/create_line_from_surface_all.F
!||    create_line_from_surface_ext       ../starter/source/model/sets/create_line_from_surface_ext.F
!||    create_line_from_surface_ext_all   ../starter/source/model/sets/create_line_from_ext_surface_ext_all.F
!||    create_seatbelt                    ../starter/source/tools/seatbelts/create_seatbelt.F
!||    create_surface_from_element        ../starter/source/model/sets/create_surface_from_element.F
!||    ddsplit                            ../starter/source/restart/ddsplit/ddsplit.F
!||    delnumbf                           ../engine/source/output/anim/generate/delnumbf.F
!||    dfunc0                             ../engine/source/output/anim/generate/dfunc0.F
!||    dfuncc                             ../engine/source/output/anim/generate/dfuncc.F
!||    dfuncc_crk                         ../engine/source/output/anim/generate/dfuncc_crk.F
!||    dfuncc_ply                         ../engine/source/output/anim/generate/dfuncc_ply.F
!||    dfuncf                             ../engine/source/output/anim/generate/dfuncf.F
!||    dfuncs                             ../engine/source/output/anim/generate/dfunc6.F
!||    dtnodams                           ../engine/source/time_step/dtnodams.F
!||    elbuf_ini                          ../engine/source/elements/elbuf/elbuf_ini.F
!||    failwave_init                      ../starter/source/materials/fail/failwave_init.F
!||    fill_gr                            ../starter/source/model/sets/fill_gr.F
!||    fill_line                          ../starter/source/model/sets/fill_gr.F
!||    fill_surf                          ../starter/source/model/sets/fill_gr.F
!||    fill_surf_ellipse                  ../starter/source/model/sets/fill_gr_surf_ellipse.F
!||    find_dt_engine                     ../starter/source/coupling/rad2rad/r2r_speedup.F
!||    friction_parts_search              ../starter/source/interfaces/inter3d1/i7sti3.F
!||    fvbag1                             ../engine/source/airbag/fvbag1.F
!||    fvmesh0                            ../engine/source/airbag/fvmesh0.F
!||    genani                             ../engine/source/output/anim/generate/genani.F
!||    genh3d                             ../engine/source/output/h3d/h3d_results/genh3d.F
!||    genstat                            ../engine/source/output/sta/genstat.F
!||    gpsstrain_skin                     ../engine/source/output/anim/generate/tensgpstrain.F
!||    hierarchy_rbody                    ../starter/source/constraints/general/rbody/hierarchy_rbody.F90
!||    hierarchy_rbody_ddm                ../starter/source/constraints/general/rbody/hierarchy_rbody.F90
!||    hireorbe3                          ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
!||    hm_grogro                          ../starter/source/groups/hm_grogro.F
!||    hm_grogronod                       ../starter/source/groups/hm_grogronod.F
!||    hm_lecgre                          ../starter/source/groups/hm_lecgre.F
!||    hm_lecgrn                          ../starter/source/groups/hm_lecgrn.F
!||    hm_lines_of_lines                  ../starter/source/groups/hm_lines_of_lines.F
!||    hm_prelecgrns                      ../starter/source/groups/hm_prelecgrns.F
!||    hm_preread_rbody                   ../starter/source/constraints/general/rbody/hm_preread_rbody.F
!||    hm_read_admas                      ../starter/source/tools/admas/hm_read_admas.F
!||    hm_read_box                        ../starter/source/model/box/hm_read_box.F
!||    hm_read_grpart                     ../starter/source/groups/hm_read_grpart.F
!||    hm_read_inicrack                   ../starter/source/initial_conditions/inicrack/hm_read_inicrack.F
!||    hm_read_inivol                     ../starter/source/initial_conditions/inivol/hm_read_inivol.F90
!||    hm_read_lines                      ../starter/source/groups/hm_read_lines.F
!||    hm_read_merge                      ../starter/source/constraints/general/merge/hm_read_merge.F
!||    hm_read_pcyl                       ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
!||    hm_read_rbe3                       ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
!||    hm_read_rbody                      ../starter/source/constraints/general/rbody/hm_read_rbody.F
!||    hm_read_retractor                  ../starter/source/tools/seatbelts/hm_read_retractor.F
!||    hm_read_sensors                    ../starter/source/tools/sensor/hm_read_sensors.F
!||    hm_read_slipring                   ../starter/source/tools/seatbelts/hm_read_slipring.F
!||    hm_read_sphcel                     ../starter/source/elements/reader/hm_read_sphcel.F
!||    hm_read_subset                     ../starter/source/model/assembling/hm_read_subset.F
!||    hm_read_surf                       ../starter/source/groups/hm_read_surf.F
!||    hm_read_surfsurf                   ../starter/source/groups/hm_read_surfsurf.F
!||    hm_read_thgrou                     ../starter/source/output/th/hm_read_thgrou.F
!||    hm_read_window_user                ../starter/source/tools/userwi/hm_read_window_user.F
!||    hm_thvarvent                       ../starter/source/output/th/hm_thvarent.F
!||    i11mainf                           ../engine/source/interfaces/int11/i11mainf.F
!||    i21mainf                           ../engine/source/interfaces/int21/i21mainf.F
!||    i21tri                             ../engine/source/interfaces/intsort/i21tri.F
!||    i24gapm                            ../starter/source/interfaces/inter3d1/i24sti3.F
!||    i24sti3                            ../starter/source/interfaces/inter3d1/i24sti3.F
!||    i25gapm                            ../starter/source/interfaces/inter3d1/i25sti3.F
!||    i25neigh                           ../starter/source/interfaces/inter3d1/i25neigh.F
!||    i25sti3                            ../starter/source/interfaces/inter3d1/i25sti3.F
!||    i25trivox                          ../engine/source/interfaces/intsort/i25trivox.F
!||    i7remnode                          ../starter/source/interfaces/inter3d1/i7remnode.F
!||    i7sti3                             ../starter/source/interfaces/inter3d1/i7sti3.F
!||    i7trivox1                          ../starter/source/interfaces/inter3d1/i7trivox1.F
!||    ini_seatbelt                       ../starter/source/tools/seatbelts/ini_seatbelt.F
!||    inint3                             ../starter/source/interfaces/inter3d1/inint3.F
!||    inintr                             ../starter/source/interfaces/interf1/inintr.F
!||    init_bcs_nrf                       ../starter/source/boundary_conditions/init_bcs_nrf.F90
!||    init_bcs_wall                      ../starter/source/boundary_conditions/init_bcs_wall.F90
!||    init_h3d_engine                    ../engine/source/output/h3d/h3d_build_fortran/init_h3d_engine.F90
!||    init_monvol                        ../starter/source/airbag/init_monvol.F
!||    init_rwall_penalty                 ../starter/source/constraints/general/rwall/init_rwall_penalty.F90
!||    insert_clause_in_set               ../starter/source/model/sets/insert_clause_in_set.F
!||    intbuf_fric_ini_starter            ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
!||    intbuf_ini_starter                 ../starter/source/interfaces/intbuf/intbuf_ini_starter.F
!||    intti1                             ../engine/source/interfaces/interf/intti1.F
!||    lectur                             ../engine/source/input/lectur.F
!||    monvol_check_delete_duplicated     ../starter/source/airbag/monvol_check_delete_duplicated.F
!||    mpp_init                           ../engine/source/mpi/interfaces/spmd_i7tool.F
!||    nloc_dmg_init                      ../starter/source/materials/fail/nloc_dmg_init.F
!||    nodnx_sms_ini                      ../engine/source/ams/sms_init.F
!||    outri                              ../starter/source/materials/time_step/outri.F
!||    outrin                             ../starter/source/materials/time_step/outri.F
!||    parsor_crk                         ../engine/source/output/anim/generate/parsor_crk.F
!||    parsor_ply                         ../engine/source/output/anim/generate/parsor_ply.F
!||    parsorc                            ../engine/source/output/anim/generate/parsorc.F
!||    pre_i2                             ../starter/source/interfaces/inter3d1/i7remnode.F
!||    prerbe3p0                          ../engine/source/constraints/general/rbe3/rbe3f.F
!||    presegmt                           ../starter/source/interfaces/interf1/presegmt.F
!||    quad_surface_buffer                ../starter/source/model/sets/quad_surface_buffer.F
!||    r2r_group                          ../starter/source/coupling/rad2rad/r2r_group.F
!||    r2r_speedup                        ../starter/source/coupling/rad2rad/r2r_speedup.F
!||    r2r_split                          ../starter/source/coupling/rad2rad/r2r_split.F
!||    r2r_void                           ../starter/source/coupling/rad2rad/r2r_void.F
!||    r2r_void_1d                        ../starter/source/coupling/rad2rad/r2r_void.F
!||    rbody_part_check                   ../starter/source/constraints/general/rbody/rbody_part_modif.F90
!||    rbody_part_modif                   ../starter/source/constraints/general/rbody/rbody_part_modif.F90
!||    rcheckmass                         ../starter/source/elements/spring/rcheckmass.F
!||    read_box_box                       ../starter/source/model/box/read_box_box.F
!||    read_eosparam                      ../engine/source/output/restart/read_eosparam.F90
!||    read_impdisp                       ../starter/source/constraints/general/impvel/read_impdisp.F
!||    read_rwall                         ../starter/source/constraints/general/rwall/read_rwall.F
!||    read_sensor_python                 ../starter/source/tools/sensor/hm_read_sensor_python.F90
!||    read_viscparam                     ../engine/source/output/restart/read_viscparam.F
!||    remn_i2_edg                        ../starter/source/interfaces/inter3d1/i7remnode.F
!||    remn_i2_edgop                      ../starter/source/interfaces/inter3d1/i7remnode.F
!||    remn_i2op                          ../starter/source/interfaces/inter3d1/i7remnode.F
!||    remn_i2op_edg25                    ../starter/source/interfaces/int25/i25remlin.F
!||    remn_self24                        ../starter/source/interfaces/inter3d1/remn_self24.F
!||    resol                              ../engine/source/engine/resol.F
!||    resol_alloc_phase1                 ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase3                 ../engine/source/engine/resol_alloc.F90
!||    resol_alloc_phase9                 ../engine/source/engine/resol_alloc.F90
!||    restalloc                          ../engine/source/output/restart/arralloc.F
!||    retrirby                           ../starter/source/constraints/general/merge/hm_read_merge.F
!||    rgbodfp                            ../engine/source/constraints/general/rbody/rgbodfp.F
!||    rgwal0_pen                         ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||    ri2_int24p_ini                     ../starter/source/interfaces/inter3d1/i7remnode.F
!||    rm_cand24                          ../starter/source/interfaces/inter3d1/i7remnode.F
!||    rpart_inivel_check                 ../starter/source/constraints/general/rbody/rbody_part_modif.F90
!||    rwall_fpen                         ../engine/source/constraints/general/rwall/rgwall_pen.F90
!||    sensor_init                        ../engine/source/tools/sensor/sensor_init.F
!||    set_user_window_nodes              ../starter/source/user_interface/user_windows_tools.F
!||    seteloff                           ../starter/source/constraints/general/rbody/hm_read_rbody.F
!||    setrbyon                           ../starter/source/constraints/general/rbody/hm_read_rbody.F
!||    sfem_exclude_dim                   ../starter/source/elements/solid/solide4/sfem_exclude.F90
!||    sfem_exclude_ini                   ../starter/source/elements/solid/solide4/sfem_exclude.F90
!||    sgrhead                            ../starter/source/elements/solid/solide/sgrhead.F
!||    sgrtails                           ../starter/source/elements/solid/solide/sgrtails.F
!||    sms_admesh_0                       ../engine/source/ams/sms_admesh.F
!||    sms_admesh_1                       ../engine/source/ams/sms_admesh.F
!||    sms_build_diag                     ../engine/source/ams/sms_build_diag.F
!||    sms_build_mat_2                    ../engine/source/ams/sms_build_mat_2.F
!||    sms_check                          ../engine/source/ams/sms_fsa_inv.F
!||    sms_ini_int                        ../engine/source/ams/sms_init.F
!||    sms_ini_jad_1                      ../engine/source/ams/sms_init.F
!||    sms_ini_jad_2                      ../engine/source/ams/sms_init.F
!||    sms_ini_jad_3                      ../engine/source/ams/sms_init.F
!||    sms_ini_kad                        ../engine/source/ams/sms_init.F
!||    sms_ini_kdi                        ../engine/source/ams/sms_init.F
!||    sms_ini_kin_1                      ../engine/source/ams/sms_init.F
!||    sms_mass_scale_2                   ../engine/source/ams/sms_mass_scale_2.F
!||    sms_mav_lt                         ../engine/source/ams/sms_pcg.F
!||    solid_surface_buffer               ../starter/source/model/sets/solid_surface_buffer.F
!||    sort_mid_pid                       ../engine/source/system/sort_mid_pid.F
!||    spgrhead                           ../starter/source/elements/sph/spgrhead.F
!||    spgrtails                          ../starter/source/elements/sph/spgrtails.F
!||    spinih                             ../starter/source/elements/sph/spinih.F
!||    splissv                            ../engine/source/elements/sph/splissv.F
!||    spmd_glob_fsum9                    ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_sort_sms                      ../engine/source/mpi/ams/spmd_sms.F
!||    spmd_userwi_rest                   ../starter/source/user_interface/user_windows_tools.F
!||    st_qaprint_element                 ../starter/source/output/qaprint/st_qaprint_element.F
!||    st_qaprint_reference_state         ../starter/source/output/qaprint/st_qaprint_reference_state.F
!||    stackgroup                         ../starter/source/stack/stackgroup.F
!||    stat_beam_mp                       ../engine/source/output/sta/stat_beam_mp.F
!||    stat_beam_spmd                     ../engine/source/output/sta/stat_beam_spmd.F
!||    stat_brick_mp                      ../engine/source/output/sta/stat_brick_mp.F
!||    stat_brick_spmd                    ../engine/source/output/sta/stat_brick_spmd.F
!||    stat_c_auxf                        ../engine/source/output/sta/stat_c_auxf.F
!||    stat_c_epspf                       ../engine/source/output/sta/stat_c_epspf.F
!||    stat_c_fail                        ../engine/source/output/sta/stat_c_fail.F
!||    stat_c_orth_loc                    ../engine/source/output/sta/stat_c_orth_loc.F
!||    stat_c_straf                       ../engine/source/output/sta/stat_c_straf.F
!||    stat_c_strafg                      ../engine/source/output/sta/stat_c_strafg.F
!||    stat_c_strsf                       ../engine/source/output/sta/stat_c_strsf.F
!||    stat_c_strsfg                      ../engine/source/output/sta/stat_c_strsfg.F
!||    stat_n_bcs                         ../engine/source/output/sta/stat_n_bcs.F
!||    stat_n_temp                        ../engine/source/output/sta/stat_n_temp.F
!||    stat_n_vel                         ../engine/source/output/sta/state_n_vel.F
!||    stat_node                          ../engine/source/output/sta/stat_node.F
!||    stat_p_aux                         ../engine/source/output/sta/stat_p_aux.F
!||    stat_p_full                        ../engine/source/output/sta/stat_p_full.F
!||    stat_quad_mp                       ../engine/source/output/sta/stat_quad_mp.F
!||    stat_quad_spmd                     ../engine/source/output/sta/stat_quad_spmd.F
!||    stat_s_auxf                        ../engine/source/output/sta/stat_s_auxf.F
!||    stat_s_eref                        ../engine/source/output/sta/stat_s_eref.F
!||    stat_s_fail                        ../engine/source/output/sta/stat_s_fail.F
!||    stat_s_ortho                       ../engine/source/output/sta/stat_s_ortho.F
!||    stat_s_straf                       ../engine/source/output/sta/stat_s_straf.F
!||    stat_s_strsf                       ../engine/source/output/sta/stat_s_strsf.F
!||    stat_shel_mp                       ../engine/source/output/sta/stat_shel_mp.F
!||    stat_shel_spmd                     ../engine/source/output/sta/stat_shel_spmd.F
!||    stat_sphcel_spmd                   ../engine/source/output/sta/stat_sphcel_spmd.F90
!||    stat_spring_mp                     ../engine/source/output/sta/stat_spring_mp.F
!||    stat_spring_spmd                   ../engine/source/output/sta/stat_spring_spmd.F
!||    stat_t_full                        ../engine/source/output/sta/stat_t_full.F
!||    stat_truss_mp                      ../engine/source/output/sta/stat_truss_mp.F
!||    stat_truss_spmd                    ../engine/source/output/sta/stat_truss_spmd.F
!||    switch_to_dtnoda                   ../engine/source/time_step/switch_to_dtnoda.F
!||    t3grhead                           ../starter/source/elements/solid_2d/tria/t3grhead.F
!||    t3grtails                          ../starter/source/elements/solid_2d/tria/t3grtails.F
!||    tensgps3                           ../engine/source/output/anim/generate/tensor6.F
!||    tensgps_skin                       ../engine/source/output/anim/generate/tensor6.F
!||    tensgpstrain                       ../engine/source/output/anim/generate/tensgpstrain.F
!||    tensor0                            ../engine/source/output/anim/generate/tensor0.F
!||    tensorc                            ../engine/source/output/anim/generate/tensorc.F
!||    tensorc_crk                        ../engine/source/output/anim/generate/tensorc_crk.F
!||    tensorc_ply                        ../engine/source/output/anim/generate/tensorc_ply.F
!||    tensors                            ../engine/source/output/anim/generate/tensor6.F
!||    th_surf_load_pressure              ../starter/source/output/th/th_surf_load_pressure.F
!||    torseur                            ../engine/source/output/anim/generate/torseur.F
!||    trirbmerge                         ../starter/source/constraints/general/merge/hm_read_merge.F
!||    update_pon_shells                  ../engine/source/engine/node_spliting/update_pon.F90
!||    userwis_front                      ../starter/source/user_interface/user_windows_tools.F
!||    ush_init                           ../starter/source/elements/elbuf_init/ush_init.F90
!||    velvec2                            ../engine/source/output/anim/generate/velvec.F
!||    velvec3                            ../engine/source/output/anim/generate/velvec.F
!||    velvecc                            ../engine/source/output/anim/generate/velvec.F
!||    w_failwave                         ../engine/source/output/restart/w_failwave.F
!||    w_fi                               ../starter/source/restart/ddsplit/w_fi.F
!||    w_th_surf_loadp                    ../starter/source/restart/ddsplit/w_th_surf_loadp.F
!||    w_th_surf_pload                    ../starter/source/restart/ddsplit/w_th_surf_pload.F
!||    write_nloc_struct                  ../engine/source/output/restart/write_nloc_struct.F
!||====================================================================

! ======================================================================================================================
! fypp template — generates my_alloc.F90
! Do NOT edit the generated my_alloc.F90 directly; edit this file and re-run fypp.
!
! Axes of variation, each represented as a fypp set:
!   TYPES     : (fortran_type, short_name_for_subroutine_suffix)
!   MEM_KINDS : (fortran_attribute, name_prefix)   allocatable vs pointer
!   IDX_KINDS : (index_integer_type, name_prefix)  integer(4) vs integer(8) dimension args
!   RANKS     : (rank_number, list_of_dim_variable_names)
!
! Subroutine naming convention:
!   my_alloc_<idx_prefix><mem_prefix><type_name>_<rank>d
!   e.g. my_alloc_8_pdouble_3d  =>  integer(8) dims, pointer, double precision, 3D
!
! Placeholder for derived types is at the bottom of the contains section.
! ======================================================================================================================

#:set TYPES     = [('real', 'real'), ('double precision', 'double'), ('integer', 'integer'), ('logical', 'logical')]
#:set MEM_KINDS = [('allocatable', ''), ('pointer', 'p')]
#:set IDX_KINDS = [('integer', ''), ('integer(8)', '8_')]
#:set RANKS     = [(1, ['n']), (2, ['n', 'm']), (3, ['l', 'm', 'n'])]
#:set ORDINALS  = ['first', 'second', 'third']

#! Macro: emit one allocation subroutine.
#! Arguments:
#!   SUB_NAME  - subroutine name string
#!   FTYPE     - Fortran type (e.g. 'real', 'double precision', 'integer', 'logical')
#!   RANK      - integer rank (1, 2, or 3)
#!   DIM_VARS  - list of dimension variable names  (e.g. ['n'] or ['n','m'] or ['l','m','n'])
#!   MEM_ATTR  - 'allocatable' or 'pointer'
#!   IDX_TYPE  - 'integer' or 'integer(8)'
#:def alloc_sub(SUB_NAME, FTYPE, RANK, DIM_VARS, MEM_ATTR, IDX_TYPE)
  #! Address of first element — used as unique tracking key per live allocation.
  #! TARGET attribute on the dummy allows c_loc on elements.
  #:set FIRST_ELEM = 'a(' + ', '.join(['lbound(a,' + str(i+1) + ')' for i in range(RANK)]) + ')'
!||====================================================================
!||    ${SUB_NAME}$   ../common_source/tools/memory/my_alloc.F90
!||--- calls      -----------------------------------------------------
!||    my_alloc_check   ../common_source/tools/memory/my_alloc.F90
!||====================================================================
        subroutine ${SUB_NAME}$(a, ${', '.join(DIM_VARS)}$, msg, stat)
  #:if MEM_ATTR == 'allocatable'
          ${FTYPE}$, dimension(${', '.join([':'] * RANK)}$), allocatable, target, intent(inout) :: a !< The allocated array
  #:else
          ${FTYPE}$, dimension(${', '.join([':'] * RANK)}$), pointer, intent(inout) :: a !< The allocated array
  #:endif
  #:for I, DV in enumerate(DIM_VARS)
    #:if RANK == 1
          ${IDX_TYPE}$, intent(in) :: ${DV}$ !< The size of the array
    #:else
          ${IDX_TYPE}$, intent(in) :: ${DV}$ !< The ${ORDINALS[I]}$ dimension of the array
    #:endif
  #:endfor
          character(len=*), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
          integer :: ierr
          allocate(a(${', '.join(DIM_VARS)}$), stat=ierr)
          if (ierr == 0 .and. present(msg)) &
            call record_alloc_addr(c_loc(${FIRST_ELEM}$), msg, int(storage_size(a), kind=8) / 8_8 * size(a, kind=8))
          if(.not. present(stat)) then
            if(present(msg)) then
              call my_alloc_check(ierr, msg=msg)
            else
              call my_alloc_check(ierr)
            end if
          end if
          if(present(stat)) stat = ierr
        end subroutine ${SUB_NAME}$
#:enddef alloc_sub

      module my_alloc_mod
        use iso_c_binding, only : c_char, c_int, c_int64_t, c_ptr, c_loc
        implicit none
        integer, parameter :: len_error_message = 100

        interface
          subroutine cpp_record_alloc(msg, msg_len, nbytes) bind(C, name="cpp_record_alloc")
            import :: c_char, c_int, c_int64_t
            character(kind=c_char), intent(in) :: msg(*)
            integer(c_int), intent(in) :: msg_len
            integer(c_int64_t), intent(in) :: nbytes
          end subroutine cpp_record_alloc

          subroutine cpp_record_alloc_addr(addr, msg, msg_len, nbytes) bind(C, name="cpp_record_alloc_addr")
            import :: c_ptr, c_char, c_int, c_int64_t
            type(c_ptr), value, intent(in) :: addr
            character(kind=c_char), intent(in) :: msg(*)
            integer(c_int), intent(in) :: msg_len
            integer(c_int64_t), intent(in) :: nbytes
          end subroutine cpp_record_alloc_addr

          subroutine cpp_record_dealloc_addr(addr) bind(C, name="cpp_record_dealloc_addr")
            import :: c_ptr
            type(c_ptr), value, intent(in) :: addr
          end subroutine cpp_record_dealloc_addr

          subroutine cpp_print_alloc_report() bind(C, name="cpp_print_alloc_report")
          end subroutine cpp_print_alloc_report
        end interface

        public :: report_alloc
        public :: record_dealloc_addr
        private :: build_msg, my_alloc_check, record_alloc_addr

#:for IDX_TYPE, IDX_PREFIX in IDX_KINDS
  #:for MEM_ATTR, MEM_PREFIX in MEM_KINDS
    #:for FTYPE, TNAME in TYPES
      #:for RANK, DIM_VARS in RANKS
        private :: my_alloc_${IDX_PREFIX}$${MEM_PREFIX}$${TNAME}$_${RANK}$d
      #:endfor
    #:endfor
  #:endfor
#:endfor

        public :: my_alloc

        interface my_alloc
#:for IDX_TYPE, IDX_PREFIX in IDX_KINDS
  #:for MEM_ATTR, MEM_PREFIX in MEM_KINDS
    #:for FTYPE, TNAME in TYPES
      #:for RANK, DIM_VARS in RANKS
          module procedure my_alloc_${IDX_PREFIX}$${MEM_PREFIX}$${TNAME}$_${RANK}$d
      #:endfor
    #:endfor
  #:endfor
#:endfor
        end interface my_alloc

      contains

! ======================================================================================================================
!                                                     TOOLS
! ======================================================================================================================
!||====================================================================
!||    build_msg      ../common_source/tools/memory/my_alloc.F90
!||--- called by ------------------------------------------------------
!||    execargcheck   ../engine/source/engine/execargcheck.F
!||    radioss2       ../engine/source/engine/radioss2.F
!||    starter0       ../starter/source/starter/starter0.F
!||====================================================================
        function build_msg(str) result(error_message)
          character(len=*), intent(in) :: str
          character(len=len_error_message) :: error_message
          if(len_trim(str) > len_error_message) then
            error_message = str(1:len_error_message)
          else
            error_message = adjustl(str) // repeat(" ", len_error_message - len_trim(str))
          end if
        end function build_msg

!||====================================================================
!||    my_alloc_check   ../common_source/tools/memory/my_alloc.F90
!||--- calls      -----------------------------------------------------
!||    arret            ../engine/source/system/arret.F
!||====================================================================
        subroutine my_alloc_check(stat,msg)
          integer, intent(in) :: stat
          character(len=len_error_message), optional,  intent(in) :: msg
          if (stat /= 0) then
            write(6, "(a,i10,a)") "Error in memory allocation"
            if(present(msg)) then
              write(6, "(a)") msg
            end if
            call arret(2)
          end if
        end subroutine my_alloc_check

        subroutine record_alloc_addr(addr, msg, nbytes)
          type(c_ptr), intent(in) :: addr
          character(len=*), intent(in) :: msg
          integer(kind=8), intent(in) :: nbytes
          integer(c_int) :: msg_len
          integer(c_int64_t) :: c_nbytes
          msg_len = len_trim(msg)
          c_nbytes = nbytes
          call cpp_record_alloc_addr(addr, msg, msg_len, c_nbytes)
        end subroutine record_alloc_addr

        subroutine record_dealloc_addr(addr)
          type(c_ptr), intent(in) :: addr
          call cpp_record_dealloc_addr(addr)
        end subroutine record_dealloc_addr

        subroutine report_alloc()
          call cpp_print_alloc_report()
        end subroutine report_alloc

! ======================================================================================================================
!                                     GENERATED ALLOCATION ROUTINES
!   Loop order: IDX_KINDS x MEM_KINDS x TYPES x RANKS
!   IDX_KINDS  : integer(4) dims (''), integer(8) dims ('8_')
!   MEM_KINDS  : allocatable (''), pointer ('p')
!   TYPES      : real, double precision, integer, logical
!   RANKS      : 1d, 2d, 3d
! ======================================================================================================================

#:for IDX_TYPE, IDX_PREFIX in IDX_KINDS
  #:for MEM_ATTR, MEM_PREFIX in MEM_KINDS
    #:for FTYPE, TNAME in TYPES
      #:for RANK, DIM_VARS in RANKS
        #:set SUB_NAME = 'my_alloc_' + IDX_PREFIX + MEM_PREFIX + TNAME + '_' + str(RANK) + 'd'
!! \brief Allocate a ${RANK}$D ${FTYPE}$ array (${MEM_ATTR}$, ${IDX_TYPE}$ dims)
$:alloc_sub(SUB_NAME, FTYPE, RANK, DIM_VARS, MEM_ATTR, IDX_TYPE)

      #:endfor
    #:endfor
  #:endfor
#:endfor

! ======================================================================================================================
!                            PLACEHOLDER — DERIVED TYPE ALLOCATION ROUTINES
!
! To add derived type support, add the type to the TYPES set above, or add a dedicated
! interface block below for types that need special treatment (e.g. no storage_size).
!
! Example future entry in TYPES:
!   #:set TYPES = [..., ('type(my_derived_type)', 'mytype')]
!
! Note: storage_size() works on derived types in Fortran 2008+, but the record_alloc
! call may need adjustment if the type contains allocatable components.
! ======================================================================================================================

      end module my_alloc_mod

// C++ wrapper layer for Zoltan calls with contiguity enforcement.
// Specifically designed for large meshes (>100K elements) with 3-6 constraints.

#include <cstdlib>
#include <cstdint>
#include <vector>
#include <cstdio>
#include <cstring>
#include <cctype>
#include <queue>
#include <unordered_set>
#include <algorithm>
#include <string>

// Zoltan header
#include "zoltan.h"

extern "C" {

// =========================================================================
// Data structure to hold graph information for Zoltan callbacks
// =========================================================================

struct ZoltanGraphData {
    int nelem;           // Number of elements (vertices)
    int ncond;           // Number of constraints
    int *xadj;           // Adjacency index array (1-indexed, Fortran style)
    int *adjncy;         // Adjacency list (1-indexed, Fortran style)
    int *vwgt;           // Vertex weights [nelem * ncond]
    int *adjwgt;         // Edge weights (optional)
    
    // For returning results
    int *partition;      // Output partition assignment
};

// =========================================================================
// Zoltan callback functions
// =========================================================================

static int zoltan_num_obj_fn(void *data, int *ierr) {
    ZoltanGraphData *gd = static_cast<ZoltanGraphData*>(data);
    *ierr = ZOLTAN_OK;
    return gd->nelem;
}

static void zoltan_obj_list_fn(void *data, int num_gid_entries, int num_lid_entries,
                                ZOLTAN_ID_PTR global_ids, ZOLTAN_ID_PTR local_ids,
                                int wgt_dim, float *obj_wgts, int *ierr) {
    ZoltanGraphData *gd = static_cast<ZoltanGraphData*>(data);
    *ierr = ZOLTAN_OK;
    
    for (int i = 0; i < gd->nelem; ++i) {
        global_ids[i] = i;
        if (local_ids) {
            local_ids[i] = i;
        }
    }
    
    if (wgt_dim > 0 && obj_wgts && gd->vwgt) {
        for (int i = 0; i < gd->nelem; ++i) {
            for (int c = 0; c < wgt_dim && c < gd->ncond; ++c) {
                size_t idx = static_cast<size_t>(i) * gd->ncond + static_cast<size_t>(c);
                obj_wgts[i * wgt_dim + c] = static_cast<float>(gd->vwgt[idx]);
            }
        }
    }
}

static void zoltan_num_edges_fn(void *data, int num_gid_entries, int num_lid_entries,
                                 int num_obj, ZOLTAN_ID_PTR global_ids, ZOLTAN_ID_PTR local_ids,
                                 int *num_edges, int *ierr) {
    ZoltanGraphData *gd = static_cast<ZoltanGraphData*>(data);
    *ierr = ZOLTAN_OK;
    
    for (int i = 0; i < num_obj; ++i) {
        int elem = global_ids[i];
        if (elem < 0 || elem >= gd->nelem) {
            *ierr = ZOLTAN_FATAL;
            return;
        }
        num_edges[i] = gd->xadj[elem + 1] - gd->xadj[elem];
    }
}

static void zoltan_edge_list_fn(void *data, int num_gid_entries, int num_lid_entries,
                                 int num_obj, ZOLTAN_ID_PTR global_ids, ZOLTAN_ID_PTR local_ids,
                                 int *num_edges,
                                 ZOLTAN_ID_PTR nbor_global_ids, int *nbor_procs,
                                 int wgt_dim, float *ewgts, int *ierr) {
    ZoltanGraphData *gd = static_cast<ZoltanGraphData*>(data);
    *ierr = ZOLTAN_OK;
    
    int offset = 0;
    for (int i = 0; i < num_obj; ++i) {
        int elem = global_ids[i];
        if (elem < 0 || elem >= gd->nelem) {
            *ierr = ZOLTAN_FATAL;
            return;
        }
        
        int adj_start = gd->xadj[elem] - 1;
        int adj_end = gd->xadj[elem + 1] - 1;
        int nedges = adj_end - adj_start;
        
        for (int j = 0; j < nedges; ++j) {
            int neighbor = gd->adjncy[adj_start + j] - 1;
            nbor_global_ids[offset + j] = neighbor;
            if (nbor_procs) {
                nbor_procs[offset + j] = 0;
            }
        }
        
        if (wgt_dim > 0 && ewgts && gd->adjwgt) {
            for (int j = 0; j < nedges; ++j) {
                ewgts[offset + j] = static_cast<float>(gd->adjwgt[adj_start + j]);
            }
        }
        
        offset += nedges;
    }
}

// =========================================================================
// Contiguity Analysis
// =========================================================================

struct PartitionStats {
    int nparts;
    std::vector<int> part_sizes;           // Elements per partition
    std::vector<int> num_components;       // Connected components per partition
    std::vector<std::vector<int>> component_sizes;  // Size of each component
    int total_components;
    int edge_cut;
};

// BFS to find connected components within a partition
static void find_components(int nelem, const int *xadj, const int *adjncy,
                           const int *partition, int target_part,
                           std::vector<int> &component_id) {
    std::vector<bool> visited(nelem, false);
    int comp_id = 0;
    
    for (int seed = 0; seed < nelem; ++seed) {
        // Skip if already visited or not in target partition
        if (visited[seed] || partition[seed] != target_part) {
            continue;
        }
        
        // BFS from seed
        std::queue<int> q;
        q.push(seed);
        visited[seed] = true;
        component_id[seed] = comp_id;
        
        while (!q.empty()) {
            int elem = q.front();
            q.pop();
            
            int adj_start = xadj[elem] - 1;
            int adj_end = xadj[elem + 1] - 1;
            
            for (int idx = adj_start; idx < adj_end; ++idx) {
                int neighbor = adjncy[idx] - 1;
                
                if (neighbor >= 0 && neighbor < nelem &&
                    !visited[neighbor] &&
                    partition[neighbor] == target_part) {
                    
                    visited[neighbor] = true;
                    component_id[neighbor] = comp_id;
                    q.push(neighbor);
                }
            }
        }
        
        comp_id++;
    }
}

static PartitionStats analyze_partition_contiguity(int nelem, const int *xadj,
                                                   const int *adjncy, const int *partition,
                                                   int nparts) {
    PartitionStats stats;
    stats.nparts = nparts;
    stats.part_sizes.resize(nparts, 0);
    stats.num_components.resize(nparts, 0);
    stats.component_sizes.resize(nparts);
    stats.total_components = 0;
    stats.edge_cut = 0;
    
    // Count partition sizes
    for (int e = 0; e < nelem; ++e) {
        int p = partition[e] - 1;  // Convert from 1-indexed
        if (p >= 0 && p < nparts) {
            stats.part_sizes[p]++;
        }
    }
    
    // Find connected components for each partition
    std::vector<int> component_id(nelem, -1);
    
    for (int p = 0; p < nparts; ++p) {
        if (stats.part_sizes[p] == 0) continue;
        
        find_components(nelem, xadj, adjncy, partition, p + 1, component_id);
        
        // Count components and their sizes for this partition
        std::vector<int> comp_sizes;
        for (int e = 0; e < nelem; ++e) {
            if (partition[e] == p + 1) {
                int cid = component_id[e];
                if (cid >= 0) {
                    if (cid >= static_cast<int>(comp_sizes.size())) {
                        comp_sizes.resize(cid + 1, 0);
                    }
                    comp_sizes[cid]++;
                }
            }
        }
        
        // Remove zeros and sort by size (descending)
        comp_sizes.erase(std::remove(comp_sizes.begin(), comp_sizes.end(), 0), comp_sizes.end());
        std::sort(comp_sizes.begin(), comp_sizes.end(), std::greater<int>());
        
        stats.num_components[p] = comp_sizes.size();
        stats.component_sizes[p] = comp_sizes;
        stats.total_components += comp_sizes.size();
    }
    
    // Compute edge cut
    for (int e = 0; e < nelem; ++e) {
        int part_e = partition[e];
        int adj_start = xadj[e] - 1;
        int adj_end = xadj[e + 1] - 1;
        
        for (int idx = adj_start; idx < adj_end; ++idx) {
            int neighbor = adjncy[idx] - 1;
            if (neighbor >= 0 && neighbor < nelem) {
                int part_n = partition[neighbor];
                if (part_e != part_n) {
                    stats.edge_cut++;
                }
            }
        }
    }
    stats.edge_cut /= 2;
    
    return stats;
}

static void print_contiguity_report(const PartitionStats &stats) {
    std::fprintf(stdout, "\n[Zoltan] ========== PARTITION CONTIGUITY REPORT ==========\n");
    std::fprintf(stdout, "[Zoltan] Total partitions: %d\n", stats.nparts);
    std::fprintf(stdout, "[Zoltan] Total connected components: %d\n", stats.total_components);
    std::fprintf(stdout, "[Zoltan] Edge cut: %d\n", stats.edge_cut);
    
    int non_contiguous = 0;
    for (int p = 0; p < stats.nparts; ++p) {
        if (stats.num_components[p] > 1) {
            non_contiguous++;
        }
    }
    
    if (non_contiguous == 0) {
        std::fprintf(stdout, "[Zoltan] ✓ ALL PARTITIONS ARE CONTIGUOUS!\n");
    } else {
        std::fprintf(stdout, "[Zoltan] ⚠ %d partitions are NON-CONTIGUOUS\n", non_contiguous);
    }
    
    std::fprintf(stdout, "[Zoltan] \n");
    std::fprintf(stdout, "[Zoltan] Per-partition details:\n");
    
    for (int p = 0; p < stats.nparts; ++p) {
        std::fprintf(stdout, "[Zoltan]   Partition %2d: %6d elements, %2d component(s)",
                     p + 1, stats.part_sizes[p], stats.num_components[p]);
        
        if (stats.num_components[p] > 1) {
            std::fprintf(stdout, " [NON-CONTIGUOUS]");
            std::fprintf(stdout, "\n[Zoltan]                Component sizes: ");
            for (size_t i = 0; i < stats.component_sizes[p].size(); ++i) {
                std::fprintf(stdout, "%d", stats.component_sizes[p][i]);
                if (i < stats.component_sizes[p].size() - 1) {
                    std::fprintf(stdout, ", ");
                }
            }
        }
        std::fprintf(stdout, "\n");
    }
    
    std::fprintf(stdout, "[Zoltan] ================================================\n\n");
}

// =========================================================================
// Main Zoltan wrapper function with contiguity control
// =========================================================================

/**
 * Partition a graph using Zoltan with multi-constraint support and contiguity control.
 *
 * Arguments (Fortran 1-indexed):
 *   NELEM   - number of elements (vertices)
 *   NCOND   - number of constraints (vertex weight dimensions)
 *   XADJ    - adjacency index array (size NELEM+1, 1-indexed values)
 *   ADJNCY  - adjacency list (1-indexed element IDs)
 *   IWD     - vertex weights, column-major: IWD[e + c*NELEM]
 *   ADJWGT  - edge weights (optional, can be NULL)
 *   NNODE   - number of partitions requested
 *   IMBALANCE - imbalance tolerance (e.g., 1.05 for 5% imbalance)
 *   METHOD  - partitioning method: 0=GRAPH, 1=HYPERGRAPH, 2=RCB, 3=HSFC
 *   ENFORCE_CONTIGUITY - 0=no enforcement, 1=report only, 2=enforce with post-processing
 *   NEC     - output: number of cut edges
 *   CEP     - output: partition assignment per element (1 to npart)
 *
 * Returns: 0 on success, -1 on error
 */
int wrap_zoltan_partition_contiguous(
    int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
    int *IWD, int *ADJWGT, int *NNODE, float *IMBALANCE, int *METHOD,
    int *ENFORCE_CONTIGUITY,
    int *NEC, int *CEP)
{
    const int nelem = *NELEM;
    const int ncond = *NCOND;
    const int npart = *NNODE;
    const float imbalance = *IMBALANCE;
    const int method = *METHOD;
    const int enforce_contiguity = *ENFORCE_CONTIGUITY;
    
    std::fprintf(stdout, "[Zoltan] Partitioning %d elements into %d parts with %d constraints\n",
                 nelem, npart, ncond);
    std::fprintf(stdout, "[Zoltan] Imbalance tolerance: %.3f\n", imbalance);
    std::fprintf(stdout, "[Zoltan] Contiguity mode: %s\n",
                 enforce_contiguity == 0 ? "disabled" :
                 enforce_contiguity == 1 ? "report only" : "enforce");
    
    // =========================================================================
    // Step 1: Initialize Zoltan
    // =========================================================================
    
    float version;
    int argc = 0;
    char **argv = nullptr;
    Zoltan_Initialize(argc, argv, &version);
    
    struct Zoltan_Struct *zz = Zoltan_Create(MPI_COMM_WORLD);
    if (!zz) {
        std::fprintf(stdout, "[Zoltan] ERROR: Failed to create Zoltan structure\n");
        return -1;
    }
    
    // =========================================================================
    // Step 2: Set Zoltan parameters optimized for contiguity
    // =========================================================================
    
    Zoltan_Set_Param(zz, "DEBUG_LEVEL", "0");
    Zoltan_Set_Param(zz, "OBJ_WEIGHT_DIM", std::to_string(ncond).c_str());
    Zoltan_Set_Param(zz, "NUM_GID_ENTRIES", "1");
    Zoltan_Set_Param(zz, "NUM_LID_ENTRIES", "1");
    Zoltan_Set_Param(zz, "RETURN_LISTS", "PARTITION");
    
    // Set partitioning method with contiguity-friendly parameters
    int chosen_method = method;
    const char *method_name;
    // Geometric methods (RCB/HSFC) require geometry callbacks; we don't have coordinates in this wrapper.
    if (method == 2 || method == 3) {
        std::fprintf(stdout,
            "[Zoltan] WARNING: Geometric method requested (%s), but no coordinates are available; falling back to PHG.\n",
            method == 2 ? "RCB" : "HSFC");
        chosen_method = 1; // Fallback to HYPERGRAPH (PHG)
    }
    switch (chosen_method) {
        case 0:  // GRAPH
            method_name = "GRAPH";
            Zoltan_Set_Param(zz, "LB_METHOD", "GRAPH");
            Zoltan_Set_Param(zz, "GRAPH_PACKAGE", "PARMETIS");
            Zoltan_Set_Param(zz, "PARMETIS_METHOD", "PARTKWAY");
            if (ADJWGT) {
                Zoltan_Set_Param(zz, "EDGE_WEIGHT_DIM", "1");
            }
            break;
            
        case 1:  // HYPERGRAPH (best for multi-constraints)
            method_name = "HYPERGRAPH";
            Zoltan_Set_Param(zz, "LB_METHOD", "HYPERGRAPH");
            Zoltan_Set_Param(zz, "HYPERGRAPH_PACKAGE", "PHG");
            
            // PHG parameters for better contiguity
            Zoltan_Set_Param(zz, "PHG_CUT_OBJECTIVE", "CONNECTIVITY");  // Minimize connectivity (better for contiguity)
            Zoltan_Set_Param(zz, "PHG_COARSEPARTITION_METHOD", "GREEDY");
            Zoltan_Set_Param(zz, "PHG_REFINEMENT_METHOD", "FM2");
            Zoltan_Set_Param(zz, "PHG_REFINEMENT_QUALITY", "2");  // More refinement passes
            break;
            
        case 2:  // RCB (requested, but not available without geometry) -> already fallen back
        case 3:  // HSFC (requested, but not available without geometry) -> already fallen back
            // Should not reach here due to fallback above
            method_name = "HYPERGRAPH";
            Zoltan_Set_Param(zz, "LB_METHOD", "HYPERGRAPH");
            Zoltan_Set_Param(zz, "HYPERGRAPH_PACKAGE", "PHG");
            Zoltan_Set_Param(zz, "PHG_CUT_OBJECTIVE", "CONNECTIVITY");
            Zoltan_Set_Param(zz, "PHG_COARSEPARTITION_METHOD", "GREEDY");
            Zoltan_Set_Param(zz, "PHG_REFINEMENT_METHOD", "FM2");
            Zoltan_Set_Param(zz, "PHG_REFINEMENT_QUALITY", "2");
            break;
            
        default:
            std::fprintf(stdout, "[Zoltan] ERROR: Unknown method %d\n", method);
            Zoltan_Destroy(&zz);
            return -1;
    }
    
    std::fprintf(stdout, "[Zoltan] Using method: %s\n", method_name);
    
    // Set imbalance tolerance
    char imb_str[32];
    std::snprintf(imb_str, sizeof(imb_str), "%.4f", imbalance);
    Zoltan_Set_Param(zz, "IMBALANCE_TOL", imb_str);
    
    // Set number of partitions
    Zoltan_Set_Param(zz, "NUM_GLOBAL_PARTS", std::to_string(npart).c_str());
    Zoltan_Set_Param(zz, "NUM_LOCAL_PARTS", std::to_string(npart).c_str());
    
    // Additional parameters for better quality
    Zoltan_Set_Param(zz, "LB_APPROACH", "PARTITION");  // Full repartition
    
    // =========================================================================
    // Step 3: Setup graph data structure
    // =========================================================================
    
    ZoltanGraphData graph_data;
    graph_data.nelem = nelem;
    graph_data.ncond = ncond;
    graph_data.xadj = XADJ;
    graph_data.adjncy = ADJNCY;
    graph_data.vwgt = IWD;
    graph_data.adjwgt = ADJWGT;
    graph_data.partition = CEP;
    
    // =========================================================================
    // Step 4: Register callback functions
    // =========================================================================
    
    Zoltan_Set_Num_Obj_Fn(zz, zoltan_num_obj_fn, &graph_data);
    Zoltan_Set_Obj_List_Fn(zz, zoltan_obj_list_fn, &graph_data);
    
    // Register graph-specific callbacks
    if (chosen_method == 0 || chosen_method == 1) {
        Zoltan_Set_Num_Edges_Multi_Fn(zz, zoltan_num_edges_fn, &graph_data);
        Zoltan_Set_Edge_List_Multi_Fn(zz, zoltan_edge_list_fn, &graph_data);
    }
    
    // =========================================================================
    // Step 5: Call Zoltan partitioner
    // =========================================================================
    
    int changes;
    int num_gid_entries;
    int num_lid_entries;
    int num_import;
    ZOLTAN_ID_PTR import_global_ids;
    ZOLTAN_ID_PTR import_local_ids;
    int *import_procs;
    int *import_to_part;
    int num_export;
    ZOLTAN_ID_PTR export_global_ids;
    ZOLTAN_ID_PTR export_local_ids;
    int *export_procs;
    int *export_to_part;
    
    int rc = Zoltan_LB_Partition(
        zz,
        &changes,
        &num_gid_entries,
        &num_lid_entries,
        &num_import,
        &import_global_ids,
        &import_local_ids,
        &import_procs,
        &import_to_part,
        &num_export,
        &export_global_ids,
        &export_local_ids,
        &export_procs,
        &export_to_part
    );
    
    if (rc != ZOLTAN_OK) {
        std::fprintf(stdout, "[Zoltan] ERROR: Partitioning failed with code %d\n", rc);
        Zoltan_Destroy(&zz);
        return -1;
    }
    
    std::fprintf(stdout, "[Zoltan] Partitioning successful, %d changes\n", changes);
    
    // =========================================================================
    // Step 6: Extract partition assignments
    // =========================================================================
    
    // Initialize all elements to partition 0
    for (int i = 0; i < nelem; ++i) {
        CEP[i] = 0;
    }
    
    // Update based on export list
    for (int i = 0; i < num_export; ++i) {
        int elem = export_global_ids[i];
        int part = export_to_part[i];
        if (elem >= 0 && elem < nelem) {
            CEP[elem] = part;
        }
    }
    
    // Convert from 0-indexed to 1-indexed for Fortran
    for (int i = 0; i < nelem; ++i) {
        CEP[i] += 1;
    }
    
    // =========================================================================
    // Step 7: Compute edge cut
    // =========================================================================
    
    *NEC = 0;
    for (int e = 0; e < nelem; ++e) {
        int part_e = CEP[e];
        int adj_start = XADJ[e] - 1;
        int adj_end = XADJ[e + 1] - 1;
        
        for (int idx = adj_start; idx < adj_end; ++idx) {
            int neighbor = ADJNCY[idx] - 1;
            if (neighbor >= 0 && neighbor < nelem) {
                int part_n = CEP[neighbor];
                if (part_e != part_n) {
                    (*NEC)++;
                }
            }
        }
    }
    *NEC /= 2;
    
    // =========================================================================
    // Step 8: Analyze and optionally enforce contiguity
    // =========================================================================
    
    if (enforce_contiguity >= 1) {
        PartitionStats stats = analyze_partition_contiguity(nelem, XADJ, ADJNCY, CEP, npart);
        print_contiguity_report(stats);
        
        // TODO: Add post-processing to enforce contiguity if enforce_contiguity == 2
        // This would involve merging small disconnected components
    }
    
    // =========================================================================
    // Step 9: Cleanup
    // =========================================================================
    
    Zoltan_LB_Free_Part(&import_global_ids, &import_local_ids, &import_procs, &import_to_part);
    Zoltan_LB_Free_Part(&export_global_ids, &export_local_ids, &export_procs, &export_to_part);
    Zoltan_Destroy(&zz);
    
    return 0;
}

// Simplified wrapper without edge weights (using HYPERGRAPH method by default)
int wrap_zoltan_partition_multiconstraint(
    int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
    int *IWD, int *NNODE, float *IMBALANCE,
    int *NEC, int *CEP)
{
    int method = 1;  // Default: HYPERGRAPH (PHG) for 3-6 constraints

    const char *env = std::getenv("RAD_PARTITIONING_METHOD");
    if (env && *env) {
        std::string s(env);
        // Normalize to upper-case
        std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c){ return static_cast<char>(std::toupper(c)); });

        if (s == "PARMETIS") {
            method = 0; // GRAPH with PARMETIS
        } else if (s == "PHG") {
            method = 1; // HYPERGRAPH (PHG)
        } else if (s == "RCB") {
            method = 2; // RCB
        } else if (s == "HSFC") {
            method = 3; // HSFC
        } else {
            std::fprintf(stdout, "[Zoltan] WARNING: Unknown RAD_PARTITIONING_METHOD='%s'. Using default PHG.\n", env);
        }
        std::fprintf(stdout, "[Zoltan] Using partitioning method from environment: %s\n", s.c_str());
    } 
    int enforce_contiguity = 1;  // Report contiguity
    return wrap_zoltan_partition_contiguous(
        NELEM, NCOND, XADJ, ADJNCY,
        IWD, nullptr, NNODE, IMBALANCE, &method, &enforce_contiguity,
        NEC, CEP);
}

} // extern "C"
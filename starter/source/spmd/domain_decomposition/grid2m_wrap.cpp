//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2026 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
// C++ wrapper layer for METIS calls.
// Exposes a single unmangled symbol per function so Fortran can bind via BIND(C, name="...").

#include <cstdlib>
#include <cstdint>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <numeric>
#include <cstdio>
#include <limits>
#include <cmath>

extern "C" {

int METIS_PartGraphKway(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                        int *IWD, int *vsize, int *ADJWGT2, int *NNODE, float *tpwgts,
                        float *UBVEC, int *OPTIONS, int *NEC, int *CEP);

int METIS_PartGraphRecursive(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                             int *IWD, int *vsize, int *ADJWGT2, int *NNODE, float *tpwgts,
                             float *UBVEC, int *OPTIONS, int *NEC, int *CEP);

int wrap_metis_partgraphkway(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                             int *IWD, int *NNODE,
                             float *UBVEC, int *OPTIONS, int *NEC, int *CEP)
{
  int *vsize = nullptr;
  int *ADJWGT2 = nullptr;
  float *tpwgts = nullptr;
  const int ierr = METIS_PartGraphKway(
    NELEM, NCOND, XADJ, ADJNCY,
    IWD, vsize, ADJWGT2, NNODE, tpwgts,
    UBVEC, OPTIONS, NEC, CEP);

  if (vsize != nullptr) {
    std::free(vsize);
  }
  if (ADJWGT2 != nullptr) {
    std::free(ADJWGT2);
  }
  if (tpwgts != nullptr) {
    std::free(tpwgts);
  }

  return ierr;
}

int wrap_metis_partgraphrecursive(int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
                                  int *IWD, int *NNODE,
                                  float *UBVEC, int *OPTIONS, int *NEC, int *CEP)
{
  int *vsize = nullptr;
  int *ADJWGT2 = nullptr;
  float *tpwgts = nullptr;

  const int ierr = METIS_PartGraphRecursive(
    NELEM, NCOND, XADJ, ADJNCY,
    IWD, vsize, ADJWGT2, NNODE, tpwgts,
    UBVEC, OPTIONS, NEC, CEP);

  if (vsize != nullptr) {
    std::free(vsize);
  }
  if (ADJWGT2 != nullptr) {
    std::free(ADJWGT2);
  }
  if (tpwgts != nullptr) {
    std::free(tpwgts);
  }

  return ierr;
}


static constexpr int VECTOR_GROUP_SIZE = 128;
static constexpr float MERGE_ALPHA = 0.5f; // Threshold factor for merging parts, merge only if part weight < (ALPHA / npart) of total weight

using metis_part_fn_t = int(*)(int*, int*, int*, int*, int*, int*, int*, int*, float*, float*, int*, int*, int*);

/**
 * Identify border vertices of a part (vertices with fewest intra-part edges).
 * Returns a list of coarse vertex IDs sorted by number of intra-part edges (ascending).
 * 
 * Arguments:
 *   part_coarse_vertices - list of coarse vertex IDs in this part
 *   coarse_to_elements - mapping from coarse vertex to list of elements
 *   part - original part assignment per element
 *   target_part_id - the part ID we're analyzing
 *   coarse_xadj, coarse_adjncy - adjacency structure of coarse graph (1-indexed)
 */
static std::vector<int> identify_border_vertices(
    const std::vector<int>& part_coarse_vertices,
    const std::vector<std::vector<int>>& coarse_to_elements,
    const int* part,
    int target_part_id,
    const std::vector<int>& coarse_xadj,
    const std::vector<int>& coarse_adjncy)
{
    // For each coarse vertex, count intra-part edges
    std::vector<std::pair<int, int>> vertex_intra_edges;  // {coarse_vertex_id, intra_edge_count}
    vertex_intra_edges.reserve(part_coarse_vertices.size());
    
    for (int cv : part_coarse_vertices) {
        int intra_edge_count = 0;
        
        int adj_start = coarse_xadj[cv] - 1;  // Convert to 0-indexed
        int adj_end = coarse_xadj[cv + 1] - 1;
        
        for (int idx = adj_start; idx < adj_end; ++idx) {
            int neighbor_cv = coarse_adjncy[idx] - 1;  // Convert to 0-indexed
            
            // Check if neighbor is in the same part by checking its elements
            bool same_part = false;
            if (!coarse_to_elements[neighbor_cv].empty()) {
                int neighbor_part = part[coarse_to_elements[neighbor_cv][0]];
                if (neighbor_part == target_part_id) {
                    same_part = true;
                }
            }
            
            if (same_part) {
                intra_edge_count++;
            }
        }
        
        vertex_intra_edges.push_back({cv, intra_edge_count});
    }
    
    // Sort by intra-edge count (ascending - fewest intra-edges first = border vertices)
    std::sort(vertex_intra_edges.begin(), vertex_intra_edges.end(),
              [](const std::pair<int,int>& a, const std::pair<int,int>& b) {
                  return a.second < b.second;
              });
    
    // Extract just the vertex IDs
    std::vector<int> border_vertices;
    border_vertices.reserve(vertex_intra_edges.size());
    for (const auto& p : vertex_intra_edges) {
        border_vertices.push_back(p.first);
    }
    
    return border_vertices;
}

/**
 * Helper function to subdivide a large part into sub-partitions of ~128 elements each.
 * Uses METIS kway partitioning (unweighted).
 * 
 * Returns a vector mapping each element in the part to its sub-partition ID (0-based).
 */
static std::vector<int> subdivide_large_part(
    const std::vector<int>& elements,
    int nelem,
    const int* XADJ,
    const int* ADJNCY,
    int* OPTIONS)
{
    const int part_size = static_cast<int>(elements.size());
    const int n_sub_parts = static_cast<int>(std::ceil(static_cast<double>(part_size) / VECTOR_GROUP_SIZE));
    
    // Build element index mapping
    std::unordered_map<int, int> elem_to_local;
    elem_to_local.reserve(part_size);
    for (int i = 0; i < part_size; ++i) {
        elem_to_local[elements[i]] = i;
    }
    
    // Build subgraph adjacency structure (1-indexed for METIS)
    std::vector<int> sub_xadj(part_size + 1);
    sub_xadj[0] = 1;  // 1-indexed
    
    std::vector<int> sub_adjncy;
    sub_adjncy.reserve(part_size * 8);  // Rough estimate
    
    for (int i = 0; i < part_size; ++i) {
        int e = elements[i];
        int adj_start = XADJ[e] - 1;      // Convert from 1-indexed
        int adj_end = XADJ[e + 1] - 1;
        
        for (int idx = adj_start; idx < adj_end; ++idx) {
            int neighbor = ADJNCY[idx] - 1;  // Convert from 1-indexed
            
            // Only include edges to elements within this part
            auto it = elem_to_local.find(neighbor);
            if (it != elem_to_local.end()) {
                int local_neighbor = it->second;
                sub_adjncy.push_back(local_neighbor + 1);  // 1-indexed
            }
        }
        
        sub_xadj[i + 1] = static_cast<int>(sub_adjncy.size()) + 1;  // 1-indexed
    }
    
    // Call METIS kway (unweighted)
    std::vector<int> sub_partition(part_size);
    int sub_ncon = 1;  // No weights
    int sub_edgecut = 0;
    
    int ierr = METIS_PartGraphKway(
        const_cast<int*>(&part_size),
        &sub_ncon,
        sub_xadj.data(),
        sub_adjncy.data(),
        nullptr,      // No vertex weights
        nullptr,      // No vsize
        nullptr,      // No edge weights
        const_cast<int*>(&n_sub_parts),
        nullptr,      // No tpwgts
        nullptr,      // No ubvec needed for unweighted
        OPTIONS,
        &sub_edgecut,
        sub_partition.data()
    );
    
    if (ierr != 1) {
        std::fprintf(stdout, "[METIS subdivide] WARNING: METIS returned error %d for part with %d elements\n",
                     ierr, part_size);
    }
    
    return sub_partition;
}

/**
 * Add fictive edges to improve connectivity for poorly-connected parts.
 * 
 * A part is considered poorly connected if it has fewer than threshold external edges,
 * where threshold = 5% of the number of coarse vertices in the part.
 * 
 * Fictive edges (weight=1) are added between border vertices of poorly-connected parts
 * and border vertices of the largest part.
 * 
 * Returns: number of fictive edges added
 */
static int add_fictive_edges_for_connectivity(
    int n_coarse,
    const std::vector<int>& elem_to_coarse,
    const std::vector<int>& coarse_to_part,
    const std::vector<std::vector<int>>& coarse_to_elements,
    const int* part,
    int nelem,
    std::vector<int>& coarse_xadj,
    std::vector<int>& coarse_adjncy,
    std::vector<int>& coarse_adjwgt)
{
    // Step 1: Group coarse vertices by their original part
    std::unordered_map<int, std::vector<int>> part_to_coarse_vertices;
    
    for (int cv = 0; cv < n_coarse; ++cv) {
        const auto& elems = coarse_to_elements[cv];
        if (elems.empty()) continue;
        
        // Determine which part this coarse vertex belongs to
        // Take the part of the first element (all elements in a coarse vertex from
        // merged small parts will have the same part ID)
        int part_id = part[elems[0]];
        
        if (part_id >= 0) {
            part_to_coarse_vertices[part_id].push_back(cv);
        }
    }
    
    if (part_to_coarse_vertices.empty()) {
        std::fprintf(stdout, "[METIS fictive] No parts found, skipping fictive edge addition\n");
        return 0;
    }
    
    // Step 2: Find the largest part (by number of coarse vertices)
    int largest_part_id = -1;
    size_t largest_part_size = 0;
    
    for (const auto& kv : part_to_coarse_vertices) {
        if (kv.second.size() > largest_part_size) {
            largest_part_size = kv.second.size();
            largest_part_id = kv.first;
        }
    }
    
    if (largest_part_id < 0) {
        std::fprintf(stdout, "[METIS fictive] Could not identify largest part\n");
        return 0;
    }
    
    std::fprintf(stdout, "[METIS fictive] Largest part: ID=%d with %zu coarse vertices\n",
                 largest_part_id, largest_part_size);
    
    // Step 3: For each part, count external edges
    std::unordered_map<int, int> part_external_edges;
    
    for (const auto& kv : part_to_coarse_vertices) {
        int pid = kv.first;
        const auto& vertices = kv.second;
        
        int external_count = 0;
        for (int cv : vertices) {
            int adj_start = coarse_xadj[cv] - 1;
            int adj_end = coarse_xadj[cv + 1] - 1;
            
            for (int idx = adj_start; idx < adj_end; ++idx) {
                int neighbor_cv = coarse_adjncy[idx] - 1;
                
                // Check if neighbor belongs to a different part
                if (!coarse_to_elements[neighbor_cv].empty()) {
                    int neighbor_part = part[coarse_to_elements[neighbor_cv][0]];
                    if (neighbor_part != pid) {
                        external_count++;
                    }
                }
            }
        }
        
        part_external_edges[pid] = external_count;
    }
    
    // Step 4: Identify poorly-connected parts (< 5% threshold)
    std::vector<int> poorly_connected_parts;
    const double threshold_ratio = 0.05;
    
    for (const auto& kv : part_to_coarse_vertices) {
        int pid = kv.first;
        if (pid == largest_part_id) continue;  // Skip the largest part
        
        size_t part_size = kv.second.size();
        int external_edges = part_external_edges[pid];
        int threshold = static_cast<int>(std::ceil(threshold_ratio * part_size));
        
        if (external_edges < threshold) {
            poorly_connected_parts.push_back(pid);
            std::fprintf(stdout, "[METIS fictive] Part %d is poorly connected: %d external edges < %d threshold (%.1f%% of %zu vertices)\n",
                         pid, external_edges, threshold, threshold_ratio * 100, part_size);
        }
    }
    
    if (poorly_connected_parts.empty()) {
        std::fprintf(stdout, "[METIS fictive] All parts are well-connected, no fictive edges needed\n");
        return 0;
    }
    
    // Step 5: Identify border vertices for largest part
    std::vector<int> largest_part_border = identify_border_vertices(
        part_to_coarse_vertices[largest_part_id],
        coarse_to_elements,
        part,
        largest_part_id,
        coarse_xadj,
        coarse_adjncy
    );
    
    if (largest_part_border.empty()) {
        std::fprintf(stdout, "[METIS fictive] ERROR: Largest part has no border vertices\n");
        return 0;
    }
    
    std::fprintf(stdout, "[METIS fictive] Largest part has %zu border vertices\n", largest_part_border.size());
    
    // Step 6: Add fictive edges between poorly-connected parts and largest part
    struct FictiveEdge {
        int cv1;
        int cv2;
    };
    std::vector<FictiveEdge> fictive_edges;
    
    for (int pid : poorly_connected_parts) {
        const auto& part_vertices = part_to_coarse_vertices[pid];
        
        // Identify border vertices for this part
        std::vector<int> part_border = identify_border_vertices(
            part_vertices,
            coarse_to_elements,
            part,
            pid,
            coarse_xadj,
            coarse_adjncy
        );
        
        if (part_border.empty()) {
            std::fprintf(stdout, "[METIS fictive] WARNING: Part %d has no border vertices, using all vertices\n", pid);
            part_border = part_vertices;
        }
        
        // Calculate number of fictive edges needed (5% of part size)
        int n_fictive = static_cast<int>(std::ceil(threshold_ratio * part_vertices.size()));
        
        // Distribute edges across border vertices (scattered pattern)
        int k_small = static_cast<int>(part_border.size());
        int k_large = static_cast<int>(largest_part_border.size());
        
        for (int i = 0; i < n_fictive; ++i) {
            int cv_small = part_border[i % k_small];
            int cv_large = largest_part_border[i % k_large];
            
            fictive_edges.push_back({cv_small, cv_large});
        }
        
        std::fprintf(stdout, "[METIS fictive] Adding %d fictive edges for part %d (distributed across %d x %d border vertices)\n",
                     n_fictive, pid, k_small, k_large);
    }
    
    // Step 7: Insert fictive edges into adjacency structure
    // We need to rebuild the adjacency arrays with the new edges
    
    // Count new degree for each vertex
    std::vector<int> added_degree(n_coarse, 0);
    for (const auto& edge : fictive_edges) {
        added_degree[edge.cv1]++;
        added_degree[edge.cv2]++;
    }
    
    // Build new XADJ
    std::vector<int> new_xadj(n_coarse + 1);
    new_xadj[0] = 1;  // 1-indexed
    for (int cv = 0; cv < n_coarse; ++cv) {
        int old_degree = coarse_xadj[cv + 1] - coarse_xadj[cv];
        int new_degree = old_degree + added_degree[cv];
        new_xadj[cv + 1] = new_xadj[cv] + new_degree;
    }
    
    size_t new_edge_count = static_cast<size_t>(new_xadj[n_coarse] - 1);
    std::vector<int> new_adjncy(new_edge_count);
    std::vector<int> new_adjwgt(new_edge_count);
    
    // Copy existing edges
    std::vector<int> write_pos(n_coarse);
    for (int cv = 0; cv < n_coarse; ++cv) {
        write_pos[cv] = new_xadj[cv] - 1;  // 0-indexed write position
        
        int old_start = coarse_xadj[cv] - 1;
        int old_end = coarse_xadj[cv + 1] - 1;
        
        for (int idx = old_start; idx < old_end; ++idx) {
            new_adjncy[write_pos[cv]] = coarse_adjncy[idx];
            new_adjwgt[write_pos[cv]] = coarse_adjwgt[idx];
            write_pos[cv]++;
        }
    }
    
    // Add fictive edges (both directions)
    for (const auto& edge : fictive_edges) {
        // Add edge cv1 -> cv2
        new_adjncy[write_pos[edge.cv1]] = edge.cv2 + 1;  // 1-indexed
        new_adjwgt[write_pos[edge.cv1]] = 1;  // Weight = 1
        write_pos[edge.cv1]++;
        
        // Add edge cv2 -> cv1
        new_adjncy[write_pos[edge.cv2]] = edge.cv1 + 1;  // 1-indexed
        new_adjwgt[write_pos[edge.cv2]] = 1;  // Weight = 1
        write_pos[edge.cv2]++;
    }
    
    // Replace old arrays with new ones
    coarse_xadj = std::move(new_xadj);
    coarse_adjncy = std::move(new_adjncy);
    coarse_adjwgt = std::move(new_adjwgt);
    
    int total_fictive = static_cast<int>(fictive_edges.size());
    std::fprintf(stdout, "[METIS fictive] Added %d fictive edges (scattered pattern)\n", total_fictive);
    
    return total_fictive;
}

static int metis_partition_with_coarsening(
    metis_part_fn_t part_fn,
    int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
    int *IWD, int *NNODE,
    float *UBVEC, int *OPTIONS, int *NEC, int *CEP, int *part)
{
    const int nelem = *NELEM;
    const int ncond = *NCOND;
    const int npart = *NNODE;

    // =========================================================================
    // Step 1: Group elements by part ID, compute per-part weights
    // =========================================================================

    std::unordered_map<int, std::vector<int>> part_to_elements;
    part_to_elements.reserve(npart);

    for (int e = 0; e < nelem; ++e) {
        int pid = part[e];
        if (pid < 0) continue;
        part_to_elements[pid].push_back(e);
    }

    std::vector<double> total_weight(ncond, 0.0);
    for (int c = 0; c < ncond; ++c) {
        for (int e = 0; e < nelem; ++e) {
            size_t idx = static_cast<size_t>(e) * ncond + static_cast<size_t>(c);
            total_weight[c] += IWD[idx];
        }
    }

    std::unordered_map<int, std::vector<double>> part_weights;
    part_weights.reserve(part_to_elements.size());

    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        const auto &elems = kv.second;
        std::vector<double> w(ncond, 0.0);
        
        for (int e : elems) {
            for (int c = 0; c < ncond; ++c) {
                size_t idx = static_cast<size_t>(e) * ncond + static_cast<size_t>(c);
                w[c] += IWD[idx];
            }
        }
        part_weights.emplace(pid, std::move(w));
    }

    // =========================================================================
    // Step 2: Classify parts as small, large, or mergeable
    // =========================================================================

    std::unordered_map<int, bool> part_merged;
    part_merged.reserve(part_to_elements.size());
    
    int n_small_parts = 0;
    int n_merged_parts = 0;
    int n_large_parts = 0;

    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        size_t size = kv.second.size();

        if (size >= static_cast<size_t>(VECTOR_GROUP_SIZE)) {
            // Large part - will be subdivided
            part_merged[pid] = false;
            n_large_parts++;
            continue;
        }

        n_small_parts++;

        // Check if small part can be merged
        bool can_merge = true;
        const auto &pw = part_weights[pid];
        for (int c = 0; c < ncond; ++c) {
            if (total_weight[c] > 0.0) {
                double ratio = pw[c] / total_weight[c];
                double threshold = MERGE_ALPHA / static_cast<double>(npart);
                if (ratio >= threshold) {
                    can_merge = false;
                    break;
                }
            }
        }
        part_merged[pid] = can_merge;
        if (can_merge) {
            n_merged_parts++;
        }
    }

    // =========================================================================
    // Fallback: if no small parts exist, call METIS directly on original graph
    // =========================================================================

    if (n_small_parts == 0 && n_large_parts == 0) {
        std::fprintf(stdout, "[METIS coarse] No parts found, using original graph\n");

        int *vsize = nullptr;
        int *ADJWGT2 = nullptr;
        float *tpwgts = nullptr;
        return part_fn(
            NELEM, NCOND, XADJ, ADJNCY,
            IWD, vsize, ADJWGT2, NNODE, tpwgts,
            UBVEC, OPTIONS, NEC, CEP);
    }

    size_t total_elements_in_parts = 0;
    for (const auto &kv : part_to_elements) {
        total_elements_in_parts += kv.second.size();
    }
    size_t elements_without_part = static_cast<size_t>(nelem) - total_elements_in_parts;
    
    std::fprintf(stdout, "[METIS coarse] Parts: %zu total (%d large >= %d elements, %d small < %d elements)\n",
                 part_to_elements.size(), n_large_parts, VECTOR_GROUP_SIZE, n_small_parts, VECTOR_GROUP_SIZE);
    std::fprintf(stdout, "[METIS coarse] Small parts: %d merged, %d rejected (weight constraint)\n",
                 n_merged_parts, n_small_parts - n_merged_parts);
    std::fprintf(stdout, "[METIS coarse] Elements: %zu in parts, %zu without part (negative ID)\n",
                 total_elements_in_parts, elements_without_part);

    // =========================================================================
    // Step 3: Build element -> coarse vertex mapping
    // =========================================================================

    std::vector<int> elem_to_coarse(nelem, -1);
    int coarse_id = 0;

    std::vector<int> coarse_to_part;
    std::vector<std::vector<int>> coarse_to_elements;
    
    // Reserve space to avoid reallocations
    size_t estimated_coarse_vertices = part_to_elements.size() * 2 + elements_without_part;
    coarse_to_part.reserve(estimated_coarse_vertices);
    coarse_to_elements.reserve(estimated_coarse_vertices);

    int total_subdivisions = 0;

    // Process parts
    for (const auto &kv : part_to_elements) {
        int pid = kv.first;
        const auto &elems = kv.second;
        size_t part_size = elems.size();

        if (part_size >= static_cast<size_t>(VECTOR_GROUP_SIZE)) {
            // Large part: subdivide into groups of ~128 elements
            std::vector<int> sub_partition = subdivide_large_part(elems, nelem, XADJ, ADJNCY, OPTIONS);
            
            // Group elements by sub-partition
            int n_sub_parts = *std::max_element(sub_partition.begin(), sub_partition.end()) + 1;
            std::vector<std::vector<int>> sub_groups(n_sub_parts);
            
            for (size_t i = 0; i < elems.size(); ++i) {
                int sub_pid = sub_partition[i] - 1;  // Convert from 1-indexed to 0-indexed
                if (sub_pid < 0 || sub_pid >= n_sub_parts) {
                    std::fprintf(stdout, "[METIS coarse] ERROR: Invalid sub-partition ID %d for element %d\n",
                                 sub_pid, elems[i]);
                    continue;
                }
                sub_groups[sub_pid].push_back(elems[i]);
            }
            
            // Create coarse vertices for each sub-group
            for (int sub_pid = 0; sub_pid < n_sub_parts; ++sub_pid) {
                const auto& sub_elems = sub_groups[sub_pid];
                if (sub_elems.empty()) continue;
                
                for (int e : sub_elems) {
                    elem_to_coarse[e] = coarse_id;
                }
                coarse_to_part.push_back(-1);  // Sub-partitions don't have a single part ID
                coarse_to_elements.push_back(sub_elems);
                coarse_id++;
            }
            
            total_subdivisions += n_sub_parts;
            
        } else if (part_merged[pid]) {
            // Small part that can be merged: all elements into one coarse vertex
            for (int e : elems) {
                elem_to_coarse[e] = coarse_id;
            }
            coarse_to_part.push_back(pid);
            coarse_to_elements.push_back(elems);
            coarse_id++;
            
        } else {
            // Small part that cannot be merged: each element becomes its own coarse vertex
            for (int e : elems) {
                elem_to_coarse[e] = coarse_id;
                coarse_to_part.push_back(-1);
                coarse_to_elements.push_back({e});
                coarse_id++;
            }
        }
    }

    // Process elements without part assignment (keep as individual coarse vertices)
    for (int e = 0; e < nelem; ++e) {
        if (part[e] < 0) {
            elem_to_coarse[e] = coarse_id;
            coarse_to_part.push_back(-1);
            coarse_to_elements.push_back({e});
            coarse_id++;
        }
    }

    int n_coarse = coarse_id;

    // Validate that all elements were assigned
    int unassigned = 0;
    for (int e = 0; e < nelem; ++e) {
        if (elem_to_coarse[e] < 0) {
            unassigned++;
        }
    }
    if (unassigned > 0) {
        for (int e = 0; e < nelem; ++e) {
            if (elem_to_coarse[e] < 0) {
                std::fprintf(stdout, "[METIS coarse] ERROR: element %d (part=%d) was not assigned a coarse vertex\n",
                             e, part[e]);
            }
        }
        std::fprintf(stdout, "[METIS coarse] ERROR: %d elements were not assigned coarse vertices\n", unassigned);
        return -1;
    }

    std::fprintf(stdout, "[METIS coarse] Large parts subdivided into %d groups\n", total_subdivisions);
    std::fprintf(stdout, "[METIS coarse] Graph: %d -> %d vertices (%.1f%% reduction)\n",
                 nelem, n_coarse, 100.0 * (1.0 - static_cast<double>(n_coarse) / nelem));

    // =========================================================================
    // Step 4: Build coarse graph adjacency with edge weights
    // =========================================================================

    // Edge structure for sorting
    struct CoarseEdge {
        int cv1;
        int cv2;
        
        bool operator<(const CoarseEdge& other) const {
            if (cv1 != other.cv1) return cv1 < other.cv1;
            return cv2 < other.cv2;
        }
    };

    // Build list of all edges (with duplicates)
    std::vector<CoarseEdge> edge_list;
    size_t estimated_edges = static_cast<size_t>(XADJ[nelem] - XADJ[0]);
    edge_list.reserve(estimated_edges);

    for (int e = 0; e < nelem; ++e) {
        int cv_e = elem_to_coarse[e];
        if (cv_e < 0 || cv_e >= n_coarse) {
            std::fprintf(stdout, "[METIS coarse] ERROR: element %d has invalid coarse vertex %d (n_coarse=%d)\n",
                         e, cv_e, n_coarse);
            continue;
        }

        int adj_start = XADJ[e] - 1;      // Convert from 1-indexed to 0-indexed
        int adj_end = XADJ[e + 1] - 1;

        for (int idx = adj_start; idx < adj_end; ++idx) {
            int neighbor = ADJNCY[idx] - 1;  // Convert from 1-indexed to 0-indexed
            if (neighbor < 0 || neighbor >= nelem) {
                std::fprintf(stdout, "[METIS coarse] ERROR: element %d has invalid neighbor %d (nelem=%d)\n",
                             e, neighbor, nelem);
                continue;
            }
            int cv_n = elem_to_coarse[neighbor];
            if (cv_n < 0 || cv_n >= n_coarse) {
                std::fprintf(stdout, "[METIS coarse] ERROR: neighbor %d has invalid coarse vertex %d (n_coarse=%d)\n",
                             neighbor, cv_n, n_coarse);
                continue;
            }
            if (cv_e < cv_n) { // Only process one direction, skip self-loops automatically
                edge_list.push_back({cv_e, cv_n});
            }
        }
    }

    // Sort edges to group duplicates together
    std::sort(edge_list.begin(), edge_list.end());

    // Count degree of each vertex (for both directions of each edge)
    std::vector<int> degree(n_coarse, 0);
    if (!edge_list.empty()) {
        int current_cv1 = edge_list[0].cv1;
        int current_cv2 = edge_list[0].cv2;
        int weight = 1;
        
        for (size_t i = 1; i <= edge_list.size(); ++i) {
            // Check if we've moved to a new edge or reached the end
            if (i == edge_list.size() || 
                edge_list[i].cv1 != current_cv1 || 
                edge_list[i].cv2 != current_cv2) {
                
                // Store edge for both vertices
                degree[current_cv1]++;
                degree[current_cv2]++;
                
                // Move to next edge if not at end
                if (i < edge_list.size()) {
                    current_cv1 = edge_list[i].cv1;
                    current_cv2 = edge_list[i].cv2;
                    weight = 1;
                }
            } else {
                weight++;
            }
        }
    }

    // Build XADJ array (CSR format)
    std::vector<int> coarse_xadj(n_coarse + 1);
    coarse_xadj[0] = 1;  // 1-indexed start for METIS
    for (int cv = 0; cv < n_coarse; ++cv) {
        int64_t next_val = static_cast<int64_t>(coarse_xadj[cv]) + static_cast<int64_t>(degree[cv]);
        if (next_val > std::numeric_limits<int>::max()) {
            std::fprintf(stdout, "[METIS coarse] ERROR: XADJ array overflow at coarse vertex %d\n", cv);
            return -1;
        }
        coarse_xadj[cv + 1] = static_cast<int>(next_val);
    }

    size_t n_coarse_edges = static_cast<size_t>(coarse_xadj[n_coarse] - 1);
    std::vector<int> coarse_adjncy(n_coarse_edges);
    std::vector<int> coarse_adjwgt(n_coarse_edges);

    // Current write position for each vertex
    std::vector<int> write_pos(n_coarse);
    for (int cv = 0; cv < n_coarse; ++cv) {
        write_pos[cv] = coarse_xadj[cv] - 1;  // Convert to 0-indexed
    }

    // Fill adjacency arrays by iterating through merged edges
    if (!edge_list.empty()) {
        int current_cv1 = edge_list[0].cv1;
        int current_cv2 = edge_list[0].cv2;
        int weight = 1;
        
        for (size_t i = 1; i <= edge_list.size(); ++i) {
            if (i == edge_list.size() || 
                edge_list[i].cv1 != current_cv1 || 
                edge_list[i].cv2 != current_cv2) {
                
                // Write edge in both directions
                coarse_adjncy[write_pos[current_cv1]] = current_cv2 + 1;  // 1-indexed
                coarse_adjwgt[write_pos[current_cv1]] = weight;
                write_pos[current_cv1]++;
                
                coarse_adjncy[write_pos[current_cv2]] = current_cv1 + 1;  // 1-indexed
                coarse_adjwgt[write_pos[current_cv2]] = weight;
                write_pos[current_cv2]++;
                
                if (i < edge_list.size()) {
                    current_cv1 = edge_list[i].cv1;
                    current_cv2 = edge_list[i].cv2;
                    weight = 1;
                }
            } else {
                weight++;
            }
        }
    }

    // =========================================================================
    // Step 5: Build coarse vertex weights
    // =========================================================================

    // Check for potential overflow in weight array size
    int64_t weight_array_size = static_cast<int64_t>(n_coarse) * static_cast<int64_t>(ncond);
    if (weight_array_size > std::numeric_limits<int>::max()) {
        std::fprintf(stdout, "[METIS coarse] ERROR: Coarse weight array size overflow\n");
        return -1;
    }

    std::vector<int> coarse_vwgt(static_cast<size_t>(weight_array_size));
    
    for (int cv = 0; cv < n_coarse; ++cv) {
        const auto &elems = coarse_to_elements[cv];
        for (int c = 0; c < ncond; ++c) {
            int64_t w = 0;
            for (int e : elems) {
                size_t idx = static_cast<size_t>(e) * ncond + static_cast<size_t>(c);
                w += IWD[idx];
            }
            if (w > std::numeric_limits<int>::max() || w < std::numeric_limits<int>::min()) {
                std::fprintf(stdout, "[METIS coarse] WARNING: Weight overflow for coarse vertex %d, constraint %d\n", cv, c);
                w = std::numeric_limits<int>::max();
            }
            size_t weight_idx = static_cast<size_t>(cv) * ncond + static_cast<size_t>(c);
            coarse_vwgt[weight_idx] = static_cast<int>(w);
        }
    }

    // =========================================================================
    // Step 5.5: Add fictive edges for poorly-connected parts
    // =========================================================================

    int n_fictive = add_fictive_edges_for_connectivity(
        n_coarse,
        elem_to_coarse,
        coarse_to_part,
        coarse_to_elements,
        part,
        nelem,
        coarse_xadj,
        coarse_adjncy,
        coarse_adjwgt
    );

    // =========================================================================
    // Step 6: Call METIS on coarse graph
    // =========================================================================

    std::vector<int> coarse_partition(n_coarse);

    int metis_ncon = ncond;
    int metis_nparts = npart;
    int metis_edgecut = 0;

    int ierr = part_fn(
        &n_coarse,
        &metis_ncon,
        coarse_xadj.data(),
        coarse_adjncy.data(),
        coarse_vwgt.data(),      // vwgt
        nullptr,                  // vsize
        coarse_adjwgt.data(),    // adjwgt
        &metis_nparts,
        nullptr,                  // tpwgts
        UBVEC,
        OPTIONS,                  // Use original OPTIONS (with NUMBERING=1)
        &metis_edgecut,
        coarse_partition.data()
    );

    *NEC = metis_edgecut;

    // =========================================================================
    // Step 7: Map coarse partition back to original elements
    // =========================================================================

    for (int e = 0; e < nelem; ++e) {
        int cv = elem_to_coarse[e];
        CEP[e] = coarse_partition[cv];
    }

    return ierr;
}

/**
 * Build a coarse graph by merging small parts and subdividing large parts, then partition with METIS.
 *
 * This function assumes Fortran calling convention with 1-based indexing.
 *
 * Arguments:
 *   NELEM   - number of elements
 *   NCOND   - number of constraints (vertex weight dimensions)
 *   XADJ    - adjacency index array (size NELEM+1, 1-indexed values)
 *   ADJNCY  - adjacency list (1-indexed element IDs)
 *   IWD     - vertex weights                                                                  
 *   NNODE   - number of partitions requested
 *   UBVEC   - imbalance tolerance per constraint (size NCOND)
 *   OPTIONS - METIS options array (must exist)
 *   NEC     - output: edge cut
 *   CEP     - output: partition assignment per element (1 to npart)
 *   part    - input: part ID per element (negative values = element not in any part)
 *
 * Returns: METIS return code (or -1 for internal errors)
 */
int wrap_metis_partgraphkway_part(
    int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
    int *IWD, int *NNODE,
    float *UBVEC, int *OPTIONS, int *NEC, int *CEP, int *part)
{
    return metis_partition_with_coarsening(
        METIS_PartGraphKway,
        NELEM, NCOND, XADJ, ADJNCY,
        IWD, NNODE,
        UBVEC, OPTIONS, NEC, CEP, part);
}

int wrap_metis_partgraphrecursive_part(
    int *NELEM, int *NCOND, int *XADJ, int *ADJNCY,
    int *IWD, int *NNODE,
    float *UBVEC, int *OPTIONS, int *NEC, int *CEP, int *part)
{
    return metis_partition_with_coarsening(
        METIS_PartGraphRecursive,
        NELEM, NCOND, XADJ, ADJNCY,
        IWD, NNODE,
        UBVEC, OPTIONS, NEC, CEP, part);
}

} // extern "C"
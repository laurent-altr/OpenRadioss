//Copyright>        OpenRadioss
//Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
//Copyright>
//Copyright>        This program is free software: you can redistribute it and/or modify
//Copyright>        it under the terms of the GNU Affero General Public License as published by
//Copyright>        the Free Software Foundation, either version 3 of the License, or
//Copyright>        (at your option) any later version.
//Copyright>
//Copyright>        This program is distributed in the hope that it will be useful,
//Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>        GNU Affero General Public License for more details.
//Copyright>
//Copyright>        You should have received a copy of the GNU Affero General Public License
//Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>        Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>        software under a commercial license.  Contact Altair to discuss further if the
//Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
// ======================================================================================================================
// c_remnode_bfs.cpp
// Dijkstra-based mesh-geodesic BFS to find secondary nodes to exclude from contact detection.
// Called from Fortran via iso_c_binding.
// ======================================================================================================================

#include <vector>
#include <queue>
#include <cmath>
#include <algorithm>
#include <cstring>
#include <limits>
#include <cstdio>

// Precision: consistent with Fortran MY_REAL / WP
#ifndef MYREAL4
using real_t = double;
#else
using real_t = float;
#endif

static constexpr real_t INFINITY_DIST = static_cast<real_t>(1.0e30);
static constexpr real_t SQRT2 = static_cast<real_t>(1.4142135623730951);

// ======================================================================================================================
// Dijkstra-based BFS for a single source segment.
// Computes geodesic distance (along mesh edges) from the source segment's nodes to all reachable
// secondary nodes, stopping when no node within dmax can be found.
//
// IMPORTANT semantic match with Fortran:
//   - Distances are ONLY propagated through secondary nodes and source segment nodes.
//   - Non-secondary, non-source nodes never get finite distances and cannot forward paths.
//   - This matches the Fortran behavior where dist1(non_secondary) stays at ep30.
//
// Returns the list of secondary node IDs (1-based) whose distance <= their removal threshold.
// ======================================================================================================================
static void dijkstra_single_segment(
    // Mesh connectivity (0-based indexing internally)
    int nrtm,
    int numnod,
    const int* irect,       // (4, nrtm) — Fortran column-major: irect(j,s) = irect[s*4 + j] (0-based)
    const int* knod2seg,    // (numnod+1) — CSR index (values are 0-based offsets)
    const int* nod2seg,     // (4*nrtm) — CSR values (1-based segment IDs from Fortran)
    const int* tagsecnd,    // (numnod) — 1 if secondary node, 0 otherwise
    const real_t* x,        // (3, numnod) — Fortran column-major: x(comp,node) = x[comp*numnod + node]
    // Gap parameters for this segment
    int igap,
    int seg_idx,            // 0-based segment index
    real_t dmax,            // maximum removal distance
    real_t gap_m_i,         // gap_m(seg)
    real_t gap_m_l_i,       // gap_m_l(seg)
    const real_t* gapsecnd, // (numnod)
    const real_t* gap_s_l_tmp, // (numnod)
    real_t gapmin,
    real_t gapmax,
    real_t drad,
    real_t dgapload,
    // Output
    std::vector<int>& removed_nodes  // filled with 1-based node IDs to remove
)
{
    // Priority queue: (distance, node_id_0based)
    using pq_entry = std::pair<real_t, int>;
    std::priority_queue<pq_entry, std::vector<pq_entry>, std::greater<pq_entry>> pq;

    // Per-node best distance. Only secondary nodes and source nodes get finite values.
    std::vector<real_t> dist(numnod, INFINITY_DIST);
    std::vector<bool> in_source_seg(numnod, false);

    // Track visited secondary nodes for efficient final collection
    std::vector<int> visited_secondary;
    visited_secondary.reserve(64);

    removed_nodes.clear();

    // Debug flag: print details for first segment
    bool debug = (seg_idx == 0);

    // Initialize: source segment nodes get distance = 0
    // irect is Fortran column-major with dims (4, nrtm):
    //   irect(j, s) in Fortran = irect[(s-1)*4 + (j-1)] in C (0-based)
    //   With 0-based seg_idx and j: irect[seg_idx*4 + j]
    int jmax = 4;
    int n3 = irect[seg_idx * 4 + 2]; // node 3 (1-based Fortran node ID)
    int n4 = irect[seg_idx * 4 + 3]; // node 4

    // Detect triangle: node4==0 or node3==node4
    if (n4 == 0 || n3 == n4) jmax = 3;

    if (debug) {
        std::fprintf(stderr, "=== C++ BFS DEBUG seg_idx=0 ===\n");
        std::fprintf(stderr, "  nrtm=%d numnod=%d dmax=%g\n", nrtm, numnod, dmax);
        std::fprintf(stderr, "  irect raw [0..7]: %d %d %d %d | %d %d %d %d\n",
            irect[0], irect[1], irect[2], irect[3],
            irect[4], irect[5], irect[6], irect[7]);
        std::fprintf(stderr, "  jmax=%d, n3=%d n4=%d\n", jmax, n3, n4);
    }

    for (int j = 0; j < jmax; ++j) {
        int node = irect[seg_idx * 4 + j] - 1; // convert to 0-based
        if (debug) {
            std::fprintf(stderr, "  source node j=%d: irect_val=%d node_0based=%d tagsecnd=%d\n",
                j, irect[seg_idx * 4 + j], node, (node >= 0 && node < numnod) ? tagsecnd[node] : -1);
            if (node >= 0 && node < numnod) {
                std::fprintf(stderr, "    knod2seg[%d]=%d knod2seg[%d]=%d (nbr adj segs=%d)\n",
                    node, knod2seg[node], node+1, knod2seg[node+1],
                    knod2seg[node+1] - knod2seg[node]);
            }
        }
        if (node < 0) continue;
        dist[node] = 0.0;
        in_source_seg[node] = true;
        pq.push({0.0, node});
    }

    // Dijkstra expansion through mesh connectivity.
    // Only secondary nodes (and source nodes) participate in distance propagation,
    // matching the Fortran behavior where non-secondary nodes keep dist=infinity.
    while (!pq.empty()) {
        auto [d, node] = pq.top();
        pq.pop();

        // Skip if we already found a shorter path to this node
        if (d > dist[node]) continue;

        // Early termination: if minimum distance exceeds dmax, stop
        if (d > dmax) break;

        // Expand: find all segments connected to this node via node-to-segment adjacency
        // knod2seg is (numnod+1): for 0-based node, range is [knod2seg[node], knod2seg[node+1])
        // nod2seg values are 1-based segment IDs from Fortran
        int seg_start = knod2seg[node];
        int seg_end   = knod2seg[node + 1];

        if (debug && d == 0.0) {
            std::fprintf(stderr, "  Expanding node %d (1-based=%d) dist=%g seg_range=[%d,%d)\n",
                node, node+1, d, seg_start, seg_end);
        }

        for (int k = seg_start; k < seg_end; ++k) {
            int seg1 = nod2seg[k] - 1; // convert to 0-based segment index
            if (seg1 < 0 || seg1 >= nrtm) continue;

            // Get number of nodes of adjacent segment
            int kmax = 4;
            int sn3 = irect[seg1 * 4 + 2];
            int sn4 = irect[seg1 * 4 + 3];
            if (sn4 == 0 || sn3 == sn4) kmax = 3;

            if (debug && d == 0.0) {
                std::fprintf(stderr, "    adj seg1=%d(1-based=%d) kmax=%d nodes:",
                    seg1, seg1+1, kmax);
                for (int ll = 0; ll < kmax; ++ll)
                    std::fprintf(stderr, " %d", irect[seg1*4+ll]);
                std::fprintf(stderr, "\n");
            }

            int relaxed_count = 0;
            for (int l = 0; l < kmax; ++l) {
                int neighbor = irect[seg1 * 4 + l] - 1; // 0-based
                if (neighbor < 0 || neighbor >= numnod) continue;

                // Skip source segment's own nodes (they already have dist=0)
                if (in_source_seg[neighbor]) continue;

                // CRITICAL: Only relax secondary nodes.
                // This matches the Fortran: non-secondary nodes never get a finite distance
                // and therefore cannot propagate paths further.
                if (tagsecnd[neighbor] == 0) continue;

                // Compute Euclidean distance between node and neighbor
                // x is Fortran x(3,numnod) column-major: x(comp,node) = x[node*3 + comp] in C
                real_t dx = x[neighbor * 3 + 0] - x[node * 3 + 0];
                real_t dy = x[neighbor * 3 + 1] - x[node * 3 + 1];
                real_t dz = x[neighbor * 3 + 2] - x[node * 3 + 2];
                real_t edge_len = std::sqrt(dx * dx + dy * dy + dz * dz);

                real_t new_dist = dist[node] + edge_len;

                if (debug && d == 0.0) {
                    std::fprintf(stderr, "      neighbor %d(1-based=%d) tagsecnd=%d edge_len=%g new_dist=%g\n",
                        neighbor, neighbor+1, tagsecnd[neighbor], edge_len, new_dist);
                }
                relaxed_count++;

                // Relaxation: update if we found a shorter path
                if (new_dist < dist[neighbor]) {
                    // Track first visit for efficient collection later
                    if (dist[neighbor] >= INFINITY_DIST) {
                        visited_secondary.push_back(neighbor);
                    }
                    dist[neighbor] = new_dist;
                    // Only push if within dmax (early cutoff for expansion)
                    if (new_dist <= dmax) {
                        pq.push({new_dist, neighbor});
                    }
                }
            }
            (void)relaxed_count;
        }
    }

    if (debug) {
        std::fprintf(stderr, "  Total visited_secondary=%zu\n", visited_secondary.size());
        for (size_t ii = 0; ii < std::min(visited_secondary.size(), (size_t)10); ++ii) {
            int n = visited_secondary[ii];
            std::fprintf(stderr, "    visited[%zu]: node %d(1-based=%d) dist=%g\n",
                ii, n, n+1, dist[n]);
        }
    }

    // Collect visited secondary nodes within their respective removal thresholds.
    // Only scan nodes that were actually visited (efficient).
    if (igap == 0) {
        for (int n : visited_secondary) {
            if (dist[n] <= dmax) {
                removed_nodes.push_back(n + 1); // return 1-based
            }
        }
    } else {
        for (int n : visited_secondary) {
            if (dist[n] >= INFINITY_DIST) continue;

            // Compute per-node effective gap threshold
            real_t gv = gapsecnd[n] + gap_m_i;
            if (igap == 3) {
                gv = std::min(gv, gap_s_l_tmp[n] + gap_m_l_i);
            }
            gv = std::min(gapmax, gv);
            gv = std::max(gapmin, gv);
            gv = std::max(drad, gv + dgapload);

            if (dist[n] <= SQRT2 * gv) {
                removed_nodes.push_back(n + 1); // return 1-based
            }
        }
    }
}

// ======================================================================================================================
// Main entry point called from Fortran.
// Processes all segments sequentially (no OpenMP here).
// Fills kremnode_counts and allocates/fills remnode_out.
//
// Memory contract:
//   - This function allocates *remnode_out and sets *remnode_size.
//   - Caller must call c_remnode_bfs_free() to release the memory.
// ======================================================================================================================
extern "C" {

void c_remnode_bfs_(
    const int* nrtm_p,
    const int* numnod_p,
    const int* igap_p,
    const int* irect,          // (4, nrtm) column-major
    const int* knod2seg,       // (numnod+1)  — Fortran 1-based CSR index
    const int* nod2seg,        // (4*nrtm)    — Fortran 1-based segment IDs
    const int* tagsecnd,       // (numnod)
    const real_t* x,           // (3, numnod) column-major
    const real_t* gap_m,       // (nrtm)
    const real_t* gap_m_l,     // (nrtm)
    const real_t* gapsecnd,    // (numnod)
    const real_t* gap_s_l_tmp, // (numnod)
    const real_t* gapmin_p,
    const real_t* gapmax_p,
    const real_t* gap_p,
    const real_t* drad_p,
    const real_t* dgapload_p,
    const real_t* gaps_mx_p,
    const real_t* gaps_l_mx_p,
    const real_t* minseg_p,
    // Outputs
    int* kremnode_counts,      // (nrtm) — per-segment count of removed nodes
    int** remnode_out,         // pointer to allocated flat array of removed node IDs
    int* remnode_size          // total number of entries in remnode_out
)
{
    const int nrtm    = *nrtm_p;
    const int numnod  = *numnod_p;
    const int igap    = *igap_p;
    const real_t gapmin   = *gapmin_p;
    const real_t gapmax   = *gapmax_p;
    const real_t gap      = *gap_p;
    const real_t drad     = *drad_p;
    const real_t dgapload = *dgapload_p;
    const real_t gaps_mx  = *gaps_mx_p;
    const real_t gaps_l_mx = *gaps_l_mx_p;
    // const real_t minseg   = *minseg_p; // not needed for Dijkstra (no level-based termination)
    (void)minseg_p; // unused

    // Convert knod2seg from Fortran 1-based CSR to 0-based.
    // Fortran: knod2seg(1)=0, knod2seg(node+1) - knod2seg(node) = degree(node)
    // nod2seg values are accessed as knod2seg(node)+1 to knod2seg(node+1) in Fortran.
    // So knod2seg[node-1] (0-based) gives the start index (0-based) into nod2seg.
    // Actually in Fortran: do k=knod2seg(node_id)+1,knod2seg(node_id+1)
    // knod2seg is indexed 1:numnod+1, with knod2seg(1)=0 typically.
    // In C with 0-based: knod2seg[node] (node 0-based) gives start, knod2seg[node+1] gives end.
    // The Fortran passes knod2seg(1:numnod+1), so in C it's knod2seg[0..numnod].
    // Fortran access: knod2seg(node_id)+1 to knod2seg(node_id+1) with node_id 1-based.
    // In C 0-based: knod2seg[node] to knod2seg[node+1] (exclusive), but nod2seg values are 1-based.
    // We handle this inside dijkstra_single_segment.

    // Accumulate all results
    std::vector<int> all_removed;
    all_removed.reserve(4 * nrtm); // initial guess

    for (int i = 0; i < nrtm; ++i) {
        // Compute dmax for this segment
        real_t dmax = 0.0;
        if (igap == 0) {
            dmax = SQRT2 * std::max(gap + dgapload, drad);
        } else if (igap == 1 || igap == 2) {
            dmax = SQRT2 * std::max(gap_m[i] + gaps_mx + dgapload, drad);
        } else if (igap == 3) {
            dmax = SQRT2 * std::max(
                std::min(gap_m[i] + gaps_mx, gap_m_l[i] + gaps_l_mx) + dgapload,
                drad);
        }

        std::vector<int> removed;
        dijkstra_single_segment(
            nrtm, numnod, irect, knod2seg, nod2seg, tagsecnd, x,
            igap, i, dmax,
            gap_m[i], gap_m_l[i],
            gapsecnd, gap_s_l_tmp,
            gapmin, gapmax, drad, dgapload,
            removed
        );

        kremnode_counts[i] = static_cast<int>(removed.size());
        all_removed.insert(all_removed.end(), removed.begin(), removed.end());
    }

    // Allocate output array and copy
    *remnode_size = static_cast<int>(all_removed.size());
    if (*remnode_size > 0) {
        *remnode_out = new int[*remnode_size];
        std::memcpy(*remnode_out, all_removed.data(), (*remnode_size) * sizeof(int));
    } else {
        *remnode_out = nullptr;
    }
}

// Free the memory allocated by c_remnode_bfs_
void c_remnode_bfs_free_(int** remnode_out)
{
    if (remnode_out && *remnode_out) {
        delete[] *remnode_out;
        *remnode_out = nullptr;
    }
}

// Uppercase variants for linker compatibility (some Fortran compilers)
void C_REMNODE_BFS(
    const int* nrtm_p, const int* numnod_p, const int* igap_p,
    const int* irect, const int* knod2seg, const int* nod2seg,
    const int* tagsecnd, const real_t* x,
    const real_t* gap_m, const real_t* gap_m_l,
    const real_t* gapsecnd, const real_t* gap_s_l_tmp,
    const real_t* gapmin_p, const real_t* gapmax_p,
    const real_t* gap_p, const real_t* drad_p,
    const real_t* dgapload_p, const real_t* gaps_mx_p,
    const real_t* gaps_l_mx_p, const real_t* minseg_p,
    int* kremnode_counts, int** remnode_out, int* remnode_size)
{
    c_remnode_bfs_(nrtm_p, numnod_p, igap_p, irect, knod2seg, nod2seg,
                   tagsecnd, x, gap_m, gap_m_l, gapsecnd, gap_s_l_tmp,
                   gapmin_p, gapmax_p, gap_p, drad_p, dgapload_p,
                   gaps_mx_p, gaps_l_mx_p, minseg_p,
                   kremnode_counts, remnode_out, remnode_size);
}

void c_remnode_bfs_free(int** p) { c_remnode_bfs_free_(p); }
void C_REMNODE_BFS_FREE(int** p) { c_remnode_bfs_free_(p); }

} // extern "C"

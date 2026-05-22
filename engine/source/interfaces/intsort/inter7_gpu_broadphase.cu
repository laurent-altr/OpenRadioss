/*Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.*/

/*
 * inter7_gpu_broadphase.cu
 *
 * GPU broad-phase collision detection for Interface Type 7.
 * See inter7_gpu_broadphase.h for the full design rationale.
 *
 * Algorithm (mirrors inter7_candidate_pairs.F90 / fill_voxel.F90)
 * ================================================================
 *
 * fill_voxel GPU path (inter7_gpu_fill_voxel_)
 * --------------------------------------------
 *   Parallel over slave nodes:
 *     1. Compute flat voxel cell index from node coordinate.
 *     2. Use atomicCAS-based linked-list insertion to build the same
 *        voxel[]/next_nod[] structure as the CPU path.
 *   Output: device copies of voxel[] and next_nod[] that mirror the CPU
 *   arrays exactly, allowing hybrid CPU/GPU operation if needed.
 *
 * candidate_pairs GPU path (inter7_gpu_candidate_pairs_)
 * -------------------------------------------------------
 *   One CUDA block per master segment (ne = 0..nrtm-1):
 *     1. Thread 0 loads segment corners, computes inflated AABB, surface
 *        normal (sx,sy,sz), and voxel range (ix1..ix2, iy1..iy2, iz1..iz2)
 *        into shared memory.
 *     2. Threads cooperatively sweep the voxel range; each thread handles
 *        a subset of the IX dimension.
 *     3. For each node found in a voxel cell the following rejection
 *        filters are applied (early exit on first rejection):
 *          (a) Corner self-pair: node global ID == M1/M2/M3/M4 → skip.
 *          (b) KREMNODE forbidden pair: binary search in the per-segment
 *              CSR list of forbidden node IDs → skip if found.
 *          (c) Expanded AABB: node outside [xmine-aaa, xmaxe+aaa] → skip.
 *          (d) Plane-distance underestimation: same formula as CPU → skip.
 *     4. Valid pairs are accumulated in shared memory and flushed to global
 *        device output using a single atomicAdd per block flush.
 *
 * PhysX integration
 * -----------------
 *   PxCudaContextManager is used for device context ownership and stream
 *   management.  This keeps CUDA contexts consistent when the calling
 *   simulation also uses PhysX (rigid-body simulation, cloth, etc.).
 *   If PhysX is not available the implementation falls back to raw CUDA.
 */

#include "inter7_gpu_broadphase.h"

#include <cuda_runtime.h>
#include <device_launch_parameters.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* ------------------------------------------------------------------ */
/* Optional PhysX CUDA context manager                                 */
/* ------------------------------------------------------------------ */
#ifdef USE_PHYSX
  #include <PxPhysicsAPI.h>
  using namespace physx;
  static PxFoundation*          g_foundation    = nullptr;
  static PxCudaContextManager*  g_cudaCtxMgr    = nullptr;
#endif

/* ------------------------------------------------------------------ */
/* Compile-time block size for the candidate kernel                    */
/* 128 threads per block gives good occupancy on SM7+ devices          */
/* ------------------------------------------------------------------ */
#define BLOCK_SIZE  128

/*
 * SPMD limitation
 * Remote (SPMD) slave nodes (jj > nsn in the Fortran linked list) are NOT
 * processed by the GPU kernel.  When nsnr > 0, the caller must supplement
 * GPU candidate pairs with CPU-computed remote-node pairs by running the
 * CPU inter7_candidate_pairs path after the GPU path, or by passing nsnr=0
 * and restricting the GPU call to local nodes only.
 */

/* ------------------------------------------------------------------ */
/* Global device state                                                 */
/* ------------------------------------------------------------------ */
static int   g_device_id   = -1;   /* -1 = not initialised             */
static bool  g_initialised = false;

/* Device persistent buffers (reallocated as needed) */
static int*        d_voxel     = nullptr;
static int*        d_next_nod  = nullptr;
static int*        d_nsv       = nullptr;
static int*        d_irect     = nullptr;
static int*        d_cand_n    = nullptr;
static int*        d_cand_e    = nullptr;
static int*        d_counter   = nullptr;   /* atomic output counter */
static int*        d_kremnod   = nullptr;
static int*        d_remnod    = nullptr;

static my_real_gpu* d_x        = nullptr;
static my_real_gpu* d_stf      = nullptr;
static my_real_gpu* d_gap_s    = nullptr;
static my_real_gpu* d_gap_m    = nullptr;
static my_real_gpu* d_curv_max = nullptr;

/* Sizes of current device allocations */
static int g_sz_voxel   = 0;
static int g_sz_nod     = 0;   /* nsn + nsnr */
static int g_sz_numnod  = 0;
static int g_sz_nrtm    = 0;
static int g_sz_cand    = 0;
static int g_sz_kremnod = 0;
static int g_sz_remnod  = 0;

/* ------------------------------------------------------------------ */
/* Helpers                                                             */
/* ------------------------------------------------------------------ */
#define CUDA_CHECK(call)                                                    \
  do {                                                                      \
    cudaError_t err = (call);                                               \
    if (err != cudaSuccess) {                                               \
      fprintf(stderr, "[GPU BPH] CUDA error %s at %s:%d\n",               \
              cudaGetErrorString(err), __FILE__, __LINE__);                 \
    }                                                                       \
  } while(0)

/* Reallocate device buffer if size has grown */
static void ensure_device_int(int** ptr, int* cur_sz, int needed)
{
    if (needed > *cur_sz) {
        if (*ptr) cudaFree(*ptr);
        CUDA_CHECK(cudaMalloc(ptr, (size_t)needed * sizeof(int)));
        *cur_sz = needed;
    }
}
static void ensure_device_real(my_real_gpu** ptr, int* cur_sz, int needed)
{
    if (needed > *cur_sz) {
        if (*ptr) cudaFree(*ptr);
        CUDA_CHECK(cudaMalloc(ptr, (size_t)needed * sizeof(my_real_gpu)));
        *cur_sz = needed;
    }
}

/* ================================================================== */
/* fill_voxel kernel                                                   */
/* Each thread processes one slave node and inserts it into the voxel  */
/* linked list using atomicExch-based compare-and-swap chaining.       */
/* ================================================================== */
__global__ void kernel_fill_voxel(
    int                    nsn,
    const int* __restrict__ nsv,       /* slave-to-global map [nsn]          */
    const my_real_gpu* __restrict__ x, /* node coords [3*numnod], col-major  */
    const my_real_gpu* __restrict__ stfn,
    my_real_gpu xmin, my_real_gpu ymin, my_real_gpu zmin,
    my_real_gpu xmax, my_real_gpu ymax, my_real_gpu zmax,
    my_real_gpu xminb, my_real_gpu yminb, my_real_gpu zminb,
    my_real_gpu xmaxb, my_real_gpu ymaxb, my_real_gpu zmaxb,
    int nbx, int nby, int nbz,
    int* __restrict__ voxel,           /* voxel head [(nbx+2)*(nby+2)*(nbz+2)] */
    int* __restrict__ next_nod         /* next-node chain [nsn]                */
)
{
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= nsn) return;

    /* Fortran 1-based index i+1 → local index i (0-based in C) */
    if (stfn[i] == (my_real_gpu)0) {
        next_nod[i] = 0;
        return;
    }

    /* Fortran: j = NSV(i+1),  x(1,j) stored at x[3*(j-1)+0] */
    int j = nsv[i] - 1;   /* 0-based global node index */
    my_real_gpu xj = x[3*j + 0];
    my_real_gpu yj = x[3*j + 1];
    my_real_gpu zj = x[3*j + 2];

    if (xj < xmin || xj > xmax || yj < ymin || yj > ymax || zj < zmin || zj > zmax) {
        next_nod[i] = 0;
        return;
    }

    /* Mirrors Fortran: MAX(1, 2+MIN(NBX, INT(NBX*(xj-xminb)/(xmaxb-xminb)))) */
    int ix = max(1, 2 + min(nbx, (int)(nbx * (xj - xminb) / (xmaxb - xminb))));
    int iy = max(1, 2 + min(nby, (int)(nby * (yj - yminb) / (ymaxb - yminb))));
    int iz = max(1, 2 + min(nbz, (int)(nbz * (zj - zminb) / (zmaxb - zminb))));

    /* Flat cell index (1-based in Fortran, 0-based C equivalent) */
    int cellid = (iz - 1) * (nbx + 2) * (nby + 2) + (iy - 1) * (nbx + 2) + ix - 1;

    /*
     * Atomic linked-list insertion.
     * voxel[cellid] holds the index (1-based, Fortran convention) of the
     * first node in the cell, or 0 if empty.
     * We insert node i+1 (Fortran 1-based) at the head atomically.
     */
    int old_head = atomicExch(&voxel[cellid], i + 1);  /* +1: Fortran 1-based */
    next_nod[i] = old_head;  /* chain the old head as our successor */
}

/*
 * Linear scan over the per-segment KREMNODE list.
 * Binary search would be faster for large lists but requires sorted input,
 * which the Fortran code does not guarantee.  Kremnode lists are typically
 * very short (O(10) entries = neighbouring elements), so linear scan is fine.
 */
__device__ __forceinline__ bool dev_kremnode_forbidden(
    const int* __restrict__ remnod, int lo, int hi, int global_node_id)
{
    for (int k = lo; k < hi; k++) {
        if (remnod[k] == global_node_id) return true;
    }
    return false;
}

/* ================================================================== */
/* Candidate pairs kernel                                              */
/* One block per master segment.  Threads cooperate over the voxel     */
/* range of the segment's inflated AABB.                               */
/* ================================================================== */
__global__ void kernel_candidate_pairs(
    /* geometry */
    int                     nrtm,
    int                     nsn,
    const int* __restrict__ irect,         /* [4 * nrtm], col-major: irect[ne + k*nrtm] */
    const my_real_gpu* __restrict__ x,     /* [3 * numnod], col-major */
    const int* __restrict__ nsv,           /* [nsn]                   */
    const int* __restrict__ voxel,         /* [(nbx+2)*(nby+2)*(nbz+2)] */
    const int* __restrict__ next_nod,      /* [nsn+nsnr]              */
    /* bounding box */
    my_real_gpu xminb, my_real_gpu yminb, my_real_gpu zminb,
    my_real_gpu xmaxb, my_real_gpu ymaxb, my_real_gpu zmaxb,
    int nbx, int nby, int nbz,
    /* gap / contact parameters */
    int igap,
    my_real_gpu marge,
    my_real_gpu tzinf,
    my_real_gpu gap,
    my_real_gpu gapmin, my_real_gpu gapmax, my_real_gpu bgapsmx,
    my_real_gpu drad,   my_real_gpu dgapload,
    const my_real_gpu* __restrict__ gap_s,
    const my_real_gpu* __restrict__ gap_m,
    const my_real_gpu* __restrict__ curv_max,
    const my_real_gpu* __restrict__ stf,
    /* kremnode rejection (CSR format) */
    int flagremnode,
    const int* __restrict__ kremnod,   /* [s_kremnod] – see below */
    const int* __restrict__ remnod,    /* [s_remnod]              */
    /*
     * KREMNODE CSR layout (0-based C, Fortran 1-based in the original):
     *   for segment ne (0-based):
     *     local forbidden: remnod[ kremnod[2*ne] .. kremnod[2*ne+1]-1 ]
     *   (sorted ascending per segment)
     */
    /* output */
    int*  cand_n,      /* [mulnsn] */
    int*  cand_e,      /* [mulnsn] */
    int*  g_counter,   /* global atomic counter */
    int   mulnsn       /* max candidates        */
)
{
    /* Each block handles one segment */
    int ne = blockIdx.x;
    if (ne >= nrtm) return;

    /* ---------------------------------------------------------------- */
    /* Shared memory                                                     */
    /* ---------------------------------------------------------------- */
    __shared__ my_real_gpu s_xx[4], s_yy[4], s_zz[4]; /* corner coords  */
    __shared__ my_real_gpu s_sx, s_sy, s_sz, s_s2;    /* surface normal */
    __shared__ my_real_gpu s_xmine, s_ymine, s_zmine;
    __shared__ my_real_gpu s_xmaxe, s_ymaxe, s_zmaxe;
    __shared__ my_real_gpu s_aaa;                      /* influence zone  */
    __shared__ int         s_m[4];                    /* corner node IDs */
    __shared__ int         s_ix1, s_iy1, s_iz1;
    __shared__ int         s_ix2, s_iy2, s_iz2;
    __shared__ int         s_krem_lo, s_krem_hi;      /* kremnode range  */
    __shared__ int         s_valid;                   /* segment alive?  */

    /*
     * No shared-memory output buffer.
     * Each valid pair is written to global memory via atomicAdd on g_counter.
     * This avoids the __syncthreads()-inside-conditional deadlock that a
     * mid-loop flush would cause (threads exit the inner while loop at
     * different times so a block-wide barrier inside it is illegal).
     * atomicAdd contention is acceptable since the broad phase produces
     * O(candidates) writes, which is small relative to the voxel traversal.
     */

    if (threadIdx.x == 0) {
        s_buf_cnt = 0;
        s_valid   = (stf[ne] != (my_real_gpu)0) ? 1 : 0;

        if (s_valid) {
            /*
             * Fortran IRECT(4,NRTM) is column-major: first index (k=1..4) varies
             * fastest in memory.  IRECT(k, ne+1) → C flat index ne*4 + (k-1).
             */
            s_m[0] = irect[ne * 4 + 0];   /* IRECT(1,ne+1) = M1 */
            s_m[1] = irect[ne * 4 + 1];   /* IRECT(2,ne+1) = M2 */
            s_m[2] = irect[ne * 4 + 2];   /* IRECT(3,ne+1) = M3 */
            s_m[3] = irect[ne * 4 + 3];   /* IRECT(4,ne+1) = M4 */

            /* Node coords (Fortran: x(k,j) → C: x[3*(j-1)+(k-1)]) */
            #define XC(j,k) x[3*((j)-1)+(k)]
            s_xx[0] = XC(s_m[0], 0); s_yy[0] = XC(s_m[0], 1); s_zz[0] = XC(s_m[0], 2);
            s_xx[1] = XC(s_m[1], 0); s_yy[1] = XC(s_m[1], 1); s_zz[1] = XC(s_m[1], 2);
            s_xx[2] = XC(s_m[2], 0); s_yy[2] = XC(s_m[2], 1); s_zz[2] = XC(s_m[2], 2);
            s_xx[3] = XC(s_m[3], 0); s_yy[3] = XC(s_m[3], 1); s_zz[3] = XC(s_m[3], 2);
            #undef XC

            s_xmaxe = max(max(s_xx[0], s_xx[1]), max(s_xx[2], s_xx[3]));
            s_xmine = min(min(s_xx[0], s_xx[1]), min(s_xx[2], s_xx[3]));
            s_ymaxe = max(max(s_yy[0], s_yy[1]), max(s_yy[2], s_yy[3]));
            s_ymine = min(min(s_yy[0], s_yy[1]), min(s_yy[2], s_yy[3]));
            s_zmaxe = max(max(s_zz[0], s_zz[1]), max(s_zz[2], s_zz[3]));
            s_zmine = min(min(s_zz[0], s_zz[1]), min(s_zz[2], s_zz[3]));

            /* Surface normal (cross product of diagonals, same as CPU) */
            s_sx = (s_yy[2]-s_yy[0])*(s_zz[3]-s_zz[1]) - (s_zz[2]-s_zz[0])*(s_yy[3]-s_yy[1]);
            s_sy = (s_zz[2]-s_zz[0])*(s_xx[3]-s_xx[1]) - (s_xx[2]-s_xx[0])*(s_zz[3]-s_zz[1]);
            s_sz = (s_xx[2]-s_xx[0])*(s_yy[3]-s_yy[1]) - (s_yy[2]-s_yy[0])*(s_xx[3]-s_xx[1]);
            s_s2 = s_sx*s_sx + s_sy*s_sy + s_sz*s_sz;

            /* Influence zone radius */
            if (igap == 0) {
                s_aaa = tzinf + curv_max[ne];
            } else {
                my_real_gpu g = min(gapmax, max(gapmin, bgapsmx + gap_m[ne])) + dgapload;
                s_aaa = marge + curv_max[ne] + max(g, drad);
            }

            /* Voxel range for inflated AABB */
            int ix1r, ix2r, iy1r, iy2r, iz1r, iz2r;
            if (nbx > 1) {
                my_real_gpu inv_dx = nbx / (xmaxb - xminb);
                ix1r = (int)((s_xmine - s_aaa - xminb) * inv_dx);
                ix2r = (int)((s_xmaxe + s_aaa - xminb) * inv_dx);
            } else { ix1r = -2; ix2r = 1; }
            if (nby > 1) {
                my_real_gpu inv_dy = nby / (ymaxb - yminb);
                iy1r = (int)((s_ymine - s_aaa - yminb) * inv_dy);
                iy2r = (int)((s_ymaxe + s_aaa - yminb) * inv_dy);
            } else { iy1r = -2; iy2r = 1; }
            if (nbz > 1) {
                my_real_gpu inv_dz = nbz / (zmaxb - zminb);
                iz1r = (int)((s_zmine - s_aaa - zminb) * inv_dz);
                iz2r = (int)((s_zmaxe + s_aaa - zminb) * inv_dz);
            } else { iz1r = -2; iz2r = 1; }

            s_ix1 = max(1, 2 + min(nbx, ix1r));
            s_iy1 = max(1, 2 + min(nby, iy1r));
            s_iz1 = max(1, 2 + min(nbz, iz1r));
            s_ix2 = max(1, 2 + min(nbx, ix2r));
            s_iy2 = max(1, 2 + min(nby, iy2r));
            s_iz2 = max(1, 2 + min(nbz, iz2r));

            /* KREMNODE range for this segment (CSR 0-based) */
            if (flagremnode == 2) {
                s_krem_lo = kremnod[2 * ne];
                s_krem_hi = kremnod[2 * ne + 1];
            } else {
                s_krem_lo = 0;
                s_krem_hi = 0;
            }
        }
    }
    __syncthreads();

    if (!s_valid) return;

    /* ---------------------------------------------------------------- */
    /* Sweep voxel range: distribute IX over threads in the block        */
    /* ---------------------------------------------------------------- */
    int ix_range = s_ix2 - s_ix1 + 1;
    int nbx2     = nbx + 2;
    int nbxnby   = nbx2 * (nby + 2);

    for (int iz = s_iz1; iz <= s_iz2; iz++) {
        for (int iy = s_iy1; iy <= s_iy2; iy++) {
            /* Base cell index for (iz, iy, *) */
            int cell_base = (iz - 1) * nbxnby + (iy - 1) * nbx2;

            for (int ix_off = threadIdx.x; ix_off < ix_range; ix_off += blockDim.x) {
                int ix = s_ix1 + ix_off;
                int cellid = cell_base + ix - 1;   /* 0-based flat index */

                /* Walk the linked list for this cell */
                int jj = voxel[cellid];
                while (jj > 0) {
                    /* Only local slave nodes (jj <= nsn).
                     * Remote SPMD nodes (jj > nsn) are handled by the CPU
                     * path (SPMD exchange precedes this call) – skip here. */
                    if (jj <= nsn) {
                        int nn = nsv[jj - 1];  /* global node ID (1-based) */

                        /* (a) Corner self-pair rejection */
                        if (nn == s_m[0] || nn == s_m[1] ||
                            nn == s_m[2] || nn == s_m[3]) {
                            jj = next_nod[jj - 1];
                            continue;
                        }

                        /* (b) KREMNODE forbidden pair rejection */
                        if (flagremnode == 2 && s_krem_lo < s_krem_hi) {
                            if (dev_kremnode_forbidden(remnod, s_krem_lo, s_krem_hi, nn)) {
                                jj = next_nod[jj - 1];
                                continue;
                            }
                        }

                        /* Node position */
                        my_real_gpu xs = x[3*(nn-1) + 0];
                        my_real_gpu ys = x[3*(nn-1) + 1];
                        my_real_gpu zs = x[3*(nn-1) + 2];

                        /* Per-node AAA when igap != 0 */
                        my_real_gpu aaa = s_aaa;
                        if (igap != 0) {
                            my_real_gpu g = min(gapmax, max(gapmin,
                                gap_s[jj-1] + gap_m[ne])) + dgapload;
                            aaa = marge + curv_max[ne] + max(g, drad);
                        }

                        /* (c) Expanded AABB rejection */
                        if (xs <= s_xmine - aaa || xs >= s_xmaxe + aaa ||
                            ys <= s_ymine - aaa || ys >= s_ymaxe + aaa ||
                            zs <= s_zmine - aaa || zs >= s_zmaxe + aaa) {
                            jj = next_nod[jj - 1];
                            continue;
                        }

                        /* (d) Plane-distance underestimation (same as CPU) */
                        my_real_gpu d1x = xs - s_xx[0], d1y = ys - s_yy[0], d1z = zs - s_zz[0];
                        my_real_gpu d2x = xs - s_xx[1], d2y = ys - s_yy[1], d2z = zs - s_zz[1];
                        my_real_gpu dd1 = d1x*s_sx + d1y*s_sy + d1z*s_sz;
                        my_real_gpu dd2 = d2x*s_sx + d2y*s_sy + d2z*s_sz;
                        if (dd1 * dd2 > (my_real_gpu)0) {
                            my_real_gpu d2  = min(dd1*dd1, dd2*dd2);
                            my_real_gpu a2  = aaa*aaa*s_s2;
                            if (d2 > a2) {
                                jj = next_nod[jj - 1];
                                continue;
                            }
                        }

                        /* Valid candidate – write directly to global output */
                        {
                            int slot = atomicAdd(g_counter, 1);
                            if (slot < mulnsn) {
                                cand_n[slot] = jj;      /* local slave index (1-based) */
                                cand_e[slot] = ne + 1;  /* local segment index (1-based) */
                            }
                        }
                    }
                    jj = next_nod[jj - 1];
                }
            }
        }
    }

}

/* ================================================================== */
/* Public C API                                                        */
/* ================================================================== */

int inter7_gpu_broadphase_init_(void)
{
    if (g_initialised) return 0;

    int device_count = 0;
    cudaError_t err = cudaGetDeviceCount(&device_count);
    if (err != cudaSuccess || device_count == 0) {
        fprintf(stderr, "[GPU BPH] No CUDA-capable device found.\n");
        return 1;
    }

    /* Pick device 0 (or the one set by CUDA_VISIBLE_DEVICES) */
    g_device_id = 0;
    CUDA_CHECK(cudaSetDevice(g_device_id));

#ifdef USE_PHYSX
    /* Initialise PhysX Foundation and CUDA context manager */
    g_foundation = PxCreateFoundation(PX_PHYSICS_VERSION,
                                      gDefaultAllocatorCallback,
                                      gDefaultErrorCallback);
    if (!g_foundation) {
        fprintf(stderr, "[GPU BPH] PhysX Foundation init failed.\n");
        return 1;
    }
    PxCudaContextManagerDesc cudaCtxDesc;
    g_cudaCtxMgr = PxCreateCudaContextManager(*g_foundation, cudaCtxDesc,
                                               PxGetProfilerCallback());
    if (!g_cudaCtxMgr || !g_cudaCtxMgr->contextIsValid()) {
        fprintf(stderr, "[GPU BPH] PhysX CUDA context init failed.\n");
        return 1;
    }
    fprintf(stderr, "[GPU BPH] PhysX CUDA context manager ready.\n");
#endif

    /* Allocate the atomic output counter */
    CUDA_CHECK(cudaMalloc(&d_counter, sizeof(int)));
    CUDA_CHECK(cudaMemset(d_counter, 0, sizeof(int)));

    g_initialised = true;
    return 0;
}

void inter7_gpu_broadphase_destroy_(void)
{
    if (!g_initialised) return;

    if (d_voxel)    { cudaFree(d_voxel);    d_voxel    = nullptr; }
    if (d_next_nod) { cudaFree(d_next_nod); d_next_nod = nullptr; }
    if (d_nsv)      { cudaFree(d_nsv);      d_nsv      = nullptr; }
    if (d_irect)    { cudaFree(d_irect);    d_irect    = nullptr; }
    if (d_cand_n)   { cudaFree(d_cand_n);   d_cand_n   = nullptr; }
    if (d_cand_e)   { cudaFree(d_cand_e);   d_cand_e   = nullptr; }
    if (d_counter)  { cudaFree(d_counter);  d_counter  = nullptr; }
    if (d_kremnod)  { cudaFree(d_kremnod);  d_kremnod  = nullptr; }
    if (d_remnod)   { cudaFree(d_remnod);   d_remnod   = nullptr; }
    if (d_x)        { cudaFree(d_x);        d_x        = nullptr; }
    if (d_stf)      { cudaFree(d_stf);      d_stf      = nullptr; }
    if (d_gap_s)    { cudaFree(d_gap_s);    d_gap_s    = nullptr; }
    if (d_gap_m)    { cudaFree(d_gap_m);    d_gap_m    = nullptr; }
    if (d_curv_max) { cudaFree(d_curv_max); d_curv_max = nullptr; }

    g_sz_voxel = g_sz_nod = g_sz_numnod = g_sz_nrtm = 0;
    g_sz_cand  = g_sz_kremnod = g_sz_remnod = 0;

#ifdef USE_PHYSX
    if (g_cudaCtxMgr) { g_cudaCtxMgr->release(); g_cudaCtxMgr = nullptr; }
    if (g_foundation)  { g_foundation->release();  g_foundation  = nullptr; }
#endif

    g_initialised = false;
}

int inter7_gpu_available_(void)
{
    return g_initialised ? 1 : 0;
}

/* ------------------------------------------------------------------ */
void inter7_gpu_fill_voxel_(
    const int*       nsn,
    const int*       nsnr,
    const int*       nbx,
    const int*       nby,
    const int*       nbz,
    const int*       nrtm,
    const int*       numnod,
    const int*       nsv,
    const my_real_gpu* x,
    const my_real_gpu* stfn,
    const my_real_gpu* box_limit  /* same layout as BMINMA in fill_voxel.F90 */
)
{
    if (!g_initialised) return;
    if (*nrtm == 0 || *nsn == 0) return;

    int h_nbx = *nbx, h_nby = *nby, h_nbz = *nbz;
    int h_nsn = *nsn, h_nsnr = *nsnr, h_numnod = *numnod;
    int voxel_sz = (h_nbx + 2) * (h_nby + 2) * (h_nbz + 2);
    int nod_sz   = h_nsn + h_nsnr;

    /* Reallocate device buffers as needed */
    ensure_device_int(&d_voxel,     &g_sz_voxel,  voxel_sz);
    ensure_device_int(&d_next_nod,  &g_sz_nod,    nod_sz);
    ensure_device_int(&d_nsv,       &g_sz_nod,    h_nsn);      /* may already be large enough */
    ensure_device_real(&d_x,        &g_sz_numnod, 3 * h_numnod);

    /* BMINMA layout: (1)=xmax,(2)=ymax,(3)=zmax,(4)=xmin,...
     * xmin = box_limit[3], xmax = box_limit[0] (0-based C) */
    my_real_gpu h_xmax  = box_limit[0], h_ymax  = box_limit[1], h_zmax  = box_limit[2];
    my_real_gpu h_xmin  = box_limit[3], h_ymin  = box_limit[4], h_zmin  = box_limit[5];
    my_real_gpu h_xmaxb = box_limit[6], h_ymaxb = box_limit[7], h_zmaxb = box_limit[8];
    my_real_gpu h_xminb = box_limit[9], h_yminb = box_limit[10],h_zminb = box_limit[11];

    /* Upload */
    CUDA_CHECK(cudaMemset(d_voxel,   0, (size_t)voxel_sz * sizeof(int)));
    CUDA_CHECK(cudaMemset(d_next_nod,0, (size_t)nod_sz   * sizeof(int)));
    CUDA_CHECK(cudaMemcpy(d_nsv, nsv,  (size_t)h_nsn * sizeof(int),       cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_x,   x,    (size_t)3*h_numnod*sizeof(my_real_gpu), cudaMemcpyHostToDevice));

    /* Also upload stfn (reuse d_gap_s buffer for this call) */
    ensure_device_real(&d_gap_s, &g_sz_nrtm, h_nsn);
    CUDA_CHECK(cudaMemcpy(d_gap_s, stfn, (size_t)h_nsn * sizeof(my_real_gpu), cudaMemcpyHostToDevice));

    /* Launch kernel */
    int threads = 256;
    int blocks  = (h_nsn + threads - 1) / threads;
    kernel_fill_voxel<<<blocks, threads>>>(
        h_nsn, d_nsv, d_x, d_gap_s,
        h_xmin, h_ymin, h_zmin, h_xmax, h_ymax, h_zmax,
        h_xminb, h_yminb, h_zminb, h_xmaxb, h_ymaxb, h_zmaxb,
        h_nbx, h_nby, h_nbz,
        d_voxel, d_next_nod
    );
    CUDA_CHECK(cudaGetLastError());
    CUDA_CHECK(cudaDeviceSynchronize());
}

/* ------------------------------------------------------------------ */
void inter7_gpu_candidate_pairs_(
    const int*       nsn,
    const int*       nrtm,
    const int*       numnod,
    const int*       nsv,
    const int*       irect,
    const my_real_gpu* x,
    const my_real_gpu* stf,
    const my_real_gpu* xyzm,
    const int*       voxel,
    const int*       next_nod,
    const int*       nsnr,
    const int*       nbx,
    const int*       nby,
    const int*       nbz,
    const int*       igap,
    const my_real_gpu* marge,
    const my_real_gpu* tzinf,
    const my_real_gpu* gap,
    const my_real_gpu* gapmin,
    const my_real_gpu* gapmax,
    const my_real_gpu* bgapsmx,
    const my_real_gpu* drad,
    const my_real_gpu* dgapload,
    const my_real_gpu* gap_s,
    const my_real_gpu* gap_m,
    const my_real_gpu* curv_max,
    const int*       flagremnode,
    const int*       s_kremnod,
    const int*       kremnod,
    const int*       s_remnod,
    const int*       remnod,
    const int*       mulnsn,
    int*             cand_n,
    int*             cand_e,
    int*             ii_stok
)
{
    if (!g_initialised) return;

    int h_nsn    = *nsn,    h_nrtm   = *nrtm;
    int h_numnod = *numnod, h_nsnr   = *nsnr;
    int h_nbx    = *nbx,    h_nby    = *nby,    h_nbz    = *nbz;
    int h_mulnsn = *mulnsn;
    int h_flagrn = *flagremnode;
    int h_igap   = *igap;

    if (h_nrtm == 0 || h_nsn == 0) { *ii_stok = 0; return; }

    int voxel_sz = (h_nbx + 2) * (h_nby + 2) * (h_nbz + 2);
    int nod_sz   = h_nsn + h_nsnr;

    /* ---- Reallocate device buffers ---- */
    ensure_device_int(&d_voxel,     &g_sz_voxel,  voxel_sz);
    ensure_device_int(&d_next_nod,  &g_sz_nod,    nod_sz);
    ensure_device_int(&d_nsv,       &g_sz_nod,    h_nsn);
    ensure_device_int(&d_irect,     &g_sz_nrtm,   4 * h_nrtm);
    ensure_device_int(&d_cand_n,    &g_sz_cand,   h_mulnsn);
    ensure_device_int(&d_cand_e,    &g_sz_cand,   h_mulnsn);
    ensure_device_real(&d_x,        &g_sz_numnod, 3 * h_numnod);
    ensure_device_real(&d_stf,      &g_sz_nrtm,   h_nrtm);
    ensure_device_real(&d_gap_s,    &g_sz_nod,    h_nsn);
    ensure_device_real(&d_gap_m,    &g_sz_nrtm,   h_nrtm);
    ensure_device_real(&d_curv_max, &g_sz_nrtm,   h_nrtm);

    /* KREMNODE CSR */
    int h_s_kremnod = *s_kremnod;
    int h_s_remnod  = *s_remnod;
    ensure_device_int(&d_kremnod, &g_sz_kremnod, max(h_s_kremnod, 1));
    ensure_device_int(&d_remnod,  &g_sz_remnod,  max(h_s_remnod,  1));

    /* ---- Upload host → device ---- */
    CUDA_CHECK(cudaMemcpy(d_voxel,    voxel,    (size_t)voxel_sz * sizeof(int),       cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_next_nod, next_nod, (size_t)nod_sz   * sizeof(int),       cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_nsv,      nsv,      (size_t)h_nsn    * sizeof(int),       cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_irect,    irect,    (size_t)4*h_nrtm * sizeof(int),       cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_x,        x,        (size_t)3*h_numnod*sizeof(my_real_gpu),cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_stf,      stf,      (size_t)h_nrtm   * sizeof(my_real_gpu),cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_gap_s,    gap_s,    (size_t)h_nsn    * sizeof(my_real_gpu),cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_gap_m,    gap_m,    (size_t)h_nrtm   * sizeof(my_real_gpu),cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_curv_max, curv_max, (size_t)h_nrtm   * sizeof(my_real_gpu),cudaMemcpyHostToDevice));
    if (h_s_kremnod > 0)
        CUDA_CHECK(cudaMemcpy(d_kremnod, kremnod, (size_t)h_s_kremnod * sizeof(int), cudaMemcpyHostToDevice));
    if (h_s_remnod > 0)
        CUDA_CHECK(cudaMemcpy(d_remnod,  remnod,  (size_t)h_s_remnod  * sizeof(int), cudaMemcpyHostToDevice));

    /* Reset output counter */
    CUDA_CHECK(cudaMemset(d_counter, 0, sizeof(int)));

    /*
     * xyzm layout (Fortran inter7_candidate_pairs.F90):
     *   xyzm(1..6)  = xmin,ymin,zmin,xmax,ymax,zmax
     *   xyzm(7..12) = xminb,yminb,zminb,xmaxb,ymaxb,zmaxb
     * In C (0-based): xyzm[0..5], xyzm[6..11]
     */
    my_real_gpu h_xminb = xyzm[6],  h_yminb = xyzm[7],  h_zminb = xyzm[8];
    my_real_gpu h_xmaxb = xyzm[9],  h_ymaxb = xyzm[10], h_zmaxb = xyzm[11];

    /* ---- Launch candidate kernel: one block per segment ---- */
    kernel_candidate_pairs<<<h_nrtm, BLOCK_SIZE>>>(
        h_nrtm, h_nsn,
        d_irect, d_x, d_nsv,
        d_voxel, d_next_nod,
        h_xminb, h_yminb, h_zminb,
        h_xmaxb, h_ymaxb, h_zmaxb,
        h_nbx, h_nby, h_nbz,
        h_igap,
        *marge, *tzinf, *gap, *gapmin, *gapmax, *bgapsmx, *drad, *dgapload,
        d_gap_s, d_gap_m, d_curv_max, d_stf,
        h_flagrn, d_kremnod, d_remnod,
        d_cand_n, d_cand_e, d_counter, h_mulnsn
    );
    CUDA_CHECK(cudaGetLastError());
    CUDA_CHECK(cudaDeviceSynchronize());

    /* ---- Read back results ---- */
    int h_count = 0;
    CUDA_CHECK(cudaMemcpy(&h_count, d_counter, sizeof(int), cudaMemcpyDeviceToHost));
    h_count = min(h_count, h_mulnsn);

    if (h_count > 0) {
        CUDA_CHECK(cudaMemcpy(cand_n, d_cand_n, (size_t)h_count * sizeof(int), cudaMemcpyDeviceToHost));
        CUDA_CHECK(cudaMemcpy(cand_e, d_cand_e, (size_t)h_count * sizeof(int), cudaMemcpyDeviceToHost));
    }
    *ii_stok = h_count;
}

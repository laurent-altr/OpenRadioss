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
 * GPU broad-phase collision detection using PhysX 5.x PxBroadPhase (eGPU).
 *
 * Why PxBroadPhase instead of a custom CUDA voxel kernel
 * =======================================================
 * PhysX ships a GPU-accelerated Sort-and-Prune (SAP) broad phase
 * (PxBroadPhaseType::eGPU) that:
 *   - runs entirely on the GPU via CUDA
 *   - supports incremental updates (only re-sorts objects that moved)
 *   - handles filter groups natively (master/slave asymmetry is free)
 *   - is heavily optimised and validated by NVIDIA
 *
 * Architecture
 * ============
 *
 * Step 1  build_slave_aabbs  (CUDA kernel, N threads for N slave nodes)
 *   Each slave node i becomes a point AABB [x,x]×[y,y]×[z,z].
 *   filterGroup = FILTER_SLAVE (0x1)
 *   PhysX suppresses pairs when (A.group & B.group) != 0, so slave–slave
 *   pairs are suppressed and only slave–master pairs are produced.
 *
 * Step 2  build_segment_aabbs  (CUDA kernel, N threads for N segments)
 *   Each master segment ne becomes an inflated AABB:
 *     [xmine - AAA, xmaxe + AAA] in each dimension
 *   where AAA is computed with bgapsmx (overestimation of gap_s).
 *   filterGroup = FILTER_MASTER (0x2)
 *
 *   Using bgapsmx makes the AABB conservative: guaranteed to contain any
 *   slave node that would pass the per-node AABB test (no false negatives).
 *   False positives are removed by the post-filter (Step 4).
 *
 * Step 3  PxBroadPhase::update()  (GPU, inside PhysX)
 *   Objects are submitted via PxBroadPhaseUpdateData with handle-indexed
 *   mBounds and mGroups arrays.  PhysX Sort-and-Prune on the GPU finds all
 *   overlapping (slave, master) AABB pairs.
 *
 * Step 4  postfilter_pairs  (CUDA kernel, one thread per PhysX pair)
 *   For each pair (slave_idx, segment_idx) returned by PhysX, apply:
 *   (a) Corner self-pair: node global ID == M1/M2/M3/M4  → reject
 *   (b) KREMNODE forbidden pair: linear scan over per-segment list → reject
 *   (c) Per-node expanded AABB with actual gap_s[slave_idx]         → reject
 *   (d) Plane-distance underestimation (same formula as CPU)        → reject
 *   Valid pairs are written to output arrays via atomicAdd.
 *
 * Master-surface vs slave-node asymmetry
 * =======================================
 * PhysX filter-group semantics: pair (A,B) suppressed iff (A.group & B.group) != 0.
 *   FILTER_SLAVE  = 0x1:  (0x1 & 0x1)=1 → slave-slave suppressed  ✓
 *   FILTER_MASTER = 0x2:  (0x2 & 0x2)=2 → master-master suppressed ✓
 *   slave-master:         (0x1 & 0x2)=0 → pair generated           ✓
 *
 * PxBroadPhaseUpdateData API notes (PhysX 5.x)
 * =============================================
 * - mBounds and mGroups are handle-indexed arrays of capacity mCapacity.
 *   If handle h is in mCreated, then mBounds[h] and mGroups[h] are read.
 *   The arrays must remain valid until fetchResults() returns.
 * - There is no mEnvIDs field; mCapacity is the array capacity.
 * - We use PxBroadPhase directly (not PxAABBManager) because
 *   PxAABBManager::update() does not accept PxBroadPhaseUpdateData.
 * - We rebuild all objects every timestep (remove-then-add) so that
 *   fetchResults().mCreatedPairs contains all current overlaps.
 *   An incremental update-only approach would miss pre-existing contacts.
 */

#include "inter7_gpu_broadphase.h"

#include <PxPhysicsAPI.h>
#include <cuda_runtime.h>
#include <stdio.h>
#include <string.h>

using namespace physx;

/* ------------------------------------------------------------------ */
/* Filter groups                                                       */
/* ------------------------------------------------------------------ */
#define FILTER_SLAVE   0x1u
#define FILTER_MASTER  0x2u

/* ------------------------------------------------------------------ */
/* Error checking                                                      */
/* ------------------------------------------------------------------ */
#define CUDA_CHECK(call)                                                    \
  do {                                                                      \
    cudaError_t _e = (call);                                               \
    if (_e != cudaSuccess)                                                  \
      fprintf(stderr,"[GPU BPH] CUDA error %s at %s:%d\n",               \
              cudaGetErrorString(_e),__FILE__,__LINE__);                   \
  } while(0)

/* ------------------------------------------------------------------ */
/* Buffer helpers                                                      */
/* ------------------------------------------------------------------ */
/* Device buffers */
static void ensure_device_int(int** p, int* sz, int n)
{
    if (n > *sz) {
        if (*p) cudaFree(*p);
        CUDA_CHECK(cudaMalloc(p, (size_t)n * sizeof(int)));
        *sz = n;
    }
}
static void ensure_device_real(my_real_gpu** p, int* sz, int n)
{
    if (n > *sz) {
        if (*p) cudaFree(*p);
        CUDA_CHECK(cudaMalloc(p, (size_t)n * sizeof(my_real_gpu)));
        *sz = n;
    }
}

/* Host buffers – raw bytes, separate size per pointer */
static void ensure_host_raw(void** p, int* sz_bytes, int nbytes)
{
    if (nbytes > *sz_bytes) {
        free(*p);
        *p = malloc((size_t)nbytes);
        *sz_bytes = nbytes;
    }
}

/* ------------------------------------------------------------------ */
/* AabbEntry: same memory layout as PxBounds3 (2 × PxVec3 of floats) */
/* ------------------------------------------------------------------ */
struct AabbEntry { float lo[3]; float hi[3]; };

static void ensure_device_aabb(AabbEntry** p, int* sz, int n)
{
    if (n > *sz) {
        if (*p) cudaFree(*p);
        CUDA_CHECK(cudaMalloc(p, (size_t)n * sizeof(AabbEntry)));
        *sz = n;
    }
}

/* ================================================================== */
/* PhysX / CUDA global state                                          */
/* ================================================================== */
static PxDefaultAllocator       g_alloc;
static PxDefaultErrorCallback   g_errcb;
static PxFoundation*            g_foundation = nullptr;
static PxCudaContextManager*    g_cudaCtxMgr = nullptr;
static PxBroadPhase*            g_broadPhase = nullptr;  /* eGPU */
static bool                     g_initialised = false;

/* Device buffers (persistent across calls, grown on demand) */
static my_real_gpu* d_x        = nullptr;  static int g_sz_numnod  = 0;
static int*         d_nsv      = nullptr;  static int g_sz_nsn     = 0;
static my_real_gpu* d_stfn     = nullptr;  /* slave stiffness, shares g_sz_nsn */
static my_real_gpu* d_gap_s    = nullptr;  /* slave gap, shares g_sz_nsn */
static int*         d_irect    = nullptr;  static int g_sz_nrtm    = 0;
static my_real_gpu* d_stf      = nullptr;  /* segment stiffness, shares g_sz_nrtm */
static my_real_gpu* d_gap_m    = nullptr;  /* segment gap, shares g_sz_nrtm */
static my_real_gpu* d_curv_max = nullptr;  /* curvature, shares g_sz_nrtm */
static int*         d_kremnod  = nullptr;  static int g_sz_kremnod = 0;
static int*         d_remnod   = nullptr;  static int g_sz_remnod  = 0;
static int*         d_cand_n   = nullptr;  static int g_sz_cand    = 0;
static int*         d_cand_e   = nullptr;
static int*         d_counter  = nullptr;

/* GPU AABB arrays */
static AabbEntry*   d_slave_aabbs   = nullptr;  static int g_sz_slave_aabbs   = 0;
static AabbEntry*   d_segment_aabbs = nullptr;  static int g_sz_segment_aabbs = 0;
static int*         d_slave_active  = nullptr;  static int g_sz_slave_act     = 0;
static int*         d_seg_active    = nullptr;  static int g_sz_seg_act       = 0;

/* GPU pair arrays (post PhysX) */
static int*         d_pair_slave    = nullptr;  static int g_sz_pairs_d       = 0;
static int*         d_pair_seg      = nullptr;

/* ---- Host arrays ---- */

/* AABB and active masks downloaded from GPU */
static AabbEntry*   h_slave_aabbs   = nullptr;  static int h_sz_sa_bytes    = 0;
static AabbEntry*   h_segment_aabbs = nullptr;  static int h_sz_seg_bytes   = 0;
static int*         h_slave_active  = nullptr;  static int h_sz_sact_bytes  = 0;
static int*         h_seg_active    = nullptr;  static int h_sz_seact_bytes = 0;

/*
 * PhysX update arrays – HANDLE-INDEXED.
 * h_bounds[handle] and h_groups[handle] must be filled for every handle
 * in h_created[].  Capacity = total_objs = nsn + nrtm.
 * h_bounds has the same memory layout as PxBounds3 (cast is safe).
 */
static AabbEntry*   h_bounds  = nullptr;  static int h_sz_bnd_bytes = 0;
static PxU32*       h_groups  = nullptr;  static int h_sz_grp_bytes = 0;
static PxU32*       h_created = nullptr;  static int h_sz_cr_bytes  = 0;
static PxU32*       h_removed = nullptr;  static int h_sz_rm_bytes  = 0;

/* Pair host buffers */
static int*         h_pair_slave = nullptr;  static int h_sz_ps_bytes = 0;
static int*         h_pair_seg   = nullptr;  static int h_sz_pe_bytes = 0;

/* ================================================================== */
/* Step 1: build slave-node AABBs                                     */
/* Point AABB at the node position; inactive if stfn==0.             */
/* ================================================================== */
__global__ void kernel_build_slave_aabbs(
    int                        nsn,
    const int* __restrict__    nsv,
    const my_real_gpu* __restrict__ x,
    const my_real_gpu* __restrict__ stfn,
    AabbEntry* __restrict__    aabbs,
    int* __restrict__          active
)
{
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i >= nsn) return;
    if (stfn[i] == (my_real_gpu)0) { active[i] = 0; return; }
    active[i] = 1;
    int j = nsv[i] - 1;
    float xj = (float)x[3*j + 0];
    float yj = (float)x[3*j + 1];
    float zj = (float)x[3*j + 2];
    aabbs[i].lo[0] = xj; aabbs[i].lo[1] = yj; aabbs[i].lo[2] = zj;
    aabbs[i].hi[0] = xj; aabbs[i].hi[1] = yj; aabbs[i].hi[2] = zj;
}

/* ================================================================== */
/* Step 2: build master-segment AABBs                                 */
/* Inflated by AAA computed with bgapsmx so the box is conservative. */
/* ================================================================== */
__global__ void kernel_build_segment_aabbs(
    int                         nrtm,
    const int* __restrict__     irect,
    const my_real_gpu* __restrict__ x,
    const my_real_gpu* __restrict__ stf,
    const my_real_gpu* __restrict__ gap_m,
    const my_real_gpu* __restrict__ curv_max,
    my_real_gpu marge, my_real_gpu tzinf,
    my_real_gpu gapmin, my_real_gpu gapmax,
    my_real_gpu bgapsmx, my_real_gpu dgapload, my_real_gpu drad,
    int         igap,
    AabbEntry* __restrict__     aabbs,
    int* __restrict__           active
)
{
    int ne = blockIdx.x * blockDim.x + threadIdx.x;
    if (ne >= nrtm) return;
    if (stf[ne] == (my_real_gpu)0) { active[ne] = 0; return; }
    active[ne] = 1;

    /* IRECT(4,NRTM) column-major: IRECT(k,ne+1) = irect[ne*4 + k] (0-based k) */
    int m0 = irect[ne*4 + 0] - 1;
    int m1 = irect[ne*4 + 1] - 1;
    int m2 = irect[ne*4 + 2] - 1;
    int m3 = irect[ne*4 + 3] - 1;

    float xx0=(float)x[3*m0],xx1=(float)x[3*m1],xx2=(float)x[3*m2],xx3=(float)x[3*m3];
    float yy0=(float)x[3*m0+1],yy1=(float)x[3*m1+1],yy2=(float)x[3*m2+1],yy3=(float)x[3*m3+1];
    float zz0=(float)x[3*m0+2],zz1=(float)x[3*m1+2],zz2=(float)x[3*m2+2],zz3=(float)x[3*m3+2];

    float xmine = fminf(fminf(xx0,xx1),fminf(xx2,xx3));
    float xmaxe = fmaxf(fmaxf(xx0,xx1),fmaxf(xx2,xx3));
    float ymine = fminf(fminf(yy0,yy1),fminf(yy2,yy3));
    float ymaxe = fmaxf(fmaxf(yy0,yy1),fmaxf(yy2,yy3));
    float zmine = fminf(fminf(zz0,zz1),fminf(zz2,zz3));
    float zmaxe = fmaxf(fmaxf(zz0,zz1),fmaxf(zz2,zz3));

    float aaa;
    if (igap == 0) {
        aaa = (float)(tzinf + curv_max[ne]);
    } else {
        float g = fminf((float)gapmax, fmaxf((float)gapmin,
                        (float)(bgapsmx + gap_m[ne]))) + (float)dgapload;
        aaa = (float)(marge + curv_max[ne]) + fmaxf(g, (float)drad);
    }

    aabbs[ne].lo[0] = xmine - aaa;
    aabbs[ne].lo[1] = ymine - aaa;
    aabbs[ne].lo[2] = zmine - aaa;
    aabbs[ne].hi[0] = xmaxe + aaa;
    aabbs[ne].hi[1] = ymaxe + aaa;
    aabbs[ne].hi[2] = zmaxe + aaa;
}

/* ================================================================== */
/* Step 4: post-filter PhysX pairs                                    */
/* ================================================================== */
__global__ void kernel_postfilter_pairs(
    int                          npairs,
    const int* __restrict__      pair_slave,
    const int* __restrict__      pair_seg,
    int                          nrtm,
    const int* __restrict__      irect,
    const my_real_gpu* __restrict__ x,
    const int* __restrict__      nsv,
    int                          igap,
    my_real_gpu                  marge,
    my_real_gpu                  gapmin, my_real_gpu gapmax,
    my_real_gpu                  dgapload, my_real_gpu drad,
    const my_real_gpu* __restrict__ gap_s,
    const my_real_gpu* __restrict__ gap_m,
    const my_real_gpu* __restrict__ curv_max,
    int                          flagremnode,
    const int* __restrict__      kremnod,  /* CSR ptr [2*nrtm]: [start,end) */
    const int* __restrict__      remnod,
    int*                         cand_n,
    int*                         cand_e,
    int*                         g_counter,
    int                          mulnsn
)
{
    int tid = blockIdx.x * blockDim.x + threadIdx.x;
    if (tid >= npairs) return;

    int slave_idx = pair_slave[tid];
    int ne        = pair_seg[tid];

    int nn = nsv[slave_idx];   /* global node ID (1-based Fortran) */

    /* (a) Corner self-pair */
    int m0 = irect[ne*4 + 0];
    int m1 = irect[ne*4 + 1];
    int m2 = irect[ne*4 + 2];
    int m3 = irect[ne*4 + 3];
    if (nn == m0 || nn == m1 || nn == m2 || nn == m3) return;

    /* (b) KREMNODE linear scan (no sort requirement) */
    if (flagremnode == 2) {
        int lo = kremnod[2 * ne];
        int hi = kremnod[2 * ne + 1];
        for (int k = lo; k < hi; k++) {
            if (remnod[k] == nn) return;
        }
    }

    int jg = nn - 1;
    my_real_gpu xs = x[3*jg + 0];
    my_real_gpu ys = x[3*jg + 1];
    my_real_gpu zs = x[3*jg + 2];

    int jm0 = m0-1, jm1 = m1-1, jm2 = m2-1, jm3 = m3-1;
    my_real_gpu xx0=x[3*jm0],xx1=x[3*jm1],xx2=x[3*jm2],xx3=x[3*jm3];
    my_real_gpu yy0=x[3*jm0+1],yy1=x[3*jm1+1],yy2=x[3*jm2+1],yy3=x[3*jm3+1];
    my_real_gpu zz0=x[3*jm0+2],zz1=x[3*jm1+2],zz2=x[3*jm2+2],zz3=x[3*jm3+2];

    my_real_gpu xmine = min(min(xx0,xx1),min(xx2,xx3));
    my_real_gpu xmaxe = max(max(xx0,xx1),max(xx2,xx3));
    my_real_gpu ymine = min(min(yy0,yy1),min(yy2,yy3));
    my_real_gpu ymaxe = max(max(yy0,yy1),max(yy2,yy3));
    my_real_gpu zmine = min(min(zz0,zz1),min(zz2,zz3));
    my_real_gpu zmaxe = max(max(zz0,zz1),max(zz2,zz3));

    /* (c) Per-node AABB with actual gap_s (igap!=0 only) */
    my_real_gpu aaa = (my_real_gpu)0;
    if (igap != 0) {
        my_real_gpu g = min(gapmax, max(gapmin, gap_s[slave_idx] + gap_m[ne])) + dgapload;
        aaa = marge + curv_max[ne] + max(g, drad);
        if (xs <= xmine - aaa || xs >= xmaxe + aaa ||
            ys <= ymine - aaa || ys >= ymaxe + aaa ||
            zs <= zmine - aaa || zs >= zmaxe + aaa) return;
    }

    /* (d) Plane-distance underestimation */
    my_real_gpu sx = (yy2-yy0)*(zz3-zz1) - (zz2-zz0)*(yy3-yy1);
    my_real_gpu sy = (zz2-zz0)*(xx3-xx1) - (xx2-xx0)*(zz3-zz1);
    my_real_gpu sz = (xx2-xx0)*(yy3-yy1) - (yy2-yy0)*(xx3-xx1);
    my_real_gpu s2 = sx*sx + sy*sy + sz*sz;
    my_real_gpu d1x=xs-xx0, d1y=ys-yy0, d1z=zs-zz0;
    my_real_gpu d2x=xs-xx1, d2y=ys-yy1, d2z=zs-zz1;
    my_real_gpu dd1=d1x*sx+d1y*sy+d1z*sz;
    my_real_gpu dd2=d2x*sx+d2y*sy+d2z*sz;
    if (dd1*dd2 > (my_real_gpu)0) {
        my_real_gpu d2 = min(dd1*dd1, dd2*dd2);
        my_real_gpu a2 = aaa*aaa*s2;
        if (d2 > a2) return;
    }

    int slot = atomicAdd(g_counter, 1);
    if (slot < mulnsn) {
        cand_n[slot] = slave_idx + 1;
        cand_e[slot] = ne + 1;
    }
}

/* ================================================================== */
/* Public C API                                                        */
/* ================================================================== */

int inter7_gpu_broadphase_init_(void)
{
    if (g_initialised) return 0;

    g_foundation = PxCreateFoundation(PX_PHYSICS_VERSION, g_alloc, g_errcb);
    if (!g_foundation) {
        fprintf(stderr, "[GPU BPH] PxCreateFoundation failed.\n");
        return 1;
    }

    PxCudaContextManagerDesc cudaCtxDesc;
    g_cudaCtxMgr = PxCreateCudaContextManager(*g_foundation, cudaCtxDesc,
                                               PxGetProfilerCallback());
    if (!g_cudaCtxMgr || !g_cudaCtxMgr->contextIsValid()) {
        fprintf(stderr, "[GPU BPH] CUDA context init failed.\n");
        g_foundation->release(); g_foundation = nullptr;
        return 1;
    }

    PxBroadPhaseDesc bpDesc(PxBroadPhaseType::eGPU);
    bpDesc.mContextManager = g_cudaCtxMgr;
    g_broadPhase = PxCreateBroadPhase(bpDesc);
    if (!g_broadPhase) {
        fprintf(stderr, "[GPU BPH] PxCreateBroadPhase(eGPU) failed.\n");
        g_cudaCtxMgr->release(); g_cudaCtxMgr = nullptr;
        g_foundation->release(); g_foundation = nullptr;
        return 1;
    }

    CUDA_CHECK(cudaMalloc(&d_counter, sizeof(int)));
    g_initialised = true;
    fprintf(stderr, "[GPU BPH] PhysX eGPU broad phase ready.\n");
    return 0;
}

void inter7_gpu_broadphase_destroy_(void)
{
    if (!g_initialised) return;

    if (g_broadPhase) { g_broadPhase->release(); g_broadPhase = nullptr; }
    if (g_cudaCtxMgr) { g_cudaCtxMgr->release(); g_cudaCtxMgr = nullptr; }
    if (g_foundation)  { g_foundation->release(); g_foundation  = nullptr; }

    auto cfree = [](void* p){ if(p) cudaFree(p); };
    cfree(d_x);      cfree(d_nsv);      cfree(d_stfn);     cfree(d_gap_s);
    cfree(d_irect);  cfree(d_stf);      cfree(d_gap_m);    cfree(d_curv_max);
    cfree(d_kremnod);cfree(d_remnod);   cfree(d_cand_n);   cfree(d_cand_e);
    cfree(d_counter);
    cfree(d_slave_aabbs); cfree(d_segment_aabbs);
    cfree(d_slave_active); cfree(d_seg_active);
    cfree(d_pair_slave);  cfree(d_pair_seg);

    free(h_slave_aabbs);   free(h_segment_aabbs);
    free(h_slave_active);  free(h_seg_active);
    free(h_bounds);        free(h_groups);
    free(h_created);       free(h_removed);
    free(h_pair_slave);    free(h_pair_seg);

    g_initialised = false;
}

int inter7_gpu_available_(void) { return g_initialised ? 1 : 0; }

/* ------------------------------------------------------------------ */
/* fill_voxel stub: not used in the PhysX path                        */
/* ------------------------------------------------------------------ */
void inter7_gpu_fill_voxel_(
    const int* nsn, const int* nsnr,
    const int* nbx, const int* nby, const int* nbz,
    const int* nrtm, const int* numnod,
    const int* nsv, const my_real_gpu* x, const my_real_gpu* stfn,
    const my_real_gpu* box_limit)
{
    (void)nsn; (void)nsnr; (void)nbx; (void)nby; (void)nbz;
    (void)nrtm; (void)numnod; (void)nsv; (void)x; (void)stfn; (void)box_limit;
}

/* ------------------------------------------------------------------ */
void inter7_gpu_candidate_pairs_(
    const int*         nsn,
    const int*         nrtm,
    const int*         numnod,
    const int*         nsv,
    const int*         irect,
    const my_real_gpu* x,
    const my_real_gpu* stfn,      /* slave node stiffness [nsn] */
    const my_real_gpu* stf,
    const my_real_gpu* xyzm,
    const int*         voxel,
    const int*         next_nod,
    const int*         nsnr,
    const int*         nbx, const int* nby, const int* nbz,
    const int*         igap,
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
    const int*         flagremnode,
    const int*         s_kremnod,
    const int*         kremnod,
    const int*         s_remnod,
    const int*         remnod,
    const int*         mulnsn,
    int*               cand_n,
    int*               cand_e,
    int*               ii_stok
)
{
    (void)xyzm; (void)voxel; (void)next_nod;
    (void)nbx; (void)nby; (void)nbz; (void)gap;

    if (!g_initialised) return;
    int h_nsn    = *nsn,  h_nrtm  = *nrtm, h_numnod = *numnod;
    int h_mulnsn = *mulnsn;

    if (h_nsn == 0 || h_nrtm == 0) { *ii_stok = 0; return; }

    /* ---- upload geometry to device ---- */
    ensure_device_real(&d_x,        &g_sz_numnod, 3 * h_numnod);
    ensure_device_int (&d_nsv,      &g_sz_nsn,    h_nsn);
    ensure_device_real(&d_stfn,     &g_sz_nsn,    h_nsn);
    ensure_device_real(&d_gap_s,    &g_sz_nsn,    h_nsn);
    ensure_device_int (&d_irect,    &g_sz_nrtm,   4 * h_nrtm);
    ensure_device_real(&d_stf,      &g_sz_nrtm,   h_nrtm);
    ensure_device_real(&d_gap_m,    &g_sz_nrtm,   h_nrtm);
    ensure_device_real(&d_curv_max, &g_sz_nrtm,   h_nrtm);
    ensure_device_int (&d_kremnod,  &g_sz_kremnod, max(*s_kremnod, 1));
    ensure_device_int (&d_remnod,   &g_sz_remnod,  max(*s_remnod,  1));
    ensure_device_int (&d_cand_n,   &g_sz_cand,   h_mulnsn);
    ensure_device_int (&d_cand_e,   &g_sz_cand,   h_mulnsn);

    CUDA_CHECK(cudaMemcpy(d_x,        x,        3*h_numnod*sizeof(my_real_gpu), cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_nsv,      nsv,      h_nsn    *sizeof(int),          cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_stfn,     stfn,     h_nsn    *sizeof(my_real_gpu),  cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_gap_s,    gap_s,    h_nsn    *sizeof(my_real_gpu),  cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_irect,    irect,    4*h_nrtm *sizeof(int),          cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_stf,      stf,      h_nrtm   *sizeof(my_real_gpu),  cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_gap_m,    gap_m,    h_nrtm   *sizeof(my_real_gpu),  cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_curv_max, curv_max, h_nrtm   *sizeof(my_real_gpu),  cudaMemcpyHostToDevice));
    if (*s_kremnod > 0)
        CUDA_CHECK(cudaMemcpy(d_kremnod, kremnod, (*s_kremnod)*sizeof(int), cudaMemcpyHostToDevice));
    if (*s_remnod > 0)
        CUDA_CHECK(cudaMemcpy(d_remnod,  remnod,  (*s_remnod) *sizeof(int), cudaMemcpyHostToDevice));

    /* ---- Step 1: slave point AABBs ---- */
    ensure_device_aabb(&d_slave_aabbs,  &g_sz_slave_aabbs, h_nsn);
    ensure_device_int (&d_slave_active, &g_sz_slave_act,   h_nsn);
    {
        int threads = 256, blocks = (h_nsn + threads-1)/threads;
        kernel_build_slave_aabbs<<<blocks, threads>>>(
            h_nsn, d_nsv, d_x, d_stfn, d_slave_aabbs, d_slave_active);
        CUDA_CHECK(cudaGetLastError());
    }

    /* ---- Step 2: segment inflated AABBs ---- */
    ensure_device_aabb(&d_segment_aabbs, &g_sz_segment_aabbs, h_nrtm);
    ensure_device_int (&d_seg_active,    &g_sz_seg_act,       h_nrtm);
    {
        int threads = 256, blocks = (h_nrtm + threads-1)/threads;
        kernel_build_segment_aabbs<<<blocks, threads>>>(
            h_nrtm, d_irect, d_x, d_stf, d_gap_m, d_curv_max,
            *marge, *tzinf, *gapmin, *gapmax, *bgapsmx, *dgapload, *drad, *igap,
            d_segment_aabbs, d_seg_active);
        CUDA_CHECK(cudaGetLastError());
    }
    CUDA_CHECK(cudaDeviceSynchronize());

    /* ---- Download AABB data to host for PhysX submission ---- */
    ensure_host_raw((void**)&h_slave_aabbs,   &h_sz_sa_bytes,    h_nsn  *sizeof(AabbEntry));
    ensure_host_raw((void**)&h_segment_aabbs, &h_sz_seg_bytes,   h_nrtm *sizeof(AabbEntry));
    ensure_host_raw((void**)&h_slave_active,  &h_sz_sact_bytes,  h_nsn  *sizeof(int));
    ensure_host_raw((void**)&h_seg_active,    &h_sz_seact_bytes, h_nrtm *sizeof(int));

    CUDA_CHECK(cudaMemcpy(h_slave_aabbs,   d_slave_aabbs,   h_nsn *sizeof(AabbEntry), cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(h_segment_aabbs, d_segment_aabbs, h_nrtm*sizeof(AabbEntry), cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(h_slave_active,  d_slave_active,  h_nsn *sizeof(int),       cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(h_seg_active,    d_seg_active,    h_nrtm*sizeof(int),       cudaMemcpyDeviceToHost));

    /* ---- Step 3: submit to PhysX GPU broad phase ----
     *
     * PxBroadPhaseUpdateData uses HANDLE-INDEXED arrays:
     *   mBounds[handle] and mGroups[handle] are accessed for each handle
     *   listed in mCreated[].
     *
     * Handle assignment:
     *   slave i   → handle = i          (0 .. nsn-1)
     *   segment ne → handle = nsn + ne  (nsn .. nsn+nrtm-1)
     *
     * Capacity = nsn + nrtm (one past the last handle).
     */
    int total_objs = h_nsn + h_nrtm;

    /* Handle-indexed arrays, capacity = total_objs */
    ensure_host_raw((void**)&h_bounds,  &h_sz_bnd_bytes, total_objs * (int)sizeof(AabbEntry));
    ensure_host_raw((void**)&h_groups,  &h_sz_grp_bytes, total_objs * (int)sizeof(PxU32));
    ensure_host_raw((void**)&h_created, &h_sz_cr_bytes,  total_objs * (int)sizeof(PxU32));

    PxU32 n_created = 0;

    for (int i = 0; i < h_nsn; i++) {
        if (!h_slave_active[i]) continue;
        PxU32 handle = (PxU32)i;
        h_bounds[handle] = h_slave_aabbs[i];
        h_groups[handle] = FILTER_SLAVE;
        h_created[n_created++] = handle;
    }
    for (int ne = 0; ne < h_nrtm; ne++) {
        if (!h_seg_active[ne]) continue;
        PxU32 handle = (PxU32)(h_nsn + ne);
        h_bounds[handle] = h_segment_aabbs[ne];
        h_groups[handle] = FILTER_MASTER;
        h_created[n_created++] = handle;
    }

    if (n_created == 0) { *ii_stok = 0; return; }

    /* PhysX 5.x PxBroadPhaseUpdateData constructor:
     *   (created, nbCreated, updated, nbUpdated, removed, nbRemoved,
     *    bounds, groups, distances=NULL, capacity=0)
     * mBounds is cast from AabbEntry* because AabbEntry == PxBounds3 layout. */
    PxBroadPhaseUpdateData addData(
        h_created, n_created,
        nullptr,   0,
        nullptr,   0,
        reinterpret_cast<const PxBounds3*>(h_bounds),
        h_groups,
        nullptr,
        (PxU32)total_objs
    );
    g_broadPhase->update(addData);

    PxBroadPhaseResults results;
    g_broadPhase->fetchResults(results);

    PxU32 npairs = results.mNbCreatedPairs;
    if (npairs == 0) {
        /* Remove objects so handles are free for the next call */
        goto remove_and_return_zero;
    }

    /* ---- Unpack PhysX pairs ---- */
    ensure_host_raw((void**)&h_pair_slave, &h_sz_ps_bytes, (int)npairs * (int)sizeof(int));
    ensure_host_raw((void**)&h_pair_seg,   &h_sz_pe_bytes, (int)npairs * (int)sizeof(int));

    {
        PxU32 valid = 0;
        for (PxU32 k = 0; k < npairs; k++) {
            PxU32 h0 = results.mCreatedPairs[k].mID0;
            PxU32 h1 = results.mCreatedPairs[k].mID1;
            /* Identify slave vs segment by handle range */
            PxU32 slave_handle, seg_handle;
            if (h0 < (PxU32)h_nsn) { slave_handle = h0; seg_handle = h1; }
            else                    { slave_handle = h1; seg_handle = h0; }
            if (seg_handle < (PxU32)h_nsn ||
                seg_handle >= (PxU32)(h_nsn + h_nrtm)) continue;
            h_pair_slave[valid] = (int)slave_handle;
            h_pair_seg[valid]   = (int)(seg_handle - h_nsn);
            valid++;
        }

        if (valid == 0) goto remove_and_return_zero;

        ensure_device_int(&d_pair_slave, &g_sz_pairs_d, (int)valid);
        ensure_device_int(&d_pair_seg,   &g_sz_pairs_d, (int)valid);
        CUDA_CHECK(cudaMemcpy(d_pair_slave, h_pair_slave, valid*sizeof(int), cudaMemcpyHostToDevice));
        CUDA_CHECK(cudaMemcpy(d_pair_seg,   h_pair_seg,   valid*sizeof(int), cudaMemcpyHostToDevice));
        CUDA_CHECK(cudaMemset(d_counter, 0, sizeof(int)));

        /* ---- Step 4: post-filter on GPU ---- */
        {
            int threads = 256, blocks = ((int)valid + threads - 1) / threads;
            kernel_postfilter_pairs<<<blocks, threads>>>(
                (int)valid,
                d_pair_slave, d_pair_seg,
                h_nrtm, d_irect, d_x, d_nsv,
                *igap, *marge, *gapmin, *gapmax, *dgapload, *drad,
                d_gap_s, d_gap_m, d_curv_max,
                *flagremnode, d_kremnod, d_remnod,
                d_cand_n, d_cand_e, d_counter, h_mulnsn);
            CUDA_CHECK(cudaGetLastError());
            CUDA_CHECK(cudaDeviceSynchronize());
        }

        int h_count = 0;
        CUDA_CHECK(cudaMemcpy(&h_count, d_counter, sizeof(int), cudaMemcpyDeviceToHost));
        h_count = min(h_count, h_mulnsn);
        if (h_count > 0) {
            CUDA_CHECK(cudaMemcpy(cand_n, d_cand_n, h_count*sizeof(int), cudaMemcpyDeviceToHost));
            CUDA_CHECK(cudaMemcpy(cand_e, d_cand_e, h_count*sizeof(int), cudaMemcpyDeviceToHost));
        }
        *ii_stok = h_count;
    }

    /* ---- Remove all objects so handles are free for the next call ---- */
    {
        ensure_host_raw((void**)&h_removed, &h_sz_rm_bytes, (int)n_created * (int)sizeof(PxU32));
        memcpy(h_removed, h_created, n_created * sizeof(PxU32));
        PxBroadPhaseUpdateData removeData(
            nullptr,   0,
            nullptr,   0,
            h_removed, n_created,
            reinterpret_cast<const PxBounds3*>(h_bounds),
            h_groups,
            nullptr,
            (PxU32)total_objs
        );
        g_broadPhase->update(removeData);
        PxBroadPhaseResults dummy;
        g_broadPhase->fetchResults(dummy);
    }
    return;

remove_and_return_zero:
    *ii_stok = 0;
    {
        ensure_host_raw((void**)&h_removed, &h_sz_rm_bytes, (int)n_created * (int)sizeof(PxU32));
        memcpy(h_removed, h_created, n_created * sizeof(PxU32));
        PxBroadPhaseUpdateData removeData(
            nullptr,   0,
            nullptr,   0,
            h_removed, n_created,
            reinterpret_cast<const PxBounds3*>(h_bounds),
            h_groups,
            nullptr,
            (PxU32)total_objs
        );
        g_broadPhase->update(removeData);
        PxBroadPhaseResults dummy;
        g_broadPhase->fetchResults(dummy);
    }
}

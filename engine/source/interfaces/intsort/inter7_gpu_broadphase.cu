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
 * GPU broad-phase collision detection using PhysX 5.x PxAABBManager (eGPU).
 *
 * Are our primitives "deformable" in the PhysX sense?
 * ====================================================
 * No.  "Deformable" in PhysX refers to PhysX-managed FEM soft bodies, cloth,
 * or fluid where PhysX owns the vertex positions and updates them internally.
 * Our slave nodes and master segments are USER-MANAGED AABBs: we compute the
 * bounding boxes ourselves each timestep and hand them to PhysX.  That is
 * exactly the purpose of PxAABBManager – it manages a collection of
 * user-provided AABBs and finds overlaps via the GPU Sort-and-Prune broad
 * phase.  Nodes whose positions change every FEM timestep are updated via
 * addObject (in our remove-then-re-add scheme, detailed below).
 *
 * Why PxAABBManager instead of PxBroadPhase directly?
 * ====================================================
 * PxAABBManager provides a clean object-level API:
 *   addObject(bounds, filterGroup) → handle
 *   removeObject(handle)
 *   update()            ← triggers GPU SAP internally; no data argument
 *   fetchResults(out)   ← overlap pairs
 *
 * PxBroadPhase::update() requires a fully assembled PxBroadPhaseUpdateData
 * with handle-indexed arrays built on the caller side.  PxAABBManager hides
 * that bookkeeping.
 *
 * Architecture
 * ============
 *
 * Step 1  build_slave_aabbs  (CUDA kernel, N threads for N slave nodes)
 *   Point AABB [x,x]×[y,y]×[z,z] for each active slave node.
 *   filterGroup = FILTER_SLAVE (0x1)
 *
 * Step 2  build_segment_aabbs  (CUDA kernel, N threads for N master segments)
 *   Inflated AABB for each active segment; inflation uses bgapsmx
 *   (overestimation of gap_s) so the box is conservative (no false negatives).
 *   filterGroup = FILTER_MASTER (0x2)
 *
 * Step 3  PxAABBManager add + update  (CPU loop → GPU SAP inside PhysX)
 *   AABBs are downloaded to host and submitted via addObject().
 *   update() triggers the GPU Sort-and-Prune.  Only slave–master pairs are
 *   produced because of the filter groups.
 *
 * Step 4  postfilter_pairs  (CUDA kernel, one thread per PhysX pair)
 *   (a) Corner self-pair                                           → reject
 *   (b) KREMNODE forbidden pair (linear scan, no sort required)    → reject
 *   (c) Per-node AABB with actual gap_s (not the conservative bgapsmx) → reject
 *   (d) Plane-distance underestimation (same formula as CPU)       → reject
 *   Valid pairs written via atomicAdd.
 *
 * Filter-group semantics (PhysX)
 * ================================
 * Pair (A,B) suppressed iff (A.group & B.group) != 0.
 *   FILTER_SLAVE  = 0x1:  (0x1 & 0x1)=1 → slave-slave suppressed  ✓
 *   FILTER_MASTER = 0x2:  (0x2 & 0x2)=2 → master-master suppressed ✓
 *   slave-master:         (0x1 & 0x2)=0 → pair generated           ✓
 *
 * Remove-then-re-add invariant
 * ============================
 * To get ALL currently overlapping pairs (not just newly started overlaps),
 * we rebuild the manager's object set every timestep:
 *
 *   [start-of-call invariant: manager is empty]
 *   addObject() for each active slave / segment
 *   update()         ← GPU SAP; all current overlaps appear as "created"
 *   fetchResults()
 *   removeObject() for each added handle
 *   update() + fetchResults() ← flush pending removes; manager empty again
 *   [end-of-call invariant: manager is empty]
 *
 * An incremental updateObject() approach would miss pre-existing contacts
 * (they would only appear in mCreatedPairs the first timestep they occur).
 *
 * Handle decode table
 * ===================
 * addObject() returns opaque PxU32 handles.  We maintain a flat host array
 * h_decode[] indexed by handle value:
 *   h_decode[h] > 0  →  slave  with local index (h_decode[h] - 1)
 *   h_decode[h] < 0  →  segment with local index (-h_decode[h] - 1)
 * With a fresh (empty) manager and no fragmentation, handles are dense
 * in [0, n_active).  The table is sized to NSN+NRTM and handles beyond
 * that range are silently skipped.
 *
 * SPMD / MPI limitation
 * =====================
 * Only local slave nodes (indices 1..nsn in NSV) are processed.
 * Remote slave nodes from other MPI ranks (nsnr > 0) are not submitted.
 * The caller must use the CPU path to cover remote-node contacts.
 */

#include "inter7_gpu_broadphase.h"

#include <PxPhysicsAPI.h>
#include <cuda_runtime.h>
#include <algorithm>
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
/* Each pointer must have its own size tracker – sharing trackers      */
/* causes the second ensure_* call to skip allocation (n <= *sz).      */
/* ------------------------------------------------------------------ */
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
static void ensure_host_raw(void** p, int* sz_bytes, int nbytes)
{
    if (nbytes > *sz_bytes) {
        free(*p);
        *p = malloc((size_t)nbytes);
        *sz_bytes = nbytes;
    }
}

/* AabbEntry: same memory layout as PxBounds3 */
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
static PxBroadPhase*            g_broadPhase = nullptr;
static PxAABBManager*           g_aabbMgr    = nullptr;  /* sits on top of g_broadPhase */
static bool                     g_initialised = false;

/* Device buffers (persistent, grown on demand, one size tracker each) */
static my_real_gpu* d_x        = nullptr;  static int g_sz_x        = 0;
static int*         d_nsv      = nullptr;  static int g_sz_nsv      = 0;
static my_real_gpu* d_stfn     = nullptr;  static int g_sz_stfn     = 0;
static my_real_gpu* d_gap_s    = nullptr;  static int g_sz_gap_s    = 0;
static int*         d_irect    = nullptr;  static int g_sz_irect    = 0;
static my_real_gpu* d_stf      = nullptr;  static int g_sz_stf      = 0;
static my_real_gpu* d_gap_m    = nullptr;  static int g_sz_gap_m    = 0;
static my_real_gpu* d_curv_max = nullptr;  static int g_sz_curv_max = 0;
static int*         d_kremnod  = nullptr;  static int g_sz_kremnod  = 0;
static int*         d_remnod   = nullptr;  static int g_sz_remnod   = 0;
static int*         d_cand_n   = nullptr;  static int g_sz_cand_n   = 0;
static int*         d_cand_e   = nullptr;  static int g_sz_cand_e   = 0;
static int*         d_counter  = nullptr;

/* GPU AABB arrays */
static AabbEntry*   d_slave_aabbs   = nullptr;  static int g_sz_slave_aabbs = 0;
static AabbEntry*   d_segment_aabbs = nullptr;  static int g_sz_seg_aabbs   = 0;
static int*         d_slave_active  = nullptr;  static int g_sz_slave_act   = 0;
static int*         d_seg_active    = nullptr;  static int g_sz_seg_act     = 0;

/* Post-PhysX pair arrays (device) */
static int*         d_pair_slave    = nullptr;  static int g_sz_pair_slave  = 0;
static int*         d_pair_seg      = nullptr;  static int g_sz_pair_seg    = 0;

/* Host arrays downloaded from GPU */
static AabbEntry*   h_slave_aabbs   = nullptr;  static int h_sz_sa_bytes    = 0;
static AabbEntry*   h_segment_aabbs = nullptr;  static int h_sz_seg_bytes   = 0;
static int*         h_slave_active  = nullptr;  static int h_sz_sact_bytes  = 0;
static int*         h_seg_active    = nullptr;  static int h_sz_seact_bytes = 0;

/*
 * PxAABBManager handle tracking.
 * h_added_handles[] stores the handles returned by addObject() this frame.
 * h_decode[handle]  maps each handle to its primitive:
 *   > 0  →  slave  index (h_decode[h] - 1)
 *   < 0  →  segment index (-h_decode[h] - 1)
 *   = 0  →  unused slot
 * The table is cleared by zeroing used slots before each remove pass.
 */
static PxU32*  h_added_handles = nullptr;  static int h_sz_ah_bytes  = 0;
static int*    h_decode        = nullptr;  static int h_sz_dec_bytes = 0;
static int     h_n_added       = 0;        /* number of objects added this frame */

/* Host pair decode buffers */
static int*    h_pair_slave    = nullptr;  static int h_sz_ps_bytes  = 0;
static int*    h_pair_seg      = nullptr;  static int h_sz_pe_bytes  = 0;

/* ================================================================== */
/* Step 1: build slave-node point AABBs                               */
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
/* Step 2: build master-segment inflated AABBs                        */
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

    /* IRECT(4,NRTM) column-major: IRECT(k,ne+1) = irect[ne*4+k] (0-based k) */
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
/* Step 4: post-filter PhysX pairs on the GPU                         */
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
    const int* __restrict__      kremnod,
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
    int nn = nsv[slave_idx];

    /* (a) Corner self-pair */
    int m0 = irect[ne*4 + 0], m1 = irect[ne*4 + 1];
    int m2 = irect[ne*4 + 2], m3 = irect[ne*4 + 3];
    if (nn == m0 || nn == m1 || nn == m2 || nn == m3) return;

    /* (b) KREMNODE linear scan */
    if (flagremnode == 2) {
        int lo = kremnod[2*ne], hi = kremnod[2*ne + 1];
        for (int k = lo; k < hi; k++) if (remnod[k] == nn) return;
    }

    int jg = nn - 1;
    my_real_gpu xs=x[3*jg], ys=x[3*jg+1], zs=x[3*jg+2];

    int jm0=m0-1, jm1=m1-1, jm2=m2-1, jm3=m3-1;
    my_real_gpu xx0=x[3*jm0],xx1=x[3*jm1],xx2=x[3*jm2],xx3=x[3*jm3];
    my_real_gpu yy0=x[3*jm0+1],yy1=x[3*jm1+1],yy2=x[3*jm2+1],yy3=x[3*jm3+1];
    my_real_gpu zz0=x[3*jm0+2],zz1=x[3*jm1+2],zz2=x[3*jm2+2],zz3=x[3*jm3+2];

    my_real_gpu xmine=min(min(xx0,xx1),min(xx2,xx3));
    my_real_gpu xmaxe=max(max(xx0,xx1),max(xx2,xx3));
    my_real_gpu ymine=min(min(yy0,yy1),min(yy2,yy3));
    my_real_gpu ymaxe=max(max(yy0,yy1),max(yy2,yy3));
    my_real_gpu zmine=min(min(zz0,zz1),min(zz2,zz3));
    my_real_gpu zmaxe=max(max(zz0,zz1),max(zz2,zz3));

    /* (c) Per-node AABB with actual gap_s */
    my_real_gpu aaa = (my_real_gpu)0;
    if (igap != 0) {
        my_real_gpu g = min(gapmax, max(gapmin, gap_s[slave_idx]+gap_m[ne])) + dgapload;
        aaa = marge + curv_max[ne] + max(g, drad);
        if (xs<=xmine-aaa || xs>=xmaxe+aaa ||
            ys<=ymine-aaa || ys>=ymaxe+aaa ||
            zs<=zmine-aaa || zs>=zmaxe+aaa) return;
    }

    /* (d) Plane-distance underestimation */
    my_real_gpu sx=(yy2-yy0)*(zz3-zz1)-(zz2-zz0)*(yy3-yy1);
    my_real_gpu sy=(zz2-zz0)*(xx3-xx1)-(xx2-xx0)*(zz3-zz1);
    my_real_gpu sz=(xx2-xx0)*(yy3-yy1)-(yy2-yy0)*(xx3-xx1);
    my_real_gpu s2=sx*sx+sy*sy+sz*sz;
    my_real_gpu dd1=(xs-xx0)*sx+(ys-yy0)*sy+(zs-zz0)*sz;
    my_real_gpu dd2=(xs-xx1)*sx+(ys-yy1)*sy+(zs-zz1)*sz;
    if (dd1*dd2 > (my_real_gpu)0) {
        my_real_gpu d2 = min(dd1*dd1, dd2*dd2);
        if (d2 > aaa*aaa*s2) return;
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
        if (g_cudaCtxMgr) { g_cudaCtxMgr->release(); g_cudaCtxMgr = nullptr; }
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

    g_aabbMgr = PxCreateAABBManager(*g_broadPhase);
    if (!g_aabbMgr) {
        fprintf(stderr, "[GPU BPH] PxCreateAABBManager failed.\n");
        g_broadPhase->release(); g_broadPhase = nullptr;
        g_cudaCtxMgr->release(); g_cudaCtxMgr = nullptr;
        g_foundation->release(); g_foundation = nullptr;
        return 1;
    }

    CUDA_CHECK(cudaMalloc(&d_counter, sizeof(int)));
    g_initialised = true;
    fprintf(stderr, "[GPU BPH] PhysX eGPU broad phase ready (PxAABBManager).\n");
    return 0;
}

void inter7_gpu_broadphase_destroy_(void)
{
    if (!g_initialised) return;

    if (g_aabbMgr)   { g_aabbMgr->release();   g_aabbMgr   = nullptr; }
    if (g_broadPhase){ g_broadPhase->release(); g_broadPhase= nullptr; }
    if (g_cudaCtxMgr){ g_cudaCtxMgr->release(); g_cudaCtxMgr= nullptr; }
    if (g_foundation) { g_foundation->release(); g_foundation= nullptr; }

    auto cfree = [](void* p){ if(p) cudaFree(p); };
    cfree(d_x);      cfree(d_nsv);      cfree(d_stfn);     cfree(d_gap_s);
    cfree(d_irect);  cfree(d_stf);      cfree(d_gap_m);    cfree(d_curv_max);
    cfree(d_kremnod);cfree(d_remnod);   cfree(d_cand_n);   cfree(d_cand_e);
    cfree(d_counter);
    cfree(d_slave_aabbs); cfree(d_segment_aabbs);
    cfree(d_slave_active); cfree(d_seg_active);
    cfree(d_pair_slave);   cfree(d_pair_seg);

    free(h_slave_aabbs);   free(h_segment_aabbs);
    free(h_slave_active);  free(h_seg_active);
    free(h_added_handles); free(h_decode);
    free(h_pair_slave);    free(h_pair_seg);

    g_initialised = false;
}

int inter7_gpu_available_(void) { return g_initialised ? 1 : 0; }

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
    const my_real_gpu* stfn,
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
    ensure_device_real(&d_x,        &g_sz_x,        3 * h_numnod);
    ensure_device_int (&d_nsv,      &g_sz_nsv,      h_nsn);
    ensure_device_real(&d_stfn,     &g_sz_stfn,     h_nsn);
    ensure_device_real(&d_gap_s,    &g_sz_gap_s,    h_nsn);
    ensure_device_int (&d_irect,    &g_sz_irect,    4 * h_nrtm);
    ensure_device_real(&d_stf,      &g_sz_stf,      h_nrtm);
    ensure_device_real(&d_gap_m,    &g_sz_gap_m,    h_nrtm);
    ensure_device_real(&d_curv_max, &g_sz_curv_max, h_nrtm);
    ensure_device_int (&d_kremnod,  &g_sz_kremnod,  (*s_kremnod > 0 ? *s_kremnod : 1));
    ensure_device_int (&d_remnod,   &g_sz_remnod,   (*s_remnod  > 0 ? *s_remnod  : 1));
    ensure_device_int (&d_cand_n,   &g_sz_cand_n,   h_mulnsn);
    ensure_device_int (&d_cand_e,   &g_sz_cand_e,   h_mulnsn);

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

    /* ---- Step 1 & 2: build AABBs on GPU ---- */
    ensure_device_aabb(&d_slave_aabbs,  &g_sz_slave_aabbs, h_nsn);
    ensure_device_int (&d_slave_active, &g_sz_slave_act,   h_nsn);
    {
        int threads = 256, blocks = (h_nsn + threads-1)/threads;
        kernel_build_slave_aabbs<<<blocks, threads>>>(
            h_nsn, d_nsv, d_x, d_stfn, d_slave_aabbs, d_slave_active);
        CUDA_CHECK(cudaGetLastError());
    }

    ensure_device_aabb(&d_segment_aabbs, &g_sz_seg_aabbs, h_nrtm);
    ensure_device_int (&d_seg_active,    &g_sz_seg_act,   h_nrtm);
    {
        int threads = 256, blocks = (h_nrtm + threads-1)/threads;
        kernel_build_segment_aabbs<<<blocks, threads>>>(
            h_nrtm, d_irect, d_x, d_stf, d_gap_m, d_curv_max,
            *marge, *tzinf, *gapmin, *gapmax, *bgapsmx, *dgapload, *drad, *igap,
            d_segment_aabbs, d_seg_active);
        CUDA_CHECK(cudaGetLastError());
    }
    CUDA_CHECK(cudaDeviceSynchronize());

    /* ---- Download to host ---- */
    ensure_host_raw((void**)&h_slave_aabbs,   &h_sz_sa_bytes,    h_nsn  *sizeof(AabbEntry));
    ensure_host_raw((void**)&h_segment_aabbs, &h_sz_seg_bytes,   h_nrtm *sizeof(AabbEntry));
    ensure_host_raw((void**)&h_slave_active,  &h_sz_sact_bytes,  h_nsn  *sizeof(int));
    ensure_host_raw((void**)&h_seg_active,    &h_sz_seact_bytes, h_nrtm *sizeof(int));
    CUDA_CHECK(cudaMemcpy(h_slave_aabbs,   d_slave_aabbs,   h_nsn *sizeof(AabbEntry), cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(h_segment_aabbs, d_segment_aabbs, h_nrtm*sizeof(AabbEntry), cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(h_slave_active,  d_slave_active,  h_nsn *sizeof(int),       cudaMemcpyDeviceToHost));
    CUDA_CHECK(cudaMemcpy(h_seg_active,    d_seg_active,    h_nrtm*sizeof(int),       cudaMemcpyDeviceToHost));

    /* ---- Step 3: submit objects to PxAABBManager ----
     *
     * Invariant on entry: manager is empty (all objects were removed at the
     * end of the previous call and flushed by the cleanup update() there).
     *
     * We register every active object via addObject(bounds, filterGroup).
     * The returned handle is stored in h_added_handles[] and the decode
     * table h_decode[handle] is populated for pair resolution.
     *
     * Decode encoding:
     *   h_decode[h] = (slave_local_idx + 1)   for slaves
     *   h_decode[h] = -(seg_local_idx + 1)    for segments
     *   h_decode[h] = 0                        unused
     *
     * With a fresh, empty manager the internal handle pool starts at 0, so
     * all handles will be in [0, total_active).  We size the table to
     * nsn + nrtm as an upper bound and skip any pair whose handles fall
     * outside that range (which would indicate an unexpected internal state).
     */
    int total_objs = h_nsn + h_nrtm;
    ensure_host_raw((void**)&h_added_handles, &h_sz_ah_bytes,  total_objs * (int)sizeof(PxU32));
    ensure_host_raw((void**)&h_decode,        &h_sz_dec_bytes, total_objs * (int)sizeof(int));
    memset(h_decode, 0, (size_t)total_objs * sizeof(int));
    h_n_added = 0;

    for (int i = 0; i < h_nsn; i++) {
        if (!h_slave_active[i]) continue;
        PxBounds3 b(
            PxVec3(h_slave_aabbs[i].lo[0], h_slave_aabbs[i].lo[1], h_slave_aabbs[i].lo[2]),
            PxVec3(h_slave_aabbs[i].hi[0], h_slave_aabbs[i].hi[1], h_slave_aabbs[i].hi[2]));
        PxU32 h = g_aabbMgr->addObject(b, FILTER_SLAVE);
        h_added_handles[h_n_added++] = h;
        if ((int)h < total_objs) h_decode[h] = i + 1;   /* positive → slave */
    }
    for (int ne = 0; ne < h_nrtm; ne++) {
        if (!h_seg_active[ne]) continue;
        PxBounds3 b(
            PxVec3(h_segment_aabbs[ne].lo[0], h_segment_aabbs[ne].lo[1], h_segment_aabbs[ne].lo[2]),
            PxVec3(h_segment_aabbs[ne].hi[0], h_segment_aabbs[ne].hi[1], h_segment_aabbs[ne].hi[2]));
        PxU32 h = g_aabbMgr->addObject(b, FILTER_MASTER);
        h_added_handles[h_n_added++] = h;
        if ((int)h < total_objs) h_decode[h] = -(ne + 1);  /* negative → segment */
    }

    if (h_n_added == 0) { *ii_stok = 0; return; }

    /* ---- Run GPU Sort-and-Prune ---- */
    g_aabbMgr->update();          /* triggers the GPU SAP internally */
    PxBroadPhaseResults results;
    g_aabbMgr->fetchResults(results);

    PxU32 npairs = results.mNbCreatedPairs;

    /* ---- Decode pairs on CPU ---- */
    PxU32 valid = 0;
    if (npairs > 0) {
        ensure_host_raw((void**)&h_pair_slave, &h_sz_ps_bytes, (int)npairs*(int)sizeof(int));
        ensure_host_raw((void**)&h_pair_seg,   &h_sz_pe_bytes, (int)npairs*(int)sizeof(int));

        for (PxU32 k = 0; k < npairs; k++) {
            PxU32 ha = results.mCreatedPairs[k].mID0;
            PxU32 hb = results.mCreatedPairs[k].mID1;
            if ((int)ha >= total_objs || (int)hb >= total_objs) continue;
            int da = h_decode[ha], db = h_decode[hb];
            int slave_idx, seg_idx;
            if      (da > 0 && db < 0) { slave_idx = da-1; seg_idx = -db-1; }
            else if (da < 0 && db > 0) { slave_idx = db-1; seg_idx = -da-1; }
            else continue;
            h_pair_slave[valid] = slave_idx;
            h_pair_seg[valid]   = seg_idx;
            valid++;
        }
    }

    /* ---- Remove all objects; flush via update so manager is empty again ---- */
    for (int k = 0; k < h_n_added; k++)
        g_aabbMgr->removeObject(h_added_handles[k]);
    g_aabbMgr->update();
    PxBroadPhaseResults dummy; g_aabbMgr->fetchResults(dummy);

    if (valid == 0) { *ii_stok = 0; return; }

    /* ---- Step 4: post-filter pairs on GPU ---- */
    ensure_device_int(&d_pair_slave, &g_sz_pair_slave, (int)valid);
    ensure_device_int(&d_pair_seg,   &g_sz_pair_seg,   (int)valid);
    CUDA_CHECK(cudaMemcpy(d_pair_slave, h_pair_slave, valid*sizeof(int), cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_pair_seg,   h_pair_seg,   valid*sizeof(int), cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemset(d_counter, 0, sizeof(int)));

    {
        int threads = 256, blocks = ((int)valid + threads - 1) / threads;
        kernel_postfilter_pairs<<<blocks, threads>>>(
            (int)valid, d_pair_slave, d_pair_seg,
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
    h_count = std::min(h_count, h_mulnsn);
    if (h_count > 0) {
        CUDA_CHECK(cudaMemcpy(cand_n, d_cand_n, h_count*sizeof(int), cudaMemcpyDeviceToHost));
        CUDA_CHECK(cudaMemcpy(cand_e, d_cand_e, h_count*sizeof(int), cudaMemcpyDeviceToHost));
    }
    *ii_stok = h_count;
}

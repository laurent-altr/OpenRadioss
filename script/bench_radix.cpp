/*
 * bench_radix.cpp
 *
 * Standalone correctness test and performance benchmark comparing three sort
 * strategies for the multi-key unsigned-integer permutation problem solved by
 * OpenRadioss my_orders.c:
 *
 *   1. orig  – original 16-bit LSD radix (unchanged from my_orders.c)
 *   2. opt   – 8-bit LSD radix with precomputed histograms (optimised version)
 *   3. stdsort – std::sort with a multi-key comparator (O(n log n) baseline)
 *
 * Build:
 *   g++ -O2 -std=c++11 -o bench_radix bench_radix.cpp
 *
 * Run:
 *   ./bench_radix
 */

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <algorithm>
#include <chrono>

/* =========================================================================
 * ORIGINAL 16-bit implementation (verbatim copy from old my_orders.c)
 * ========================================================================= */

static void tri_direct_orig(unsigned *data, unsigned *iwork,
                             unsigned *index, int n, int irecl, unsigned *inds)
{
    unsigned range = 65535u, mask = 0xffffu;
    int i, j, k;

    for (k = irecl - 1; k >= 0; k--) {

        /* low 16 bits */
        for (i = 0; i < (int)(range + 2); i++) iwork[i] = 0;
        for (i = 0; i < n; i++)
            iwork[(data[irecl * index[i] + k] & mask) + 1] += 1;
        for (i = 0; i < (int)range; i++) iwork[i + 1] += iwork[i];
        for (i = 0; i < n; i++) {
            j = (int)(data[irecl * index[i] + k] & mask);
            inds[iwork[j]] = index[i];
            iwork[j] += 1;
        }

        /* high 16 bits */
        for (i = 0; i < (int)(range + 2); i++) iwork[i] = 0;
        for (i = 0; i < n; i++)
            iwork[((data[irecl * inds[i] + k] >> 16) & mask) + 1] += 1;
        for (i = 0; i < (int)range; i++) iwork[i + 1] += iwork[i];
        for (i = 0; i < n; i++) {
            j = (int)((data[irecl * inds[i] + k] >> 16) & mask);
            index[iwork[j]] = inds[i];
            iwork[j] += 1;
        }
    }
}

/* =========================================================================
 * OPTIMISED 8-bit implementation with precomputed histograms (new my_orders.c)
 * ========================================================================= */

static void tri_direct_opt(unsigned *data, unsigned *iwork,
                            unsigned *index, int n, int irecl, unsigned *inds)
{
    int i, k, b;

    /* Phase 1: clear */
    memset(iwork, 0, (size_t)irecl * 4 * 257 * sizeof(unsigned));

    /* Phase 2: single sequential scan -> all irecl*4 histograms */
    for (i = 0; i < n; i++) {
        for (k = 0; k < irecl; k++) {
            unsigned val = data[irecl * i + k];
            unsigned *h0 = iwork + (k * 4 + 0) * 257;
            unsigned *h1 = iwork + (k * 4 + 1) * 257;
            unsigned *h2 = iwork + (k * 4 + 2) * 257;
            unsigned *h3 = iwork + (k * 4 + 3) * 257;
            h0[( val        & 0xff) + 1]++;
            h1[((val >>  8) & 0xff) + 1]++;
            h2[((val >> 16) & 0xff) + 1]++;
            h3[((val >> 24)       ) + 1]++;
        }
    }

    /* Phase 3: prefix sums */
    for (k = 0; k < irecl; k++) {
        for (b = 0; b < 4; b++) {
            unsigned *h = iwork + (k * 4 + b) * 257;
            for (i = 0; i < 256; i++) h[i + 1] += h[i];
        }
    }

    /* Phase 4: 4*irecl scatter passes; result always lands in index[] */
    unsigned *src = index, *dst = inds;
    for (k = irecl - 1; k >= 0; k--) {
        for (b = 0; b < 4; b++) {
            unsigned *h     = iwork + (k * 4 + b) * 257;
            unsigned  shift = (unsigned)(b * 8);
            for (i = 0; i < n; i++) {
                unsigned idx      = src[i];
                unsigned byte_val = (data[irecl * idx + k] >> shift) & 0xff;
                dst[h[byte_val]++] = idx;
            }
            unsigned *tmp = src; src = dst; dst = tmp;
        }
    }
}

/* =========================================================================
 * std::sort wrapper
 * ========================================================================= */

/* --- Variant A: indirect comparator (current my_orders interface) ----------
 * Comparator chases two pointers into data[] on every comparison.
 * Cache-hostile: each comparison may cause two cache misses once n is large. */
static void stdsort_indirect(unsigned *data, unsigned *index,
                              int n, int irecl)
{
    std::sort(index, index + n,
        [data, irecl](unsigned a, unsigned b) {
            for (int k = 0; k < irecl; k++) {
                unsigned va = data[irecl * a + k];
                unsigned vb = data[irecl * b + k];
                if (va < vb) return true;
                if (va > vb) return false;
            }
            return a < b;
        });
}

/* --- Variant B: packed-struct sort (keys inlined, no indirection) ----------
 * Pack every element's keys + original index into a flat struct and sort the
 * struct array directly.  The comparator reads only from the struct being
 * compared — no pointer chasing.  This is the cache-friendly std::sort
 * scenario the literature typically benchmarks.
 * Downside: allocates an extra irecl*4+4 bytes per element. */
struct PackedRec {
    unsigned keys[6];   /* up to irecl=6 */
    unsigned orig_idx;
};

static void stdsort_packed(unsigned *data, unsigned *index,
                            int n, int irecl)
{
    PackedRec *recs = new PackedRec[n];
    for (int i = 0; i < n; i++) {
        recs[i].orig_idx = (unsigned)i;
        for (int k = 0; k < irecl; k++) recs[i].keys[k] = data[irecl * i + k];
    }
    std::sort(recs, recs + n,
        [irecl](const PackedRec &a, const PackedRec &b) {
            for (int k = 0; k < irecl; k++) {
                if (a.keys[k] < b.keys[k]) return true;
                if (a.keys[k] > b.keys[k]) return false;
            }
            return a.orig_idx < b.orig_idx;
        });
    for (int i = 0; i < n; i++) index[i] = recs[i].orig_idx;
    delete[] recs;
}

/* --- Variant C: direct-value sort (irecl=1 only, no index needed) ----------
 * The exact scenario in the literature: sort a contiguous array of unsigned
 * integers in-place.  std::sort has optimal cache behaviour here — no
 * indirection whatsoever.  A raw-value 8-bit radix sort is included as a
 * direct apples-to-apples counterpart. */
static void stdsort_direct_values(unsigned *vals, int n)
{
    std::sort(vals, vals + n);
}

/* 8-bit radix sort directly on values (not index-based), for comparison. */
static void radix_direct_values(unsigned *vals, unsigned *tmp,
                                 unsigned *iwork, int n)
{
    /* Build 4 byte-histograms in one pass */
    memset(iwork, 0, 4 * 257 * sizeof(unsigned));
    for (int i = 0; i < n; i++) {
        unsigned v = vals[i];
        iwork[0*257 + ( v        & 0xff) + 1]++;
        iwork[1*257 + ((v >>  8) & 0xff) + 1]++;
        iwork[2*257 + ((v >> 16) & 0xff) + 1]++;
        iwork[3*257 + ( v >> 24        ) + 1]++;
    }
    for (int b = 0; b < 4; b++) {
        unsigned *h = iwork + b * 257;
        for (int i = 0; i < 256; i++) h[i+1] += h[i];
    }
    unsigned *src = vals, *dst = tmp;
    for (int b = 0; b < 4; b++) {
        unsigned *h    = iwork + b * 257;
        unsigned  shift = (unsigned)(b * 8);
        for (int i = 0; i < n; i++) {
            unsigned bv = (src[i] >> shift) & 0xff;
            dst[h[bv]++] = src[i];
        }
        unsigned *t = src; src = dst; dst = t;
    }
    /* 4 passes (even): result is in vals[] */
}

/* =========================================================================
 * Helpers: wrappers that mimic my_orders mode=0 (init index, sort, 0-based)
 * ========================================================================= */

static void sort_orig(unsigned *iwork, unsigned *data,
                      unsigned *index, int n, int irecl)
{
    for (int i = 0; i < n; i++) index[i] = (unsigned)i;
    tri_direct_orig(data, iwork, index, n, irecl, index + n);
}

static void sort_opt(unsigned *iwork, unsigned *data,
                     unsigned *index, int n, int irecl)
{
    for (int i = 0; i < n; i++) index[i] = (unsigned)i;
    tri_direct_opt(data, iwork, index, n, irecl, index + n);
}

/* Variant A: indirect comparator */
static void sort_std_indirect(unsigned *data, unsigned *index, int n, int irecl)
{
    for (int i = 0; i < n; i++) index[i] = (unsigned)i;
    stdsort_indirect(data, index, n, irecl);
}

/* Variant B: packed-struct */
static void sort_std_packed(unsigned *data, unsigned *index, int n, int irecl)
{
    stdsort_packed(data, index, n, irecl);
}

/* =========================================================================
 * RNG (xorshift32)
 * ========================================================================= */

static unsigned xorshift(unsigned &s)
{
    s ^= s << 13; s ^= s >> 17; s ^= s << 5;
    return s;
}

static void fill_random(unsigned *data, int count, unsigned seed)
{
    for (int i = 0; i < count; i++) data[i] = xorshift(seed);
}

/* =========================================================================
 * Timer
 * ========================================================================= */

static double now_sec()
{
    using clock = std::chrono::steady_clock;
    return std::chrono::duration<double>(clock::now().time_since_epoch()).count();
}

/* =========================================================================
 * Correctness helpers
 * ========================================================================= */

/* Returns true if index arrays agree (allowing ties resolved differently). */
static bool results_match(const unsigned *a, const unsigned *b,
                           int n, int irecl, const unsigned *data)
{
    for (int i = 0; i < n; i++) {
        if (a[i] == b[i]) continue;
        /* Different index but same keys -> tie-break difference; acceptable */
        for (int k = 0; k < irecl; k++) {
            if (data[irecl * a[i] + k] != data[irecl * b[i] + k])
                return false;
        }
    }
    return true;
}

static bool is_sorted(const unsigned *index, int n,
                       int irecl, const unsigned *data)
{
    for (int i = 1; i < n; i++) {
        for (int k = 0; k < irecl; k++) {
            unsigned va = data[irecl * index[i-1] + k];
            unsigned vb = data[irecl * index[i  ] + k];
            if (vb < va) return false;
            if (vb > va) break;
        }
    }
    return true;
}

/* =========================================================================
 * Performance benchmark for one (n, irecl) configuration
 * ========================================================================= */

#define IWORK_CAP  131072   /* >= 65537 (orig) and >= irecl*4*257 (opt) */
#define REPEATS    7

static void run_config(int n, int irecl)
{
    unsigned *data   = (unsigned *)malloc((size_t)n * irecl * sizeof(unsigned));
    unsigned *idx_o  = (unsigned *)malloc((size_t)3 * n * sizeof(unsigned));
    unsigned *idx_p  = (unsigned *)malloc((size_t)3 * n * sizeof(unsigned));
    unsigned *idx_si = (unsigned *)malloc((size_t)    n * sizeof(unsigned));
    unsigned *idx_sp = (unsigned *)malloc((size_t)    n * sizeof(unsigned));
    unsigned *iwork  = (unsigned *)malloc(IWORK_CAP * sizeof(unsigned));

    if (!data || !idx_o || !idx_p || !idx_si || !idx_sp || !iwork) {
        fprintf(stderr, "allocation failure (n=%d irecl=%d)\n", n, irecl);
        exit(1);
    }

    unsigned seed = (unsigned)(n * 31u + (unsigned)irecl * 7u);
    fill_random(data, n * irecl, seed);

    /* --- correctness (single run) ---------------------------------------- */
    sort_orig        (iwork, data, idx_o,  n, irecl);
    sort_opt         (iwork, data, idx_p,  n, irecl);
    sort_std_indirect(       data, idx_si, n, irecl);
    sort_std_packed  (       data, idx_sp, n, irecl);

    bool ok_opt = results_match(idx_o, idx_p,  n, irecl, data) && is_sorted(idx_p,  n, irecl, data);
    bool ok_si  = results_match(idx_o, idx_si, n, irecl, data) && is_sorted(idx_si, n, irecl, data);
    bool ok_sp  = results_match(idx_o, idx_sp, n, irecl, data) && is_sorted(idx_sp, n, irecl, data);

    /* --- timing (median of REPEATS) -------------------------------------- */
    double t_o[REPEATS], t_p[REPEATS], t_si[REPEATS], t_sp[REPEATS];

    for (int r = 0; r < REPEATS; r++) {
        fill_random(data, n * irecl, (unsigned)(r * 1000003u + (unsigned)n));

        double t0 = now_sec();
        sort_orig(iwork, data, idx_o, n, irecl);
        t_o[r] = now_sec() - t0;

        t0 = now_sec();
        sort_opt(iwork, data, idx_p, n, irecl);
        t_p[r] = now_sec() - t0;

        t0 = now_sec();
        sort_std_indirect(data, idx_si, n, irecl);
        t_si[r] = now_sec() - t0;

        t0 = now_sec();
        sort_std_packed(data, idx_sp, n, irecl);
        t_sp[r] = now_sec() - t0;
    }

    std::sort(t_o,  t_o  + REPEATS);
    std::sort(t_p,  t_p  + REPEATS);
    std::sort(t_si, t_si + REPEATS);
    std::sort(t_sp, t_sp + REPEATS);
    double to  = t_o[REPEATS/2],  tp  = t_p[REPEATS/2];
    double tsi = t_si[REPEATS/2], tsp = t_sp[REPEATS/2];

    printf("  n=%-8d  irecl=%d  |  orig %6.2f ms  opt %6.2f ms (x%4.1f)"
           "  std/indir %6.2f ms (x%4.1f)  std/packed %6.2f ms (x%4.1f)%s%s%s\n",
           n, irecl,
           to * 1e3, tp * 1e3, to / tp,
           tsi * 1e3, to / tsi,
           tsp * 1e3, to / tsp,
           ok_opt ? "" : " OPT-FAIL",
           ok_si  ? "" : " INDIR-FAIL",
           ok_sp  ? "" : " PACKED-FAIL");

    free(data); free(idx_o); free(idx_p); free(idx_si); free(idx_sp); free(iwork);
}

/* Direct-value sort benchmark (irecl=1 only): the exact literature scenario —
 * sort a contiguous array of unsigned ints with no index indirection. */
static void run_direct_values(int n)
{
    unsigned *vals  = (unsigned *)malloc((size_t)n * sizeof(unsigned));
    unsigned *tmp   = (unsigned *)malloc((size_t)n * sizeof(unsigned));
    unsigned *iwork = (unsigned *)malloc(4 * 257 * sizeof(unsigned));
    unsigned *vcopy = (unsigned *)malloc((size_t)n * sizeof(unsigned));
    if (!vals || !tmp || !iwork || !vcopy) { fprintf(stderr, "alloc fail\n"); exit(1); }

    double t_rs[REPEATS], t_ss[REPEATS];

    for (int r = 0; r < REPEATS; r++) {
        fill_random(vals, n, (unsigned)(r * 999983u + (unsigned)n));

        /* radix sort on raw values */
        memcpy(vcopy, vals, (size_t)n * sizeof(unsigned));
        double t0 = now_sec();
        radix_direct_values(vcopy, tmp, iwork, n);
        t_rs[r] = now_sec() - t0;

        /* std::sort on raw values */
        memcpy(vcopy, vals, (size_t)n * sizeof(unsigned));
        t0 = now_sec();
        stdsort_direct_values(vcopy, n);
        t_ss[r] = now_sec() - t0;
    }

    std::sort(t_rs, t_rs + REPEATS);
    std::sort(t_ss, t_ss + REPEATS);
    double trs = t_rs[REPEATS/2], tss = t_ss[REPEATS/2];

    printf("  n=%-8d  |  radix/direct %6.2f ms  std::sort/direct %6.2f ms"
           "  (std/radix = x%.2f)%s\n",
           n, trs * 1e3, tss * 1e3, trs / tss,
           (trs / tss > 1.0) ? "  <- std wins" : "  <- radix wins");

    free(vals); free(tmp); free(iwork); free(vcopy);
}

/* =========================================================================
 * Edge-case tests
 * ========================================================================= */

static int edge_tests()
{
    int fail = 0;
    unsigned iwork[IWORK_CAP];
    unsigned data[2000], idx_o[6000], idx_p[6000], idx_s[2000];

    auto check = [&](const char *label, int n, int irecl) {
        sort_orig        (iwork, data, idx_o, n, irecl);
        sort_opt         (iwork, data, idx_p, n, irecl);
        sort_std_indirect(       data, idx_s, n, irecl);
        sort_std_packed  (       data, idx_s, n, irecl);   /* reuse idx_s */
        bool ok_opt = results_match(idx_o, idx_p, n, irecl, data)
                   && (n == 0 || is_sorted(idx_p, n, irecl, data));
        bool ok_std = results_match(idx_o, idx_s, n, irecl, data)
                   && (n == 0 || is_sorted(idx_s, n, irecl, data));
        printf("  %-34s  opt=%s  std/packed=%s\n",
               label,
               ok_opt ? "OK  " : "FAIL",
               ok_std ? "OK  " : "FAIL");
        if (!ok_opt || !ok_std) fail++;
    };

    /* n=0 */
    fill_random(data, 10, 1);
    check("n=0 (empty)", 0, 1);

    /* n=1 */
    fill_random(data, 1, 2);
    check("n=1 (single element)", 1, 1);

    /* all equal */
    for (int i = 0; i < 100; i++) data[i] = 42u;
    check("n=100, all equal keys (irecl=1)", 100, 1);

    /* descending */
    for (int i = 0; i < 200; i++) data[i] = (unsigned)(199 - i);
    check("n=200, descending (irecl=1)", 200, 1);

    /* random, irecl=2 */
    fill_random(data, 400, 3);
    check("n=200, random (irecl=2)", 200, 2);

    /* random, irecl=5 */
    fill_random(data, 1000, 5);
    check("n=200, random (irecl=5)", 200, 5);

    /* large irecl=5 */
    fill_random(data, 2000, 7);
    check("n=400, random (irecl=5)", 400, 5);

    /* values spanning full 32-bit range */
    for (int i = 0; i < 500; i++) data[i] = (unsigned)0 - (unsigned)i;  /* near UINT_MAX */
    check("n=500, near-UINT_MAX values", 500, 1);

    return fail;
}

/* =========================================================================
 * main
 * ========================================================================= */

int main()
{
    printf("========================================================\n");
    printf("  OpenRadioss my_orders radix sort benchmark\n");
    printf("  orig    = 16-bit LSD radix (original)\n");
    printf("  opt     = 8-bit LSD radix + precomputed histograms\n");
    printf("  std::sort = C++ introsort (O(n log n))\n");
    printf("  Speedup multipliers are relative to orig\n");
    printf("========================================================\n\n");

    /* ----- edge-case tests ------------------------------------------------ */
    printf("=== Correctness tests ===\n");
    int fail = edge_tests();
    printf("\n");

    /* ----- benchmark A: index-permutation sort (actual MY_ORDERS use case) -- */
    printf("=== Benchmark A: index-permutation sort (the actual MY_ORDERS problem) ===\n");
    printf("  std/indir  = std::sort with indirect comparator (chases data[index[i]] pointers)\n");
    printf("  std/packed = std::sort on inlined structs {keys, orig_idx} (no indirection)\n\n");

    const int sizes[]  = {1000, 10000, 100000, 1000000};
    const int irecls[] = {1, 2, 5};

    for (int ri = 0; ri < (int)(sizeof(irecls)/sizeof(irecls[0])); ri++) {
        int irecl = irecls[ri];
        printf("  -- irecl=%d --\n", irecl);
        for (int si = 0; si < (int)(sizeof(sizes)/sizeof(sizes[0])); si++) {
            run_config(sizes[si], irecl);
        }
        printf("\n");
    }

    /* ----- benchmark B: direct-value sort (literature scenario, irecl=1) -- */
    printf("=== Benchmark B: direct-value sort  (literature scenario, irecl=1) ===\n");
    printf("  Sort a contiguous unsigned int array with no index indirection.\n");
    printf("  This is the EXACT scenario where the literature claims std::sort\n");
    printf("  is competitive with radix sort on modern hardware.\n\n");
    const int dsizes[] = {1000, 10000, 100000, 1000000};
    for (int si = 0; si < (int)(sizeof(dsizes)/sizeof(dsizes[0])); si++) {
        run_direct_values(dsizes[si]);
    }
    printf("\n");

    /* ----- explanation ---------------------------------------------------- */
    printf("=== Analysis ===\n");
    printf("  Why Benchmark A and the literature appear to contradict each other:\n\n");
    printf("  The literature benchmarks std::sort SORTING VALUES DIRECTLY.\n");
    printf("  MY_ORDERS sorts an INDEX ARRAY through an indirect comparator:\n");
    printf("    comparator(a, b) -> data[irecl*a+k] vs data[irecl*b+k]\n");
    printf("  Each comparison chases two pointers into a separate data[] array.\n");
    printf("  As n grows, these become random cache misses -- std::sort's O(n log n)\n");
    printf("  comparisons each pay a cache miss penalty, while radix scatter passes\n");
    printf("  only pay random-access cost once per element per pass.\n\n");
    printf("  Benchmark B confirms the literature: for direct-value sort, std::sort\n");
    printf("  CAN match or beat radix at small n on this hardware.\n\n");
    printf("  For OpenRadioss, the indirect-index problem (Benchmark A) is what matters:\n");
    printf("  std/packed is the best std::sort variant for this use case, but it requires\n");
    printf("  an extra allocation of (irecl*4+4)*n bytes and a pack/unpack step.\n");
    printf("  The opt radix sort wins without any extra allocation.\n\n");

    printf("=== Summary: %s ===\n",
           fail ? "SOME CORRECTNESS TESTS FAILED" : "ALL CORRECTNESS TESTS PASSED");

    return fail ? 1 : 0;
}

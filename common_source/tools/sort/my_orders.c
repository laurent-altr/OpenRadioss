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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define _FCALL

void tri_direct(unsigned *data,unsigned *iwork, unsigned *index,int n,int irecl,unsigned *inds)
{
    /* 8-bit LSD radix sort with single-pass histogram build.
     *
     * iwork layout: iwork[(k*4 + b)*257 + 0..256] = histogram for key k, byte b.
     * Total iwork entries used: irecl * 4 * 257
     *   (e.g. 1028 for irecl=1, 2056 for irecl=2, 5140 for irecl=5)
     * All callers allocate >= 70000 entries -- no API change required.
     *
     * 4*irecl scatter passes total (always a multiple of 4, hence even),
     * so the final result always lands in index[] without any extra copy.
     */
    int i, k, b;

    /* Phase 1: clear all histograms with a single memset */
    memset(iwork, 0, (size_t)irecl * 4 * 257 * sizeof(unsigned));

    /* Phase 2: single sequential scan through data[] to build all histograms.
     * Accessing data[irecl*i + k] for i=0..n-1 is a forward sequential sweep --
     * cache-friendly. All 4*irecl histogram arrays fit comfortably in L1 cache
     * (max 5140 entries = ~20 KB for irecl=5). */
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

    /* Phase 3: convert histograms to exclusive prefix sums (cumulative offsets).
     * After this, h[v] holds the starting output position for bucket v. */
    for (k = 0; k < irecl; k++) {
        for (b = 0; b < 4; b++) {
            unsigned *h = iwork + (k * 4 + b) * 257;
            for (i = 0; i < 256; i++)
                h[i + 1] += h[i];
        }
    }

    /* Phase 4: 4*irecl scatter passes in LSD order.
     * Outer loop: k from irecl-1 down to 0  (least-significant key first)
     * Inner loop: b from 0 to 3             (least-significant byte first)
     * Buffers alternate between index[] and inds[] each pass.
     * After 4*irecl (even) swaps src always points back to index[]. */
    unsigned *src = index, *dst = inds;
    for (k = irecl - 1; k >= 0; k--) {
        for (b = 0; b < 4; b++) {
            unsigned *h = iwork + (k * 4 + b) * 257;
            unsigned shift = (unsigned)(b * 8);
            for (i = 0; i < n; i++) {
                unsigned idx = src[i];
                unsigned byte_val = (data[irecl * idx + k] >> shift) & 0xff;
                dst[h[byte_val]++] = idx;
            }
            /* swap source and destination for the next pass */
            unsigned *tmp = src; src = dst; dst = tmp;
        }
    }
    /* src == index[] after 4*irecl even swaps -- result is already there */
}
void my_orders_(int *mode,unsigned *iwork,unsigned *data,unsigned *index,int *n,int *irecl)
{
    int i;
    if(*mode == 0){
        for(i = 0 ;i < *n ; i++) index[i] = i ;
        tri_direct(data,iwork,index,*n,*irecl,index+*n);
        for(i = 0 ;i < *n ; i++) index[i] += 1 ;/* c => fortran */
    } else if(*mode == 10){
        for(i = 0 ;i < *n ; i++) index[i] -= 1 ;
        tri_direct(data,iwork,index,*n,*irecl,index+*n);
        for(i = 0 ;i < *n ; i++) index[i] += 1 ;/* c => fortran */
    } else {
        *mode = -1;
    }
}
void _FCALL MY_ORDERS(int *mode,int *iwork,int *data,int *index,int *n,int *irecl)
{my_orders_(mode,(unsigned*)iwork,(unsigned*)data,(unsigned*)index,n,irecl);}

void my_orders(int *mode,int *iwork,int *data,int *index,int *n,int *irecl)
{my_orders_(mode,(unsigned*)iwork,(unsigned*)data,(unsigned*)index,n,irecl);}

void my_orders__(int *mode,int *iwork,int *data,int *index,int *n,int *irecl)
{my_orders_(mode,(unsigned*)iwork,(unsigned*)data,(unsigned*)index,n,irecl);}


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

// C++ wrappers around STL sort, callable from Fortran via iso_c_binding.
// Only canonical (no-underscore) symbols are exported; name mangling is
// handled entirely on the Fortran side through bind(C, name='...').

#include <algorithm>
#include <utility>
#include <vector>

#ifdef MYREAL8
#define my_real double
#else
#define my_real float
#endif

// ---------------------------------------------------------------------------
// Internal generic key-value sort
// ---------------------------------------------------------------------------

template<typename K, typename V>
static void stlsort_kv_impl(int n, K *keys, V *values)
{
    std::vector<std::pair<K, V>> pairs(n);
    for (int i = 0; i < n; ++i)
        pairs[i] = {keys[i], values[i]};

    std::sort(pairs.begin(), pairs.end(),
              [](const std::pair<K,V> &a, const std::pair<K,V> &b){
                  return a.first < b.first;
              });

    for (int i = 0; i < n; ++i) {
        keys[i]   = pairs[i].first;
        values[i] = pairs[i].second;
    }
}

// ---------------------------------------------------------------------------
// Public C interface  (consumed by cppsort_mod via iso_c_binding)
// ---------------------------------------------------------------------------

extern "C" {

// Sort a real array in ascending order
void stlsort(int *len, my_real *array)
{
    std::sort(array, array + *len);
}

// Sort integer keys, carrying integer values
void stlsort_int_int(int *len, int *keys, int *values)
{
    stlsort_kv_impl<int, int>(*len, keys, values);
}

// Sort real keys, carrying integer values
void stlsort_real_int(int *len, my_real *keys, int *values)
{
    stlsort_kv_impl<my_real, int>(*len, keys, values);
}

// Sort real keys, carrying real values
void stlsort_real_real(int *len, my_real *keys, my_real *values)
{
    stlsort_kv_impl<my_real, my_real>(*len, keys, values);
}

} // extern "C"

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

#include <unordered_map>
#include <string>
#include <vector>
#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstring>

// ------------------------------------------------------------------------------------
// Internal storage: maps allocation site message -> cumulated bytes allocated
// ------------------------------------------------------------------------------------
static std::unordered_map<std::string, uint64_t>& get_alloc_map()
{
    static std::unordered_map<std::string, uint64_t> alloc_map;
    return alloc_map;
}

// ------------------------------------------------------------------------------------
// Internal storage: maps allocation site message -> peak (high-water mark) bytes
// ------------------------------------------------------------------------------------
static std::unordered_map<std::string, uint64_t>& get_peak_map()
{
    static std::unordered_map<std::string, uint64_t> peak_map;
    return peak_map;
}

// ------------------------------------------------------------------------------------
// Helper: trim trailing spaces from a Fortran string
// ------------------------------------------------------------------------------------
static std::string trim(const char* str, int len)
{
    // Find the last non-space character
    int end = len - 1;
    while (end >= 0 && (str[end] == ' ' || str[end] == '\0')) {
        --end;
    }
    if (end < 0) return std::string();
    return std::string(str, end + 1);
}

// ------------------------------------------------------------------------------------
// C interface: record an allocation
//   msg     : Fortran character string (not null-terminated)
//   msg_len : length of the msg string
//   nbytes  : number of bytes allocated (passed as int64)
// ------------------------------------------------------------------------------------
extern "C" void cpp_record_alloc(const char* msg, const int* msg_len, const int64_t* nbytes)
{
    std::string key = trim(msg, *msg_len);
    if (key.empty()) return;
    uint64_t& current = get_alloc_map()[key];
    current += static_cast<uint64_t>(*nbytes);
    // Update peak (high-water mark)
    uint64_t& peak = get_peak_map()[key];
    if (current > peak) peak = current;
}

// ------------------------------------------------------------------------------------
// C interface: record a deallocation (subtracts bytes from the allocation map)
//   msg     : Fortran character string (not null-terminated)
//   msg_len : length of the msg string
//   nbytes  : number of bytes deallocated (passed as int64)
// ------------------------------------------------------------------------------------
extern "C" void cpp_record_dealloc(const char* msg, const int* msg_len, const int64_t* nbytes)
{
    std::string key = trim(msg, *msg_len);
    if (key.empty()) return;
    auto& alloc_map = get_alloc_map();
    auto it = alloc_map.find(key);
    if (it != alloc_map.end()) {
        uint64_t to_subtract = static_cast<uint64_t>(*nbytes);
        if (it->second > to_subtract) {
            it->second -= to_subtract;
        } else {
            alloc_map.erase(it);
        }
    }
}

// ------------------------------------------------------------------------------------
// C interface: print a report of the top 100 allocation sites by cumulated size
// ------------------------------------------------------------------------------------
extern "C" void cpp_print_alloc_report()
{
    auto& alloc_map = get_alloc_map();
    auto& peak_map = get_peak_map();

    if (alloc_map.empty() && peak_map.empty()) {
        std::printf("\n === ALLOCATION REPORT: no allocations recorded ===\n\n");
        return;
    }

    // Merge keys from both maps (peak_map may have entries removed from alloc_map)
    struct entry_t {
        std::string key;
        uint64_t current;
        uint64_t peak;
    };
    std::vector<entry_t> entries;
    entries.reserve(peak_map.size());
    for (auto& kv : peak_map) {
        uint64_t cur = 0;
        auto it = alloc_map.find(kv.first);
        if (it != alloc_map.end()) cur = it->second;
        entries.push_back({kv.first, cur, kv.second});
    }

    // Sort descending by peak bytes
    std::sort(entries.begin(), entries.end(),
              [](const entry_t& a, const entry_t& b) {
                  return a.peak > b.peak;
              });

    // Print header
    std::printf("\n");
    std::printf(" ====================================================================================================\n");
    std::printf("               MEMORY ALLOCATION REPORT - TOP 100 ALLOCATION SITES (>= 1 MB peak)\n");
    std::printf(" ====================================================================================================\n");
    std::printf(" %4s  %-55s %12s %12s\n", "Rank", "Allocation site (msg)", "Current (MB)", "Peak (MB)");
    std::printf(" %-4s  %-55s %12s %12s\n", "----",
                "-------------------------------------------------------",
                "------------", "------------");

    // Print up to 100 entries, skip those with peak < 1 MB
    static const uint64_t ONE_MB = 1024UL * 1024UL;
    size_t rank = 0;
    uint64_t total_current = 0;
    uint64_t total_peak = 0;
    for (size_t i = 0; i < entries.size() && rank < 100; ++i) {
        uint64_t peak_mb = entries[i].peak / ONE_MB;
        if (peak_mb < 1) break;  // sorted descending, so all remaining are < 1 MB
        ++rank;
        std::printf(" %4zu  %-55s %12lu %12lu\n", rank, entries[i].key.c_str(),
                    static_cast<unsigned long>(entries[i].current / ONE_MB),
                    static_cast<unsigned long>(peak_mb));
        total_current += entries[i].current;
        total_peak += entries[i].peak;
    }

    // Print totals
    uint64_t grand_current = 0;
    uint64_t grand_peak = 0;
    for (auto& e : entries) {
        grand_current += e.current;
        grand_peak += e.peak;
    }

    std::printf(" %-4s  %-55s %12s %12s\n", "----",
                "-------------------------------------------------------",
                "------------", "------------");
    std::printf(" %4s  %-55s %12lu %12lu\n", "", "Displayed total (MB)",
                static_cast<unsigned long>(total_current / ONE_MB),
                static_cast<unsigned long>(total_peak / ONE_MB));
    std::printf(" %4s  %-55s %12lu %12lu\n", "", "Grand total (MB)",
                static_cast<unsigned long>(grand_current / ONE_MB),
                static_cast<unsigned long>(grand_peak / ONE_MB));
    std::printf(" ====================================================================================================\n");
    std::printf("\n");
}

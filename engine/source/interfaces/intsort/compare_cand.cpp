#include <vector>
#include <utility>
#include <iostream>
#include <algorithm>

extern "C"
{

    void compare_cand(int *cand_n, int *cand_e, int ii_stok, int *cand_n_ref, int *cand_e_ref, int ii_stok_ref)
    {
        std::vector<std::pair<int, int>> candidates, candidates_ref;
        for (int i = 0; i < ii_stok; ++i)
        {
            candidates.emplace_back(cand_n[i], cand_e[i]);
        }
        for (int i = 0; i < ii_stok_ref; ++i)
        {
            candidates_ref.emplace_back(cand_n_ref[i], cand_e_ref[i]);
        }

        std::sort(candidates.begin(), candidates.end());
        std::sort(candidates_ref.begin(), candidates_ref.end());

        int num_matches = 0;
        int i = 0, j = 0;
        while (i < ii_stok && j < ii_stok_ref)
        {
            if (candidates[i] == candidates_ref[j])
            {
                num_matches++;
                ++i;
                ++j; 
            }
            else if (candidates[i] < candidates_ref[j])
            {
                ++i; 
            }
            else
            {
                ++j; 
            }
        }

        std::cout << "number of candidates: " << ii_stok << "( "<<ii_stok_ref<<")\n";
        if (num_matches == ii_stok_ref)
        {
            std::cout << "Success: All candidates_ref are found in candidates.\n";
        }
        else
        {
            std::cout << "Failure: Not all candidates_ref are found in candidates.\n";
        }
    }
}

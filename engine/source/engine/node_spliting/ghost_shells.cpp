#include <iostream>
#include <vector>
#include <algorithm>
#include <set>

// define the type reverse_connectivity as a vector<vector<int>>
typedef std::vector<std::vector<int>> reverse_connectivity;

// Fortran callable functions

extern "C"
{
    // function build_reverse_connectivity(shells,nb_shells,mask,nspmd) result(c) bind(C,name="cpp_build_reverse_connectivity")

    reverse_connectivity * cpp_build_reverse_connectivity(
        int *shells, // 4 x nb_shells array : node id of the shells
        int nb_shells,
        int *mask, // nspmd x nb_shells array: 1 => shell is to be considered
        int nspmd)
    {
        reverse_connectivity *c = new reverse_connectivity(nspmd);
        // find the maximum node id in the array shells of size 4 x nb_shells
        int max_node_id = 0;
        for (int i = 0; i < nb_shells; i++)
        {
            // consider only if one value of mask(1:nspmd,i) is 1
            bool consider = false;
            for (int j = 0; j < nspmd; j++)
            {
                if (mask[j * nb_shells + i] == 1)
                {
                    consider = true;
                    break;
                }
            }
            for (int j = 0; j < 4; j++)
            {
                if (shells[i * 4 + j] > max_node_id)
                {
                    max_node_id = shells[i * 4 + j];
                }
            }
        }
        std::vector<std::set<int>>  node2shell(max_node_id + 1); 


        for (int i = 0; i < nb_shells; i++)
        {
            // consider only if one value of mask(1:nspmd,i) is 1
            bool consider = false;
            for (int j = 0; j < nspmd; j++)
            {
                if (mask[j * nb_shells + i] == 1)
                {
                    consider = true;
                    break;
                }
            }
            if (!consider)
                continue;

            for (int j = 0; j < 4; j++)
            {
                int node_id = shells[i * 4 + j];
                node2shell[node_id].insert(i);
            }
        }

        // convert node2shell to reverse_connectivity
        for (int i = 0; i < max_node_id + 1; i++)
        {
            std::set<int> &shells_set = node2shell[i];
            if (shells_set.size() > 0)
            {
                (*c)[i].resize(shells_set.size());
                std::copy(shells_set.begin(), shells_set.end(), (*c)[i].begin());
            }
        }

        return c;
    }


    int* cpp_get_shells_list(void *c, int p, int* n)
    {
        // p is the process id
        // n is the size of the shell list [out]
        reverse_connectivity *rc = static_cast<reverse_connectivity *>(c);

        *n = (*rc)[p].size();
        if (*n == 0)
        {
            return nullptr;
        }
        else
        {
            // return the pointer do *data
            return (*rc)[p].data();
       }
    }
    void cpp_destroy_reverse_connectivity(void *c)
    {
        // destroy the reverse_connectivity object
        reverse_connectivity *rc = static_cast<reverse_connectivity *>(c);
        //free each vector in the reverse_connectivity
        for (auto &vec : *rc)
        {
            vec.clear();
        }
        // free the reverse_connectivity object
        delete rc;
        rc = nullptr;
    }
}
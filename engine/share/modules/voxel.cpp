#include "voxel.h"

#ifdef MYREAL8
#define my_real double
#else
#define my_real float
#endif

extern "C"
{
    void *Voxel_new(int nbx, int nby, int nbz, int nbsurfaces, int nbnodes)
    {
        Voxel *v = new Voxel();
        v->nbx = nbx;
        v->nby = nby;
        v->nbz = nbz;
        // allocate the vector of nodes
        v->nodes.resize(nbnodes);
        // fill with
        // there are nbx*nby*nbz cells
        v->cells.resize(nbx * nby * nbz);
        // allocate the vector of surfaces
        v->surfaceBounds.resize(nbsurfaces);
        v->surfaceNodes.resize(nbsurfaces);
        v->surfaceCandidates.resize(nbsurfaces);
        return v;
    }

    void Voxel_restart(void *v, int nbx, int nby, int nbz, int nbsurfaces, int nbnodes)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        voxel->nbx = nbx;
        voxel->nby = nby;
        voxel->nbz = nbz;
        voxel->cells.clear();
        voxel->cells.resize(nbx * nby * nbz);
        voxel->surfaceBounds.clear();
        voxel->surfaceNodes.clear();
        voxel->surfaceCandidates.clear();
        voxel->nodes.resize(nbnodes);
        voxel->surfaceBounds.resize(nbsurfaces);
        voxel->surfaceNodes.resize(nbsurfaces);
        voxel->surfaceCandidates.resize(nbsurfaces);
    }

    void Voxel_set_bounds(void *v, double xmin, double ymin, double zmin,
                          double xmax, double ymax, double zmax)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        voxel->bounds[XMIN] = xmin;
        voxel->bounds[YMIN] = ymin;
        voxel->bounds[ZMIN] = zmin;
        voxel->bounds[XMAX] = xmax;
        voxel->bounds[YMAX] = ymax;
        voxel->bounds[ZMAX] = zmax;
        // #ifdef DEBUG_PRINT
        std::cout << "Voxel bounds min: " << xmin << " " << ymin << " " << zmin << std::endl;
        std::cout << "Voxel bounds max: " << xmax << " " << ymax << " " << zmax << std::endl;
        // #endif
    }

    void Voxel_get_bounds(void *v, double *xmin, double *ymin, double *zmin,
                          double *xmax, double *ymax, double *zmax)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        *xmin = voxel->bounds[XMIN];
        *ymin = voxel->bounds[YMIN];
        *zmin = voxel->bounds[ZMIN];
        *xmax = voxel->bounds[XMAX];
        *ymax = voxel->bounds[YMAX];
        *zmax = voxel->bounds[ZMAX];
    }

    std::vector<Node> getCellsInWrappedRange(short int xmin, short int xmax, short int ymin, short int ymax,
                                             short int zmin, short int zmax, size_t nbx, size_t nby, size_t nbz)
    {
        std::vector<Node> cells;

        if (xmin > xmax || ymin > ymax || zmin > zmax)
        {
            std::cerr << "Error: Invalid range for cell coordinates." << std::endl;
            std::cerr << "xmin: " << xmin << ", xmax: " << xmax << std::endl;
            std::cerr << "ymin: " << ymin << ", ymax: " << ymax << std::endl;
            std::cerr << "zmin: " << zmin << ", zmax: " << zmax << std::endl;
            exit(1);
        }
        // Handle cases where the range is larger than the grid
        // If max-min+1 >= grid size, include all grid cells in that dimension
        bool includeAllX = (xmax >= xmin && xmax - xmin + 1 >= nbx) || (xmin > xmax && nbx - xmin + xmax + 1 >= nbx);
        bool includeAllY = (ymax >= ymin && ymax - ymin + 1 >= nby) || (ymin > ymax && nby - ymin + ymax + 1 >= nby);
        bool includeAllZ = (zmax >= zmin && zmax - zmin + 1 >= nbz) || (zmin > zmax && nbz - zmin + zmax + 1 >= nbz);

        // Handle X dimension
        std::vector<short int> xIndices;
        if (includeAllX)
        {
            for (short int i = 0; i < nbx; i++)
            {
                xIndices.push_back(i);
            }
        }
        else if (xmin <= xmax)
        {
            // No wrapping in X
            for (short int i = xmin; i <= xmax; i++)
            {
                xIndices.push_back(i);
            }
        }
        else
        {
            // Wrapping in X
            for (short int i = xmin; i < nbx; i++)
            {
                xIndices.push_back(i);
            }
            for (short int i = 0; i <= xmax; i++)
            {
                xIndices.push_back(i);
            }
        }

        // Handle Y dimension
        std::vector<short int> yIndices;
        if (includeAllY)
        {
            for (short int j = 0; j < nby; j++)
            {
                yIndices.push_back(j);
            }
        }
        else if (ymin <= ymax)
        {
            // No wrapping in Y
            for (short int j = ymin; j <= ymax; j++)
            {
                yIndices.push_back(j);
            }
        }
        else
        {
            // Wrapping in Y
            for (short int j = ymin; j < nby; j++)
            {
                yIndices.push_back(j);
            }
            for (short int j = 0; j <= ymax; j++)
            {
                yIndices.push_back(j);
            }
        }

        // Handle Z dimension
        std::vector<short int> zIndices;
        if (includeAllZ)
        {
            for (short int k = 0; k < nbz; k++)
            {
                zIndices.push_back(k);
            }
        }
        else if (zmin <= zmax)
        {
            // No wrapping in Z
            for (short int k = zmin; k <= zmax; k++)
            {
                zIndices.push_back(k);
            }
        }
        else
        {
            // Wrapping in Z
            for (short int k = zmin; k < nbz; k++)
            {
                zIndices.push_back(k);
            }
            for (short int k = 0; k <= zmax; k++)
            {
                zIndices.push_back(k);
            }
        }

        // Special case for edge boundaries
        // If max exactly equals the grid size, we need to include cell 0 as well
        if (xmax == nbx - 1 && xmin > 0)
            xIndices.push_back(0);
        if (ymax == nby - 1 && ymin > 0)
            yIndices.push_back(0);
        if (zmax == nbz - 1 && zmin > 0)
            zIndices.push_back(0);

        // Build the Cartesian product of the three index arrays
        for (short int i : xIndices)
        {
            for (short int j : yIndices)
            {
                for (short int k : zIndices)
                {
                    Node cell = {i, j, k};
                    cells.push_back(cell);
                }
            }
        }

        return cells;
    }

    void Voxel_initialize(void *v, int *irect, int nrtm, my_real *gap, int *nsv, int nsn, my_real *X, int numnod)
    {
        // X = coordinates of the nodes, size 3*numnod
        // nrtm = number of surfaces
        // irect(1:4, 1:nrtm) = global id of the 4 nodes of the surface
        // nsv(i) = global id of the node i
        // gap(1:nrtm) = gap of the surface, i.e. the distance beyond the surface to be considered
        Voxel *voxel = static_cast<Voxel *>(v);
        GridMapper mapper(voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);

        // Create a map to convert global ids of irect, into nsv ids, if they exist
        std::unordered_map<int, int> glob2nsv;
        for (int i = 0; i < nsn; i++)
        {
            glob2nsv[nsv[i] - 1] = i; // Fortran to C++ index conversion
        }

        // Loop over the surfaces
        for (int i = 0; i < nrtm; i++)
        {
            // Loop over the 4 nodes of the surface
            for (int j = 0; j < 4; j++)
            {
                // get the global id of the node
                int globId = irect[j + 4 * i] - 1; // Fortran to C++ index conversion
                voxel->surfaceNodes[i][j] = -1;    // initialize the surfaceNodes
                // check if the node exists in nsv
                if (glob2nsv.find(globId) != glob2nsv.end())
                {
                    // get the local id of the node
                    int localId = glob2nsv[globId];
                    // add the node to the surfaceNodes
                    voxel->surfaceNodes[i][j] = localId;
                }
            }
        }
        // print nbx nby, nbz
        // Loop over the surfaces, add the surfaces to the cells it crosses
        for (int i = 0; i < nrtm; i++)
        {
            // get the gap of the surface
            double gapValue = static_cast<double>(gap[i]);
            // get the coordinates of the surface
            const size_t i1 = irect[0 + 4 * i] - 1; // Fortran to C++ index conversion
            const size_t i2 = irect[1 + 4 * i] - 1;
            const size_t i3 = irect[2 + 4 * i] - 1;
            const size_t i4 = irect[3 + 4 * i] - 1;
            // get the coordinates of the nodes
            const double x1 = static_cast<double>(X[3 * i1]);
            const double y1 = static_cast<double>(X[3 * i1 + 1]);
            const double z1 = static_cast<double>(X[3 * i1 + 2]);
            const double x2 = static_cast<double>(X[3 * i2]);
            const double y2 = static_cast<double>(X[3 * i2 + 1]);
            const double z2 = static_cast<double>(X[3 * i2 + 2]);
            const double x3 = static_cast<double>(X[3 * i3]);
            const double y3 = static_cast<double>(X[3 * i3 + 1]);
            const double z3 = static_cast<double>(X[3 * i3 + 2]);
            const double x4 = static_cast<double>(X[3 * i4]);
            const double y4 = static_cast<double>(X[3 * i4 + 1]);
            const double z4 = static_cast<double>(X[3 * i4 + 2]);
            const double xmin = std::min(std::min(x1, x2), std::min(x3, x4)) - gapValue;
            const double ymin = std::min(std::min(y1, y2), std::min(y3, y4)) - gapValue;
            const double zmin = std::min(std::min(z1, z2), std::min(z3, z4)) - gapValue;
            const double xmax = std::max(std::max(x1, x2), std::max(x3, x4)) + gapValue;
            const double ymax = std::max(std::max(y1, y2), std::max(y3, y4)) + gapValue;
            const double zmax = std::max(std::max(z1, z2), std::max(z3, z4)) + gapValue;
            const Node minCoords = mapper.mapMin(xmin, ymin, zmin);
            const Node maxCoords = mapper.mapMax(xmax, ymax, zmax);
            if(maxCoords[0] < minCoords[0] || maxCoords[1] < minCoords[1] || maxCoords[2] < minCoords[2])
            {
                std::cerr << "Error: Invalid range for surface coordinates" << std::endl;
                std::cerr << "xmin: " << xmin << ", xmax: " << xmax << std::endl;
                std::cerr << "ymin: " << ymin << ", ymax: " << ymax << std::endl;
                std::cerr << "zmin: " << zmin << ", zmax: " << zmax << std::endl;
                std::cerr << "minCoords: " << minCoords[0] << " " << minCoords[1] << " " << minCoords[2] << std::endl;
                std::cerr << "maxCoords: " << maxCoords[0] << " " << maxCoords[1] << " " << maxCoords[2] << std::endl;
                std::cerr << "nbx: " << voxel->nbx << " nby: " << voxel->nby << " nbz: " << voxel->nbz << std::endl;
                std::cerr<< "bounds: " << voxel->bounds[XMIN] << " " << voxel->bounds[YMIN] << " " << voxel->bounds[ZMIN] << std::endl;
                exit(1);
            }

            std::vector<Node> cells = getCellsInWrappedRange(minCoords[0], maxCoords[0], minCoords[1], maxCoords[1], minCoords[2], maxCoords[2], voxel->nbx, voxel->nby, voxel->nbz);
            // add the surface to the cells it crosses
            for (auto cell : cells)
            {
                size_t index = coord_to_index(cell, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
                // check if the cell exists
//                if (voxel->cells.find(index) == voxel->cells.end())
//                {
//                    // create the new cell
//                    voxel->cells[index] = Cell();
//                }
                // add the surface to the cell
                // voxel->cells[index].surfaces.insert(i);
                voxel->cells[index].surfaces.push_back(i);
            }
            Surf newCoords;
            newCoords[XMIN] = minCoords[0];
            newCoords[YMIN] = minCoords[1];
            newCoords[ZMIN] = minCoords[2];
            newCoords[XMAX] = maxCoords[0];
            newCoords[YMAX] = maxCoords[1];
            newCoords[ZMAX] = maxCoords[2];
            voxel->surfaceBounds[i] = newCoords;
        }

        // Loop over the nodes, add the nodes to the cells they belong to
        for (int i = 0; i < nsn; ++i)
        {
            // get the coordinates of the node
            const size_t i1 = nsv[i] - 1; // Fortran to C++ index conversion
            const double x = static_cast<double>(X[3 * i1]);
            const double y = static_cast<double>(X[3 * i1 + 1]);
            const double z = static_cast<double>(X[3 * i1 + 2]);
            // get the index of the cell
            size_t index = coord_to_index(x, y, z, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
            // check if the cell exists
//            if (voxel->cells.find(index) == voxel->cells.end())
//            {
//                // create the new cell
//                voxel->cells[index] = Cell();
//            }
            // add the node to the cell
            // voxel->cells[index].nodes.insert(i);
            voxel->cells[index].nodes.push_back(i);

            // add the node to the nodes vector
            // voxel->nodes[i] = coord_to_grid(x, y, z, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
            voxel->nodes[i] = mapper.mapMin(x, y, z);

            // loop over the surfaces crossing the cell
            for (auto surfId : voxel->cells[index].surfaces)
            {
                // check the 4 nodes
                // if (voxel->surfaceNodes[surfId].find(i) == voxel->surfaceNodes[surfId].end())
                if (voxel->surfaceNodes[surfId][0] != i && voxel->surfaceNodes[surfId][1] != i &&
                    voxel->surfaceNodes[surfId][2] != i && voxel->surfaceNodes[surfId][3] != i)
                {
                    // add the node to the surfaceCandiates
                    voxel->surfaceCandidates[surfId].push_back(i);
                }
            }
        }
    }

    void Voxel_delete(void *v)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        delete voxel;
    }
    void Voxel_update_node(void *v, double x, double y, double z, int nodeId)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        --nodeId; // Fortran to C++ index conversion
        size_t newIndex = coord_to_index(x, y, z, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        size_t oldIndex = coord_to_index(voxel->nodes[nodeId], voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        if (newIndex != oldIndex)
        {
            // remove the node from the old cell
            // voxel->cells[oldIndex].nodes.erase(nodeId);
            swap_and_pop(voxel->cells[oldIndex].nodes, nodeId);
            // remove the node from candidates list of all surfaces crossing the old cell
            for (auto surfId : voxel->cells[oldIndex].surfaces)
            {
                // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
                // if (voxel->surfaceNodes[surfId].find(nodeId) == voxel->surfaceNodes[surfId].end())
                if (voxel->surfaceNodes[surfId][0] != nodeId && voxel->surfaceNodes[surfId][1] != nodeId &&
                    voxel->surfaceNodes[surfId][2] != nodeId && voxel->surfaceNodes[surfId][3] != nodeId)
                {
                    // remove the node from the surfaceCandiates
                    //        voxel->surfaceCandidates[surfId].erase(nodeId);
                    swap_and_pop(voxel->surfaceCandidates[surfId], nodeId);
                }
            }

            // add the node to the new cell
            // check if the new cell exists
         //   if (voxel->cells.find(newIndex) == voxel->cells.end())
         //   {
         //       // create the new cell
         //       voxel->cells[newIndex] = Cell();
         //   }
            // voxel->cells[newIndex].nodes.insert(nodeId);
            voxel->cells[newIndex].nodes.push_back(nodeId);

            voxel->nodes[nodeId] = index_to_coord(newIndex, voxel->nbx, voxel->nby, voxel->nbz);
            // loop over voxel->cells[newIndex].surfaces
            for (auto surfId : voxel->cells[newIndex].surfaces)
            {
                // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
                // if (voxel->surfaceNodes[surfId].find(nodeId) == voxel->surfaceNodes[surfId].end())
                // check the 4 nodes
                if (voxel->surfaceNodes[surfId][0] != nodeId && voxel->surfaceNodes[surfId][1] != nodeId &&
                    voxel->surfaceNodes[surfId][2] != nodeId && voxel->surfaceNodes[surfId][3] != nodeId)
                {
                    // add the node to the surfaceCandiates of all surfaces crossing the new cell
                    // voxel->surfaceCandidates[surfId].insert(nodeId);
                    voxel->surfaceCandidates[surfId].push_back(nodeId);
                }
            }
        }
    }

    void Voxel_update_surf(void *v, double xmin, double ymin, double zmin,
                           double xmax, double ymax, double zmax, int surfId)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        --surfId; // Fortran to C++ index conversion
        Surf newCoords;
        GridMapper mapper(voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        //Node minCoords = coord_to_grid(xmin, ymin, zmin, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz, false);
        //Node maxCoords = coord_to_grid(xmax, ymax, zmax, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz, true);
        Node minCoords = mapper.mapMin(xmin, ymin, zmin);
        Node maxCoords = mapper.mapMax(xmax, ymax, zmax);
        if(maxCoords[0] < minCoords[0] || maxCoords[1] < minCoords[1] || maxCoords[2] < minCoords[2])
        {
            std::cerr << "Error: Invalid range for surface coordinates" << std::endl;
            std::cerr << "xmin: " << xmin << ", xmax: " << xmax << std::endl;
            std::cerr << "ymin: " << ymin << ", ymax: " << ymax << std::endl;
            std::cerr << "zmin: " << zmin << ", zmax: " << zmax << std::endl;
            std::cerr << "minCoords: " << minCoords[0] << " " << minCoords[1] << " " << minCoords[2] << std::endl;
            std::cerr << "maxCoords: " << maxCoords[0] << " " << maxCoords[1] << " " << maxCoords[2] << std::endl;
            std::cerr << "nbx: " << voxel->nbx << " nby: " << voxel->nby << " nbz: " << voxel->nbz << std::endl;
            std::cerr<< "bounds: " << voxel->bounds[XMIN] << " " << voxel->bounds[YMIN] << " " << voxel->bounds[ZMIN] << std::endl;
            exit(1);
        }

 
//        if(xmin > xmax || ymin > ymax || zmin > zmax)
//        {
//            std::cerr << "Error: Invalid range for surface coordinates." << std::endl;
//            std::cerr << "xmin: " << xmin << ", xmax: " << xmax << std::endl;
//            std::cerr << "ymin: " << ymin << ", ymax: " << ymax << std::endl;
//            std::cerr << "zmin: " << zmin << ", zmax: " << zmax << std::endl;
//            exit(1);
//        }
        newCoords[XMIN] = minCoords[0];
        newCoords[YMIN] = minCoords[1];
        newCoords[ZMIN] = minCoords[2];
        newCoords[XMAX] = maxCoords[0];
        newCoords[YMAX] = maxCoords[1];
        newCoords[ZMAX] = maxCoords[2];

        // check if  voxel->surfaceBounds[surfId] exists
        if (voxel->surfaceBounds.size() <= static_cast<size_t>(surfId))
        {
            std::cerr << "Error: surface id " << surfId << " does not exist" << std::endl;
            return;
        }

        if (newCoords != voxel->surfaceBounds[surfId])
        {
            std::vector<Node> oldCells = getCellsInWrappedRange(voxel->surfaceBounds[surfId][XMIN], voxel->surfaceBounds[surfId][XMAX],
                                                                voxel->surfaceBounds[surfId][YMIN], voxel->surfaceBounds[surfId][YMAX],
                                                                voxel->surfaceBounds[surfId][ZMIN], voxel->surfaceBounds[surfId][ZMAX],
                                                                voxel->nbx, voxel->nby, voxel->nbz);
            // remove the surface from the old cells
            for (const auto &cell : oldCells)
            {
                size_t index = coord_to_index(cell, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
                //if (voxel->cells.find(index) != voxel->cells.end())
                //if the cell is not empty
                if(voxel->cells[index].surfaces.size() > 0)
                {
                    // voxel->cells[index].surfaces.erase(surfId);
                    swap_and_pop(voxel->cells[index].surfaces, surfId);
                    for (auto nodeId : voxel->cells[index].nodes)
                    {
                        // check the 4 nodes
                        if (voxel->surfaceNodes[surfId][0] != nodeId && voxel->surfaceNodes[surfId][1] != nodeId &&
                            voxel->surfaceNodes[surfId][2] != nodeId && voxel->surfaceNodes[surfId][3] != nodeId)
                        {
                            // voxel->surfaceCandidates[surfId].erase(nodeId);
                            swap_and_pop(voxel->surfaceCandidates[surfId], nodeId);
                        }
                    }
                }
            }
            // add the surface to the new cells
            std::vector<Node> newCells = getCellsInWrappedRange(newCoords[XMIN], newCoords[XMAX],
                                                                newCoords[YMIN], newCoords[YMAX],
                                                                newCoords[ZMIN], newCoords[ZMAX],
                                                                voxel->nbx, voxel->nby, voxel->nbz);
            for (const auto &cell : newCells)
            {
                size_t index = coord_to_index(cell, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
                // check if the cell exists
            //    if (voxel->cells.find(index) == voxel->cells.end())
            //    {
            //        // create the new cell
            //        voxel->cells[index] = Cell();
            //    }
            //    // add the surface to the cell
                // voxel->cells[index].surfaces.insert(surfId);
                voxel->cells[index].surfaces.push_back(surfId);
                // update surfaceCandidates
                for (auto nodeId : voxel->cells[index].nodes)
                {
                    // if (voxel->surfaceNodes[surfId].find(nodeId) == voxel->surfaceNodes[surfId].end())
                    //  check the 4 nodes
                    if (voxel->surfaceNodes[surfId][0] != nodeId && voxel->surfaceNodes[surfId][1] != nodeId &&
                        voxel->surfaceNodes[surfId][2] != nodeId && voxel->surfaceNodes[surfId][3] != nodeId)
                    {
                        // voxel->surfaceCandidates[surfId].insert(nodeId);
                        voxel->surfaceCandidates[surfId].push_back(nodeId);
                    }
                }
            }
            // update the surface
            voxel->surfaceBounds[surfId] = newCoords;
        }
    }

    int Voxel_get_max_candidates(void *v)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        int maxCandidates = 0;
        for (size_t i = 0; i < voxel->surfaceCandidates.size(); i++)
        {
            if (voxel->surfaceCandidates[i].size() > maxCandidates)
            {
                maxCandidates = voxel->surfaceCandidates[i].size();
            }
        }
        return maxCandidates;
    }

    void Voxel_get_candidates(void *v, int ne, int *cands, int *nb)
    {
        size_t id = static_cast<size_t>(ne - 1); // index of the surface, C to Fortran conversion
        Voxel *voxel = static_cast<Voxel *>(v);
        // get the candidates for the surface and fill cands
        size_t counter = 0;
        for (auto it = voxel->surfaceCandidates[id].begin(); it != voxel->surfaceCandidates[id].end(); ++it)
        {
            // C to Fortran index conversion
            int index = static_cast<int>(*it) + 1;
            *cands = index;
            cands++;
        }
        // set the number of candidates
        *nb = static_cast<int>(voxel->surfaceCandidates[id].size());
        // debug print
    }
    //Copyless version
    void Voxel_get_candidates_data(void *v, int ne, int **cands, int *nb)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        *nb = static_cast<int>(voxel->surfaceCandidates[ne - 1].size());
        *cands = voxel->surfaceCandidates[ne - 1].data();
    }
}
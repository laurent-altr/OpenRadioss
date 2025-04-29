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

    using Node = std::array<short int, 3>;

    std::vector<Node> inline getCellsInRange(short int xmin, short int xmax,
                                             short int ymin, short int ymax,
                                             short int zmin, short int zmax,
                                             size_t nbx, size_t nby, size_t nbz)
    {
        std::vector<Node> cells;

        //       // Clamp input to grid boundaries (optional depending on your use case)
        xmin = std::max<short int>(0, std::min(xmin, static_cast<short int>(nbx - 1)));
        xmax = std::max<short int>(0, std::min(xmax, static_cast<short int>(nbx - 1)));
        ymin = std::max<short int>(0, std::min(ymin, static_cast<short int>(nby - 1)));
        ymax = std::max<short int>(0, std::min(ymax, static_cast<short int>(nby - 1)));
        zmin = std::max<short int>(0, std::min(zmin, static_cast<short int>(nbz - 1)));
        zmax = std::max<short int>(0, std::min(zmax, static_cast<short int>(nbz - 1)));

        //       // Early exit if range is invalid
        //       if (xmin > xmax || ymin > ymax || zmin > zmax)
        //           return cells;

        size_t xcount = xmax - xmin + 1;
        size_t ycount = ymax - ymin + 1;
        size_t zcount = zmax - zmin + 1;

        cells.reserve(xcount * ycount * zcount);

        for (short int i = xmin; i <= xmax; ++i)
        {
            for (short int j = ymin; j <= ymax; ++j)
            {
                for (short int k = zmin; k <= zmax; ++k)
                {
                    cells.push_back({i, j, k});
                }
            }
        }

        return cells;
    }

    void Voxel_initialize(void *v, int *irect, int nrtm, my_real *gap, int *nsv, int nsn, my_real *X, int numnod, my_real *stf, my_real *stfn)
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
            if (stf[i] <= static_cast<my_real>(0))
            {
                continue;
            }
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
            double xmin = std::min(std::min(x1, x2), std::min(x3, x4)) - gapValue;
            double ymin = std::min(std::min(y1, y2), std::min(y3, y4)) - gapValue;
            double zmin = std::min(std::min(z1, z2), std::min(z3, z4)) - gapValue;
            double xmax = std::max(std::max(x1, x2), std::max(x3, x4)) + gapValue;
            double ymax = std::max(std::max(y1, y2), std::max(y3, y4)) + gapValue;
            double zmax = std::max(std::max(z1, z2), std::max(z3, z4)) + gapValue;
            Node minCoords = mapper.mapMin(xmin, ymin, zmin);
            Node maxCoords = mapper.mapMax(xmax, ymax, zmax);
//            if (maxCoords[0] < minCoords[0] || maxCoords[1] < minCoords[1] || maxCoords[2] < minCoords[2])
//            {
//                std::cerr << "Error: Invalid range for surface coordinates" << std::endl;
//                std::cerr << "xmin: " << xmin << ", xmax: " << xmax << std::endl;
//                std::cerr << "ymin: " << ymin << ", ymax: " << ymax << std::endl;
//                std::cerr << "zmin: " << zmin << ", zmax: " << zmax << std::endl;
//                std::cerr << "minCoords: " << minCoords[0] << " " << minCoords[1] << " " << minCoords[2] << std::endl;
//                std::cerr << "maxCoords: " << maxCoords[0] << " " << maxCoords[1] << " " << maxCoords[2] << std::endl;
//                std::cerr << "nbx: " << voxel->nbx << " nby: " << voxel->nby << " nbz: " << voxel->nbz << std::endl;
//                std::cerr << " bounds: " << voxel->bounds[XMIN] << " " << voxel->bounds[YMIN] << " " << voxel->bounds[ZMIN] << std::endl;
//                std::cerr << " bounds: " << voxel->bounds[XMAX] << " " << voxel->bounds[YMAX] << " " << voxel->bounds[ZMAX] << std::endl;
//                xmin = std::max(xmin, voxel->bounds[XMIN]);
//                xmax = std::min(xmax, voxel->bounds[XMAX]);
//                ymin = std::max(ymin, voxel->bounds[YMIN]);
//                ymax = std::min(ymax, voxel->bounds[YMAX]);
//                zmin = std::max(zmin, voxel->bounds[ZMIN]);
//                zmax = std::min(zmax, voxel->bounds[ZMAX]);
//
//                xmin = std::min(xmin, voxel->bounds[XMAX]);
//                xmax = std::max(xmax, voxel->bounds[XMIN]);
//                ymin = std::min(ymin, voxel->bounds[YMAX]);
//                ymax = std::max(ymax, voxel->bounds[YMIN]);
//                zmin = std::min(zmin, voxel->bounds[ZMAX]);
//                zmax = std::max(zmax, voxel->bounds[ZMIN]);
//                minCoords = mapper.mapMin(xmin, ymin, zmin);
//                maxCoords = mapper.mapMax(xmax, ymax, zmax);
//            }
//
            for (short int ii = minCoords[0]; ii <= maxCoords[0]; ++ii)
            {
                for (short int jj = minCoords[1]; jj <= maxCoords[1]; ++jj)
                {
                    for (short int kk = minCoords[2]; kk <=  maxCoords[2]; ++kk)
                    {
                        size_t index = COORD_TO_INDEX(ii, jj, kk, voxel->nbx, voxel->nby);
                        voxel->cells[index].surfaces.push_back(i);
                    }
                }
            }

//            Surf newCoords;
//            newCoords[XMIN] = minCoords[0];
//            newCoords[YMIN] = minCoords[1];
//            newCoords[ZMIN] = minCoords[2];
//            newCoords[XMAX] = maxCoords[0];
//            newCoords[YMAX] = maxCoords[1];
//            newCoords[ZMAX] = maxCoords[2];
//            voxel->surfaceBounds[i] = newCoords;
              voxel->surfaceBounds[i] = {
                  minCoords[0],  // XMIN
                  minCoords[1],  // YMIN
                  minCoords[2],  // ZMIN
                  maxCoords[0],  // XMAX
                  maxCoords[1],  // YMAX
                  maxCoords[2]   // ZMAX
              };
        }

        // Loop over the nodes, add the nodes to the cells they belong to
        for (int i = 0; i < nsn; ++i)
        {
            if (stfn[i] <= static_cast<my_real>(0))
            {
                continue;
            }

            // get the coordinates of the node
            const size_t i1 = nsv[i] - 1; // Fortran to C++ index conversion
            const double x = static_cast<double>(X[3 * i1]);
            const double y = static_cast<double>(X[3 * i1 + 1]);
            const double z = static_cast<double>(X[3 * i1 + 2]);
            // get the index of the cell
            size_t index = coord_to_index(x, y, z, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
            voxel->cells[index].nodes.push_back(i);
            // add the node to the nodes vector
            voxel->nodes[i] = mapper.mapMin(x, y, z);

            // loop over the surfaces crossing the cell
            for (auto surfId : voxel->cells[index].surfaces)
            {
                // check the 4 nodes
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

    void inline Voxel_delete_node(void *v, int nodeId)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        if (voxel->nodes[nodeId][0] < 0)
        {
            return;
        }
        size_t index = coord_to_index(voxel->nodes[nodeId], voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        // remove the node from the cell
        swap_and_pop(voxel->cells[index].nodes, nodeId);
        // remove the node from candidates list of all surfaces crossing the cell
        for (auto surfId : voxel->cells[index].surfaces)
        {
            // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
            if (voxel->surfaceNodes[surfId][0] != nodeId && voxel->surfaceNodes[surfId][1] != nodeId &&
                voxel->surfaceNodes[surfId][2] != nodeId && voxel->surfaceNodes[surfId][3] != nodeId)
            {
                swap_and_pop(voxel->surfaceCandidates[surfId], nodeId);
            }
        }
        voxel->nodes[nodeId] = {-1, -1, -1}; // set the node to -1
    }

    void inline Voxel_update_node(void *v, double x, double y, double z, int nodeId)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        size_t newIndex = coord_to_index(x, y, z, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        size_t oldIndex = coord_to_index(voxel->nodes[nodeId], voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        if (newIndex != oldIndex)
        {
            // remove the node from the old cell
            swap_and_pop(voxel->cells[oldIndex].nodes, nodeId);
            // remove the node from candidates list of all surfaces crossing the old cell
            for (auto surfId : voxel->cells[oldIndex].surfaces)
            {
                // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
                if (voxel->surfaceNodes[surfId][0] != nodeId && voxel->surfaceNodes[surfId][1] != nodeId &&
                    voxel->surfaceNodes[surfId][2] != nodeId && voxel->surfaceNodes[surfId][3] != nodeId)
                {
                    swap_and_pop(voxel->surfaceCandidates[surfId], nodeId);
                }
            }

            voxel->cells[newIndex].nodes.push_back(nodeId);

            voxel->nodes[nodeId] = index_to_coord(newIndex, voxel->nbx, voxel->nby, voxel->nbz);
            // loop over voxel->cells[newIndex].surfaces
            for (auto surfId : voxel->cells[newIndex].surfaces)
            {
                // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
                // check the 4 nodes
                if (voxel->surfaceNodes[surfId][0] != nodeId && voxel->surfaceNodes[surfId][1] != nodeId &&
                    voxel->surfaceNodes[surfId][2] != nodeId && voxel->surfaceNodes[surfId][3] != nodeId)
                {
                    // add the node to the surfaceCandiates of all surfaces crossing the new cell
                    voxel->surfaceCandidates[surfId].push_back(nodeId);
                }
            }
        }
    }

    // Helper function to process a range of cells for surface addition or removal
    void processCellRange(Voxel *voxel, int surfId,
                          short int xStart, short int xEnd,
                          short int yStart, short int yEnd,
                          short int zStart, short int zEnd,
                          bool isAddOperation)
    {
        for (short int x = xStart; x <= xEnd; x++)
        {
            for (short int y = yStart; y <= yEnd; y++)
            {
                for (short int z = zStart; z <= zEnd; z++)
                {
                    size_t index = coord_to_index(x, y, z, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);

                    if (isAddOperation)
                    {
                        // Add surface to cell
                        voxel->cells[index].surfaces.push_back(surfId);

                        // Update surfaceCandidates for new cells
                        for (auto nodeId : voxel->cells[index].nodes)
                        {
                            // Check if the node is not one of the four nodes defining the surface
                            if (voxel->surfaceNodes[surfId][0] != nodeId &&
                                voxel->surfaceNodes[surfId][1] != nodeId &&
                                voxel->surfaceNodes[surfId][2] != nodeId &&
                                voxel->surfaceNodes[surfId][3] != nodeId)
                            {
                                // Since we're adding to differential regions, the node shouldn't be in candidates yet
                                voxel->surfaceCandidates[surfId].push_back(nodeId);
                            }
                        }
                    }
                    else
                    {
                        // Remove surface from cell
                        if (voxel->cells[index].surfaces.size() > 0)
                        {
                            swap_and_pop(voxel->cells[index].surfaces, surfId);

                            // Remove nodes from candidates list
                            for (auto nodeId : voxel->cells[index].nodes)
                            {
                                // Check if the node is not one of the four nodes defining the surface
                                if (voxel->surfaceNodes[surfId][0] != nodeId &&
                                    voxel->surfaceNodes[surfId][1] != nodeId &&
                                    voxel->surfaceNodes[surfId][2] != nodeId &&
                                    voxel->surfaceNodes[surfId][3] != nodeId)
                                {
                                    swap_and_pop(voxel->surfaceCandidates[surfId], nodeId);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    void inline Voxel_delete_surf(void *v, int surfId)
    {
        Voxel *voxel = static_cast<Voxel *>(v);

        // Get the old bounds for cleaner code
        const Surf &oldCoords = voxel->surfaceBounds[surfId];

        // Calculate the range of cells to process
        short int xStart = oldCoords[XMIN];
        short int xEnd = oldCoords[XMAX];
        short int yStart = oldCoords[YMIN];
        short int yEnd = oldCoords[YMAX];
        short int zStart = oldCoords[ZMIN];
        short int zEnd = oldCoords[ZMAX];

        // if oldCoords are negative, the surface is already deleted
        if (xStart < 0 || yStart < 0 || zStart < 0)
        {
            return;
        }

        // Process the range of cells for surface removal
        processCellRange(voxel, surfId, xStart, xEnd, yStart, yEnd, zStart, zEnd, false);

        // Remove the surface from the list of surfaces
        voxel->surfaceBounds[surfId] = {-1, -1, -1, -1, -1, -1}; // Reset bounds to zero
    }

    void inline Voxel_update_surf(void *v, double xmin, double ymin, double zmin,
                                  double xmax, double ymax, double zmax, int surfId)
    {
        Voxel *voxel = static_cast<Voxel *>(v);

        // Early validation of surfId
        //        if (voxel->surfaceBounds.size() <= static_cast<size_t>(surfId))
        //        {
        //            std::cerr << "Error: surface id " << surfId << " does not exist" << std::endl;
        //            return;
        //        }

        // Calculate grid coordinates using mapping function
        GridMapper mapper(voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        Node minCoords = mapper.mapMin(xmin, ymin, zmin);
        Node maxCoords = mapper.mapMax(xmax, ymax, zmax);

        // Ensure coordinates are within valid grid range
        minCoords[0] = std::max(minCoords[0], static_cast<short int>(0));
        minCoords[1] = std::max(minCoords[1], static_cast<short int>(0));
        minCoords[2] = std::max(minCoords[2], static_cast<short int>(0));

        maxCoords[0] = std::min(maxCoords[0], static_cast<short int>(voxel->nbx - 1));
        maxCoords[1] = std::min(maxCoords[1], static_cast<short int>(voxel->nby - 1));
        maxCoords[2] = std::min(maxCoords[2], static_cast<short int>(voxel->nbz - 1));

        // Create the new surface bounds
        Surf newCoords;
        newCoords[XMIN] = minCoords[0];
        newCoords[YMIN] = minCoords[1];
        newCoords[ZMIN] = minCoords[2];
        newCoords[XMAX] = maxCoords[0];
        newCoords[YMAX] = maxCoords[1];
        newCoords[ZMAX] = maxCoords[2];

        // If no change in surface bounds, return early
        if (newCoords == voxel->surfaceBounds[surfId])
        {
            return; // No change, no need to update
        }

        // Get references to old bounds for cleaner code
        const Surf &oldCoords = voxel->surfaceBounds[surfId];

        // Calculate overlapping region
        const short int overlapXmin = std::max(minCoords[0], oldCoords[XMIN]);
        const short int overlapYmin = std::max(minCoords[1], oldCoords[YMIN]);
        const short int overlapZmin = std::max(minCoords[2], oldCoords[ZMIN]);
        const short int overlapXmax = std::min(maxCoords[0], oldCoords[XMAX]);
        const short int overlapYmax = std::min(maxCoords[1], oldCoords[YMAX]);
        const short int overlapZmax = std::min(maxCoords[2], oldCoords[ZMAX]);

        // Check if there's a valid overlap
        const bool hasOverlap = (overlapXmin <= overlapXmax) &&
                                (overlapYmin <= overlapYmax) &&
                                (overlapZmin <= overlapZmax);

        // Process regions - using a general assumption that there usually IS overlap
        if (!hasOverlap)
        {
            // No overlap - simpler case: remove all old cells, add all new cells
            processCellRange(voxel, surfId,
                             oldCoords[XMIN], oldCoords[XMAX],
                             oldCoords[YMIN], oldCoords[YMAX],
                             oldCoords[ZMIN], oldCoords[ZMAX], false);

            processCellRange(voxel, surfId,
                             minCoords[0], maxCoords[0],
                             minCoords[1], maxCoords[1],
                             minCoords[2], maxCoords[2], true);
        }
        else
        {
            // There is overlap - process the differential regions

            // Process regions to remove (old cells not in new bounds)

            // Left region of old bounds
            if (oldCoords[XMIN] < overlapXmin)
            {
                processCellRange(voxel, surfId,
                                 oldCoords[XMIN], overlapXmin - 1,
                                 oldCoords[YMIN], oldCoords[YMAX],
                                 oldCoords[ZMIN], oldCoords[ZMAX], false);
            }

            // Right region of old bounds
            if (oldCoords[XMAX] > overlapXmax)
            {
                processCellRange(voxel, surfId,
                                 overlapXmax + 1, oldCoords[XMAX],
                                 oldCoords[YMIN], oldCoords[YMAX],
                                 oldCoords[ZMIN], oldCoords[ZMAX], false);
            }

            // Process middle X slice, but non-overlapping Y slices
            // Bottom region
            if (oldCoords[YMIN] < overlapYmin)
            {
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 oldCoords[YMIN], overlapYmin - 1,
                                 oldCoords[ZMIN], oldCoords[ZMAX], false);
            }

            // Top region
            if (oldCoords[YMAX] > overlapYmax)
            {
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmax + 1, oldCoords[YMAX],
                                 oldCoords[ZMIN], oldCoords[ZMAX], false);
            }

            // Process middle X and Y slices, but non-overlapping Z slices
            // Front region
            if (oldCoords[ZMIN] < overlapZmin)
            {
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmin, overlapYmax,
                                 oldCoords[ZMIN], overlapZmin - 1, false);
            }

            // Back region
            if (oldCoords[ZMAX] > overlapZmax)
            {
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmin, overlapYmax,
                                 overlapZmax + 1, oldCoords[ZMAX], false);
            }

            // Process regions to add (new cells not in old bounds)

            // Left region of new bounds
            if (minCoords[0] < overlapXmin)
            {
                processCellRange(voxel, surfId,
                                 minCoords[0], overlapXmin - 1,
                                 minCoords[1], maxCoords[1],
                                 minCoords[2], maxCoords[2], true);
            }

            // Right region of new bounds
            if (maxCoords[0] > overlapXmax)
            {
                processCellRange(voxel, surfId,
                                 overlapXmax + 1, maxCoords[0],
                                 minCoords[1], maxCoords[1],
                                 minCoords[2], maxCoords[2], true);
            }

            // Process middle X slice, but non-overlapping Y slices
            // Bottom region
            if (minCoords[1] < overlapYmin)
            {
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 minCoords[1], overlapYmin - 1,
                                 minCoords[2], maxCoords[2], true);
            }

            // Top region
            if (maxCoords[1] > overlapYmax)
            {
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmax + 1, maxCoords[1],
                                 minCoords[2], maxCoords[2], true);
            }

            // Process middle X and Y slices, but non-overlapping Z slices
            // Front region
            if (minCoords[2] < overlapZmin)
            {
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmin, overlapYmax,
                                 minCoords[2], overlapZmin - 1, true);
            }

            // Back region
            if (maxCoords[2] > overlapZmax)
            {
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmin, overlapYmax,
                                 overlapZmax + 1, maxCoords[2], true);
            }
        }

        // Update the surface bounds
        voxel->surfaceBounds[surfId] = newCoords;
    }

    // update the surfaces, then the nodes
    void Voxel_update(void *v, int *irect, int nrtm, my_real *gap, int *nsv, int nsn, my_real *X, int numnod, my_real *stf, my_real *stfn)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        GridMapper mapper(voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        // Loop over the surfaces, add the surfaces to the cells it crosses
        for (int i = 0; i < nrtm; i++)
        {
            if (stf[i] <= static_cast<my_real>(0))
            {
                Voxel_delete_surf(v, i);
                continue;
            }
            // get the gap of the surface
            const double gapValue = static_cast<double>(gap[i]);
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
            double xmin = std::min(std::min(x1, x2), std::min(x3, x4)) - gapValue;
            double ymin = std::min(std::min(y1, y2), std::min(y3, y4)) - gapValue;
            double zmin = std::min(std::min(z1, z2), std::min(z3, z4)) - gapValue;
            double xmax = std::max(std::max(x1, x2), std::max(x3, x4)) + gapValue;
            double ymax = std::max(std::max(y1, y2), std::max(y3, y4)) + gapValue;
            double zmax = std::max(std::max(z1, z2), std::max(z3, z4)) + gapValue;
            Voxel_update_surf(v, xmin, ymin, zmin, xmax, ymax, zmax, i);
        }

        for (int i = 0; i < nsn; ++i)
        {
            if (stfn[i] <= static_cast<my_real>(0))
            {
                Voxel_delete_node(v, i);
                continue;
            }
            // get the coordinates of the node
            const size_t i1 = nsv[i] - 1; // Fortran to C++ index conversion
            const double x = static_cast<double>(X[3 * i1]);
            const double y = static_cast<double>(X[3 * i1 + 1]);
            const double z = static_cast<double>(X[3 * i1 + 2]);
            Voxel_update_node(v, x, y, z, i);
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
    // Copyless version
    void Voxel_get_candidates_data(void *v, int ne, int **cands, int *nb)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        *nb = static_cast<int>(voxel->surfaceCandidates[ne - 1].size());
        *cands = voxel->surfaceCandidates[ne - 1].data();
    }
}
#include "voxel.h"

#define DEBUG_VOXEL 1
#define DEBUG_SURF 0
#define DEBUG_NODE 9705

#ifdef MYREAL8
#define my_real double
#else
#define my_real float
#endif

extern "C"
{
    void *Voxel_new(int nbx, int nby, int nbz, int nbsurfaces, int nbnodes, int nbGlobalNodes)
    {
        Voxel *v = new Voxel();
        v->nbx = nbx;
        v->nby = nby;
        v->nbz = nbz;
        v->nsnGlob = nbGlobalNodes;
        // allocate the vector of nodes
        v->nodes.resize(nbnodes);
        // fill with
        // there are nbx*nby*nbz cells
        v->cells.resize(nbx * nby * nbz);
        // allocate the vector of surfaces
        v->surfaceBounds.resize(nbsurfaces);
        v->surfaceNodes.resize(nbsurfaces);
        v->surfaceCandidates.resize(nbsurfaces);
        v->nodesRemote.resize(nbGlobalNodes);
        v->surfaceCandidatesRemote.resize(nbsurfaces);
        v->globalToIREM.resize(nbGlobalNodes);
        for (int i = 0; i < nbGlobalNodes; ++i)
        {
            v->nodesRemote[i] = DEAD;
            v->globalToIREM[i] = DEAD;
            //std::cout<<"globalToIREM["<<i<<"] = "<<v->globalToIREM[i]<<std::endl;
        }
 
        return v;
    }

    void Voxel_restart(void *v, int nbx, int nby, int nbz, int nbsurfaces, int nbnodes, int nbGlobalNodes)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        voxel->nbx = nbx;
        voxel->nby = nby;
        voxel->nbz = nbz;
        voxel->nsnGlob = nbGlobalNodes;
        voxel->cells.clear();
        voxel->cells.resize(nbx * nby * nbz);
        voxel->surfaceBounds.clear();
        voxel->surfaceNodes.clear();
        voxel->surfaceCandidates.clear();
        voxel->nodesRemote.clear();
        voxel->surfaceCandidatesRemote.clear();
        voxel->nodes.resize(nbnodes);
        voxel->surfaceBounds.resize(nbsurfaces);
        voxel->surfaceNodes.resize(nbsurfaces);
        voxel->surfaceCandidates.resize(nbsurfaces);
        voxel->nodesRemote.resize(nbGlobalNodes);
        voxel->globalToIREM.clear();
        voxel->globalToIREM.resize(nbGlobalNodes);
        // fill with remote nodes with DEAD
        for (int i = 0; i < nbGlobalNodes; ++i)
        {
            voxel->nodesRemote[i] = DEAD;
            voxel->globalToIREM[i] = DEAD;
            //std::cout<<"globalToIREM["<<i<<"] = "<<voxel->globalToIREM[i]<<std::endl;
        }
        voxel->surfaceCandidatesRemote.resize(nbsurfaces);
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

    void Voxel_initialize(void *v, int *irect, int nrtm, my_real *gap, int *nsv, int nsn, my_real *X, int numnod, my_real *stf, my_real *stfn, int *IREM, my_real *XREM, int RSIZ, int ISIZ, int NSNR)
    {
        // X = coordinates of the nodes, size 3*numnod
        // nrtm = number of surfaces
        // irect(1:4, 1:nrtm) = global id of the 4 nodes of the surface
        // nsv(i) = global id of the node i
        // gap(1:nrtm) = gap of the surface, i.e. the distance beyond the surface to be considered
        Voxel *voxel = static_cast<Voxel *>(v);
        GridMapper mapper(voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);

        // create a vecor of size nbx*nby*nbz
        std::vector<int> count_surf_per_cell(voxel->nbx * voxel->nby * voxel->nbz, 0);

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

            for (short int ii = minCoords[0]; ii <= maxCoords[0]; ++ii)
            {
                for (short int jj = minCoords[1]; jj <= maxCoords[1]; ++jj)
                {
                    for (short int kk = minCoords[2]; kk <= maxCoords[2]; ++kk)
                    {
                        size_t index = COORD_TO_INDEX(ii, jj, kk, voxel->nbx, voxel->nby);
                        // voxel->cells[index].surfaces.push_back(i);
                        count_surf_per_cell[index]++;
                    }
                }
            }

            voxel->surfaceBounds[i] = {
                minCoords[0], // XMIN
                minCoords[1], // YMIN
                minCoords[2], // ZMIN
                maxCoords[0], // XMAX
                maxCoords[1], // YMAX
                maxCoords[2]  // ZMAX
            };
        }

        // Loop over the cells, reserve space for the surfaces
        for (size_t i = 0; i < voxel->cells.size(); ++i)
        {
            // reserve space for the surfaces
            if (count_surf_per_cell[i] > 0)
            {
                voxel->cells[i].surfaces.reserve(count_surf_per_cell[i]);
                voxel->cells[i].nodes.reserve(4);
            }
        }

        // now fill the cells
        for (int i = 0; i < nrtm; ++i)
        {
            if (stf[i] <= static_cast<my_real>(0))
            {
                continue;
            }
            const Surf &bounds = voxel->surfaceBounds[i];
            for (short int ii = bounds[XMIN]; ii <= bounds[XMAX]; ++ii)
            {
                for (short int jj = bounds[YMIN]; jj <= bounds[YMAX]; ++jj)
                {
                    for (short int kk = bounds[ZMIN]; kk <= bounds[ZMAX]; ++kk)
                    {
                        size_t index = COORD_TO_INDEX(ii, jj, kk, voxel->nbx, voxel->nby);
#ifdef DEBUG_SURF
                        if(i == DEBUG_SURF)
                        {
                            std::cout<<"Add surface "<<i<<" to cell "<<index<<std::endl;
                            std::cout<<"Cell coord "<<ii<<" "<<jj<<" "<<kk<<std::endl;
                            std::cout<<"Cell index "<<index<<std::endl;
                        }
#endif
                        voxel->cells[index].surfaces.push_back(i);
                    }
                }
            }
        }
        // reserve 4 candidate per surface
        for (int i = 0; i < nrtm; ++i)
        {
            if (stf[i] <= static_cast<my_real>(0))
            {
                continue;
            }
            voxel->surfaceCandidates[i].reserve(4);
        }

        // Loop over the local nodes, add the nodes to the cells they belong to
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
            // size_t index = coord_to_index(x, y, z, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
            const size_t index = mapper.toIndex(x, y, z);
            voxel->cells[index].nodes.push_back(i);
            // add the node to the nodes vector
            voxel->nodes[i] = mapper.mapToIndex(x, y, z);


#ifdef DEBUG_NODE
              if(i == DEBUG_NODE)
              {
                  std::cout<<"mapper.toIndex(x, y, z) "<<mapper.toIndex(x, y, z)<<std::endl;
                  std::cout<<"mapToIndex(x, y, z) "<<mapper.mapToIndex(x, y, z)<<std::endl;
                  // print the coordinates of the node
                    std::cout<<"NODE "<<i<<" coord "<<x<<" "<<y<<" "<<z<<std::endl;

                  // convert back index to Node
                   Node c1 = index_to_coord(mapper.toIndex(x,y,z), voxel->nbx, voxel->nby, voxel->nbz);
                   Node c2 = index_to_coord(mapper.mapToIndex(x,y,z), voxel->nbx, voxel->nby, voxel->nbz);
                    std::cout<<"NODE toIndex "<<i<<" coord "<<c1[0]<<" "<<c1[1]<<" "<<c1[2]<<std::endl;
                    std::cout<<"NODE mapToIndex"<<i<<" coord "<<c2[0]<<" "<<c2[1]<<" "<<c2[2]<<std::endl;
                  std::cout<<"NODE "<<i<<" cell index "<<index<<std::endl;
                  std::cout<< "NODE "<<i<<" toIndex "<<mapper.toIndex(x, y, z)<<std::endl;                                       
                  auto Node = index_to_coord(voxel->nodes[i], voxel->nbx, voxel->nby, voxel->nbz);
                  std::cout<<"NODE "<<i<<" cell coord "<<Node[0]<<" "<<Node[1]<<" "<<Node[2]<<std::endl;
                  std::cout<<"NODE "<<i<<" cell index "<< COORD_TO_INDEX(Node[0], Node[1], Node[2], voxel->nbx, voxel->nby)<<std::endl;
                  std::cout<<"Global id "<<nsv[i]<<" local id "<<i<<std::endl;
                  std::cout<<"Number of surf in that cell "<<voxel->cells[index].surfaces.size()<<std::endl;
                  //print bounds of the domain
                    std::cout<<"bounds "<<voxel->bounds[XMIN]<<" "<<voxel->bounds[YMIN]<<" "<<voxel->bounds[ZMIN]<<" "
                             <<voxel->bounds[XMAX]<<" "<<voxel->bounds[YMAX]<<" "<<voxel->bounds[ZMAX]<<std::endl;
              }
#endif

            // loop over the surfaces crossing the cell
            for (auto surfId : voxel->cells[index].surfaces)
            {
                // check the 4 nodes
#ifdef DEBUG_NODE
                if( i == DEBUG_NODE)
                {
                    std::cout<<"NODE "<<i<<" in surface "<<surfId<<std::endl;
                }
#endif
                if (voxel->surfaceNodes[surfId][0] != i && voxel->surfaceNodes[surfId][1] != i &&
                    voxel->surfaceNodes[surfId][2] != i && voxel->surfaceNodes[surfId][3] != i)
                {
#ifdef DEBUG_NODE
                    if( i == DEBUG_NODE)
                    {
                        std::cout<<"Add node "<<i<<" to surface "<<surfId<<" candidates"<<std::endl;                                                         
                    }
#endif
                    // add the node to the surfaceCandiates
                    voxel->surfaceCandidates[surfId].push_back(i);
                }
            }
        }
        // remote nodes
        constexpr size_t idGlob = 3 ; // position in IREM of the global id in 1:nsnGlob
        constexpr size_t idLoc = 0 ; // position in IREM of the id internal to the remote process
        constexpr size_t idX = 0 ; // position in XREM of the x coordinate
        constexpr size_t idY = 1 ; // position in XREM of the y coordinate
        constexpr size_t idZ = 2 ; // position in XREM of the z coordinate
        voxel->nsnr= NSNR;
        for(int i = 0; i < NSNR; ++i)
        {
            // get the global id of the node
            int globId = IREM[i * ISIZ + idGlob] - 1; // Fortran to C++ index conversion
            // get the local id of the node
            int locId = IREM[i * ISIZ + idLoc] - 1; // Fortran to C++ index conversion
            // get the coordinates of the node
            double x = static_cast<double>(XREM[i * RSIZ + idX]);
            double y = static_cast<double>(XREM[i * RSIZ + idY]);
            double z = static_cast<double>(XREM[i * RSIZ + idZ]);
            bool isAlive = voxel->isInDomain(x, y, z);                                     
            if(isAlive)
            {
                voxel->globalToIREM[globId] = i; // map the global id to the local id
                //std::cout<<"REMOTE NODE "<<i<<" global id "<<globId<<std::endl;
                voxel->nodesRemote[globId] = mapper.mapToIndex(x, y, z); // map the global id to the local id
                const size_t index = mapper.toIndex(x, y, z); // get the index of the cell
                voxel->cells[index].nodesRemote.push_back(globId); // add the remote node to the cell

                for(auto surfId : voxel->cells[index].surfaces)
                {
                    //write an error message if globId is already in the surfaceCandidates
#ifdef DEBUG_VOXEL
                      auto it = std::find(voxel->surfaceCandidatesRemote[surfId].begin(), voxel->surfaceCandidatesRemote[surfId].end(), globId);
                      if(it != voxel->surfaceCandidatesRemote[surfId].end())
                      {
                          std::cout<<"Error: Initialize NodeRemote "<<globId<<" already in surface candidates list for surface "<<surfId<<std::endl;
                          std::cout<<"x= "<<x<<" y=" <<y<<" z= "<<z<<std::endl;
                          std::cout<<"Node coordinates "<<x<<" "<<y<<" "<<z<<std::endl;
                          std::cout<<"Cell index "<<index<<std::endl;
                          std::cout<<"Surface bounds "<<voxel->surfaceBounds[surfId][XMIN]<<" "<<voxel->surfaceBounds[surfId][YMIN]<<" "<<voxel->surfaceBounds[surfId][ZMIN]<<" "
                                   <<voxel->surfaceBounds[surfId][XMAX]<<" "<<voxel->surfaceBounds[surfId][YMAX]<<" "<<voxel->surfaceBounds[surfId][ZMAX]<<std::endl;
                          std::abort();
                      }
#endif

                    // add the node to the surfaceCandiates
                    //std::cout<<"Add remote node "<<globId<<" to surface "<<surfId<<" candidates"<<std::endl;
                    voxel->surfaceCandidatesRemote[surfId].push_back(globId);
#ifdef DEBUG_SURF_REMOTE
                      if(surfId == DEBUG_SURF)
                      {
                          std::cout<<"Add remote node "<<globId<<" to surface "<<surfId<<" candidates"<<std::endl;
                          std::cout<<"Local id "<<locId<<std::endl;
                      }
#endif
                }
            }
            else
            {
                //std::cout<<"DEAD REMOTE NODE "<<i<<" global id "<<globId<<std::endl;
//                if(globId == 8) 
//                {
//                    std::cout<<"DEAD REMOTE NODE "<<i<<" global id "<<globId<<std::endl;
//                    std::cout<<"x= "<<x<<" y=" <<y<<" z= "<<z<<std::endl;
//                    std::cout<<"Node coordinates "<<x<<" "<<y<<" "<<z<<std::endl;
//                }
                voxel->nodesRemote[globId] = DEAD;
                voxel->globalToIREM[globId] = DEAD; // map the global id to the local id
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
        if (voxel->nodes[nodeId] == DEAD)
        {
            return;
        }
        size_t index = voxel->nodes[nodeId];
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
        voxel->nodes[nodeId] = DEAD;
    }

    void inline Voxel_delete_node_remote(void *v, int nodeId)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        if (voxel->nodesRemote[nodeId] == DEAD)
        {
            return;
        }
        size_t index = voxel->nodesRemote[nodeId];
        // remove the node from the cell
        swap_and_pop(voxel->cells[index].nodesRemote, nodeId);
        // remove the node from candidates list of all surfaces crossing the cell
        for (auto surfId : voxel->cells[index].surfaces)
        {
            // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
            //std::cout<<" delete node "<<nodeId<<" from surface "<<surfId<<std::endl;
//            if(nodeId == 483) 
//            {
//                std::cout<<" delete node "<<nodeId<<" from surface "<<surfId<<" in cell"<<index<<std::endl;
//            }
            swap_and_pop(voxel->surfaceCandidatesRemote[surfId], nodeId);
        }
        voxel->nodesRemote[nodeId] = DEAD;
    }

    void inline Voxel_update_node(void *v, int nodeId, const GridMapper &mapper)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        const size_t oldIndex = voxel->nodesOld[nodeId];
        const size_t newIndex = voxel->nodes[nodeId];                        
        const Node oldCoord = index_to_coord(oldIndex, voxel->nbx, voxel->nby, voxel->nbz);
        const Node newCoord = index_to_coord(newIndex, voxel->nbx, voxel->nby, voxel->nbz);

        if (newIndex != oldIndex)
        {
 //           if(nodeId == 483)
 //           {
 //               std::cout<<"NODE UPDATE old index "<<oldIndex<<" new index "<<newIndex<<std::endl;
 //               std::cout<<"old coords "<<oldCoord[0]<<" "<<oldCoord[1]<<" "<<oldCoord[2]<<std::endl;
 //               std::cout<<"new coords "<<newCoord[0]<<" "<<newCoord[1]<<" "<<newCoord[2]<<std::endl;
 //           }
 //           // remove the node from the old cell
            swap_and_pop(voxel->cells[oldIndex].nodes, nodeId);
            // remove the node from candidates list of all surfaces crossing the old cell
            for (auto surfId : voxel->cells[oldIndex].surfaces)
            {
                // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
                if (voxel->surfaceNodes[surfId][0] != nodeId && voxel->surfaceNodes[surfId][1] != nodeId &&
                    voxel->surfaceNodes[surfId][2] != nodeId && voxel->surfaceNodes[surfId][3] != nodeId)
                {
                    if(!is_in_bounds(newCoord, voxel->surfaceBounds[surfId]) && is_in_bounds(oldCoord, voxel->surfaceBoundsOld[surfId]))
                    { // if the node moved out of the new surface bounds, then remove it from the candidates list
                        swap_and_pop(voxel->surfaceCandidates[surfId], nodeId);
 //                       if(surfId == 869 && nodeId == 483)
 //                       {
 //                           std::cout<<"NODE UPDATE swap and pop "<<nodeId<<" to surface "<<surfId<<" in cell "<<oldIndex<<std::endl;
 //                       }
                    }
                }
            }

            voxel->cells[newIndex].nodes.push_back(nodeId);
//            if(nodeId == 483)
//            {
//                std::cout<<"NODE UPDATE add node "<<nodeId<<"  to cell "<<newIndex<<std::endl;
//                std::cout<<"new coords "<<newCoord[0]<<" "<<newCoord[1]<<" "<<newCoord[2]<<std::endl;
//            }
            voxel->nodes[nodeId] = newIndex;
            // loop over voxel->cells[newIndex].surfaces
            size_t pos = 0;
            for (auto surfId : voxel->cells[newIndex].surfaces)
            {
                ++pos;
 //               if(surfId == 869)
 //               {
 //                   std::cout<<"SURF 869 found in cell "<<newIndex<<" at position "<<pos<<std::endl;

 //               }
                // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
                // check the 4 nodes
                if (voxel->surfaceNodes[surfId][0] != nodeId && voxel->surfaceNodes[surfId][1] != nodeId &&
                    voxel->surfaceNodes[surfId][2] != nodeId && voxel->surfaceNodes[surfId][3] != nodeId)
                {
                    // add the node to the surfaceCandiates of all surfaces crossing the new cell
                    if(!is_in_bounds(oldCoord, voxel->surfaceBoundsOld[surfId]) && is_in_bounds(newCoord, voxel->surfaceBounds[surfId]))
                    { // if the node moved into the new surface bounds, then add it to the candidates list
                        // find nodeId in the list, print an error if is already found
//                        auto it = std::find(voxel->surfaceCandidates[surfId].begin(), voxel->surfaceCandidates[surfId].end(), nodeId);
//                        if (it != voxel->surfaceCandidates[surfId].end())
//                        {
//                            std::cout <<pos<<" Error: Node " << nodeId << " already in surface candidates list for surface " << surfId << std::endl;
//                            // old node coordinate
//                            std::cout << "Old Node: " << oldCoord[0] << " " << oldCoord[1] << " " << oldCoord[2] << "cell idex " << oldIndex << std::endl;
//                            // new node coordinate
//                            std::cout << "New Node: " << newCoord[0] << " " << newCoord[1] << " " << newCoord[2] << "cell idex " << newIndex << std::endl;
//                            // display old bounds of surface, and new bounds of surface
//                            const Surf &boundsOld = voxel->surfaceBoundsOld[surfId];
//                            const Surf &boundsNew = voxel->surfaceBounds[surfId];
//                            std::cout << "Old Bounds: " << boundsOld[XMIN] << " " << boundsOld[YMIN] << " " << boundsOld[ZMIN] << " "
//                                      << boundsOld[XMAX] << " " << boundsOld[YMAX] << " " << boundsOld[ZMAX] << std::endl;
//                            std::cout << "New Bounds: " << boundsNew[XMIN] << " " << boundsNew[YMIN] << " " << boundsNew[ZMIN] << " "
//                                        << boundsNew[XMAX] << " " << boundsNew[YMAX] << " " << boundsNew[ZMAX] << std::endl;
//                            std::abort();
//                        }
//                    if(surfId == 869 && nodeId == 483)
//                    {
//                        std::cout<<pos<<" NODE UPDATE push back "<<nodeId<<" to surface "<<surfId<<" in cell "<<newIndex<<std::endl;
//                    }
                        voxel->surfaceCandidates[surfId].push_back(nodeId);
                    }
                }
            }
        }
    }
    bool inline Voxel_update_node_remote(void *v, double x, double y, double z, int nodeId, const GridMapper &mapper)
    {
        Voxel *voxel = static_cast<Voxel *>(v);

        const size_t oldIndex = voxel->nodesRemoteOld[nodeId];
        size_t newIndex;                             
        // if in bounds, use mapper.toIndex, else use DEAD
        if (voxel->isInDomain(x, y, z))
        {
            newIndex = mapper.toIndex(x, y, z);
        }
        else
        {
            newIndex = DEAD;
        }

//        if(nodeId == 170) 
//        {
//            std::cout<<"NODE UPDATE remote old index "<<oldIndex<<" new index "<<newIndex<<std::endl;
//            std::cout<<"old coords "<<x<<" "<<y<<" "<<z<<std::endl;
//            auto Node = index_to_coord(voxel->nodesRemote[nodeId], voxel->nbx, voxel->nby, voxel->nbz);
//            std::cout<<"NODE "<<nodeId<<" cell coord "<<Node[0]<<" "<<Node[1]<<" "<<Node[2]<<std::endl;
//            std::cout<<"NODE "<<nodeId<<" cell index "<< COORD_TO_INDEX(Node[0], Node[1], Node[2], voxel->nbx, voxel->nby)<<std::endl;
//        }
//
        if (newIndex != oldIndex)
        {
            if (oldIndex != DEAD)
            {
                // remove the node from the old cell
                swap_and_pop(voxel->cells[oldIndex].nodesRemote, nodeId);
                // remove the node from candidates list of all surfaces crossing the old cell
                for (auto surfId : voxel->cells[oldIndex].surfaces)
                {
//                    if(surfId == 0)
//                    {
//                        std::cout<<"NODE UPDATE remote swap and pop "<<nodeId<<" to surface "<<surfId<<" in cell "<<oldIndex<<std::endl;
//                    }
                    swap_and_pop(voxel->surfaceCandidatesRemote[surfId], nodeId);
                }
            }

            voxel->nodesRemote[nodeId] = newIndex;
//            if(nodeId == 170) 
//            {
//                std::cout<<"NODE UPDATE "<<nodeId<<"  newIndex = "<<newIndex<<std::endl;
//            }
            if(newIndex != DEAD)
            {
                const size_t surfaceCount = voxel->cells[newIndex].surfaces.size();
                if (surfaceCount > 0)
                {
                    voxel->nodesRemote[nodeId] = newIndex;
//                    if(nodeId == 170) 
//                    {
//                        std::cout<<__LINE__<<" NODE "<<nodeId<<" cell index "<<newIndex<<std::endl;
//                        std::cout<<__LINE__<<" NODE "<<nodeId<<" cell coord "<<x<<" "<<y<<" "<<z<<std::endl;
//                        std::cout<<__LINE__<<" NODE "<<nodeId<<" cell index "<< COORD_TO_INDEX(x, y, z, voxel->nbx, voxel->nby)<<std::endl;
//                    }
                }
                else
                {
//                    voxel->nodesRemote[nodeId] = DEAD;
//
//            if(nodeId == 170) 
//            {
//                std::cout<<"NODE KILLED BECAUSE OF EMPTY CELL "<<nodeId<<"  newIndex = "<<voxel->nodesRemote[nodeId]<<std::endl;
//            }
 
                }

            }

            if (newIndex != DEAD)
            {
                // add the node to the cell, only if it contains at least one surface
                voxel->cells[newIndex].nodesRemote.push_back(nodeId);
                // loop over voxel->cells[newIndex].surfaces
                for (auto surfId : voxel->cells[newIndex].surfaces)
                {
                    // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
                    // check the 4 nodes
                    // add the node to the surfaceCandiates of all surfaces crossing the new cell
//                    if(nodeId == 1) 
//                    {
//                        std::cout<<" add node "<<nodeId<<" to surface "<<surfId<<" in cell "<<newIndex<<std::endl;
//                    }

                    auto oldCoord = index_to_coord(oldIndex, voxel->nbx, voxel->nby, voxel->nbz);
                    auto newCoord = index_to_coord(newIndex, voxel->nbx, voxel->nby, voxel->nbz);
                    
//                    if(!is_in_bounds(oldCoord, voxel->surfaceBoundsOld[surfId]) && is_in_bounds(newCoord, voxel->surfaceBounds[surfId]))
                    {
#ifdef DEBUG_VOXEL
                        // print an error message if nodeId is already in the surfaceCandidates
                          auto it = std::find(voxel->surfaceCandidatesRemote[surfId].begin(), voxel->surfaceCandidatesRemote[surfId].end(), nodeId);
                          if (it != voxel->surfaceCandidatesRemote[surfId].end())
                          {
                              std::cout <<"Error: update NodeRemote " << nodeId << " already in surface candidates list for surface " << surfId << std::endl;
                              std::cout<<"Node coordinates "<<x<<" "<<y<<" "<<z<<std::endl;
                              std::cout<<"oldCoord "<<oldCoord[0]<<" "<<oldCoord[1]<<" "<<oldCoord[2]<<" oldIndex "<<oldIndex<<std::endl;
                              std::cout<<"newCoord "<<newCoord[0]<<" "<<newCoord[1]<<" "<<newCoord[2]<<" newIndex "<<newIndex<<std::endl;
                              std::cout<<"Cell index "<<newIndex<<std::endl;
                              std::cout<<"voxel->nodesRemote["<<nodeId<<"] "<<voxel->nodesRemote[nodeId]<<std::endl;
                              std::cout<<"voxel->nodesRemoteOld["<<nodeId<<"] "<<voxel->nodesRemoteOld[nodeId]<<std::endl;
                              std::cout<<"Surface bounds "<<voxel->surfaceBounds[surfId][XMIN]<<" "<<voxel->surfaceBounds[surfId][YMIN]<<" "<<voxel->surfaceBounds[surfId][ZMIN]<<" "
                                       <<voxel->surfaceBounds[surfId][XMAX]<<" "<<voxel->surfaceBounds[surfId][YMAX]<<" "<<voxel->surfaceBounds[surfId][ZMAX]<<std::endl;
                              std::abort();
                          }
#endif
                        voxel->surfaceCandidatesRemote[surfId].push_back(nodeId);
                   }
                }
            }
        }


//        if(nodeId == 170) 
//        {
//            std::cout<<__LINE__<< "NODE END UPDATE remote "<<nodeId<<"  newIndex="<<voxel->nodesRemote[nodeId]<<std::endl;
//        }
        return newIndex != DEAD; // node is in the domain 
    }

    // Helper function to process a range of cells for surface addition or removal
    void inline processCellRange(Voxel *voxel, int surfId,
                                 const short int &xStart, const short int &xEnd,
                                 const short int &yStart, const short int &yEnd,
                                 const short int &zStart, const short int &zEnd,
                                 const bool &isAddOperation)
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
//                            auto it = std::find(voxel->cells[index].surfaces.begin(), voxel->cells[index].surfaces.end(), surfId);
//                            if(it != voxel->cells[index].surfaces.end())
//                            {
//                                std::cout<<"SURF ADD Error: surface "<<surfId<<" already in cell "<<index<<std::endl;
//                                std::abort();
//                            }
//

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
                                // A node may alreay be in the list, if it moved along with the surface
                                // in that case, the old node coordinate was in the old surface bounds
                                const size_t oldIndex = voxel->nodesOld[nodeId];
                                const size_t newIndex = voxel->nodes[nodeId];
                                const Node nodeCoordOld = index_to_coord(oldIndex, voxel->nbx, voxel->nby, voxel->nbz); 
                                if(!is_in_bounds(nodeCoordOld, voxel->surfaceBoundsOld[surfId]) && oldIndex == newIndex)
                                {                                                                                       
                                  // find nodeId in the list, print an error if is already found
 //                                   auto it = std::find(voxel->surfaceCandidates[surfId].begin(), voxel->surfaceCandidates[surfId].end(), nodeId);
 //                                   if (it != voxel->surfaceCandidates[surfId].end())
 //                                   {
 //                                       std::cout << "Error: Node " << nodeId << " already in surface candidates list for surface " << surfId << std::endl;
 //                                       std::cout<<"oldIndex "<<voxel->nodesOld[nodeId]<<" newIndex "<<voxel->nodes[nodeId]<<std::endl;                                                                                            
 //                                       // stop the program
 //                                       std::abort();

 //                                   }

//                    if(surfId == 869 && nodeId == 483)
//                    {
//                        std::cout<<"SURF UPDATE push back "<<nodeId<<" to surface "<<surfId<<" in cell "<<index<<std::endl;
//                        std::cout<<" coordinates "<<x<<" "<<y<<" "<<z<<std::endl;
//                    }
//
                                    voxel->surfaceCandidates[surfId].push_back(nodeId);
                                }
                            }
                        }
                        for(auto nodeId : voxel->cells[index].nodesRemote)
                        {
                            // A node may alreay be in the list, if it moved along with the surface
                            // in that case, the old node coordinate was in the old surface bounds
                            const size_t oldIndex = voxel->nodesRemoteOld[nodeId];
                            const size_t newIndex = voxel->nodesRemote[nodeId];
                            const Node nodeCoordOld = index_to_coord(oldIndex, voxel->nbx, voxel->nby, voxel->nbz); 
//                            if(!is_in_bounds(nodeCoordOld, voxel->surfaceBoundsOld[surfId]) && oldIndex == newIndex)
                            {                                                                                       
                                // find nodeId in the list, print an error if is already found
//                                auto it = std::find(voxel->surfaceCandidatesRemote[surfId].begin(), voxel->surfaceCandidatesRemote[surfId].end(), nodeId);
//                                if(it != voxel->surfaceCandidatesRemote[surfId].end())
//                                {
//                                    std::cout << "Process NodeRemote " << nodeId << " already in surface candidates list for surface " << surfId << std::endl;
//                                    std::cout<<"oldIndex "<<voxel->nodesRemoteOld[nodeId]<<" newIndex "<<voxel->nodesRemote[nodeId]<<std::endl;                                                                                            
//                                    // stop the program
//                                    std::abort();
//
//                                }
                                voxel->surfaceCandidatesRemote[surfId].push_back(nodeId);
                            }
                        }
                    }
                    else
                    {
                        // Remove surface from candidate list
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
                                    // we remove it only if the new coordinate is not in the new surface bounds
                                    const size_t newIndex = voxel->nodes[nodeId];
                                    const size_t oldIndex = voxel->nodesOld[nodeId];
                                    const Node nodeCoordNew = index_to_coord(newIndex, voxel->nbx, voxel->nby, voxel->nbz);
                                    const Node nodeCoordOld = index_to_coord(oldIndex, voxel->nbx, voxel->nby, voxel->nbz);
                                    if(is_in_bounds(nodeCoordOld, voxel->surfaceBoundsOld[surfId]) && !is_in_bounds(nodeCoordNew, voxel->surfaceBounds[surfId]))
                                    {  // Remove if the node was in the old surface bounds, but not in the new surface bounds 
                                        swap_and_pop(voxel->surfaceCandidates[surfId], nodeId);
                                    }
                                }
                            }

                            for(auto nodeId : voxel->cells[index].nodesRemote)
                            {
                                // we remove it only if the new coordinate is not in the new surface bounds
                                const size_t newIndex = voxel->nodesRemote[nodeId];
                                const size_t oldIndex = voxel->nodesRemoteOld[nodeId];
                                const Node nodeCoordNew = index_to_coord(newIndex, voxel->nbx, voxel->nby, voxel->nbz);
                                const Node nodeCoordOld = index_to_coord(oldIndex, voxel->nbx, voxel->nby, voxel->nbz);
 //                               if(is_in_bounds(nodeCoordOld, voxel->surfaceBoundsOld[surfId]) && !is_in_bounds(nodeCoordNew, voxel->surfaceBounds[surfId]))
                                {  // Remove if the node was in the old surface bounds, but not in the new surface bounds 

//                                    if(surfId == 0)
//                                    {
//                                        std::cout<<"PROCESS SURF DELETE remove node "<<nodeId<<" from surface "<<surfId<<" in cell "<<index<<std::endl;
//                                    }
//                                    swap_and_pop(voxel->surfaceCandidatesRemote[surfId], nodeId);
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


    void inline Voxel_update_surf(void *v,  int surfId, const GridMapper &mapper)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        const Surf &newCoords = voxel->surfaceBounds[surfId];
        const Surf &oldCoords = voxel->surfaceBoundsOld[surfId];


        // If no change in surface bounds, return early
        if (voxel->surfaceBoundsOld[surfId] == voxel->surfaceBounds[surfId])
        {
            return; // No change, no need to update
        }

        // Get references to old bounds for cleaner code
  //     if(surfId == 869)
  //      {
  //          std::cout<<"INFO SURF UPDATE old surface "<<surfId<<std::endl;
  //          std::cout<<"INFO oldCoords min "<<oldCoords[XMIN]<<" "<<oldCoords[YMIN]<<" "<<oldCoords[ZMIN]<<std::endl;                                                                                            
  //          std::cout<<"INFO oldCoords max "<<oldCoords[XMAX]<<" "<<oldCoords[YMAX]<<" "<<oldCoords[ZMAX]<<std::endl;
  //          std::cout<<"INFO newCoords min "<<newCoords[XMIN]<<" "<<newCoords[YMIN]<<" "<<newCoords[ZMIN]<<std::endl;
  //          std::cout<<"INFO newCoords max "<<newCoords[XMAX]<<" "<<newCoords[YMAX]<<" "<<newCoords[ZMAX]<<std::endl;
  //      }

        // Calculate overlapping region
        const short int overlapXmin = std::max(newCoords[XMIN], oldCoords[XMIN]);
        const short int overlapYmin = std::max(newCoords[YMIN], oldCoords[YMIN]);
        const short int overlapZmin = std::max(newCoords[ZMIN], oldCoords[ZMIN]);
        const short int overlapXmax = std::min(newCoords[XMAX], oldCoords[XMAX]);
        const short int overlapYmax = std::min(newCoords[YMAX], oldCoords[YMAX]);
        const short int overlapZmax = std::min(newCoords[ZMAX], oldCoords[ZMAX]);

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
                             newCoords[XMIN], newCoords[XMAX],
                             newCoords[YMIN], newCoords[YMAX],
                             newCoords[ZMIN], newCoords[ZMAX], true);
        }
        else
        {
            // There is overlap - process the differential regions

            // Process regions to remove (old cells not in new bounds)

            // Left region of old bounds
            if (oldCoords[XMIN] < overlapXmin)
            {
//                if(surfId == 869)
//                {
//                    std::cout<<"SURF REMOVE left region "<<surfId<<std::endl;
//                    std::cout<<"X "<<oldCoords[XMIN]<<" to "<<overlapXmin - 1<<std::endl;
//                    std::cout<<"Y "<<oldCoords[YMIN]<<" to "<<oldCoords[YMAX]<<std::endl;   
//                    std::cout<<"Z "<<oldCoords[ZMIN]<<" to "<<oldCoords[ZMAX]<<std::endl;
//
//                }
                processCellRange(voxel, surfId,
                                 oldCoords[XMIN], overlapXmin - 1,
                                 oldCoords[YMIN], oldCoords[YMAX],
                                 oldCoords[ZMIN], oldCoords[ZMAX], false);
            }

            // Right region of old bounds
            if (oldCoords[XMAX] > overlapXmax)
            {
 //               if(surfId == 869)
 //               {
 //                   std::cout<<"SURF REMOVE right region "<<surfId<<std::endl;
 //                   std::cout<<"X "<<overlapXmax + 1<<" to "<<oldCoords[XMAX]<<std::endl;
 //                   std::cout<<"Y "<<oldCoords[YMIN]<<" to "<<oldCoords[YMAX]<<std::endl;   
 //                   std::cout<<"Z "<<oldCoords[ZMIN]<<" to "<<oldCoords[ZMAX]<<std::endl;

 //               }
                processCellRange(voxel, surfId,
                                 overlapXmax + 1, oldCoords[XMAX],
                                 oldCoords[YMIN], oldCoords[YMAX],
                                 oldCoords[ZMIN], oldCoords[ZMAX], false);
            }

            // Process middle X slice, but non-overlapping Y slices
            // Bottom region
            if (oldCoords[YMIN] < overlapYmin)
            {
  //              if(surfId == 869)
  //              {
  //                  std::cout<<"SURF REMOVE bottom region "<<surfId<<std::endl;
  //                  std::cout<<"X "<<overlapXmin<<" to "<<overlapXmax<<std::endl;
  //                  std::cout<<"Y "<<oldCoords[YMIN]<<" to "<<overlapYmin - 1<<std::endl;   
  //                  std::cout<<"Z "<<oldCoords[ZMIN]<<" to "<<oldCoords[ZMAX]<<std::endl;
  //              }
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 oldCoords[YMIN], overlapYmin - 1,
                                 oldCoords[ZMIN], oldCoords[ZMAX], false);
            }

            // Top region
            if (oldCoords[YMAX] > overlapYmax)
            {
//                if(surfId == 869)
//                {
//                    std::cout<<"SURF REMOVE top region "<<surfId<<std::endl;
//                    std::cout<<"X "<<overlapXmin<<" to "<<overlapXmax<<std::endl;
//                    std::cout<<"Y "<<overlapYmax + 1<<" to "<<oldCoords[YMAX]<<std::endl;   
//                    std::cout<<"Z "<<oldCoords[ZMIN]<<" to "<<oldCoords[ZMAX]<<std::endl;
//
//                }
//
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmax + 1, oldCoords[YMAX],
                                 oldCoords[ZMIN], oldCoords[ZMAX], false);
            }

            // Process middle X and Y slices, but non-overlapping Z slices
            // Front region
            if (oldCoords[ZMIN] < overlapZmin)
            {

 //       if(surfId == 869)
 //       {
 //           std::cout<<"SURF REMOVE front region "<<surfId<<std::endl;
 //           std::cout<<"X "<<overlapXmin<<" to "<<overlapXmax<<std::endl;
 //           std::cout<<"Y "<<overlapYmin<<" to "<<overlapYmax<<std::endl;
 //           std::cout<<"Z "<<oldCoords[ZMIN]<<" to "<<overlapZmin - 1<<std::endl;
 //       }


                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmin, overlapYmax,
                                 oldCoords[ZMIN], overlapZmin - 1, false);
            }

            // Back region
            if (oldCoords[ZMAX] > overlapZmax)
            {

//        if(surfId == 869)
//        {
//            std::cout<<"SURF REMOVE back region "<<surfId<<std::endl;
//            std::cout<<"X "<<overlapXmin<<" to "<<overlapXmax<<std::endl;
//            std::cout<<"Y "<<overlapYmin<<" to "<<overlapYmax<<std::endl;
//            std::cout<<"Z "<<overlapZmax + 1<<" to "<<oldCoords[ZMAX]<<std::endl;
//        }
// 
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmin, overlapYmax,
                                 overlapZmax + 1, oldCoords[ZMAX], false);
            }

            // Process regions to add (new cells not in old bounds)

            // Left region of new bounds
            if (newCoords[XMIN] < overlapXmin)
            {
 //               if(surfId == 869)
 //               {
 //                   std::cout<<"SURF ADD add surface left region "<<surfId<<std::endl;
 //                   std::cout<<"X "<<newCoords[XMIN]<<" to "<<overlapXmin - 1<<std::endl;
 //                   std::cout<<"Y "<<newCoords[YMIN]<<" to "<<newCoords[YMAX]<<std::endl;   
 //                   std::cout<<"Z "<<newCoords[ZMIN]<<" to "<<newCoords[ZMAX]<<std::endl;
 //               }
                processCellRange(voxel, surfId,
                                 newCoords[XMIN], overlapXmin - 1,
                                 newCoords[YMIN], newCoords[YMAX],
                                 newCoords[ZMIN], newCoords[ZMAX], true);
            }

            // Right region of new bounds
            if (newCoords[XMAX] > overlapXmax)
            {
 //               if(surfId == 869)
 //               {
 //                   std::cout<<"SURF ADD add surface right region "<<surfId<<std::endl;
 //                   std::cout<<"X "<<overlapXmax + 1<<" to "<<newCoords[XMAX]<<std::endl;
 //                   std::cout<<"Y "<<newCoords[YMIN]<<" to "<<newCoords[YMAX]<<std::endl;   
 //                   std::cout<<"Z "<<newCoords[ZMIN]<<" to "<<newCoords[ZMAX]<<std::endl;

 //               }
                processCellRange(voxel, surfId,
                                 overlapXmax + 1, newCoords[XMAX],
                                 newCoords[YMIN], newCoords[YMAX],
                                 newCoords[ZMIN], newCoords[ZMAX], true);
            }

            // Process middle X slice, but non-overlapping Y slices
            // Bottom region
            if (newCoords[YMIN] < overlapYmin)
            {
  //              if(surfId == 869)
  //              {
  //                  std::cout<<"SURF ADD middle X, but non-overlapping Y slices "<<surfId<<std::endl;
  //                  std::cout<<"X "<<overlapXmin<<" to "<<overlapXmax<<std::endl;
  //                  std::cout<<"Y "<<newCoords[YMIN]<<" to "<<overlapYmin - 1<<std::endl;
  //                  std::cout<<"Z "<<newCoords[ZMIN]<<" to "<<newCoords[ZMAX]<<std::endl;
  //              }
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 newCoords[YMIN], overlapYmin - 1,
                                 newCoords[ZMIN], newCoords[ZMAX], true);
            }

            // Top region
            if (newCoords[YMAX] > overlapYmax)
            {
//                if(surfId == 869)
//                {
//                    std::cout<<"SURF ADD add surface top region "<<surfId<<std::endl;
//                    std::cout<<"X "<<overlapXmin<<" to "<<overlapXmax<<std::endl;
//                    std::cout<<"Y "<<overlapYmax + 1<<" to "<<newCoords[YMAX]<<std::endl;   
//                    std::cout<<"Z "<<newCoords[ZMIN]<<" to "<<newCoords[ZMAX]<<std::endl;
//                }
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmax + 1, newCoords[YMAX],
                                 newCoords[ZMIN], newCoords[ZMAX], true);
            }

            // Process middle X and Y slices, but non-overlapping Z slices
            // Front region
            if (newCoords[ZMIN] < overlapZmin)
            {
 //               if(surfId == 869)
 //               {
 //                   std::cout<<"SURF ADD add surface front region "<<surfId<<std::endl;
 //                   std::cout<<"X "<<overlapXmin<<" to "<<overlapXmax<<std::endl;
 //                   std::cout<<"Y "<<overlapYmin<<" to "<<overlapYmax<<std::endl;   
 //                   std::cout<<"Z "<<newCoords[ZMIN]<<" to "<<overlapZmin - 1<<std::endl;
 //               }
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmin, overlapYmax,
                                 newCoords[ZMIN], overlapZmin - 1, true);
            }

            // Back region
            if (newCoords[ZMAX] > overlapZmax)
            {
  //              if(surfId == 869)
  //              {
  //                  std::cout<<"SURF ADD add surface back region "<<surfId<<std::endl;
  //                  std::cout<<"X "<<overlapXmin<<" to "<<overlapXmax<<std::endl;
  //                  std::cout<<"Y "<<overlapYmin<<" to "<<overlapYmax<<std::endl;   
  //                  std::cout<<"Z "<<overlapZmax + 1<<" to "<<newCoords[ZMAX]<<std::endl;
  //              }
                processCellRange(voxel, surfId,
                                 overlapXmin, overlapXmax,
                                 overlapYmin, overlapYmax,
                                 overlapZmax + 1, newCoords[ZMAX], true);
            }
        }

        //voxel->surfaceBounds[surfId] = newCoords;
    }
    void Voxel_update_remote_coords(void *v, int *IREM, my_real *XREM, int RSIZ, int ISIZ, int NSNR)
    {
        // IREM is a 2D array of size (NSNR,ISIZ)
        // XREM is a 2D array of size (NSNR,RSIZ)
        // nsnGlob is the number of global nodes
        // NSNR is the number of remote nodes: number of nodes sent by remote MPI processes
        Voxel *voxel = static_cast<Voxel *>(v);
        voxel->nodesRemoteOld = voxel->nodesRemote;

//        std::cout<<"Node remote new of 170 "<<voxel->nodesRemote[170]<<std::endl;
//        std::cout<<"Node remote old of 170 "<<voxel->nodesRemoteOld[170]<<std::endl;
        
        GridMapper mapper(voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        constexpr size_t idGlob = 3 ; // position in IREM of the global id in 1:nsnGlob
        constexpr size_t idLoc = 0 ; // position in IREM of the id internal to the remote process
        constexpr size_t idX = 0 ; // position in XREM of the x coordinate
        constexpr size_t idY = 1 ; // position in XREM of the y coordinate
        constexpr size_t idZ = 2 ; // position in XREM of the z coordinate
        voxel->nsnr= NSNR;
        for(int i = 0; i < NSNR; ++i)
        {
            // get the global id of the node
            int globId = IREM[i * ISIZ + idGlob] - 1; // Fortran to C++ index conversion
            // get the local id of the node
            int locId = IREM[i * ISIZ + idLoc] - 1; // Fortran to C++ index conversion
            // get the coordinates of the node
            double x = static_cast<double>(XREM[i * RSIZ + idX]);
            double y = static_cast<double>(XREM[i * RSIZ + idY]);
            double z = static_cast<double>(XREM[i * RSIZ + idZ]);
            bool isAlive = voxel->isInDomain(x, y, z);                                     
            if(isAlive)
            {
                voxel->nodesRemote[globId] = mapper.mapToIndex(x, y, z); // map the global id to the local id
            }
            else
            {
                voxel->nodesRemote[globId] = DEAD;
            }
        }
       

    }
    void Voxel_update_remote(void *v, int *IREM, my_real *XREM, int RSIZ, int ISIZ, int NSNR)
    {
        // IREM is a 2D array of size (NSNR,ISIZ)
        // XREM is a 2D array of size (NSNR,RSIZ)
        // nsnGlob is the number of global nodes
        // NSNR is the number of remote nodes: number of nodes sent by remote MPI processes
        Voxel *voxel = static_cast<Voxel *>(v);
        GridMapper mapper(voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        constexpr size_t idGlob = 3 ; // position in IREM of the global id in 1:nsnGlob
        constexpr size_t idLoc = 0 ; // position in IREM of the id internal to the remote process
        constexpr size_t idX = 0 ; // position in XREM of the x coordinate
        constexpr size_t idY = 1 ; // position in XREM of the y coordinate
        constexpr size_t idZ = 2 ; // position in XREM of the z coordinate
        voxel->nsnr= NSNR;
        // set globalToIREM to DEAD
        //std::cout<<"GlobalToLocalRemote size "<<voxel->globalToIREM.size()<<std::endl;
        for(int i = 0; i < voxel->globalToIREM.size(); ++i)
        {
            voxel->globalToIREM[i] = DEAD;
        }


        for(int i = 0; i < NSNR; ++i)
        {
            // get the global id of the node
            int globId = IREM[i * ISIZ + idGlob] - 1; // Fortran to C++ index conversion
            // get the local id of the node
            int locId = IREM[i * ISIZ + idLoc] - 1; // Fortran to C++ index conversion
            // get the coordinates of the node
            double x = static_cast<double>(XREM[i * RSIZ + idX]);
            double y = static_cast<double>(XREM[i * RSIZ + idY]);
            double z = static_cast<double>(XREM[i * RSIZ + idZ]);
            bool isAlive = Voxel_update_node_remote(v, x, y, z, globId, mapper);
            if(isAlive)
            {
                voxel->globalToIREM[globId] = i; // map the global id to the local id
            }
        }

    }
    // update the surfaces, then the nodes
    void Voxel_update(void *v, int *irect, int nrtm, my_real *gap, int *nsv, int nsn, my_real *X, int numnod, my_real *stf, my_real *stfn, int *IREM, my_real *XREM, int RSIZ, int ISIZ, int NSNR)
    {

        Voxel *voxel = static_cast<Voxel *>(v);
        GridMapper mapper(voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);

        voxel->nodesOld = voxel->nodes;

        for (int i = 0; i < nsn; ++i)
        {
            if (stfn[i] <= static_cast<my_real>(0))
            {
                voxel->nodes[i] = DEAD;
            }
            else
            {
            // get the coordinates of the node
            const size_t i1 = nsv[i] - 1; // Fortran to C++ index conversion
            const double x = static_cast<double>(X[3 * i1]);
            const double y = static_cast<double>(X[3 * i1 + 1]);
            const double z = static_cast<double>(X[3 * i1 + 2]);
            voxel->nodes[i] = mapper.mapToIndex(x, y, z);
            }
        }
   


        Voxel_update_remote_coords(v, IREM, XREM, RSIZ, ISIZ, NSNR);
//        if(voxel->nodesRemote.size() > 170)
//        {
//        std::cout<<"BEGINING Node remote new of 170 "<<voxel->nodesRemote[170]<<std::endl;
//        std::cout<<"BEGINING Node remote old of 170 "<<voxel->nodesRemoteOld[170]<<std::endl;
//        }
 
        

        // Loop over the surfaces, add the surfaces to the cells it crosses
        voxel->surfaceBoundsOld = voxel->surfaceBounds;

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
            Node minCoords = mapper.mapMin(xmin, ymin, zmin);
            Node maxCoords = mapper.mapMax(xmax, ymax, zmax);
    
            // Ensure coordinates are within valid grid range
            minCoords[0] = std::max(minCoords[0], static_cast<short int>(0));
            minCoords[1] = std::max(minCoords[1], static_cast<short int>(0));
            minCoords[2] = std::max(minCoords[2], static_cast<short int>(0));
    
            maxCoords[0] = std::min(maxCoords[0], static_cast<short int>(voxel->nbx - 1));
            maxCoords[1] = std::min(maxCoords[1], static_cast<short int>(voxel->nby - 1));
            maxCoords[2] = std::min(maxCoords[2], static_cast<short int>(voxel->nbz - 1));
            

            voxel->surfaceBounds[i][XMIN] = minCoords[0];
            voxel->surfaceBounds[i][YMIN] = minCoords[1];
            voxel->surfaceBounds[i][ZMIN] = minCoords[2];
            voxel->surfaceBounds[i][XMAX] = maxCoords[0];
            voxel->surfaceBounds[i][YMAX] = maxCoords[1];
            voxel->surfaceBounds[i][ZMAX] = maxCoords[2];

    
            Voxel_update_surf(v, i, mapper);
        }
        // update local nodes
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
            Voxel_update_node(v, i, mapper);
        }
        //update remote nodes
//         if(voxel->nodesRemote.size() > 170)
//        {
//        std::cout<<"END-1 Node remote new of 170 "<<voxel->nodesRemote[170]<<std::endl;
//        std::cout<<"END-1 Node remote old of 170 "<<voxel->nodesRemoteOld[170]<<std::endl;
//        }
 
        Voxel_update_remote(v, IREM, XREM, RSIZ, ISIZ, NSNR);
 //        if(voxel->nodesRemote.size() > 170)
 //       {
 //       std::cout<<"END Node remote new of 170 "<<voxel->nodesRemote[170]<<std::endl;
 //       std::cout<<"END Node remote old of 170 "<<voxel->nodesRemoteOld[170]<<std::endl;
 //       }
 
 


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
    void Voxel_get_candidates_data(void *v, int ne, int **cands, int *nb, int *irect, int *nsv)
    {
        Voxel *voxel = static_cast<Voxel *>(v);
        *nb = static_cast<int>(voxel->surfaceCandidates[ne - 1].size());
        *cands = voxel->surfaceCandidates[ne - 1].data();


        // Debug: loop over all nodes, to check if they are in the candidates, using is_in_bounds
#ifdef DEBUG_VOXEL
        size_t candidate_counter = 0;
        for(size_t it = 0 ; it < voxel->nodes.size();++it)                                                               
        {
            size_t index = voxel->nodes[it];
            Node coord = index_to_coord(index, voxel->nbx, voxel->nby, voxel->nbz);
            if(is_in_bounds(coord, voxel->surfaceBounds[ne - 1]))
            {
                // check if the node belongs to the surface
                size_t globId = nsv[it]; // Fortran to C++ index conversion
                bool belongs_to_surf = (globId == irect[0 + 4 * (ne - 1)] ) || (globId == irect[1 + 4 * (ne - 1)] ) || (globId == irect[2 + 4 * (ne - 1)] ) || (globId == irect[3 + 4 * (ne - 1)] ); 


                //find in surfaceCandidates
                auto it2 = std::find(voxel->surfaceCandidates[ne - 1].begin(), voxel->surfaceCandidates[ne - 1].end(), it);
                if(it2 == voxel->surfaceCandidates[ne - 1].end() && !belongs_to_surf)
                {
                    std::cout << "position voxel->nodes "<<it<<std::endl;
                    std::cout << "Error: MISSING LOCAL CANDIDATE " << it << " is in the surface bounds, but not in the candidates for surface " << ne-1 << std::endl;
                    std::cout << "coordinate of the node: " << coord[0] << " " << coord[1] << " " << coord[2] << std::endl;
                    std::cout << "min Coordinate of the surface: " << voxel->surfaceBounds[ne - 1][XMIN] << " " << voxel->surfaceBounds[ne - 1][YMIN] << " " << voxel->surfaceBounds[ne - 1][ZMIN] << std::endl;
                    std::cout << "max Coordinate of the surface: " << voxel->surfaceBounds[ne - 1][XMAX] << " " << voxel->surfaceBounds[ne - 1][YMAX] << " " << voxel->surfaceBounds[ne - 1][ZMAX] << std::endl;

                    std::cout <<" Surface Nodes: "<<irect[0 + 4 * (ne - 1)] << " " << irect[1 + 4 * (ne - 1)] << " " << irect[2 + 4 * (ne - 1)] << " " << irect[3 + 4 * (ne - 1)] << std::endl;
                    std::cout << "Global node id: " << globId << std::endl;
                    std::abort();
                }
                if(!belongs_to_surf) candidate_counter++;

            }
        }
       
      // check if candidate_counter == *nb
        if(candidate_counter != *nb)
        {
            std::cout << "Error: candidate_counter != *nb" << std::endl;
            std::cout << "candidate_counter: " << candidate_counter << std::endl;
            std::cout << "*nb: " << *nb << std::endl;
            // print all cands filled so far
            for(int j = 0; j < *nb; j++)
            {
                std::cout<<"cand["<<j<<"]="<<(*cands)[j]<<std::endl;
            }
            std::abort();
        }
#endif

    }

    void Voxel_get_candidates_remote(void *v, int ne, int *cands, int *nb, int *IREM, my_real *XREM, int RSIZ, int ISIZ, int nsnr)
    {
        size_t id = static_cast<size_t>(ne - 1); // index of the surface, C to Fortran conversion
        Voxel *voxel = static_cast<Voxel *>(v);

        size_t counter = 0;
        counter = 0;
        for (auto it = voxel->surfaceCandidatesRemote[id].begin(); it != voxel->surfaceCandidatesRemote[id].end(); ++it)
        {
            // C to Fortran index conversion
            int index = static_cast<int>(*it);
            // convert the global id from 1 to nsnGlob, into a local id in the MPI buffer IREM
            size_t locId = voxel->globalToIREM[index]; 

            if(locId == DEAD)
            {
                continue; // skip dead nodes
            }
            counter++;
#ifdef DEBUG_VOXEL
            if(counter > voxel->nsnr)
            {
                std::cout << "Error: too many candidates for surface " << ne << std::endl;
                std::cout << "Max candidates: " << voxel->nsnr << std::endl;
                std::cout << "Current candidates: " << counter << std::endl;
                // print all cands filled so far/too
                for(int j = 0; j < voxel->nsnr; j++)
                {
                    std::cout<<"cand["<<j<<"]="<<cands[j]<<std::endl;
                }
                std::cout<<"FAILING to insert candidate "<<locId+1<<std::endl;
                // abort
                std::abort();
            }
#endif
            cands[counter - 1] = static_cast<int>(locId); // C to Fortran index conversion
        }


#ifdef DEBUG_VOXEL
        // debug check; all values in cands should be unique
       std::set<int> uniqueCandidates(cands, cands + counter);
       if (uniqueCandidates.size() != counter)
       {
           std::cout << "Error: duplicate candidates for surface " << ne << std::endl;
           std::cout << "Number of candidates: " << counter << std::endl;
           // print all cands filled so far
           for(int j = 0; j < counter; j++)
           {
               std::cout<<"cand["<<j<<"]="<<cands[j]<<std::endl;
           }
           // abort
           std::abort();
       }

     // Check all nodes in IREM/XREM to see if they are in the box of the surface. If they are, they should be in the candidates

       GridMapper mapper = GridMapper(voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
       for(size_t i = 0; i < voxel->nsnr; ++i)
       {
           // get the global id of the node
           int globId = IREM[i * ISIZ + 3] - 1; // Fortran to C++ index conversion
           // get the local id of the node
           //int locId = IREM[i * ISIZ + 0] - 1; // Fortran to C++ index conversion
           // get the coordinates of the node
           double x = static_cast<double>(XREM[i * RSIZ + 0]);
           double y = static_cast<double>(XREM[i * RSIZ + 1]);
           double z = static_cast<double>(XREM[i * RSIZ + 2]);

           int locId = voxel->globalToIREM[globId]; 
           if(locId != DEAD)
           {
               //Error if locId is not i 
               if(locId != i)
               {
                   std::cout << "Error: locId is not i" << std::endl;
                   std::cout << "locId: " << locId << std::endl;
                   std::cout << "i: " << i << std::endl;
                   std::abort();
               }
           }

           size_t index = mapper.mapToIndex(x, y, z);
           Node newCoords = index_to_coord(index, voxel->nbx, voxel->nby, voxel->nbz);
           Surf bounds = voxel->surfaceBounds[ne - 1];
           if(is_in_bounds(newCoords, bounds))
           {
               // cands is storing the list of candidate numbered from 0 to nsnr-1 here
               auto it = std::find(cands, cands + counter, locId);
               if(it == cands + counter)
               {
                   std::cout << "Error: node " << globId << " id in xrem " << locId << " is not in the candidates for surface " << ne-1 << std::endl;
                   std::cout<< "Nodes coordinate: " << newCoords[0] << " " << newCoords[1] << " " << newCoords[2] << std::endl;
                   std::cout << "min Coordinate of the surface: " << voxel->surfaceBounds[ne - 1][XMIN] << " " << voxel->surfaceBounds[ne - 1][YMIN] << " " << voxel->surfaceBounds[ne - 1][ZMIN] << std::endl;
                   std::cout << "max Coordinate of the surface: " << voxel->surfaceBounds[ne - 1][XMAX] << " " << voxel->surfaceBounds[ne - 1][YMAX] << " " << voxel->surfaceBounds[ne - 1][ZMAX] << std::endl;
                   std::cout << "Number of candidates: " << counter << std::endl;
                   // print all cands filled so far
                   for(int j = 0; j < counter; j++)
                   {
                       std::cout<<"cand["<<j<<"]="<<cands[j]<<std::endl;
                   }
                   std::abort();
               }
           }
       }
#endif

        // set the number of candidates
        *nb = counter; 
    }
}
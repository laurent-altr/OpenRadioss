#include <iostream>
#include <vector>
#include <array>
#include <unordered_map>
#include <set>
#include <cmath>

constexpr size_t XMIN = 0;
constexpr size_t YMIN = 1;
constexpr size_t ZMIN = 2;
constexpr size_t XMAX = 3;
constexpr size_t YMAX = 4;
constexpr size_t ZMAX = 5;

using Node = std::array<short int, 3>; // coordinate id of the cell containing that node
using Surf = std::array<short int, 6>; // a surface can cross multiple cells

struct Cell
{
    std::set<size_t> nodes;    // list of nodes in the cell
    std::set<size_t> surfaces; // list of surfaces in the cell
};

// a voxel is a 3D grid of cells. Each cell contains the list of the nodes it contains, and the list of the surfaces it crosses
class Voxel
{
public:
    size_t nbx, nby, nbz;                            // dimension of the grid
    std::array<double, 6> bounds;                    // bounds of the grid
    std::unordered_map<size_t, Cell> cells;          // map of nodes to their index in the vector
    std::vector<Node> nodes;                         // vector of secondary nodes
    std::vector<Surf> surfaceBounds;                 // vector of main surfaceBounds
    std::vector<std::set<size_t>> surfaceNodes;      // vector of secondary surfaceBounds
    std::vector<std::set<size_t>> surfaceCandidates; // vector of secondary node candidates for broad phase collision with the surface
};

// A 3D grid of cell is meshing the space defined by the bounds
// it is divded into nbx*nby*nbz cells
// I want to convert a 3D coordinate to a cell index.
// if the coordinates are outside the bounds, I still want to return a cell index, provided that the space is extended
// for instance, if a node is right outside the xmax bound, then its x coordinate is 0 (we cycle back to the beginning of the grid)

// define a macro fonction to convert a 3D coordinate to a 1D index
#define COORD_TO_INDEX(x, y, z, nbx, nby) ((x) + (y) * (nbx) + (z) * (nbx) * (nby))

size_t safe_coord_to_index(size_t x, size_t y, size_t z, size_t nbx, size_t nby, size_t nbz)
{
    // Ensure coordinates are in bounds
    x = (x >= nbx) ? nbx - 1 : x;
    y = (y >= nby) ? nby - 1 : y;
    z = (z >= nbz) ? nbz - 1 : z;
    return COORD_TO_INDEX(x, y, z, nbx, nby);
}

size_t coord_to_index(double x, double y, double z, const std::array<double, 6> &bounds,
                      size_t nbx, size_t nby, size_t nbz)
{
    // Calculate relative position in each dimension (0.0 to 1.0)
    double rx = (bounds[XMAX] > bounds[XMIN])
                    ? (x - bounds[XMIN]) / (bounds[XMAX] - bounds[XMIN])
                    : 0.5; // If min=max, place in middle

    double ry = (bounds[YMAX] > bounds[YMIN])
                    ? (y - bounds[YMIN]) / (bounds[YMAX] - bounds[YMIN])
                    : 0.5;

    double rz = (bounds[ZMAX] > bounds[ZMIN])
                    ? (z - bounds[ZMIN]) / (bounds[ZMAX] - bounds[ZMIN])
                    : 0.5;

    // Handle wrapping for values outside the bounds (both above and below)
    rx = rx - floor(rx); // This handles both positive and negative values correctly
    ry = ry - floor(ry);
    rz = rz - floor(rz);

    // Convert to grid indices
    size_t ix = static_cast<size_t>(rx * nbx);
    size_t iy = static_cast<size_t>(ry * nby);
    size_t iz = static_cast<size_t>(rz * nbz);

    // Ensure indices are within bounds (this is technically redundant with the wrapping above)
    ix = ix % nbx;
    iy = iy % nby;
    iz = iz % nbz;

    // Convert to linear index
    size_t index = COORD_TO_INDEX(ix, iy, iz, nbx, nby);
    return index;
}

Node coord_to_grid(double x, double y, double z, const std::array<double, 6> &bounds,
                   size_t nbx, size_t nby, size_t nbz)
{
    // Calculate relative position in each dimension (0.0 to 1.0)
    // Add handling for zero-width dimensions
    double rx = (bounds[XMAX] > bounds[XMIN])
                    ? (x - bounds[XMIN]) / (bounds[XMAX] - bounds[XMIN])
                    : 0.5; // If min=max, place in middle

    double ry = (bounds[YMAX] > bounds[YMIN])
                    ? (y - bounds[YMIN]) / (bounds[YMAX] - bounds[YMIN])
                    : 0.5;

    double rz = (bounds[ZMAX] > bounds[ZMIN])
                    ? (z - bounds[ZMIN]) / (bounds[ZMAX] - bounds[ZMIN])
                    : 0.5;

    // Handle wrapping for values outside the bounds
    rx = rx - floor(rx); // This handles both positive and negative values
    ry = ry - floor(ry);
    rz = rz - floor(rz);

    // Convert to grid indices, handling the edge case of exact boundary
    // When rx=1.0 (exactly at XMAX), we want index nbx-1, not 0
    size_t ix = (rx >= 1.0) ? nbx - 1 : static_cast<size_t>(rx * nbx);
    size_t iy = (ry >= 1.0) ? nby - 1 : static_cast<size_t>(ry * nby);
    size_t iz = (rz >= 1.0) ? nbz - 1 : static_cast<size_t>(rz * nbz);

    // Ensure indices are within bounds (protection for extreme cases)
    ix = (ix >= nbx) ? nbx - 1 : ix;
    iy = (iy >= nby) ? nby - 1 : iy;
    iz = (iz >= nbz) ? nbz - 1 : iz;

    Node coord;
    coord[0] = static_cast<short int>(ix);
    coord[1] = static_cast<short int>(iy);
    coord[2] = static_cast<short int>(iz);
    return coord;
}

size_t coord_to_index(Node coord, const std::array<double, 6> &bounds,
                      size_t nbx, size_t nby, size_t nbz)
{
    // cast to size_t
    size_t x = static_cast<size_t>(coord[0]);
    size_t y = static_cast<size_t>(coord[1]);
    size_t z = static_cast<size_t>(coord[2]);
    return COORD_TO_INDEX(x, y, z, nbx, nby);
}

size_t coord_to_index(short int x, short int y, short int z, const std::array<double, 6> &bounds,
                      size_t nbx, size_t nby, size_t nbz)
{
    return COORD_TO_INDEX(static_cast<size_t>(x), static_cast<size_t>(y), static_cast<size_t>(z), nbx, nby);
}

Node index_to_coord(size_t index, size_t nbx, size_t nby, size_t nbz)
{
    // Calculate maximum valid index
    size_t max_index = nbx * nby * nbz - 1;

    // Clamp index to valid range
    index = (index > max_index) ? max_index : index;

    Node coord;
    coord[0] = static_cast<short int>(index % nbx);
    index /= nbx;
    coord[1] = static_cast<short int>(index % nby);
    index /= nby;
    coord[2] = static_cast<short int>(index % nbz);
    return coord;
}

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

        // allocate the vector of surfaces
        v->surfaceBounds.resize(nbsurfaces);
        v->surfaceNodes.resize(nbsurfaces);
        v->surfaceCandidates.resize(nbsurfaces);
        return v;
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
    }

    std::vector<Node> getCellsInWrappedRange(short int xmin, short int xmax, short int ymin, short int ymax,
                                             short int zmin, short int zmax, size_t nbx, size_t nby, size_t nbz)
    {
        std::vector<Node> cells;

        // Handle X dimension
        std::vector<short int> xIndices;
        if (xmin <= xmax)
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
        if (ymin <= ymax)
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
        if (zmin <= zmax)
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
                // check if the node exists in nsv
                if (glob2nsv.find(globId) != glob2nsv.end())
                {
                    // get the local id of the node
                    int localId = glob2nsv[globId];
                    // add the node to the surfaceNodes
                    voxel->surfaceNodes[i].insert(localId);
                }
            }
        }

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
            const Node minCoords = coord_to_grid(xmin, ymin, zmin, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
            const Node maxCoords = coord_to_grid(xmax, ymax, zmax, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
            std::vector<Node> cells = getCellsInWrappedRange(minCoords[0], maxCoords[0], minCoords[1], maxCoords[1], minCoords[2], maxCoords[2], voxel->nbx, voxel->nby, voxel->nbz);
            // add the surface to the cells it crosses
            for (auto cell : cells)
            {
                size_t index = coord_to_index(cell, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
                // check if the cell exists
                if (voxel->cells.find(index) == voxel->cells.end())
                {
                    // create the new cell
                    voxel->cells[index] = Cell();
                }
                // add the surface to the cell
                voxel->cells[index].surfaces.insert(i);
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
            if (voxel->cells.find(index) == voxel->cells.end())
            {
                // create the new cell
                voxel->cells[index] = Cell();
            }
            // add the node to the cell
            voxel->cells[index].nodes.insert(i);
            // add the node to the nodes vector
            voxel->nodes[i] = coord_to_grid(x, y, z, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
            // loop over the surfaces crossing the cell
            for (auto surfId : voxel->cells[index].surfaces)
            {
                // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
                if (voxel->surfaceNodes[surfId].find(i) == voxel->surfaceNodes[surfId].end())
                {
                    // add the node to the surfaceCandiates
                    voxel->surfaceCandidates[surfId].insert(i);
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
            voxel->cells[oldIndex].nodes.erase(nodeId);
            // remove the node from candidates list of all surfaces crossing the old cell
            for (auto surfId : voxel->cells[oldIndex].surfaces)
            {
                // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
                if (voxel->surfaceNodes[surfId].find(nodeId) == voxel->surfaceNodes[surfId].end())
                {
                    // remove the node from the surfaceCandiates
                    voxel->surfaceCandidates[surfId].erase(nodeId);
                }
            }

            // add the node to the new cell
            // check if the new cell exists
            if (voxel->cells.find(newIndex) == voxel->cells.end())
            {
                // create the new cell
                voxel->cells[newIndex] = Cell();
            }
            voxel->cells[newIndex].nodes.insert(nodeId);
            voxel->nodes[nodeId] = index_to_coord(newIndex, voxel->nbx, voxel->nby, voxel->nbz);
            // loop over voxel->cells[newIndex].surfaces
            for (auto surfId : voxel->cells[newIndex].surfaces)
            {
                // check if the node is in the surfaceNodes, because nodes that defines the surface cannot be a candidate for collision
                if (voxel->surfaceNodes[surfId].find(nodeId) == voxel->surfaceNodes[surfId].end())
                {
                    // add the node to the surfaceCandiates of all surfaces crossing the new cell
                    voxel->surfaceCandidates[surfId].insert(nodeId);
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

        Node minCoords = coord_to_grid(xmin, ymin, zmin, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        Node maxCoords = coord_to_grid(xmax, ymax, zmax, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
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
                if (voxel->cells.find(index) != voxel->cells.end())
                {
                    voxel->cells[index].surfaces.erase(surfId);
                    for (auto nodeId : voxel->cells[index].nodes)
                    {
                        if (voxel->surfaceNodes[surfId].find(nodeId) == voxel->surfaceNodes[surfId].end())
                        {
                            voxel->surfaceCandidates[surfId].erase(nodeId);
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
                if (voxel->cells.find(index) == voxel->cells.end())
                {
                    // create the new cell
                    voxel->cells[index] = Cell();
                }
                // add the surface to the cell
                voxel->cells[index].surfaces.insert(surfId);
                // update surfaceCandidates
                for (auto nodeId : voxel->cells[index].nodes)
                {
                    if (voxel->surfaceNodes[surfId].find(nodeId) == voxel->surfaceNodes[surfId].end())
                    {
                        voxel->surfaceCandidates[surfId].insert(nodeId);
                    }
                }
            }
            // for (short int i = newCoords[XMIN]; i <= newCoords[XMAX]; i++)
            // {
            //     for (short int j = newCoords[YMIN]; j <= newCoords[YMAX]; j++)
            //     {
            //         for (short int k = newCoords[ZMIN]; k <= newCoords[ZMAX]; k++)
            //         {
            //             size_t index = coord_to_index(i, j, k, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
            //             if (voxel->cells.find(index) == voxel->cells.end())
            //             {
            //                 // create the new cell
            //                 voxel->cells[index] = Cell();
            //             }
            //             voxel->cells[index].surfaces.insert(surfId);
            //             for (auto nodeId : voxel->cells[index].nodes)
            //             {
            //                 if (voxel->surfaceNodes[surfId].find(nodeId) == voxel->surfaceNodes[surfId].end())
            //                 {
            //                     voxel->surfaceCandidates[surfId].insert(nodeId);
            //                 }
            //             }
            //         }
            //     }
            // }
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
        //        if(ne == 1) {
        //            // write bounds to std
        //            // print all the cells of the node ne = 1, id = 0
        //            std::vector<Node> cells = getCellsInWrappedRange(voxel->surfaceBounds[0][XMIN], voxel->surfaceBounds[0][XMAX],
        //                                                        voxel->surfaceBounds[0][YMIN], voxel->surfaceBounds[0][YMAX],
        //                                                        voxel->surfaceBounds[0][ZMIN], voxel->surfaceBounds[0][ZMAX],
        //                                                        voxel->nbx, voxel->nby, voxel->nbz);
        //            std::cout << "Cells of surface 1: " << std::endl;
        //            for(auto cell : cells) {
        //                std::cout << "Cell: " << cell[0] << " " << cell[1] << " " << cell[2] << std::endl;
        //            }
        //        // check coordinates of node 33 (32 in C++)
        //            std::cout<<"Node 33 coordinates: " << voxel->nodes[32][0] << " " << voxel->nodes[32][1] << " " << voxel->nodes[32][2] << std::endl;
        //
        //        }
    }
}
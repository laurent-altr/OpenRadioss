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

struct Cell {
    std::set<size_t> nodes; // list of nodes in the cell
    std::set<size_t> surfaces; // list of surfaces in the cell
};

// a voxel is a 3D grid of cells. Each cell contains the list of the nodes it contains, and the list of the surfaces it crosses
class Voxel {
public:
    size_t nbx,nby,nbz; // dimension of the grid
    std::array<double, 6> bounds; // bounds of the grid
    std::unordered_map<size_t, Cell> cells; // map of nodes to their index in the vector
    std::vector<Node> nodes; // vector of secondary nodes
    std::vector<Surf> surfaceBounds; // vector of main surfaceBounds
};


// A 3D grid of cell is meshing the space defined by the bounds
// it is divded into nbx*nby*nbz cells
// I want to convert a 3D coordinate to a cell index. 
// if the coordinates are outside the bounds, I still want to return a cell index, provided that the space is extended 
// for instance, if a node is right outside the xmax bound, then its x coordinate is 0 (we cycle back to the beginning of the grid)

// define a macro fonction to convert a 3D coordinate to a 1D index
#define COORD_TO_INDEX(x,y,z,nbx,nby) ((x) + (y)*(nbx) + (z)*(nbx)*(nby))
size_t coord_to_index(double x, double y, double z, const std::array<double, 6> &bounds, 
                     size_t nbx, size_t nby, size_t nbz) {
    // Calculate relative position in each dimension (0.0 to 1.0)
    double rx = (x - bounds[XMIN]) / (bounds[XMAX] - bounds[XMIN]);
    double ry = (y - bounds[YMIN]) / (bounds[YMAX] - bounds[YMIN]);
    double rz = (z - bounds[ZMIN]) / (bounds[ZMAX] - bounds[ZMIN]);
    
    // Handle wrapping for values outside the bounds (both above and below)
    rx = rx - floor(rx);  // This handles both positive and negative values correctly
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

size_t coord_to_index(Node coord, const std::array<double, 6> &bounds, 
                     size_t nbx, size_t nby, size_t nbz) {
    //cast to size_t
    size_t x = static_cast<size_t>(coord[0]);
    size_t y = static_cast<size_t>(coord[1]);
    size_t z = static_cast<size_t>(coord[2]);
    return COORD_TO_INDEX(x, y, z, nbx, nby);
}

size_t coord_to_index(short int x, short int y, short int z, const std::array<double, 6> &bounds, 
                     size_t nbx, size_t nby, size_t nbz) {
    return COORD_TO_INDEX(static_cast<size_t>(x), static_cast<size_t>(y), static_cast<size_t>(z), nbx, nby);
}

Node index_to_coord(size_t index, size_t nbx, size_t nby, size_t nbz) {
    Node coord;
    coord[0] = index % nbx;
    index /= nbx;
    coord[1] = index % nby;
    index /= nby;
    coord[2] = index % nbz;
    return coord;
}





extern "C" 
{
    void * Voxel_new(int nbx, int nby, int nbz, int nbsurfaces, int nbnodes ) {
        Voxel *v = new Voxel();
        v->nbx = nbx;
        v->nby = nby;
        v->nbz = nbz;
        // allocate the vector of nodes
        v->nodes.resize(nbnodes);
        // fill with 

        // allocate the vector of surfaces
        v->surfaceBounds.resize(nbsurfaces);
        return v;
    }

    void Voxel_delete(void *v) {
        Voxel *voxel = static_cast<Voxel*>(v);
        delete voxel;
    }
    void Voxel_update_node(void *v, double x, double y , double z, int nodeId) {
        Voxel *voxel = static_cast<Voxel*>(v);

        size_t newIndex = coord_to_index(x, y, z, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
        size_t oldIndex = coord_to_index(voxel->nodes[nodeId], voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz); 

        if(newIndex != oldIndex) {
            // remove the node from the old cell
            voxel->cells[oldIndex].nodes.erase(nodeId);
            // add the node to the new cell

            // check if the new cell exists
            if(voxel->cells.find(newIndex) == voxel->cells.end()) {
                // create the new cell
                voxel->cells[newIndex] = Cell();
            }
            voxel->cells[newIndex].nodes.insert(nodeId);
            voxel->nodes[nodeId] = index_to_coord(newIndex, voxel->nbx, voxel->nby, voxel->nbz);     
        }
    }

    void Voxel_update_surf(void *v, double xmin, double ymin, double zmin, 
                           double xmax, double ymax, double zmax, int surfId) {
        Voxel *voxel = static_cast<Voxel*>(v);
        
        Surf newCoords; 

        Node minCoords = index_to_coord(coord_to_index(xmin, ymin, zmin, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz), voxel->nbx, voxel->nby, voxel->nbz);
        Node maxCoords = index_to_coord(coord_to_index(xmax, ymax, zmax, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz), voxel->nbx, voxel->nby, voxel->nbz);
        newCoords[XMIN] = minCoords[0];
        newCoords[YMIN] = minCoords[1];
        newCoords[ZMIN] = minCoords[2];
        newCoords[XMAX] = maxCoords[0];
        newCoords[YMAX] = maxCoords[1];
        newCoords[ZMAX] = maxCoords[2];

        if(newCoords != voxel->surfaceBounds[surfId]) {
            // remove the surface from the old cells
            for(short int i = voxel->surfaceBounds[surfId][XMIN]; i <= voxel->surfaceBounds[surfId][XMAX]; i++) {
                for(short int j = voxel->surfaceBounds[surfId][YMIN]; j <= voxel->surfaceBounds[surfId][YMAX]; j++) {
                    for(short int k = voxel->surfaceBounds[surfId][ZMIN]; k <= voxel->surfaceBounds[surfId][ZMAX]; k++) {
                        size_t index = coord_to_index(i, j, k, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
                        if(voxel->cells.find(index) != voxel->cells.end()) {
                            voxel->cells[index].surfaces.erase(surfId);
                        }
                    }
                }
            }
            // add the surface to the new cells
            for (short int i = newCoords[XMIN]; i <= newCoords[XMAX]; i++) {
                for(short int j = newCoords[YMIN]; j <= newCoords[YMAX]; j++) {
                    for(short int k = newCoords[ZMIN]; k <= newCoords[ZMAX]; k++) {
                        size_t index = coord_to_index(i, j, k, voxel->bounds, voxel->nbx, voxel->nby, voxel->nbz);
                        if(voxel->cells.find(index) == voxel->cells.end()) {
                            // create the new cell
                            voxel->cells[index] = Cell();
                        }
                        voxel->cells[index].surfaces.insert(surfId);
                    }
                }
            }
            // update the surface
            voxel->surfaceBounds[surfId] = newCoords;
        }
    }
}
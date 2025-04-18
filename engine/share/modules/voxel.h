#include <iostream>
#include <vector>
#include <array>
#include <unordered_map>
#include <set>
#include <cmath>
#include <chrono>
#include <algorithm>

constexpr size_t XMIN = 0;
constexpr size_t YMIN = 1;
constexpr size_t ZMIN = 2;
constexpr size_t XMAX = 3;
constexpr size_t YMAX = 4;
constexpr size_t ZMAX = 5;

using Node = std::array<short int, 3>; // coordinate id of the cell containing that node
using Surf = std::array<short int, 6>; // a surface can cross multiple cells


//swap_and_pop :

template <typename T>
void swap_and_pop(std::vector<T>& vec, size_t index) {
    if (index < vec.size()) {
        std::swap(vec[index], vec.back());
        vec.pop_back();
    }
}

//void swap_and_pop(std::vector<Node>& vec, size_t index) {
//    if (index < vec.size()) {
//        std::swap(vec[index], vec.back());
//        vec.pop_back();
//    }
//}



class GridMapper {
private:
    // Grid dimensions
    const size_t nbx, nby, nbz;
    
    // Precomputed values for coordinate conversion
    const double x_min, y_min, z_min;
    const double x_factor, y_factor, z_factor;
    const bool x_valid, y_valid, z_valid;
    
public:
    GridMapper(const std::array<double, 6>& bounds, size_t nbx_, size_t nby_, size_t nbz_)
        : nbx(nbx_), nby(nby_), nbz(nbz_),
          x_min(bounds[0]), y_min(bounds[1]), z_min(bounds[2]),
          x_valid(bounds[3] > bounds[0]),
          y_valid(bounds[4] > bounds[1]),
          z_valid(bounds[5] > bounds[2]),
          // Precompute conversion factors (handles division once)
          x_factor(x_valid ? nbx / (bounds[3] - bounds[0]) : 0.0),
          y_factor(y_valid ? nby / (bounds[4] - bounds[1]) : 0.0),
          z_factor(z_valid ? nbz / (bounds[5] - bounds[2]) : 0.0)
    {}
    
    // For minimum coordinates (round down on boundaries)
    Node inline mapMin(double x, double y, double z) const {
        // Direct conversion to indices using precomputed factors
        double dx = x_valid ? (x - x_min) * x_factor : 0.5 * nbx;
        double dy = y_valid ? (y - y_min) * y_factor : 0.5 * nby;
        double dz = z_valid ? (z - z_min) * z_factor : 0.5 * nbz;
        
        // Truncate to integer indices
        size_t ix = static_cast<size_t>(dx);
        size_t iy = static_cast<size_t>(dy);
        size_t iz = static_cast<size_t>(dz);
        
        // Bounds checking (use branch prediction-friendly style)
        ix = ix < nbx ? ix : nbx - 1;
        iy = iy < nby ? iy : nby - 1;
        iz = iz < nbz ? iz : nbz - 1;
        
        return {
            static_cast<short int>(ix),
            static_cast<short int>(iy),
            static_cast<short int>(iz)
        };
    }
    
    // For maximum coordinates (round up on boundaries)
    Node inline mapMax(double x, double y, double z) const {
        // Direct conversion to indices using precomputed factors
        double dx = x_valid ? (x - x_min) * x_factor : 0.5 * nbx;
        double dy = y_valid ? (y - y_min) * y_factor : 0.5 * nby;
        double dz = z_valid ? (z - z_min) * z_factor : 0.5 * nbz;
        
        // Check if exactly on boundary (using integer equality)
        size_t ix_floor = static_cast<size_t>(dx);
        size_t iy_floor = static_cast<size_t>(dy);
        size_t iz_floor = static_cast<size_t>(dz);
        
        bool exact_x = dx == static_cast<double>(ix_floor);
        bool exact_y = dy == static_cast<double>(iy_floor);
        bool exact_z = dz == static_cast<double>(iz_floor);
        
        // If exactly on boundary and not at max, increment
        size_t ix = exact_x && ix_floor < nbx - 1 ? ix_floor + 1 : ix_floor;
        size_t iy = exact_y && iy_floor < nby - 1 ? iy_floor + 1 : iy_floor;
        size_t iz = exact_z && iz_floor < nbz - 1 ? iz_floor + 1 : iz_floor;
        
        // Bounds checking
        ix = ix < nbx ? ix : nbx - 1;
        iy = iy < nby ? iy : nby - 1;
        iz = iz < nbz ? iz : nbz - 1;
        
        return {
            static_cast<short int>(ix),
            static_cast<short int>(iy),
            static_cast<short int>(iz)
        };
    }
};

struct Cell
{
    std::vector<int> nodes;    // list of nodes in the cell
    std::vector<int> surfaces; // list of surfaces in the cell
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
    std::vector<std::set<int>> surfaceNodes;      // vector of secondary surfaceBounds
    std::vector<std::set<int>> surfaceCandidates; // vector of secondary node candidates for broad phase collision with the surface
};

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

// isMaxCoord: true when computing maxCoords, false when computing minCoords
Node coord_to_grid(double x, double y, double z, const std::array<double, 6> &bounds,
                   size_t nbx, size_t nby, size_t nbz, bool isMaxCoord = false)
{
    // Calculate relative position in each dimension (0.0 to 1.0)
    double rx = (bounds[XMAX] > bounds[XMIN])
                    ? (x - bounds[XMIN]) / (bounds[XMAX] - bounds[XMIN])
                    : 0.5;
    double ry = (bounds[YMAX] > bounds[YMIN])
                    ? (y - bounds[YMIN]) / (bounds[YMAX] - bounds[YMIN])
                    : 0.5;
    double rz = (bounds[ZMAX] > bounds[ZMIN])
                    ? (z - bounds[ZMIN]) / (bounds[ZMAX] - bounds[ZMIN])
                    : 0.5;
    
    // Check for coordinates exactly at the max boundary
    bool xAtMax = (std::abs(rx - 1.0) < 1e-10);
    bool yAtMax = (std::abs(ry - 1.0) < 1e-10);
    bool zAtMax = (std::abs(rz - 1.0) < 1e-10);
    
    // Handle wrapping for values outside the bounds
    rx = rx - floor(rx);
    ry = ry - floor(ry);
    rz = rz - floor(rz);
    
    // Convert to grid indices, special handling for exact boundaries based on min/max
    size_t ix, iy, iz;
    
    if (isMaxCoord) {
        // For maxCoords: If at max boundary, use last cell
        ix = xAtMax ? (nbx - 1) : static_cast<size_t>(rx * nbx);
        iy = yAtMax ? (nby - 1) : static_cast<size_t>(ry * nby);
        iz = zAtMax ? (nbz - 1) : static_cast<size_t>(rz * nbz);
    } else {
        // For minCoords: Always use wrap-around behavior
        ix = static_cast<size_t>(rx * nbx);
        iy = static_cast<size_t>(ry * nby);
        iz = static_cast<size_t>(rz * nbz);
    }
    
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


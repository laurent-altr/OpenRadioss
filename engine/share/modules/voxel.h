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
inline void swap_and_pop(std::vector<T>& vec, const T& value) {
    // finds the value, swaps it with the last element, and pops the last element 
    auto it = std::find(vec.begin(), vec.end(), value);
    if (it != vec.end()) {
        std::swap(*it, vec.back());
        vec.pop_back();
    }
}

class GridMapper {
private:
    // Grid dimensions
    const size_t nbx, nby, nbz;
    
    // Boundary values
    const double x_min, y_min, z_min;
    const double x_max, y_max, z_max;
    
    // Precomputed scaling factors for faster index calculation
    const double x_scale, y_scale, z_scale;
    
public:
    
    GridMapper(const std::array<double, 6>& bounds, size_t nbx_, size_t nby_, size_t nbz_)
        : nbx(nbx_), nby(nby_), nbz(nbz_),
          x_min(bounds[XMIN]), y_min(bounds[YMIN]), z_min(bounds[ZMIN]),
          x_max(bounds[XMAX]), y_max(bounds[YMAX]), z_max(bounds[ZMAX]),
          // Precompute scaling factors in the constructor, no validity check needed
          x_scale(nbx / (x_max - x_min)),
          y_scale(nby / (y_max - y_min)),
          z_scale(nbz / (z_max - z_min))
    {}
    
    // For minimum coordinates
    Node inline mapMin(double x, double y, double z) const {
        // Calculate relative position in each dimension (0.0 to 1.0)
        double rx = (x - x_min) / (x_max - x_min);
        double ry = (y - y_min) / (y_max - y_min);
        double rz = (z - z_min) / (z_max - z_min);
        
        // Handle wrapping for values outside the bounds
        rx = rx - floor(rx);
        ry = ry - floor(ry);
        rz = rz - floor(rz);
        
        // Convert to grid indices
        size_t ix = static_cast<size_t>(rx * nbx);
        size_t iy = static_cast<size_t>(ry * nby);
        size_t iz = static_cast<size_t>(rz * nbz);
        
        // Ensure indices are within bounds
        ix = (ix >= nbx) ? nbx - 1 : ix;
        iy = (iy >= nby) ? nby - 1 : iy;
        iz = (iz >= nbz) ? nbz - 1 : iz;
        
        return {
            static_cast<short int>(ix),
            static_cast<short int>(iy),
            static_cast<short int>(iz)
        };
    }
    
    // For maximum coordinates
    Node inline mapMax(double x, double y, double z) const {
        // Calculate relative position in each dimension (0.0 to 1.0)
        double rx = (x - x_min) / (x_max - x_min);
        double ry = (y - y_min) / (y_max - y_min);
        double rz = (z - z_min) / (z_max - z_min);
        
        // Check for coordinates exactly at the max boundary
        bool xAtMax = (std::abs(rx - 1.0) < 1e-10);
        bool yAtMax = (std::abs(ry - 1.0) < 1e-10);
        bool zAtMax = (std::abs(rz - 1.0) < 1e-10);
        
        // Handle wrapping for values outside the bounds
        rx = rx - floor(rx);
        ry = ry - floor(ry);
        rz = rz - floor(rz);
        
        // Convert to grid indices with special handling for max boundary
        size_t ix = xAtMax ? (nbx - 1) : static_cast<size_t>(rx * nbx);
        size_t iy = yAtMax ? (nby - 1) : static_cast<size_t>(ry * nby);
        size_t iz = zAtMax ? (nbz - 1) : static_cast<size_t>(rz * nbz);
        
        // Ensure indices are within bounds
        ix = (ix >= nbx) ? nbx - 1 : ix;
        iy = (iy >= nby) ? nby - 1 : iy;
        iz = (iz >= nbz) ? nbz - 1 : iz;
        
        return {
            static_cast<short int>(ix),
            static_cast<short int>(iy),
            static_cast<short int>(iz)
        };
    }
    
    // Optimized function to convert coordinates directly to linear index
    // For use when coordinates are guaranteed to be within the domain
    size_t inline toIndex(double x, double y, double z) const {
        // Calculate grid indices directly using precomputed scaling factors
        size_t ix = static_cast<size_t>((x - x_min) * x_scale);
        size_t iy = static_cast<size_t>((y - y_min) * y_scale);
        size_t iz = static_cast<size_t>((z - z_min) * z_scale);
        
        // Ensure indices are within bounds (for safety against floating-point precision issues)
        ix = (ix >= nbx) ? nbx - 1 : ix;
        iy = (iy >= nby) ? nby - 1 : iy;
        iz = (iz >= nbz) ? nbz - 1 : iz;
        
        // Convert to linear index
        return ix + iy * nbx + iz * nbx * nby;
    }
    
    // Faster version without bounds checking - use only when you're absolutely sure
    // the coordinates are within domain and precision issues won't cause problems
    size_t inline toIndexUnsafe(double x, double y, double z) const {
        size_t ix = static_cast<size_t>((x - x_min) * x_scale);
        size_t iy = static_cast<size_t>((y - y_min) * y_scale);
        size_t iz = static_cast<size_t>((z - z_min) * z_scale);
        
        return ix + iy * nbx + iz * nbx * nby;
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
    //std::unordered_map<size_t, Cell> cells;          // map of nodes to their index in the vector
    std::vector<Cell> cells;                            // vector of cells
    std::vector<Node> nodes;                         // vector of secondary nodes
    std::vector<Surf> surfaceBounds;                 // vector of main surfaceBounds
    std::vector<std::array<int,4>> surfaceNodes;      // vector of secondary surfaceBounds
    std::vector<std::vector<int>> surfaceCandidates; // vector of secondary node candidates for broad phase collision with the surface
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

size_t inline coord_to_index(double x, double y, double z, const std::array<double, 6> &bounds,
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



size_t inline coord_to_index(Node coord, const std::array<double, 6> &bounds,
                      size_t nbx, size_t nby, size_t nbz)
{
    // cast to size_t
    size_t x = static_cast<size_t>(coord[0]);
    size_t y = static_cast<size_t>(coord[1]);
    size_t z = static_cast<size_t>(coord[2]);
    return COORD_TO_INDEX(x, y, z, nbx, nby);
}

size_t inline coord_to_index(short int x, short int y, short int z, const std::array<double, 6> &bounds,
                      size_t nbx, size_t nby, size_t nbz)
{
    return COORD_TO_INDEX(static_cast<size_t>(x), static_cast<size_t>(y), static_cast<size_t>(z), nbx, nby);
}

Node inline index_to_coord(size_t index, size_t nbx, size_t nby, size_t nbz)
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


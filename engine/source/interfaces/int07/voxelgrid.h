#ifndef VOXEL_H
#define VOXEL_H

#include "bitarray.h"
#include <mutex>
#include <memory>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <unordered_map>
#include <vector>
class VoxelGrid
{
public:
    // Delete the copy constructor and assignment operator to prevent copying
    VoxelGrid(const VoxelGrid &) = delete;
    VoxelGrid &operator=(const VoxelGrid &) = delete;

    // Static method to access the single instance of VoxelGrid
    static VoxelGrid &getInstance(size_t numInts, size_t xDim, size_t yDim, size_t zDim)
    {
        std::call_once(initInstanceFlag, [&]()
                       { instance.reset(new VoxelGrid(numInts, xDim, yDim, zDim)); });
        return *instance;
    }

    void resize(size_t x, size_t y, size_t z, size_t map_max_size)
    {
        clear();
        xSize = x;
        ySize = y;
        zSize = z;
        cells.reserve(map_max_size);
    }

    void finalize()
    {

        cells.rehash(cells.size()*2);
    }

    void analyze_collisions()
    {
        size_t bucket_count = cells.bucket_count();
        size_t total_elements = cells.size();
        size_t collisions = 0;

        for (size_t i = 0; i < bucket_count; ++i)
        {
            size_t bucket_size = cells.bucket_size(i);
            if (bucket_size > 1)
            {
                collisions += (bucket_size - 1); // Count collisions (extra elements in the bucket)
            }
        }

        double load_factor = cells.load_factor();

        std::cout << "Total buckets: " << bucket_count << std::endl;
        std::cout << "Total elements: " << total_elements << std::endl;
        std::cout << "Total collisions: " << collisions << std::endl;
        std::cout << "Load factor: " << load_factor << std::endl;
        std::cout << "Average bucket size: " << (double)total_elements / bucket_count << std::endl;
    }

    // Set a bit using 3D coordinates
    inline void setBit(size_t x, size_t y, size_t z, bool value)
    {
        const size_t index = convert3DTo1D(x, y, z);
        bitArray.setBit(index, value);
    }
    inline bool getBits(size_t x, size_t y, size_t z, size_t number) const
    {
        const size_t index = convert3DTo1D(x, y, z);
        return bitArray.getBits(index, number);
    }

    // Get a bit using 3D coordinates
    inline bool getBit(size_t x, size_t y, size_t z) const
    {
        const size_t index = convert3DTo1D(x, y, z);
        return bitArray.getBit(index);
    }

    // Set a byte using 3D coordinates (starting at a specific 3D index)
    inline void setByte(size_t x, size_t y, size_t z, uint8_t value)
    {
        const size_t byteIndex = convert3DTo1D_coarse(x, y, z);
        bitArray.setByte(byteIndex, value);
    }

    // Get a byte using 3D coordinates (starting at a specific 3D index)
    inline const uint8_t &getByte(size_t x, size_t y, size_t z) const
    {
        const size_t byteIndex = convert3DTo1D_coarse(x, y, z);
        return bitArray.getByte(byteIndex);
    }

    // Get 8 bits corresponding to a 2x2x2 block
    inline const uint8_t &get8bits(size_t x, size_t y, size_t z) const
    {
        const size_t byteIndex = convert3DTo1D_coarse(x, y, z);              
        return bitArray.getByte(byteIndex);
    }

    // Get 64 bits corresponding to an 8x8x8 block
    // The memcopy can be avoided if the BitArray is stored as an array of uint64_t instead of uint8_t
    // or if we take care of the memory alignment
    inline uint64_t get64bits(size_t x, size_t y, size_t z) const
    {
        const size_t byteIndex = convert3DTo1D(x * 8, y * 8, z * 8) / 8;
        return *reinterpret_cast<const uint64_t *>(bitArray.getPointer() + byteIndex);
    }

    // Get the vector of vertex indices in a cell
    inline const std::vector<int> &getCell(size_t x, size_t y, size_t z) const
    {
        const size_t cell_id = convert3DTo1D(x, y, z);
        //const auto& it =cells[cell_id];
        //return it;
        return cells.at(cell_id);

    }
    // Get the vector of vertex indices in a cell
    const std::vector<int> &getCell(size_t cell_id) const
    {
        auto it = cells.find(cell_id);
        if (it != cells.end())
        {
            return it->second;
        }
        else
        {
            return empty_vector;
        }
    }

    // Function to add a vertex to a cell and update the bitArray
    void addVertexToCell(size_t x, size_t y, size_t z, int vertex_id)
    {
        const size_t cell_id = convert3DTo1D(x, y, z);

        auto it = cells.find(cell_id);
        if (it != cells.end())
        {
            it->second.push_back(vertex_id);
        }
        else
        {
            cells[cell_id] = {vertex_id};
        }
        updateBitArray(cell_id);
    }

    void addVerticesToCell(size_t x, size_t y, size_t z, std::vector<int> &&vertex_ids)
    {
        const size_t cell_id = convert3DTo1D(x, y, z);
        // Check the cache
        auto it = cells.find(cell_id);
        if (it != cells.end())
        {
            it->second.insert(it->second.end(), std::make_move_iterator(vertex_ids.begin()), std::make_move_iterator(vertex_ids.end()));
        }
        else
        {
            cells[cell_id] = std::move(vertex_ids);
        }
        // Update the bitArray based on the current state of the cell
        updateBitArray(cell_id);
    }

    // Public method to reset the memory
    void clear()
    {
        // Loop over all keys in the cells map and reset corresponding bits in bitArray
        for (const auto &pair : cells)
        {
            size_t cell_id = pair.first;
            bitArray.unSetBit(cell_id);
        }
        // Clear the cells map
        cells.clear();

    }

private:
    VoxelGrid(size_t numInts, size_t xDim, size_t yDim, size_t zDim)
        : bitArray(numInts),
          xSize(xDim), ySize(yDim), zSize(zDim) {
          halfXSize = xDim >> 1;
          halfYSize = yDim >> 1;
          halfXYSize = halfXSize * halfYSize;
          }

    // Static instance pointer and flag for thread-safe initialization
    static std::unique_ptr<VoxelGrid> instance;
    static std::once_flag initInstanceFlag;
    static std::once_flag print_collisions;

    BitArray bitArray;                                  // Private BitArray instance for managing bits
    std::unordered_map<size_t, std::vector<int>> cells; // A cell contains the vector of the vertex indices it contains
    static const std::vector<int> empty_vector;         // Empty vector to return when the cell is not found

    size_t xSize, ySize, zSize; // Dimensions of the VoxelGrid
    // Precompute the shifts and multiplications
    size_t halfXSize;
    size_t halfYSize;
    size_t halfXYSize;


//    inline size_t convert3DTo1D(size_t x, size_t y, size_t z) const
//    {
//        return z * (xSize * ySize) + y * xSize + x;
//    }
    inline size_t convert3DTo1D(size_t x, size_t y, size_t z) const
    {
    
        // Calculate the block coordinates and fine block coordinates in one step
        const size_t blockX = x >> 1;
        const size_t blockY = y >> 1;
        const size_t blockZ = z >> 1;
        const size_t fineX = x & 0x1;
        const size_t fineY = y & 0x1;
        const size_t fineZ = z & 0x1;
    
        // Combine the block index and fine block index calculations
        const size_t blockIndex = (blockZ * halfXYSize) + (blockY * halfXSize) + blockX;
        const size_t fineBlockIndex = (fineZ << 2) | (fineY << 1) | fineX;
    
        // Combine the block index and fine block index to get the final 1D index
        const size_t cell_id = (blockIndex << 3) + fineBlockIndex;
        return cell_id;
    }

    // input are in coarse coordinates
    inline size_t convert3DTo1D_coarse(size_t x, size_t y, size_t z) const
    {
        return z * (halfXYSize) + y * halfXSize + x;
    }


    // Efficiently reset the grid by only clearing bits that correspond to non-empty cells
    void reset()
    {
        // Loop over all keys in the cells map and reset corresponding bits in bitArray
        for (const auto &pair : cells)
        {
            size_t cell_id = pair.first;
            bitArray.unSetBit(cell_id);
        }
        // Clear the cells map
        cells.clear();
    }

    // Helper function to update the bitArray when a cell becomes non-empty
    void updateBitArray(size_t cell_id)
    {
        if (cells[cell_id].empty())
        {
            // If the cell becomes empty, remove it from the map and unset the bit
            cells.erase(cell_id);
            bitArray.unSetBit(cell_id);
        }
        else
        {
            // If the cell is not empty, set the bit
            bitArray.setBit(cell_id);
        }
    }
};

const std::vector<int> VoxelGrid::empty_vector = {};
// Initialize the static members
std::unique_ptr<VoxelGrid> VoxelGrid::instance = nullptr;
std::once_flag VoxelGrid::initInstanceFlag;
std::once_flag VoxelGrid::print_collisions;

#endif // VOXEL_H

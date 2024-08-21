#ifndef VOXEL_H
#define VOXEL_H

#include "bitarray.h"
#include <mutex>
#include <memory>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <map>
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
    }

    void finalize()
    {
        // rehash the cells
        //cells.max_load_factor(0.5); // Lower the load factor to reduce collisions
    //    cells.rehash(cells.size()*2);
     //   std::call_once(print_collisions, [&]()
     //                  { analyze_collisions(); });
    }

//    void analyze_collisions()
//    {
//        size_t bucket_count = cells.bucket_count();
//        size_t total_elements = cells.size();
//        size_t collisions = 0;
//
//        for (size_t i = 0; i < bucket_count; ++i)
//        {
//            size_t bucket_size = cells.bucket_size(i);
//            if (bucket_size > 1)
//            {
//                collisions += (bucket_size - 1); // Count collisions (extra elements in the bucket)
//            }
//        }
//
//        double load_factor = cells.load_factor();
//
//        std::cout << "Total buckets: " << bucket_count << std::endl;
//        std::cout << "Total elements: " << total_elements << std::endl;
//        std::cout << "Total collisions: " << collisions << std::endl;
//        std::cout << "Load factor: " << load_factor << std::endl;
//        std::cout << "Average bucket size: " << (double)total_elements / bucket_count << std::endl;
//    }

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
        const size_t byteIndex = convert3DTo1D(x, y, z) / 8;
        bitArray.setByte(byteIndex, value);
    }

    // Get a byte using 3D coordinates (starting at a specific 3D index)
    inline const uint8_t &getByte(size_t x, size_t y, size_t z) const
    {
        const size_t byteIndex = convert3DTo1D(x, y, z) / 8;
        return bitArray.getByte(byteIndex);
    }

    // Get 8 bits corresponding to a 2x2x2 block
    inline const uint8_t &get8bits(size_t x, size_t y, size_t z) const
    {
        const size_t byteIndex = convert3DTo1D(x * 2, y * 2, z * 2) / 8;
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

        // Check the cache
        // if (cell_id == lastCellID && lastCellPtr) {
        if (false)
        {
            lastCellPtr->push_back(vertex_id);
            lastCellPtr = &cells[cell_id]; // Re-assign after potential reallocation
        }
        else
        {
            auto it = cells.find(cell_id);
            if (it != cells.end())
            {
                it->second.push_back(vertex_id);
                // Update the cache
                lastCellID = cell_id;
                lastCellPtr = &it->second;
            }
            else
            {
                cells[cell_id] = {vertex_id};
                // Update the cache
                lastCellID = cell_id;
                lastCellPtr = &cells[cell_id];
            }
        }

        // Update the bitArray based on the current state of the cell
        updateBitArray(cell_id);
    }

    void addVerticesToCell(size_t x, size_t y, size_t z, std::vector<int> &&vertex_ids)
    {
        const size_t cell_id = convert3DTo1D(x, y, z);
        // Check the cache
        if (false)
        {
            lastCellPtr->insert(lastCellPtr->end(), std::make_move_iterator(vertex_ids.begin()), std::make_move_iterator(vertex_ids.end()));
        }
        else
        {
            auto it = cells.find(cell_id);
            if (it != cells.end())
            {
                it->second.insert(it->second.end(), std::make_move_iterator(vertex_ids.begin()), std::make_move_iterator(vertex_ids.end()));
                // Update the cache
                lastCellID = cell_id;
                lastCellPtr = &it->second;
            }
            else
            {
                cells[cell_id] = std::move(vertex_ids);
                // Update the cache
                lastCellID = cell_id;
                lastCellPtr = &cells[cell_id];
            }
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

        // Reset the cache
        lastCellID = 0;
        lastCellPtr = nullptr;
    }
      std::vector<int> getChunk(size_t xMin, size_t xMax, size_t y, size_t z) const {

        size_t minKey = convert3DTo1D(xMin,y,z);
        size_t maxKey = convert3DTo1D(xMax,y,z);
        std::vector<int> result;

        // Find the first key that is not less than minKey
        auto it = cells.lower_bound(minKey);

        // Iterate through the map, concatenating vectors within the specified range
        while (it != cells.end() && it->first <= maxKey) {
            result.insert(result.end(), it->second.begin(), it->second.end());
            ++it;
        }

        // Move the result to avoid copying
        return std::move(result);
    }

private:
    VoxelGrid(size_t numInts, size_t xDim, size_t yDim, size_t zDim)
        : bitArray(numInts),
          lastCellID(0), lastCellPtr(nullptr),
          xSize(xDim), ySize(yDim), zSize(zDim) {}

    // Static instance pointer and flag for thread-safe initialization
    static std::unique_ptr<VoxelGrid> instance;
    static std::once_flag initInstanceFlag;
    static std::once_flag print_collisions;



    BitArray bitArray;                                  // Private BitArray instance for managing bits
    std::map<size_t, std::vector<int>> cells; // A cell contains the vector of the vertex indices it contains
    static const std::vector<int> empty_vector;         // Empty vector to return when the cell is not found

    // Cache for the last accessed cell
    mutable size_t lastCellID;             // Last accessed cell ID
    mutable std::vector<int> *lastCellPtr; // Pointer to the last accessed cell's vector

    size_t xSize, ySize, zSize; // Dimensions of the VoxelGrid

    // Convert 3D coordinates to a 1D index where 64 consecutive bits correspond to an 8x8x8 block
    //    inline size_t convert3DTo1D(size_t x, size_t y, size_t z) const {
    //        // Calculate the coarse block coordinates (8x8x8 blocks)
    //        const size_t blockX = x / 8;
    //        const size_t blockY = y / 8;
    //        const size_t blockZ = z / 8;
    //
    //        // Calculate the fine block coordinates within the 8x8x8 block (2x2x2 sub-block)
    //        const size_t fineX = (x % 8) / 2;
    //        const size_t fineY = (y % 8) / 2;
    //        const size_t fineZ = (z % 8) / 2;
    //
    //        // Calculate the bit position within the 2x2x2 sub-block
    //        const size_t bitX = x % 2;
    //        const size_t bitY = y % 2;
    //        const size_t bitZ = z % 2;
    //
    //        // Coarse block index in the grid
    //        const size_t blockIndex = (blockZ * (ySize / 8) * (xSize / 8)) + (blockY * (xSize / 8)) + blockX;
    //
    //        // Fine block index within the 8x8x8 block (each block is 64 bits, or 8 * 2x2x2 blocks)
    //        const size_t fineBlockIndex = (fineZ * 4) + (fineY * 2) + fineX;
    //
    //        // Bit index within the 2x2x2 sub-block (using Z-order curve or Morton order)
    //        const size_t bitIndex = (bitZ << 2) | (bitY << 1) | bitX;
    //
    //        // Combine the block index, fine block index, and bit index to get the final 1D index
    //        return (blockIndex * 64) + (fineBlockIndex * 8) + bitIndex;
    //    }

    inline size_t convert3DTo1D(size_t x, size_t y, size_t z) const
    {
        return z * (xSize * ySize) + y * xSize + x;
    }
    inline size_t convert3DTo1D_test(size_t x, size_t y, size_t z) const
    {
        // Calculate the coarse block coordinates (4x4x4 blocks)
        const size_t blockX = x >> 2; // Equivalent to x / 4
        const size_t blockY = y >> 2; // Equivalent to y / 4
        const size_t blockZ = z >> 2; // Equivalent to z / 4

        // Calculate the fine block coordinates within the 4x4x4 block (2x2x2 sub-block)
        const size_t fineX = (x >> 1) & 0x1; // Extract the bit corresponding to (x % 4) / 2
        const size_t fineY = (y >> 1) & 0x1; // Extract the bit corresponding to (y % 4) / 2
        const size_t fineZ = (z >> 1) & 0x1; // Extract the bit corresponding to (z % 4) / 2

        // Calculate the bit position within the 2x2x2 sub-block
        const size_t bitX = x & 0x1; // Extract the least significant bit (x % 2)
        const size_t bitY = y & 0x1; // Extract the least significant bit (y % 2)
        const size_t bitZ = z & 0x1; // Extract the least significant bit (z % 2)

        // Coarse block index in the grid
        const size_t blockIndex = (blockZ * (ySize >> 2) * (xSize >> 2)) + (blockY * (xSize >> 2)) + blockX;

        // Fine block index within the 4x4x4 block (each block is 64 bits, or 8 * 2x2x2 blocks)
        const size_t fineBlockIndex = (fineZ << 2) | (fineY << 1) | fineX;

        // Bit index within the 2x2x2 sub-block (using Z-order curve or Morton order)
        const size_t bitIndex = (bitZ << 2) | (bitY << 1) | bitX;

        // Combine the block index, fine block index, and bit index to get the final 1D index
        const size_t cell_id = (blockIndex << 6) + (fineBlockIndex << 3) + bitIndex;
        return cell_id;
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

        // Reset the cache
        lastCellID = 0;
        lastCellPtr = nullptr;
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

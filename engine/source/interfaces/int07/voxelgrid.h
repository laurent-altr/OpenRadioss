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
    VoxelGrid(const VoxelGrid&) = delete;
    VoxelGrid& operator=(const VoxelGrid&) = delete;

    // Static method to access the single instance of VoxelGrid
    static VoxelGrid& getInstance(size_t numInts, size_t xDim, size_t yDim, size_t zDim) {
        std::call_once(initInstanceFlag, [&](){
            instance.reset(new VoxelGrid(numInts, xDim, yDim, zDim));
        });
        return *instance;
    }


    // Set a bit using 3D coordinates
    inline void setBit(size_t x, size_t y, size_t z, bool value)
    {
        const size_t index = convert3DTo1D(x, y, z);
        bitArray.setBit(index, value);
    }

    // Get a bit using 3D coordinates
    inline bool getBit(size_t x, size_t y, size_t z) const
    {
        const size_t index = convert3DTo1D(x, y, z);
        std::cout << "Index: " << index << std::endl;
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
    const std::vector<int> &getCell(size_t x, size_t y, size_t z) const
    {
        const size_t cell_id = convert3DTo1D(x, y, z);
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
    void addVertexToCell(size_t x, size_t y, size_t z, int vertex_id) {
        const size_t cell_id = convert3DTo1D(x, y, z);

        // Check the cache
        if (cell_id == lastCellID && lastCellPtr) {
            lastCellPtr->push_back(vertex_id);
            lastCellPtr = &cells[cell_id]; // Re-assign after potential reallocation
        } else {
            auto it = cells.find(cell_id);
            if (it != cells.end()) {
                it->second.push_back(vertex_id);
                // Update the cache
                lastCellID = cell_id;
                lastCellPtr = &it->second;
            } else {
                cells[cell_id] = {vertex_id};
                // Update the cache
                lastCellID = cell_id;
                lastCellPtr = &cells[cell_id];
            }
        }

        // Update the bitArray based on the current state of the cell
        updateBitArray(cell_id);
    }

    void addVerticesToCell(size_t x, size_t y, size_t z, std::vector<int>&& vertex_ids) {
        const size_t cell_id = convert3DTo1D(x, y, z);
        // Check the cache
        if (cell_id == lastCellID && lastCellPtr) {
            lastCellPtr->insert(lastCellPtr->end(), std::make_move_iterator(vertex_ids.begin()), std::make_move_iterator(vertex_ids.end()));
        } else {
            auto it = cells.find(cell_id);
            if (it != cells.end()) {
                it->second.insert(it->second.end(), std::make_move_iterator(vertex_ids.begin()), std::make_move_iterator(vertex_ids.end()));
                // Update the cache
                lastCellID = cell_id;
                lastCellPtr = &it->second;
            } else {
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
    void clear() {
        // Loop over all keys in the cells map and reset corresponding bits in bitArray
        for (const auto& pair : cells) {
            size_t cell_id = pair.first;
            bitArray.unSetBit(cell_id);
        }
        // Clear the cells map
        cells.clear();

        // Reset the cache
        lastCellID = 0;
        lastCellPtr = nullptr;
    }


private:
    VoxelGrid(size_t numInts, size_t xDim, size_t yDim, size_t zDim)
        : bitArray(numInts),
          lastCellID(0), lastCellPtr(nullptr),
          xSize(xDim), ySize(yDim), zSize(zDim) {}

    // Static instance pointer and flag for thread-safe initialization
    static std::unique_ptr<VoxelGrid> instance;
    static std::once_flag initInstanceFlag;

    BitArray bitArray;                                  // Private BitArray instance for managing bits
    std::unordered_map<size_t, std::vector<int>> cells; // A cell contains the vector of the vertex indices it contains
    static const std::vector<int> empty_vector;         // Empty vector to return when the cell is not found

    // Cache for the last accessed cell
    mutable size_t lastCellID;  // Last accessed cell ID
    mutable std::vector<int>* lastCellPtr;  // Pointer to the last accessed cell's vector

    const size_t xSize, ySize, zSize; // Dimensions of the VoxelGrid

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


inline size_t convert3DTo1D(size_t x, size_t y, size_t z) const {
    // Calculate the coarse block coordinates (4x4x4 blocks)
    const size_t blockX = x >> 2;  // Equivalent to x / 4
    const size_t blockY = y >> 2;  // Equivalent to y / 4
    const size_t blockZ = z >> 2;  // Equivalent to z / 4

    // Calculate the fine block coordinates within the 4x4x4 block (2x2x2 sub-block)
    const size_t fineX = (x >> 1) & 0x1;  // Extract the bit corresponding to (x % 4) / 2
    const size_t fineY = (y >> 1) & 0x1;  // Extract the bit corresponding to (y % 4) / 2
    const size_t fineZ = (z >> 1) & 0x1;  // Extract the bit corresponding to (z % 4) / 2

    // Calculate the bit position within the 2x2x2 sub-block
    const size_t bitX = x & 0x1;  // Extract the least significant bit (x % 2)
    const size_t bitY = y & 0x1;  // Extract the least significant bit (y % 2)
    const size_t bitZ = z & 0x1;  // Extract the least significant bit (z % 2)


//    std::cout<<"BlockX: "<<blockX<<" BlockY: "<<blockY<<" BlockZ: "<<blockZ<<std::endl;
//    std::cout<<"FineX: "<<fineX<<" FineY: "<<fineY<<" FineZ: "<<fineZ<<std::endl;
//    std::cout<<"BitX: "<<bitX<<" BitY: "<<bitY<<" BitZ: "<<bitZ<<std::endl;
    // global id ; block x, block y , block z ; fine x, fine y, fine z ; bit x, bit y, bit z
    // 0         ;       0,        0,       0 ;      0,       0,      0; 0,      0,      0
    // 1         ;       0,        0,       0 ;      0,       0,      0; 1,      0,      0
    // 2         ;       0,        0,       0 ;      0,       0,      0; 0,      1,      0
    // 3         ;       0,        0,       0 ;      0,       0,      0; 1,      1,      0
    // 4         ;       0,        0,       0 ;      0,       0,      0; 0,      0,      1                                                                 
    // 5         ;       0,        0,       0 ;      0,       0,      0; 1,      0,      1
    // 6         ;       0,        0,       0 ;      0,       0,      0; 0,      1,      1
    // 7         ;       0,        0,       0 ;      0,       0,      0; 1,      1,      1
    // 8         ;       0,        0,       0 ;      0,       0,      0; 0,      0,      0
    // 9         ;       0,        0,       0 ;      0,       0,      0; 1,      0,      0
    // 10        ;       0,        0,       0 ;      0,       0,      0; 0,      1,      0
    // 64        ;       1,        0,       0 ;      0,       0,      0; 0,      0,      0
    // 65        ;       1,        0,       0 ;      0,       0,      0; 1,      0,      0

    // Coarse block index in the grid
    const size_t blockIndex = (blockZ * (ySize >> 2) * (xSize >> 2)) + (blockY * (xSize >> 2)) + blockX;

    // Fine block index within the 4x4x4 block (each block is 64 bits, or 8 * 2x2x2 blocks)
    const size_t fineBlockIndex = (fineZ << 2) | (fineY << 1) | fineX;

    // Bit index within the 2x2x2 sub-block (using Z-order curve or Morton order)
    const size_t bitIndex = (bitZ << 2) | (bitY << 1) | bitX;

    // Combine the block index, fine block index, and bit index to get the final 1D index
    return (blockIndex << 6) + (fineBlockIndex << 3) + bitIndex;
}


    // Efficiently reset the grid by only clearing bits that correspond to non-empty cells
    void reset() {
        // Loop over all keys in the cells map and reset corresponding bits in bitArray
        for (const auto& pair : cells) {
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
    void updateBitArray(size_t cell_id) {
        if (cells[cell_id].empty()) {
            // If the cell becomes empty, remove it from the map and unset the bit
            cells.erase(cell_id);
            bitArray.unSetBit(cell_id);
        } else {
            // If the cell is not empty, set the bit
            bitArray.setBit(cell_id);
        }
    }


};

    
const std::vector<int> VoxelGrid::empty_vector = {};
// Initialize the static members
std::unique_ptr<VoxelGrid> VoxelGrid::instance = nullptr;
std::once_flag VoxelGrid::initInstanceFlag;

#endif // VOXEL_H

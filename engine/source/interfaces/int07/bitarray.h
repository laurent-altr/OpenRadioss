#ifndef BITARRAY_H
#define BITARRAY_H

#include <cstdint>
#include <iostream>
#include <cstring>

class BitArray {
public:
    // Constructor that accepts an external memory block (int array) and its size
    BitArray(int* memory, size_t numInts)
        : bits(reinterpret_cast<uint8_t*>(memory)),
          numBits(numInts * sizeof(int) * 8),
          ownsMemory(false) {}

    // New constructor that allocates memory and sets all bits to 0
    BitArray(size_t num)
        : numBits(num), ownsMemory(true) {
        // Allocate memory for the bit array
        size_t numBytes = (num + 7) / 8;  // Calculate the number of bytes needed
        bits = new uint8_t[num];

        // Initialize all bits to 0
        std::memset(bits, 0, numBytes);
    }

    // Destructor to free allocated memory if it was allocated by this class
    ~BitArray() {
        if (ownsMemory) {
            delete[] bits;
        }
    }

    // Retrieves the bit value at the specified index
    inline bool getBit(size_t index) const {
        const size_t byteIndex = index / 8;
        const size_t bitOffset = index % 8;
        const bool result = bits[byteIndex] & (static_cast<uint8_t>(1) << bitOffset);
        std::cout<<"ByteIndex: "<<byteIndex<<" BitOffset: "<<bitOffset<<" Result: "<<result<<std::endl;
        return result;
    }

    // Sets or clears the bit at the specified index
    inline void setBit(size_t index, bool value) {
        const size_t byteIndex = index / 8;
        const size_t bitOffset = index % 8;
        const uint8_t mask = static_cast<uint8_t>(1 << bitOffset);
        if (value) {
            bits[byteIndex] |= mask;
        } else {
            bits[byteIndex] &= ~mask;
        }
    }

    inline void setBit(size_t index) {
        const size_t byteIndex = index / 8;
        const size_t bitOffset = index % 8;
        const uint8_t mask = static_cast<uint8_t>(1 << bitOffset);
        bits[byteIndex] |= mask;
    }

    inline void unSetBit(size_t index) {
        const size_t byteIndex = index / 8;
        const size_t bitOffset = index % 8;
        const uint8_t mask = static_cast<uint8_t>(1 << bitOffset);
        bits[byteIndex] &= ~mask;
    }

    // Retrieves the byte value at the specified byte index
    inline const uint8_t& getByte(size_t byteIndex) const {
        return bits[byteIndex];
    }

    // Sets the byte value at the specified byte index
    inline void setByte(size_t byteIndex, uint8_t value) {
        bits[byteIndex] = value;
    }

    inline const uint8_t* getPointer() const {
        return bits;
    }

    // Prints the entire bit array in a human-readable format
    void print() const {
        for (size_t i = 0; i < numBits; ++i) {
            std::cout << getBit(i);
            if ((i + 1) % 32 == 0) std::cout << " ";
        }
        std::cout << std::endl;
    }

private:
    uint8_t* bits;       // Pointer to the underlying memory block
    size_t numBits;      // Total number of bits in the array
    bool ownsMemory;     // Flag to indicate whether this class owns the memory
};

#endif // BITARRAY_H

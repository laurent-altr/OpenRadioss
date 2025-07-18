#ifndef COUPLING_H
#define COUPLING_H

#include <string>
#include <vector>

// Abstract base class for coupling adapters
class CouplingAdapter {
private:
    int groupNodeId_; // the group node ID for this adapter. 
    int surfaceId_; // the surface ID for this adapter, not used yet

public:
    virtual ~CouplingAdapter() = default;
    
    // Configuration
    virtual bool configure(const std::string& configFile) = 0;
    virtual void setNodes(const std::vector<int>& nodeIds) = 0;
    
    // Initialization
    virtual bool initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) = 0;
    
    // Data exchange
    virtual void writeData(const double* values, int totalNodes, double dt, int dataType) = 0;
    virtual void readData(double* values, int totalNodes, double dt, int dataType) = 0;
    
    // Simulation control
    virtual void advance(double& dt) = 0;
    virtual bool isCouplingOngoing() const = 0;
    virtual bool requiresWritingCheckpoint() const {return false; };
    virtual bool requiresReadingCheckpoint() const {return false; };
    
    // Finalization
    virtual void finalize() = 0;
    
    // Getters
    virtual bool isActive() const = 0;
    virtual double getMaxTimeStepSize() const = 0;
    virtual int getNumberOfCouplingNodes() const = 0;
    int getGroupNodeId() const { return groupNodeId_; }
    void setGroupNodeId(int id) { groupNodeId_ = id; }
    virtual int getCommunicator() const { return 0; }

    // Data types that can be exchanged during the coupling process
    enum class DataType {
         NOTHING = 0,
         DISPLACEMENTS = 1,
         FORCES = 2,
         POSITIONS = 3,
         DATA_COUNT = 4 // Total number of data types
    };
    
    // What to do with the received data
    enum class Mode {
         SKIP = 0,
         REPLACE = 1, // For positions, replace the existing data
         ADD = 2      // For forces, we add to the existing data
     };

     // Helper functions to convert between strings and DataType enums
    static DataType stringToDataType(const std::string& str) {
        if (str == "DISPLACEMENTS") return DataType::DISPLACEMENTS;
        if (str == "FORCES") return DataType::FORCES;
        if (str == "POSITIONS") return DataType::POSITIONS;
        return DataType::NOTHING;
    }
    
    static std::string dataTypeToString(DataType type) {
        switch (type) {
            case DataType::DISPLACEMENTS: return "DISPLACEMENTS";
            case DataType::FORCES: return "FORCES";
            case DataType::POSITIONS: return "POSITIONS";
            case DataType::NOTHING: return "NOTHING";
            case DataType::DATA_COUNT: break; // Don't convert this
        }
        return "NOTHING";
    }
};

#endif

#ifndef PRECICE_COUPLING_ADAPTER_H
#define PRECICE_COUPLING_ADAPTER_H

#include "coupling.h"
#include <memory>
#include <array>

#ifdef WITH_PRECICE
#include "precice/precice.hpp"

class PreciceCouplingAdapter : public CouplingAdapter {
public:
    PreciceCouplingAdapter();
    ~PreciceCouplingAdapter() override;
    
    // Implement abstract interface
    bool configure(const std::string& configFile) override;
    void setNodes(const std::vector<int>& nodeIds) override;
    bool initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) override;
    void writeData(const double* values, int totalNodes, double dt, int dataType) override;
    void readData(double* values, int totalNodes, double dt, int dataType) override;
    void advance(double& dt) override;
    bool isCouplingOngoing() const override;
    bool requiresWritingCheckpoint() const override;
    bool requiresReadingCheckpoint() const override;
    void finalize() override;
    bool isActive() const override;
    double getMaxTimeStepSize() const override;
    int getNumberOfCouplingNodes() const override;

    struct CouplingData {
        bool isActive = false; // Whether this data type is active
        Mode mode = Mode::SKIP; // Mode for this data type
        std::vector<double> buffer;
    };

private:
    // Configuration data
    bool active_;
    std::string participantName_;
    std::string configFile_;
    std::string meshName_;

    std::array<CouplingData, static_cast<size_t>(DataType::DATA_COUNT)> readData_;
    std::array<CouplingData, static_cast<size_t>(DataType::DATA_COUNT)> writeData_;
    
    // Coupling data
    std::vector<int> couplingNodeIds_;  // Radioss node IDs
    std::vector<int> vertexIds_;        // preCICE vertex IDs
    double maxTimeStepSize_;
    std::unique_ptr<precice::Participant> precice_;
    
    // Helper functions
    void extractNodeData(const double* globalValues, int totalNodes, int dataType);
    void injectNodeData(double* globalValues, int totalNodes, int dataType);
};

#endif
#endif 

#ifndef CWIPI_COUPLING_ADAPTER_H
#define CWIPI_COUPLING_ADAPTER_H

#include "coupling.h"
#include <memory>
#include <array>

#ifdef WITH_CWIPI
#include "cwipi.h"
#include <mpi.h>

class CwipiCouplingAdapter : public CouplingAdapter {
public:
    CwipiCouplingAdapter();
    ~CwipiCouplingAdapter() override;
    
    // Implement abstract interface
    bool configure(const std::string& configFile) override;
    void setNodes(const std::vector<int>& nodeIds) override;
    void setMesh(const int* elem_node_offsets, const int* elem_node_indices, int num_elements);
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
        int sendRequest = -1;   // Track async send request
        int recvRequest = -1;   // Track async receive request
    };

private:
    // Configuration data
    bool active_;
    std::string applicationName_;
    std::string coupledAppName_;
    std::string couplingName_;
    std::string exchangeName_;
    int dimension_;
    double tolerance_;
    int order_;
    
    std::array<CouplingData, static_cast<size_t>(DataType::DATA_COUNT)> readData_;
    std::array<CouplingData, static_cast<size_t>(DataType::DATA_COUNT)> writeData_;
    
    // Coupling data
    std::vector<int> couplingNodeIds_;
    double maxTimeStepSize_;
    bool initialized_;
    
    // CWIPI specific data
    MPI_Comm localComm_;
    
    // Mesh connectivity storage
    std::vector<int> eltsConnecPointer_;
    std::vector<int> eltsConnec_;
    int numElements_;
    
    // Helper functions
    void extractNodeData(const double* globalValues, int totalNodes, int dataType);
    void injectNodeData(double* globalValues, int totalNodes, int dataType);
    std::string getFieldName(DataType type);
    int getTag(DataType type);
};

#endif
#endif

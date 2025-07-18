#ifndef DUMMY_COUPLING_ADAPTER_H
#define DUMMY_COUPLING_ADAPTER_H

#include "coupling.h"

// Dummy adapter for when no coupling library is available
class DummyCouplingAdapter : public CouplingAdapter {
public:
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
};

#endif // DUMMY_COUPLING_ADAPTER_H

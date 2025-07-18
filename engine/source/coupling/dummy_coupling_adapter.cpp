#include "dummy_coupling_adapter.h"
#include <iostream>

bool DummyCouplingAdapter::configure(const std::string& configFile) {
//    std::cout << "No coupling library available - coupling disabled" << std::endl;
    return true;
}

void DummyCouplingAdapter::setNodes(const std::vector<int>& nodeIds) {
    // Do nothing
}

bool DummyCouplingAdapter::initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) {
//    std::cout << "Dummy coupling adapter initialized (no actual coupling)" << std::endl;
    return true;
}

void DummyCouplingAdapter::writeData(const double* values, int totalNodes, double dt, int dataType) {
    // Do nothing
}

void DummyCouplingAdapter::readData(double* values, int totalNodes, double dt, int dataType) {
    // Do nothing
}

void DummyCouplingAdapter::advance(double& dt) {
    // Do nothing - dt remains unchanged
}

bool DummyCouplingAdapter::isCouplingOngoing() const {
    return false; // No coupling, so never ongoing
}

bool DummyCouplingAdapter::requiresWritingCheckpoint() const {
    return false;
}

bool DummyCouplingAdapter::requiresReadingCheckpoint() const {
    return false;
}

void DummyCouplingAdapter::finalize() {
    // Do nothing
}

bool DummyCouplingAdapter::isActive() const {
    return false;
}

double DummyCouplingAdapter::getMaxTimeStepSize() const {
    return 1e30; // No time step limit
}

int DummyCouplingAdapter::getNumberOfCouplingNodes() const {
    return 0;
}

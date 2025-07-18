#include "coupling_c_interface.h"
#include "coupling_factory.h"
#include "coupling.h"
#include <string>
#include <vector>

extern "C" {

void* coupling_adapter_create() {
    return createCouplingAdapter();
}

void coupling_adapter_destroy(void* adapter) {
    delete static_cast<CouplingAdapter*>(adapter);
}

int coupling_adapter_configure(void* adapter, const char* filename) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->configure(std::string(filename)) ? 1 : 0;
}

void coupling_adapter_set_nodes(void* adapter, const int* nodeIds, int numNodes) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    std::vector<int> nodes(nodeIds, nodeIds + numNodes);
    ca->setNodes(nodes);
}

int coupling_adapter_initialize(void* adapter, const double* coordinates, 
                               int totalNodes, int mpiRank, int mpiSize) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->initialize(coordinates, totalNodes, mpiRank, mpiSize) ? 1 : 0;
}

void coupling_adapter_write_data(void* adapter, const double* values, 
                                int totalNodes, double dt, int dataType) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    ca->writeData(values, totalNodes, dt, dataType);
}

void coupling_adapter_read_data(void* adapter, double* values, 
                               int totalNodes, double dt, int dataType) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    ca->readData(values, totalNodes, dt, dataType);
}

void coupling_adapter_advance(void* adapter, double* dt) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    ca->advance(*dt);
}

int coupling_adapter_is_coupling_ongoing(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->isCouplingOngoing() ? 1 : 0;
}

int coupling_adapter_requires_writing_checkpoint(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->requiresWritingCheckpoint() ? 1 : 0;
}

int coupling_adapter_requires_reading_checkpoint(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->requiresReadingCheckpoint() ? 1 : 0;
}

void coupling_adapter_finalize(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    ca->finalize();
}

int coupling_adapter_is_active(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->isActive() ? 1 : 0;
}

double coupling_adapter_get_max_time_step_size(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->getMaxTimeStepSize();
}

int coupling_adapter_get_num_coupling_nodes(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->getNumberOfCouplingNodes();
}

int coupling_adapter_get_group_node_id(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->getGroupNodeId();
}

int coupling_adapter_get_surface_id(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->getSurfaceId();
}

int coupling_adapter_get_communicator(void* adapter) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    return ca->getCommunicator();
}


void coupling_adapter_set_mesh(void* adapter, const int* elem_node_offsets, const int* elem_node_indices, int num_elements) {
    CouplingAdapter* ca = static_cast<CouplingAdapter*>(adapter);
    
    // Try to cast to CwipiCouplingAdapter to call setMesh
#ifdef WITH_CWIPI
    CwipiCouplingAdapter* cwipi_adapter = dynamic_cast<CwipiCouplingAdapter*>(ca);
    if (cwipi_adapter) {
        cwipi_adapter->setMesh(elem_node_offsets, elem_node_indices, num_elements);
        return;
    }
#endif
    
}

} // extern "C"

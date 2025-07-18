#include "cwipi_coupling_adapter.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>

#ifdef WITH_CWIPI

CwipiCouplingAdapter::CwipiCouplingAdapter() 
    : active_(false)
    , maxTimeStepSize_(0.0)
    , couplingId_(-1)
    , meshId_(-1)
    , initialized_(false)
{
}

CwipiCouplingAdapter::~CwipiCouplingAdapter() {
    if (active_) {
        finalize();
    }
}

bool CwipiCouplingAdapter::configure(const std::string& configFile) {
    std::ifstream file(configFile);
    if (!file.is_open()) {
        std::cout << "No " << configFile << " file found in the current directory" << std::endl;
        std::cout << "CWIPI will not be active" << std::endl;
        active_ = false;
        return false;
    }
    
    std::string line;
    while (std::getline(file, line)) {
        // Remove leading/trailing whitespace
        line.erase(0, line.find_first_not_of(" \t"));
        line.erase(line.find_last_not_of(" \t") + 1);
        
        if (line.empty() || line[0] == '#') continue;
        
        // Parse CWIPI configuration
        // Expected format: /CWIPI/KEY/VALUE
        std::vector<std::string> parts;
        std::istringstream iss(line);
        std::string part;
        
        while (std::getline(iss, part, '/')) {
            if (!part.empty()) {
                parts.push_back(part);
            }
        }
        
        if (parts.size() >= 3 && parts[0] == "CWIPI") {
            std::string key = parts[1];
            std::string value = parts[2];
            
            if (key == "APPLICATION_NAME") {
                applicationName_ = value;
            } else if (key == "CONFIG_FILE") {
                configFile_ = value;
            } else if (key == "MESH_NAME") {
                meshName_ = value;
            } else if (key == "READ") {
                DataType dataType = stringToDataType(value);
                if (dataType == DataType::NOTHING) {
                    std::cout << "Warning: Unknown data type '" << value << "' in read configuration." << std::endl;
                    continue;
                }
                readData_[static_cast<size_t>(dataType)].isActive = true;
                if (DataType::POSITIONS == dataType) {
                    readData_[static_cast<size_t>(dataType)].mode = Mode::REPLACE;
                } else if (DataType::FORCES == dataType) {
                    readData_[static_cast<size_t>(dataType)].mode = Mode::ADD;
                }
            } else if (key == "WRITE") {
                DataType dataType = stringToDataType(value);
                if (dataType == DataType::NOTHING) {
                    std::cout << "Warning: Unknown data type '" << value << "' in write configuration." << std::endl;
                    continue;
                }
                writeData_[static_cast<size_t>(dataType)].isActive = true;
            } else if (key == "INTERFACE") {
                setGroupNodeId(std::stoi(value));
            }
        }
    }
    
    active_ = true;
    return true;
}

void CwipiCouplingAdapter::setNodes(const std::vector<int>& nodeIds) {
    couplingNodeIds_ = nodeIds;
    
    // Allocate buffers for 3D data
    int bufferSize = couplingNodeIds_.size() * 3;
    
    for (size_t i = 0; i < readData_.size(); ++i) {
        if (readData_[i].isActive) {
            readData_[i].buffer.resize(bufferSize);
        }
    }
    
    for (size_t i = 0; i < writeData_.size(); ++i) {
        if (writeData_[i].isActive) {
            writeData_[i].buffer.resize(bufferSize);
        }
    }
}

bool CwipiCouplingAdapter::initialize(const double* coordinates, int totalNodes, int mpiRank, int mpiSize) {
    if (!active_) return false;
    
    try {
        // Initialize CWIPI
        // TODO: Implement CWIPI initialization
        // This is where you would call cwipi_init() and related functions
        
        // Example skeleton (to be implemented):
        // cwipi_init(MPI_COMM_WORLD, applicationName_.c_str(), &couplingId_);
        
        // Set up mesh
        // TODO: Define mesh vertices and connectivity
        
        // Set up coupling fields
        // TODO: Define coupling fields for data exchange
        
        initialized_ = true;
        maxTimeStepSize_ = 1e30; // Default large value
        
    } catch (const std::exception& e) {
        std::cerr << "Error initializing CWIPI: " << e.what() << std::endl;
        return false;
    }
    
    return true;
}

void CwipiCouplingAdapter::writeData(const double* values, int totalNodes, double dt, int dataType) {
    if (!active_ || !initialized_) return;
    
    if (!writeData_[static_cast<size_t>(dataType)].isActive) {
        return;
    }
    
    // TODO: Implement CWIPI data writing
    // extractNodeData(values, totalNodes, dataType);
    // Call appropriate CWIPI functions to send data
}

void CwipiCouplingAdapter::readData(double* values, int totalNodes, double dt, int dataType) {
    if (!active_ || !initialized_) return;
    
    if (!readData_[static_cast<size_t>(dataType)].isActive) {
        return;
    }
    
    // TODO: Implement CWIPI data reading
    // Call appropriate CWIPI functions to receive data
    // injectNodeData(values, totalNodes, dataType);
}

void CwipiCouplingAdapter::advance(double& dt) {
    if (!active_ || !initialized_) return;
    
    // TODO: Implement CWIPI time advancement
    // This might involve synchronization calls
}

bool CwipiCouplingAdapter::isCouplingOngoing() const {
    if (!active_ || !initialized_) return false;
    
    // TODO: Implement coupling status check
    // Return true if coupling should continue
    return true;
}

bool CwipiCouplingAdapter::requiresWritingCheckpoint() const {
    return false; // CWIPI typically doesn't require checkpoints
}

bool CwipiCouplingAdapter::requiresReadingCheckpoint() const {
    return false; // CWIPI typically doesn't require checkpoints
}

void CwipiCouplingAdapter::finalize() {
    if (!active_) return;
    
    if (initialized_) {
        // TODO: Implement CWIPI finalization
        // cwipi_finalize();
        initialized_ = false;
    }
    
    // Clear all data structures
    couplingNodeIds_.clear();
    for (auto& data : readData_) {
        data.buffer.clear();
        data.isActive = false;
    }
    for (auto& data : writeData_) {
        data.buffer.clear();
        data.isActive = false;
    }
    
    active_ = false;
}

bool CwipiCouplingAdapter::isActive() const {
    return active_;
}

double CwipiCouplingAdapter::getMaxTimeStepSize() const {
    return maxTimeStepSize_;
}

int CwipiCouplingAdapter::getNumberOfCouplingNodes() const {
    return couplingNodeIds_.size();
}

void CwipiCouplingAdapter::extractNodeData(const double* globalValues, int totalNodes, int dataType) {
    if (!writeData_[dataType].isActive) {
        return;
    }
    
    for (size_t i = 0; i < couplingNodeIds_.size(); ++i) {
        int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
        writeData_[dataType].buffer[i * 3] = globalValues[nodeId * 3];
        writeData_[dataType].buffer[i * 3 + 1] = globalValues[nodeId * 3 + 1];
        writeData_[dataType].buffer[i * 3 + 2] = globalValues[nodeId * 3 + 2];
    }
}

void CwipiCouplingAdapter::injectNodeData(double* globalValues, int totalNodes, int dataType) {
    if (!readData_[dataType].isActive) {
        return;
    }
    
    if (readData_[dataType].mode == Mode::ADD) {
        for (size_t i = 0; i < couplingNodeIds_.size(); ++i) {
            int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
            globalValues[nodeId * 3] += readData_[dataType].buffer[i * 3];
            globalValues[nodeId * 3 + 1] += readData_[dataType].buffer[i * 3 + 1];
            globalValues[nodeId * 3 + 2] += readData_[dataType].buffer[i * 3 + 2];
        }
    } else if (readData_[dataType].mode == Mode::REPLACE) {
        for (size_t i = 0; i < couplingNodeIds_.size(); ++i) {
            int nodeId = couplingNodeIds_[i] - 1; // Convert to 0-based indexing
            globalValues[nodeId * 3] = readData_[dataType].buffer[i * 3];
            globalValues[nodeId * 3 + 1] = readData_[dataType].buffer[i * 3 + 1];
            globalValues[nodeId * 3 + 2] = readData_[dataType].buffer[i * 3 + 2];
        }
    }
}

#endif 

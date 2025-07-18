#include "coupling_factory.h"
#include "dummy_coupling_adapter.h"

#ifdef WITH_PRECICE
#include "precice_coupling_adapter.h"
#endif

#ifdef WITH_CWIPI
#include "cwipi_coupling_adapter.h"
#endif

CouplingAdapter* createCouplingAdapter() {
#ifdef WITH_PRECICE
    return new PreciceCouplingAdapter();
#elif defined(WITH_CWIPI)
    return new CwipiCouplingAdapter();
#else
    return new DummyCouplingAdapter();
#endif
}

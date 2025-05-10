#ifdef MPI
#include "mpif.h"
#endif

        COMMON /MPI_COMM/ SPMD_COMM_WORLD, APPLICATION_ID
        INTEGER ::  SPMD_COMM_WORLD
        INTEGER :: APPLICATION_ID


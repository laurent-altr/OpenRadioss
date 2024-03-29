#
# CMake project file for Radioss Starter
# --------------------------------------
 
cmake_minimum_required (VERSION 2.8)
project (OpenRadioss)

set (actual_directory ${CMAKE_CURRENT_SOURCE_DIR})


message (STATUS "source directory : ${actual_directory} ")
message (STATUS "VS_BUILD : ${VS_BUILD} ")

if (NOT DEFINED build)
   set (build "both")
endif ()

message(STATUS "build : ${build} ")

if(NOT DEFINED arch)
    if(WIN32)
        set(arch "win64")
    elseif(UNIX AND NOT APPLE)
        set(arch "linux64_gf")
    endif()
endif()



#cmake -G Ninja -Darch=%arch% -Dbuild=%build% -Dstarter=%starter% -Dengine=%engine% -DVS_BUILD=1 -Dprecision=%prec% %MPI% -Ddebug=%debug%  -Dstatic_link=%static% -DCMAKE_BUILD_TYPE=%build_type% -DCMAKE_Fortran_COMPILER=%Fortran_comp% -DCMAKE_C_COMPILER=%C_comp% -DCMAKE_CPP_COMPILER=%CPP_comp% -DCMAKE_CXX_COMPILER=%CXX_comp% ..
# print CMAKE_Fortran_COMPILE
if(UNIX AND NOT APPLE)
    if(NOT DEFINED CMAKE_Fortran_COMPILER)
        find_program(GFORTRAN_COMPILER gfortran)
        if(GFORTRAN_COMPILER)
            set(CMAKE_Fortran_COMPILER "${GFORTRAN_COMPILER}")
        else()
            message(FATAL_ERROR "gfortran not found")
        endif()
    endif()
    if(NOT DEFINED CMAKE_CPP_COMPILER)
        find_program(GCC_COMPILER g++)
        if(GCC_COMPILER)
            set(CMAKE_CPP_COMPILER "${GCC_COMPILER}")
        else()
            message(FATAL_ERROR "g++ not found")
        endif()
    endif()
    if(NOT DEFINED CMAKE_C_COMPILER)
        find_program(GCC_COMPILER gcc)
        if(GCC_COMPILER)
            set(CMAKE_C_COMPILER "${GCC_COMPILER}")
        else()
            message(FATAL_ERROR "gcc not found")
        endif()
    endif()
endif()




message( STATUS "CMAKE_Fortran_COMPILER: " ${CMAKE_Fortran_COMPILER} )
message( STATUS "CMAKE_C_COMPILER: " ${CMAKE_C_COMPILER} )
message( STATUS "CMAKE_CPP_COMPILER: " ${CMAKE_CPP_COMPILER} )

if ( ${build} STREQUAL "starter" OR ${build} STREQUAL "both" )
    message (STATUS "=============================================== ")
    message (STATUS "Building Starter ")
    message (STATUS "=============================================== ")
    if(NOT DEFINED starter)
        set(EXEC_NAME "openradioss-starter")
    else()
        set (EXEC_NAME ${starter})
    endif()
    message (STATUS "starter name : ${EXEC_NAME} ")
    add_subdirectory(${actual_directory}/starter)
    message (STATUS "=============================================== ")
endif()

if ( ${build} STREQUAL "engine" OR ${build} STREQUAL "both" )
    message (STATUS "=============================================== ")
    message (STATUS "Building Engine ")
    message (STATUS "=============================================== ")
    if(NOT DEFINED engine)
        set(EXEC_NAME "openradioss-engine")
    else()
        set (EXEC_NAME ${engine})
    endif()
    message (STATUS "engine name : ${EXEC_NAME} ")
    add_subdirectory(${actual_directory}/engine)
    message (STATUS "=============================================== ")
endif()


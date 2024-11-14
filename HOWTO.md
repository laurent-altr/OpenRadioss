# Building OpenRadioss

* [Build environment on Linux](#build-environment-on-linux)
  * [System prerequisites](#system-prerequisites)
  * [Compiler and development tools](#compiler-and-development-tools)
  * [OpenMPI installation](#openmpi-installation)
* [Build environment on Linux ARM64](#build-environment-on-linux-arm64)
  * [System prerequisites](#system-prerequisites-for-linux-arm64)
  * [Compiler and development tools](#compiler-and-development-tools-for-linux-arm64)
  * [OpenMPI installation](#openmpi-installation-for-linux-arm64)
* [Build environment on Windows](#build-environment-on-windows)
  * [Compiler environment](#compiler-environment)
  * [Build environment using cmd DOS shell](#build-environment-using-cmd-dos-shell)
  * [Build environment using Visual Studio](#build-environment-using-visual-studio-2019)
  * [Building environment using cygwin](#building-environment-using-cygwin)
* [How to build OpenRadioss](#how-to-build-openradioss)
  * [Build defaults](#build-defaults)
  * [Get the source](#get-the-source)
  * [Building on Linux](#building-on-linux)
  * [Building on Linux Arm64](#building-on-linux-arm64)
  * [Build OpenRadioss on Windows with cmd Shell](#build-openradioss-on-windows-with-cmd-shell)
  * [Build OpenRadioss with Visual Studio](#build-openradioss-with-visual-studio)
  * [Build OpenRadioss with cygwin](#build-openradioss-with-cygwin)
* [How to build OpenRadioss on Linux with Container using Apptainer](#how-to-build-openradioss-on-linux-with-container-using-apptainer)
* [How to debug with Visual Studio](./doc/Visual_Studio_Debugger.md)

## Build environment on Linux

### System prerequisites

Linux system with glibc version 2.17 or higher:

* CentOS/RHEL 7, CentOS Stream 8, RHEL 8, Rocky Linux 8, Rocky Linux 9
* Ubuntu 20.0.4 or higher
* [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install): OpenRadioss works with WSL/WSL2 Ubuntu 20.04 LTS, WSL2 Ubuntu 22.x

### Compiler and development tools

You will need GCC/Gfortran version 11 or higher,
[Cmake](https://cmake.org/) version 2.8 or higher, and GNU make.

Install as sudo or root

* RHEL 7, CentOS 7

            yum install devtoolset-11
            yum install devtoolset-11-libasan-devel
            yum install devtoolset-11-libubsan-devel
            yum install libasan6
            yum install libubsan
            yum install make
            yum install cmake
            yum install perl
            yum install python
            yum install git-lfs

  To enable the devtoolset-11, you can run `scl enable devtoolset-11 bash`

* RHEL 8, CentOS Stream 8, Rocky Linux 8

           dnf install gcc-toolset-11-toolchain
           dnf install gcc-toolset-11-libasan-devel
           dnf install gcc-toolset-11-libubsan-devel
           dnf install libasan
           dnf install libasan6
           dnf install libubsan
           dnf install make
           dnf install cmake
           dnf install python
           dnf install perl
           dnf install git-lfs

  Installed python is Python3. To create the link from python3 to python,

  type:

           alternatives --config python

           Select python3.

  To enable the devtoolset-11, you can run `source /opt/rh/gcc-toolset-11/enable`

* Ubuntu

           apt-get update
           apt-get upgrade
           apt-get install build-essential
           apt-get install libasan6
           apt-get install libubsan1
           apt-get install gfortran
           apt-get install cmake
           apt-get install perl
           apt-get install python3
           apt-get install python-is-python3
           apt-get install git-lfs

### OpenMPI installation

OpenMPI is needed to build OpenRadioss with OpenMPI support.
It is recommended to build and install OpenMPI from OpenMPI website using gcc compiler.

1. Download OpenMPI tarball from  [www.openmpi.org](https://www.open-mpi.org/software/ompi/v4.1)
   preferred version is [OpenMPI v4.1.2](https://download.open-mpi.org/release/open-mpi/v4.1/openmpi-4.1.2.tar.gz)

            wget https://download.open-mpi.org/release/open-mpi/v4.1/openmpi-4.1.2.tar.gz

2. Decompress and enter the folder:

            tar -xvzf openmpi-4.1.2.tar.gz
            cd openmpi-4.1.2

3. Build and install OpenMPI

**you need root or sudo rights on your computer**.

        ./configure --prefix=/opt/openmpi
        make
        make install

## Build environment on Linux ARM64

### System prerequisites for Linux ARM64

This version works on ARMv8-A and higher architectures using Linux OS:

For the supported Hardware list, visit :

<https://developer.arm.com/Tools%20and%20Software/Arm%20Compiler%20for%20Linux#Supported-Devices>

The Linux ARM64 version is built using armflang and armclang compiler.
They are available for the following Linux versions:

* RHEL 7, RHEL 8, RHEL 9 for ARM64
* Suse SLES 15 for ARM64
* Ubuntu 20.04, Ubuntu 22.04 for ARM64

### Compiler and development tools for Linux ARM64

#### Development environment

##### Install Development tools on your machine

As root or sudo user:

On RHEL 8, Rocky Linux 8, Suse...

     dnf install cmake
     dnf install python
     dnf install perl
     dnf install git
     dnf install git-lfs
     dnf install environment-modules

On Ubuntu 20.x, 22.x, 23.x...

     apt-get update
     apt-get upgrade
     apt-get install build-essential
     apt-get install gfortran
     apt-get install cmake
     apt-get install perl
     apt-get install python3
     apt-get install python-is-python3
     apt-get install git
     apt-get install git-lfs
     apt-get install environment-modules

##### Install ArmFlang compiler

ARM compilers and ARM Performance libraries are used to build OpenRadioss.
ArmFlang 24.04 is recommended to build OpenRadioss. It uses the module system to setup the compiler.

* ArmFlang compilers can be downloaded at:
<https://developer.arm.com/downloads/-/arm-compiler-for-linux>
* Follow ArmFlang installation instructions: <https://developer.arm.com/documentation/102621/0100>

* To see the list of available modules and load the compiler settings type:

      module list
      module load acfl/24.04

**Note:**

* The module system was found to be improperly configured on some systems. If the module command is not found, add in your .bashrc shell file:

      source /etc/profile 

* If the compiler could not be found with *module list*, add */opt/arm/modulefiles* in the *MODULEPATH* variable. Add this setting in your .bashrc file to have it permanent.

      export MODULEPATH=$MODULEPATH:/opt/arm/modulefiles

### OpenMPI installation for Linux ARM64

OpenMPI is needed to build OpenRadioss with OpenMPI support.
It is recommended to build and install OpenMPI from OpenMPI website using gcc compiler.

1. Download OpenMPI tarball from  [www.openmpi.org](https://www.open-mpi.org/software/ompi/v4.1)
   preferred version is [OpenMPI v4.1.2](https://download.open-mpi.org/release/open-mpi/v4.1/openmpi-4.1.2.tar.gz)

            wget https://download.open-mpi.org/release/open-mpi/v4.1/openmpi-4.1.2.tar.gz

2. Decompress and enter the folder:

            tar -xvzf openmpi-4.1.2.tar.gz
            cd openmpi-4.1.2

3. Build and install OpenMPI

Load the gcc/gfortran compiler module from ArmFlang installation using module environment tool:

* Find the available GNU compiler with:

        module avail

* Load the gnu compiler with the module environment tool: here an example with gnu 11.2.0

        module load gnu/11.2.0

**you need root or sudo rights on your computer**.

        ./configure --prefix=/opt/openmpi
        make
        make install

## Build environment on Windows

OpenRadioss was tested with OneAPI 2023.2 + Visual Studio 2019.

This chapter explains how to setup Windows on different build configurations

* Compiler environment
* OpenRadioss build environment using cmd.exe
* OpenRadioss build using cygwin
* OpenRadioss build environment using Visual Studio.

### Compiler environment

1. Intel OneAPI requires Visual Studio Community, Enterprise or Professional Edition installed.
   For all prerequisites, visit : <https://www.intel.com/content/www/us/en/developer/articles/system-requirements/intel-oneapi-base-toolkit-system-requirements.html>
   **It is recommended to upgrade Visual Studio to the latest available one.**

2. Download one API Base Toolkit and one API HPC Toolkit

    * Visit one API Base Toolkit Download page: [oneAPI Base Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/base-toolkit-download.html)
    * Visit one API HPC Toolkit Download page: [oneAPI HPC Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit-download.html)

3. Install Toolkits

   Minimum required packages are

   * In the Base Toolkit: Intel DPC++/C++, Intel Math Kernel Library, Intel distribution for Python.
   * In the HPC Toolkit: Intel Intel® oneAPI DPC++/C++ Compiler, Intel® Fortran Compiler, Intel® MPI Library

   **Notes:**

   * Intel OneAPI plugin for Visual Studio is recommended to use Intel OneAPI in Visual Studio 2019
   * Choose the default directory to install Intel oneAPI

4. Install Git

   * Install Git for Windows from: [https://git-scm.com/downloads](https://git-scm.com/downloads).
The Git Bash tool is not needed, but can be installed.

5. Post installation tasks with git

* Install git-lfs

            git lfs install

* Add in Git global environment the autocrlf flag

            git config --global core.autocrlf true

* Create the ssh key & set it in GitHub

            ssh-keygen -t rsa
  
  **Note: Accept all defaults, Standard directory, no passphrase**

* Set your git parameters as in [CONTRIBUTING.md](./CONTRIBUTING.md)

### Build environment using cmd DOS shell

Building using cmd.exe is using cmake.exe and ninja.exe
Both are shipped with Visual Studio 2019.

1. Setup the compiler
   Load compiler settings in cmd.exe using the following command :

         call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2019

   cmd.exe can be launched using a batch script to ease usage

         @echo off
         call "C:\Program Files (x86)\Intel\oneAPI\setvars.bat" intel64 vs2019
         cmd.exe

### Build environment using Visual Studio 2019

**Notes:**

* Following the procedure was tested on Visual Studio 2019

* Visual Studio Graphical environment must be installed.
* Visual Studio using cmake and ninja for compilation.
* It is recommended to update Visual Studio to the most recent release.
* Cmake + Builders must be installed in Visual Studio: Visual Studio is using Cmake and ninja builder (available with cmake package)
* Intel OneAPI plugin for VS2019 must be installed and running. Otherwise Compiler is not found.

### Building environment using cygwin

1. Install Cygwin

   * Download setup-x86-64 setup from: <https://www.cygwin.com/install.html>
    Direct access is: [setup-x86_64.exe](https://www.cygwin.com/setup-x86_64.exe)

   * execute setup-x86_64.exe
   *
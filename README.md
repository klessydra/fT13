<img src="/pics/Klessydra_Logo.png" width="400">

# KLESSYDRA-fT13 FT PROCESSOR

Intro: The Klessydra processing core family is a set of processors featuring full compliance with RISC-V, and pin-to-pin compatible with the PULPino Riscy cores. Klessydra-fT13 is a bare-metal 32-bit temprally redundant fault tolerant processor fully supporting the RV32IM from the RISC-V ISA. 

Architecture:

The fT13 furhter supports the vector accelerator present in the T13 and S1.

The Coprocessor is a highly parametrizable accelerator, with up to 256-bit SIMD+MIMD execution capabilities. It comprises the Multipurpose Functional Unit, and the Scratchpad Memory Interface. The custom instruction set supported are listed in the Technincal manuals in the Docs folder. In addition to SIMD execution, the coprocessor supports subword-SIMD to further accelerate 8-bit and 16-bit integer data types.

The coprocessor features a parametrizable set of Scratchpad memories 'SPMs' (parametrizable being their size and number, and their bank numbers will automatically expand to match the SIMD configuration). 

The coprocessor can be configured to run in three different modes:

1) Shared Coprocessor: Where the coprocessor is shared by all the harts (SIMD Coprocessor).
2) Fully Symmetrical Coprocessor: Where each hart has its dedicated MFU and SPMI. (SIMD+MIMD Coprocessor ver.1).
3) Heterogeneous coprocessor: Where the harts share the functional units in the MFU, but each hart maintains it own dedicated SPMI (SIMD+MIMD coprocessor ver.2).

Parameters:
- N = Number of SPMs in the SPMI.
- M = Number of SPMIs, as well as control logic for every hart.
- D = Number of Functional Units per MFU, and banks per SPM (i.e. determines the SIMD width).
- F = Number of Functional Units per hart (i.e. determines the MIMD width).

<p align="center">
<img src="/pics/Vector Coprocessor.png" width="500">
</p> 

# Using Klessydra-fT13

This guide explains how one can download and install Pulpino, and it's 
modified version of the riscv-gnu toolchain. It also demonstrates
how to patch the offcial riscv-toolchain in order to add the klessydra custom
vector extensions. And then it shows how you can easily merge the Klessydra-Core 
with the Pulpino project.

###########################################################################################
- Prerequisites as indicated by the pulpino group
	- ModelSim in reasonably recent version (we tested it with versions 10.2c)
	- CMake >= 2.8.0, versions greater than 3.1.0 recommended due to support for ninja
	- riscv-toolchain, there are two choices for getting the toolchain: 

  		1) RECOMENDED OPTION: Use the custom version of the RISC-V toolchain from ETH. 
  		The ETH versions supports all the ISA extensions that were incorporated 
	  	into the RI5CY core as well as the reduced base instruction set for zero-riscy.
	        " https://github.com/pulp-platform/ri5cy_gnu_toolchain.git "

		2) Or download the official RISC-V toolchain supported by Berkeley.
 	       	" https://github.com/riscv/riscv-gnu-toolchain "


	  	Please make sure you are using the newlib version of the toolchain.
	- python2 >= 2.6
	
###########################################################################################

PROCEDURE:
1.	Install the following packeges:
		
		sudo apt-get install git cmake python-yaml tcsh autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev

2.	Download and build the "ri5cy_gnu_toolchain"

		a) git clone https://github.com/pulp-platform/ri5cy_gnu_toolchain.git
		
		b) cd ri5cy_gnu_toolchain
		
		c) make ZERORISCY=1

		d) in case you need to build for RISCY cores, then just do "make" instead, and then add the symbolic links as shown from step 4.
		
	When the build is done, add the path **_<path_to_toolchain>/ri5cy_gnu_toolchain/install/bin_** to the environmental variables

3.	To run the klessydra tests, you have to download and patch the official riscv-toolchain, and then build it. Instructions for doing so are included in the README.md file
	inside the folder called "toolchain_files".

4.	Download PULPino-Klessydra:

		a) git clone https://github.com/klessydra/pulpino-klessydra
		
		b) cd pulpino-klessydra
		
		c) ./update-ips.py	

5.	OPTIONAL: After the update scipt is done, then you will be able to test Klessydra-m.
		-Navigate to "sw" folder inside pulpino and execute the following commands

		a) mkdir build
		
		b) cp cmake_configure.klessydra-m.gcc.sh build/
		
		c) cd build
		
		d) ./cmake_configure.klessydra-m.gcc.sh
		   (Execute the above script twice if you ever change the variable that changes the riscv-compiler, since changing the compiler flushes the values of the variables in the cmake cache and gives an error. Executing the script for a second time after changing the riscv-compiler will let the variables be redfined again)
		   
		e) make vcompile

		For running Klessydra tests; the variable "USE_KLESSYDRA_TEST" in the above shell file is set to '1' by default. You only need to build and run your test
		f) (e.g.  make vect_sum_single_funct_call_all_test_perf.vsimc)
		General tests for all "Txx" versions of Klessydra are also available
		g) (e.g.  make barrier_test.vsimc)
		
		h) You can run one of the PULPino native tests,  (e.g. make testALU.vsimc)
			
	IT"S DONE!!!!!!

Supplimentary Information:

6.	In order to run tests in Modelsim, go to the build folder and do the following:
		make nameofthetest.vsim (or .vsimc to run a test without modelsim GUI)

# fT13 Configuration Parameter List

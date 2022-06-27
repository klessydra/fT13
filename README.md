<img src="/pics/Klessydra_Logo.png" width="400">

# KLESSYDRA-fT13 FT PROCESSOR

Intro: The Klessydra processing core family is a set of processors featuring full compliance with RISC-V, and pin-to-pin compatible with the PULPino Riscy cores. Klessydra-fT13 is a bare-metal 32-bit temporally and spatial redundant fault tolerant processor fully supporting the RV32IM from the RISC-V ISA, born from Klessydra-T13 microarchitecture. 

Architecture:

The redundant Architecture depicted in the following figure, exploits the architecture of the IMT processor to obtain redundancy between the three hardware threads, combining both spatial and temporal redundancy obtained thanks to the Buffered TMR technique and its own applications. Klessydra-fT13 has three threads, each having their own set of signals thus incorporating spatial redundancy, with threads that follow the same instruction in an interleaved fashion, providing temporal redundancy. At the end of the execute stage of each thread, the writeback results from the corresponding thread are buffered, and then voted and written back to the Register File (RF) only after the last thread execution, in order to avoid a Write Back (WB) of an unvoted result that might have a fault. By following this idea, it is possible to use a single voted TMR RF in the WB stage triggered during the WB of the last thread. Additionally, there is some pipeline interlocking and RF bypass logic to directly pass the operands among threads avoiding long stalls as shown in Figure 2.

<p align="center">
<img src="/pics/Klessydra-fT13_microarchitecture.png" width="800">
</p> 

Figure 1:
Registerfile and Writeback stage modifications:

<p align="center">
<img src="/pics/Klessydra-fT13_regfile_wb_modifications.png" width="700">
</p> 
Figure 2:
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
Figure 3

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

The following illustrates briefly the parameters of the fT13, and their usage settings.

- For more details about the Klessydra processing cores, please refer to the technincal manual in Docs
- For more details about the Klessydra runtime libraries, please refer to the software runtime manual in Docs

Extensions of fT13x core:

The fT13 can be configed in many ways in the from the "cmake_configure.klessydra-ft1-3th.gcc.sh" found in the sw forlder:

You will find the following generics that will be passed to the RTL. **_Read the comments next to the variables before modifying_**:
1)  "THREAD_POOL_SIZE=3" sets the number of hardware threads. This should be set to 3.
2)	"LUTRAM_RF=0" this variable creates a LUTRAM based registerfile instead of a flip-flop based one, it is good for FPGA synthesis as LUTRAMs based regfiles are more efficient than FF based ones. **_For that fT13 version, it should be set to 0, so no LUTRAM will be used._**
3)	"RV32E=0" this enables the embedded extension of the RISCV ISA, and makes the regfile to be half its original size (16 regs only).
4)	"RV32M=1" this enable the M-extension of the RISCV ISA. The mul instruction is a single cycle instructions, and the mulh/hu/hsu instructions need 3 cycles. divisions are slow, and can be up to 32 cycles, however fast single cycle divisions are availabe for special cases (div by 0, numerator < denominator, numerator is 0, and numerator equals the denominator).
5)	"superscalar_exec_en=1"  Enables superscalar execution when set to 1, else the stall of the pipeline will depend on tha latency of the instruction executing. This more than doubles the speed of the core in many applications, however if in the exceptional case the RTL is not simulating correctly, disable this and see whether the RTL will work again.
6)	"accl_en=0"  Enables the generation of the hardware accelerator of the T13. **_Its use in fT13 has not yet been debugged, so it should be set to 0_**
7)	"replicate_accl_en=0" Once set, it replicates the accelerator for every thread, this increases the parallelism of the fT13 by allocating a dedicated accelerator for each hart in the fT13. It should b set to 1 in order to have TMR replicas of the single VCUs. **_Its use in fT13 has not yet been debugged, so it should be set to 0_**
8)	"multithreaded_accl_en=0" Set this to 1 to let the replicated accelerator have shared functional units, but maintain dedicated SPM memories for each hardware thread (note: replicate_accl_en must be set to '1').
9)	"SPM_NUM = 3 " The number of scratchpads available "Minimum allowed is 2". When the acclerator is replicated, each hardware thread will have scratchpads equal to SPM_NUM, so in a THREAD_POOL_SIZE of 3 we will have 3*SPM_NUM scratchpads in totals. **_Its use in fT13 has not yet been debugged._**
10)	Addr_Width = 13" This address is for scratchpads. Setting this will make the size of the spm to be: "2^Addr_Width -1"
11)	"SIMD = 1" Changing the SIMD, would increase the DLP by increasing the number of the functional units in the accelerator, and the number of banks in the spms acordingly (can be power of 2 only e.g. 1,2,4,8) no more than SIMD 8 is allowed. Setting this to '8' with replicate_accl_en and superscalar_exec_en being set to '1' as well will make the accelerator run at peak performance. **_Its use in fT13 has not yet been debugged._**
12)	"MCYCLE_EN" Can be set to 1 or 0 only. Setting to zero will disable MCYCLE and MCYCLEH
13)	"MINSTRET_EN" Can be set to 1 or 0 only. Setting to zero will disable MINSTRET and MINSTRETH
14)	"MHPMCOUNTER_EN" Can be set to 1 or 0 only. Setting to zero will disable all performance counters except "MCYCLE/H" and "MINSTRET/H"
15)	"count_all" Perfomance counters count for all the harts instead of there own hart
16)	"debug_en" Generates the debug unit, the debug unit is elimentary and might need some further evaluation and testing
17)	"tracer_en" Generates an instruction tracer, used for debugging


Hope you like it :D


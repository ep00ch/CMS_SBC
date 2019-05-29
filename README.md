# CMS ROMs

This project contains data downloaded from the Read Only Memory (ROM) of a 
Creative Micro Systems (CMS) 9619 Advanced Single Board Computer (SBC) and a 
CMS 6909 MPU module.

The ROMs were downloaded using an Apple IIe, so the  data is in the 
Apple II monitor format (.mon). However, they are easily changed to other more 
standard formats using the scripts contained in the makefile.

## Background

The CMS 9619 and 9609 computers are EXORBus compatible, have MC6809 processors and a number of PIAs and ACIAs, making them fairly capable SBCs. The ROMS appear to be versions of the MICROWARE SYSTEMS CORPORATION DEBUG machine monitor. [The source code of an earlier version of it](http://www.retro.co.za/6809/documents/debug09.pdf) is available online. The sections with similar functions have comments straight from that document. The [debugger in Nitros-9](https://github.com/boisy/nitros9/blob/master/level1/cmds/debug.asm) contains similar code. There also appears to be user code in U17_9619 that I have not looked at yet.

## Prerequisites

The makefile uses:
* sed, xxd - to convert from the Apple II monitor format to hex, binary, or hexdump format.

* [f9dasm](https://github.com/Arakula/f9dasm) - to disassemble the binary, and incorprate the comments.

* lwasm from [LWTools](https://github.com/milliluk/LWTools) - to assemble the source files back to binaries.

* diff - to compare the original binary hexdump to the compiled binary hexdump.


## Makefile Commands

The included makefile includes a number of commands to do the various file 
conversions. Basically, the make target should be specified as the base filename
 you want to convert, with the new extension for the file type you want. 
Supported extensions are: hex, hexdump, info, bin, asm

For example, the command:

`make U7_9619.asm`

will convert the U7_9619.mon file to a more standard hex format, then convert it to a binary, then disassemble it using f9dasm.

The command:

`make U7_9619.diff`

do the same as above, then assemble the the .asm file using lwtools, then generate a hexdump of the re-assembled binary. Finally, it compares the original binary hexdump to the re-assembled binary hexdump.

The command:

`make all`

will convert all of the mon files, disassemble them, and combine them into final EPROM_9619.asm and EPROM_9609.asm files for each model number.

## TO DO

The disassembly comments are not complete yet. It is a work in progress, but still helpful to see what the monitors do.

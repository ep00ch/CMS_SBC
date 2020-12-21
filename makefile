SRC=$(wildcard *.mon)
O_SRC=$(sort ${SRC})
F9DASM=f9dasm -noconv -noflex -cchar \*

ASSEMBLYS=	6530_U10.asm \
	9600_U23.asm \
	9609_U22_U23.asm \
	9611_U19.asm \
	9619_U17.asm \
	9619_U7.asm \
	9639_U26.asm \
	9639_boot.asm \
	9642_U3.asm \
	XK-300_U12.asm

$(warning ${O_SRC} )

#Format from Apple II system monitor dump, ROM saved to $8000
%.mon : %.mondmp
	sed -e '/*/d' -e '/^$$/d' -e 's/-/:/g' $< > $@

%.bin : %.mon
	xxd -r -seek -$$((16#8000)) -g1 -c8 $< > $@

%.aif : %.mon
	c2t -2 $< $@

#Format from DEBUG09/19 dump
#:V C000 DFFF
%.bin : %.dbgdmp
	xxd -r -seek -$$((16#`head -c4 $<`)) $< >$@

#Transition from mondmp to dbgdmp
%.mon.dbgdmp : %.bin
	xxd -g1 -c16 -u -o $$((16#C000)) $< | \
	sed -e 's/^0000\(....\):\(\( [[:xdigit:]]\{2\}\)*\)  .*/\1\2/' | \
	tr '[:lower:]' '[:upper:]' > $@ 

%.raw.dbgdmp : %.raw
	xxd -g1 -c16 -u -o $$((16#C000)) $< | \
	sed -e 's/^0000\(....\):\(\( [[:xdigit:]]\{2\}\)*\)  .*/\1\2/' | \
	tr '[:lower:]' '[:upper:]' > $@ 

%.hex : %.bin
	xxd $< > $@

#%.info : %.bin
	#echo "FILE \"$<\" E000" > $@

9609_U22_U23.asm : 9609_U22_U23.info 9609_U22.bin 9609_U23.bin 9619_U7.info
	$(F9DASM) -info $< -out $@

%.asm : %.info %.bin
	$(F9DASM) -info $< -out $@

9600_U23.asm : 9600_U23.s
	# remove the offending space between opcode and register
	sed -e 's/ X$$/ ,X/' -e 's/ X / ,X /' -e's/O,NOC/noc/' $< | \
	cut -b -10,12- > $@

9600_U23.hex : 9600_U23.s19
	cut -b 5-70 $< | sed -e 's/^..../&: /' -e '$$d' > $@


%.raw %.raw.bin : %.asm
	lwasm --pragma=noforwardrefmax -9 -f raw -o $@ $<

# Compare before and after disassembly and reassembly.
%.diff : %.dbgdmp %.raw.dbgdmp
	diff -s $^ | tee $@

# Format to dump to RAM using DEBUG19.
%.dbge : %.asm
	lwasm -f hex -o $*.debug.tmp $<
	echo "E " | cat - $*.debug.tmp | \
	sed -e '4,$$s/....://' | \
	tr -d '\r\n,'| tr ':' '\n' > $@
	echo $$'\n' >> $@
	@cat $@

# Format to dump to EEPROM using DEBUG19 and ignore errors.
# The value is checked by the monitor before the 
# EEPROM write interval is reached, causing an error,
# but it will write it, and the writing continues, slowly!
%.dbgee : %.raw
	xxd -c1 -u -o $$((16#A000)) $< | \
	sed -e 's/0000/E /' -e 's/  .*//' -e 's/: /:/'| \
	tr '[:lower:]' '[:upper:]' | tr ':' '\n'> $@

help : 
	@echo "Uses xxd, f9dasm, lwasm, and diff to disassemble, reassemble, and\n\
	  compare 6809 code as exported from the Apple II monitor or DEBUG19. Try: \n\
	     make all \n\
	     make 9619_U7.hex\n     make 9619_U7.asm\n\
	     make 9619_U7.raw\n     make 9619_U7.diff\n\
	     make clean is aggressive, don't keep modifications in this folder."

all : 9609_U22_U23.asm 9619_U7.asm 9639_U26.asm 9642_U3.asm XK-300_U12.asm

# This sets up for a git commit with a few viewable asm files for convenience.
clean-git : | clean 9619_U7.asm 9642_U3.asm XK-300_U12.asm
	git status

rm-%:
	sed -e "s/ /.$*" $(ROMS) 

clean:
	rm -f *.mon
	rm -f *.aif
	rm -f *.hex
	rm -f *.bin
	rm -f *.tmp
	rm -f *.raw
	rm -f *.diff
	rm -f *.dbgee
	rm -f *.dbge
	rm -f $(ASSEMBLYS)

.PHONY: clean all 9609 9619 help

.SECONDARY: %.hex

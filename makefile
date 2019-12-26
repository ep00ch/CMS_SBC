SRC=$(wildcard *.mon)
O_SRC=$(sort ${SRC})
F9DASM=f9dasm -noconv -noflex

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

%9609.asm : %9609.info U22_9609.bin U23_9609.bin
	$(F9DASM) -info $< -out $@

%.asm : %.info %.bin
	$(F9DASM) -info $< -out $@

%.raw %.raw.bin : %.asm
	lwasm -f raw -o $@ $<

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
	     make U7_9619.hex\n     make U7_9619.asm\n\
	     make U7_9619.raw\n     make U7_9619.diff\n\
	     make clean is aggressive, don't keep modifications in this folder."

all : U22_U23_9609.asm U7_9619.asm U26_9639.asm U3_9642.asm

# This sets up for a git commit with the viewable U7_9619.asm file.
clean-git : | clean U7_9619.asm U26_9639.asm
	git status

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
	#rm -f *9.asm
	rm -i *.asm

.PHONY: clean all 9609 9619 help

.SECONDARY: %.hex

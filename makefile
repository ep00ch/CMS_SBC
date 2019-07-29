SRC=$(wildcard *.mon)
O_SRC=$(sort ${SRC})
F9DASM=f9dasm -noconv -noflex

$(warning ${O_SRC} )

%.mon : %.mondmp
	sed -e '/*/d' -e '/^$$/d' -e 's/-/:/g' $< > $@

%.bin : %.mon
	xxd -r -seek -$$((16#8000)) -g1 -c8 $< > $@

%.hex : %.bin
	xxd $< > $@

#%.info : %.bin
#	echo "FILE \"$<\" E000" > $@


%9619.info :
	echo "## This file is automatically generated. Do not edit. ##\n" > $@.tmp
	cat  *9619.info >> $@.tmp
	mv   $@.tmp $@

%9609.info :
	echo "## This file is automatically generated. Do not edit. ##\n" > $@.tmp
	cat  *9609.info >> $@.tmp
	mv   $@.tmp $@

%9609.asm : %9609.info U22_9609.bin U23_9609.bin
	$(F9DASM) -info $< -out $@

%9619.asm : %9619.info U7_9619.bin U17_9619.bin
	$(F9DASM) -info $< -out $@

%.asm : %.info %.bin
	$(F9DASM) -info $< -out $@

%.raw %.raw.bin : %.asm
	lwasm -f raw -o $@ $<

%.raw.mon : %.raw
	xxd -g1 -c8 -u -o $$((16#8000)) $< | sed -e 's/0000//' -e 's/  .*//' | tr '[:lower:]' '[:upper:]' > $@

%.diff : %.mon %.raw.mon
	diff -s $^ | tee $@

fix : U7_9619.mondmp U7_9619.info
	sed -e 's/FD 16 A0 00/FD 16 F8 00/' U7_9619.mondmp > U7_9619_fix.mondmp
	sed -e 's/U7_9619/U7_9619_fix/' U7_9619.info > U7_9619_fix.info

help : 
	echo "Uses xxd, f9dasm, lwasm, and diff to disassemble, reassemble, and compare\n\
	  6809 code as exported from the Apple II monitor. Try: \n\
	     make all \n\
	     make U7_9619.hex\n     make U7_9619.asm\n\
	     make U7_9619.raw\n     make U7_9619.diff"


all : EPROM_9619.asm EPROM_9609.asm

clean:
	rm -f *.mon
	rm -f *.hex
	rm -f *.bin
	rm -f *_fix.mondmp
	rm -f *_fix.info
	rm -f *.tmp
	rm -f EPROM_9609.info EPROM_9619.info
	rm -f *.asm
	rm -f *.raw
	rm -f *.diff

.PHONY: clean all 9609 9619 help

.SECONDARY: %.hex

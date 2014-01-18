COMP=gfortran
FLAGS=-std=legacy -fcoarray=single -fbacktrace -mcmodel=medium
OBJECTS=BEGIN.o BLOCK1.o BLOCK2.o BLOCK3.o BLOCK4.o BLOCK6.o CGDB3.o CODES.o GRIDSZ.o NODES.o QRATE.o SOLTWO.o SOLWKS.o WRTBPD.o UINITL.o

%.o: %.FOR
	$(COMP) $(FLAGS) -c $<

all: cvtmap boast3

boast3: $(OBJECTS)
	$(COMP) $(FLAGS) MAIN.FOR $(OBJECTS) -o boast3 

cvtmap:
	$(COMP) $(FLAGS) CVTMAP.FOR -o cvtmap

.PHONY: clean

clean:
	rm -rf *.o

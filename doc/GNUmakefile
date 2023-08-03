#!/usr/bin/make -f
include ../auto.make

clean-doxy:
	-rm -fr libcwal-* libs2-*

clean: clean-doxy

Doxyfile-a := Doxyfile-amalgamation
DISTCLEAN_FILES += Doxyfile $(Doxyfile-a)

s2.amal.h := $(DIR.top-abs)/libs2.h
cwal.amal.h := $(DIR.top-abs)/libcwal.h
$(s2.amal.h) $(cwal.amal.h):
	$(MAKE) -C $(dir $@) $(notdir $@)

$(Doxyfile-a): $(Doxyfile-a).in | $(s2.amal.h)
	@echo "ERROR: need to reconfigure to create Doxyfile!"; \
	exit 1

Doxyfile: Doxyfile.in | $(cwal.amal.h)
	@echo "ERROR: need to reconfigure to create Doxyfile!"; \
	exit 1

doc: Doxyfile $(Doxyfile-a)
	@echo "Generating API docs..."
	@doxygen
	@echo "Done with the main docs - doing s2 amalgamation..."
	doxygen $(Doxyfile-a)
	@echo "Output is in: "; ls -1td libcwal-* libs2-*

all:
	@echo "Run 'make doc' to generate the API docs with doxygen."

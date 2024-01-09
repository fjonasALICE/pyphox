FC = g95
#~ FC = g77
AR = /usr/bin/ar
RANLIB = /usr/bin/ranlib
#
LIBPDFA = libpdfa.a
VPATH = eks98:eps08:eps09:fgs10:hkm01:hkn04:hkn07:nds:dssz

%.o : %.f
	$(FC) -c -o $@ $(FFLAGS) $<
%.o : %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

PDFALIB = pdfa.o eks98.o nDS.o npdf04.o eps08.o eps09.o npdf07e.o npdf07ev2.o fgs10.o DSSZ.o

plot = npdfplot.o

all:	${LIBPDFA}

${LIBPDFA}:	${PDFALIB}
	${AR} rc ${LIBPDFA} $?
	${RANLIB} ${LIBPDFA}

npdfplot: $(plot)
	$(LINKER) -o $@.exe $^ \
	-L. -lpdfa

clean:
	rm -f ${LIBPDFA} ${PDFALIB} *.mod

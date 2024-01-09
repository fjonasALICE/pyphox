#~ FC = g95
AR = /usr/bin/ar
RANLIB = /usr/bin/ranlib
#
LIBFRAG = libfrag.a
VPATH = bfg:akk05:akk08:bfgw:bkk:dss:hkns: \
	kkp:kretzer:kkks08:aesss:dsv:eks98:eps08:eps09:nds: \
	fgs10:hkm01:hkn04:hkn07
FC         = g95
LINKER     = $(FC)
FFLAGS     = -g

%.o : %.f
	$(FC) -std=legacy -c -o $@ $(FFLAGS) $<
%.o : %.f90
	$(FC) -std=legacy -c -o $@ $(FFLAGS) $<

FRAGLIB = fragmentation.o ffakk.o akk.o akk08.o distribPert.o distribNonPert_setII.o distribNonPert_setI.o \
	locate_bfg.o polin2_bfg.o polint_bfg.o grille_had_charged.o locate_bfgw.o polin2_bfgw.o polint_bfgw.o bkk.o fDSS.o \
	hknsff07.o hknsff07v2.o kkp.o pkhff.o kkks08_D.o fint.o etafrag.o lambpar.o

CALLFF = callff.o

all:	${LIBFRAG}

${LIBFRAG}:	${FRAGLIB}
	${AR} rc ${LIBFRAG} $?
	${RANLIB} ${LIBFRAG}

callff: $(CALLFF) $(FRAGLIB)
	$(LINKER) $(LDFLAGS) -o callff.exe $^
	
clean:
	rm -f ${LIBFRAG} ${FRAGLIB} *.mod

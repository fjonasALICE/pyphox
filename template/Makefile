#   
#  Makefile for hadro-production of a large-pT photons + jet (+X)
#                               or a large-pT hadron + jet (+X)
# if hadron fragmentation function are chosen in parameter.indat, 
# only the fragmentation parts ("onehad") are compiled
# the proper compilation option is called automatically by "start.pl" 
# 
#  GNUmake (use gmake)
# CHOIXHISTO = ntuple : output as ntuple 
# CHOIXHISTO = histo :  output filled directly in histograms
CHOIXHISTO	= histo

FC     =    gfortran
ADD_LIB_G77		= -L/usr/lib/gcc/x86_64-linux-gnu/11 -lg2c
ADD_LIB_GFORTRAN		= -L/usr/lib/gcc/x86_64-linux-gnu/8 -lgfortran
ADD_LIB_GPP		= -L/usr/lib/gcc/x86_64-linux-gnu/11 -lstdc++

ifeq ($(FC),g95)
MAKEBASES= Makefile_g95.linux
FFLAGS	   = -O1 -fno-automatic -std=legacy -no-pie 
LDFLAGS    = 
ADD_LIB	=$(ADD_LIB_GFORTRAN) $(ADD_LIB_GPP)
#~ ADD_LIB	= $(ADD_LIB_GFORTRAN) $(ADD_LIB_GPP)
endif
ifeq ($(FC),gfortran)
MAKEBASES= Makefile_gfortran.linux
FFLAGS	   = -O1 -fno-automatic -std=legacy -no-pie
LDFLAGS    = 
ADD_LIB	= $(ADD_LIB_GPP)
#~ ADD_LIB	= $(ADD_LIB_GPP)
endif
ifeq ($(FC),ifort)
MAKEBASES= Makefile_ifort.linux
FFLAGS	   = -O
LDFLAGS    = 
ADD_LIB	= $(ADD_LIB_GPP)
#~ ADD_LIB	= $(ADD_LIB_GPP)
endif

LINKER	   = $(FC)

FRAGDIR = ../../frag
NPDFDIR = ../../pdfa

GRACELDIR     = ../../basesv5.1
BASESLIB      = bases

#---------- LHAPDF ----------
PATHLHAPDF	= $(shell lhapdf-config --prefix)
LHAPDFLIB	= $(shell lhapdf-config --libs)
#------------------------------

#
#------------------------------------------------------------------------------

ROOTCONFIG   := /software/flo/alice/sw/ubuntu2204_x86-64/ROOT/latest/bin/root-config

ARCH         := $(root-config --arch)
PLATFORM     := $(root-config --platform)

CXX           =
ObjSuf        = o
SrcSuf        = cxx
ExeSuf        =
DllSuf        = so
OutPutOpt     = -o # keep whitespace after "-o"

ROOTCFLAGS   = $(shell root-config --cflags)
ROOTLDFLAGS  = $(shell root-config --ldflags)
ROOTLIBS     = $(shell root-config --libs)
ROOTGLIBS    = $(shell root-config --glibs)
# ROOTGLIBS    = `root-config --glibs`
HASTHREAD    = $(shell root-config --has-thread)

CXX           = g++
CXXFLAGS      = -O -Wall -fPIC
#~ LD            = g++
#~ LD            = g95
LDFLAGS       = -O
SOFLAGS       = -shared

CXXFLAGS     += $(ROOTCFLAGS)
LDFLAGS      += $(ROOTLDFLAGS)
LIBS         += $(ROOTLIBS) $(SYSLIBS)
# LIBS          = -L/software/flo/alice/sw/ubuntu1804_x86-64/ROOT/v6-24-06-local6/lib -lCore -lImt -lRIO -lNet -lHist -lGraf -lGraf3d -lGpad -lROOTVecOps -lTree -lTreePlayer -lRint -lPostscript -lMatrix -lPhysics -lMathCore -lThread -lMultiProc -lROOTDataFrame -pthread -lm -ldl -rdynamic
GLIBS         = $(ROOTGLIBS) $(SYSLIBS)
#------------------------------------------------------------------------------
%.o : %.cxx
	$(CXX) -std=legacy -c -o $@ $(CXXFLAGS) $(GLIBS) $<


  VPATH = ../../src/dir/hcoeff:../../src/dir/src: \
  	../../src/onef/hcoeff:../../src/onef/src: \
	../../src/histo:../../src/histo/perlmod:../../src/ntuple: \
	../../src/main:../../src/miscel:../../src/vector

%.o : %.f
	$(FC) -std=legacy -c -o $@ $(FFLAGS) $<
%.o : %.f90
	$(FC) -std=legacy -c -o $@ $(FFLAGS) $<

FRAG = fragfun_all.o

MDIR = htermone_jet.o crossone_jet.o onephotd_jet.o dir_phot_jet.o \
	h12one_jet.o h13one_jet.o h23one_jet.o h14one_jet.o \
	h24one_jet.o h34one_jet.o consone_jet.o 
	
MONE = htermtwo_jet.o crosstwo_jet.o onephoto_jet.o one_phot_jet.o \
	h12two_jet.o h13two_jet.o h23two_jet.o h14two_jet.o \
	h24two_jet.o h34two_jet.o constwo_jet.o

  MC = alfas_lhapdf.o distfunb_lhapdf.o param_lhapdf.o secteff.o alfa_qed.o \
	choiscale.o charnum.o modified_trig.o luxury.o

ifeq ($(CHOIXHISTO),ntuple)
# creation d'un ntuple
  MAIN = doubletosingled.o isolement.o \
  jet_definition.o  doubletosingleo.o error_pdf.o ftn_c_ntuple.o end_root_ntuple.o \
  init_root_ntuple.o remplie_ntuple.o ntuple.o onephot_jet.o
endif
ifeq ($(CHOIXHISTO),histo)
# resultats directement dans les histo
  MAIN = ftn_c.o onephot_jet_histo.o histo_selection_jet.o isolement.o \
  jet_definition.o doubletosingled_histo.o further_param.o \
  doubletosingleo_histo.o  error_pdf.o \
  remplie.o init_root.o end_root.o
endif

MCV = vecsum.o vecaddmulcons.o vecaddvec.o vecinit.o \
	vecmuladd.o vecmulcons.o vecmulsub.o \
	vecmulvec.o

onephotFocalAcc_2022_11_19_dir_NLO_8800GeV_5Mevts_NNPDF40_0_NNPDF40_0_iso2GeVinR04_scl_10_10_10_BORNBOX: $(MAIN) $(MC) $(FRAG) $(MDIR) $(MONE) $(MCV)
	$(LINKER) $(LDFLAGS) -o $@.exe $^ \
	-L$(GRACELDIR) -l$(BASESLIB) \
	-L$(FRAGDIR) -lfrag \
	-L$(NPDFDIR) -lpdfa \
	$(LHAPDFLIB) \
	$(ADD_LIB) $(LIBS) 

onehadtest_a: $(MAIN) $(MC) $(FRAG) onephotd_jet_dum.o $(MONE) $(MCV)
	$(LINKER) $(LDFLAGS) -o $@.exe $^ \
	-L$(GRACELDIR) -l$(BASESLIB) \
	-L$(FRAGDIR) -lfrag \
	-L$(NPDFDIR) -lpdfa \
	$(LHAPDFLIB) \
	$(ADD_LIB) $(LIBS) 

bases:
	cd $(GRACELDIR); make -f $(MAKEBASES);

fraglib:
	cd $(FRAGDIR);make "FC = $(FC)";

npdflib:
	cd $(NPDFDIR);make "FC = $(FC)";

clean:	
	rm -f *.exe
	cd $(GRACELDIR);rm -f *.o *.a 
	cd $(NPDFDIR);rm -f *.o libpdfa.a
	cd $(FRAGDIR);rm -f *.o libfrag.a
	rm -f $(MAIN) $(MC) $(MDIR) $(MONE) $(FRAG) $(MCV) \
	onephotd_jet_dum.o *.mod




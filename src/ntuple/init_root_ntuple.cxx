#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TNtuple.h"
#include "TTree.h"
#include "TRandom.h"
#include "diphox_evt.h"

TFile *hfile;
TTree *t2;
diphox_evt_t dip;
norma_evt_t norma_cxx;

void Init_ntuple(char* path_rootfile)
{
	// Create a new ROOT binary machine independent file.
	// Note that this file may contain any kind of ROOT objects, histograms,
	// pictures, graphics objects, detector geometries, tracks, events, etc..
	// This file is now becoming the current directory.
        
	hfile = new TFile(path_rootfile,"RECREATE","");
	// Create the Ntuple
	t2 = new TTree("t2","exemple de Tree avec Diphox"); 
        t2->SetMaxTreeSize(10000000000);
	//	
	t2->Branch("iprov",&dip.iprov,"iprov/I");
	t2->Branch("ntrack",&dip.ntrack,"ntrack/I");
	t2->Branch("nb_member",&dip.nb_member,"nb_member/I");
	t2->Branch("x3",&dip.x3,"x3/D");
	t2->Branch("energy",dip.e,"e[ntrack]/D");
	t2->Branch("px",dip.px,"px[ntrack]/D");
	t2->Branch("py",dip.py,"py[ntrack]/D");
	t2->Branch("pz",dip.pz,"pz[ntrack]/D");
	//~ t2->Branch("pdf_weight",dip.pdf_weight,"pdf_weight[nb_member]/D");
	//t2->Branch("x3",&dip.x3,"x3/F");
	//~ t2->Branch("x4",&dip.x4,"x4/F");
	//t2->Branch("energy",dip.e,"e[ntrack]/F");
	//t2->Branch("px",dip.px,"px[ntrack]/F");
	//t2->Branch("py",dip.py,"py[ntrack]/F");
	//t2->Branch("pz",dip.pz,"pz[ntrack]/F");
	t2->Branch("pdf_weight",dip.pdf_weight,"pdf_weight[nb_member]/F");
}

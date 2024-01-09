#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TNtuple.h"
#include "TRandom.h"

TFile *hfile;
TH1D *hp20;
TH1D *hp21;
TH1D *hp22;
TH1D *hp40;
TH1D *hp41;

void InitH1(char* path_rootfile)
{
	Double_t xx1[14] = {6.,8.,10.,12.,14.,16.,18.,20.,25.,30.,40.,60.,80.,100};
	Double_t xx2[14] = {6.,8.,10.,12.,14.,16.,18.,20.,25.,30.,40.,60.,80.,100};
	// Create a new ROOT binary machine independent file.
	// Note that this file may contain any kind of ROOT objects, histograms,
	// pictures, graphics objects, detector geometries, tracks, events, etc..
	// This file is now becoming the current directory.

	// hfile = new TFile("../Test_2024_01_09_nNNPDF30_0_nNNPDF30_0_scl10_10_10_dir_5020_yminneg08_ymax08_R04_E2/ggdTest_2024_01_09_nNNPDF30_0_nNNPDF30_0_scl10_10_10_dir_5020_yminneg08_ymax08_R04_E2.root","RECREATE","");
	hfile = new TFile(path_rootfile,"RECREATE","");

  // Create some histograms 
	hp20   = new TH1D("hp20","d#sigma^{#gamma}_{dir,TRUE}/dp_{T}^{#gamma}(-0.8<y<0.8)",1000,0.,250.);
	hp21   = new TH1D("hp21","Photon(-0.8<y<0.8)Jet(-5.8<y<5.8)",1000,0.,250.);
	hp22   = new TH1D("hp22","d#sigma^{#gamma}_{dir,TRUE}/dy^{#gamma}",200,-10.,10.);
	hp40   = new TH1D("hp40","dsigmalo/dptgamma",13,xx1);
	hp41   = new TH1D("hp41","dsigmanlo/dptgamma",13,xx2);
  
}

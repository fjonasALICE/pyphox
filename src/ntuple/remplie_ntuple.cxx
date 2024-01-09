#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TNtuple.h"
#include "TTree.h"
#include "TRandom.h"
#include "Riostream.h"
#include "diphox_evt.h"


extern TTree *t2;
extern diphox_evt_t dip;

void remplie() {
  //cout << " test remplie_ntuple :" << dip.ntrack << "\n";
  //cout << " test remplie_ntuple :" << dip.x3 << "\n";
	t2->Fill();
	return;
}

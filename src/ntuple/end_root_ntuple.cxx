#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TNtuple.h"
#include "TTree.h"
#include "TRandom.h"
#include "Riostream.h"
#include "diphox_evt.h"
#include "TVector.h"

extern TFile *hfile;
extern TTree *t2;
extern norma_evt_t norma_cxx;

void End_ntuple()
{
	// Save all objects in this file
  float my_tab[3];
  my_tab[0] = float(norma_cxx.nb_evt);
  my_tab[1] = norma_cxx.xsec;
  my_tab[2] = norma_cxx.sqrt_s;
  Float_t *tab = my_tab;
  TVectorT<float> *v = new TVectorT<float> (3,tab);
  t2->GetUserInfo()->Add(v);
  hfile = t2->GetCurrentFile();
  hfile->Write();
  // Close the file. Note that this is automatically done when you leave
  // the application.
  hfile->Close();
}

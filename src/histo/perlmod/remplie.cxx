#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TNtuple.h"
#include "TRandom.h"
#include "Riostream.h"

extern TH1D *hp20;
extern TH1D *hp21;
extern TH1D *hp22;
extern TH1D *hp40;
extern TH1D *hp41;

void remplie(Int_t iprov,Double_t pt3,Double_t y3,Double_t ptjet_lead,Double_t yjet_lead,
            Double_t fi_gamma_jet,Double_t qt_gamma_jet,Double_t edep,Double_t z_gamma_trig,
            Double_t z_jet_trig,Double_t x_obs_plus,Double_t x_obs_moins,
            Double_t x_ll_plus,Double_t x_ll_moins,Double_t weight)
{
if (iprov ==11) {
if  (-0.8 <=y3 && y3 <=0.8)
 {
  hp40->Fill(pt3,weight);
}
}
if  (-0.8 <=y3 && y3 <=0.8)
 {
  hp20->Fill(pt3,weight);
}
if  ((-0.8 <=y3 && y3 <=0.8)
 && (-5.8 <=yjet_lead && yjet_lead <=5.8)
) {
  hp21->Fill(pt3,weight);
}
  hp22->Fill(y3,weight);
if  (-0.8 <=y3 && y3 <=0.8)
 {
  hp41->Fill(pt3,weight);
}

}

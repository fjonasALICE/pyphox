#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TNtuple.h"
#include "TRandom.h"
#include "Riostream.h"

extern TFile *hfile;
extern TH1D *hp20;
extern TH1D *hp21;
extern TH1D *hp22;
extern TH1D *hp40;
extern TH1D *hp41;

void EndH1(Int_t numb_event,Double_t first_int)
{
	const Int_t nb_histo_equi = 3;
	const Int_t nb_histo_nonequi = 2;
	Int_t i;
	Double_t bin_content,bin_error;
	Double_t bin_size[nb_histo_equi];
	Double_t norma[nb_histo_equi];
	Int_t bin[nb_histo_equi] = {1000,1000,200};
	Double_t xmin[nb_histo_equi] = {0.,0.,-10.};
	Double_t xmax[nb_histo_equi] = {250.,250.,10.};
	for (i=0;i<nb_histo_equi;i++) {
	  bin_size[i] = (xmax[i]-xmin[i])/(Double_t)bin[i];
	  norma[i] = first_int/((Double_t)numb_event*bin_size[i]);
	}
	for(i=0;i<bin[0];i++) {
		bin_content = hp20->GetBinContent(i+1);
		bin_error = hp20->GetBinError(i+1);
		bin_content = bin_content*norma[0];
		bin_error = bin_error*norma[0];
		hp20->SetBinContent(i+1,bin_content);
		hp20->SetBinError(i+1,bin_error);
	}
	for(i=0;i<bin[1];i++) {
		bin_content = hp21->GetBinContent(i+1);
		bin_error = hp21->GetBinError(i+1);
		bin_content = bin_content*norma[1];
		bin_error = bin_error*norma[1];
		hp21->SetBinContent(i+1,bin_content);
		hp21->SetBinError(i+1,bin_error);
	}
	for(i=0;i<bin[2];i++) {
		bin_content = hp22->GetBinContent(i+1);
		bin_error = hp22->GetBinError(i+1);
		bin_content = bin_content*norma[2];
		bin_error = bin_error*norma[2];
		hp22->SetBinContent(i+1,bin_content);
		hp22->SetBinError(i+1,bin_error);
	}

	Double_t xnorma;
	Double_t xsize;
	Double_t xx1[14] = {6.,8.,10.,12.,14.,16.,18.,20.,25.,30.,40.,60.,80.,100};
	for (i=0;i<=12;i++) {
		xsize = xx1[i+1]-xx1[i];
		xnorma = first_int/((Double_t)numb_event*xsize);
		bin_content = hp40->GetBinContent(i+1);
		bin_error = hp40->GetBinError(i+1);
		bin_content = bin_content*xnorma;
		bin_error = bin_error*xnorma;
		hp40->SetBinContent(i+1,bin_content);
		hp40->SetBinError(i+1,bin_error);
	}

	Double_t xx2[14] = {6.,8.,10.,12.,14.,16.,18.,20.,25.,30.,40.,60.,80.,100};
	for (i=0;i<=12;i++) {
		xsize = xx2[i+1]-xx2[i];
		xnorma = first_int/((Double_t)numb_event*xsize);
		bin_content = hp41->GetBinContent(i+1);
		bin_error = hp41->GetBinError(i+1);
		bin_content = bin_content*xnorma;
		bin_error = bin_error*xnorma;
		hp41->SetBinContent(i+1,bin_content);
		hp41->SetBinError(i+1,bin_error);
	}

	// Save all objects in this file
	hfile->Write();
	// Close the file. Note that this is automatically done when you leave
	// the application.
	hfile->Close();
}

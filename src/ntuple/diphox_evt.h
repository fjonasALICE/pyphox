const Int_t MAXTRK = 3;
const Int_t IMAX_ERRORPDF=1000;

typedef struct {
  Int_t iprov;
  Int_t ntrack;
  Int_t nb_member;
  Double_t x3;
  Double_t e[MAXTRK];
  Double_t px[MAXTRK];
  Double_t py[MAXTRK];
  Double_t pz[MAXTRK];
  Float_t pdf_weight[IMAX_ERRORPDF];
} diphox_evt_t;
/*typedef struct {*/
/*Int_t iprov;*/
/*Int_t ntrack;*/
/*Int_t nb_member;*/
/*Float_t x3;*/
/*Float_t e[MAXTRK];*/
/*Float_t px[MAXTRK];*/
/*Float_t py[MAXTRK];*/
/*Float_t pz[MAXTRK];*/
/*Float_t pdf_weight[IMAX_ERRORPDF];*/
/*} diphox_evt_t;*/

typedef struct {
	Int_t nb_evt;
	Float_t sqrt_s,xsec;
} norma_evt_t;

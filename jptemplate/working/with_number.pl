# initialisation
$fichier_sortie = "temp.dat";
$fichier_exe = "lance.exe";
if (-e $fichier_exe) {
  unlink $fichier_exe;
}
#
# $case = "case1";
# $case = "case2";
# $case = "case3";
$case = "case4";
#
# Pt_gamma = [26,30,34,39,44,50,60,70,80,90,110,130,150,170,200,230,300];
# $r_pt_gamma = [26,30,34,39,44,50,60,70,80,90,110,130,150,170,200,230,300];
# region 1 et 2
# $r_pt_gamma = [26,30,34,39,44,50,60,70,80,90,110,130,150,170,200,230,300];
# region 3 et 4
if ( ($case eq "case2") || ($case eq "case4")) {
  $r_pt_gamma = [30,34,39,44,50,60,70,80,90,110,130,150,200,300];
}
elsif ( ($case eq "case1") || ($case eq "case3")) {
  $r_pt_gamma = [30,34,39,44,50,60,70,80,90,110,130,150,170,200,230,300];
}
# gamETA = [-1 - 0] and [0 - 1.]
if ($case eq "case1") {
  $rap_gamma_max = 0.; 
  $rap_gamma_min = -1.;
  $rap_jet_max = 0.;
  $rap_jet_min = -0.8;
}
elsif ($case eq  "case2") {
  $rap_gamma_max = 0.; 
  $rap_gamma_min = -1.;
  $rap_jet_max = -1.5;
  $rap_jet_min = -2.5;
}
elsif ($case eq "case3") {
  $rap_gamma_max = 0.; 
  $rap_gamma_min = -1.;
  $rap_jet_max = 0.8;
  $rap_jet_min = 0.;
}
elsif ($case eq "case4") {
  $rap_gamma_max = 0.; 
  $rap_gamma_min = -1.;
  $rap_jet_max = 2.5;
  $rap_jet_min = 1.5;
}
# jetETA = [-0.8-0; 0.-0.8; -2.5- -1.5; 1.5-2.5]
# $rap_jet_max = 0.;
# $rap_jet_min = -0.8;
# $rap_jet_max = -1.5;
# $rap_jet_min = -2.5;
# $rap_jet_max = 0.8;
# $rap_jet_min = 0.;
# $rap_jet_max = 2.5;
# $rap_jet_min = 1.5;
# case 1st:
#        (GamETA > -1.0 && GamETA < 0.0 && JetETA > -0.8 && JetETA < 0.0)
#        OR
#        (GamETA >  0.0 && GamETA < 1.0 && JetETA >  0.0 && JetETA < 0.8)
# case 2nd:
#        (GamETA > -1.0 && GamETA < 0.0 && JetETA >  0.0 && JetETA < 0.8)
#        OR
#        (GamETA >  0.0 && GamETA < 1.0 && JetETA > -0.8 && JetETA < 0.0)
# case 3rd:
#        (GamETA > -1.0 && GamETA < 0.0 && JetETA > -2.5 && JetETA <-1.5)
#        OR
#        (GamETA >  0.0 && GamETA < 1.0 && JetETA >  1.5 && JetETA < 2.5)
# case 4th:
#        (GamETA > -1.0 && GamETA < 0.0 && JetETA >  1.5 && JetETA < 2.5)
#        OR
#        (GamETA >  0.0 && GamETA < 1.0 && JetETA > -2.5 && JetETA <-1.5)
#
# $name_file = "_phojet_incl_".$case."_pts2_";
$name_file = "_phojet_incl_".$case."_pts2_61m_sm_";
# $name_file = "_phojet_incl_".$case."_pt_";
#
open(EWRITE,">param_perl.txt") || die "cannot open param_perl.txt";
  print EWRITE "[".join(',',@$r_pt_gamma)."]\n";  
  print EWRITE "$rap_gamma_max\n";  
  print EWRITE "$rap_gamma_min\n";  
  print EWRITE "$rap_jet_max\n";  
  print EWRITE "$rap_jet_min\n";  
  print EWRITE "$case\n";  
  print EWRITE "$name_file\n";  
close(EWRITE);
$nmax = $#$r_pt_gamma-1;
print " test: $nmax\n";
#
# for ($n=0;$n<=15;$n++) {
for ($n=0;$n<=$nmax;$n++) {
# $n = 0;
$pt_gamma_max = $r_pt_gamma->[$n+1];
$pt_gamma_min = $r_pt_gamma->[$n];
# jet pTmin=0.65 *pTgamma for pTgamm in {26-39};
# jet pTmin=0.70 *pTgamma for pTgamm in {39-80} and
# jet pTmin=0.80 *pTgamma for pTgamm in {80-300}
# if ($r_pt_gamma->[$n] <= 39) {
# $pt_jet_min = 0.65*$r_pt_gamma->[$n]
# }
# elsif ( ($r_pt_gamma->[$n] > 39) && ($r_pt_gamma->[$n] <= 80) ) {
# $pt_jet_min = 0.70*$r_pt_gamma->[$n]
# }
# elsif ( ($r_pt_gamma->[$n] > 80) && ($r_pt_gamma->[$n] <= 300) ) {
# $pt_jet_min = 0.80*$r_pt_gamma->[$n]
# }
$pt_jet_min = 15;
#
$name_run = $name_file.$n;
open(EWRITE,">$fichier_sortie") || die "cannot open $fichier_sortie";
#
print EWRITE "#########################################################################\n";
print EWRITE "# * This file is treated by a perlscript, so all lines beginning by # are\n";
print EWRITE "#   not read. \n";
print EWRITE "# * The ordering of the parameters cannot be changed without changing \n";
print EWRITE "#   the  perlscript\n";
print EWRITE "#########################################################################\n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "#                  NAMES AND PATHS FOR OUTPUT FILES\n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Name of the run: string which serves to form : \n";
print EWRITE "# 1) the names of the input parameter files,\n";
print EWRITE "# 2) the names of the executable files ,\n";
print EWRITE "# 3) the path to save the .bs files from the working directory into\n";
print EWRITE "#    the following directories which will be created if they do not \n";
print EWRITE "#    exist:\n";
print EWRITE "#    result.string/dir.string\n";
print EWRITE "#    result.string/onef.string\n";
print EWRITE "#    result.string/twof.string\n";
print EWRITE "# Note that only alpha-numeric characters are allowed, namely\n";
print EWRITE "# letters, numbers or _\n";
print EWRITE "#\n";
print EWRITE "$name_run\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Name for the histo: string which serves to form\n";
print EWRITE "# the names of the histograms or of the ntuple.\n";
print EWRITE "$name_run\n";
print EWRITE "#########################################################################\n";
print EWRITE "# String to form the path where the ntuple or the histograms will be put\n";
print EWRITE "#\n";
print EWRITE "../pawres\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Please select the format of the output: \"histo\" will give directly \n";
print EWRITE "# histograms, \"ntuple\" will produce a ntuple. Here also the Makefile is \n";
print EWRITE "# modified in order to load only the necessary files\n";
print EWRITE "#\n";
print EWRITE "histo\n";
print EWRITE "#########################################################################\n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "#                         INPUT\n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "#########################################################################\n";
print EWRITE "#                         PDF\n";
print EWRITE "#########################################################################\n";
print EWRITE "# With this version of the code, only cteq5, cteq6, mrs99, mrst01, bbs or  \n";
print EWRITE "# pdf from pdflib (cernlib) can be used. If the user wants to use another \n";
print EWRITE "# PDF set, he has to provide a new file which interfaces his preferred \n";
print EWRITE "# choice with pdflib, see for example the file \"pftopdg_mrs99.f\" in the \n";
print EWRITE "# directory \"pdfs\". \n";
print EWRITE "# First the user has to choose if the PDF come from PDFLIB or not\n";
print EWRITE "# Please select \"pdflib\" or \"not_pdflib\"\n";
print EWRITE "# Note that if the user wants to have different PDF for\n";
print EWRITE "# each hadron in initial state, he must choose \"pdflib\"\n";
print EWRITE "not_pdflib\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Type of initial hadron H1:0 proton, 1 anti-proton, 3 pion\n";
print EWRITE "#\n";
print EWRITE "0\n";
print EWRITE "#########################################################################\n";
print EWRITE "# If \"pdflib\" has been chosen, put here the number of the group\n";
print EWRITE "# else select a PDF: possible sets are\n";
print EWRITE "# \"mrs99\",\"mrst01\",\"mrst04\",\"cteq5\",\"cteq6\",\"bbs\"\n";
print EWRITE "#\n";
print EWRITE "cteq6\n";
print EWRITE "#########################################################################\n";
print EWRITE "#Following the pdflib convention (hadron h1): nset\n";
print EWRITE "#if \"pdflib\" is not used\n";
print EWRITE "#	if \"mrs99\" has been selected above, put 198+mode \n";
print EWRITE "#      (for different choices see pdfs/mrs99/mrs99.f)\n";
print EWRITE "#                  for example for mode 2 (higher gluon) put 198+2=200\n";
print EWRITE "#	if \"mrst01\" has been selected above, put 199+mode \n";
print EWRITE "#      (for different choices see pdfs/mrst01/mrst01.f)\n";
print EWRITE "#                  for example for mode 1 (default) put 199+1=200\n";
print EWRITE "#	if \"cteq5\" has been selected above, put iset \n";
print EWRITE "#      (for different choices see pdfs/cteq5/Ctq5Pdf.f)\n";
print EWRITE "#	if \"cteq6\" has been selected above, put nset = 1=cteq6m\n";
print EWRITE "#	nset = 200 = cteq6.1m or see the file jetphox_1.0/pdfs/pftopdg_cteq61.f\n";
print EWRITE "#	this number has no importance with bbs \n";
print EWRITE "#	if \"mrst04\" has been selected above, put 1 (2 is for NNLO) \n";
print EWRITE "#\n";
# print EWRITE "1\n";
print EWRITE "200\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Type of initial hadron H2:0 proton, 1 anti-proton, 3 pion\n";
print EWRITE "#\n";
print EWRITE "1\n";
print EWRITE "#########################################################################\n";
print EWRITE "# If \"pdflib\" has been chosen, put here the number of the group\n";
print EWRITE "# else select a PDF: possible sets are\n";
print EWRITE "# \"mrs99\",\"mrst01\",\"mrst04\",\"cteq5\",\"cteq6\",\"bbs\"\n";
print EWRITE "#\n";
print EWRITE "cteq6\n";
print EWRITE "#########################################################################\n";
print EWRITE "#Following the pdflib convention (hadron h1): nset\n";
print EWRITE "#if \"pdflib\" is not used\n";
print EWRITE "#	if \"mrs99\" has been selected above, put 198+mode \n";
print EWRITE "#      (for different choices see pdfs/mrs99/mrs99.f)\n";
print EWRITE "#                  for example for mode 2 (higher gluon) put 198+2=200\n";
print EWRITE "#	if \"mrst01\" has been selected above, put 199+mode \n";
print EWRITE "#      (for different choices see pdfs/mrst01/mrst01.f)\n";
print EWRITE "#                  for example for mode 1 (default) put 199+1=200\n";
print EWRITE "#	if \"cteq5\" has been selected above, put iset \n";
print EWRITE "#      (for different choices see pdfs/cteq5/Ctq5Pdf.f)\n";
print EWRITE "#	if \"cteq6\" has been selected above, put nset = 1=cteq6m\n";
print EWRITE "#	nset = 200 = cteq6.1m or see the file jetphox_1.0/pdfs/pftopdg_cteq61.f\n";
print EWRITE "#	this number has no importance with bbs \n";
print EWRITE "#	if \"mrst04\" has been selected above, put 1 (2 is for NNLO) \n";
print EWRITE "#\n";
# print EWRITE "1\n";
print EWRITE "200\n";
print EWRITE "#########################################################################\n";
print EWRITE "#                     FRAGMENTATION FUNCTION\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Type of fragmentation functions for hadron/photon 3\n";
print EWRITE "#\n";
print EWRITE "# conventions: 3digit number, xyz\n";
print EWRITE "# \n";
print EWRITE "# x = group label:\n";
print EWRITE "#\n";
print EWRITE "#              x = 1  :  bkk (binnewies/kniehl/kramer)         \n";
print EWRITE "#              x = 2  :  kkp (kniehl/kramer/poetter) \n";
print EWRITE "#              x = 3  :  owens (only for photon)\n";
print EWRITE "#              x = 4  :  bouhris et al. (photon) (no LO) \n";
print EWRITE "#              x = 5  :  bouhris et al. (all charged) (no LO)\n";
print EWRITE "#              x = 6  :  Kretzer (only y=1,2,7 possible)\n";
print EWRITE "#\n";
print EWRITE "# y = hadron label:             \n";
print EWRITE "#\n";
print EWRITE "#     y   = 0 : gamma\n";
print EWRITE "#     y   = 1 : (pi^+ + pi^-)  /2\n";
print EWRITE "#     y   = 2 : (k^+ + k^-)    /2   \n";
print EWRITE "#     y   = 3 : (k^0 + k^0_bar)/2   \n";
print EWRITE "#     y   = 4 : (p + p_bar)    /2\n";
print EWRITE "#     y   = 5 : (pi^0)\n";
print EWRITE "#     y   = 6 : (n + n_bar)    /2\n";
print EWRITE "#     y   = 7 : (h^+ + h^-) \n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "# \n";
print EWRITE "#\n";
print EWRITE "# z = iset: \n";
print EWRITE "#\n";
print EWRITE "#      iset = 0 :  lo\n";
print EWRITE "#      iset = 1 : nlo for bkk,kkp and Kretzer, nlo set for Bourhis et al.\n";
print EWRITE "#      iset = 2 : further sets\n";
print EWRITE "#\n";
print EWRITE "# For example:     \n";
print EWRITE "#\n";
print EWRITE "# 150  : bkk, pi_0,  lo\n";
print EWRITE "# 151  : bkk, pi_0, nlo\n";
print EWRITE "# \n";
print EWRITE "# 250  : kkp, pi_0,  lo\n";
print EWRITE "# 251  : kkp, pi_0, nlo\n";
print EWRITE "#\n";
print EWRITE "# 301  : owens,   gamma, nlo\n";
print EWRITE "#\n";
print EWRITE "# 401  : bouhris, gamma, nlo, set1\n";
print EWRITE "# 402  : bouhris, gamma, nlo, set2\n";
print EWRITE "#\n";
print EWRITE "# 571  : bouhris, all charged, nlo, set1\n";
print EWRITE "# 572  : bouhris, all charged, nlo, set2\n";
print EWRITE "# 573  : bouhris, all charged, nlo, set3\n";
print EWRITE "#\n";
print EWRITE "# 671  : kretzer, all charged, nlo\n";
print EWRITE "#\n";
print EWRITE "402\n";
print EWRITE "#########################################################################\n";
print EWRITE "#                         SCALES\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Choice of the scale:\n";
print EWRITE "#       1 (pt3+pt4)*cm, \n";
print EWRITE "#       2 sqrt(pt3^2+pt4^2)*cm, \n";
print EWRITE "#       3 mgg*cm,\n";
print EWRITE "#       4 cm*pt_photon\n";
print EWRITE "#       5 cm*pt_photon*sqrt((1+exp(-2*abs(y*)))/2)\n";
print EWRITE "# pt3 and pt4 are the transverse momentum of the two photons, mgg is \n";
print EWRITE "# the invariant mass of them\n";
print EWRITE "#\n";
print EWRITE "5\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Value of cm for initial state factorisation scale\n";
print EWRITE "#\n";
# print EWRITE "1.D0\n";
print EWRITE "0.5D0\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Value of cm for renormalisation scale\n";
print EWRITE "#\n";
# print EWRITE "1.D0\n";
print EWRITE "0.5D0\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Value of cm for final state factorisation scale\n";
print EWRITE "#\n";
# print EWRITE "1.D0\n";
print EWRITE "0.5D0\n";
print EWRITE "#########################################################################\n";
print EWRITE "#                         COUPLING, ETC...\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Number of loops in alpha_em:0 constant else running (routine taken from \n";
print EWRITE "# jetset)\n";
print EWRITE "#\n";
print EWRITE "0\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Number of loops in alpha_s evolution:1 LO, 2 NLO (value obtained by \n";
print EWRITE "# numerical inversion of the solution of R.G equation)\n";
print EWRITE "#\n";
print EWRITE "2\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Number of active flavours\n";
print EWRITE "#\n";
print EWRITE "5\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Value of (hbar*c)^2 in GeV^2 pb to get cross sections in pb\n";
print EWRITE "#\n";
print EWRITE ".38935D+9\n";
print EWRITE "#########################################################################\n";
print EWRITE "# For the direct part:0 born only,1 box only, 2 born+box\n";
print EWRITE "#\n";
print EWRITE "0\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Value of PTM in GeV\n";
print EWRITE "# Typically PTM must be of the order of 0.5 % of the PT minimum of the\n";
print EWRITE "# final photon/hadron \n";
print EWRITE "# for LHC, with PT of the photons/hadrons > 25 GeV, PTM = 0.1 GeV works\n";
print EWRITE "# for Tevatron, with PT of the photons/hadrons > 12 GeV, PTM = 0.05 GeV \n";
print EWRITE "# works\n";
print EWRITE "#\n";
print EWRITE "0.05D0\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Value of R_th\n";
print EWRITE "# R_th = 0.1 or less\n";
print EWRITE "#\n";
print EWRITE ".1D0\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Please choose which contribution of the program you want to run:\n";
print EWRITE "# dir		direct contribution\n";
print EWRITE "# onef		one fragmentation contribution\n";
print EWRITE "# the items must be separated by a comma!!!!\n";
print EWRITE "# for example: dir,onef will run the two contributions\n";
print EWRITE "#\n";
print EWRITE "dir,onef\n";
print EWRITE "#########################################################################\n";
print EWRITE "# If TRUE the Higher Order corrections are calculated\n";
print EWRITE "# else if FALSE no H.O. corrections are computed\n";
print EWRITE "# the number of values here must match the number of contributions \n";
print EWRITE "# already selected\n";
print EWRITE "# for example if dir,onef has been selected and one wants to compute\n";
print EWRITE "# all these three contributions at NLO, you must put here\n";
print EWRITE "# TRUE,TRUE if you put only one TRUE the program assumes that you\n";
print EWRITE "# want to compute all the contribution at HO\n";
print EWRITE "#\n";
print EWRITE "TRUE\n";
print EWRITE "#########################################################################\n";
print EWRITE "# If TRUE the Leading Order terms are calculated\n";
print EWRITE "# else if FALSE no L.O. terms are computed\n";
print EWRITE "# the number of values here not necessarily match the number of contributions \n";
print EWRITE "# already selected\n";
print EWRITE "#\n";
print EWRITE "TRUE\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Set to TRUE if you want to compute the physical integrated cross section \n";
print EWRITE "# (for example to  test the PTM or R_th dependence)\n";
print EWRITE "# here one assumes that the user wants to use the same option for all\n";
print EWRITE "# contributions\n";
print EWRITE "#\n";
print EWRITE "TRUE\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Set to TRUE if you want to compute differential cross sections through a \n";
print EWRITE "# NTUPLE or HISTOGRAMS. It is used to make the grid. In this case, the \n";
print EWRITE "# program integrates the absolute value of differential cross sections. \n";
print EWRITE "# Note that this flag acts only on the creation of .bs files (creation \n";
print EWRITE "# of the grids).\n";
print EWRITE "# Here one assumes that the user wants to use the same option for all\n";
print EWRITE "# contributions\n";
print EWRITE "#\n";
print EWRITE "FALSE\n";
print EWRITE "#########################################################################\n";
print EWRITE "# If true only generation. This flag acts only on the generation of \n";
print EWRITE "# events just by reading the already created .bs files\n";
print EWRITE "# here one assumes that the user wants to use the same option for all\n";
print EWRITE "# contributions\n";
print EWRITE "#\n";
print EWRITE "FALSE\n";
print EWRITE "#########################################################################\n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "#                         KINEMATICS\n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "#########################################################################\n";
print EWRITE "#0 collider mode, 1 fixed target mode\n";
print EWRITE "#\n";
print EWRITE "0\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Value of ebeam or sqrt(s) depending on the preceeding flag\n";
print EWRITE "#\n";
print EWRITE "1960.D0\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Value of the maximal rapidity for the photon\n";
print EWRITE "#\n";
print EWRITE "".&write_double($rap_gamma_max)."\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Value of the minimal rapidity for the photon\n";
print EWRITE "#\n";
print EWRITE "".&write_double($rap_gamma_min)."\n";
print EWRITE "#########################################################################\n";
print EWRITE "#Value of ptmax in GeV for the photon\n";
print EWRITE "#\n";
print EWRITE "".&write_double($pt_gamma_max)."\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Value of ptmin in GeV for the photon\n";
print EWRITE "#\n";
print EWRITE "".&write_double($pt_gamma_min)."\n";
print EWRITE "#########################################################################\n";
print EWRITE "#PARAMETERS FOR ISOLATION\n";
print EWRITE "#########################################################################\n";
print EWRITE "# flag to choose different maximum transverse energy cut deposited in \n";
print EWRITE "# the isolation cone\n";
print EWRITE "# if there is no isolation criterion, choose 1 and put the maximum \n";
print EWRITE "# Et allowed: sqrt(s)/2\n";
print EWRITE "# 	1 value in GeV\n";
print EWRITE "#       2 Fraction of photon transverse momentum\n";
print EWRITE "2	\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Radius of isolation cone\n";
print EWRITE "#\n";
print EWRITE "0.4D0\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Depending on the flag: value of Etmax (in GeV) or fraction of photon\n";
print EWRITE "# transverse momentum\n";
print EWRITE "#\n";
print EWRITE "0.07D0\n";
print EWRITE "#########################################################################\n";
print EWRITE "#PARAMETERS FOR JETS\n";
print EWRITE "#########################################################################\n";
print EWRITE "# flag to choose inclusive mode or photon+jet; inclusive: 0, jets: 1\n";
print EWRITE "1\n";
print EWRITE "##########################################################################\n";
print EWRITE "# choice of jet algorithm: \"kt\": Durham kt, \"co\": cone, \"se\": with seeds\n";
print EWRITE "# \"d0\": the midpoint E scheme algorithm\n";
print EWRITE "#\n";
print EWRITE "d0\n";
print EWRITE "##########################################################################\n";
print EWRITE "# value of r_kt\n";
print EWRITE "1.D0\n";
print EWRITE "##########################################################################\n";
print EWRITE "# value of r_cone\n";
print EWRITE "0.7D0\n";
print EWRITE "##########################################################################\n";
print EWRITE "# value of r_sep\n";
print EWRITE "0.7D0\n";
print EWRITE "########################################################################\n";
print EWRITE "# choice of the merging rules: \"sn\": snow-mass, \"ho\": houches99\n";
print EWRITE "#\n";
print EWRITE "ho\n";
print EWRITE "########################################################################\n";
print EWRITE "# Acceptance of the jet: \n";
print EWRITE "# an event is accepted if:\n";
print EWRITE "#\"gp\": the highest pt jet verifies the cut in rapidity and transverse \n";
print EWRITE "# momentum\n";
print EWRITE "#\"up\":one of the jets verifies the cut in rapidity and transverse \n";
print EWRITE "# momentum\n";
print EWRITE "#\n";
print EWRITE "gp\n";
print EWRITE "##########################################################################\n";
print EWRITE "# value of ptjet_max\n";
print EWRITE "980.D0\n";
print EWRITE "##########################################################################\n";
print EWRITE "# value of ptjet_min\n";
print EWRITE "".&write_double($pt_jet_min)."\n";
print EWRITE "##########################################################################\n";
print EWRITE "# value of yjet_max\n";
print EWRITE "".&write_double($rap_jet_max)."\n";
print EWRITE "##########################################################################\n";
print EWRITE "# value of yjet_min\n";
print EWRITE "".&write_double($rap_jet_min)."\n";
print EWRITE "#########################################################################\n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "#                         BASES/SPRING\n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "#########################################################################\n";
print EWRITE "# For all the following items except for the number of tries for SPRING, \n";
print EWRITE "# the user can choose different numbers for the different contributions. \n";
print EWRITE "# If only one number is specified, the program takes this number for all \n";
print EWRITE "# the contributions\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Number of generated events in the ntuple or histograms\n";
print EWRITE "#\n";
print EWRITE "4000000\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Number of iteration for the grid step\n";
print EWRITE "#\n";
print EWRITE "20\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Number of iteration for the integration step\n";
print EWRITE "#\n";
print EWRITE "10\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Number of calls per iteration for two to three\n";
print EWRITE "#\n";
print EWRITE "5000000\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Number of calls per iteration for quasi two to two\n";
print EWRITE "#\n";
print EWRITE "2500000\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Number of calls per iteration for true two to two\n";
print EWRITE "#\n";
print EWRITE "1000000\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Number of tries for spring\n";
print EWRITE "# here one assumes that the user wants to use the same option for all\n";
print EWRITE "# contributions\n";
print EWRITE "#\n";
print EWRITE "1000\n";
print EWRITE "#########################################################################\n";
print EWRITE "# Accuracy in per cent for Bases\n";
print EWRITE "#\n";
print EWRITE "0.03D0\n";
print EWRITE "#########################################################################\n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "#                         TEST\n";
print EWRITE "#\n";
print EWRITE "#\n";
print EWRITE "# This section is only for performing tests on specific sub-processes. Do  \n";
print EWRITE "# not modify the default values below.\n";
print EWRITE "#########################################################################\n";
print EWRITE "# To select process in direct part:\n";
print EWRITE "# In some routines, there is a loop over the number of partonic \n";
print EWRITE "# sub-processes. For the direct part, the loop runs from 1 to 2\n";
print EWRITE "# the user can select one sub-process by changing the lower (upper) value\n";
print EWRITE "# of the loop (for instance putting 1 1 for the two next values the user \n";
print EWRITE "# obtains results for qi + qbi only)\n";
print EWRITE "#	qi + qk --> jet + ph\n";
print EWRITE "#\n";
print EWRITE "#	1 : d + u --> jet + ph\n";
print EWRITE "#	2 : d + dp --> jet + ph\n";
print EWRITE "#	3 : u + up --> jet + ph\n";
print EWRITE "#\n";
print EWRITE "#	qi + qbk --> jet + ph\n";
print EWRITE "#\n";
print EWRITE "#	4 : d + ub --> jet + ph\n";
print EWRITE "#	5 : d + dpb --> jet + ph\n";
print EWRITE "#	6 : u + upb --> jet + ph\n";
print EWRITE "#\n";
print EWRITE "#	7 : qi + qi --> jet + ph\n";
print EWRITE "#\n";
print EWRITE "#	qi + qbi --> jet + ph\n";
print EWRITE "#	8 : d + db --> jet + ph\n";
print EWRITE "#	9 : u + ub --> jet + ph\n";
print EWRITE "#\n";
print EWRITE "#	10 : qi + g --> jet + ph\n";
print EWRITE "#\n";
print EWRITE "#	11 : g + g --> jet + ph\n";
print EWRITE "#########################################################################\n";
print EWRITE "1\n";
print EWRITE "11\n";
print EWRITE "#########################################################################\n";
print EWRITE "# To select process in one fragmentation part:\n";
print EWRITE "# Here the loop over the partonic sub-processes runs from 1 to 18\n";
print EWRITE "# D means a quark of charge -1/3 (Db anti quark D, Dp another quark of \n";
print EWRITE "# charge -1/3 different from D)\n";
print EWRITE "# U means a quark of charge 2/3 (Ub anti quark U, Up another quark of \n";
print EWRITE "# charge 2/3 different from U)\n";
print EWRITE "#	1 : qi + qk --> jet + qk\n";
print EWRITE "#	2 : qi + qk --> jet + g\n";
print EWRITE "#	3 : qi + qbk --> jet + qbk\n";
print EWRITE "#	4 : qi + qbk --> jet + g\n";
print EWRITE "#	5 : qi + qi --> jet + qi\n";
print EWRITE "#	6 : qi + qi --> jet + g\n";
print EWRITE "#	7 : qi + qbi --> jet + qbk\n";
print EWRITE "#	8 : qi + qbi --> jet + qbi\n";
print EWRITE "#	9 : qi + qbi --> jet + g\n";
print EWRITE "#	10 : qi + g --> jet + qk\n";
print EWRITE "#	11 : qi + g --> jet + qbk\n";
print EWRITE "#	12 : qi + g --> jet + qbi\n";
print EWRITE "#	13 : qi + g --> jet + g\n";
print EWRITE "#	14 : qi + g --> jet + qi\n";
print EWRITE "#	15 : g + g --> jet + qi\n";
print EWRITE "#	16 : g + g --> jet + g\n";
print EWRITE "#########################################################################\n";
print EWRITE "1\n";
print EWRITE "16\n";
#
close(EWRITE);
#
system("perl start.pl --parameter=$fichier_sortie --histo=param_histo.indat");
#
# creation d'un executable
open(EWRITE,">>$fichier_exe") || die "cannot open $fichier_exe";
print EWRITE "run".$name_run.".exe\n";
close(EWRITE);
}
#
system("chmod +x $fichier_exe");
#
sub write_double {
  my($number) = @_;
  my $temp = "";
  if ($number =~ m/\./) {
    $temp = "$number"."D0";
  }
  else {
    $temp = "$number".".D0";
  }
  return $temp;
}

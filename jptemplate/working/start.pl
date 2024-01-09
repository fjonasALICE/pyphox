#! /usr/local/bin/perl -w
# Ce programme genere:
# 1) les trois fichiers de parametres
# 2) les directoires pour stocker les fichiers .bs
# 3) un fichier executable qui permet de tout lancer a la fois
################################################################################
# nom des fichiers de parametres
################################################################################
use Histo_equi;
use Histo_nonequi;
#
use Getopt::Long;
GetOptions("parameter=s" => \$fichier_parametre,
           "histo=s" => \$fichier_histo,
           "basedir=s" => \$base_dir);
# valeur par defaut des fichiers de parametres
unless ($fichier_parametre) {
  $fichier_parametre = "parameter.indat";
}
unless ($fichier_histo) {
  $fichier_histo = "param_histo.indat";
}
unless ($base_dir) {
  $base_dir = '';
}
################################################################################
# initialisation
################################################################################
%hash_ntuple = ("dir" => "ggd","onef" => "ggo");
%hash_var = ();
@list_var_clef = qw/path_bs nom_histo path_histo choix_histo H1 H2
 lhapdf_path choix_pdf1 choix_pdf2 choix_member1 choix_member2 error_pdf nuclear_pdf frag choix_scale 
 cm_M cm_mu cm_MF loop_alfaem loop_alfas active_flavour hbarr_c box_nobox
 ptm r_th choix_process ho lo integ_function integ_abs_function generation 
 collider_fixe_target sqrt_s ymax ymin ptmax ptmin flag_iso r_isol etmax ifjet 
 algorithm rkt rc rsep merging acceptance ptjetmax ptjetmin yjetmax yjetmin
 nb_generated seed nb_itera_grid nb_itera_integ nb_call_two_to_three
 nb_call_q_two_to_two nb_call_two_to_two nb_try accuracy 
 jmin_direct jmax_direct jmin_onef jmax_onef/;
$ivar = 0;
################################################################################
# on lit le fichier de donnees et on remplit le hashage %hash_var 
# avec les cles prises dans @list_var_clef
################################################################################
open (EREAD,$fichier_parametre)|| die "cannot open $fichier_parametre" ;
while(<EREAD>) {
  chomp;
# il faut absolument enlever les blancs en debut de ligne sinon l'ordre 
# unless ne joue pas son role
  s/^\s+//;
  unless (/^#/) {
# on enleve les blancs qui pourraient se trouver dans ou autour des 
# variables que l'on lit
    s/\s+//g; 
    $hash_var{$list_var_clef[$ivar]} = "$_";
    $ivar++;
  }
}
close (EREAD);
#pour selectionner une sortie histogramme ou ntuple
$histo = $hash_var{"choix_histo"};
# nom pour creer le chemin des directories ou seront sauves les fichiers .bs 
# on teste si cette chaine ne contient que des caracteres alpha-numeriques
if ($hash_var{"path_bs"} =~ /\W/) {  
  print "Illegal caracters in the name of the run\n";
  exit 7;
}
$name_bs = $hash_var{"path_bs"};
$main_directory ="result".$name_bs;
if ($base_dir) {
  $main_directory =join("/",$base_dir,$main_directory);
}
# test : no jet algo if ntuple is selected
if (($hash_var{"choix_histo"} eq "ntuple") && $hash_var{"ifjet"} != 0){
  print "If the option ntuple is selected, please select the inclusive photon mode (and not photon+jet)\n";
  exit(8);
}
################################################################################
# build the bases library if necessary
################################################################################
unless (-e "../../basesv5.1/libbases.a"){
print "*** Compilation of the BASES library\n";
system("make bases");}
################################################################################
# build the fraglib library if necessary
################################################################################
unless (-e "../../frag/libfrag.a"){
print "*** Compilation of the FRAG library\n";
system("make fraglib");}
################################################################################
# build the npdflib library if necessary
################################################################################
unless (-e "../../pdfa/libpdfa.a"){
print "*** Compilation of the NPDF library\n";
system("make npdflib");}
################################################################################
# check if error pdfs and histograms have been chosen altogether
################################################################################
if ( ($hash_var{error_pdf} eq "TRUE") && ($hash_var{choix_member} != 0) ){
  print "You cannot select the PDF error flag with a member different from 0!\n";
  exit (12);
} 
################################################################################
# check if hadron or photon frag functions have been selected and
# modify/compile Makefile accordingly
################################################################################
$phothad = "";
$frag = $hash_var{"frag"};
@listfrag = split(//,$frag);
$set_ff = $listfrag[0]*10 + $listfrag[1];
$dizaine = $listfrag[2];
$unite = $listfrag[3];
if ($dizaine==0) {
  $phothad = "photon";
}
#~ elsif ( ($dizaine > 5) || ($unite > 5) ) {
  #~ print "Please check the number which selects the fragmentation functions\n";
  #~ print "The set you chose is not implemented !\n";
#~ }
else {
  $phothad = "hadron";
}
#
if ($phothad eq "photon") {
  $target = "onephot";
}
else {
  $target = "onehad";
}
#
if ($set_ff > 15) {
  print "This set does not exist\n";
}
@tab_nom_ff = qw (akk08 dss hkns akk05
bkk kkp kretzer bfgwbest bfgwlarge bfgwlow kkks08 );
if ( ($set_ff <= 4) || ($set_ff == 7) || ($set_ff == 11) ) {
$dir_ff = $tab_nom_ff[$set_ff-1];
$path_ff = "../../frag/".$tab_nom_ff[$set_ff-1];
system("ln -f -s $path_ff .");}
# link for nPDF sets
$npdfset=$hash_var{"nuclear_pdf"};
if ( $npdfset>0 ){
@tab_nom_npdf = qw (nds nds eks98 eps08 hkn04 eps09 hkn07 fgs10 fgs10 );
if ( ($npdfset >=601)&&($npdfset<=631) ){
  $npdfset=6;
}
$dir_npdf = $tab_nom_npdf[$npdfset-1];
$path_npdf = "../../pdfa/".$tab_nom_npdf[$npdfset-1];
system("ln -f -s $path_npdf .");}
################################################################################
# modifie le Makefile si necessaire
################################################################################
$chemin_lhapdf =$hash_var{"lhapdf_path"};
open(EP,"Makefile") || die "cannot open Makefile" ;
open(ES,">Makefile_temp") || die "cannot create Makefile_temp" ;
while (<EP>) {
 chomp;
 if (/^CHOIXHISTO\t=/) {
   s/CHOIXHISTO\t=\s(\w+)/CHOIXHISTO\t= $histo/;
 }
 if (/^$target/) {
   s/^$target(\w*):/$target$name_bs:/;
 }
 if (/^PATHLHAPDF/) {
  s/PATHLHAPDF\t=\s([\w\/\-\.]+)/PATHLHAPDF\t= $chemin_lhapdf/
}
 print ES "$_\n";
}
close(EP);
close(ES);
system("mv Makefile_temp Makefile");
################################################################################
# creation d'un directory, s'il n'existe pas
################################################################################
unless (-e $main_directory) {
  system("mkdir $main_directory");
}
#
################################################################################
# on evalue le Pt max que l'on peut produire pour les partons
# qui reculent contre le photon
################################################################################
$racine_de_s = $hash_var{"sqrt_s"};
$racine_de_s =~ s/D0$//i;
$pt_max = eval($racine_de_s/2);
#
@list_process = split(/,/,$hash_var{"choix_process"});
################################################################################
# on teste si l'utilisateur ne fait pas de betises
################################################################################
$test_dir = 0;
foreach $process  (@list_process) {
  $test_dir = $test_dir || ($process eq "dir");
}
if (($target eq "onehad") && $test_dir) {
  print "The selection of the contributions does not match the fragmentation functions chosen\n";
  exit 1;
}
if ( ($hash_var{"H1"} <= 3) && ($hash_var{"H2"} <= 3) && ($hash_var{"nuclear_pdf"} != 0) )  {
  print "Proton PDF should be chosen for p-p and p-pbar collisions\n";
  exit 10;
}
# fin du test
################################################################################
# on genere les fichiers de parametres pour le programme fortran et 
# le fichier executable
################################################################################
$executable = "run".$name_bs.".exe";
open(EALL,">$executable") || die "cannot create $executable" ;
foreach $process (@list_process) {
  $ntuple = $hash_ntuple{"$process"};
  $name_dir = join("",$process,$name_bs);
  $name_dir = join("/",$main_directory,$name_dir);
  $name_histo = $ntuple.$hash_var{"nom_histo"};
  $name_path = join("/",$hash_var{"path_histo"},$name_histo);
  $name_dat = join("_","param",$process);
  $name_dat = $name_dat.$name_bs;
  # on compte les caracteres de $main_directory, le nombre doit etre inferieur a 128
  @temp_car = split(//,$name_dir);
  $nb_car_dir = $#temp_car+1;
  @temp_car = split(//,$name_path);
  $nb_car_path = $#temp_car+1;
  #~ print "test nb caracteres : $main_directory $nb_car_dir\n";
  if ( ($nb_car_dir > 256) || ($nb_car_path> 256) ) {
    print "the number of charcaters of the path for storing the result or the ntuple/histo exceed 256, the program stop\n";
    exit(0);
  }
  #creation d'un directory, s'il n'existe pas
  unless (-e $name_dir) {
    system("mkdir $name_dir");
  }
  #
  open(ES,">$name_dat.dat") || die "cannot create $name_dat.dat" ;
  print ES "'$name_dir/' \t\t path for the .bs file\n";
  print ES "'$name_path' \t\t path for the ntuple\n";
# ordre inverse : le code LHAGLUE puis le type des hadrons h1 et h2
  print ES "$hash_var{\"choix_pdf1\"} \t\t LHAGLUE code first pdf\n";
  print ES "$hash_var{\"choix_pdf2\"} \t\t LHAGLUE code second pdf\n";
  print ES "$hash_var{\"choix_member1\"} \t\t member of the first pdf\n";
  print ES "$hash_var{\"choix_member2\"} \t\t member of the second pdf\n";
  print ES "$hash_var{\"error_pdf\"} \t\t Flag pdf errors\n";
  print ES "$hash_var{\"H1\"} \t\t type of hadron H1:0 proton, 1 anti-proton,  aaazzz nucleus\n";
  print ES "$hash_var{\"H2\"} \t\t type of hadron H2:0 proton, 1 anti-proton,  aaazzz nucleus\n";
  print ES "$hash_var{\"nuclear_pdf\"} \t\t nPDF effects (0: none, 1: nDS, ...)\n";
  print ES "$hash_var{\"loop_alfaem\"} \t\t nb of loop in alpha_em:0 constant else jetset\n";
  print ES "$hash_var{\"box_nobox\"} \t\t for the direct part:0 born only,1 box only, 2 born+box\n"; 
  print ES "$hash_var{\"active_flavour\"} \t\t nb of active flavours\n";
  print ES "$hash_var{\"hbarr_c\"} \t\t value of (hbarr*c)^2\n";
  print ES "$hash_var{\"loop_alfas\"} \t\t flag for alpha_s : 0 given by LHAPDF, 1,2 given by solution of RGE\n";
  print ES "0 \t\t factorisation scheme:0 MSBARR, 1 DIS\n";
  print ES "0 \t\t flag to recover old results of Aurenche et al.\n";
  print ES "$hash_var{\"collider_fixe_target\"} \t\t 0 collider mode, 1 fixed target mode\n";
  print ES "$hash_var{\"sqrt_s\"} \t\t value of ebeam or sqrt(s) depending on the preceeding flag\n";
  print ES "$hash_var{\"ymax\"} \t\t value of ymax for the photon\n";
  print ES "$hash_var{\"ymin\"} \t\t value of ymin for the photon\n";
  print ES "6.D0 \t\t value of ymax for the parton forming the jet\n";
  print ES "-6.D0 \t\t value of ymin for the parton forming the jet\n";
  print ES "$hash_var{\"ptmax\"} \t\t value of ptmax in GeV for the photon\n";
  print ES "$hash_var{\"ptmin\"} \t\t value of ptmin in GeV for the photon\n";
  print ES "$pt_max.D0 \t\t value of ptmax in GeV for the jet\n";
  print ES "0.D0 \t\t value of ptmin in GeV for the jet\n";
  print ES "$hash_var{\"frag\"} \t\t type of fragmentation functions for photon/hadron :21 owens, 22 bourhis\n";
  print ES "$hash_var{\"ptm\"} \t\t value of PTM in GeV\n";
  print ES "$hash_var{\"r_th\"} \t\t value of R\n";
  print ES "$hash_var{\"jmin_direct\"} \t\t to select process in direct part\n";
  print ES "$hash_var{\"jmax_direct\"} \t\t to select process in direct part\n";
  print ES "$hash_var{\"jmin_onef\"} \t\t to select process in one brem part\n";
  print ES "$hash_var{\"jmax_onef\"} \t\t to select process in one brem part\n";
  $temp = list_to_hash1(\@list_process,[split(/,/,$hash_var{"ho"})])->{$process};
  print ES "$temp \t\t to select NLO\n";
  $temp = list_to_hash1(\@list_process,[split(/,/,$hash_var{"lo"})])->{$process};
  print ES "$temp \t\t to select LO\n";
  print ES "$hash_var{\"integ_function\"} \t\t if true only integration (integrated cross-section)\n";
  print ES "$hash_var{\"integ_abs_function\"} \t\t if true only integration (just to make the grid, no physical meaning)\n";
  print ES "$hash_var{\"generation\"} \t\t if true only generation\n";
  $temp = list_to_hash1(\@list_process,[split(/,/,$hash_var{"nb_generated"})])->{$process};
  print ES "$temp \t\t nb of generated events\n";
  $temp = list_to_hash1(\@list_process,[split(/,/,$hash_var{"seed"})])->{$process};
  print ES "$temp \t\t seed for generation\n";
  $temp = list_to_hash1(\@list_process,[split(/,/,$hash_var{"nb_itera_grid"})])->{$process};
  print ES "$temp \t\t nb of iteration for the grid step\n";
  $temp = list_to_hash1(\@list_process,[split(/,/,$hash_var{"nb_itera_integ"})])->{$process};
  print ES "$temp \t\t nb of iteration for the integration step\n";
  $temp = list_to_hash1(\@list_process,[split(/,/,$hash_var{"nb_call_two_to_three"})])->{$process};
  print ES "$temp \t\t nb of calls per iteration for two to three\n";
  $temp = list_to_hash1(\@list_process,[split(/,/,$hash_var{"nb_call_q_two_to_two"})])->{$process};
  print ES "$temp \t\t nb of calls per iteration for quasi two to two\n";
  $temp = list_to_hash1(\@list_process,[split(/,/,$hash_var{"nb_call_two_to_two"})])->{$process};
  print ES "$temp \t\t nb of calls per iteration for true two to two\n";
  print ES "$hash_var{\"nb_try\"} \t\t nb of try for spring\n";
  $temp = list_to_hash1(\@list_process,[split(/,/,$hash_var{"accuracy"})])->{$process};
  print ES "$temp \t\t accuracy in per cent for Bases\n";
  print ES "$hash_var{\"choix_scale\"} \t\t choice of the scale:1 (pt3+pt4)*cm, 2 sqrt(pt3^2+pt4^2)*cm, 3 mgg*cm, 4 cm*pt_photon\n";
  print ES "$hash_var{\"cm_M\"} \t\t value of cm for initial state factorisation scale\n";
  print ES "$hash_var{\"cm_mu\"} \t\t value of cm for renormalisation scale\n";
  print ES "$hash_var{\"cm_MF\"} \t\t value of cm for final state factorisation scale\n";
  if ($process eq "dir") {
    print ES "TRUE \t\t to select the direct part\n";
    print ES "FALSE \t\t to select the one brem part\n";
  }
  elsif ($process eq "onef") {
    print ES "FALSE \t\t to select the direct part\n";
    print ES "TRUE \t\t to select the one brem part\n";
  }
  print ES "$hash_var{\"flag_iso\"} \t\t flag for isolation energy\n";
  @list_r_isol = split(/,/,$hash_var{"r_isol"});
  @list_et = split(/,/,$hash_var{"etmax"});
  if ($hash_var{"flag_iso"} == 4) {
    $i = 0;
    my ($r_isol,$r_isol_max,$epsilon,$n_power);
    foreach my $r (@list_r_isol) {
      $r_isol =$list_r_isol[$i];
      $r_isol =~ s/[Dd]//;
      $r_isol_max =$list_r_isol[0]; 
      $r_isol_max =~ s/[Dd]//;
      $epsilon =$list_et[0]; 
      $epsilon =~ s/[Dd]//; 
      $n_power =$list_et[1]; 
      $n_power =~ s/[Dd]//; 
      $val_etmax = $epsilon*( (1. - cos($r_isol)) / (1. - cos($r_isol_max)) )**($n_power);
      $i++;
      push(@list_etmax,$val_etmax);
    }
  }
  else {
    @list_etmax = @list_et;
  }
  #~ print ES "$hash_var{\"r_isol\"} \t\t value of isolation cone\n";
  #~ print ES "$hash_var{\"etmax\"} \t\t value of maximum Et deposited in the cone\n";
  $nb_cone = 9;
  my $tmp1 = $nb_cone+1;
  print ES "$tmp1 \t\t number of crowns + the inner cone\n";
  my $tmp2 = $#list_r_isol+1;
  print ES "$tmp2 \t\t effective number of crowns + the inner cone\n";
  for ($i=0;$i<=$nb_cone;$i++) {
    if ($i<=$#list_r_isol ) {
      print ES "$list_r_isol[$i] \t\t size of the different isolation cone\n";
    }
    else {
      print ES "0.d0 \t\t dummy value\n";
    }
  }
  for ($i=0;$i<=$nb_cone;$i++) {
    if ($i<=$#list_etmax ) {
      print ES "$list_etmax[$i] \t\t value of maximum Et deposited in the differentcone\n";
    }
    else {
      print ES "0.d0 \t\t dummy value\n";
    }
  }
  print ES "$hash_var{\"ifjet\"} \t\t 0 nojet(inclusive), 1 jet\n";
  print ES "'$hash_var{\"algorithm\"}' \t\t algorithm of the jet\n";
  print ES "$hash_var{\"rkt\"} \t\t value of r for kt algorithm\n";
  print ES "$hash_var{\"rc\"} \t\t value of r for cone algorithm\n";
  print ES "$hash_var{\"rsep\"} \t\t value of r sep\n";
  print ES "'$hash_var{\"merging\"}' \t\t merging rules\n";
  print ES "'$hash_var{\"acceptance\"}' \t\t acceptance of the events\n";
  print ES "$hash_var{\"ptjetmax\"} \t\t value of pt_jet_max\n";
  print ES "$hash_var{\"ptjetmin\"} \t\t value of pt_jet_min\n";
  print ES "$hash_var{\"yjetmax\"} \t\t value of y_jet_max\n";
  print ES "$hash_var{\"yjetmin\"} \t\t value of y_jet_min\n";
  close(ES);
  # creation du fichier executable
  print EALL "export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:$chemin_lhapdf/lib\n";
#  print EALL "export ROOTSYS=/LINUX/LAPPSL5/64bits/Root/pro\n";
#  print EALL "export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:\$ROOTSYS/lib\n";
  print EALL "./".$target.$name_bs.".exe < $name_dat.dat > $name_dir/jetphox.log\n";
  print EALL "rm $name_dat.dat\n";
  print EALL "rm -rf $dir_ff\n";
  if ( $npdfset>0 ){print EALL "rm -rf $dir_npdf \n";}
} 
close(EALL);
system("chmod +x $executable");
################################################################################
# cette routine prend deux listes @liste1 et @liste2 et en fait un
# tableau associatif avec @list1 comme cles et @list2 comme valeurs
# @list1 et @list2 doivent avoir le meme nombre d'elements
################################################################################
sub list_to_hash {
  my ($rl_list1,$rl_list2) = @_;
  my ($nb_element1,$nb_element2,$i,%hash_temp,$rh_hash_temp);
  $nb_element1 = @$rl_list1;
  $nb_element2 = @$rl_list2;
  if ($nb_element1 == $nb_element2) {
    for ($i=0;$i<$nb_element1;$i++) {
      $hash_temp{"$rl_list1->[$i]"} = $rl_list2->[$i];
    }
  }
  else {
    print "Erreur dans list_to_hash\n";
    print "les deux listes n'ont pas le memes nombres d'elements\n";
    print "nombre d'elements de la liste 1: $nb_element1\n";
    print "nombre d'elements de la liste 2: $nb_element2\n";
 }
  $rh_hash_temp = {%hash_temp};
  return($rh_hash_temp);
}
################################################################################
# cette routine est comme la precedente sauf que on n'exige pas
# que les deux listes aient le meme nombre d'elements
################################################################################
sub list_to_hash1 {
  my ($rl_list1,$rl_list2) = @_;
  my ($nb_element1,$nb_element2,$i,%hash_temp,$rh_hash_temp);
  $nb_element1 = @$rl_list1;
  $nb_element2 = @$rl_list2;
  if ($nb_element1 <= $nb_element2) {
    for ($i=0;$i<$nb_element1;$i++) {
      $hash_temp{"$rl_list1->[$i]"} = $rl_list2->[$i];
    }
  }
  elsif ($nb_element1 > $nb_element2) {
    for ($i=0;$i<$nb_element1;$i++) {
      if ($rl_list2->[$i]) {
        $hash_temp{"$rl_list1->[$i]"} = $rl_list2->[$i];
      }
      else {
        $rl_list2->[$i] = $rl_list2->[0];
        $hash_temp{"$rl_list1->[$i]"} = $rl_list2->[$i];
      }
    }
  }
  $rh_hash_temp = {%hash_temp};
  return($rh_hash_temp);
}
################################################################################
######################## partie pour histogramme ############################ 
################################################################################
# initialisation
$nom_fichier_root_init = "../../src/histo/perlmod/init_root.cxx";
$nom_fichier_root_end = "../../src/histo/perlmod/end_root.cxx";
$nom_fichier_head_init = "../../src/histo/perlmod/init_root.h";
$nom_fichier_head_end = "../../src/histo/perlmod/end_root.h";
$nom_fichier_histogrammation = "../../src/histo/perlmod/further_param.f";
$nom_fichier_root_remplissage = "../../src/histo/perlmod/remplie.cxx";
$nom_fichier_head_remplissage = "../../src/histo/perlmod/remplie.h";
#
$rl_temp = [split(/,/,$hash_var{"nb_generated"})];
$nb_event = $rl_temp->[0];
$ivar = 0;
$ihisto_equi = 0;
$ihisto_nonequi = 0;
$iscatter_equi = 0;
%hash_var_histo = ();
@list_var_histo_clef = qw/ cut_pt_photon ymax ymin algorithm rkt rcone rsep
 merging acceptance ptjetmax ptjetmin yjetmax yjetmin degree_or_rad/;
@list_titre_equi = ();
@list_titre_nonequi = ();
@list_variable_equi = ();
@list_variable_nonequi = ();
@list_order_equi = ();
@list_order_nonequi = ();
@list_nb_bin_equi = ();
@list_nb_bin_nonequi = ();
@list_ref_bin_value = ();
@list_xmin_equi = ();
@list_xmax_equi = ();
@list_ref_histo_equi = ();
%hash_variable = ( "pt_gamma" => ["pt3"],
		   "y_gamma" => ["y3"],
		   "pt_jet_leading" => ["ptjet_lead"],
		   "y_jet_leading" => ["yjet_lead"],
		   "pt_pair_gamma_jet" => ["qt_gamma_jet"],
		   "phi_gamma_jet" => ["fi_gamma_jet"],
		   "et_deposited" => ["edep"],
		   "pt_balance_gamma" => ["z_gamma_trig"],
		   "pt_balance_jet" => ["z_jet_trig"],
		   "x_observable_plus" => ["x_obs_plus"],
		   "x_observable_moins" => ["x_obs_moins"],
		   "x_leading_log_plus" => ["x_ll_plus"],
		   "x_leading_log_moins" => ["x_ll_moins"]);
$rh_variable = \%hash_variable;
$name_histo = $main_directory."/histo".$hash_var{"nom_histo"}.".outdat";
################################################################################
# on lit le fichier de donnees et on remplit le hashage %hash_var_histo
################################################################################
open (EREAD,$fichier_histo) || die "cannot open $fichier_histo";
open (EWRITE,">$name_histo") || die "cannot open $name_histo";
while(<EREAD>) {
  chomp;
# il faut absolument enlever les blancs en debut de ligne sinon l'ordre 
# unless ne joue pas son role
  s/^\s+//;
  unless (/^#/ || /^histo_equi/ || /^histo_nonequi/ || /^scatter_equi/) {  
# on enleve les blancs qui pourraient se trouver dans ou autour des 
# variables que l'on lit
    s/\s+//g; 
    $hash_var_histo{$list_var_histo_clef[$ivar]} = "$_";
    print EWRITE "$list_var_histo_clef[$ivar] $_\n";
    $ivar++;
  }
  if (/^histo_equi/) { 
# on enleve les blancs qui sont devant et derriere la chaine et l'on fait en 
# sorte qu'il n'y ait qu'un blanc entre les champs 
    s/\s$//;
    s/\s+/ /g;
    my ($temp_dummy,$temp_variable,$temp_order,$temp_cut,$temp_titre,$temp_nb_bin,
    $temp_xmin,$temp_xmax);
    print EWRITE "$_\n";
    ($temp_dummy,$temp_variable,$temp_order,$temp_cut,$temp_titre,
    $temp_nb_bin,$temp_xmin,$temp_xmax) = split(/ /,$_);
    unless (exists($hash_variable{$temp_variable})) {
      die "The variable $temp_variable does not exist\nPlease check the equidistant histogram variables\n";
    }
    push (@list_variable_equi,$temp_variable);
    push (@list_order_equi,$temp_order);
    push (@list_cut_equi,$temp_cut);
    push (@list_titre_equi,$temp_titre);
    push (@list_nb_bin_equi,$temp_nb_bin);
    push (@list_xmin_equi,$temp_xmin);
    push (@list_xmax_equi,$temp_xmax);
    $ihisto_equi++;
  }
  elsif (/^histo_nonequi/) { 
# on enleve les blancs qui sont devant et derriere la chaine et l'on fait en 
# sorte qu'il n'y ait qu'un blanc entre les champs 
    s/\s$//;
    s/\s+/ /g;
    my ($temp_dummy,$temp_variable,$temp_order,$temp_cut,$temp_titre,
    $temp_nb_bin,@temp_bin_value,$rl_temp);
    print EWRITE "$_\n";
    ($temp_dummy,$temp_variable,$temp_order,$temp_cut,$temp_titre,
    $temp_nb_bin,@temp_bin_value) = split(/ /,$_);
    unless (exists($hash_variable{$temp_variable})) {
      die "The variable $temp_variable does not exist\nPlease check the non equidistant histogram variables\n";
    }
    push (@list_variable_nonequi,$temp_variable);
    push (@list_order_nonequi,$temp_order);
    push (@list_cut_nonequi,$temp_cut);
    push (@list_titre_nonequi,$temp_titre);
    push (@list_nb_bin_nonequi,$temp_nb_bin);
    $rl_temp = \@temp_bin_value;
    push (@list_ref_bin_value,$rl_temp);
    $ihisto_nonequi++;
  }
  elsif (/^scatter_equi/) { 
# on enleve les blancs qui sont devant et derriere la chaine et l'on fait en 
# sorte qu'il n'y ait qu'un blanc entre les champs 
    s/\s$//;
    s/\s+/ /g;
    my ($temp_dummy,$temp_variablex,$temp_variabley,$temp_order,$temp_cut,$temp_titre,
    $temp_nb_binx,$temp_xmin,$temp_xmax,$temp_nb_biny,$temp_ymin,
    $temp_ymax);
    print EWRITE "$_\n";
    ($temp_dummy,$temp_variablex,$temp_variabley,$temp_order,$temp_cut,$temp_titre,
    $temp_nb_binx,$temp_xmin,$temp_xmax,$temp_nb_biny,$temp_ymin,
    $temp_ymax) = split(/ /,$_);
    unless (exists($hash_variable{$temp_variablex}) ||
     exists($hash_variable{$temp_variabley})) {
      die "One of the variable $temp_variablex or $temp_variabley does not exist\nPlease check the scatter-plot variables\n";
    }
    push (@list_scatter_variablex_equi,$temp_variablex);
    push (@list_scatter_variabley_equi,$temp_variabley);
    push (@list_scatter_order_equi,$temp_order);
    push (@list_scatter_cut_equi,$temp_cut);
    push (@list_scatter_titre_equi,$temp_titre);
    push (@list_scatter_nb_binx_equi,$temp_nb_binx);
    push (@list_scatter_xmin_equi,$temp_xmin);
    push (@list_scatter_xmax_equi,$temp_xmax);
    push (@list_scatter_nb_biny_equi,$temp_nb_biny);
    push (@list_scatter_ymin_equi,$temp_ymin);
    push (@list_scatter_ymax_equi,$temp_ymax);
    $iscatter_equi++;
  }
}
close (EWRITE);
close (EREAD);
################################################################################
# pour chaque histo avec bins equidistants, on donne:
# un entier
# un identificateur
# la variable qui remplit l'histogramme
# un flag pour savoir si on remplit l'histogramme avec les resultats Leading Log ou Next-to-Leading Log
# un tableau (en reference) pour les coupures specifiques a cet histogramme
# un titre
# le nombre de bin
# le maximum et minimum  de la variable
# ici on cree une reference a tous les objets du package Histo_equi
################################################################################
for ($ih=0;$ih < $ihisto_equi;$ih++) {
    my $r_histo = Histo_equi::new(eval($ih+1),eval(20+$ih),
		    $list_variable_equi[$ih],$list_order_equi[$ih],
		    $list_cut_equi[$ih],
		    $list_titre_equi[$ih],$list_nb_bin_equi[$ih],
		    $list_xmin_equi[$ih],$list_xmax_equi[$ih]);
    push (@list_ref_histo_equi,$r_histo);
}
################################################################################
# pour chaque histo avec bins non equidistants, on donne:
# un entier
# un identificateur
# la variable qui remplit l'histogramme
# un flag pour savoir si on remplit l'histogramme avec les resultats Leading Log ou Next-to-Leading Log
# un tableau (en reference) pour les coupures specifiques a cet histogramme
# un titre
# le nombre de bin
# un tableau donnant les valeurs inferieurs de chaque bin + la valeur superieure pour le dernier bin
# ici on cree une reference a tous les objets du package Histo_nonequi
################################################################################
for ($ih=0;$ih < $ihisto_nonequi;$ih++) {
    my $r_histo = Histo_nonequi::new(eval($ih+1),eval(40+$ih),
		    $list_variable_nonequi[$ih],$list_order_nonequi[$ih],
		    $list_cut_nonequi[$ih],
		    $list_titre_nonequi[$ih],$list_nb_bin_nonequi[$ih],
		    $list_ref_bin_value[$ih]);
    push (@list_ref_histo_nonequi,$r_histo);
}
################################################################################
# pour chaque scatterplot avec bins equidistants, on donne:
# un entier
# un identificateur
# les variables qui remplissent le scatterplot
# un flag pour savoir si on remplit le scatterplot avec les resultats Leading Log ou Next-to-Leading Log
# un tableau (en reference) pour les coupures specifiques a ce scatterplot
# un titre
# le nombre de bin pour la variable x
# le maximum et minimum  de la variable x
# le nombre de bin pour la variable y
# le maximum et minimum  de la variable y
# ici on cree une reference a tous les objets du package Scatter_equi
################################################################################
for ($ih=0;$ih < $iscatter_equi;$ih++) {
    my $r_scatter = Scatter_equi::new(eval($ih+1),eval(80+$ih),
		    $list_scatter_variablex_equi[$ih],
		    $list_scatter_variabley_equi[$ih],
		    $list_scatter_order_equi[$ih],
		    $list_scatter_cut_equi[$ih],
		    $list_scatter_titre_equi[$ih],
		    $list_scatter_nb_binx_equi[$ih],
		    $list_scatter_xmin_equi[$ih],
		    $list_scatter_xmax_equi[$ih],
		    $list_scatter_nb_biny_equi[$ih],
		    $list_scatter_ymin_equi[$ih],
		    $list_scatter_ymax_equi[$ih]);
    push (@list_ref_scatter_equi,$r_scatter);
}
################################################################################
# creation du fichier d'initialisation de root
################################################################################
open (INITCREATE,">$nom_fichier_root_init")|| 
die "cannot create  $nom_fichier_root_init";
open (INITCREATEH,">$nom_fichier_head_init")|| 
die "cannot create  $nom_fichier_head_init";
$initcreate = *INITCREATE;
#~ $initcreate = *INITCREATE;
print INITCREATE "#include \"TFile.h\"\n";
print INITCREATE "#include \"TH1.h\"\n";
print INITCREATE "#include \"TH2.h\"\n";
print INITCREATE "#include \"TProfile.h\"\n";
print INITCREATE "#include \"TNtuple.h\"\n";
print INITCREATE "#include \"TRandom.h\"\n";
print INITCREATE "\n";
print INITCREATE "TFile *hfile;\n";
foreach $r_histo (@list_ref_histo_equi) {
    $r_histo->Histo_equi::print_global($initcreate,"");
}
foreach $r_histo (@list_ref_histo_nonequi) {
    $r_histo->Histo_nonequi::print_global($initcreate,"");
}
print INITCREATE "\n";
print INITCREATE "void InitH1(char* path_rootfile)\n";
print INITCREATEH "void InitH1(char* path_rootfile)\n";
print INITCREATE "{\n";
#print INITCREATE "\tstring rt;\n";
#print INITCREATE "\trt = *path_rzfile + \".root\";\n";
if ($ihisto_nonequi != 0) {
  foreach $r_histo (@list_ref_histo_nonequi) {
    my(@temp_val1) = @{$r_histo->{"value_bin"}};     
    print INITCREATE "\tDouble_t xx".$r_histo->{"label"}."[".eval($#temp_val1+1)."] = {".join(",",@temp_val1)."};\n";
  }
}
print INITCREATE "\t// Create a new ROOT binary machine independent file.\n";
print INITCREATE "\t// Note that this file may contain any kind of ROOT objects, histograms,\n";
print INITCREATE "\t// pictures, graphics objects, detector geometries, tracks, events, etc..\n";
print INITCREATE "\t// This file is now becoming the current directory.\n";
print INITCREATE "\n";
print ($hash_var{"path_histo"},$hash_var{"nom_histo"},"\n");
print INITCREATE "\t// hfile = new TFile(\"".$hash_var{"path_histo"}."/ggd".$hash_var{"nom_histo"}.".root"."\",\"RECREATE\",\"\");\n";
print INITCREATE "\thfile = new TFile(path_rootfile,\"RECREATE\",\"\");\n";
print INITCREATE "\n";
print INITCREATE "  // Create some histograms \n";
foreach $r_histo (@list_ref_histo_equi) {
    $r_histo->Histo_equi::print_root($initcreate);
}
foreach $r_histo (@list_ref_histo_nonequi) {
    $r_histo->Histo_nonequi::print_root($initcreate);
}
print INITCREATE "  \n";
print INITCREATE "}\n";
close (INITCREATE);
close (INITCREATEH);
#
################################################################################
# creation du fichier de fermeture de root
################################################################################
#~ $nom_fichier_fortran_end = "test_end.cxx";
#~ @temp_val1 = ();
open (ENDCREATE,">$nom_fichier_root_end")|| 
die "cannot create  $nom_fichier_root_end";
open (ENDCREATEH,">$nom_fichier_head_end")|| 
die "cannot create  $nom_fichier_head_end";
$endcreate = *ENDCREATE;
print ENDCREATE "#include \"TFile.h\"\n";
print ENDCREATE "#include \"TH1.h\"\n";
print ENDCREATE "#include \"TH2.h\"\n";
print ENDCREATE "#include \"TProfile.h\"\n";
print ENDCREATE "#include \"TNtuple.h\"\n";
print ENDCREATE "#include \"TRandom.h\"\n";
print ENDCREATE "#include \"Riostream.h\"\n";
print ENDCREATE "\n";
print ENDCREATE "extern TFile *hfile;\n";
foreach $r_histo (@list_ref_histo_equi) {
    $r_histo->Histo_equi::print_global($endcreate,"extern");
}
foreach $r_histo (@list_ref_histo_nonequi) {
    $r_histo->Histo_nonequi::print_global($endcreate,"extern");
}
print ENDCREATE "\n";
print ENDCREATE "void EndH1(Int_t numb_event,Double_t first_int)\n";
print ENDCREATEH "void EndH1(Int_t numb_event,Double_t first_int)\n";
print ENDCREATE "{\n";
print ENDCREATE "\tconst Int_t nb_histo_equi = ".$ihisto_equi.";\n";
print ENDCREATE "\tconst Int_t nb_histo_nonequi = ".$ihisto_nonequi.";\n";
print ENDCREATE "\tInt_t i;\n";
print ENDCREATE "\tDouble_t bin_content,bin_error;\n";
#~ print ENDCREATE "\tTH1D hres;\n";
if ($ihisto_equi != 0) {
  print ENDCREATE "\tDouble_t bin_size[nb_histo_equi];\n";
  print ENDCREATE "\tDouble_t norma[nb_histo_equi];\n";
  foreach $r_histo (@list_ref_histo_equi) {
    push(@temp_val1,$r_histo->{"nb_bin"});     
    push(@temp_val2,$r_histo->{"min_bin"});     
    push(@temp_val3,$r_histo->{"max_bin"});     
  }
  print ENDCREATE "\tInt_t bin[nb_histo_equi] = {".join(",",@temp_val1)."};\n";
  print ENDCREATE "\tDouble_t xmin[nb_histo_equi] = {".join(",",@temp_val2)."};\n";
  print ENDCREATE "\tDouble_t xmax[nb_histo_equi] = {".join(",",@temp_val3)."};\n";
  print ENDCREATE "\tfor (i=0;i<nb_histo_equi;i++) {\n";
  print ENDCREATE "\t  bin_size[i] = (xmax[i]-xmin[i])/(Double_t)bin[i];\n";
  print ENDCREATE "\t  norma[i] = first_int/((Double_t)numb_event*bin_size[i]);\n";   
  print ENDCREATE "\t}\n";  
  foreach $r_histo (@list_ref_histo_equi) {
      $r_histo->Histo_equi::print_norma($endcreate);
  }
}
print ENDCREATE "\n";  
if ($ihisto_nonequi != 0) {
  print ENDCREATE "\tDouble_t xnorma;\n";
  print ENDCREATE "\tDouble_t xsize;\n";
  foreach $r_histo (@list_ref_histo_nonequi) {
    my(@temp_val) = @{$r_histo->{"value_bin"}};
    my $name_hist = "xx".$r_histo->{"label"};
    print ENDCREATE "\tDouble_t ".$name_hist."[".eval($#temp_val+1)."] = {".join(",",@temp_val)."};\n";
    $new_val = $#temp_val-1;
    print ENDCREATE "\tfor (i=0;i<=$new_val;i++) {\n";
    print ENDCREATE "\t\txsize = ".$name_hist."[i+1]-".$name_hist."[i];\n";   
    print ENDCREATE "\t\txnorma = first_int/((Double_t)numb_event*xsize);\n";   
    $r_histo->Histo_nonequi::print_norma($endcreate);
    print ENDCREATE "\t}\n";
    print ENDCREATE "\n";
  }
}
print ENDCREATE "\t// Save all objects in this file\n";
print ENDCREATE "\thfile->Write();\n";
print ENDCREATE "\t// Close the file. Note that this is automatically done when you leave\n";
print ENDCREATE "\t// the application.\n";
print ENDCREATE "\thfile->Close();\n";
print ENDCREATE "}\n";
close (ENDCREATE);
close (ENDCREATEH);
#
################################################################################
# creation du fichier de remplissage des histogrammes
################################################################################
#~ $nom_fichier_remplissage = "test_remplie.cxx";
open (REMPLISSAGE,">$nom_fichier_root_remplissage")|| 
die "cannot create  $nom_fichier_root_remplissage";
open (REMPLISSAGEH,">$nom_fichier_head_remplissage")|| 
die "cannot create  $nom_fichier_head_remplissage";
$remplissage = *REMPLISSAGE;
print REMPLISSAGE "#include \"TFile.h\"\n";
print REMPLISSAGE "#include \"TH1.h\"\n";
print REMPLISSAGEH "#include \"TH1.h\"\n";
print REMPLISSAGE "#include \"TH2.h\"\n";
print REMPLISSAGE "#include \"TProfile.h\"\n";
print REMPLISSAGE "#include \"TNtuple.h\"\n";
print REMPLISSAGE "#include \"TRandom.h\"\n";
print REMPLISSAGE "#include \"Riostream.h\"\n";
print REMPLISSAGE "\n";
#~ print REMPLISSAGE "extern TH1D *hpx;\n";
foreach $r_histo (@list_ref_histo_equi) {
    $r_histo->Histo_equi::print_global($remplissage,"extern");
}
foreach $r_histo (@list_ref_histo_nonequi) {
    $r_histo->Histo_nonequi::print_global($remplissage,"extern");
}
print REMPLISSAGE "\n";
print REMPLISSAGE "void remplie(Int_t iprov,Double_t pt3,Double_t y3,Double_t ptjet_lead,Double_t yjet_lead,\n";
print REMPLISSAGE "            Double_t fi_gamma_jet,Double_t qt_gamma_jet,Double_t edep,Double_t z_gamma_trig,\n";
print REMPLISSAGE "            Double_t z_jet_trig,Double_t x_obs_plus,Double_t x_obs_moins,\n";
print REMPLISSAGE "            Double_t x_ll_plus,Double_t x_ll_moins,Double_t weight)\n";
print REMPLISSAGEH "void remplie(Int_t iprov,Double_t pt3,Double_t y3,Double_t ptjet_lead,Double_t yjet_lead,\n";
print REMPLISSAGEH "            Double_t fi_gamma_jet,Double_t qt_gamma_jet,Double_t edep,Double_t z_gamma_trig,\n";
print REMPLISSAGEH "            Double_t z_jet_trig,Double_t x_obs_plus,Double_t x_obs_moins,\n";
print REMPLISSAGEH "            Double_t x_ll_plus,Double_t x_ll_moins,Double_t weight)\n";
print REMPLISSAGE "{\n";
$order = "lo";
print REMPLISSAGE "if (iprov ==11) {\n";
foreach $r_histo (@list_ref_histo_equi) {
    $r_histo->Histo_equi::print_remplissage($remplissage,$order,$rh_variable);
}
foreach $r_histo (@list_ref_histo_nonequi) {
    $r_histo->Histo_nonequi::print_remplissage($remplissage,$order,$rh_variable);
}
print REMPLISSAGE "}\n";
$order = "nlo";
foreach $r_histo (@list_ref_histo_equi) {
    $r_histo->Histo_equi::print_remplissage($remplissage,$order,$rh_variable);
}
foreach $r_histo (@list_ref_histo_nonequi) {
    $r_histo->Histo_nonequi::print_remplissage($remplissage,$order,$rh_variable);
}
print REMPLISSAGE "\n";
print REMPLISSAGE "}\n";
close (REMPLISSAGE);
close (REMPLISSAGEH);
#
################################################################################
# creation du fichier des parametres pour l'histogrammation
################################################################################
open (HISTO,">$nom_fichier_histogrammation")|| 
die "cannot create  $nom_fichier_histogrammation";
$histo = *HISTO;
print HISTO "\tsubroutine further_param(ptgamma_min,ymax,ymin,algorithm,\n";
print HISTO "     #\tr_kt,r_c,r_sep,merging,acceptance,ptjet_max,\n";
print HISTO "     #\tptjet_min,yjet_max,yjet_min)\n";
print HISTO "\timplicit real*8 (a-h,l-z)\n";
print HISTO "\tcharacter*2 algorithm,merging,acceptance\n";
print HISTO "\tptgamma_min = $hash_var_histo{\"cut_pt_photon\"}\n";
print HISTO "\tymax = $hash_var_histo{\"ymax\"}\n";
print HISTO "\tymin = $hash_var_histo{\"ymin\"}\n";
print HISTO "\talgorithm = \'$hash_var_histo{\"algorithm\"}\'\n";
print HISTO "\tmerging = \'$hash_var_histo{\"merging\"}\'\n";
print HISTO "\tacceptance = \'$hash_var_histo{\"acceptance\"}\'\n";
print HISTO "\tr_kt = $hash_var_histo{\"rkt\"}\n";
print HISTO "\tr_c = $hash_var_histo{\"rcone\"}\n";
print HISTO "\tr_sep = $hash_var_histo{\"rsep\"}\n";
print HISTO "\tptjet_max = $hash_var_histo{\"ptjetmax\"}\n";
print HISTO "\tptjet_min = $hash_var_histo{\"ptjetmin\"}\n";
print HISTO "\tyjet_max = $hash_var_histo{\"yjetmax\"}\n";
print HISTO "\tyjet_min = $hash_var_histo{\"yjetmin\"}\n";
print_return($histo);
close (HISTO);
#
#
sub print_return{
  my ($df) = @_;
  print $df "\treturn\n";
  print $df "\tend\n";
}
#
################################################################################
# on tourne le Makefile
################################################################################
system("make $target$name_bs");
#

################################################################################
# Definition du package Histo_nonequi
################################################################################
package Histo_nonequi;
#
@ISA = qw(Patron);
#
################################################################################
# Definition du package Histo_nonequi
################################################################################
package Histo_nonequi;
sub new {
  my ($i,$id,$variable,$order,$cut,$title,$nb_bin,$rl_bin_value) = @_;
  my $r_hash_histo = {
    "id" => $id,
    "title" => $title,
    "variable" => $variable,
    "order" => $order,
    "cut" => $cut,
    "nb_bin" => $nb_bin,
    "bin" => "xx".$i,
    "value_bin" => $rl_bin_value,
    "normalisation" => "norma[".eval($i-1)."]",
    "label" => $i
  };
  bless $r_hash_histo,'Histo_equi';
  return $r_hash_histo;
}
#
sub print_global {
  my ($r_histo,$df,$str) = @_;
  my ($temp_var) = "hp".$r_histo->{"id"};
  $str .= " " if ($str);
  print $df $str."TH1D *hp".$r_histo->{"id"}.";\n";
}
#
sub print_root {
  my ($r_histo,$df) = @_;
  my ($temp_var) = "hp".$r_histo->{"id"};
  print $df "\t$temp_var   = new TH1D(\"".$temp_var."\",\"".$r_histo->{"title"}."\",".
  $r_histo->{"nb_bin"}.",".$r_histo->{"bin"}.");\n";
}
sub print_norma {
  my ($r_histo,$df) = @_;
  print $df "\t\tbin_content = hp".$r_histo->{"id"}."->GetBinContent(i+1);\n";
  print $df "\t\tbin_error = hp".$r_histo->{"id"}."->GetBinError(i+1);\n";
  print $df "\t\tbin_content = bin_content*xnorma;\n";
  print $df "\t\tbin_error = bin_error*xnorma;\n";
  print $df "\t\thp".$r_histo->{"id"}."->SetBinContent(i+1,bin_content);\n";
  print $df "\t\thp".$r_histo->{"id"}."->SetBinError(i+1,bin_error);\n";
}
#
1;
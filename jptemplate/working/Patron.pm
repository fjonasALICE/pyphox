package Patron;
#
sub print_remplissage {
  my ($r_histo,$df,$order,$rh_variable) = @_;
  my ($rl_temp,$first_parenthese,$last_parenthese,$machin,$truc,$variable);
  #
  if ($r_histo->{"order"} eq $order) {
    $extra_cut =  $r_histo->{"cut"};
    $extra_cut =~ s/\s+//g;
    @temp_cut = split(//,$extra_cut);
    $first_car = shift(@temp_cut);
    $last_car = pop(@temp_cut);
    $remaining = join('',@temp_cut);
    if ( ($first_car eq '[') && ($last_car eq ']') ) {
      $opera = ' <=';
      $logi = '&&';
    }
    elsif ( ($first_car eq ']') && ($last_car eq '[') ) {
      $opera = ' >= ';
      $logi = '||';
    }
    else {
      exit(12);
    }
    eval('$rl_temp = ['.$remaining.']');
    $first_parenthese = "";
    $last_parenthese = "";
    if (scalar(@$rl_temp) > 3){
      $first_parenthese = "(";
      $last_parenthese = ")";
    }
    if ($rl_temp->[0]) {
      for ($i=1;$i<=scalar(@$rl_temp);$i=$i+3) {
        $machin = ($i==1)? "if  $first_parenthese":" && ";
        print $df "$machin";
        for ($j=1;$j<=scalar(@{$rh_variable->{$rl_temp->[$i]}});$j++) {
	  $truc = ($j == 1)? "":"  && ";
          print $df "$truc($rl_temp->[$i-1]"."$opera".
	  @{$rh_variable->{"$rl_temp->[$i]"}}[$j-1]
	  ." ".$logi." ".
	  @{$rh_variable->{"$rl_temp->[$i]"}}[$j-1]
	  ."$opera"."$rl_temp->[$i+1])\n";
	}
      }
      print $df "$last_parenthese {\n";
      foreach $variable (@{$rh_variable->{$r_histo->{"variable"}}}) {
        print $df "  hp".$r_histo->{"id"}."->Fill($variable,weight);\n";
      }
      print $df "}\n";
    }
    else {
      foreach $variable (@{$rh_variable->{$r_histo->{"variable"}}}) {
        print $df "  hp".$r_histo->{"id"}."->Fill($variable,weight);\n";
      }
    }
  }
}
#
1;
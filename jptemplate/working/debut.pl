#! /usr/bin/perl -w
#
open(EP,"parameter.indat") || die "cannot open parameter.indat" ;
open(ES,">parameter_temp.pl") || die "cannot create parameter_temp.pl" ;
while (<EP>) {
 chomp;
#  if (/^CHOIXPDF\t=/) {
#    s/CHOIXPDF\t=\s(\w+)/CHOIXPDF\t= $pdf/;
#  }
#  if (/^CHOIXHISTO\t=/) {
#    s/CHOIXHISTO\t=\s(\w+)/CHOIXHISTO\t= $histo/;
#  }
#  if (/^$target/) {
#    s/^$target(\w*):/$target$name_bs:/;
#  }
 s/\"/\\\"/g;
 print ES "print EWRITE \"$_\\n\";\n";
}
close(EP);
close(ES);

# --- parsing arguments
$esmf = "FALSE";
$tim = "FALSE";
$mpi = "FALSE";
$pun = "FALSE";
$f95 = "FALSE";
$omp = "FALSE";
$dos = "FALSE";
$unx = "FALSE";
$cry = "FALSE";
$sgi = "FALSE";
$imp = "FALSE";
$cvi = "FALSE";
#Casey 080108: Added these line.
$adc = "FALSE";
$nadc = "FALSE";
while ( $ARGV[0]=~/-.*/ )
   {
   if ($ARGV[0]=~/-esmf/) {$esmf="TRUE";shift;}
   if ($ARGV[0]=~/-timg/) {$tim="TRUE";shift;}
   if ($ARGV[0]=~/-mpi/) {$mpi="TRUE";shift;}
   if ($ARGV[0]=~/-pun/) {$pun="TRUE";shift;}
   if ($ARGV[0]=~/-f95/) {$f95="TRUE";shift;}
   if ($ARGV[0]=~/-omp/) {$omp="TRUE";shift;}
   if ($ARGV[0]=~/-dos/) {$dos="TRUE";shift;}
   if ($ARGV[0]=~/-unix/) {$unx="TRUE";shift;}
   if ($ARGV[0]=~/-cray/) {$cry="TRUE";shift;}
   if ($ARGV[0]=~/-sgi/) {$sgi="TRUE";shift;}
   if ($ARGV[0]=~/-impi/) {$imp="TRUE";shift;}
   if ($ARGV[0]=~/-cvis/) {$cvi="TRUE";shift;}
#Casey 080108: Added these lines.
   if ($ARGV[0]=~/-adcirc/) {$adc="TRUE";shift;}
   if ($ARGV[0]=~/-swan/) {$nadc="TRUE";shift;}
   }

# --- make a list of all files
@files = ();
foreach (@ARGV) {
   @files = (@files , glob );
}

# --- change each file if necessary
foreach $file (@files)
{
# --- set output file name
  if ($unx=~/TRUE/ && $omp=~/TRUE/)
  {
    ($tempf)=split(/.ftn/, $file);
    $ext = ($file =~ m/ftn90/) ? "F90" : "F";
    $outfile = join(".",$tempf,$ext);
  }
  elsif ($unx=~/TRUE/)
  {
    ($tempf)=split(/.ftn/, $file);
    $ext = ($file =~ m/ftn90/) ? "f90" : "f";
    $outfile = join(".",$tempf,$ext);
  }
  else
  {
    ($tempf)=split(/.ftn/, $file);
    $ext = ($file =~ m/ftn90/) ? "f90" : "for";
    $outfile = join(".",$tempf,$ext);
  }
# --- process file
  if (   (! -e $outfile)            #outfile doesn't exist
      || (-M $file < -M $outfile) ) #.ftn file recently modified
  {
    open file or die "can't open $file\n";
    open(OUTFILE,">".$outfile);
    while ($line=<file>)
    {
      $newline=$line;
      # ESMF must be processed first
      if ($esmf=~/TRUE/) {$newline=~s/^!ESMF//;}
      else               {$newline=~s/^!!ESMF//;} #second "!" is negation
      if ($tim=~/TRUE/) {$newline=~s/^!TIMG//;}
      if ($mpi=~/TRUE/) {$newline=~s/^!MPI//;}
      if ($pun=~/TRUE/) {$newline=~s/^!PUN//;}
      if ($f95=~/TRUE/) {$newline=~s/^!F95//;}
      if ($omp=~/TRUE/) {$newline=~s/^!OMP//;}
      if ($dos=~/TRUE/) {$newline=~s/^!DOS//;}
      if ($unx=~/TRUE/) {$newline=~s/^!UNIX//;}
      if ($cry=~/TRUE/) {$newline=~s/^!\/Cray//;}
      if ($sgi=~/TRUE/) {$newline=~s/^!\/SGI//;}
      if ($imp=~/TRUE/) {$newline=~s/^!\/impi//;}
      if ($cvi=~/TRUE/) {$newline=~s/^!CVIS//;}
#Casey 080108: Added these lines.
      if ($adc=~/TRUE/) {$newline=~s/^!ADC//;}
      if ($nadc=~/TRUE/) {$newline=~s/^!NADC//;}
      print OUTFILE $newline;
    }
    close file;
    close(OUTFILE);
  }
}

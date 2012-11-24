for A in *-?.gz; 
do 
   echo "processing " $A
   gunzip $A;
   Name=`basename $A .gz`
   ../dem_prolog1 $Name >$Name.pl
   gzip $Name.pl
   rm $Name
done

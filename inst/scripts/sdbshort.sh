#!/bin/sh
AWK=$(which gawk) || AWK=$(which awk)
$AWK -f sdbshort.awk ../extdata/dödsorsaker\ -\ data\ -\ antal\ döda\ -*.csv > ../extdata/sdb_doda_no.csv

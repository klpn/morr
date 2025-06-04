#!/bin/sh
AWK=$(which gawk) || AWK=$(which awk)
$AWK -f propyrs_sdb.awk -v ca1="$1" -v ca2="$2" ../extdata/sdb_doda_no.csv \
    | sort -n -t',' -k1,1 -k2,2 -k4,4

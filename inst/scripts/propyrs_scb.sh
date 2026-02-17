#!/bin/sh
AWK=$(which gawk) || AWK=$(which awk)
$AWK -f propyrs_scb.awk -v ca1="$1" -v ca2="$2" ../extdata/TAB961_sv_gt0.tsv \
    | sort -n -t',' -k1,1 -k2,2 -k4,4

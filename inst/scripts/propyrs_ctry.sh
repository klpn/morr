#!/bin/sh
AWK=$(which gawk) || AWK=$(which awk)
grep -h "^$1" ../extdata/Mort* ../extdata/pop | $AWK -v ca1="$2" -v ca2="$3" -v li="$4" \
        -f propyrs_ctry.awk | sort -n -t',' -k1,1 -k2,2 -k4,4

#!/bin/sh
grep -a -E '("|[1-9][0-9]*).$' ../extdata/TAB961_sv.csv |
    iconv -f windows-1252 -t utf-8 |
    sed -E -f scbshort.sed > ../extdata/TAB961_sv_gt0.tsv


BEGIN {
    FS = "\t"
    print "ctry,yr,li,age,sex,ca1,ca2,rat"
    li = "SCB"
    split(ca1, cas1, "!")
    split(ca2, cas2, "!")
}

$3~cas1[1] && (length(cas1[2])==0 || $3!~cas1[2]) {
    ca1n[$6,$1,$5,$4] += $7
    ca1n[$6,$1,$5,-1] += $7
}

$3~cas2[1] && (length(cas2[2])==0 || $3!~cas2[2]) {
    ca2n[$6,$1,$5,$4] += $7
    ca2n[$6,$1,$5,-1] += $7    
}

END {
    for (yrregsexage in ca1n) {
        split(yrregsexage, yrssep, SUBSEP)
        nom = ca1n[yrregsexage]
        denom = ca2n[yrregsexage]
        yr = yrssep[1]
        reg = yrssep[2]
        sex = yrssep[3]
        age = yrssep[4]
        if (age < 2)
            whoage = age + 2
        else
            whoage = age/5 + 6
        if (denom > 0) {
            rat = nom/denom
            printf("SCB%s,%d,%s,%d,%d,%d,%d,%f\n",
                   reg, yr, li, whoage, sex, nom, denom, rat)
        }
    }
}

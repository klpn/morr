BEGIN {
    FS = ","
    print "ctry,yr,li,age,sex,ca1,ca2,rat"
    li = "10SE"
    regn[0] = ""
    regn[1] = 110
    regn[3] = 121
    regn[4] = 122
    regn[5] = 123
    regn[6] = 211
    regn[7] = 212
    regn[8] = 213
    regn[9] = 214
    regn[10] = 221
    regn[12] = 224
    regn[13] = 231
    regn[14] = 232
    regn[17] = 311
    regn[18] = 124
    regn[19] = 125
    regn[20] = 312
    regn[21] = 313
    regn[22] = 321
    regn[23] = 322
    regn[24] = 331
    regn[25] = 332
}

$4~ca1 {
    ca1n[$2,$3,$5,$6] += $7
}

$4~ca2 {
    ca2n[$2,$3,$5,$6] += $7
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
        if (age == 99)
            whoage = 1
        else {
            if (age == 1)
                whoage = 2
            else
                whoage = age+5
        }
        nuts = regn[reg]
        if (denom > 0) {
            rat = nom/denom
            printf("SE%s,%d,%s,%d,%d,%d,%d,%f\n",
                   nuts, yr, li, whoage, sex, nom, denom, rat)
        }
    }
}

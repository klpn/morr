BEGIN {
        FS = ","
        print "ctry,yr,li,age,sex,ca1,ca2,rat"
	split(ca1, cas1, "!")
	split(ca2, cas2, "!")
}

function add_age(v) {
        for(i=1; i<=25; i++) {
                c = i+9
                v[$1,$4,i,$7] += $c
        }
}

$5~li && $6~cas1[1] && (length(cas1[2])==0 || $6!~cas1[2]){
        add_age(ca1n)
}

$5~li && $6~cas2[1] && (length(cas2[2])==0 || $6!~cas2[2]){
        add_age(ca2n)
}

END {
        for (cyragesex in ca1n) {
                split(cyragesex, yassep, SUBSEP)
                n1 = ca1n[cyragesex]
                n2 = ca2n[cyragesex]
                if (n2 > 0) {
                        rat = n1 / n2
			ctry = yassep[1]
                        yr = yassep[2]
                        age = yassep[3]
                        sex = yassep[4]
                        printf("%d,%d,%d,%d,%d,%d,%d,%.3f\n",
                               ctry, yr, li, age, sex, n1, n2, rat)
                }
        }
}

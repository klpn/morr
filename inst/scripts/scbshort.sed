s/\r//
s/",/\t/g
s/"//g
s/region/region\tregname/
s/ /\t/
s/(-[0-9]*|\+)? år\t/\t/g
s/män\t/1\t/
s/kvinnor\t/2\t/

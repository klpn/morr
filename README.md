# morr

This  R package can be used to analyze trends in cause-specific mortality
for different countries, using data from
[WHO Mortality Database](https://www.who.int/data/data-collection-tools/who-mortality-database),
which should be downloaded an and unzipped in  `extdata`
(`inst/extdata` when the package is developed), which can be done by
running `../scripts/download.sh` in that directory, provided that
[curl](https://curl.se/) and [unzip](https://infozip.sourceforge.net/UnZip.html)
are available.

## Usage examples

To plot cause of death pattern for Sweden, all ages.

```{.r}
capat <- c("ihd", "othhd", "othath", "othcirc", "str",
"neurdegnovd", "diab", "chresp", "inf", "othdis", "illdef", "ext", "tum")
capatplot(1, 4290, capat)
```

## Cause definitions

Causes are defined in the JSON file [conf/morr.json](inst/conf/morr.json),
similar to the configuration for my [Mortchartgen](https://github.com/klpn/Mortchartgen.jl)
and [MortIntl.jl](https://github.com/klpn/MortIntl.jl/) projects,
with regular expressions for each list format, mostly corresponding to a specific
ICD version. They will then be processed by the AWK script
[scripts/propyrs_ctry.awk](inst/scripts/propyrs_ctry.awk).

Cause expressions of the format `<ca>` will be recursively expanded to
the expressions for cause `ca` for the corresponding list. The AWK
script splits the expressions on `!` so that causes matching the
second part will be excluded, which can be useful for defining
residual categories.  For example, this expression for `othdis` on
ICD-8 matches any cause not included in the other causes in the
`capat` vector above.

```
"08A": "A[01]!<all>|<circ>|<neurdegnovd>|<diab>|<chresp>|<inf>|<illdef>|<ext>|<tum>"
```

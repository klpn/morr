#' @import jsonlite
#' @import dplyr
#' @import ggplot2
#' @import purrr

miconf <- fromJSON(system.file("conf", "morr.json", package = "morr"))
ctries <- fromJSON(system.file("conf", "countries.json", package = "morr"))
datapath <- system.file("extdata", package = "morr")
cafs <- list()

agelabs <- c("0–ω", "0", "1", "2", "3", "4",
             mapply(function(x) sprintf("%d–%d", (x-5)*5-5, (x-5)*5-1), 7:22),
             "85–89|ω", "90–94", "95–ω")
aw <- data.frame(age = 1:25, age_w = c(1, rep(2,7), rep(3,6), 4:14))
agelabs_w <- c(agelabs[1], "0–14", "15–44", agelabs[15:25])
sexlabs <- c("men", "women")

awjoin <- function(caframe) {
     inner_join(caframe, aw) |> group_by(yr, sex, age_w) |>
         summarise(ca1 = sum(ca1), ca2 = sum(ca2))
}

caframe <- function(ctry, ca1, ca2) {
    caf <- data.frame()
    oldwd <- getwd()
    setwd(system.file("scripts", package = "morr"))
    for (li in names(miconf[["causes"]][[ca1]][["causeexpr"]])) {
        le <- miconf[["listexpr"]][[li]]
        ca1e <- ce.expand(miconf[["causes"]][[ca1]][["causeexpr"]][[li]], li)
        ca2e <- ce.expand(miconf[["causes"]][[ca2]][["causeexpr"]][[li]], li)
        if (ctry=="all") {
            pipestr <- sprintf("./propyrs_ctry.sh \"[0-9]\" \"%s\" \"%s\"",
                               ca1e, ca2e, le)
        } else {
            pipestr <- sprintf("./propyrs_ctry.sh %s \"%s\" \"%s\" \"%s\"",
                               ctry, ca1e, ca2e, le)
        }
        caf <- bind_rows(caf, read.csv(pipe(pipestr)))
    }
    setwd(oldwd)
    caf
}

#' @export
ctry_caf <- function(ctry, ca1, ca2) {
    cacomb <- sprintf("%s-%s", ca1, ca2)
    caall <- sprintf("%s-all", cacomb)
    cafpath <- sprintf("%s/%s-%s.csv", datapath, cacomb, ctry)
    if (caall %in% names(cafs)) {
        cafs[[caall]] |> filter(ctry==!!ctry)
    } else if (file.exists(cafpath)) {
        read.csv(cafpath)
    } else {
        caf <- caframe(ctry, ca1, ca2)
        write.csv(caf, cafpath, quote = FALSE, row.names = FALSE)
        caf
    }
}

#' @export
ctry_awsyplot <- function(ctry, ca1, ca2, minyr, maxyr) {
    ctrylab <- ctries[[ctry]][["name"]]
    ca1lab <- miconf[["causes"]][[ca1]][["alias"]][["en"]]
    ca2lab <- miconf[["causes"]][[ca2]][["alias"]][["en"]]
    caframe <- ctry_caf(ctry, ca1, ca2) |>
        filter(sex < 9 & yr >= minyr & yr <= maxyr)
    awsyplot(caframe, ctrylab, ca1lab, ca2lab, "fixed")
}

alw <- function(aw) {
    if (length(unique(aw)) == 12) {
        c(agelabs_w[1:11], "85–ω")
    } else {
        agelabs_w
    }
}

awsyplot <- function(caframe, rlab, ca1lab, ca2lab, fwscales) {
    awjoin(caframe) |>
        ggplot(aes(x = yr, y = ca1/ca2, col = factor(sex, labels = sexlabs))) +
        geom_point() + geom_smooth(span = 0.3) +
        labs(col = "sex", x = "year", y = "ratio",
             title = sprintf("Deaths %s/%s %s", ca1lab, ca2lab, rlab)) +
        facet_wrap(scales = fwscales, ~factor(age_w, labels = alw(age_w))) +
        theme(text = element_text(size = 10))
}

#' @export
capatplot <- function(ag, ctry, cas) {
    ctrylab <- ctries[[as.character(ctry)]][["name"]]
    cas.list <- list()
    calabs <- c()
    for (cind in seq_along(cas)) {
        ca <- cas[cind]
        cas.list[[sprintf("%02d",cind)]] <- ctry_caf(ctry, ca, "all")
        calabs <- append(calabs, miconf[["causes"]][[ca]][["alias"]][["en"]])
    }
    cas.frame <- bind_rows(cas.list, .id="ca")
    cas.frame |> filter(age == ag & sex < 9) |>
        ggplot(aes(x = yr, y = ca1/ca2, fill = factor(ca, labels = calabs))) +
        geom_area(col="black", alpha=0.5) +
        labs(fill = "cause", x = "year", y = "ratio",
             title = sprintf("Causes of death %s age %s", ctrylab, agelabs[ag])) +
        facet_wrap(~factor(sex, labels = sexlabs))
}

ce.expand <- function(ce, li) {
    if (grepl("<[a-z]+>", ce)) {
        ca <- sub(".*<([a-z]+)>.*", "\\1", ce)
        cae <- miconf[["causes"]][[ca]][["causeexpr"]][[li]]
        ce <- ce.expand(gsub(sprintf("<%s>", ca), cae, ce), li)
    }
    ce
}

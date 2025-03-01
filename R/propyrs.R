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
aw <- data.frame(age = 2:25, age_w = c(rep(2,7), rep(3,6), 4:14))
aw0 <- data.frame(age = 2:25, age_w = 1)
agelabs_w <- c(agelabs[1], "0–14", "15–44", agelabs[15:25])
sexlabs <- c("men", "women")

awjoin <- function(caframe) {
    caf0 <- filter(caframe, age > 1) |>
        mutate(ca2 = coalesce(ca2, 0), age = if_else(ca2==0 & age>23, 23, age)) |>
        group_by(ctry, yr, sex, age) |>  summarise(ca1=sum(ca1), ca2=sum(ca2))
    bind_rows(inner_join(caf0, aw0), inner_join(caf0, aw)) |>
        group_by(yr, sex) |>
        mutate(length = if_else(age==23 & age_w==1 & max(age)==23, 3, 1)) |>
        group_by(yr, sex, age_w) |> summarise(meanrat = mean((ca1*length)/ca2),
                                     ca1 = sum(ca1), ca2 = sum(ca2), rat = ca1/ca2)
}

caframe <- function(ctry, ca1, ca2) {
    caf <- data.frame()
    oldwd <- getwd()
    setwd(system.file("scripts", package = "morr"))
    for (li in names(miconf[["causes"]][[ca1]][["causeexpr"]])) {
        le <- miconf[["listexpr"]][[li]]
        ca1e <- ce.expand(miconf[["causes"]][[ca1]][["causeexpr"]][[li]], li)
        if (ca2=="pop")
            ca2e <- ca2
        else
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
ctry_awsyplot <- function(ctry, ca1, ca2, minyr, maxyr, fwscales = "fixed", ycol = "rat") {
    ctrylab <- ctries[[ctry]][["name"]]
    ca1lab <- miconf[["causes"]][[ca1]][["alias"]][["en"]]
    ca2lab <- miconf[["causes"]][[ca2]][["alias"]][["en"]]
    caframe <- ctry_caf(ctry, ca1, ca2) |>
        filter(sex < 9 & yr >= minyr & yr <= maxyr)
    awsyplot(caframe, ctrylab, ca1lab, ca2lab, fwscales, ycol)
}

alw <- function(aw) {
    if (length(unique(aw)) == 12) {
        c(agelabs_w[1:11], "85–ω")
    } else {
        agelabs_w
    }
}

awsyplot <- function(caframe, rlab, ca1lab, ca2lab, fwscales, ycol) {
    awjoin(caframe) |>
        ggplot(aes(x = yr, y = !!sym(ycol), col = factor(sex, labels = sexlabs))) +
        geom_point() + geom_smooth(span = 0.3) +
        labs(col = "sex", x = "year", y = "ratio",
             title = sprintf("Deaths %s/%s %s", ca1lab, ca2lab, rlab)) +
        facet_wrap(scales = fwscales, ~factor(age_w, labels = alw(age_w))) +
        theme(text = element_text(size = 10))
}

#' @export
capatplot <- function(ag, ctry, cas, aw = FALSE, ca2 = "all") {
    ctrylab <- ctries[[as.character(ctry)]][["name"]]
    cas.list <- list()
    if (aw) {
        alabs <- agelabs_w
        agcol <- sym("age_w")
    } else {
        alabs <- agelabs
        agcol <- sym("age")
    }
    calabs <- c()
    ca2lab <- miconf[["causes"]][[ca2]][["alias"]][["en"]]
    for (cind in seq_along(cas)) {
        ca <- cas[cind]
        caf <- ctry_caf(ctry, ca, ca2)
        caf$rat <- caf$ca1 / caf$ca2
        if (aw) caf <- awjoin(caf)
        cas.list[[sprintf("%02d",cind)]] <- caf
        calabs <- append(calabs, miconf[["causes"]][[ca]][["alias"]][["en"]])
    }
    cas.frame <- bind_rows(cas.list, .id="ca")
    cas.frame |> filter(!!agcol == ag & sex < 9) |>
        ggplot(aes(x = yr, y = rat, fill = factor(ca, labels = calabs))) +
        geom_area(col="black", alpha=0.5) +
        labs(fill = "cause", x = "year", y = sprintf("deaths cause/%s", ca2lab),
             title = sprintf("Causes of death %s age %s", ctrylab, alabs[ag])) +
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

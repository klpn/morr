#' @import jsonlite
#' @import dplyr
#' @import ggplot2
#' @import purrr
#' @import tidyr

miconf <- fromJSON(system.file("conf", "morr.json", package = "morr"))
ctries <- fromJSON(system.file("conf", "countries.json", package = "morr"))
datapath <- system.file("extdata", package = "morr")
cafs <- list()

agelabs <- c("0–ω", "0", "1", "2", "3", "4",
             mapply(function(x) sprintf("%d–%d", (x-5)*5-5, (x-5)*5-1), 7:22),
             "85–89|ω", "90–94", "95–ω")
aw <- data.frame(age = 2:25, age_w = c(rep(2,7), rep(3,6), 4:14))
aw_m <- data.frame(age = 2:25, age_w = c(rep(2,11), rep(3,2), 4:14))
aw_o <- data.frame(age = 2:25, age_w = c(rep(2,13), 3:13))
aw_y <- data.frame(age = 2:25,
                   age_w = c(rep(2,7), rep(3,2), rep(4,2), rep(5,2), rep(6,4), rep(7,7)))
aw0 <- data.frame(age = 2:25, age_w = 1)
agelabs_w <- c(agelabs[1], "0–14", "15–44", agelabs[15:25])
agelabs_w_m <- c(agelabs[1], "0–34", "35–44", agelabs[15:25])
agelabs_w_o <- c(agelabs[1], "0–44", agelabs[15:25])
agelabs_w_y <- c(agelabs[1], "0–14", "15–24", "25–34", "35–44", "45–64", "65–ω")
sexlabs <- c("men", "women")

awjoin <- function(caframe, awframe) {
    caf0 <- filter(caframe, age > 1) |>
        mutate(ca2 = coalesce(ca2, 0),
               age = case_when(ca2==0 & age>23 ~ 23, ca2==0 & age<7 ~ 3, .default = age)) |>
        group_by(ctry, yr, sex, age) |>  summarise(ca1=sum(ca1), ca2=sum(ca2))
    bind_rows(inner_join(caf0, aw0), inner_join(caf0, awframe)) |>
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
        } else if (ctry=="SE") {
            if (grepl("SE", li))
                pipestr <- sprintf("./propyrs_sdb.sh \"%s\" \"%s\"", ca1e, ca2e)
            else
                pipestr <- NA
        } else {
            pipestr <- sprintf("./propyrs_ctry.sh %s \"%s\" \"%s\" \"%s\"",
                               ctry, ca1e, ca2e, le)
        }
        if (!is.na(pipestr))
            caf <- bind_rows(caf, read.csv(pipe(pipestr)))
    }
    setwd(oldwd)
    caf
}

#' Generate a data frame with cause of death ratios over time for one or more regions
#'
#' @description
#' If a CSV file with matching name exists in the extdata subdirectory, it will be used.
#' Otherwise, the result will be saved to such a file, which can be re-used to avoid
#' the relatively computing-intensive calculations with regular expressions on the
#' WHO data files.
#' 
#' @param ctry WHO region code (use "all" for all regions).
#' @param ca1 Numerator cause.
#' @param ca2 Denominator cause (use "pop" for population, to get death rates).
#' @examples
#' ctry_caf(4290, "ihd", "all")
#' @export
ctry_caf <- function(ctry, ca1, ca2) {
    cacomb <- sprintf("%s-%s", ca1, ca2)
    caall <- sprintf("%s-all", cacomb)
    if (grepl("SE", ctry))
        sctry <- "SE"
    else
        sctry <- ctry
    cafpath <- sprintf("%s/%s-%s.csv", datapath, cacomb, sctry)
    if (caall %in% names(cafs)) {
        cafs[[caall]] |> filter(grepl(!!ctry, ctry))
    } else if (file.exists(cafpath)) {
        read.csv(cafpath) |> filter(grepl(!!ctry, ctry))
    } else {
        caf <- caframe(sctry, ca1, ca2)
        write.csv(caf, cafpath, quote = FALSE, row.names = FALSE)
        caf |> filter(grepl(!!ctry, ctry))
    }
}

#' Plots cause of death ratios for different age groups in a region
#' 
#' @param ctry WHO region code (use "all" for all regions).
#' @param ca1 Numerator cause.
#' @param ca2 Denominator cause (use "pop" for population, to get death rates).
#' @param minyr Minimum year to be plotted.
#' @param maxyr Maximum year to be plotted.
#' @param fwscales Using free or fixed scales for the plot.
#' @param ycol Column to use for the plot. Use "meanrat" for mean of death rates over age groups.
#' @param aws Data frame with age groups for aggregation.
#'   The standard frame "aw" is suited for causes related to aging, with higher age groups fine-grained.
#'   The frame "aw_y" is suited for causes commong among young adults, like HIV and external causes.
#' @param alabs Vector with labels of the same length as the age groups.
#' @examples
#' ctry_awsyplot("4290", "ihd", "all", 1951, 2023)
#' @export
ctry_awsyplot <- function(ctry, ca1, ca2, minyr, maxyr,
                          fwscales = "fixed", ycol = "rat", aws = aw, alabs = agelabs_w) {
    ctrylab <- ctries[[ctry]][["name"]]
    ca1lab <- miconf[["causes"]][[ca1]][["alias"]][["en"]]
    ca2lab <- miconf[["causes"]][[ca2]][["alias"]][["en"]]
    caframe <- ctry_caf(ctry, ca1, ca2) |>
        filter(sex < 9 & yr >= minyr & yr <= maxyr)
    awsyplot(caframe, ctrylab, ca1lab, ca2lab, fwscales, ycol, aws, alabs)
}

alw <- function(aw) {
    if (length(unique(aw)) == 12) {
        c(agelabs_w[1:11], "85–ω")
    } else {
        agelabs_w
    }
}

awsyplot <- function(caframe, rlab, ca1lab, ca2lab, fwscales, ycol, aws, alabs) {
    awjoin(caframe, aws) |>
        ggplot(aes(x = yr, y = !!sym(ycol), col = factor(sex, labels = sexlabs))) +
        geom_point() + geom_smooth(span = 0.3) +
        labs(col = "sex", x = "year", y = "ratio",
             title = sprintf("Deaths %s/%s %s", ca1lab, ca2lab, rlab)) +
        facet_wrap(scales = fwscales, ~factor(age_w, labels = alabs)) +
        theme(text = element_text(size = 10))
}

#' Plots cause of death pattern for a region over time
#' 
#' @param ctry WHO region code (use "all" for all regions).
#' @param cas Vector of cause codes.
#' @param aws Data frame with age groups for aggregation.
#'   The standard frame "aw" is suited for causes related to aging, with higher age groups fine-grained.
#'   The frame "aw_y" is suited for causes commong among young adults, like HIV and external causes.
#' @param alabs Vector with labels of the same length as the age groups.
#' @param ca2 Denominator cause (use "pop" for population to get death rates).
#' @examples
#' capat <- c("ihd", "othhd", "othath", "othcirc", "str", "neurdegnovd", "diab",
#' "chresp", "inf", "othdis", "illdef", "ext", "tum")
#' capatplot(1, 4290, capat)
#' @export
capatplot <- function(ag, ctry, cas, aws = NA, alabs = agelabs, ca2 = "all") {
    ctrylab <- ctries[[as.character(ctry)]][["name"]]
    cas.list <- list()
    if (is.data.frame(aws)) {
        agcol <- sym("age_w")
    } else {
        agcol <- sym("age")
    }
    calabs <- c()
    ca2lab <- miconf[["causes"]][[ca2]][["alias"]][["en"]]
    for (cind in seq_along(cas)) {
        ca <- cas[cind]
        caf <- ctry_caf(ctry, ca, ca2)
        caf$rat <- caf$ca1 / caf$ca2
        if (is.data.frame(aws)) caf <- awjoin(caf, aws)
        cas.list[[sprintf("%02d",cind)]] <- caf
        calabs <- append(calabs, miconf[["causes"]][[ca]][["alias"]][["en"]])
    }
    cas.frame <- bind_rows(cas.list, .id="ca")
    cas.frame |> filter(!!agcol == ag & sex < 9) |>
        complete(ca, yr, age, sex, fill = list(ca1 = 0, ca2 = 1, rat = 0.0)) |>
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

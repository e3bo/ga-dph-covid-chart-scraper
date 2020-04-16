#!/usr/bin/env Rscript

library(tidyverse)
library(xml2)
library(V8)

ct <- v8()
addr <- "https://dph.georgia.gov/covid-19-daily-status-report"
dest <- "report.html"

utils::download.file(addr, destfile = dest)
gout <- system2("grep",
                args = c("pym.Parent", dest),
                stdout = TRUE)
ref_gout <-
  "    var pymParent = new pym.Parent('covid19dashdph', 'https://d20s4vd27d0hk0.cloudfront.net', {'title':'COVID-19 Daily Status Report'});"
stopifnot(gout == ref_gout)
dash_url <- str_split(gout, ",")[[1]][2] %>% str_remove_all(" |'")
dest2 <- "dash.html"
utils::download.file(dash_url, dest2)

dash_page <- dest2 %>% xml2::read_html()
txt <- xml_find_all(dash_page, ".//head/script[2]") %>% xml_text()
ct$eval(src = txt)

cgender <- ct$get("cgender.chart.data")
cage <- ct$get("cage.chart.data")
crace <- ct$get("crace.chart.data")
ccasecum <- ct$get("ccasecum.chart.data")
ccaseday <- ct$get("ccaseday.chart.data")
cdeathday <-
  ct$get("cdeathday.chart.data") ## NOTE: code says date is of sympom onset, but caption of cumulative chart in webpage says otherwise. We go with the caption.
cdeathcum <- ct$get("cdeathcum.chart.data")

# check that cumulative and daily counts are consistent
comp <- function(cum, dly) {
  dfc <- cum$dataPoints[[1]]
  dfd <- dly$dataPoints[[1]]
  dfd$y <- cumsum(dfd$y)
  stopifnot(isTRUE(all.equal(dfd, dfc)))
}

comp(cdeathcum, cdeathday)
comp(ccasecum, ccaseday)

# get report generation time
tstamp <- xml_child(dash_page, 2) %>% xml_child(2) %>% xml_text()
tag <- "Report Generated On :"
stopifnot(str_detect(tstamp, tag))
report_time <-
  str_remove(tstamp, tag) %>% lubridate::mdy_hms(tz = "EST")

# get the hash of the source file
md5 <-
  system2("md5sum", dest2, stdout = TRUE) %>% str_split(" ") %>% "[["(1) %>% "["(1)

# save the data of interest

left <-
  cdeathcum$dataPoints[[1]] %>% rename(cumulative_deaths = y) %>% select(-sid)
right <-
  ccasecum$dataPoints[[1]] %>% rename(cumulative_cases = y) %>% select(-sid)
full_join(left, right, by = "label") %>% mutate(date = lubridate::dmy(label)) %>%
  select(-label) %>% arrange(date) %>% select(date, cumulative_cases, cumulative_deaths) %>%
  add_column(dph_report_generation_time = report_time,
             dashbord_md5sum = md5) %>%
  write_csv(path = "GA-DPH-CanvasJS-data-cases-deaths.csv")


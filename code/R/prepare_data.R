# --- Header -------------------------------------------------------------------
# Prepares the "Explore Discretionary Accruals" display 
#
# (C) TRR 266 -  See LICENSE file for details 
# ------------------------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(modelr)
library(broom)
library(lubridate)
library(ExPanDaR)

mleadlag <- function(x, n, ts_id) {
  pos <- match(as.numeric(ts_id) + n, as.numeric(ts_id))
  x[pos]
}

winsorize <- function(df, drop = NULL, ...) {
  if(is.null(drop)) treat_outliers(df, ...)
  else {
    vars <- !(names(df) %in% drop)
    ret <- df
    ret[vars] <- treat_outliers(ret[vars], ...)
    return(ret)
  }
}

# --- Prepare base sample ------------------------------------------------------

ff12 <- read_csv(
  "data/external/fama_french_12_industries.csv", col_types = cols()
)

base_sample <- readRDS("data/pulled/cstat_global_fund.rds") %>%
  filter(
    indfmt == "INDL",
    fyr == 12,
    !is.na(at),
    at > 0,
    sale > 0
  ) %>%
  mutate(sic = ifelse(!is.na(sich), sprintf("%04d", sich), sic)) %>%
  left_join(ff12, by = "sic") %>%
  filter(!is.na(ff12_ind) & !ff12_ind %in%  c("Finance", "Other")) 


smp <- base_sample %>%
  group_by(gvkey) %>%
  mutate(
    dpo = 365*(0.5*(mleadlag(ap, -1, fyear) + ap)/cogs),
    dih = 365*(0.5*(mleadlag(invt, -1, fyear) + invt)/cogs),
    dso = 365*(0.5*(mleadlag(rect, -1, fyear) + rect)/sale),
    dwc = dih + dso - dpo,
    eqratio = ceq/at,
    cashratio = che/at
  ) %>% 
  group_by(curcd, fyear) %>%
  mutate(size = ntile(at, 10)) %>%
  ungroup() %>%
  filter(
    !is.na(dwc), ff12_ind != "Finance", 
    fyear >= 2000, fyear <= 2020,
    !is.na(eqratio), !is.na(cashratio)
  ) %>%
  group_by(loc, fyear) %>% filter(n() > 100) %>% ungroup() %>%
  select(
    gvkey, conm, loc, ff12_ind, fyear,
    dpo, dih, dso, dwc, size, eqratio, cashratio
  )

smp  %>%
  group_by(gvkey, fyear) %>%
  filter(n() > 1) -> dups

if(nrow(dups) > 0) stop(
  "Observations are not identified by gvkey and fyear."
)

smp_win <- winsorize(smp)
saveRDS(smp, "data/generated/opcycle_sample.rds")
saveRDS(smp_win, "data/generated/opcycle_sample_win.rds")

# Use the below to interactively explore the data

if (FALSE) {
  conf <- readRDS("data/external/expand_config.rds")
  # This might take a while to start up because of the large sample
  ExPanD(
    smp, cs_id = c("gvkey", "conm"), ts_id = "fyear",
    config_list = conf,
    title = "Operating Cycle of Publicly-Listed Firms"
  )
}



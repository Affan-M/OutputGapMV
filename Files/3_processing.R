#--- Processing file ---#

# Packages
library(tidyverse) # Data manipulation + Plotting library (GGplot)
library(plyr) # Rounding function
library(scales) # Plotting addition
library(gridExtra) # Plotting addition
library(patchwork) # Plotting addition
library(data.table) # Data manipulation
library(glue) # String manipulation
library(zoo) # Date manipulation
library(plotly) # Interactive chart package
library(readxl) # Read excel files
library(openxlsx) # Write excel files
library(fpp2) # Forecast package
library(seasonal) # Some additional seasonal adjustment methods
library(mFilter) # HP filter and Bandpass filter
library(strucchange) # Bai-Perron test
library(tseries) # Provides Stationarity testing (KPSS)
library(urca) # Provides Stationarity testing (ADF)
library(openxlsx) # Write to xlsx package
library(RJDemetra) # JDemetra for seasonal adjustment

# Sourcing preceding scripts
source("1_bnf_methods.R")
source("2_outputgap_functions.R")
source("5_theme.R")

#### Data Import ####
files <- list.files(path = "input_QNA/") %>% .[!str_detect(., pattern = "^~\\$")]

Revision_files <- list()

for (f in files) {
  
  glue("input_QNA/{f}") %>%
    read_excel(., sheet = "Table 1") %>%
    as.data.table() %>%
    .[6,3:ncol(.)] %>%
    as.numeric() %>%
    ts(., start = c(2003, 1), frequency = 4) -> x
  
  data.table("Period" = time(x) %>% as.yearqtr(),
             "Value" = as.numeric(x),
             "Vintage" = str_replace(f, ".xlsx", "")) %>%
    .[Value > 0] -> Revision_files[[f]]
  
}

rbindlist(Revision_files) -> Revision_dt

# Tourism GVA
glue("input_QNA/{files[length(files)]}") %>%
  read_excel(., sheet = "Table 1") %>%
  as.data.table() %>%
  .[18,3:ncol(.)] %>%
  as.numeric() %>%
  .[!is.na(.)] %>%
  ts(., start = c(2003, 1), frequency = 4) -> x

data.table("Period" = time(x) %>% as.yearqtr(),
           "Variable" = "Tourism",
           "Value" = as.numeric(x)) -> Tourism_dt


#### Required Initial Analysis ####
# Unique vintages
Revision_dt[, Vintage] %>% unique() -> vintage

# Most recent vintage
Revision_dt[, Vintage] %>% unique() %>% .[length(.)] -> v
Revision_dt[Vintage == v] -> QNA_dt

# QNA time series
QNA_dt[, Value] %>%
  ts(., start = c(2003, 1), frequency = 4) -> QNA_ts

# Seasonally adjusted, logged, multiplied by 100
x13(QNA_ts, spec = "RSA1") -> x13_model
x13_model[["final"]][["series"]][, "sa"] %>%
  log()*100 -> QNA_sa

# Growth
data.table("Period" = time(QNA_sa) %>% as.yearqtr(),
           "QNA" = QNA_sa) %>%
  .[, "QNA" := as.numeric(QNA)] %>%
  .[, "GrowthPrec" := (exp((QNA-lag(QNA, n=1))/100)-1)*100] %>%
  .[, "GrowthCorr" := (exp((QNA-lag(QNA, n=4))/100)-1)*100] -> Growth.QNA

Growth.QNA[, c("Period", "GrowthPrec", "GrowthCorr")] %>%
  melt(., id.var = "Period", variable.name = "Variable", value.name = "Value") %>%
  .[!is.na(Value)] -> Growth.QNA.g

# Unadjusted corresponding growth rate vs Adjusted corresponding growth rate
copy(QNA_dt) %>%
  .[, "UnadjCorr" := (Value/lag(Value, n=4)-1)*100] %>%
  merge(., Growth.QNA[, c("Period", "GrowthCorr")], by = "Period") %>%
  .[, "Difference" := (UnadjCorr-GrowthCorr)] -> Corr_test_dt


# Combining QNA and Tourism
copy(QNA_dt) %>% .[, !"Vintage"] %>%
  .[, "Variable" := "QGDP"] %>%
  rbind(., Tourism_dt) -> QNA_Tou_dt


#### Mean Absolute Revision ####
# Calculating corresponding quarter growth rates for all periods and vintages
growth_list <- list()

for (v in vintage) {
  
  p2 <- as.yearqtr(v)
  p1 <- as.yearqtr(v)-1
  
  Revision_dt[Period %in% c(p2, p1)] -> Tb
  
  Tb[, .("Growth" = (Value/lag(Value)-1)*100), by = "Vintage"] %>%
    .[!is.na(Growth)] %>%
    .[, `:=` ("Period" = p2,
              "Estimate_number" = 1:nrow(.))] %>%
    .[, c("Period", "Vintage", "Growth", "Estimate_number")] -> growth_list[[v]]
  
}

rbindlist(growth_list) -> Growth_Tb

# Calculating the revision triangle (as NBS does it) for each estimate
Growth_Tb[, Estimate_number] %>% unique() %>% .[-1] -> estimate_number

revision_list <- list()

for (i in estimate_number) {
  
  Growth_Tb[Estimate_number %in% c((i-1), i)] %>%
    .[, .("Revision" = Growth - lag(Growth)), by = "Period"] %>%
    .[!is.na(Revision)] %>%
    .[, "Estimate_number" := glue("{i} vs. {i-1}")] -> revision_list[[i]]
  
}

rbindlist(revision_list) %>%
  .[, .("MAR" = mean(abs(Revision))), by = "Estimate_number"] -> MAR

# Full revision table
order <- MAR[order(word(Estimate_number, 1) %>% as.numeric()), Estimate_number] %>%
  as.character()

rbindlist(revision_list) %>%
  dcast(., Period ~ Estimate_number, value.var = "Revision") %>%
  .[, c("Period", ..order)] -> Rev_table

# Adding means to full revision table
Rev_table[, "Period" := as.character(Period)]
copy(Rev_table) %>% .[-(2:nrow(.))] %>%
  melt(., id.var = "Period", variable.name = "Variable", value.name = "Value") -> dt
dt[, "Period" := "MAR"]

dt[, Variable] %>% unique() %>% as.character() -> series

for (i in series) {
  
  Rev_table[, ..i] %>%
    as_vector() %>%
    as.numeric() %>%
    abs() %>%
    mean(., na.rm = TRUE) -> mean_rev
  
  dt[Variable == i, "Value" := mean_rev]
  
}

dt %>%
  dcast(., Period ~ Variable, value.var = "Value") %>%
  rbind(Rev_table, .) -> Rev_table_F

#### Trend-Cycle ####
trend_vintage_list <- list()
cycle_vintage_list <- list()

for (v in vintage) {
  
  print(v)
  
  # Selecting data and extracting time series
  Revision_dt[Vintage == v, Value] %>%
    ts(., start = c(2003, 1), frequency = 4) -> data_ts
  
  # Running the trend_cycle function
  tc_list <- trend_cycle(tseries = data_ts)
  
  # Extracting the trend and cycle data tables from list
  Trend.dt <- tc_list[["trend"]]
  Cycle.dt <- tc_list[["cycle"]]
  
  # Attaching vintage name for each
  Trend.dt %>%
    melt(., id.var = "Period", variable.name = "Method", value.name = "Value") %>%
    .[, "Vintage" := v] %>%
    .[, "Value" := as.numeric(Value)] %>%
    .[!is.na(Value)] -> Trend.dtx
  
  Cycle.dt %>%
    melt(., id.var = "Period", variable.name = "Method", value.name = "Value") %>%
    .[, "Vintage" := v] %>%
    .[, "Value" := as.numeric(Value)] %>%
    .[!is.na(Value)] -> Cycle.dtx
  
  # Sending to final list
  trend_vintage_list[[v]] <- Trend.dtx
  cycle_vintage_list[[v]] <- Cycle.dtx
  
}

trend_final <- rbindlist(trend_vintage_list)
cycle_final <- rbindlist(cycle_vintage_list)

#### Revision Analysis ####
rev_dt <- revision_analysis(dt = cycle_final, est = 6)

rev_dt %>%
  melt(., id.var = c("Vintage", "Method"), variable.name = "Measure", value.name = "Value") %>%
  .[, .("Value" = mean(Value)), by = c("Method", "Measure")] %>%
  dcast(., Method ~ Measure, value.var = "Value") %>%
  melt(., id.var = "Method", variable.name = "Measure", value.name = "Value") -> rev_dtx

# rev_dtx %>% dcast(Method ~ Measure, value.var = "Value") %>% View()

# rev_dt2 <- revision_analysis2(cycle_final, 6)


#### Recession Dates Table ####
for (i in 1:nrow(Growth.QNA)) {
  qtr_t <- 0
  qtr_tp1 <- 0
  qtr_tm1 <- 0
  
  if (i == 1) {
    Growth.QNA[i,GrowthPrec] -> qtr_t
    Growth.QNA[(i+1),GrowthPrec] -> qtr_tp1
  } else if (i == nrow(Growth.QNA)) {
    Growth.QNA[(i-1),GrowthPrec] -> qtr_tm1
    Growth.QNA[i,GrowthPrec] -> qtr_t
  } else {
    Growth.QNA[(i-1),GrowthPrec] -> qtr_tm1
    Growth.QNA[i,GrowthPrec] -> qtr_t
    Growth.QNA[(i+1),GrowthPrec] -> qtr_tp1
  }
  
  if ((qtr_t < 0 & qtr_tp1 < 0) | (qtr_tm1 < 0 & qtr_t < 0)) {
    Growth.QNA[i, "Recession" := 1]
  }
}

Growth.QNA[is.na(Recession), "Recession" := 0]

# Creating a recession dates table
Growth.QNA[Recession == 1, Period] %>% unique() -> dates

copy(Growth.QNA) %>%
  .[, "Period_end" := Period] %>%
  setnames(., old = "Period", new = "Period_start") %>%
  .[1, c("Period_start","Period_end")] -> Recession_Dates_Tb
Recession_Dates_Tb[1,] <- NA
Recession_Dates_Tb[1, "Period_start" := dates[1]]

i = 2
rn <- 1

while (i < length(dates)) {
  check_date_prev <- Recession_Dates_Tb[rn, Period_start]
  check_date <- dates[i]
  
  while ((check_date_prev+1/4) == check_date) {
    check_date_prev <- check_date
    i <- i + 1
    check_date <- dates[i]
  }
  
  Recession_Dates_Tb[rn, "Period_end" := dates[i-1]]
  rn <- rn + 1
  
  copy(Recession_Dates_Tb) %>% .[1,] %>%
    .[, `:=` ("Period_start" = check_date)] %>%
    rbind(Recession_Dates_Tb, .) -> Recession_Dates_Tb
  
  i <- i+1
  
}

Recession_Dates_Tb[nrow(Recession_Dates_Tb), "Period_end" := dates[length(dates)]]

#### Confidence Bands ####
window <- 40
bnf_final <- bnf(y = QNA_sa, demean = "dm", wind = window)
bnf_final[["cycle_se"]] -> se

t <- 1.960
UB_95 <- (bnf_final[["cycle"]] + t * se)
LB_95 <- (bnf_final[["cycle"]] - t * se)

t <- 1.282
UB_80 <- (bnf_final[["cycle"]] + t * se)
LB_80 <- (bnf_final[["cycle"]] - t * se)

data.table("Period" = time(bnf_final[["cycle"]]) %>% as.yearqtr(),
           "Cycle" = bnf_final[["cycle"]],
           "UB_95" = UB_95,
           "LB_95" = LB_95,
           "UB_80" = UB_80,
           "LB_80" = LB_80) %>%
  setnames(., old = colnames(.),
           new = c("Period", "Cycle", "UB_95", "LB_95", "UB_80", "LB_80")) -> BNF_full


#### GDP correlation with filter estimates ####

dcast(cycle_final[Vintage == v], Period ~ Method, value.var = "Value") -> Tb1

copy(Growth.QNA) %>%
  .[, c("Period", "QNA", "GrowthPrec")] %>%
  merge(., Tb1, by = "Period") %>%
  .[, !"Period"] %>%
  cor(use = "pairwise.complete.obs") -> CorrTb

CorrTb %>%
  as.data.table(., keep.rownames = "Method") %>%
  .[!(Method %in% c("QNA", "GrowthPrec")), c("Method", "GrowthPrec")] %>%
  .[, "GrowthPrec" := GrowthPrec * 100] %>%
  setnames(., old = "GrowthPrec", new = "Correlation with GDP Growth (Preceding)") -> Tb

Tb[str_detect(Method, "BNF1|BP Short|HP|Piecewise") & !str_detect(Method, "HPAlt")] %>%
  .[order(-`Correlation with GDP Growth (Preceding)`)] -> Corr_Tb_f

recode(Corr_Tb_f$Method,
       "BNF1" = "BN Filter: 40-window Demeaning",
       "BNF2" = "BN Filter: Sample Mean Demeaning",
       "BNF3" = "BN Filter: Manual Break Dates",
       "BP Long" = "BP Filter: 6-32 quarters",
       "BP Short" = "BP Filter: 4-24 quarters",
       "HP" = "HP Filter",
       "Linear" = "Linear Model",
       "Quadratic" = "Quadratic Model",
       "Piecewise" = "Piecewise Model") -> Corr_Tb_f$Method


#### Final filter ####
BNF1_final <- merge(Growth.QNA[, c("Period", "QNA")],
      cycle_final[Vintage == v & Method == "BNF1", c("Period", "Method", "Value")] %>%
        dcast(Period ~ Method, value.var = "Value"), by = "Period") %>%
  setnames(., old = "BNF1", new = "Cycle") %>%
  .[, "Trend" := QNA - Cycle]

BNF1_final_m <- BNF1_final %>%
  melt(., id.var = "Period", variable.name = "Variable", value.name = "Value")

BNF1_final_m[Variable != "Cycle", "Panel" := 1]
BNF1_final_m[Variable == "Cycle", "Panel" := 2]

BNF1_final_m[, "Variable" := factor(Variable,
                                    levels = c("QNA", "Trend", "Cycle"))]


#### Generate Excel File ####
list("QNA unadj" = QNA_dt, "QNA SA" = Growth.QNA,
     "Methods-Latest vintage" = dcast(cycle_final[Vintage == v], Period ~ Method, value.var = "Value"),
     "BNF Final" = merge(BNF1_final[, !"Cycle"], BNF_full, by = "Period")) %>%
  openxlsx::write.xlsx(., file = "OutputGap.xlsx")

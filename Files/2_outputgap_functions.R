#--- Functions used for Output Gap ----#
# options(digits = 15)

#### Function to easily create and merge data tables from tslm/lm objects ####
trend_dt <- function(tslm, modelname, merge_dt) {
  x <- tslm$fitted.values
  time <- merge_dt[, Period] %>% max()
  
  data.table("Period" = x %>% ts(., end = c(year(time),quarter(time)), frequency = 4) %>% time() %>% as.yearqtr(),
             "Value" = x) %>%
    setnames(., old = "Value", new = modelname) -> dt
  
  merge(merge_dt, dt, by = "Period", all = TRUE) -> dt
  return(dt)
}

cycle_dt <- function(tslm, modelname, merge_dt) {
  x <- tslm$residuals
  time <- merge_dt[, Period] %>% max()
  
  data.table("Period" = x %>% ts(., end = c(year(time),quarter(time)), frequency = 4) %>% time() %>% as.yearqtr(),
             "Value" = x) %>%
    setnames(., old = "Value", new = modelname) -> dt
  
  merge(merge_dt, dt, by = "Period", all = TRUE) -> dt
  return(dt)
}

#### Function to create and merge data tables from mFilter objects ####
# Used for HP Filter, BP Filters and BNF
trend_dt_f <- function(filter_obj, modelname, merge_dt) {
  x <- filter_obj[["trend"]]
  
  data.table("Period" = x %>% time() %>% as.yearqtr(),
             "Value" = x) %>%
    setnames(., old = "Value.Series 1", new = modelname) -> dt
  
  merge(merge_dt, dt, by = "Period", all = TRUE) -> dt
  return(dt)
}

cycle_dt_f <- function(filter_obj, modelname, merge_dt) {
  filter_class <- class(filter_obj)
  x <- filter_obj[["cycle"]]
  
  if (filter_class == "mFilter") {
    filter_title <- filter_obj[["title"]]
    
    if (str_detect(filter_title, "Hodrick-Prescott")) {
      
      data.table("Period" = x %>% time() %>% as.yearqtr(),
                 "Value" = x) %>%
        setnames(., old = "Value", new = modelname) -> dt
      
    } else if (str_detect(filter_title, "Chiristiano-Fitzgerald")) {
      
      data.table("Period" = x %>% time() %>% as.yearqtr(),
                 x) %>%
        setnames(., old = "Series 1", new = modelname) -> dt
      
    }
  } else if (filter_class == "bnf") {
    
    data.table("Period" = x %>% time() %>% as.yearqtr(),
               x) %>%
      setnames(., old = "Cycle", new = modelname) -> dt
    
  }
  
  merge(merge_dt, dt, by = "Period", all = TRUE) -> dt
  return(dt)
}

#### Function to do the seasonal adjustment and all the trend-cycle methods ####
trend_cycle <- function(tseries) {
  
  #### Data processing ####
  ## Seasonal adjustment and logging
  x13_model <- x13(tseries, spec = "RSA1")
  data <- x13_model[["final"]][["series"]][, "sa"]
  QNA <- log(data)*100
  
  ## Dataframe preparation
  data.table("Period" = time(QNA) %>% as.yearqtr(),
             "QNA" = QNA) -> QNA.dt
  
  # Adding in Trend variable
  QNA.dt[, "trend" := 1:nrow(QNA.dt)]
  
  # Creating dataframes to put in trend and cycle variables from different models.
  Trend.dt <- data.table("Period" = time(QNA) %>% as.yearqtr())
  Cycle.dt <- data.table("Period" = time(QNA) %>% as.yearqtr())
  
  #### Modelling ####
  Model_list <- list()
  Filter_list <- list()
  
  #### Linear model ####
  ### Intercept + Trend
  lm1 <- tslm(QNA ~ trend)
  Model_list[["Linear"]] <- lm1
  
  #### Quadratic model ####
  ### Intercept + trend + trend^2
  qm1 <- tslm(QNA ~ trend + I(trend^2))
  Model_list[["Quadratic"]] <- qm1
  
  #### Piecewise linear trend model ####
  # Breakpoints - based on autoplots
  d1 <- "2004 Q4" %>% as.yearqtr() # -> Tsunami
  d2 <- "2008 Q4" %>% as.yearqtr() # -> Financial crisis
  d3 <- "2020 Q1" %>% as.yearqtr() # -> COVID-19 pandemic
  
  # Adding in dummy variables to the dt
  QNA.dt[, "d1" := ifelse(Period >= d1, 1, 0)]
  QNA.dt[, "d2" := ifelse(Period >= d2, 1, 0)]
  QNA.dt[, "d3" := ifelse(Period >= d3, 1, 0)]
  
  if (length(tseries) < 69) {
    
    pw1 <- lm(formula = QNA ~ trend + d1 + d2, data = QNA.dt)
    
  } else {
    
    pw1 <- lm(formula = QNA ~ trend + d1 + d2 + d3, data = QNA.dt)
    
  }
  
  Model_list[["Piecewise"]] <- pw1
  
  #### HP Filter ####
  # HP 1
  hp1 <- hpfilter(QNA, type = "lambda", freq = 1600, drift = FALSE)
  Filter_list[["HP"]] <- hp1
  
  #### HP Filter alternative ####
  copy(QNA.dt) %>%
    .[, c("Period", "QNA")] %>%
    .[, "QNA" := as.numeric(QNA)] %>%
    .[, `:=` (t_8 = lag(QNA, 8),
              t_9 = lag(QNA, 9),
              t_10 = lag(QNA, 10),
              t_11 = lag(QNA, 11))] %>%
    .[!is.na(t_11)] -> hpalt_df
  
  time <- hpalt_df[1, Period]
  
  hpalt_df[, `:=` ("QNA" = ts(QNA, start = c(year(time), quarter(time)), frequency = 4),
                   "t_8" = ts(t_8, start = c(year(time), quarter(time)), frequency = 4),
                   "t_9" = ts(t_9, start = c(year(time), quarter(time)), frequency = 4),
                   "t_10" = ts(t_10, start = c(year(time), quarter(time)), frequency = 4),
                   "t_11" = ts(t_11, start = c(year(time), quarter(time)), frequency = 4))]
  
  hpalt <- lm(formula = QNA ~ t_8 + t_9 + t_10 + t_11, data = hpalt_df)
  
  Model_list[["HPAlt"]] <- hpalt
  
  #### Bandpass Filter ####
  # Using Christiano-Fitzgerald approximation for Bandpass filter
  cf1 <- cffilter(QNA, type = "asymmetric", pl = 6, pu = 32, root = TRUE, drift = TRUE) # Long
  cf2 <- cffilter(QNA, type = "asymmetric", pl = 4, pu = 24, root = TRUE, drift = TRUE) # Short
  
  Filter_list[["BP Long"]] <- cf1
  Filter_list[["BP Short"]] <- cf2
  
  # tslm(formula = QNA ~ trend) %>%
  #   .[["residuals"]] %>%
  #   cffilter(., type = "asymmetric", pl = 6, pu = 32, root = FALSE, drift = FALSE)
  
  
  #### BN decomposition ####
  # Automatically determined delta and rolling mean with window = 40 demeaning method
  # Rolling mean used to avoid the issue of structural breaks
  window <- 40
  bnf1 <- bnf(y = QNA, demean = "dm", wind = window)
  
  # Automatically determined delta and full sample mean demeaning method
  bnf2 <- bnf(y = QNA)   # use default 'p', 'd0' and 'dt' values and "sm" demean
  
  # Automatically determined delta and piecewise mean with 'breaks' = c(8, 24, 69)
  # demeaning method. 8, 24 and 69 based on dummies specified above.
  
  # If function to specify breaks. Need to provide 2 quarters of additional data for
  # bnf to run without any issues. Although the break occurred at 69th observation,
  # have to provide it with 71 observations for it to run.
  
  if (length(tseries) < 71) {
    
    bnf3 <- bnf(y = QNA, demean = "pm", breaks = c(8, 24)) # if 'breaks' empty does 'sm'
    
  } else {
    
    bnf3 <- bnf(y = QNA, demean = "pm", breaks = c(8, 24, 69)) # if 'breaks' empty does 'sm'
    
  }
  
  Filter_list[["BNF1"]] <- bnf1
  Filter_list[["BNF2"]] <- bnf2
  Filter_list[["BNF3"]] <- bnf3
  
  #### Extracting the trend and cycle components ####
  # By removing the trend component for tslm/lm models and getting the cycle
  # object for others
  for (i in names(Model_list)) {
    Model_list[[i]] %>% trend_dt(., i, Trend.dt) -> Trend.dt
    Model_list[[i]] %>% cycle_dt(., i, Cycle.dt) -> Cycle.dt
  }
  
  # Extracting the assigned trend and cycle components from the filter objects
  for (i in names(Filter_list)) {
    if (!str_detect(i, "BNF")) {
      Filter_list[[i]] %>% trend_dt_f(., i, Trend.dt) -> Trend.dt
      Filter_list[[i]] %>% cycle_dt_f(., i, Cycle.dt) -> Cycle.dt
    } else {
      Filter_list[[i]] %>% cycle_dt_f(., i, Cycle.dt) -> Cycle.dt
    }
  }
  
  final_list <- list("trend" = Trend.dt, "cycle" = Cycle.dt)
  
  return(final_list)
  
}

#### Function to do the revision analysis ####
revision_analysis <- function(dt, est) {
  
  # dt = cycle
  # est = estimate number to compare to
  
  dt[, Method] %>% unique() %>% as.character() -> method
  
  # All vintages for which a fourth estimate exists
  dt[, Vintage] %>% unique() %>% .[1:(length(.)-(est-1))] -> vintage
  
  final_list <- list()
  
  for (m in method) {
    dt[Method == m] -> dt_x
    
    # Calculation of Measures
    calc_measures <- list()
    
    for (v in vintage) {
      
      expost_vint <- (as.yearqtr(v)+((est-1)/4)) %>% as.character()
      
      dt_x[Vintage == v] -> realtime
      dt_x[Vintage == expost_vint & Period <= v] -> expost
      
      rbind(realtime, expost) %>%
        dcast(., Period ~ Vintage, value.var = "Value") %>%
        setnames(., old = c(v, expost_vint), new = c("realtime", "expost")) %>%
        .[!is.na(realtime) & !is.na(expost)] %>%
        .[, "Revision" := (expost - realtime)] -> rev_dt
      
      # Correlation
      rev_dt[, c("realtime", "expost")] %>%
        cor(.) %>%
        .[1,2] * 100 -> Correlation
      
      # Same sign
      copy(rev_dt) %>%
        .[, "Samesign" := ifelse(sign(realtime) == sign(expost), 1, 0)] %>%
        .[, Samesign] -> ss_vec
      
      (sum(ss_vec)/length(ss_vec)*100) -> SameSign
      
      # Standard deviation
      rev_dt[, Revision] %>% sd() -> StDev
      
      # RMSE
      rev_dt[, Revision]^2 %>% mean() %>% sqrt() -> RMSE
      
      data.table("Vintage" = v,
                 "Method" = m,
                 "Correlation" = Correlation,
                 "Same Sign" = SameSign,
                 "Standard Deviation" = StDev,
                 "Root Mean Square" = RMSE) -> calc_dt
      
      calc_dt -> calc_measures[[v]]
      
    }
    
    rbindlist(calc_measures) -> final_list[[m]]
    
  }
  
  rbindlist(final_list) -> final_dt
  
  return(final_dt)
  
}


# revision_analysis2 <- function(dt, est) {
#   
#   # dt = cycle
#   # est = estimate number to compare to
#   
#   dt[, Method] %>% unique() %>% as.character() -> method
#   
#   # All vintages for which a fourth estimate exists
#   dt[, Vintage] %>% unique() %>% .[1:(length(.)-(est-1))] -> vintage
#   
#   final_list <- list()
#   
#   for (m in method) {
#     dt[Method == m] -> dt_x
#     
#     # Calculation of Measures
#     revision_list <- list()
#     
#     for (v in vintage) {
#       
#       expost_vint <- (as.yearqtr(v)+((est-1)/4)) %>% as.character()
#       
#       dt_x[Vintage == v] -> realtime
#       dt_x[Vintage == expost_vint & Period <= v] -> expost
#       
#       rbind(realtime, expost) %>%
#         dcast(., Period ~ Vintage, value.var = "Value") %>%
#         setnames(., old = c(v, expost_vint), new = c("realtime", "expost")) %>%
#         .[!is.na(realtime) & !is.na(expost)] %>%
#         .[, "Revision" := (expost - realtime)] -> rev_dt
#       
#       rev_dt -> revision_list[[v]]
#       
#     }
#     
#     full_rev_dt <- rbindlist(revision_list)
#     
#     # Correlation
#     full_rev_dt[, c("realtime", "expost")] %>%
#       cor(.) %>%
#       .[1,2] * 100 -> Correlation
#     
#     # Same sign
#     copy(full_rev_dt) %>%
#       .[, "Samesign" := ifelse(sign(realtime) == sign(expost), 1, 0)] %>%
#       .[, Samesign] -> ss_vec
#     
#     (sum(ss_vec)/length(ss_vec)*100) -> SameSign
#     
#     # Standard deviation
#     full_rev_dt[, Revision] %>% sd() -> StDev
#     
#     # RMSE
#     full_rev_dt[, Revision]^2 %>% mean() %>% sqrt() -> RMSE
#     
#     data.table("Vintage" = v,
#                "Method" = m,
#                "Correlation" = Correlation,
#                "Same Sign" = SameSign,
#                "Standard Deviation" = StDev,
#                "Root Mean Square" = RMSE) -> calc_dt
#     
#     calc_dt -> final_list[[m]]
#     
#   }
#   
#   rbindlist(final_list) -> final_dt
#   
#   return(final_dt)
#   
# }

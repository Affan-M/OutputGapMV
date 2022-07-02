# Tests

source("processing.R")

#### Test: Correlation Output gap (t) & Output Growth (t+h) ####
# Function to test the correlation between cycle today and output growth in the future
merge(cycle_final, Growth.QNA[, c("Period", "GrowthPrec")], by = "Period") -> Combined_dt

Ct_Of <- function(dt,h) {
  
  data.table("Vintage" = as.character(),
             "Method" = as.character(),
             "Coefficient" = as.numeric(),
             "Correlation" = as.numeric(),
             "Only_h_corr" = as.numeric()) -> dtx
  
  dt[, Vintage] %>% unique() %>% .[1:(length(.)-h)] -> vintage
  dt[, Method] %>% unique() %>% as.character() -> method
  
  for (i in 1:length(vintage)) {
    for (m in method) {
      
      v1 <- vintage[i]
      v2 <- (as.yearqtr(v1)+h/4) %>% as.character()
      
      dt[Vintage == v1 & Method == m, c("Period", "Value")] -> Tb1
      dt[Vintage == v2, c("Period", "GrowthPrec")] %>% unique() -> Tb2
      
      merge(Tb2, Tb1, by = "Period", all = TRUE) -> Tb
      
      # Regression
      lm(formula = GrowthPrec ~ lag(Value, h), data = Tb) -> Regression
      Regression[["coefficients"]] %>% .[2] %>% as.numeric() -> coef
      
      # Correlation
      copy(Tb) %>%
        .[, "Val_minus_h" := lag(Value, h)] %>%
        .[!is.na(Val_minus_h), c("Val_minus_h", "GrowthPrec")] -> Tb3
      
      Tb3 %>% cor() %>% .[1,2] -> corr
      Tb3 %>% tail(., n=h) %>% cor() %>% .[1,2] -> corr2
      
      
      # Adding to data table
      data.table("Vintage" = v,
                 "Method" = m,
                 "Coefficient" = coef,
                 "Correlation" = corr,
                 "Only_h_corr" = corr2) %>%
        rbind(dtx, .) -> dtx
    }
  }
  
  return(dtx)
}

Combined_Correlation_List <- list()

for (i in 2:8) {
  
  Ct_Of(Combined_dt, i) -> Tb_f
  
  Tb_f[, .("Coeff" = mean(Coefficient),
           "Corr" = mean(Correlation),
           "Corr_h" = mean(Only_h_corr)), by = c("Method")] %>%
    .[, "h" := i] -> Combined_Correlation_List[[i]]
  
}

rbindlist(Combined_Correlation_List) -> Combined_Correlation_Dt

# Charts
ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_line(data = Combined_Correlation_Dt[Method %in% c("BNF1", "BP Long", "HP")],
            aes(x = h, y = Corr, color = Method)) +
  labs(title = "Correlation - Whole Series")

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_line(data = Combined_Correlation_Dt[Method %in% c("BNF1", "BP Long", "HP")],
            aes(x = h, y = Corr_h, color = Method)) +
  labs(title = "Correlation - Only h quarters")

#### Test: Correlation Output gap (t) & Output (t+h) - Log Approximation ####
# Function to test the correlation between cycle today and output in the future
merge(cycle_final, Growth.QNA[, c("Period", "QNA")], by = "Period") -> Combined_dt

Ct_Of <- function(dt,h) {
  
  data.table("Vintage" = as.character(),
             "Method" = as.character(),
             "Coefficient" = as.numeric(),
             "Correlation" = as.numeric(),
             "Only_h_corr" = as.numeric()) -> dtx
  
  dt[, Vintage] %>% unique() %>% .[1:(length(.)-h)] -> vintage
  dt[, Method] %>% unique() %>% as.character() -> method
  
  for (i in 1:length(vintage)) {
    for (m in method) {
      
      v1 <- vintage[i]
      v2 <- (as.yearqtr(v1)+h/4) %>% as.character()
      
      dt[Method == m] -> Tbx
      
      Tbx[Vintage == v1] %>% .[, c("Period", "Value")] -> Tb1
      Tbx[Vintage == v2] %>% .[, "Growth" := (QNA-lag(QNA,n=h))] %>%
        .[, c("Period", "Growth")] -> Tb2
      
      merge(Tb2, Tb1, by = "Period", all = TRUE) -> Tb
      
      # Regression
      lm(formula = Growth ~ lag(Value, h), data = Tb) -> Regression
      Regression[["coefficients"]] %>% .[2] %>% as.numeric() -> coef
      
      # Correlation
      copy(Tb) %>%
        .[, "Val_minus_h" := lag(Value, h)] %>%
        .[!is.na(Val_minus_h), c("Val_minus_h", "Growth")] -> Tb3
      
      Tb3 %>% cor() %>% .[1,2] -> corr
      Tb3 %>% tail(., n=h) %>% cor() %>% .[1,2] -> corr2
      
      
      # Adding to data table
      data.table("Vintage" = v1,
                 "Method" = m,
                 "Coefficient" = coef,
                 "Correlation" = corr,
                 "Only_h_corr" = corr2) %>%
        rbind(dtx, .) -> dtx
    }
  }
  
  return(dtx)
}

Combined_Correlation_List <- list()

for (i in 1:8) {
  
  Ct_Of(Combined_dt, i) -> Tb_f
  
  Tb_f[, .("Coeff" = mean(Coefficient),
           "Corr" = mean(Correlation),
           "Corr_h" = mean(Only_h_corr)), by = c("Method")] %>%
    .[, "h" := i] -> Combined_Correlation_List[[i]]
  
  # Tb_f2 <- Tb_f[, "h" := i]
  # Combined_Correlation_List[[i]] <- Tb_f2
  
}

rbindlist(Combined_Correlation_List) -> Combined_Correlation_Dt

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_line(data = Combined_Correlation_Dt,
            aes(x = h, y = Corr, color = Method)) +
  labs(title = "Correlation - Whole Series")

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_line(data = Combined_Correlation_Dt,
            aes(x = h, y = Corr_h, color = Method)) +
  labs(title = "Correlation - Only h quarters")


#### Test: Correlation Output gap (t) & Output (t+h) - Exact Growth ####
# Function to test the correlation between cycle today and output in the future
merge(cycle_final, Growth.QNA[, c("Period", "QNA")], by = "Period") -> Combined_dt

Ct_Of <- function(dt,h) {
  
  data.table("Vintage" = as.character(),
             "Method" = as.character(),
             "Coefficient" = as.numeric(),
             "Correlation" = as.numeric(),
             "Only_h_corr" = as.numeric()) -> dtx
  
  dt[, Vintage] %>% unique() %>% .[1:(length(.)-h)] -> vintage
  dt[, Method] %>% unique() %>% as.character() -> method
  
  for (i in 1:length(vintage)) {
    for (m in method) {
      
      v1 <- vintage[i]
      v2 <- (as.yearqtr(v1)+h/4) %>% as.character()
      
      dt[Method == m] -> Tbx
      
      Tbx[Vintage == v1] %>% .[, c("Period", "Value")] -> Tb1
      Tbx[Vintage == v2] %>% .[, "Growth" := (exp((QNA-lag(QNA, n=h))/100)-1)*100] %>%
        .[, c("Period", "Growth")] -> Tb2
      
      merge(Tb2, Tb1, by = "Period", all = TRUE) -> Tb
      
      # Regression
      lm(formula = Growth ~ lag(Value, h), data = Tb) -> Regression
      Regression[["coefficients"]] %>% .[2] %>% as.numeric() -> coef
      
      # Correlation
      copy(Tb) %>%
        .[, "Val_minus_h" := lag(Value, h)] %>%
        .[!is.na(Val_minus_h), c("Val_minus_h", "Growth")] -> Tb3
      
      Tb3 %>% cor() %>% .[1,2] -> corr
      Tb3 %>% tail(., n=h) %>% cor() %>% .[1,2] -> corr2
      
      
      # Adding to data table
      data.table("Vintage" = v1,
                 "Method" = m,
                 "Coefficient" = coef,
                 "Correlation" = corr,
                 "Only_h_corr" = corr2) %>%
        rbind(dtx, .) -> dtx
    }
  }
  
  return(dtx)
}

Combined_Correlation_List <- list()

for (i in 1:8) {
  
  Ct_Of(Combined_dt, i) -> Tb_f
  
  Tb_f[, .("Coeff" = mean(Coefficient),
           "Corr" = mean(Correlation),
           "Corr_h" = mean(Only_h_corr)), by = c("Method")] %>%
    .[, "h" := i] -> Combined_Correlation_List[[i]]
  
  # Tb_f2 <- Tb_f[, "h" := i]
  # Combined_Correlation_List[[i]] <- Tb_f2
  
}

rbindlist(Combined_Correlation_List) -> Combined_Correlation_Dt

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_line(data = Combined_Correlation_Dt,
            aes(x = h, y = Corr, color = Method)) +
  labs(title = "Correlation - Whole Series")

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_line(data = Combined_Correlation_Dt,
            aes(x = h, y = Corr_h, color = Method)) +
  labs(title = "Correlation - Only h quarters")


#### Test: Potential Output ####
(QNA_sa + bnf_final[["cycle"]]) %>%
  autoplot(., series = "Potential") +
  autolayer(QNA_sa, series = "QNA")


cycle_final[Method == "Linear" & Vintage == "2020 Q1", Value] %>%
  ts(., end = c(2020, 1), frequency = 4) -> t1

(t1 + QNA_sa) %>%
  autoplot() +
  autolayer(QNA_sa, series = "QNA")


cycle_final[Method == "HP" & Vintage == "2020 Q1", Value] %>%
  ts(., end = c(2020,1), frequency = 4) -> t1

(t1 + QNA_sa) %>%
  autoplot() +
  autolayer(QNA_sa, series = "QNA")

#### Test: End Sample effect ####
cycle_final[Method == "HP"] -> Tb

ggplot() +
  geom_line(data = Tb[!Vintage == "2020 Q1"],
            aes(x = as.Date.yearqtr(Period), y = Value, color = Vintage)) +
  geom_line(data = Tb[Vintage == "2020 Q1"],
            aes(x = as.Date.yearqtr(Period), y = Value, color = Vintage),
            color = "black", alpha = 0.7)

ggplotly(p)


cycle_final[Method == "BNF1"] %>%
  ggplot() +
  geom_line(aes(x = as.Date.yearqtr(Period), y = Value, color = Vintage))


#### Test: Turning point analysis ####

# QNA + Output gap
max_limit <- max(plyr::round_any(max(cycle_final[,Value]), f = ceiling, accuracy = 10),
                 plyr::round_any(max(Growth.QNA.g[,Value]), f = ceiling, accuracy = 10))

min_limit <- max(plyr::round_any(min(cycle_final[,Value]), f = floor, accuracy = 10),
                 plyr::round_any(min(Growth.QNA.g[,Value]), f = floor, accuracy = 10))

ggplot() +
  geom_rect(data = Recession_Dates_Tb,
            aes(xmin = as.Date.yearqtr(Period_start), xmax = as.Date.yearqtr(Period_end),
                ymin = min_limit, ymax = max_limit), alpha = 0.3) +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_line(data = cycle_final[Vintage == "2021 Q2" & Method %in% c("BNF1", "HP", "BP Short", "Piecewise")],
            aes(x = as.Date.yearqtr(Period), y = Value, color = Method)) +
  geom_line(data = Growth.QNA.g,
            aes(x = as.Date.yearqtr(Period), y = Value, color = Variable),
            size = 0.5) +
  theme_classic() -> p

ggplotly(p)

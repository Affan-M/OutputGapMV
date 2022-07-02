#--- Charts ----#

# Sourcing preceding scripts
source("3_processing.R")
# source("5_theme.R")

#### QNA + TA ####

# Secondary axis transformation
temp <- QNA_Tou_dt[Variable == "Tourism"]$Value
translation_factor <- min(temp) + diff(range(temp)) / 100000
sec_axis_trns <- function(x, inverse = FALSE) {
  if (!inverse) {
    return((x - translation_factor) / 0.25)
  } else {
    return(x * 0.25 + translation_factor)
  }
}

ggplot() +
  geom_line(data = QNA_Tou_dt[Variable == "QGDP"],
            aes(x = as.Date.yearmon(Period), y = Value, color = Variable),
            size = 0.5) +
  geom_line(data = QNA_Tou_dt[Variable == "Tourism"],
            aes(x = as.Date.yearmon(Period), y = sec_axis_trns(Value), color = Variable),
            size = 0.5) +
  scale_y_continuous(sec.axis = sec_axis(~sec_axis_trns(., inverse = TRUE),
                                         breaks = pretty_breaks(n=10),
                                         labels = comma),
                     breaks = pretty_breaks(n=10),
                     labels = comma) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_color_manual(values = cols,
                     labels = c("QGDP" = "QGDP (Left axis)",
                                "Tourism" = "Tourism (Right axis)")) +
  theme_mma() +
  labs(title = "\n",
       subtitle = " ",
       caption = "") -> p


ggdraw(p) +
  draw_text("Figure 1: Quarterly GDP (QGDP) vs. gross value added (GVA) by\ntourism, 2003 - 2020",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(millions of MVR)", 
            x = 0.01, y = 0.89, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color) +
  draw_text("Source: Maldives Bureau of Statistics", 
            x = 0.01, y = 0.03, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/QNA_Tou.pdf", width = .width, height = .height, units = "in")

#### QNA ####

# # Seasonally unadjusted
# ggplot() +
#   geom_line(data = QNA_dt,
#             aes(x = as.Date.yearqtr(Period), y = Value),
#             size = 0.5, color = cols[1]) +
#   scale_y_continuous(breaks = pretty_breaks(n=10),
#                      labels = comma) +
#   scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
#   theme_mma() +
#   labs(title = "\n",
#        subtitle = " ",
#        caption = "\n") -> p
# 
# ggdraw(p) +
#   draw_text("Figure x: Quarterly National Accounts of Maldives, 2003 - 2020",
#             x = 0.01, y = 0.98, hjust = 0, vjust = 1,
#             size = font_title_size,
#             family = font_h,
#             color = headings_color,
#             lineheight = 0.85) +
#   draw_text("(millions of MVR)", 
#             x = 0.01, y = 0.93, hjust = 0, vjust = 1,
#             size = font_sub_size,
#             family = font_sub,
#             color = headings_color) +
#   draw_text("Source: Maldives Bureau of Statistics", 
#             x = 0.01, y = 0.03, hjust = 0, vjust = 1,
#             size = font_sub_size,
#             family = font_sub,
#             color = headings_color)
# 
# ggsave("charts/QNA_nsa.pdf", width = .width, height = .height, units = "in")

#### STL ####
QNA_ts %>%
  stl(., s.window = "period") %>%
  autoplot() -> p

p +
  scale_y_continuous(breaks = pretty_breaks(n=5),
                     labels = comma) +
  theme_mma() +
  labs(title = "\n \n",
       subtitle = "") -> p

ggdraw(p) +
  draw_text("Figure 2: Time series properties of QNA using STL\ndecomposition",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(millions of MVR)", 
            x = 0.01, y = 0.89, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/STL.pdf", width = .width, height = .height+0.75, units = "in")

#### ACF + PACF ####
p1 <- ggAcf(QNA_ts) +
  scale_y_continuous(breaks = pretty_breaks(n=5),
                     labels = comma) +
  ggtitle("\n\n\n Autocorrelation function (ACF)") +
  theme_mma() +
  theme(plot.title = element_text(size = font_title_size-2, family = font_h, color = headings_color))

p2 <- ggPacf(QNA_ts) +
  scale_y_continuous(breaks = pretty_breaks(n=5),
                     labels = comma) +
  ggtitle("\n Partial autocorrelation function (PACF)") +
  theme_mma() +
  theme(plot.title = element_text(size = font_title_size-2, family = font_h, color = headings_color))

p <- grid.arrange(p1,p2, ncol = 1)

ggdraw(p) +
  draw_text("Figure 3: Autocorrelation and partial autocorrelation function\ncorrelogram",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85)

ggsave("charts/ACFPACF.pdf", width = .width, height = .height, units = "in")


#### X13 Decomposition ####
x13_model[["final"]][["series"]][, "t"] -> x13_t
x13_model[["final"]][["series"]][, "s"] -> x13_s
x13_model[["final"]][["series"]][, "i"] -> x13_i

data.table("Period" = time(x13_t) %>% as.yearqtr(),
           "trend" = x13_t,
           "seasonal" = x13_s,
           "irregular" = x13_i) %>%
  melt(., id.var = "Period", variable.name = "Variable", value.name = "Value") %>%
  ggplot() +
  geom_line(aes(x = as.Date.yearqtr(Period), y = Value),
            size = 0.5, color = cols[1]) +
  scale_y_continuous(breaks = pretty_breaks(n=5),
                     labels = comma) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  facet_grid(Variable ~., scales = "free_y") +
  theme_mma() +
  labs(title = "",
       subtitle = "") -> p

# p1 <- x13_model[["final"]][["series"]][, "t"] %>%
#   autoplot() +
#   scale_y_continuous(breaks = pretty_breaks(n=7),
#                      labels = comma) +
#   ggtitle("\n\n  Trend component") +
#   theme_mma() +
#   theme(plot.title = element_text(size = font_title_size-2, family = font_h, color = headings_color)) 
# 
# p2 <- x13_model[["final"]][["series"]][, "s"] %>%
#   autoplot() +
#   scale_y_continuous(breaks = pretty_breaks(n=7),
#                      labels = comma) +
#   ggtitle("  Seasonal component") +
#   theme_mma() +
#   theme(plot.title = element_text(size = font_title_size-2, family = font_h, color = headings_color)) 
# 
# p3 <- x13_model[["final"]][["series"]][, "i"] %>%
#   autoplot() +
#   scale_y_continuous(breaks = pretty_breaks(n=7),
#                      labels = comma) +
#   ggtitle("  Residuals") +
#   theme_mma() +
#   theme(plot.title = element_text(size = font_title_size-2, family = font_h, color = headings_color)) 
# 
# p4 <- x13_model[["final"]][["series"]][, "sa"] %>%
#   autoplot() +
#   scale_y_continuous(breaks = pretty_breaks(n=7),
#                      labels = comma) +
#   ggtitle("Seasonally adjusted") +
#   theme_mma() +
#   theme(plot.title = element_text(size = font_title_size-2, family = font_h, color = headings_color)) 
# 
# p <- grid.arrange(p1,p2,p3, ncol = 1)

ggdraw(p) +
  draw_text("Figure 4: X13 decomposition",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(millions of MVR)", 
            x = 0.01, y = 0.94, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/X13decomp.pdf", width = .width, height = .height+0.75, units = "in")


#### Subseries plots ####
max_limit <- QNA_ts %>% max() %>% plyr::round_any(accuracy = 2000, f = ceiling)
min_limit <- QNA_ts %>% min() %>% plyr::round_any(accuracy = 2000, f = floor)

p1 <- QNA_ts %>%
  ggmonthplot() +
  scale_y_continuous(breaks = pretty_breaks(n=10),
                     labels = comma,
                     limits = c(min_limit, max_limit)) +
  labs(title = "\n\n\n Seasonally unadjusted QNA",
       subtitle = " (millions of MVR)") +
  theme_mma() +
  theme(plot.title = element_text(size = font_title_size-2, family = font_h, color = headings_color, lineheight = 0.85),
        plot.subtitle = element_text(size = font_sub_size-2, family = font_h, color = headings_color, lineheight = 0))

max_limit <- QNA_sa %>% max() %>% plyr::round_any(accuracy = 20, f = ceiling)
min_limit <- QNA_sa %>% min() %>% plyr::round_any(accuracy = 20, f = floor)

p2 <- QNA_sa %>%
  ggmonthplot() +
  scale_y_continuous(breaks = pretty_breaks(n=10),
                     labels = comma,
                     limits = c(min_limit, max_limit)) +
  labs(title = "\n\n Seasonally adjusted QNA",
       subtitle = " (100 times log real GDP)") +
  theme_mma() +
  theme(plot.title = element_text(size = font_title_size-2, family = font_h, color = headings_color, lineheight = 0.85),
        plot.subtitle = element_text(size = font_sub_size-2, family = font_h, color = headings_color, lineheight = 0))

p <- grid.arrange(p1,p2, ncol = 1)

ggdraw(p) +
  draw_text("Figure 5: Subseries plots of QNA, before and after seasonal\nadjustment",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85)

ggsave("charts/Subseries.pdf", width = .width, height = (.height+1), units = "in")


#### MAR ####
order <- MAR[order(word(Estimate_number, 1) %>% as.numeric()), Estimate_number] %>% as.character()
max_limit <- plyr::round_any(max(MAR[,MAR]), f = ceiling, accuracy = 0.2)

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_bar(data = MAR,
           aes(x = factor(Estimate_number, levels = order), y = MAR),
           stat = "identity", fill = cols[1], width = 0.5) +
  scale_y_continuous(breaks = seq(0, max_limit, 0.2),
                     labels = comma,
                     limits = c(0, max_limit)) +
  theme_mma() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "\n") -> p

# ggplot() +
#   geom_hline(yintercept = 0, size = 0.25, colour="black") +
#   geom_bar(data = MAR,
#            aes(x = factor(Estimate_number, levels = rev(order)), y = MAR),
#            stat = "identity", fill = cols[1], width = 0.5) +
#   scale_y_continuous(breaks = seq(0, max_limit, 0.2),
#                      labels = comma,
#                      limits = c(0, max_limit)) +
#   coord_flip() +
#   theme_mma() +
#   labs(title = "\n") -> p

ggdraw(p) +
  draw_text("Figure 6: Mean absolute revision (MAR)",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85)

ggsave("charts/MAR.pdf", width = .width, height = .height, units = "in")


#### Revision Size ####
copy(rev_dtx) -> Tb
recode(Tb$Method,
       "BNF1" = "BN Filter: 40-window Demeaning",
       "BNF2" = "BN Filter: Sample Mean Demeaning",
       "BNF3" = "BN Filter: Manual Break Dates",
       "BP Long" = "BP Filter: 6-32 quarters",
       "BP Short" = "BP Filter: 4-24 quarters",
       "HP" = "HP Filter",
       "HPAlt" = "HP Filter Alternative",
       "Linear" = "Linear Model",
       "Quadratic" = "Quadratic Model",
       "Piecewise" = "Piecewise Model") -> Tb$Method

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_bar(data = Tb[Measure %in% c("Root Mean Square", "Standard Deviation")],
           aes(x = fct_reorder(Method, -Value), y = Value, fill = Measure),
           stat = "identity", position = "dodge", width = 0.75) +
  scale_y_continuous(breaks = pretty_breaks(n=5)) +
  scale_fill_manual(values = cols) +
  coord_flip() +
  theme_mma() +
  labs(title = "\n") -> p

ggdraw(p) +
  draw_text("Figure 7: Size of revisions",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85)

ggsave("charts/RevSize.pdf", width = .width, height = .height, units = "in")


#### Revision Correlation ####
ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_bar(data = Tb[Measure %in% c("Correlation", "Same Sign")],
           aes(x = fct_reorder(Method, Value), y = Value, fill = Measure),
           stat = "identity", position = "dodge", width = 0.75) +
  scale_y_continuous(breaks = pretty_breaks(n=5)) +
  scale_fill_manual(values = cols) +
  coord_flip() +
  theme_mma() +
  labs(title = "\n") -> p

ggdraw(p) +
  draw_text("Figure 8: Correlation and same sign proportion of estimates",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85)

ggsave("charts/RevCorr.pdf", width = .width, height = .height, units = "in")


#### Recession Chart ####
max_limit <- plyr::round_any(max(Growth.QNA.g[,Value]), f = ceiling, accuracy = 10)
min_limit <- plyr::round_any(min(Growth.QNA.g[,Value]), f = floor, accuracy = 10)

min_date <- Growth.QNA.g[, Period] %>% min() %>% year() %>% paste0("-01-01") %>% as.Date()
max_date <- Growth.QNA.g[, Period] %>% max() %>% year() %>% paste0("-12-31") %>% as.Date()

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_rect(data = Recession_Dates_Tb,
            aes(xmin = as.Date.yearqtr(Period_start), xmax = as.Date.yearqtr(Period_end),
                ymin = min_limit, ymax = max_limit), alpha = 0.3) +
  geom_line(data = Growth.QNA.g,
            aes(x = as.Date.yearqtr(Period), y = Value, color = Variable),
            size = 0.5) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",
               limits = c(min_date, max_date)) +
  scale_y_continuous(breaks = seq(min_limit, max_limit, 10),
                     limits = c(min_limit, max_limit)) +
  scale_color_manual(values = cols,
                     labels = c("GrowthPrec" = "Preceding",
                                "GrowthCorr" = "Corresponding")) +
  theme_mma() +
  labs(title = "\n",
       subtitle = "",
       caption = "") -> p

ggdraw(p) +
  draw_text("Figure 9: Preceding quarter growth rates of seasonally adjusted\nQNA",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(percent)", 
            x = 0.01, y = 0.89, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color) +
  draw_text("Note: Recessions highlighted by the grey bars.", 
            x = 0.01, y = 0.03, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/Reccesion.pdf", width = .width, height = .height, units = "in")


#### Final Filters ####
copy(cycle_final) %>%
  .[Vintage == v & str_detect(Method, "BNF1|BP Short|HP|Piecewise") & !str_detect(Method, "HPAlt")] -> Tb

max_limit <- plyr::round_any(max(Tb[, Value]), f = ceiling, accuracy = 10)
min_limit <- plyr::round_any(min(Tb[, Value]), f = floor, accuracy = 10)

min_date <- Tb[, Period] %>% min() %>% year() %>% paste0("-01-01") %>% as.Date()
max_date <- Tb[, Period] %>% max() %>% year() %>% paste0("-12-31") %>% as.Date()

recode(Tb$Method,
       "BNF1" = "BN Filter: 40-window Demeaning",
       "BP Short" = "BP Filter: 4-24 quarters",
       "HP" = "HP Filter",
       "Piecewise" = "Piecewise Model") -> Tb$Method

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_rect(data = Recession_Dates_Tb,
            aes(xmin = as.Date.yearqtr(Period_start), xmax = as.Date.yearqtr(Period_end),
                ymin = min_limit, ymax = max_limit), alpha = 0.3) +
  geom_line(data = Tb,
            aes(x = as.Date.yearqtr(Period), y = Value, color = fct_rev(Method)),
            size = 0.5) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",
               limits = c(min_date, max_date)) +
  scale_y_continuous(breaks = seq(min_limit, max_limit, 10),
                     limits = c(min_limit, max_limit)) +
  scale_color_manual(values = cols) +
  theme_mma() +
  guides(color = guide_legend(nrow = 2, byrow = FALSE)) +
  labs(title = "",
       subtitle = "",
       caption = "") -> p

ggdraw(p) +
  draw_text("Figure 10: Estimated output gap from the selected methods",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(100 times log real GDP)", 
            x = 0.01, y = 0.93, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color) +
  draw_text("Note: Recessions highlighted by the grey bars.", 
            x = 0.01, y = 0.03, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/FinalFilters.pdf", width = .width, height = .height, units = "in")


#### GDP Correlation with Estimates ####
max_limit <- plyr::round_any(max(Corr_Tb_f[, `Correlation with GDP Growth (Preceding)`]), f = ceiling, accuracy = 10)

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_bar(data = melt(Corr_Tb_f, id.var = "Method", variable.name = "Measure", value = "Value"),
           aes(x = fct_reorder(str_wrap(Method, width = 20), -Value), y = Value),
           stat = "identity", position = "dodge", width = 0.35, fill = cols[1]) +
  scale_y_continuous(breaks = seq(0, max_limit, 10),
                     limits = c(0, max_limit)) +
  theme_mma() +
  labs(title = "\n",
       subtitle = "") -> p

ggdraw(p) +
  draw_text("Figure 11: Correlation between current output gap estimates and current\nGDP growth (preceding quarter)",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(percent)", 
            x = 0.01, y = 0.89, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/GDPCorrelFilters.pdf", width = .width, height = .height, units = "in")


#### BN Filter - Potential + OG ####
min_date <- BNF1_final_m[, Period] %>% min() %>% year() %>% paste0("-01-01") %>% as.Date()
max_date <- BNF1_final_m[, Period] %>% max() %>% year() %>% paste0("-12-31") %>% as.Date()

ggplot(data = BNF1_final_m) +
  geom_line(aes(x = as.Date.yearqtr(Period), y = Value, color = Variable),
            size = 0.5) +
  facet_grid(Panel~., scales = "free_y") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",
               limits = c(min_date, max_date)) +
  scale_y_continuous(breaks = pretty_breaks(n=5)) +
  scale_color_manual(values = cols,
                     labels = c("QNA" = "Seasonally adjusted QNA",
                                "Trend" = "Potential output",
                                "Cycle" = "Output gap")) +
  theme_mma() +
  theme(strip.text = element_blank(),
        panel.spacing = unit(1.5, "lines")) +
  labs(title = "\n",
       subtitle = "") -> p

ggdraw(p) +
  draw_text("Figure 12: Seasonally adjusted QNA, potential output and output\ngap estimated by the BN Filter (40-window demeaning)",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(100 times log)", 
            x = 0.01, y = 0.89, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/BNF1_final.pdf", width = .width, height = .height, units = "in")


#### Confidence Bands ####
max_limit <- plyr::round_any(max(BNF_full[, UB_95]), f = ceiling, accuracy = 10)
min_limit <- plyr::round_any(min(BNF_full[, LB_95]), f = floor, accuracy = 10)

min_date <- BNF_full[, Period] %>% min() %>% year() %>% paste0("-01-01") %>% as.Date()
max_date <- BNF_full[, Period] %>% max() %>% year() %>% paste0("-12-31") %>% as.Date()

ggplot(data = BNF_full) +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_line(aes(x = as.Date.yearqtr(Period), y = Cycle), color = cols[1]) +
  geom_ribbon(aes(x = as.Date.yearqtr(Period), ymin = LB_95, ymax = UB_95),
              fill = cols[1], alpha = 0.2) +
  geom_ribbon(aes(x = as.Date.yearqtr(Period), ymin = LB_80, ymax = UB_80),
              fill = cols[1], alpha = 0.2) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",
               limits = c(min_date, max_date)) +
  scale_y_continuous(breaks = seq(min_limit, max_limit, 10),
                     limits = c(min_limit, max_limit)) +
  theme_mma() +
  labs(title = "\n",
       subtitle = "") -> p

ggdraw(p) +
  draw_text("Figure 13: Estimated output gap from the BN filter with 80% and\n95% confidence bands",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(100 times log real GDP)", 
            x = 0.01, y = 0.89, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/ConfBands.pdf", width = .width, height = .height, units = "in")


#### Corresponding Growth Rates ####
Corr_test_dt[, c("Period", "UnadjCorr", "GrowthCorr")] %>%
  melt(., id.var = "Period", variable.name = "Variable", value.name = "Value") %>%
  .[!is.na(Value)] -> Tb

max_limit <- plyr::round_any(max(Tb[, Value]), f = ceiling, accuracy = 10)
min_limit <- plyr::round_any(min(Tb[, Value]), f = floor, accuracy = 10)

min_date <- Tb[, Period] %>% min() %>% year() %>% paste0("-01-01") %>% as.Date()
max_date <- Tb[, Period] %>% max() %>% year() %>% paste0("-12-31") %>% as.Date()

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_line(data = Tb,
            aes(x = as.Date.yearqtr(Period), y = Value, color = Variable),
            size = 0.5) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",
               limits = c(min_date, max_date)) +
  scale_y_continuous(breaks = seq(min_limit, max_limit, 10),
                     limits = c(min_limit, max_limit)) +
  scale_color_manual(values = cols,
                     labels = c("UnadjCorr" = "Seasonally unadjusted",
                                "GrowthCorr" = "Seasonally adjusted")) +
  theme_mma() +
  labs(title = "\n",
       subtitle = "") -> p

ggdraw(p) +
  draw_text("Figure 14: Corresponding quarterly growth rates (seasonally \nadjusted vs. unadjusted)",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(percent)", 
            x = 0.01, y = 0.89, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/CorrGrowth.pdf", width = .width, height = .height, units = "in")


#### BN Filter Estimates ####
cycle_final[Vintage == v & str_detect(Method, "BNF")] -> Tb

recode(Tb$Method,
       "BNF1" = "BN Filter: 40-window Demeaning",
       "BNF2" = "BN Filter: Sample Mean Demeaning",
       "BNF3" = "BN Filter: Manual Break Dates") -> Tb$Method

max_limit <- plyr::round_any(max(Tb[, Value]), f = ceiling, accuracy = 10)
min_limit <- plyr::round_any(min(Tb[, Value]), f = floor, accuracy = 10)

min_date <- Tb[, Period] %>% min() %>% year() %>% paste0("-01-01") %>% as.Date()
max_date <- Tb[, Period] %>% max() %>% year() %>% paste0("-12-31") %>% as.Date()

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_rect(data = Recession_Dates_Tb,
            aes(xmin = as.Date.yearqtr(Period_start), xmax = as.Date.yearqtr(Period_end),
                ymin = min_limit, ymax = max_limit), alpha = 0.3) +
  geom_line(data = Tb,
            aes(x = as.Date.yearqtr(Period), y = Value, color = Method),
            size = 0.5) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",
               limits = c(min_date, max_date)) +
  scale_y_continuous(breaks = seq(min_limit, max_limit, 10),
                     limits = c(min_limit, max_limit)) +
  scale_color_manual(values = cols) +
  theme_mma() +
  labs(title = "",
       subtitle = "",
       caption = "") +
  guides(color = guide_legend(nrow = 2)) -> p

ggdraw(p) +
  draw_text("Figure 15: BN filter estimates",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(100 times log real GDP)", 
            x = 0.01, y = 0.93, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color) +
  draw_text("Note: Recessions highlighted by the grey bars.", 
            x = 0.01, y = 0.03, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/BNF.pdf", width = .width, height = .height, units = "in")


#### BP and HP Filter ####
cycle_final[Vintage == v & str_detect(Method, "BP|HP")] -> Tb

recode(Tb$Method,
       "BP Long" = "BP Filter: 6-32 quarters",
       "BP Short" = "BP Filter: 4-24 quarters",
       "HP" = "HP Filter",
       "HPAlt" = "HP Filter Alternative") -> Tb$Method

max_limit <- plyr::round_any(max(Tb[, Value]), f = ceiling, accuracy = 10)
min_limit <- plyr::round_any(min(Tb[, Value]), f = floor, accuracy = 10)

min_date <- Tb[, Period] %>% min() %>% year() %>% paste0("-01-01") %>% as.Date()
max_date <- Tb[, Period] %>% max() %>% year() %>% paste0("-12-31") %>% as.Date()

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_rect(data = Recession_Dates_Tb,
            aes(xmin = as.Date.yearqtr(Period_start), xmax = as.Date.yearqtr(Period_end),
                ymin = min_limit, ymax = max_limit), alpha = 0.3) +
  geom_line(data = Tb,
            aes(x = as.Date.yearqtr(Period), y = Value, color = Method),
            size = 0.5) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",
               limits = c(min_date, max_date)) +
  scale_y_continuous(breaks = seq(min_limit, max_limit, 10),
                     limits = c(min_limit, max_limit)) +
  scale_color_manual(values = cols) +
  theme_mma() +
  labs(title = "",
       subtitle = "",
       caption = "") +
  guides(color = guide_legend(nrow = 2)) -> p

ggdraw(p) +
  draw_text("Figure 16: BP filter, HP filter and HP filter alternative estimates",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(100 times log real GDP)", 
            x = 0.01, y = 0.93, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color) +
  draw_text("Note: Recessions highlighted by the grey bars.", 
            x = 0.01, y = 0.03, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/BPHP.pdf", width = .width, height = .height, units = "in")


#### Deterministic Trends Models' Estimates ####
cycle_final[Vintage == v & str_detect(Method, "Linear|Quadratic|Piecewise")] -> Tb

max_limit <- plyr::round_any(max(Tb[, Value]), f = ceiling, accuracy = 10)
min_limit <- plyr::round_any(min(Tb[, Value]), f = floor, accuracy = 10)

min_date <- Tb[, Period] %>% min() %>% year() %>% paste0("-01-01") %>% as.Date()
max_date <- Tb[, Period] %>% max() %>% year() %>% paste0("-12-31") %>% as.Date()

recode(Tb$Method,
       "Linear" = "Linear Model",
       "Quadratic" = "Quadratic Model",
       "Piecewise" = "Piecewise Model") -> Tb$Method

ggplot() +
  geom_hline(yintercept = 0, size = 0.25, colour="black") +
  geom_rect(data = Recession_Dates_Tb,
            aes(xmin = as.Date.yearqtr(Period_start), xmax = as.Date.yearqtr(Period_end),
                ymin = min_limit, ymax = max_limit), alpha = 0.3) +
  geom_line(data = Tb,
            aes(x = as.Date.yearqtr(Period), y = Value, color = Method),
            size = 0.5) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y",
               limits = c(min_date, max_date)) +
  scale_y_continuous(breaks = seq(min_limit, max_limit, 10),
                     limits = c(min_limit, max_limit)) +
  scale_color_manual(values = cols) +
  theme_mma() +
  labs(title = "",
       subtitle = "",
       caption = "") -> p

ggdraw(p) +
  draw_text("Figure 17: Linear, quadratic and piecewise model estimates",
            x = 0.01, y = 0.98, hjust = 0, vjust = 1,
            size = font_title_size,
            family = font_h,
            color = headings_color,
            lineheight = 0.85) +
  draw_text("(100 times log real GDP)", 
            x = 0.01, y = 0.93, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color) +
  draw_text("Note: Recessions highlighted by the grey bars.", 
            x = 0.01, y = 0.03, hjust = 0, vjust = 1,
            size = font_sub_size,
            family = font_sub,
            color = headings_color)

ggsave("charts/Deterministic.pdf", width = .width, height = .height, units = "in")

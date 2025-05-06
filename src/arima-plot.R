#' Plot Arima
#' 
#' Compare different ARIMA fits across different training sets. Briefly, 
#' fitting ARIMA models in fable is done by searching in the space of possible 
#' models the one with best performance; this seach starts by computing the 
#' optimal number of differences (d and D), then  evaluates different models' 
#' performance using AICc (corrected). Here plot the count of different ARIMA 
#' models as well as count of single terms (p, d, q, P, D, Q or ar, ma, sar, 
#' sma); implicit and explicit refers to whether higher terms imply lower ones
#' (e.g., p2 implies p1) or not (full count of terms).
#' 
#' Ref: Shumway, Time-series analysis book; Hyndman, Forecasting: Principles 
#' and Practice.
#'
#' @author Ensor Palacios, email{ensorrafael.palacios@bristol.ac.uk}
#' @date 2025-04-24

# Import packages --------------------------------------------------------------
library(conflicted)
import::from(here, here)
import::from(magrittr, "%>%")
import::from(dplyr, select, filter, mutate)
import::from(tidyr, pivot_longer)
library(purrr)
library(ggplot2)


# Load Data --------------------------------------------------------------------
data_path <- paste0(here(), "/output/fits/fits_short.RDS")
fit_all <- readRDS(file = data_path)
fit_fable <- # extract fable fits (containing ARIMA models)
  fit_all$fable


# Arima terms ------------------------------------------------------------------
arima_model <- 
  fit_fable %>%
  mutate(
    model =
      map_chr(arima_d, \(x) {
        x %>%
          pluck("fit", "spec") %>% 
          select(p:Q) %>% 
          paste0(names(.), ., collapse = "-")
      })
  )

arima_terms_i <- # implicit count (e.g., p2 implies p1) - better to separate m.
  fit_fable %>%
  mutate(
    map(arima_d, \(x) {
      # browser()
      x %>%
        pluck("fit", "spec") %>% 
        select(p:Q) %>% 
        paste0(names(.), .) %>% 
        t() %>% as.data.frame()
    }) %>% list_rbind()
  ) %>% 
  pivot_longer(cols = V1:V6) %>% 
  mutate(
    value = value %>%
      factor(
        levels = c(
          "p0", "p1","p2", "d0", "d1", "d2", "q0", "q1", "q2", "P0",
          "P1", "P2", "D0", "D1", "D2", "Q0", "Q1", "Q2"
          )
        )
  )

arima_terms_e <- # explicit count (e.g., full count for ar1 when ar2 present) 
  fit_fable %>%
  select(arima_d) %>%
  coef() %>%
  dplyr::filter(!grepl("days_", term))
  

# Plot -------------------------------------------------------------------------
plt_model <- 
  arima_model %>%
  ggplot(aes(model, fill = site, group = site)) +
  geom_bar(
    position = position_dodge2(preserve = "single", padding=0),
    just = 0.5) +
  scale_x_discrete(guide = guide_axis(angle = 45))

plt_term_i <-
  arima_terms_i %>%
  dplyr::filter(!grepl("0", value)) %>% 
  ggplot(aes(value, fill= site, group = site)) +
  geom_bar(
    position = position_dodge2(preserve = "single", padding=0),
    just = 0.5)

plt_term_e <-
  arima_terms_e %>%
  ggplot(aes(term, fill= site, group = site)) +
  geom_bar(
    position = position_dodge2(preserve = "single", padding=0),
    just = 0.5)


# Save -------------------------------------------------------------------------
save_path = here("output/plots/arima_models/")
if (!file.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
}

plt_model %>% 
  ggsave(
    file = paste0(save_path, "arima_models.eps"),
    width = 15, height = 8.88,
    dpi = 500
  )
plt_term_i %>%
  ggsave(
    file = paste0(save_path, "arima_terms_implicit.eps"),
    width = 15, height = 8.88,
    dpi = 500
  )

plt_term_e %>%
  ggsave(
    file = paste0(save_path, "arima_terms_explicit.eps"),
    width = 15, height = 8.88,
    dpi = 500
  )

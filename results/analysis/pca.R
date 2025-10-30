library(googlesheets4)
library(dplyr)
library(tidyr)
library(ggfortify)
library(ggplot2)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(pheatmap)
library(patchwork)
library(viridis)

url <- "https://docs.google.com/spreadsheets/d/1rFnJkfpF-YfK04uGa-IzjzZYy-czLQZEiiLOF4hr3_w/edit?usp=sharing"

################################################################################
################################################################################
###########################  numerical and complete ############################
################################################################################
################################################################################

methods <- read_sheet(url, sheet = "Cleaned Methods - ALL") %>% 
  filter(benchmark) %>% 
  select(Method, imputation_function) %>% 
  rename("elegant_name" = "Method",
         "imputation_fun" = "imputation_function") %>% 
  arrange(tolower(elegant_name)) %>% 
  mutate(nb = 1:n())

imputation_summary <- readRDS("./results/imputation_summary_M13.RDS")

imputation_summary <- imputation_summary %>% 
  filter(!(method %in% c("mice_cart50", "mice_cart100", "superimputer", 
                         "supersuperimputer", "engression", "missmda_em"))) %>% 
  
  filter(set_id != "oes10") %>% # Had Missing Data in it!
  filter(set_id != "pyrimidines") %>%  # weird error for collinearity
  filter(set_id != "solder") %>%   # always error
  filter(set_id != "Ozone") %>%  # always error (in score)
  filter(set_id != "colic") %>%  # always error 
  filter(set_id != "tao") %>%  # exact same as oceanbuoys
  filter(set_id != "meatspec") %>%  # high correlations
  filter(set_id != "exa") %>%  # weird
  filter(!(method %in% c("min", "cm", "halfmin",
                         "minProb")))

small_sets <- c("star", "tvdoctor", "cheddar", "eco", "leafburn", "stat500", "savings",
                "chicago", "sat", "seatpos", "fpe", "pyrimidines", "Animals_na", 
                "employee", "mammalsleep", "chredlin")

imputation_summary <- imputation_summary %>% 
  filter(case == "complete") %>% 
  filter(!(method %in% c("gbmImpute", 
                         "missmda_MIFAMD_reg", "missmda_MIFAMD_em",
                         "SVTImpute"))) %>% 
  filter(!(set_id %in% small_sets)) %>% 
  left_join(methods, by = "imputation_fun")  %>% 
  mutate(method = elegant_name) %>% 
  select(-elegant_name)


imputation_summary <- imputation_summary %>% 
  filter(case == "complete") %>% 
  filter(!(method %in% c("mice_default", "gbmImpute", 
                         "missmda_mifamd_reg", "missmda_mifamd_em",
                         "SVTImpute"))) 

error_summary <- readRDS("./results/comp_errors_num.RDS") %>% 
  filter(!is.na(measure), !is.na(score)) %>% 
  merge(methods) %>% 
  mutate(method = elegant_name) %>% 
  select(-elegant_name)


imputation_summary <- rbind(imputation_summary, error_summary) %>%  
  group_by(method, set_id, mechanism, ratio, rep, measure) %>% 
  filter(if (n() > 1) !is.na(score) else TRUE) %>% 
  filter(measure != "IScore")


pca_dat <- imputation_summary %>%
  filter(set_id != "meatspec") %>% 
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter( measure == "energy_std") %>% 
  unique() %>% 
  group_by(nb, method, set_id, mechanism, ratio) %>%
  reframe(score = mean(score, na.rm = TRUE)) %>%  
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  select(nb, method, ranking, case_id) %>% 
  pivot_wider(names_from = case_id, values_from = ranking)


#### PCA and processing

pca_res <- PCA(scale(pca_dat[, -c(1, 2)]))

df <- pca_res$var$cor[, c(1, 2)] %>%
  as.data.frame()

df_ind <- pca_res$ind$coord[, c(1, 2)] %>% 
  as.data.frame() %>%
  mutate(method = unlist(pca_dat[, 2]),
         nb = unlist(pca_dat[, 1]))

colnames(df)[1:2] <- paste0("dim ", c(1, 2))

pca_res_pctg <- pca_res$eig[, 2][1:2]

performance <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter( measure == "energy_std") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  group_by(method) %>% 
  reframe(mean_ranking = mean(ranking))


p2 <- df_ind %>% 
  right_join(performance) %>% 
  ggplot() +
  geom_text_repel(aes(x = Dim.1, y = Dim.2, label = nb, col = mean_ranking), 
                  size = 5,
                  max.overlaps = Inf, 
                  # segment.alpha = 0.5,
                  min.segment.length = 0,
                  point.padding = 2) +
  geom_point(aes(x = Dim.1, y = Dim.2, colour = mean_ranking), alpha = 0.8, size = 2) +
  # xlim(-11, 13) +
  # ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 17) +
  xlab(paste0("Dim 1 (", round(pca_res_pctg[1], 1), "%)")) +
  ylab(paste0("Dim 2 (", round(pca_res_pctg[2], 1), "%)")) +
  scale_color_continuous("Averaged \nranking") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8,0.85),
        legend.direction = "horizontal",
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  guides(color = guide_colourbar(barwidth = 7, barheight = 1)) +
  scale_size(guide = "none")


df_legend <- df_ind %>% 
  right_join(performance) %>%
  select(nb, method) %>%
  arrange(nb, method)

tbl <- ggplot(df_legend, aes(x = 0, y = reorder(nb, -nb), label = paste(nb, method))) +
  geom_text(hjust = 0, size = 4) +
  theme_void() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(5, 0, -5, 0), 
    plot.background = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_cartesian(clip = "off") +
  xlim(0, 1)


source("./data/get_datasets_dim.R")
sets_dim <- readRDS("./data/datasets/sets_dim.RDS")
sets_dim %>%  arrange(n_col)


supp_df <- df %>%
  mutate(var = rownames(.)) %>%
  mutate(set_id = sapply(var, function(ith) strsplit(ith, " ")[[1]][1])) %>% 
  merge(sets_dim) %>% 
  mutate(n_columns = cut(n_col, breaks = c(1, 10, 100, 315))) %>% 
  group_by(n_columns) 


supp_df <- bind_rows(
  supp_df %>% slice_max(`dim 1`, n = 5),
  supp_df %>% slice_max(`dim 2`, n = 5),
  supp_df %>% ungroup() %>% slice_min(`dim 2`, n = 5)
) %>% distinct()

p1 <- df %>%
  mutate(var = rownames(.)) %>%
  mutate(set_id = sapply(var, function(ith) strsplit(ith, " ")[[1]][1])) %>% 
  merge(sets_dim) %>% 
  mutate(n_columns = cut(n_col, breaks = c(1, 10, 100, 315)),
         arrow_length = sqrt(`dim 1`^2 + `dim 2`^2)) %>% 
  arrange(arrow_length) %>% 
  ggplot(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]), col = n_columns)) +
  geom_segment(aes(x = 0,
                   y = 0,
                   xend = get(colnames(df)[1]),
                   yend = get(colnames(df)[2])),
               arrow = arrow(), size = 1, alpha = 0.2, show.legend = FALSE) +
  geom_point(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]))) +
  theme_minimal(base_size = 17) +
  xlab(paste0("Dim 1 (", round(pca_res_pctg[1], 1), "%)")) +
  ylab(paste0("Dim 2 (", round(pca_res_pctg[2], 1), "%)")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text_repel(supp_df, mapping = aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]),
                                         label = var), hjust = -0.2, size = 5, show.legend = FALSE,
                  col = "black", max.overlaps = Inf, segment.alpha = 0.6,
                  min.segment.length = 0, direction = "y") +
  xlim(0, 1.3) +
  scale_colour_viridis_d(name = "Number of columns\nin a dataset") +
  theme(legend.position = "bottom")

p1

# 
# p2 + p1 + 
#   plot_layout(widths = c(2, 1)) + 
#   plot_annotation(tag_levels = 'A') &
#   theme(plot.tag = element_text(size = 20, face = "bold"))+
#   theme(legend.position = "bottom") +
#   plot_layout(guides = 'collect')

##############################################################################

performance_on_all <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter( measure == "energy_std") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  group_by(method) %>% 
  reframe(mean_ranking = mean(ranking))


performance_on_large <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter(measure == "energy_std") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  merge(sets_dim) %>% 
  mutate(large = ifelse(n_col > 100, TRUE, FALSE)) %>% 
  group_by(method, large) %>% 
  reframe(mean_ranking_large = mean(ranking)) %>% 
  filter(large) %>% 
  select(-large)


perf_on_large_vs_all <- performance_on_all %>% 
  merge(performance_on_large) %>% 
  mutate(rank_diff = mean_ranking - mean_ranking_large) %>% 
  select(method, rank_diff)


p3 <- df_ind %>% 
  merge(perf_on_large_vs_all) %>% 
  ggplot() +
  geom_text_repel(aes(x = Dim.1, y = Dim.2, label = nb, col = rank_diff), 
                  size = 5,
                  max.overlaps = Inf, 
                  # segment.alpha = 0.5,
                  min.segment.length = 0,
                  point.padding = 2) +
  geom_point(aes(x = Dim.1, y = Dim.2, colour = rank_diff)) +
  # xlim(-11, 13) +
  # ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 17) +
  xlab(paste0("Dim 1 (", round(pca_res_pctg[1], 1), "%)")) +
  ylab(paste0("Dim 2 (", round(pca_res_pctg[2], 1), "%)")) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.85),
        legend.direction = "horizontal") +
  guides(color = guide_colourbar(barwidth = 7, barheight = 1)) +
  scale_color_gradient("Rank \ndifference")


#### paper plot - I took the size 20 x 13 (PDF)

(tbl | (p2 / p3) | p1) +
  plot_layout(widths = c(0.35, 1.8, 1.1), heights = c(1, 1, 1.1)) + 
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 20, face = "bold"))

################################################################################
################################################################################
###########################  numerical and mixed complete ######################
################################################################################
################################################################################

imputation_summary <- readRDS("./results/imputation_summary_M13.RDS")

imputation_summary <- imputation_summary %>% 
  filter(!(method %in% c("mice_cart50", "mice_cart100", "superimputer", 
                         "supersuperimputer", "engression", "missmda_em"))) %>% 
  
  filter(set_id != "oes10") %>% # Had Missing Data in it!
  filter(set_id != "pyrimidines") %>%  # weird error for collinearity
  filter(set_id != "solder") %>%   # always error
  filter(set_id != "Ozone") %>%  # always error (in score)
  filter(set_id != "colic") %>%  # always error 
  filter(set_id != "tao") %>%  # exact same as oceanbuoys
  filter(set_id != "meatspec") %>%  # high correlations
  filter(set_id != "exa") %>%  # weird
  filter(!(method %in% c("min", "cm", "halfmin",
                         "minProb")))

small_sets <- c("star", "tvdoctor", "cheddar", "eco", "leafburn", "stat500", "savings",
                "chicago", "sat", "seatpos", "fpe", "pyrimidines", "Animals_na", 
                "employee", "mammalsleep")

methods_cat <- unique((imputation_summary %>% filter(case == "categorical"))$method)

imputation_summary <- imputation_summary %>%
  filter(case %in% c("complete", "categorical")) %>% 
  filter(method %in% methods_cat) %>% 
  filter(!(set_id %in% small_sets)) %>% 
  merge(methods) %>% 
  mutate(method = elegant_name) %>% 
  select(-elegant_name)

imputation_summary <- rbind(imputation_summary, error_summary) %>%  
  group_by(method, set_id, mechanism, ratio, rep, measure) %>% 
  filter(if (n() > 1) !is.na(score) else TRUE) %>% 
  filter(measure != "IScore") %>% 
  filter(method %in% methods_cat) %>% 
  filter(!(method %in% c("gbmImpute", "missmda_mifamd_reg", "missmda_mifamd_em",
                         "SVTImpute", "missmda_famd_em", "missmda_famd_reg"))) 

pca_dat <- imputation_summary %>%
  filter(set_id != "meatspec") %>% 
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter( measure == "energy_std") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  select(method, ranking, case_id) %>% 
  pivot_wider(names_from = case_id, values_from = ranking)


#### PCA and processing

pca_res <- PCA(scale(pca_dat[, -1]))

df <- pca_res$var$cor[, c(1, 2)] %>%
  as.data.frame() %>% 
  ungroup()

df_ind <- pca_res$ind$coord[, c(1, 2)] %>% 
  as.data.frame() %>%
  mutate(method = unlist(pca_dat[, 1]))

colnames(df)[1:2] <- paste0("dim ", c(1, 2))

pca_res_pctg <- pca_res$eig[, 2][1:2]


#######  BIPLOT


sets_dim <- readRDS("./data/datasets/sets_dim.RDS") %>% 
  mutate(n_fac = 0)

nms <- sub(".RDS", "", list.files("./data/datasets/complete_backup/categorical/"))

sets_dim <- rbind(sets_dim, lapply(nms, function(i) {
  dat <- readRDS(paste0("./data/datasets/complete_backup/categorical/", i, ".RDS"))
  data.frame(set_id = i,
             n_row = nrow(dat),
             n_col = ncol(dat),
             n_fac = sum(sapply(dat, function(i) is.factor(i))))
}) %>%  bind_rows())

supp_df <- bind_rows(
  df %>% slice_max(`dim 1`, n = 5),
  df %>% slice_max(`dim 2`, n = 5),
  df %>% slice_min(`dim 2`, n = 5)
) %>% distinct() %>% 
  mutate(var = rownames(.))


p1 <- df %>%
  mutate(var = rownames(.)) %>%
  mutate(set_id = sapply(var, function(ith) strsplit(ith, " ")[[1]][1])) %>% 
  merge(sets_dim) %>% 
  as_tibble() %>% 
  arrange(n_fac) %>% 
  mutate(n_fac = factor(n_fac, levels = unique(n_fac))) %>% 
  ggplot(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]), colour = n_fac)) +
  geom_segment(aes(x = 0,
                   y = 0,
                   xend = get(colnames(df)[1]),
                   yend = get(colnames(df)[2])),
               arrow = arrow(), size = 1, alpha = 0.3, show.legend = FALSE) +
  geom_point(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]))) +
  theme_minimal(base_size = 15) +
  xlab(paste0("Dim 1 (", round(pca_res_pctg[1], 1), "%)")) +
  ylab(paste0("Dim 2 (", round(pca_res_pctg[2], 1), "%)")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text_repel(supp_df, mapping = aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]),
                                         label = var), hjust = -0.2, size = 5, show.legend = FALSE,
                  col = "black", max.overlaps = Inf, segment.alpha = 0.6,
                  min.segment.length = 0, direction = "y") +
  xlim(0, 1.1) +
  theme(legend.position = "bottom") +
  scale_colour_viridis_d(name = "Number of categorical \ncolumns in a dataset") +
  guides(colour = guide_legend(nrow = 1))



performance_on_all <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter( measure == "energy_std") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  group_by(method) %>% 
  reframe(mean_ranking = mean(ranking))


performance_on_mixed <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter(measure == "energy_std") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  merge(sets_dim) %>% 
  mutate(mixed = n_fac > 0) %>% 
  group_by(method, mixed) %>% 
  reframe(mean_ranking_mixed = mean(ranking)) %>% 
  filter(mixed) %>% 
  select(-mixed)


perf_on_large_vs_mixed <- performance_on_all %>% 
  merge(performance_on_mixed) %>% 
  mutate(rank_diff = mean_ranking - mean_ranking_mixed) %>% 
  select(method, rank_diff)



p3 <- df_ind %>% 
  merge(perf_on_large_vs_mixed) %>% 
  ggplot() +
  geom_text_repel(aes(x = Dim.1, y = Dim.2, label = method, colour = rank_diff), size = 6,
                  max.overlaps = 100) +
  geom_point(aes(x = Dim.1, y = Dim.2, colour = rank_diff)) +
  # xlim(-11, 13) +
  # ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 15) +
  xlab(paste0("Dim 1 (", round(pca_res_pctg[1], 1), "%)")) +
  ylab(paste0("Dim 2 (", round(pca_res_pctg[2], 1), "%)")) +
  guides(color = guide_colourbar(barwidth = 9, barheight = 0.5)) +
  scale_color_gradient("Rank\ndifference") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.1),
        legend.direction = "horizontal")

p2 <- df_ind %>% 
  merge(performance_on_all) %>% 
  ggplot() +
  geom_text_repel(aes(x = Dim.1, y = Dim.2, label = method, colour = mean_ranking), size = 6,
                  max.overlaps = 100) +
  geom_point(aes(x = Dim.1, y = Dim.2, colour = mean_ranking)) +
  # xlim(-11, 13) +
  # ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 15) +
  xlab(paste0("Dim 1 (", round(pca_res_pctg[1], 1), "%)")) +
  ylab(paste0("Dim 2 (", round(pca_res_pctg[2], 1), "%)")) +
  scale_color_continuous("Averaged \nranking") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.1),
        legend.direction = "horizontal") +
  guides(color = guide_colourbar(barwidth = 9, barheight = 0.5)) 



( (p2 / p3) | p1) +
  plot_layout(widths = c(1.5, 1.5)) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 20, face = "bold")) 


################################################################################
################################################################################
###########################  incomplete ########################################
################################################################################
################################################################################

methods <- read_sheet(url, sheet = "Cleaned Methods - ALL") %>% 
  filter(benchmark) %>% 
  select(Method, imputation_function) %>% 
  rename("elegant_name" = "Method",
         "imputation_fun" = "imputation_function")

numerical_incomplete <- readRDS("./results/imputation_summary_incomplete_categorical.RDS") %>% 
  merge(methods) %>% 
  mutate(method = elegant_name)


imputation_summary_incomplete <- rbind(numerical_incomplete,
                                       readRDS("./results/imputation_summary_incomplete_numerical.RDS"))


methods_cat <- unique((imputation_summary_incomplete)$method)

imputation_summary <- imputation_summary_incomplete %>% 
  filter(case %in% c("incomplete", "incomplete_categorical")) %>% 
  filter(method %in% methods_cat) %>% 
  mutate(measure = ifelse(measure == "IScore_cat", "IScore", measure)) %>% 
  filter(measure == "IScore")

pca_dat <- imputation_summary %>%
  filter(set_id != "meatspec") %>% 
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter( measure == "IScore") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  select(method, ranking, case_id) %>% 
  pivot_wider(names_from = case_id, values_from = ranking)


#### PCA and processing

pca_res <- PCA(scale(pca_dat[, -1]))

df <- pca_res$var$cor[, c(1, 2)] %>%
  as.data.frame() %>% 
  ungroup()

df_ind <- pca_res$ind$coord[, c(1, 2)] %>% 
  as.data.frame() %>%
  mutate(method = unlist(pca_dat[, 1]))

colnames(df)[1:2] <- paste0("dim ", c(1, 2))

pca_res_pctg <- pca_res$eig[, 2][1:2]


#######  BIPLOT


sets_dim <- readRDS("./data/datasets/sets_dim.RDS") %>% 
  mutate(n_fac = 0)

nms <- sub(".RDS", "", list.files("./data/datasets/complete_backup/categorical/"))

sets_dim <- rbind(sets_dim, lapply(nms, function(i) {
  dat <- readRDS(paste0("./data/datasets/incomplete_backup/categorical/", i, ".RDS"))
  data.frame(set_id = i,
             n_row = nrow(dat),
             n_col = ncol(dat),
             n_fac = sum(sapply(dat, function(i) is.factor(i))))
}) %>%  bind_rows())

supp_df <- bind_rows(
  df %>% slice_max(`dim 1`, n = 5),
  df %>% slice_max(`dim 2`, n = 5),
  df %>% slice_min(`dim 2`, n = 5)
) %>% distinct() %>% 
  mutate(var = rownames(.))


p1 <- df %>%
  mutate(var = rownames(.)) %>%
  mutate(set_id = sapply(var, function(ith) strsplit(ith, " ")[[1]][1])) %>% 
  merge(sets_dim) %>% 
  as_tibble() %>% 
  arrange(n_fac) %>% 
  mutate(n_fac = factor(n_fac, levels = unique(n_fac))) %>% 
  ggplot(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]), colour = n_fac)) +
  geom_segment(aes(x = 0,
                   y = 0,
                   xend = get(colnames(df)[1]),
                   yend = get(colnames(df)[2])),
               arrow = arrow(), size = 1, alpha = 0.3, show.legend = FALSE) +
  geom_point(aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]))) +
  theme_minimal(base_size = 15) +
  xlab(paste0("Dim 1 (", round(pca_res_pctg[1], 1), "%)")) +
  ylab(paste0("Dim 2 (", round(pca_res_pctg[2], 1), "%)")) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text_repel(supp_df, mapping = aes(x = get(colnames(df)[1]), y = get(colnames(df)[2]),
                                         label = var), hjust = -0.2, size = 5, show.legend = FALSE,
                  col = "black", max.overlaps = Inf, segment.alpha = 0.6,
                  min.segment.length = 0, direction = "y") +
  xlim(0, 1.1) +
  theme(legend.position = "bottom") +
  scale_colour_viridis_d(name = "Number of categorical \ncolumns in a dataset") +
  guides(colour = guide_legend(nrow = 1))



performance_on_all <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter( measure == "IScore") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  group_by(method) %>% 
  reframe(mean_ranking = mean(ranking))


performance_on_mixed <- imputation_summary %>%
  filter(!is.na(measure)) %>% 
  select(-imputation_fun) %>% 
  filter(measure == "IScore") %>% 
  unique() %>% 
  group_by(method, set_id, mechanism, ratio) %>% 
  reframe(score = mean(score, na.rm = TRUE)) %>% 
  mutate(case_id = paste(set_id, mechanism, ratio)) %>% 
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  group_by(set_id, mechanism, ratio) %>% 
  mutate(ranking =  {
    ranking <- rep(NA, length(score))
    ranking[!is.na(score)] <- rank(score[!is.na(score)])
    ranking[is.na(ranking)] <- length(is.na(ranking)) + 1
    ranking
  }) %>% 
  ungroup() %>% 
  merge(sets_dim) %>% 
  mutate(mixed = n_fac > 0) %>% 
  group_by(method, mixed) %>% 
  reframe(mean_ranking_mixed = mean(ranking)) %>% 
  filter(mixed) %>% 
  select(-mixed)


perf_on_large_vs_mixed <- performance_on_all %>% 
  merge(performance_on_mixed) %>% 
  mutate(rank_diff = mean_ranking - mean_ranking_mixed) %>% 
  select(method, rank_diff)



p3 <- df_ind %>% 
  merge(perf_on_large_vs_mixed) %>% 
  ggplot() +
  geom_text_repel(aes(x = Dim.1, y = Dim.2, label = method, colour = rank_diff), size = 4,
                  max.overlaps = 100) +
  geom_point(aes(x = Dim.1, y = Dim.2, colour = rank_diff)) +
  # xlim(-11, 13) +
  # ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 15) +
  xlab(paste0("Dim 1 (", round(pca_res_pctg[1], 1), "%)")) +
  ylab(paste0("Dim 2 (", round(pca_res_pctg[2], 1), "%)")) +
  guides(color = guide_colourbar(barwidth = 9, barheight = 0.5)) +
  scale_color_gradient("Rank\ndifference") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.9),
        legend.direction = "horizontal")

p2 <- df_ind %>% 
  merge(performance_on_all) %>% 
  ggplot() +
  geom_text_repel(aes(x = Dim.1, y = Dim.2, label = method, colour = mean_ranking), size = 4,
                  max.overlaps = 100) +
  geom_point(aes(x = Dim.1, y = Dim.2, colour = mean_ranking)) +
  # xlim(-11, 13) +
  # ylim(-11, 13) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 15) +
  xlab(paste0("Dim 1 (", round(pca_res_pctg[1], 1), "%)")) +
  ylab(paste0("Dim 2 (", round(pca_res_pctg[2], 1), "%)")) +
  scale_color_continuous("Averaged \nranking") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.2, 0.9),
        legend.direction = "horizontal") +
  guides(color = guide_colourbar(barwidth = 9, barheight = 0.5)) 



( (p2 / p3) | p1) +
  plot_layout(widths = c(1.5, 1.5)) +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 20, face = "bold")) 




##########################################


##### Supplementary plots ####





#load packages and set up environment
library(tidyverse)
library(janitor)
library(tidyclust)
library(factoextra)
library(tune)
library(ggrepel)
library(GGally)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

#read in functions
source("scripts/functions/clean_colname.R")
source("scripts/functions/read_standard_stats.R")

#read in data and parse
files <- list.files("inputs", full.names = TRUE, pattern = "Standard") |>
  set_names()

fbref_data <- read_standard_stats(files)

glimpse(fbref_data)

fbref_data <- fbref_data |>
  select(
    squad,
    player_count,
    age,
    poss,
    Gls,
    Ast,
    PK,
    PKatt,
    CrdY,
    CrdR,
    xG,
    npxG,
    xAG,
    PrgC,
    PrgP
  )

#cluster
fbref_data_no_squad <- select(fbref_data, -squad)

hclust_wss_plot <- fviz_nbclust(
  fbref_data_no_squad,
  FUN = hcut,
  method = "wss",
  k.max = 10
)

hclust_wss_plot

nclust_hclust <- 3

hc_spec <- hier_clust(
  num_clusters = nclust_hclust,
  linkage_method = "ward"
)

hc_spec

hc_fit <- hc_spec |>
  fit(~., data = fbref_data_no_squad)

str(hc_fit, max.level = 1)

plot(hc_fit$fit, labels = fbref_data$squad)

#PCA

#tidy PCA code from https://clauswilke.com/blog/2020/09/07/pca-tidyverse-style/
pca_fit <- fbref_data_no_squad |>
  prcomp(scale = TRUE) # do PCA on scaled data

pca_fit |>
  tidy(matrix = "eigenvalues") |>
  filter(PC <= 10) |>
  ggplot(aes(PC, percent)) +
  geom_col() +
  scale_x_continuous(breaks = c(1:10))

# plot rotation matrix
pca_rot <- pca_fit |>
  tidy(matrix = "rotation") |>
  pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") |>
  rename(common_name = column)

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20,
  ends = "first",
  type = "closed",
  length = grid::unit(3, "pt")
)

pca_outliers <- pca_rot |>
  filter(
    PC1 == max(PC1) |
      PC2 == max(PC2) |
      PC3 == max(PC3) |
      PC1 == min(PC1) |
      PC2 == min(PC2) |
      PC3 == min(PC3)
  )

pca_outliers_max <- pca_rot |>
  select(common_name, PC1:PC3) |>
  pivot_longer(-common_name) |>
  group_by(name) |>
  slice_max(order_by = value, n = 1) |>
  ungroup() |>
  distinct(common_name)

pca_outliers_min <- pca_rot |>
  select(common_name, PC1:PC3) |>
  pivot_longer(-common_name) |>
  group_by(name) |>
  slice_min(order_by = value, n = 1) |>
  ungroup() |>
  distinct(common_name)

pca_outliers <- bind_rows(pca_outliers_max, pca_outliers_min) |>
  left_join(pca_rot)

pca_rot |>
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style, lwd = .2) +
  geom_point(
    data = pca_outliers,
    color = "#904C2F"
  ) +
  geom_label_repel(
    data = pca_outliers,
    aes(label = common_name),
    color = "#904C2F"
  ) +
  coord_obs_pred()

pca_fit |>
  augment(fbref_data) |>
  ggplot(aes(.fittedPC1, .fittedPC2, label = squad)) +
  geom_point() +
  geom_label_repel()


#pairwise scatter
my_fn <- function(data, mapping, ...) {
  # Using default ggplot density function

  p <- ggplot(data = data, mapping = mapping) +
    #stat_density2d(
    #  aes(fill = after_stat(density)),
    #  geom = "tile",
    #  contour = FALSE
    #) +
    geom_point() +
    scale_fill_gradientn(colours = viridis::viridis(100, option = "viridis")) +
    scale_x_continuous(labels = NULL) +
    scale_y_continuous(labels = NULL) +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.text = element_text(size = 4)
    )
  p
}

pairwise_scatter <- ggpairs(
  fbref_data_no_squad,
  lower = list(continuous = my_fn)
)

ggsave(
  "outputs/pairwise_scatter.png",
  plot = pairwise_scatter,
  width = 15,
  height = 15,
  dpi = 300
)

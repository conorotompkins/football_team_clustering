#load packages and set up environment
library(tidyverse)
library(janitor)
library(tidyclust)
library(factoextra)
library(tune)
library(ggrepel)
library(GGally)
library(tidytext)
library(plotly)

options(scipen = 999, digits = 4)

theme_set(theme_bw())

# #read in functions
# source("scripts/functions/clean_colname.R")
# source("scripts/functions/read_standard_stats.R")
# source("scripts/functions/read_squad_goalkeeping.R")
# source("scripts/functions/read_squad_shooting.R")
# source("scripts/functions/read_squad_passing.R")

# #read in data and parse
# standard_files <- list.files(
#   "inputs",
#   pattern = "Standard",
#   full.names = TRUE
# ) |>
#   set_names()

# gk_files <- list.files(
#   "inputs",
#   full.names = TRUE,
#   pattern = "Goalkeeping"
# ) |>
#   set_names()

# shooting_files <- list.files(
#   "inputs",
#   full.names = TRUE,
#   pattern = "Shooting"
# ) |>
#   set_names()

# passing_files <- list.files(
#   "inputs",
#   full.names = TRUE,
#   pattern = "Passing"
# ) |>
#   set_names()

# standard_df <- read_standard_stats(standard_files)

# gk_df <- read_squad_goalkeeping(gk_files)

# shooting_df <- read_squad_shooting(shooting_files)

# passing_df <- read_squad_passing(passing_files)

# fbref_data <- list(
#   standard_df,
#   gk_df,
#   shooting_df,
#   passing_df
# ) |>
#   reduce(left_join, by = "squad")

fbref_data_raw <- read_csv("input/cleaned/fbref_data_cleaned.csv")

glimpse(fbref_data_raw)

col_prefixes <- c(
  "std",
  "shooting",
  "passing",
  "pass_types",
  "defense",
  "goalkeeping_adv",
  "goalkeeping",
  "gca",
  "misc",
  "possession"
)

col_prefixes <- str_c("^", col_prefixes, "_") |> str_c(collapse = "|")

fbref_data <- fbref_data_raw |>
  select(
    -c(
      starts_with("std_goals"),
      starts_with("std_assists"),
      std_xg_team,
      std_xg_opponent,
      std_xg_np_team,
      std_xg_np_opponent,
      std_xg_plus_xa_np_team,
      std_xg_plus_xa_np_opponent,
      starts_with("shooting_gls")
    )
  ) |>
  select(
    comp,
    season_end_year,
    squad,
    starts_with("std"),
    starts_with("shooting"),
    starts_with("passing"),
    starts_with("pass_types"),
    starts_with("defense"),
    starts_with("goalkeeping"),
    starts_with("goalkeeping_adv"),
    starts_with("gca"),
    starts_with("misc"),
    starts_with("possession")
  ) |>
  rename_with(~ str_remove(.x, col_prefixes))

glimpse(fbref_data)


# fbref_data |>
#   select(contains("att_total_team"))

# names(fbref_data) |>
#   enframe() |>
#   filter(str_detect(value, "att_total_team|types_passes_attempted_team"))

# fbref_data |>
#   select(squad, comp, season_end_year, contains("_sh_pk_team")) |>
#   view()

#glimpse(fbref_data)

#cluster
fbref_data_no_squad <- select(fbref_data, -c(comp, season_end_year, squad))

hclust_wss_plot <- fviz_nbclust(
  fbref_data_no_squad,
  FUN = hcut,
  method = "wss",
  k.max = 10
)

hclust_wss_plot

nclust_hclust <- 5

hc_spec <- hier_clust(
  num_clusters = nclust_hclust,
  linkage_method = "ward"
)

hc_spec

hc_fit <- hc_spec |>
  fit(~., data = fbref_data_no_squad)

str(hc_fit, max.level = 1)

plot(hc_fit$fit, labels = FALSE)

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

pca_rot |>
  select(1:7) |>
  pivot_longer(cols = starts_with("PC")) |>
  rename(
    pc = name,
    rot = value,
    variable = common_name
  ) |>
  mutate(
    rot_abs = abs(rot),
    sign = sign(rot) |> as.factor()
  ) |>
  arrange(pc, desc(rot_abs)) |>
  group_by(pc) |>
  slice_max(rot_abs, n = 5) |>
  mutate(variable = reorder_within(variable, by = rot_abs, within = pc)) |>
  ggplot(aes(x = rot_abs, y = variable, fill = sign)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, .3))) +
  scale_y_reordered() +
  labs(x = "Contribution", y = NULL) +
  facet_wrap(vars(pc), scales = "free") +
  theme(axis.text.y = element_text(size = 9))

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20,
  ends = "first",
  type = "closed",
  length = grid::unit(3, "pt")
)

pca_outliers_max <- pca_rot |>
  select(common_name, PC1:PC3) |>
  pivot_longer(-common_name) |>
  group_by(name) |>
  slice_max(order_by = value, n = 3) |>
  ungroup() |>
  distinct(common_name)

pca_outliers_min <- pca_rot |>
  select(common_name, PC1:PC3) |>
  pivot_longer(-common_name) |>
  group_by(name) |>
  slice_min(order_by = value, n = 3) |>
  ungroup() |>
  distinct(common_name)

pca_outliers <- bind_rows(pca_outliers_max, pca_outliers_min) |>
  left_join(pca_rot) |>
  distinct()

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

pca_rot |>
  ggplot(aes(PC1, PC3)) +
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

pca_rot |>
  ggplot(aes(PC2, PC3)) +
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

pca_rot

team_pca <- pca_fit |>
  augment(fbref_data) |>
  select(-.rownames) |>
  rename_with(~ str_remove(.x, ".fitted")) |>
  mutate(id = str_c(squad, season_end_year, sep = "\n")) |>
  select(comp, season_end_year, squad, id, PC1:PC5)

team_pca |>
  ggplot(aes(PC1, PC2, label = id)) +
  geom_point(aes(color = comp)) +
  geom_label_repel(
    data = team_pca |>
      filter(percent_rank(abs(PC1)) > .99 | percent_rank(abs(PC2)) > .99)
  )

team_pca |>
  ggplot(aes(PC2, PC3, label = id)) +
  geom_point(aes(color = comp)) +
  geom_label_repel(
    data = team_pca |>
      filter(percent_rank(abs(PC2)) > .99 | percent_rank(abs(PC3)) > .99)
  )

team_pca |>
  ggplot(aes(PC1, PC3, label = id)) +
  geom_point(aes(color = comp)) +
  geom_label_repel(
    data = team_pca |>
      filter(percent_rank(abs(PC1)) > .99 | percent_rank(abs(PC3)) > .99)
  )

team_pca |>
  ggplot(aes(PC1, PC4, label = id)) +
  geom_point(aes(color = comp)) +
  geom_label_repel(
    data = team_pca |>
      filter(percent_rank(abs(PC1)) > .99 | percent_rank(abs(PC4)) > .99)
  )

team_pca |>
  ggplot(aes(PC1, PC5, label = id)) +
  geom_point(aes(color = comp)) +
  geom_label_repel(
    data = team_pca |>
      filter(percent_rank(abs(PC1)) > .99 | percent_rank(abs(PC5)) > .99)
  )

# Convert the ggplot object to a plotly object for 3D plotting
p <- plot_ly(
  team_pca,
  x = ~PC1,
  y = ~PC2,
  z = ~PC3,
  color = ~comp,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, symbol = "circle")
) %>%
  layout(
    title = "3D Scatter Plot",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )

# Display the 3D scatter plot
p

#graph pc1 by team over time compared to global distribution

pctiles <- c(.01, .25, .5, .75, .99)
pctiles_labs <- (pctiles * 100) |> str_pad(width = 2, side = "left", pad = "0")

pc1_pctiles <- team_pca |>
  reframe(
    value = quantile(PC1, probs = pctiles),
    pctile_name = pctiles_labs,
    .by = season_end_year
  ) |>
  pivot_wider(
    names_from = pctile_name,
    values_from = value,
    names_prefix = "p_"
  )

team_pca |>
  filter(squad == "Manchester City") |>
  ggplot(aes(x = season_end_year)) +
  geom_ribbon(data = pc1_pctiles, aes(ymin = p_01, ymax = p_99), alpha = .1) +
  geom_line(data = pc1_pctiles, aes(y = p_50)) +
  geom_line(aes(y = PC1), color = "lightblue", lwd = 2) +
  labs(y = "PC1")

#graph pc2 by team over time compared to global distribution

pctiles <- c(.01, .25, .5, .75, .99)
pctiles_labs <- (pctiles * 100) |> str_pad(width = 2, side = "left", pad = "0")

pc2_pctiles <- team_pca |>
  reframe(
    value = quantile(PC2, probs = pctiles),
    pctile_name = pctiles_labs,
    .by = season_end_year
  ) |>
  pivot_wider(
    names_from = pctile_name,
    values_from = value,
    names_prefix = "p_"
  )

pc2_pctiles

team_pca |>
  filter(squad == "Eibar") |>
  ggplot(aes(x = season_end_year)) +
  geom_ribbon(data = pc2_pctiles, aes(ymin = p_01, ymax = p_99), alpha = .1) +
  geom_line(data = pc2_pctiles, aes(y = p_50)) +
  geom_line(aes(y = PC2), color = "lightblue", lwd = 2) +
  labs(y = "PC2")

#pairwise scatter
my_fn <- function(data, mapping, ...) {
  # Using default ggplot density function

  p <- ggplot(data = data, mapping = mapping) +
    geom_density2d_filled() +
    #geom_point() +
    scale_fill_viridis_d() +
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
  "output/pairwise_scatter.png",
  plot = pairwise_scatter,
  width = 30,
  height = 30,
  dpi = 300
)

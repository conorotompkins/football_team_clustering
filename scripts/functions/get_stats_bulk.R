library(worldfootballR)
library(tidyverse)
library(janitor)

source("scripts/functions/clean_colname.R")

#standard
df_standard <- load_fb_big5_advanced_season_stats(
  season_end_year = c(2019:2023),
  stat_type = "standard",
  team_or_player = "team"
) |>
  as_tibble()

df_standard <- df_standard |>
  rename_with(clean_colname) |>
  clean_names() |>
  select(
    -c(
      starts_with("mp"),
      starts_with("starts"),
      starts_with("mins"),
      min_playing,
      prg_p_progression,
      ends_with("per"),
      url
    )
  ) |>
  #glimpse() #|>
  rename(
    age_avg = age,
    possession_pct = poss,
    goals = gls,
    assists = ast,
    goals_plus_assists = g_a,
    goals_np = g_minus_pk,
    pk_made = pk,
    sh_pk = p_katt,
    cards_yellow = crd_y,
    cards_red = crd_r,
    xg = x_g_expected,
    xg_np = npx_g_expected,
    xassisted_goals = x_ag_expected,
    xg_plus_xa_np = `npx_g_x_ag_expected`,
    progressive_carries = prg_c_progression
  ) |>
  #glimpse()
  pivot_wider(
    id_cols = c(season_end_year, squad, comp),
    names_from = team_or_opponent,
    values_from = 5:20
  ) |>
  select(
    -c(
      num_players_opponent,
      age_avg_opponent
    )
  )

glimpse(df_standard)

df_standard |>
  pivot_longer(
    cols = -c(season_end_year, comp, squad),
    values_transform = as.character
  ) |>
  summarize(pct_na = mean(is.na(value)), .by = c(season_end_year, comp)) |>
  arrange(desc(pct_na))

df_standard |>
  pivot_longer(
    cols = -c(season_end_year, comp, squad),
    values_transform = as.character
  ) |>
  summarize(
    pct_na = mean(is.na(value)),
    .by = c(season_end_year, comp, name)
  ) |>
  arrange(desc(pct_na))

#shooting
df_shooting <- load_fb_big5_advanced_season_stats(
  season_end_year = c(2019:2023),
  stat_type = "shooting",
  team_or_player = "team"
) |>
  as_tibble()

glimpse(df_shooting)

df_shooting <- df_shooting |>
  rename_with(clean_colname) |>
  clean_names() |>
  #glimpse() |>
  select(
    -c(
      contains("per_90"),
      ends_with("per"),
      num_players,
      pk_standard,
      x_g_expected,
      p_katt_standard,
      npx_g_expected,
      g_minus_x_g_expected,
      url
    )
  ) |>
  #glimpse() #|>
  rename(
    sot_standard = so_t_standard,
    sot_pct_standard = so_t_percent_standard,
    g_per_sot_standard = g_per_so_t_standard,
    avg_sh_dist = dist_standard,
    sh_fk = fk_standard,
    xg_per_sh = npx_g_per_sh_expected,
    g_minus_xg_np = np_g_minus_x_g_expected
  ) |>
  #glimpse()
  pivot_wider(
    id_cols = c(season_end_year, squad, comp),
    names_from = team_or_opponent,
    values_from = 5:14
  )

df_shooting |>
  pivot_longer(
    cols = -c(season_end_year, comp, squad),
    values_transform = as.character
  ) |>
  summarize(pct_na = mean(is.na(value)), .by = c(season_end_year, comp)) |>
  arrange(desc(pct_na))

df_shooting |>
  pivot_longer(
    cols = -c(season_end_year, comp, squad),
    values_transform = as.character
  ) |>
  summarize(
    pct_na = mean(is.na(value)),
    .by = c(season_end_year, comp, name)
  ) |>
  arrange(desc(pct_na))


fbref_data <- list(
  df_standard,
  df_shooting
) |>
  reduce(left_join, by = c("squad", "comp", "season_end_year"))

glimpse(fbref_data)

write_csv(fbref_data, "input/cleaned/fbref_data_cleaned.csv")

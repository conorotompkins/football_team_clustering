library(worldfootballR)
library(tidyverse)
library(janitor)

source("scripts/functions/clean_colname.R")

####standard
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

####shooting
df_shooting <- load_fb_big5_advanced_season_stats(
  season_end_year = c(2019:2023),
  stat_type = "shooting",
  team_or_player = "team"
) |>
  as_tibble()

df_shooting <- df_shooting |>
  rename_with(clean_colname) |>
  clean_names() |>
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
  rename(
    sot_standard = so_t_standard,
    sot_pct_standard = so_t_percent_standard,
    g_per_sot_standard = g_per_so_t_standard,
    avg_sh_dist = dist_standard,
    sh_fk = fk_standard,
    xg_per_sh = npx_g_per_sh_expected,
    g_minus_xg_np = np_g_minus_x_g_expected
  ) |>
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

#passing
df_passing <- load_fb_big5_advanced_season_stats(
  season_end_year = c(2019:2023),
  stat_type = "passing",
  team_or_player = "team"
) |>
  as_tibble()

df_passing <- df_passing |>
  rename_with(clean_colname) |>
  clean_names() |>
  select(
    -c(
      num_players,
      starts_with("mins"),
      x_a_expected,
      a_minus_x_ag_expected,
      url
    )
  ) |>
  rename_with(
    ~ str_c("pass_", .x),
    .cols = -c(comp, season_end_year, squad, team_or_opponent)
  ) |>
  select(
    -c(
      pass_cmp_total,
      pass_cmp_short,
      pass_cmp_medium,
      pass_cmp_long,
      pass_ast
    )
  ) |>
  rename(
    pass_xassisted_gls = pass_x_ag,
    pass_xassists = pass_x_a,
    pass_a_minus_xassisted_gls = pass_a_minus_x_ag,
    pass_key = pass_kp,
    pass_enter_final_third = pass_final_third,
    pass_enter_penalty_area = pass_ppa,
    pass_enter_penalty_area_cross = pass_crs_pa,
    pass_progressive = pass_prg_p
  ) |>
  pivot_wider(
    id_cols = c(season_end_year, squad, comp),
    names_from = team_or_opponent,
    values_from = 5:22
  )

####defense
df_defense <- load_fb_big5_advanced_season_stats(
  season_end_year = c(2019:2023),
  stat_type = "defense",
  team_or_player = "team"
) |>
  as_tibble()

df_defense <- df_defense |>
  rename_with(clean_colname) |>
  clean_names() |>
  glimpse() |>
  select(
    -c(
      num_players,
      starts_with("mins"),
      tkl_challenges,
      lost_challenges,
      blocks_blocks,
      tkl_int,
      url
    )
  ) |>
  rename(
    tackles = tkl_tackles,
    tackles_won_ball = tkl_w_tackles,
    tackles_def_3rd = def_3rd_tackles,
    tackles_mid_3rd = mid_3rd_tackles,
    tackles_att_3rd = att_3rd_tackles,
    dribbles_challenged = att_challenges,
    dribbles_challenged_success_pct = tkl_percent_challenges,
    blocked_shots = sh_blocks,
    blocked_passes = pass_blocks,
    interceptions = int,
    clearances = clr,
    error_sh_against = err
  ) |>
  glimpse() |>
  rename_with(
    ~ str_c("defense_", .x),
    .cols = -c(comp, season_end_year, squad, team_or_opponent)
  ) |>
  glimpse() |>
  pivot_wider(
    id_cols = c(season_end_year, squad, comp),
    names_from = team_or_opponent,
    values_from = 5:16
  )


####combine

fbref_data <- list(
  df_standard,
  df_shooting,
  df_passing,
  df_defense
) |>
  reduce(left_join, by = c("squad", "comp", "season_end_year"))

glimpse(fbref_data)

write_csv(fbref_data, "input/cleaned/fbref_data_cleaned.csv")

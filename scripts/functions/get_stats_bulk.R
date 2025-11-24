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

####pass types
df_pass_types <- load_fb_big5_advanced_season_stats(
  season_end_year = c(2019:2023),
  stat_type = "passing_types",
  team_or_player = "team"
) |>
  as_tibble()

df_pass_types <- df_pass_types |>
  rename_with(clean_colname) |>
  clean_names() |>
  select(
    -c(
      num_players,
      starts_with("mins"),
      url
    )
  ) |>
  rename(
    attempted = att,
    type_free_kick = fk_pass,
    type_throughball = tb_pass,
    type_switch = sw_pass,
    type_cross = crs_pass,
    type_throw_in = ti_pass,
    type_corner = ck_pass,
    type_corner_in = in_corner,
    type_corner_out = out_corner,
    type_corner_straight = str_corner,
    completed = cmp_outcomes,
    offsides = off_outcomes,
    blocked_by_opp = blocks_outcomes
  ) |>
  rename_with(
    ~ str_c("pass_types_", .x),
    .cols = -c(comp, season_end_year, squad, team_or_opponent)
  ) |>
  pivot_wider(
    id_cols = c(season_end_year, squad, comp),
    names_from = team_or_opponent,
    values_from = 5:19
  )

glimpse(df_pass_types)

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
  rename_with(
    ~ str_c("defense_", .x),
    .cols = -c(comp, season_end_year, squad, team_or_opponent)
  ) |>
  pivot_wider(
    id_cols = c(season_end_year, squad, comp),
    names_from = team_or_opponent,
    values_from = 5:16
  )

####goalkeeping
df_goalkeeping <- load_fb_big5_advanced_season_stats(
  season_end_year = c(2019:2023),
  stat_type = "keepers",
  team_or_player = "team"
) |>
  as_tibble()

df_goalkeeping <- df_goalkeeping |>
  rename_with(clean_colname) |>
  clean_names() |>
  glimpse() |>
  select(
    -c(
      num_players,
      mp_playing,
      starts_playing,
      min_playing,
      mins_per_90,
      ga,
      ga90,
      w,
      d,
      l,
      cs,
      url
    )
  ) |>
  rename(
    clean_sheet_pct = cs_percent,
    sh_pk = p_katt_penalty,
    pk_against = pka_penalty,
    pk_against_saved = p_ksv_penalty,
    pk_against_missed = p_km_penalty,
    pk_save_pct = save_percent_penalty
  ) |>
  mutate(
    pk_save_pct = case_when(
      pk_against == 0 & pk_against_saved == 0 ~ 0,
      .default = pk_save_pct
    )
  ) |>
  rename_with(
    ~ str_c("goalkeeping", .x),
    .cols = -c(comp, season_end_year, squad, team_or_opponent)
  ) |>
  pivot_wider(
    id_cols = c(season_end_year, squad, comp),
    names_from = team_or_opponent,
    values_from = 5:13
  )

glimpse(df_goalkeeping)

df_goalkeeping |>
  pivot_longer(
    cols = -c(season_end_year, comp, squad),
    values_transform = as.character
  ) |>
  summarize(pct_na = mean(is.na(value)), .by = c(season_end_year, comp)) |>
  arrange(desc(pct_na))

df_goalkeeping |>
  pivot_longer(
    cols = -c(season_end_year, comp, squad),
    values_transform = as.character
  ) |>
  summarize(
    pct_na = mean(is.na(value)),
    .by = c(season_end_year, comp, name)
  ) |>
  arrange(desc(pct_na))

####goalkeeping advanced
df_goalkeeping_adv <- load_fb_big5_advanced_season_stats(
  season_end_year = c(2019:2023),
  stat_type = "keepers_adv",
  team_or_player = "team"
) |>
  as_tibble()

df_goalkeeping_adv <- df_goalkeeping_adv |>
  rename_with(clean_colname) |>
  clean_names() |>
  select(
    -c(
      num_players,
      mins_per_90,
      ga_goals,
      pka_goals,
      number_opa_per_90_sweeper,
      stp_crosses,
      att_gk_passes,
      url
    )
  ) |>
  rename(
    ga_fk = fk_goals,
    ga_ck = ck_goals,
    ga_og = og_goals,
    ps_xg_against = p_sx_g_expected,
    ps_xg_per_sot = p_sx_g_per_so_t_expected,
    ps_xg_against_minus_goals_against = p_sx_g_per_minus_expected,
    gk_pass_launch_att = cmp_launched,
    gk_pass_launch_success_pct = cmp_percent_launched,
    gk_pass_att = att_passes,
    gk_pass_throw_att = thr_passes,
    gk_pass_launch_pct = launch_percent_passes,
    gk_pass_avg_length = avg_len_passes,
    gk_goal_kick_att = att_goal,
    gk_goal_kick_launch_pct = launch_percent_goal,
    gk_goal_kick_avg_length = avg_len_goal,
    gk_cross_against = opp_crosses,
    gk_cross_stop_pct = stp_percent_crosses,
    gk_defensive_actions_outside_penalty_area = number_opa_sweeper,
    gk_avg_distance_sweeper = avg_dist_sweeper
  ) |>
  rename_with(
    ~ str_c("goalkeeping_adv_", .x),
    .cols = -c(comp, season_end_year, squad, team_or_opponent)
  ) |>
  pivot_wider(
    id_cols = c(season_end_year, squad, comp),
    names_from = team_or_opponent,
    values_from = 5:25
  )

glimpse(df_goalkeeping_adv)

####gca
df_gca <- load_fb_big5_advanced_season_stats(
  season_end_year = c(2019:2023),
  stat_type = "gca",
  team_or_player = "team"
) |>
  as_tibble()

glimpse(df_gca)

df_gca <- df_gca |>
  rename_with(clean_colname) |>
  clean_names() |>
  select(
    -c(
      num_players,
      mins_per_90,
      sca_sca,
      sca90_sca,
      gca_gca,
      gca90_gca,
      url
    )
  ) |>
  rename(
    sca_from_pass_live = pass_live_sca,
    sca_from_pass_dead = pass_dead_sca,
    sca_from_takeon = to_sca,
    sca_from_shot = sh_sca,
    sca_from_foul = fld_sca,
    sca_from_defense = def_sca,
    gca_from_pass_live = pass_live_gca,
    gca_from_pass_dead = pass_dead_gca,
    gca_from_takeon = to_gca,
    gca_from_shot = sh_gca,
    gca_from_foul = fld_gca,
    gca_from_defense = def_gca
  ) |>
  pivot_wider(
    id_cols = c(season_end_year, squad, comp),
    names_from = team_or_opponent,
    values_from = 5:16
  )

####misc
df_misc <- load_fb_big5_advanced_season_stats(
  season_end_year = c(2019:2023),
  stat_type = "misc",
  team_or_player = "team"
) |>
  as_tibble()

df_misc <- df_misc |>
  rename_with(clean_colname) |>
  clean_names() |>
  select(
    -c(
      num_players,
      mins_per_90,
      url,
      crd_y,
      crd_r,
      x2crd_y,
      crs,
      int,
      tkl_w,
      p_kwon,
      p_kcon,
      won_percent_aerial
    )
  ) |>
  rename(
    fouls_committed = fls,
    fouls_drawn = fld,
    offsides = off
  ) |>
  rename_with(
    ~ str_c("misc_", .x),
    .cols = -c(comp, season_end_year, squad, team_or_opponent)
  ) |>
  pivot_wider(
    id_cols = c(season_end_year, squad, comp),
    names_from = team_or_opponent,
    values_from = 5:11
  )

glimpse(df_misc)

####combine
fbref_data <- list(
  df_standard,
  df_shooting,
  df_passing,
  df_pass_types,
  df_defense,
  df_goalkeeping,
  df_goalkeeping_adv,
  df_gca,
  df_misc
) |>
  reduce(left_join, by = c("squad", "comp", "season_end_year"))

glimpse(fbref_data)

fbref_data |>
  pivot_longer(
    cols = -c(season_end_year, comp, squad),
    values_transform = as.character
  ) |>
  summarize(pct_na = mean(is.na(value)), .by = c(season_end_year, comp)) |>
  filter(pct_na > 0) |>
  nrow() ==
  0

write_csv(fbref_data, "input/cleaned/fbref_data_cleaned.csv")

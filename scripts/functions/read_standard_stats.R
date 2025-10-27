read_standard_stats <- function(files) {
  fb_df <- files |>
    map(read_tsv, skip = 1) |>
    list_rbind()

  df_1 <- fb_df |>
    select(1:4) |>
    clean_names() |>
    rename(player_count = number_pl)

  df_2 <- fb_df |>
    select(5:8) |>
    clean_names()

  df_3 <- fb_df |>
    select(9:16) |>
    rename_with(clean_colname)

  df_4 <- fb_df |>
    select(17:20) |>
    rename_with(clean_colname)

  df_5 <- fb_df |>
    select(21:22) |>
    rename_with(clean_colname)

  list(df_1, df_2, df_3, df_4, df_5) |>
    list_cbind() |>
    rename(
      possession_pct = poss,
      nineties = x90s,
      goals = Gls,
      assists = Ast,
      goals_plus_assists = `G+A`,
      goals_np = `G-PK`,
      pk_made = PK,
      pk_attempted = PKatt,
      cards_yellow = CrdY,
      cards_red = CrdR,
      xg = xG,
      xg_np = npxG,
      xa = xAG,
      xg_plus_xa_np = `npxG+xAG`,
      progressive_carries = PrgC,
      progressive_passes = PrgP
    )
}

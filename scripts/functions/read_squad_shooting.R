read_squad_shooting <- function(files) {
  main_df <- files |>
    read_tsv(skip = 1)

  df_1 <- main_df |>
    select(1) |>
    rename_with(clean_colname) |>
    clean_names()

  df_2 <- main_df |>
    select(4:15) |>
    rename_with(clean_colname) |>
    clean_names()

  df_3 <- main_df |>
    select(16:20) |>
    select(c(`npxG`, `npxG/Sh`, `np:G-xG`)) |>
    rename_with(~ str_replace(.x, "npxG", "np_xg")) |>
    rename_with(~ str_replace(.x, "\\/Sh", "_per_shot")) |>
    rename(np_g_minus_xg = `np:G-xG`)

  combined <- list(df_1, df_2, df_3) |>
    list_cbind()

  glimpse(combined)

  combined |>
    select(
      squad,
      sh,
      so_t,
      so_t_percent,
      g_sh,
      g_so_t,
      dist,
      fk,
      #pk,
      #np_xg,
      np_xg_per_shot,
      np_g_minus_xg
    ) |>
    rename(
      shots = sh,
      sot = so_t,
      sot_pct = so_t_percent,
      g_per_shot = g_sh,
      g_per_sot = g_so_t,
      shot_distance = dist,
      sh_fk = fk,
      #xg_np = np_xg,
      xg_per_shot_np = np_xg_per_shot,
      g_minus_xg_np = np_g_minus_xg
    )
}

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

  list(df_1, df_2, df_3) |>
    list_cbind()
}

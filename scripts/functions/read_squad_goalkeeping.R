read_squad_goalkeeping <- function(files) {
  df_1 <- files |>
    read_tsv(skip = 1) |>
    select(1:2) |>
    rename_with(clean_colname) |>
    clean_names() |>
    rename(goalkeeper_count = number_pl)

  df_2 <- files |>
    read_tsv(skip = 1) |>
    select(3:6) |>
    clean_names()

  df_3 <- files |>
    read_tsv(skip = 1) |>
    select(7:16) |>
    rename_with(clean_colname) |>
    clean_names()

  df_4 <- files |>
    read_tsv(skip = 1) |>
    select(17:21) |>
    rename_with(clean_colname) |>
    rename(PKsave_percent = `Save%`) |>
    rename_with(~ str_replace(.x, "PK", "PK_")) |>
    clean_names()

  combined <- list(df_1, df_2, df_3, df_4) |>
    list_cbind()

  combined |>
    select(
      squad,
      goalkeeper_count,
      ga,
      so_ta,
      save_percent,
      cs_percent,
      pk_a,
      pk_save_percent
    ) |>
    rename(
      goals_a = ga,
      sot_a = so_ta,
      save_pct = save_percent,
      clean_sheet_pct = cs_percent,
      pk_save_pct = pk_save_percent
    )
}

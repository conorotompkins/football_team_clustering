read_squad_goalkeeping <- function(files) {
  df_1 <- files |>
    read_tsv(skip = 1) |>
    select(1:2) |>
    rename_with(clean_colname) |>
    clean_names() |>
    rename(goalkeeper_count = number_pl)

  df_2 <- files |>
    read_tsv(skip = 1) |>
    select(3:6)

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

  list(df_1, df_2, df_3, df_4) |>
    list_cbind()
}

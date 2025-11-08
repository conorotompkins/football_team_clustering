read_squad_passing <- function(files) {
  main_file <- read_tsv(files, skip = 1)

  df1 <- main_file |>
    select(1) |>
    rename_with(clean_colname) |>
    clean_names()

  df2 <- main_file |>
    select(4:8) |>
    rename_with(clean_colname) |>
    clean_names() |>
    rename_with(~ str_c("pass_", .x, "_total"))

  df3 <- main_file |>
    select(9:11) |>
    rename_with(clean_colname) |>
    clean_names() |>
    rename_with(~ str_c("pass_", .x, "_short"))

  df4 <- main_file |>
    select(12:14) |>
    rename_with(clean_colname) |>
    clean_names() |>
    rename_with(~ str_c("pass_", .x, "_medium"))

  df5 <- main_file |>
    select(15:17) |>
    rename_with(clean_colname) |>
    clean_names() |>
    rename_with(~ str_c("pass_", .x, "_long"))

  df6 <- main_file |>
    select(18:26) |>
    rename_with(clean_colname) |>
    clean_names() |>
    rename_with(~ str_c("pass_", .x)) |>
    rename(
      pass_xg_assisted = pass_x_ag,
      pass_x_assist = pass_x_a,
      pass_assists_minus_x_a = pass_a_x_ag,
      pass_key = pass_kp,
      pass_final_third_entry = pass_x1_3,
      pass_penalty_area_entry = pass_ppa,
      pass_cross_into_penalty_area = pass_crs_pa,
      pass_progressive = pass_prg_p
    )

  list(df1, df2, df3, df4, df5, df6) |>
    list_cbind()
}

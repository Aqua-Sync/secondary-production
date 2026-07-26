make_summary_table <- function(df, center = ".epred", lower = ".lower", upper = ".upper",
                               center_interval = "median_cri", digits = 1) {
  df %>%
    mutate(
      .center_val = round(.data[[center]], digits),
      .lower_val = round(.data[[lower]], digits),
      .upper_val = round(.data[[upper]], digits),
      center_interval = paste0(.center_val, " (", .lower_val, " to ", .upper_val, ")")) %>% 
    select(-.center_val, -.lower_val, -.upper_val)
}
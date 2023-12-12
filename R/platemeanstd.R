#' Calculating the Mean and standard deviation between replicates
#'
#' @param df A dataframe.
#' @returns A dataframe.
#' @export

platemeanstd <- function(df){
  dfstats <- df |>
    pivot_longer(!time,names_to="Well",values_to="OD600")|>
    mutate(Repgroup = str_extract(Well, "[aA-zZ]{1,1}"))|>
    group_by(Repgroup, time)|>
    summarize(Mean = mean(OD600), Std = sd(OD600))|>
    left_join(idkey, by = "Repgroup")
}

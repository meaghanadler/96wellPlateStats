#' Removes a bad replicate, then calculates the Mean and standard deviation between replicates
#'
#' @param df A dataframe.
#' @param well A column header
#' @returns A dataframe.
#' @export

badwell<- function(df, well){
  dfgoodwellz <- df |>
    select(-c({{well}}))|>
    pivot_longer(!time,names_to="Well",values_to="OD600")|>
    mutate(Repgroup = str_extract(Well, "[aA-zZ]{1,1}"))|>
    group_by(Repgroup, time)|>
    summarize(Mean = mean(OD600), Std = sd(OD600))|>
    left_join(idkey, by = "Repgroup")
}

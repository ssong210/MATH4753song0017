#
#' @title myddt: Plot and Dataframe by SPECIES
#'
#' @description Creates a plot, csv file, and a list terminal output for ddt
#' data filtered by species.
#'
#' @usage myddt(df, SPECIES)
#'
#' @param df data frame, should hold the contents of DDT.csv
#' @param SPECIES only entries of this species is used in the plot
#'
#' @importFrom dplyr '%>%' filter
#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth ggtitle
#' @importFrom utils write.csv
#'
#' @details This function is made specifically for the DDT.csv data set,
#' and will print a LENGTH vs WEIGHT plot of the data points of the specified
#' SPECIES, colored by RIVER and with a quadratic curve of best fit. This
#' function also creates a .csv file named "LvsWforSPECIES.csv" with SPECIES
#' substituted with the actual SPECIES argument used for the function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ddt=read.csv("DDT.CSV")
#' myddt(df=ddt, SPECIES="CCATFISH")
#' }
#'
myddt = function(df,SPECIES){

  cond = df$SPECIES==SPECIES # Creates the condition that the data entry be of the given species
  df1 <- df %>% filter({{cond}}) # Filters data frame for the entries that match condition

  g <- ggplot(df1, aes_string(x="WEIGHT",y="LENGTH")) + # Creates LENGTH vs WEIGHT plot
    geom_point(aes_string(color = "RIVER" )) + # Colors the point by RIVER variable
    geom_smooth(formula = y~x +I(x^2), method = "lm") + # Adds quadratic curve to plot
    ggtitle("Stephen Song") # Adds title with my name
  print(g) # Prints the plot

  write.csv(df1,file=paste("LvsWfor",SPECIES,".csv",sep=""),row.names=FALSE) # Writes csv file

  river_freq = table(df$RIVER)/length(df$RIVER) # Creates table of RIVER relative frequencies

  list("DDT Original Dataframe" = df, "DDT Subset Dataframe" = df1,
       "RIVER Relative Freqs" = river_freq) # Prints named list of dataframes and table

}

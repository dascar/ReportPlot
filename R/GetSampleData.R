#' Creating Sample Data for Plotting
#'
#' @return A data.frame with the appropriate shape for the plotting function ReportPlot.
#' @examples
#' data <- GetSampleData()
GetSampleData <- function(){

  out <- data.frame(
    year   = c(rep(2016, 12), rep(2017, 12), rep(2018, 12), rep(2019, 12)),
    month  = c(1:12, 1:12, 1:12, 1:12),
    sold = c(c(900, 700, 1000, 1200, 1300, 1400, 1420, 1100, 1300, 1000, 1200, 1300),
             c(700, 600, 1200, 1200, 1200, 1400, 1420, 1200, 1400, 1100, 1200, 1400),
             c(980, 900, 1300, 1200, 1500, 1400, 1420, 1100, 1200, 1100, 1300, 1300),
             c(900, 800, 1400, 1200, 1300, 1400, 1420, 1300, 1300, 1200, 1200, 1300)),
    cpm  = c(c(650, 800, 1300, 1420, 1230, 1320, 1300, 1250, 1320, 1450, 1100, 1400),
             c(750, 880, 1200, 1420, 1110, 1300, 1200, 1150, 1310, 1150, 1200, 1500),
             c(850, 900, 1100, 1320, 1200, 1300, 1100, 1250, 1320, 1450, 1100, 1400),
             c(850, 900, 1100, 1320, 1200, 1300, 1100, 1250, 1320, 1450, 1100, 1400))-500)

  # Datamanagement
  out$month <- as.character(factor(out$month,
                                 levels = 1:12,
                                 labels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))


  return(out)
}

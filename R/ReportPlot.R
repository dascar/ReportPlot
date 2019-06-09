#' Generating the Plot
#'
#' The function was developed for a shiny report of a medical device manufacturer. \href{https://www.data-science-archtect.de}{More about the project and developer: Data-Science-Architect}.
#'
#' @param x A data.frame with the shape of: See GetSampleData
#' @param main Title of the plot.
#' @param ylab Label for the y-Axis.
#' @param connection Can be either spline or linear
#' @param legend_line Legend for curve/line in the plot
#' @param legend_bar Legeng for bars in the plot
#' @return Returns the Plot.
#' @examples
#' data <- GetSampleData()
#' ReportPlot(data)
ReportPlot <- function(x,
                       main = "Productname",
                       ylab = "cpm (Moving Average)",
                       connection = "spline",
                       legend_line = "Compliants per Million sold; \nMoving Average on 3 Month base",
                       legend_bar = "Quantity Sold"){


  # TODO: Those two should be flexible parameters and passed as arguments
  nMonths = 12
  nYears = 4

  backupPar <- par(no.readonly = TRUE)


  # Calculate Moving Average for cpm
  cpmMA <- stats::filter(x$cpm,
                         filter = c(0.3333,0.3333,0.3333),
                         sides = 2)[(nrow(x)-nMonths+1):(nrow(x)-1)]

  aggSold <- aggregate(sold ~ year, data = x, FUN = mean)
  aggcpm  <- aggregate(cpm  ~ year, data = x, FUN = mean)

  # Preparing Colors
  colors <- brewer.pal(7, "Blues")
  combinedValues <- c(aggSold[1:nYears,2],x[(nrow(x)-11):nrow(x),3])

  # Global Graphical Parameters
  par(las = 1,
      cex = 0.7,
      mar = c(10,5,6,5),
      xpd = FALSE)

  plot(combinedValues,
       bty = "n",
       axes = FALSE,
       xlab = "",
       ylab = ylab,
       type = "n")
  title(main = main,
        cex = 3)

  for(i in 1:(nYears - 1)){
    rect(xleft  = i - 0.3,
         xright = i + 0.3,
         ybottom= 50,
         ytop   = combinedValues[i],
         border = 0,
         col    = colors[3])
  }

  rect(xleft  = nYears - 0.3,
       xright = nYears + 0.3,
       ybottom= 50,
       ytop   = combinedValues[nYears],
       border = 0,
       col    = colors[4])


  for(i in (nYears+1):(nYears+1 + nMonths+1)){
    rect(xleft  = i - 0.3,
         xright = i + 0.3,
         ybottom= 50,
         ytop   = combinedValues[i],
         border = 0,
         col    = colors[2])
  }

  axis(side = 1,
       at = c(0:(nYears+1 + nMonths+1)),
       labels = c("", "2013", "2014", "2015", "2016", x$month[(nrow(x)-(nMonths-1)):nrow(x)], "", ""),
       las = 2)
  axis(side = 4)

  lines(x = c(nYears+0.5,nYears+0.5), y = c(0,max(combinedValues)), lty = "dashed")
  mtext("Previous Months", line = 6, side = 1, cex = 0.8, at = nMonths)
  mtext("Previous Years", line = 6, side = 1, cex = 0.8, at = 2.5)



  ##### cpm Moving Average
  valuesForcpm <- c(aggcpm$cpm[(nrow(aggcpm) - nYears):nrow(aggcpm)],cpmMA,x$cpm[nrow(x)])

  par(new=TRUE)
  plot(1:(nYears+nMonths),
       valuesForcpm,
       type="n",
       col="blue",
       xaxt="n",
       yaxt="n",
       xlab="",
       ylab="",
       bty = "n")
  axis(side = 2)

  points(1:nYears,
         aggcpm$cpm[(nrow(aggcpm) - nYears):nrow(aggcpm)],
         col = "black",
         pch = 19,
         cex = 1.5)

  points(x = (nYears+1):(nYears + nMonths -1),
         y = cpmMA,
         col = "black",
         pch = 1)

  points(x = nYears + nMonths,
         y = x$cpm[nrow(x)])


  if (connection == "spline"){

      #### Cubic Spline Connection
      splines_cpmMA <- splinefun(x = (nYears+1):(nYears + nMonths),
                                 y = c(cpmMA,x$cpm[nrow(x)]))
      curve(splines_cpmMA,
            add = TRUE,
            from = (nYears+1),
            to = (nYears + nMonths- 1),
            col = colors[4])
      curve(splines_cpmMA,
            add = TRUE,
            lty = "dashed",
            from = (nYears + nMonths-1),
            to = (nYears + nMonths),
            col = "black")

  } else if (connection == "linear"){
    linearSplines <- approxfun(x = (nYears+1):(nYears + nMonths),
                             y = c(cpmMA,x$cpm[nrow(x)]))

    curve(linearSplines,
        add = TRUE,
        from = (nYears+1),
        to = (nYears + nMonths- 1),
        col = colors[4])
    curve(linearSplines,
        add = TRUE,
        lty = "dashed",
        from = (nYears + nMonths-1),
        to = (nYears + nMonths),
        col = "black")
    }

  par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
  plot(c(0,1), c(0,1), type='n', bty='n', xaxt='n', yaxt='n')

  legend("bottomleft",
         bty = "n",
         lty = 1,
         legend = c(legend_line),
         col = colors[4],
         cex = 0.7)

  par(fig=c(0, 1, 0, 1), oma=c(0, 15, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
  plot(c(0,1), c(0,1), type='n', bty='n', xaxt='n', yaxt='n')

  legend("bottomleft",
         bty = "n",
         fill = colors[2],
         border = colors[2],
         legend = c(legend_bar),
         col = colors[4],
         cex = 0.7)

  par(backupPar)
}

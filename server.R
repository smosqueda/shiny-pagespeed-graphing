library(jsonlite)
library(gtx)
#library(survival)
#library(DT)
#require(cowplot)
require(gridExtra)
library(ggplot2)
library(xkcd)

shinyServer(function(input, output) {

  
  incrementByOne <- function(n) {
    #print(n+1)
    return(n + 1)
  }
  
  output$plotDataTest <- renderPlot({
    
    return(NULL)
    
  })
  
  dataInput <- reactive({
     input$days 
  })
  
  output$plotData <- renderPlot({  
    days <- dataInput()
    d_url <- paste("http://127.0.0.1:8084/svc/dashboards/pagespeed/v1/pageaverage?numDays=",days,sep="")
    d.raw.data <- readLines(d_url,warn = "F")
    
    ##[{"avg":52.0333,"stdDev":1.3287421,"date":"2015-12-15"},
    
    data <- fromJSON(d.raw.data)

    #str(data)
    x <- data$avg
    xx <- data$stdDev
    xMax <- max(data$stdDev)
    print(paste("xMax",xMax))
    par(mfrow=c(3,1),mar=c(5,5,4,4), xpd=TRUE)
    plot(x, xlim=c(days, 1), ylim=c(1,100), pch=4, col="blue",col.axis="black",lty=2,font.lab=477,lwd=4, 
         xaxt="n",yaxt="n", xlab="Days", ylab="Score 1:100", font = 18, type="n", tck=1, cex.axis = 1.5, main="Page Speed Score")
    
    colors <- contrasting.rainbow(22)
    
    lines(x, NULL, type = "b", pch = 22, col = colors[1], lty = 2, lwd=2)
    lines(xx, NULL, type = "b", pch = 21, col = colors[2], lty = 2, lwd=2)
    at <- rev(1:days)
    axis(1, at = at, labels = at, col.axis = "blue", las = 1, cex.axis=1.5, tck=1, col.ticks="light gray")
    
    at <- c(0,10,20,30,40,50,60,70,80,90,100)
    l <- seq(0, 100, by = 10)
    axis(2, at = at, labels = l, col.axis = "black", las = 2, tck=1, col.ticks="light gray", cex.axis = 1.5)
    legend("topright", legend = c("average","standard-deviation"), cex=1.0, bty="n", col=colors, pch=19, lwd=c(2.5,2.5), inset=c(0,0))
  #})

  #output$dailyRecs <- renderPlot({
    days <- dataInput()
    p_url <- "http://127.0.0.1:8084/svc/dashboards/pagespeed/v1/pagestats"
    d.raw.data <- readLines(p_url,warn = "F")
    print(paste("days is",days))
    #[{"id":"8473","responseCode":200,"title":"Tampa Bay, Florida news | Tampa Bay Times/St. Pete Times","ruleGroups":{"SPEED":{"score":55}},
    # "pageStats":{"numberResources":225,"numberHosts":73,"totalRequestBytes":54902,"numberStaticResources":67,"htmlResponseBytes":169337,
    #  "textResponseBytes":3571,"cssResponseBytes":484714,"imageResponseBytes":931010,"javascriptResponseBytes":3395835,"otherResponseBytes":95389,
    #  "numberJsResources":73,"numberCssResources":11},"testTs":1450211408000},
    
    #     $ id          : chr  "8543" "8542" "8541" "8540" ...
    #     $ responseCode: int  200 200 200 200 200 200 200 200 200 200 ...
    #     $ title       : chr  "Tampa Bay, Florida news | Tampa Bay Times/St. Pete Times" "Tampa Bay, Florida news | Tampa Bay Times/St. Pete Times" "Tampa Bay, Florida news | Tampa Bay Times/St. Pete Times" "Tampa Bay, Florida news | Tampa Bay Times/St. Pete Times" ...
    #     $ ruleGroups  :'data.frame':	5895 obs. of  1 variable:
    #       ..$ SPEED:'data.frame':	5895 obs. of  1 variable:
    #       .. ..$ score: int  45 48 48 47 49 48 47 47 47 47 ...
    #     $ pageStats   :'data.frame':	5895 obs. of  12 variables:
    #       ..$ numberResources        : int  235 210 209 234 235 208 235 235 209 234 ...
    #     ..$ numberHosts            : int  79 59 58 74 76 58 72 71 59 71 ...
    #     ..$ totalRequestBytes      : int  47733 41448 41082 49297 49333 40618 48169 48230 42129 46586 ...
    #     ..$ numberStaticResources  : int  83 77 77 77 77 77 77 80 74 78 ...
    #     ..$ htmlResponseBytes      : int  183621 155729 150664 180597 182344 152366 182121 162260 153320 159309 ...
    #     ..$ textResponseBytes      : int  3588 3578 3515 3578 3514 3578 3578 3578 3578 3514 ...
    #     ..$ cssResponseBytes       : int  522513 522499 522501 522506 522519 522507 522506 522502 522508 522492 ...
    #     ..$ imageResponseBytes     : int  1450924 1264144 1241332 1322755 1254869 1227729 1267344 1281935 1303582 1305859 ...
    #     ..$ javascriptResponseBytes: int  3579385 3616186 3646354 3353058 3332578 3150981 3330317 3391699 3039507 3330820 ...
    #     ..$ otherResponseBytes     : int  250705 247194 247128 256884 258617 246603 257221 256169 246814 255438 ...
    #     ..$ numberJsResources      : int  87 73 73 75 75 71 75 78 70 75 ...
    #     ..$ numberCssResources     : int  15 15 15 15 15 15 15 15 15 15 ...
    print("GETTING NEXT PLOT")
   # par(oma=c(2.5,0,0,0),mgp=c(3,1,0), xpd=TRUE)
    stats <- fromJSON(d.raw.data)
    d_data <- stats[1:days,]
    #    daily[, c("daily$title", "daily$responseCode")]
    print("GETTING pageStats")
    str(d_data$pageStats)
    x1 <- d_data$pageStats$numberResources
    str(x1)
    xMax <- max(d_data$pageStats$numberResources)
    #str(xMax)
    topBytes <- seq(1,xMax, by = 15)
    #da_par <- par(mfrow=c(2,2),mar=c(5,5,4,4), xpd=TRUE)
    #par(da_par)
    plot(x1, xlim=c(days, 1), ylim=c(1,xMax), pch=4, col="blue",col.axis="black",lty=2,font.lab=477,lwd=5, 
         xaxt="n",yaxt="n", xlab="Days", ylab="Resource Count", font = 18, type="n", tck=1, main="Resource Counts")
    colors <- contrasting.rainbow(22)
    lines(x1, NULL, type = "b", pch = 19, col = colors[1], lty = 3, lwd=3)
    
    x2 <- d_data$pageStats$numberHosts
    lines(x2, NULL, type = "b", pch = 19, col = colors[2], lty = 3, lwd=3)
    
    x3 <- d_data$pageStats$numberStaticResources
    lines(x3, NULL, type = "b", pch = 19, col = colors[3], lty = 3, lwd=3)
    
    x4 <- d_data$pageStats$numberJsResources
    lines(x4, NULL, type = "b", pch = 19, col = colors[4], lty = 3, lwd=3)
    
    x5 <- d_data$pageStats$numberCssResources
    lines(x5, NULL, type = "b", pch = 19, col = colors[5], lty = 3, lwd=3)
    
    at <- rev(1:days)
    axis(1, at = at, labels = at, col.axis = "blue", las = 1, cex.axis=1.5, tck=1, col.ticks="light gray")
    
    l <- seq(1, xMax, by = 15)
    at <- length(l)
    axis(2, at = l, labels = l, col.axis = "black", las = 2, tck=1, col.ticks="light gray", cex.axis = 1.5)
    legend("topright", legend = c("Resources","Hosts","Static Resources","Js Resources","Css Resources"), cex=1.0, bty="n", col=colors, pch=19, lwd=c(2.5,2.5), inset=c(0,0))
    
    #par(da_par)
    
   #})
   
  ## SECOND PLOT:
    #par(oma=c(2.5,0,0,0),mgp=c(3,1,0), xpd=TRUE)
    b1 <- d_data$pageStats$javascriptResponseBytes
    #str(b1)
    xMax <- max(b1)
    #str(xMax)
    topBytes <- seq(1,xMax, by = 250000)
    #par(mfrow=c(1,1),mar=c(5,5,4,4), xpd=TRUE)
    plot(b1, xlim=c(days, 1), ylim=c(1,xMax), pch=4, col="blue",col.axis="black",lty=2,font.lab=477,lwd=4, 
          xaxt="n",yaxt="n", xlab="Days", ylab="Bytes", font = 18, type="n", tck=1, main="Resources Bytes")
    colors <- contrasting.rainbow(22)
    
    lines(b1, NULL, type = "b", pch = 19, col = colors[1], lty = 2, lwd=2)
    
    b2 <- d_data$pageStats$htmlResponseBytes
    lines(b2, NULL, type = "b", pch = 19, col = colors[2], lty = 2, lwd=2)
    
    b3 <- d_data$pageStats$totalRequestBytes
    lines(b3, NULL, type = "b", pch = 19, col = colors[3], lty = 2, lwd=2)
    
    b4 <- d_data$pageStats$textResponseBytes
    lines(b4, NULL, type = "b", pch = 19, col = colors[4], lty = 2, lwd=2)
    
    b5 <- d_data$pageStats$cssResponseBytes
    lines(b5, NULL, type = "b", pch = 19, col = colors[5], lty = 2, lwd=2)
    
    b6 <- d_data$pageStats$imageResponseBytes
    lines(b6, NULL, type = "b", pch = 19, col = colors[6], lty = 2, lwd=2)
    
    b7 <- d_data$pageStats$otherResponseBytes
    lines(b7, NULL, type = "b", pch = 19, col = colors[7], lty = 2, lwd=2)
    
    at <- rev(1:days)
    #hozontal
    axis(1, at = at, labels = at, col.axis = "blue", las = 1, cex.axis=1.5, tck=1, col.ticks="light gray")
    
    l <- seq(0, xMax, by = 250000)
    at <- length(l)
    #Vertical
    axis(2, at = l, labels = l, col.axis = "black", las = 2, tck=1, col.ticks="light gray", cex.axis = 1.5)
    legend("topright", legend = c("javascripts","html","total bytes", "text","css","images","other"), cex=1.0, bty="n", col=colors, pch=19, lwd=c(2.5,2.5), inset=c(0,0))
#     par(oma=c(2.5,0,0,0),mgp=c(3,1,0), xpd=TRUE)
    #par(mfrow=c(2, 1),mar=c(5,5,4,4), xpd=TRUE)
  })
  
#   output$somePieCharts = renderUI({
#     # Pie Chart with Percentages
#     slices <- c(10, 12, 4, 16, 8) 
#     lbls <- c("US", "UK", "Australia", "Germany", "France")
#     pct <- round(slices/sum(slices)*100)
#     lbls <- paste(lbls, pct) # add percents to labels 
#     lbls <- paste(lbls,"%",sep="") # ad % to labels 
#     pie(slices,labels = lbls, col=rainbow(length(lbls)),
#         main="Pie Chart of Countries")
#   })

  
#  output$ggPlotVersionExample <- renderPlot({
#     # create factors with value labels 
#     mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
#                           labels=c("3gears","4gears","5gears")) 
#     mtcars$am <- factor(mtcars$am,levels=c(0,1),
#                         labels=c("Automatic","Manual")) 
#     mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
#                          labels=c("4cyl","6cyl","8cyl")) 
#     str(mtcars)
#     # Kernel density plots for mpg
#     # grouped by number of gears (indicated by color)
#     qplot(mpg, data=mtcars, geom="density", fill=gear, alpha=I(.5), 
#           main="Distribution of Gas Milage", xlab="Miles Per Gallon", 
#           ylab="Density")
#     
#  })

  # GGPLOT
output$ggPlotVersion <- renderPlot({
  this_base <- "0003_xkcd-and-direct-labelling"
  days <- dataInput()
  real_days <- incrementByOne(days) # historical data
  usr_url <- paste("http://10.130.102.143:8084/svc/dashboards/pagespeed/v1/userGA?numDays=",real_days,sep="")
  usr.raw.data <- readLines(usr_url,warn = "F")
  u_stats <- fromJSON(usr.raw.data)
  days <- days - 1
  u_data <- u_stats[1:days,]
  str(u_data)
  dfs <- stack(u_data)
  #str(dfs)
  cols <- colnames(u_data)
  str(dfs$values)
  
  
  dat <- data.frame(dens = c(u_data$avgSessionDuration, u_data$bounceRate)
                    , lines = rep(c("Avg Duration", "Bounce Rate"))) #, each = 200000
  
  #Plot 2
  plot1 <- ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5) + ggtitle("Session Duration & Bounce Rate")
  theme(plot.title = element_text(color="#666666", face="bold", size=20))
  dat2 <- data.frame(dens = c(u_data$pageViews, u_data$users, u_data$newUsers, u_data$hits)
                     , lines = rep(c("Page Views", "Users", "New Users", "Hits"))) #, each = 200000
#   #Plot.
  plot2 <- ggplot(dat2, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5) + ggtitle("User Activity")
  theme(plot.title = element_text(color="#666666", face="bold", size=20))
  grid.arrange(plot1, plot2, ncol=1, nrow=2)
  
})
  

  
})

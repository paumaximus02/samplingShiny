library(shiny)

# Stable vars to use when adding one sample at a time
stats <- NULL
stats_offset <- 0

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  data <- reactive({
    rheavy <- function(n) return(rt(n,6))
    rheavyyy <- function(n) return(rt(n,2.25))
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   heavy = rheavy,
                   heavyyy = rheavyyy,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    stat <- switch(input$statistic,
                   mean = mean,
                   median = median,
                   stdev = sd,
                   var = var,
                   max = max,
                   min = min)
    # in addition to input$dist and input$n react to changes in...
    input$resample
    input$checkbox
    input$reps
    input$zoominbox
    input$normdensity
    
    dist(input$n) # draw n samples
  })
  
  # Reactive expression to update the list of stats when the distribution changes
  doReset <- reactive({
    rheavy <- function(n) return(rt(n,6))
    rheavyyy <- function(n) return(rt(n,2.25))
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   heavy = rheavy,
                   heavyyy = rheavyyy,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    stat <- switch(input$statistic,
                   mean = mean,
                   median = median,
                   stdev = sd,
                   var = var,
                   max = max,
                   min = min)
    
    # in addition to input$dist react to changes in...
    input$checkbox
    input$n
    input$reps
    input$zoominbox
    input$normdensity
    
    stats<<-NULL
    print("reset")
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    # Plot parameters...
    tcol="orange"      # fill colors
    acol="orangered"   # color for added samples
    tscale=2;          # label rescaling factor
    
    dist <- input$dist
    n <- input$n
    reps <- input$reps
    x<-data()
    doReset()
    rheavy <- function(n) return(rt(n,6))
    rheavyyy <- function(n) return(rt(n,2.25))
    stat <- switch(input$statistic,
                   mean = mean,
                   median = median,
                   stdev = sd,
                   var = var,
                   max = max,
                   min = min)
    
    # Add to list of sample stats or repeat sampling reps times depending on checkbox
    if (input$checkbox) {
      if (length(stats)==0) {stats_offset<<-input$resample}
      stats[input$resample-stats_offset+1]<<-stat(x)
    }
    else {
      stats<<-1:reps
      for (i in 1:reps) {
        stats[i] <<-stat(switch(dist,
                                norm = rnorm(n),
                                unif = runif(n),
                                heavy = rheavy(n),
                                heavyyy = rheavyyy(n),
                                lnorm = rlnorm(n),
                                exp = rexp(n),
                                rnorm(n)))
      }
    }
    
    # set plot range
    xmin = switch(dist, norm = -3.5, unif = 0, heavy = -4.5, heavyyy = -6, lnorm = 0, exp = 0, -3)
    xmax = switch(dist, norm =  3.5, unif = 1, heavy =  4.5, heavyyy =  6, lnorm = 4, exp = 4,  3)
    
    # do not plot outliers
    xrm<-x
    xrm[x>xmax]<-NA
    xrm[x<xmin]<-NA
    statskeep <- stats
    stats[stats>xmax]<<-NA
    stats[stats<xmin]<<-NA
    stats <- sort(stats) # sorting trims out any NA
    
    par(mfrow=c(3,1),mar=c(8,6,2,2)) 
    
    # plot true distribution
    x0 = seq(xmin,xmax,length.out=512);
    dheavy <- function(x) return(dt(x,6))
    dheavyyy <- function(x) return(dt(x,2.25))
    y0 = switch(dist,
                norm = dnorm(x0),
                unif = dunif(x0),
                heavy = dheavy(x0),
                heavyyy = dheavyyy(x0),
                lnorm = dlnorm(x0),
                exp = dexp(x0),
                dnorm(x0))
    #y0=y0/sum(y0);
    plot(x0,y0,type="l",lwd=0,col=NULL,main="Population",xlab="",ylab="Density",frame=F,cex.lab=tscale, cex.axis=tscale, cex.main=tscale, cex.sub=tscale) 
    polygon(c(xmin,x0,xmax),c(0,y0,0),col=tcol,border=NA)
    
    
    # plot typical sample
    hist(xrm, 
         breaks=seq(xmin,xmax,length.out=50),
         main="Typical Sample",
         warn.unused = FALSE,
         col=tcol,
         border=tcol,
         xlab="",
         cex.lab=tscale,
         cex.axis=tscale,
         cex.main=tscale,
         cex.sub=tscale)
    if (any(x<xmin)) {
      points(rep(xmin-0.1,sum(x<xmin)),rbeta(sum(x<xmin),2,2),lwd=2,col=tcol,cex=tscale)
    }
    if (any(x>xmax)) {
      points(rep(xmax+0.1,sum(x>xmax)),rbeta(sum(x>xmax),2,2),lwd=2,col=tcol,cex=tscale)
    }
    
    # plot list of sample stats with the latest sample highlighted and N(mu,sigma^2/n)
    breaks_mh=seq(xmin,xmax,length.out=100);
    if (input$zoominbox){
      df = mean(diff(breaks_mh))
      breaks_mh = seq(min(stats)-df,max(stats)+df,length.out=100)
    }
    if(input$statistic=="mean"){
    y0 = dnorm(x0,switch(dist,
                         norm = 0,
                         unif = 0.5,
                         heavy = 0,
                         heavyyy = 0,
                         lnorm = exp(1/2),
                         exp = 1,
                         0),
               switch(dist,
                      norm = 1/sqrt(n),
                      unif = 1/sqrt(12)/sqrt(n),
                      heavy = sqrt(3/2)/sqrt(n),
                      heavyyy = 3/sqrt(n),
                      lnorm = sqrt((exp(1)-1)*(exp(1)))/sqrt(n),
                      exp = 1/sqrt(n),
                      0))
    } else {
      y0 = dnorm(x0,mean(statskeep),sd(statskeep))
    }
    
    y0=y0/sum(y0)*length(stats)*mean(diff(breaks_mh))/mean(diff(x0))
    
    titleLabel <- switch(input$statistic,
                         mean = "Sample Means",
                         median = "Sample Medians",
                         stdev = "Sample Standard Deviations",
                         var = "Sample Variances",
                         max = "Sample Maximums",
                         min = "Sample Minimums")
    
    nh<-hist(stats,
             breaks=breaks_mh,
             main=titleLabel,
             warn.unused = FALSE,
             col=tcol,
             border=tcol,
             xlab="",
             cex.lab=tscale,
             cex.axis=tscale,
             cex.main=tscale,
             cex.sub=tscale)
    if (stat(x)>xmin && stat(x)<xmax) {
      hist(stat(x),
           breaks=breaks_mh,
           col=acol,
           border=acol,
           add=TRUE,
           ylim=c(0,max(y0,max(nh$counts))))
    }
  if (input$normdensity){
      points(x0,y0,type="l",lwd=2)
    }
    print(input$resample)
  },width=600,height=600)
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
  
})
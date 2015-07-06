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
    
    dist <- function(n,type){
      switch(type,
             norm = rnorm(n),
             unif = runif(n),
             heavy = rt(n,6),
             heavyyy = rt(n,2.25),
             lnorm = rlnorm(n),
             exp = rexp(n),
             rnorm(n))
    }
    
    # in addition to input$dist and input$n react to changes in...
    input$resample
    input$checkbox
    input$reps
    input$zoominbox
    input$normdensity
    input$statistic
    
    dist(input$n,input$dist)
  })
  
  # Reactive expression to update the list of stats when the distribution changes
  # (this seems unecessary)
  doReset <- reactive({
    # in addition to input$dist react to changes in...
    input$dist
    input$statistic
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
    
    #    dist <- input$dist
    n <- input$n
    reps <- input$reps
    x<-data()
    doReset()
    stat <- function(x,type){
      switch(type,
             mean = mean(x),
             median = median(x),
             stdev = sd(x),
             var = var(x),
             max = max(x),
             min = min(x)
      )
    }
    
    dist <- function(n,type){
      switch(type,
             norm = rnorm(n),
             unif = runif(n),
             heavy = rt(n,6),
             heavyyy = rt(n,2.25),
             lnorm = rlnorm(n),
             exp = rexp(n),
             rnorm(n)
      )
    }
    
    mu = switch(input$dist,
                norm = 0,
                unif = 0.5,
                heavy = 0,
                heavyyy = 0,
                lnorm = exp(1/2),
                exp = 1,
                0)
    
    sigma = switch(input$dist,                      
                   norm = 1,
                   unif = 1/sqrt(12),
                   heavy = sqrt(3/2),
                   heavyyy = 3,
                   lnorm = sqrt((exp(1)-1)*(exp(1))),
                   exp = 1,
                   1)
    
    # Add to list of sample stats or repeat sampling reps times depending on checkbox
    if (input$checkbox) {
      if (length(stats)==0) {stats_offset<<-input$resample}
      stats[input$resample-stats_offset+1]<<-stat(x,input$statistic)
    }
    else {
      stats<<-1:reps
      for (i in 1:reps) {
        stats[i] <<-stat(dist(n,input$dist),input$statistic)
      }
    }
    
    # set plot range
    xmin = switch(input$dist, norm = -3.5, unif = 0, heavy = -4.5, heavyyy = -6, lnorm = 0, exp = 0, -3)
    xmax = switch(input$dist, norm =  3.5, unif = 1, heavy =  4.5, heavyyy =  6, lnorm = 4, exp = 4,  3)
    
    # do not plot outliers
    xrm<-x
    xrm[x>xmax]<-NA
    xrm[x<xmin]<-NA
    statsTrim <- stats # possibly trim ends for plotting but compute everything with orig
    
    par(mfrow=c(3,1),mar=c(8,6,2,2)) 
    
    # plot true distribution
    x0 = seq(xmin,xmax,length.out=512);
    y0 = switch(input$dist,
                norm = dnorm(x0),
                unif = dunif(x0),
                heavy = dt(x0,6),
                heavyyy = dt(x0,2.25),
                lnorm = dlnorm(x0),
                exp = dexp(x0),
                dnorm(x0))
    #y0=y0/sum(y0);
    pmean <- mu
    psigma <- sigma
    titlestr <- bquote(bold(Population)~(list(mu==.(signif(mu,3)),~sigma==.(signif(sigma,3)))))
    #titlestr = paste("Population ( mean = ",signif(mu,3)," sigma = ",signif(sigma,3),")")
    plot(x0,y0,type="l",lwd=0,col=NULL,main=titlestr,xlab="",ylab="Density",frame=F,cex.lab=tscale, cex.axis=tscale, cex.main=tscale, cex.sub=tscale,ylim=c(0,1.05*max(y0))) 
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
    k = 10;
    bw = sd(stats)/k;
    
    qbound = switch(input$statistic,
                  mean = c(xmin,xmax),
                  median = c(xmin,xmax),
                  stdev = c(0,1.1*max(stats)),
                  var = c(0,1.1*max(stats)),
                  max = c(xmin,xmax),
                  min = c(xmin,xmax))
    qtrim = qbound

    if (input$zoominbox){
      logSpread = input$statistic=="var" | input$statistic=="stdev"
      if (logSpread&input$dist=="heavyyy")
      { 
        qtrim = c(max(0,min(statsTrim)),unname(quantile(statsTrim,p=.95)))
        qbound = c(max(0,min(statsTrim)),unname(quantile(statsTrim,p=.96)))
      }
      else
      {
        qtrim = unname(quantile(statsTrim,p=c(.01,.99)))
        qbound = unname(quantile(statsTrim,p=c(.005,.995)))
      }
    }
    
    statsTrim <- statsTrim[ statsTrim>qtrim[1] & statsTrim<qtrim[2] ]
    breaks_mh = seq(qbound[1],qbound[2],length=100)    
    x0 = seq(qbound[1],qbound[2],length=400)
    
    if(input$statistic=="mean"){
      y0 = dnorm(x0,mu,sigma/sqrt(n))
    } else {
      y0 = dnorm(x0,mean(stats),sd(stats))
    }
    
    
    y0=y0/sum(y0)*length(stats)*mean(diff(breaks_mh))/mean(diff(x0))
    
    xb1 <- signif(mean(stats),3)
    s1 <- signif(sd(stats),3)
    
    titleLabel <- switch(input$statistic,
                         mean = bquote(bold(Sample~Means)~(list(mu[bar(x)]%~~%.(xb1),~sigma[bar(x)]%~~%.(s1)))),
                         median = bquote(bold(Sample~Medians)~(list(mu[median]%~~%.(xb1),~sigma[median]%~~%.(s1)))),
                         stdev = bquote(bold(Sample~Std~Devs)~(list(mu[s]%~~%.(xb1),~sigma[s]%~~%.(s1)))),
                         var = bquote(bold(Sample~Variances)~(list(mu[s^2]%~~%.(xb1),~sigma[s^2]%~~%.(s1)))),
                         max = bquote(bold(Sample~Max)~(list(mu[max]%~~%.(xb1),~sigma[max]%~~%.(s1)))),
                         min = bquote(bold(Sample~Min)~(list(mu[min]%~~%.(xb1),~sigma[min]%~~%.(s1)))))
       
    nh<-hist(statsTrim,
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
    
    if (stat(x,input$statistic)>xmin && stat(x,input$statistic)<xmax) {
      hist(stat(x,input$statistic),
           breaks=breaks_mh,
           col=acol,
           border=acol,
           add=TRUE,
           ylim=c(0,max(y0,max(nh$counts))))
    }
    
    if (any(stats<qtrim[1])) {
      nLow = sum(stats<qtrim[1])
      maxCount = max(nh$counts)
      text(qbound[1],.9*maxCount,paste(nLow," lower not shown"),cex=tscale,adj=c(0,1))
    }
    if (any(stats>qtrim[2])) {
      nHigh = sum(stats>qtrim[2])
      #points(rep(qbound[2]+0.1,nHigh),4*rbeta(nHigh,2,2),lwd=2,col=tcol,cex=tscale)
      maxCount = max(nh$counts)
      text(qbound[2],.9*maxCount,paste(nHigh," higher not shown"),cex=tscale,adj=c(1,1))
    }
    
    if (input$normdensity){
      points(x0,y0,type="l",lwd=2)
    }
    
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
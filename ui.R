library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sampling Distribution"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Parent distribution type:",
                   c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Log-normal" = "lnorm",
                     "Exponential" = "exp",
                     #"heavy tails" = "heavy",
                     "Heavy tails" = "heavyyy")),
      radioButtons("statistic","Statistic to sample:",
                   c("Mean" = "mean",
                     "Median" = "median",
                     "Std. Dev." = "stdev",
                     "Variance" = "var"
                   )),
      sliderInput("n", 
                  "Sample size:", 
                  value = 10,
                  step = 10,
                  min = 10, 
                  max = 500),
      sliderInput("reps", 
                  "Number of repetitions:", 
                  value = 200,
                  min = 1, 
                  max = 5000),
      checkboxInput("checkbox", label = "Add samples one at a time", value = FALSE),
      actionButton("resample", label = "Draw New Sample"),
      checkboxInput("zoominbox", label = "Zoom in on sampling distribution", value = FALSE),
      checkboxInput("normdensity", label = "Show normal density", value = FALSE),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
))
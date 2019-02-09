# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/



library(shiny)
library(ggplot2)


# Define UI for application that draws a bar chart
ui <- fluidPage(
  
  
  # Sidebar with a drop down box to choose the y axis variable 
  sidebarLayout(
    sidebarPanel(
      selectInput("yvariable",
                  label="Measurement:",
                  choices=c(
                    "Average Percent Body Weight Lost Per Day" = "avgdailypercent"
                  )
      )
    ),
    
    # Show the generated plot
    mainPanel(
      plotOutput("selecteddata")
    )
  )
)


# Define server logic required to draw a bar chart
server <- function(input, output) {
  
  output$selecteddata <- renderPlot({
    # the logic to set the Y-variable for our graph
    if (input$yvariable=="total.loss"){
      y=water$total.loss
      yname="Total Body Weight Lost"
      )}
    
    else if (input$yvariable=="total.percent"){
      y=water$total.percent
      yname= "Total Percent Body Weight Lost"}
    
    else if (input$yvariable=="avg.daily"){
      y=water$avg.daily
      yname="Average Body Weight Lost Per Day"}
    
    else if (input$yvariable=="avgdailypercent"){
      y=water$avgdailypercent
      yname="Average Percent Body Weight Lost Per Day"}
    
    else {
      y=water$days.survived
      yname="Days Survived at 0% Relative Humidity"}
    
    
    # ggplot boxplot
    #creates the ggplot object, fill= chooses what you fill based on
    ggplot(water, aes(status,y,fill=status))+
      #adds the bar chart aesthetic
      geom_bar()+
      #adding the xlabel
      xlab("Percent body weight lost daily")+
      #adding the ylabel which is chosen by our logic above
      ylab(yname)+
      #setting the "theme"
      theme(panel.background=element_rect(fill="white"),axis.line=element_line(size=.1,colour="black"),panel.grid=element_line(size=.01, colour="grey"))+
      #setting the breaks for our discrete binning system
      scale_fill_discrete(name="status", breaks=c("EP","EV","LP","LV"))
  
  
  
  # Run the application 
  shinyApp(ui = ui, server = server)
  
  
library(shiny)
library(vroom)
library(tidyverse)
library(bslib)
library(cowplot)
library(DT)
library(shinythemes)

Food_Impacts <- read.csv("Food_Production.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),

  tags$figure(
    align = "left",
    tags$img(
      src = "food_icon.jpg",
      width = 400,
      alt = "Some Delicious Food"
    )
  ),
  
  titlePanel("Food Product Impacts"),
  
  navlistPanel(
    "Choose Output!",
    tabPanel("Table", 
             selectInput("Food_Type1", "Food Type", c("All","Vegetarian", "Meat"), selected = "All"),
             DTOutput("Food_Impacts_Table")),
    tabPanel("Plots",
             selectInput("Food_Type2", "Food Type", c("All","Vegetarian", "Meat"), selected = "All"),
             selectInput("Select_Plot", "Select Plot", c("Total Emissions", "Land Use","Water Use","Eutrophying Emissions"), selected = "Total Emissions"),
             plotOutput("plot")))
  
 
  
 
  )



server <- function(input, output, session) { 
  
  
  All_df <- Food_Impacts %>% select(c(Food.product,Land.use.change,Animal.Feed,Farm,Processing,Transport,Packging,Retail))
  
  Vegetarian_df <- All_df[-c(34:38,42:43),]
  
  Vegetarian_df2 <- Food_Impacts[-c(34:38,42:43),]
  
  Meat_df <- All_df[c(34:38,42:43),]
  
  Meat_df2 <- Food_Impacts[c(34:38,42:43),]
  
  df <- reactive({
    if(input$Food_Type1 == "All") {
      a <- All_df
    }
    else if(input$Food_Type1 == "Vegetarian") {
      a <- Vegetarian_df
    }
    else {
      a <- Meat_df
    }
  })
  
  df2 <- reactive({
    if(input$Food_Type2 == "All") {
      b <- Food_Impacts
    }
    else if(input$Food_Type2 == "Vegetarian") {
      a <- Vegetarian_df2
    }
    else {
      a <- Meat_df2
    }
  })

    output$Food_Impacts_Table <-renderDT(df(), options = list(lengthChange = TRUE))
  
  output$plot <- renderPlot({
    
    if (input$Select_Plot == "Total Emissions") {
      ggplot(df2(), aes(x=Food.product, y=Total_emissions, group = 1)) +
        geom_line(color="red", lwd = 1.5) +
        labs(y = "Total Emissions") +
        theme(axis.text.x = element_text(face = "bold",
                                         size = 12, angle = 90))}
    else if(input$Select_Plot == "Land Use") { 
       ggplot(df2(), aes(x=Food.product, y=Land.use.per.1000kcal..m..per.1000kcal., group = 1)) +
       geom_line(color = "blue", lwd = 1.5) +
       labs(y = "Land Use per 1000 K Cal") +
       theme(axis.text.x = element_text(face = "bold",
                                          size = 12, angle = 90))}
    else if(input$Select_Plot == "Water Use") { 
      ggplot(df2(), aes(x=Food.product, y=Scarcity.weighted.water.use.per.1000kcal..liters.per.1000.kilocalories., group = 1))+
        geom_line(color = "green", lwd = 1.5) +
        labs(y= "Scarcity Weighted Water User per 1000 K Cal") +
        theme(axis.text.x = element_text(face = "bold",
                                         size = 12, angle = 90))}
    else if(input$Select_Plot == "Eutrophying Emissions") { 
      ggplot(df2(), aes(x=Food.product, y=Eutrophying.emissions.per.1000kcal..gPO.eq.per.1000kcal., group = 1))+
      geom_line(color = "purple", lwd = 1.5) +
      labs(y= "Eutropying Emissions") +
      theme(axis.text.x = element_text(face = "bold",
                                         size = 12, angle = 90))}
  })
  
  }


# Run the application 
shinyApp(ui, server)

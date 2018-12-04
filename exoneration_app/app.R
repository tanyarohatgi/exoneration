#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(ggplot2)
library(shinythemes)

r1 <- read_rds("r.rds")
exon1 <- read_rds("exon.rds")
by_crime1 <- read_rds("by_crime.rds")

by_crime2 <- by_crime1 %>%
  mutate(crime_type2 = case_when(crime_type %in% c("Drug Possession or Sale", "Weapon Possession or Sale", "Immigration", "Military Justice Offense", "Other") ~ "Other",
                                 crime_type %in% "Violent Crimes (Non-Sexual)" ~ "Violent Crimes (Non-Sexual)",
                                 crime_type %in% "Violent Crimes (Sexual)" ~ "Violent Crimes (Sexual)",
                                 crime_type %in% "Non-Violent Crimes" ~ "Non-Violent Crimes",
                                 crime_type %in% "Vary-by-Case Crimes" ~ "Vary-by-Case Crimes"))
  
by_crime2$crime_type2 <- as.factor(by_crime2$crime_type2)


#APP CODE:

ui <- fluidPage(theme = shinytheme("paper"),
   
  
   titlePanel("U.S. Exoneration Data: 1989-2018"),
   
   tabsetPanel(
               tabPanel("Plots"),
               
               br(),
               
                tabsetPanel(
                  tabPanel("Summary Plots",
                  
                  br(),
                  
                  sidebarPanel(
                    selectInput(inputId = "a",
                                label = "Select Variable to Examine Exoneration Through the Years:",
                                choices = c("Type of Crime" = "by_crime1$crime_type", "Race of Exoneree" = "by_crime1$race",
                                            "Sex of Exoneree (Time of Conviction)" = "by_crime1$sex", 
                                            "Time Served" = "by_crime1$served")
                  
                  
                )),
                
               
                
                mainPanel(plotOutput("PlotA"))
      
      ),
       
          tabPanel("Type of Crime (Detail)",
          
          br(),
          
          sidebarPanel(
            selectInput(inputId = "b",
                        label = "Select Type of Crime:",
                        choices = levels(by_crime2$crime_type2)
                    
            )),
          

          mainPanel(plotOutput("PlotB"))
          
        )
      
      )
      

   )
   
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$PlotA <- renderPlot({ 
  
  by_crime %>%
    ggplot(aes_string("exonerated")) + geom_bar(aes_string(fill = input$a), width = 0.9) + theme_minimal() + 
    theme(legend.position = "bottom") +
    xlab("") + ylab("") + labs(fill = "") +
    ggtitle("U.S. Exonerations, 1989 - 2018:") +
    labs(subtitle = "Break-Downs by Type of Crime, Approx. Time Served, and Demograhic Characteristics.")
 
   })
  
  
  output$PlotB <- renderPlot({
    
    by_crime2 %>%
      filter(crime_type2 == input$b) %>%
      ggplot(aes_string("worst_crime_display")) + geom_bar(aes_string(fill = "state")) +
      ggtitle("The Worst Crimes Exonerees Were Convicted For, by State:") +
      labs(subtitle = "This graphic shows both convictions and exonerations by crime and state.") +
      theme(legend.position = "bottom") + theme_minimal() +  xlab("") + ylab("") + labs(fill = "") +
      coord_flip() + theme(legend.position="bottom", legend.key.size = unit(0.17, "cm")) +
      guides(fill = guide_legend(nrow = 6, byrow = TRUE))
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


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
library(stringr)

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

x <- c("DNA", "FC", "MWID", "F/MFE", "P/FA", "OM", "ILD")
new <- as.data.frame(x)
new$x <- as.factor(new$x)

new1 <- new %>%
  mutate(x = case_when(x == "DNA" ~ "DNA Evidence",
                       x == "FC" ~ "False Confession",
                       x == "MWID" ~ "Mistaken Witness ID",
                       x == "F/MFE" ~ "False or Misleading Forensic Evidence",
                       x == "P/FA" ~ "Perjury or False Accusation",
                       x == "OM" ~ "Official Misconduct",
                       x == "ILD" ~ "Insufficient Legal Defense"))

by_crime_new <- by_crime1 %>%
  mutate(basis = str_replace_all(basis, c("DNA" = "DNA Evidence",
                           "FC" = "False Confession",
                           "MWID" = "Mistaken Witness ID",
                           "F/MFE" = "False or Misleading Forensic Evidence",
                           "P/FA" = "Perjury or False Accusation",
                           "OM" = "Official Misconduct",
                           "ILD" = "Insufficient Legal Defense")))

new1$x <- as.factor(new1$x)
by_crime_new$basis <- as.factor(by_crime_new$basis)

#APP CODE:

ui <- fluidPage(theme = shinytheme("paper"),
   
  
   titlePanel("U.S. Exoneration Data: 1989-2018"),
   
   tabsetPanel(
               tabPanel("Plots"),
               
               br(),
               
                tabsetPanel(
                  tabPanel("Summary Plots",
                  
                  br(),
                  
                  sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "a",
                                label = "Select Variable to Examine Exoneration Through the Years:",
                                choices = c("Type of Crime" = "by_crime1$crime_type", "Race of Exoneree" = "by_crime1$race",
                                            "Sex of Exoneree (Time of Conviction)" = "by_crime1$sex")
                  
                  
                )),
                
               
                
                mainPanel(plotOutput("PlotA")))
      
      ),
       
          tabPanel("Type of Crime (Detail)",
          
          br(),
          
          sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "b",
                        label = "A By-State Look at the Data: Select Type of Crime:",
                        choices = levels(by_crime2$crime_type2)
                    
            )),
          

          mainPanel(plotOutput("PlotB"))
        ),
        
        br(),
        br(),
        
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "c",
                        label = "A By-Race Look at the Data: Select Type of Crime:",
                        choices = levels(by_crime2$crime_type2)
                        
            )),
          
          
          mainPanel(plotOutput("PlotC"))
        )
        
        ),
      
 
        tabPanel("Exoneration Basis",
                 
                 br(),
                 
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(inputId = "d",
                                 label = "Select Basis of Exoneration:",
                                 choices = levels(new1$x)
                                 
                                 
                     )),
                   
                   
                   
                   mainPanel(plotOutput("PlotD")))
                 
        ),
      
      tabPanel("Conviction Integrity Units",
               
               br(),
               
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "e",
                               label = "Select Basis of Exoneration:",
                               choices = levels(new1$x)
                               
                               
                   )),
                 
                 
                 
                 mainPanel(plotOutput("PlotE")))
               
      )
      
      )
      

   )
   
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$PlotA <- renderPlot({ 
  
  by_crime1 %>%
    ggplot(aes_string("exonerated")) + geom_bar(aes_string(fill = input$a), width = 0.9) + theme_minimal() + 
    theme(legend.position = "bottom") +
    xlab("") + ylab("") + labs(fill = "") +
    ggtitle("U.S. Exonerations, 1989 - 2018:") +
    labs(subtitle = "Break-Downs by Type of Crime and Demograhic Characteristics.")
 
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
  
  output$PlotC <- renderPlot({
    
    by_crime2 %>%
      filter(crime_type2 == input$c) %>%
      ggplot(aes_string("worst_crime_display")) + geom_bar(aes_string(fill = "race")) +
      ggtitle("The Worst Crimes Exonerees Were Convicted For, by Race:") +
      labs(subtitle = "This graphic shows both convictions and exonerations by crime and race") +
      theme(legend.position = "bottom") + theme_minimal() +  xlab("") + ylab("") + labs(fill = "") +
      coord_flip() + theme(legend.position="bottom", legend.key.size = unit(0.17, "cm")) +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE))
    
  })
  
  output$PlotD <- renderPlot({
    
    t4 <- reactive({
      
      by_crime_new %>%
        mutate(b = ifelse(str_detect(basis, input$d), T, F)) %>%
        filter(b == T)
      
    })
    
    t5 <- by_crime1 %>%
      count(race)
    
    t6 <- reactive({  
      
      t4() %>%
      count(race)
      
    })
    
    t7 <- reactive({ 
      
      left_join(t5, t6(), by = "race") %>%
        transmute(race = race, Total = n.x, Based = n.y) %>%
        gather("count.type", "value", Total, Based)
      
    }) 
    
      t7() %>%
      ggplot(aes(race, value, fill = as.factor(count.type))) + geom_bar(stat="identity", position="dodge") +
      ggtitle("Factual Basis for Exonerations, by Race:") + 
      labs(subtitle = "Comparative view of total exonerations and those based, at least in part, on specific mistakes/misconduct during trial.") + 
      xlab("") + ylab("") + theme_minimal() + scale_fill_manual("", values = c("Total" = "turquoise2", "Based" = "pink")) +
      coord_flip()
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


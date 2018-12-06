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
library(janitor)

#ADDITIONAL DATA CLEANING/MANIPULATING: 

by_crime1 <- read_rds("by_crime.rds")

by_crime1$crime_type <- as.factor(by_crime1$crime_type)

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

by_crime3 <- by_crime2 %>%
  mutate(exonerated = case_when(
    exonerated >= 2013 & exonerated <= 2018 ~ "2013 - Present",
    exonerated >= 2008 & exonerated <= 2012 ~ "2008 - 2012",
    exonerated >= 2003 & exonerated <= 2007 ~ "2003 - 2007"
  ))

by_crime3$exonerated <- as.factor(by_crime3$exonerated)
by_crime3$county <- as.factor(by_crime3$county)
by_crime3$race <- as.factor(by_crime3$race)


#APP CODE:

ui <- fluidPage(theme = shinytheme("cosmo"),
   
                tags$head(
                  tags$style(
                    ".title {margin: auto; width: 500px; text-align:center; font-size : 20px;}"
                  )
                ),
  
                tags$div(class="title", titlePanel(strong("U.S. Exoneration Data: 1989-2018"))),
                h4("TANYA ROHATGI", align = "center"),
   
   tabsetPanel(
     
     tabPanel("Plots and Findings",
               br(),
                tabsetPanel(type = "pills",
                  tabPanel("Introductory Plots",
                  
                  br(),
                  
                  sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "a",
                                label = "Select Variable to Examine Exoneration Through the Years:",
                                choices = c("Type of Crime" = "by_crime1$crime_type", "Race of Exoneree" = "by_crime1$race",
                                            "Gender of Exoneree" = "by_crime1$sex")
                               ),
                    helpText("NOTES:"),
                    helpText("1. The type-of-crime chart details only the worst crime the exoneree was convicted for, and does not include any other crimes/misdemeanors they may have committed."),
                    helpText("2. The gender of the exoneree refers to their gender at the time of conviction.")),
                
                mainPanel(plotOutput("PlotA"),
                          p(""),
                          p("Type of Crime: While we see the consistent presence of violent offense convictions through the years, there has been a recent spike in the percentage of exonerations for non-violent and drug-related offence convictions.
                            This is somewhat surprising; it would be reasonable to expect that scare re-consideration resources would be spent on high-profile cases - such as of those on death row - where the crimes committed were most likely violent.",
                            style = "font-size : 10pt"),
                          p("Race: We can see that the number of exonerations in the U.S. has increased steadily over time, and sharply in the last few years. 
                            The percentages of African-Americans and Caucasions who have been exonerated in this period have ranged from 20% to 56% and 21% to 44% respectively.
                            Despite constituting around 12% of the country's population, African-Americans form 34% of the incarcerated population, contrasted against the nearly 64% majority Caucasian population's 30%.
                            The exoneration parity between the two races, then, means that while the existing prison population percentage remains steady, the racial disparity in incarceration remains as well.",
                            style = "font-size : 10pt"),
                          p("In this project, I will attempt to explore both these phenomena in greater detail.",
                            style = "font-size : 10pt")
                          ))
      
                          ),
       
          tabPanel("Type of Crime, State, and Race",
          
          br(),
          
          sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "b",
                        label = "A By-State Look at the Data: Select Type of Crime:",
                        choices = levels(by_crime2$crime_type2)
                    
            )),
          

          mainPanel(plotOutput("PlotB"),
                    p("Here the most significant thing we see is that Texas has exonerated the greatest number of exonerees charged with drug crimes, which after murder and sexual assault is the third-most represented kind of conviction.",
                      style = "font-size : 10pt"),
                    p("It is also a crime for which, to our knowledge, a greater number of African-Americans have been falsely accused - as we see in the chart below. This leads us to the question: is this a disproportionate number? To put it another way, is it reflective of racial disparities in drug-crime related incarceration? The answer seems to be in the affirmative. [ADD LINK]",
                      style = "font-size : 10pt"),
                    p("Why, however, is Texas leaps and bounds ahead of its fellow states when in comes to overturning narcotics convictions? I explore this in the next section.",
                      style = "font-size : 10pt")
                    )
        ),
        
        br(),
        br(),
        
        sidebarLayout(
          sidebarPanel(
            selectInput(inputId = "c",
                        label = "A By-Race Look at the Data: Select Type of Crime:",
                        choices = levels(by_crime2$crime_type2)
                        
            )),
          
          
          mainPanel(plotOutput("PlotC")
                    )
        )
        
        ),
      
      
      tabPanel("Conviction Integrity Units",
               
               br(),
               
               sidebarLayout(
                 sidebarPanel(
                   helpText("A Conviction Integrity Unit (CIU) is a division of a prosecutorial office that works to prevent, identify, and remedy false convictions."),
                   checkboxInput("extra", label = "Show CIU-Facilitated Exonerations by Year and Crime Type."),
                   
                   conditionalPanel(
                     condition = "input$extra == TRUE",
                     uiOutput("conditionalInput")
                     
                   )
                   
                   ),
                 
                 
                 mainPanel(plotOutput("PlotE"),
                           p("CIUs play no definitive role in exonerations before 2003. Given this we can conclude that they only began to come into existence around this time. Their involvement has grown rapidly through the years following - from only 2 exonerations secured between 2003 and 2007, to 21 between 2008 and 2012, to 266 between 2013 and now. This indicates a growing (but by no means suffient, becuase only 12 states have CIUs that have resulted in even a single exoneration) degree of comprehension in prosecutorial offices that wrongful convictions are a serious and frequent problem, and must be remedied.",
                             style = "font-size : 10pt"),
                           p("We see that the maximum number of exonerations come from Harris County, Texas, and they are overwhelmingly for cases of drug posession or sale. The only two other states with CIUs that have resulted in significant numbers of exonerations secured are New York and Illinois, the majority of their successes being for exonerees convicted of violent crimes.",
                             style = "font-size : 10pt"),
                           p("A CIU that dedicates resources to non-violent/less severe crimes can be explanation enough for looking at drug crimes in the first place. However, what is it that pushed Harris county to do so? Until 2014, defendents were asked for their pleas based on faulty field-test kits that would yeild false positives, before lab results on the substances were returned, and before trial. And 99.5% of drug convictions in the county resulted from plea deals. [ADD LINK].
                             Harris county, then, is one of the only prosecutorial offices in the entire nation that has identified at least one systemic issue in its criminal justice system and is working actively to remedy it.",
                             style = "font-size : 10pt")
                 )
               
      )),
      
      tabPanel("Exoneration Basis",
               
               br(),
               
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "d",
                               label = "Select Basis of Exoneration:",
                               choices = levels(new1$x)
           
                   )),

                 mainPanel(plotOutput("PlotD"),
                           p("Some key take-aways from this plot: official misconduct, wherein police, prosecutors, or other officials abused their power or the workings of the judicial process, was a contributing factor in over half of the convictions of both African-American and Caucasian exonerees.
                             This has also been true for perjury of false accusations. However mistaken witness ID, wherein one or more witness confirmed - mistakenly - that they saw the exoneree commit the crime, sent more black exonerees to jail than those of any other race.",
                             style = "font-size : 10pt")
                           
                           ))
      
       )
      
    
   )),
   
   tabPanel("About This Project")
   
))



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$conditionalInput <- renderUI({
    
    if(input$extra == TRUE) {
      
      sidebarLayout(
      selectInput(inputId = "f",
                  label = "Select Time Period:",
                  choices = levels(by_crime3$exonerated)),
      
      checkboxInput("county", label = "Show same plot, but with county fill instead of crime type.")
                  
      )
      
    }
    
  })
  
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
      ggplot(aes(race, value)) + geom_bar(aes(fill = count.type), stat="identity", position="dodge") +
      ggtitle("Factual Basis for Exonerations, by Race:") + 
      labs(subtitle = "Comparative view of total exonerations and those based, at least in part, on specific mistakes/misconduct during trial.") + 
      xlab("") + ylab("") + theme_minimal() + scale_fill_manual("", values = c("Total" = "royalblue1", "Based" = "orchid1")) +
      coord_flip()
    
  })
  
  
  output$PlotE <- renderPlot({
    
    if(input$extra == TRUE) {
      
 by_crime3 %>%
        mutate(tags = str_extract(tags, "CIU")) %>%
        filter(tags == "CIU") %>%
        select(-basis) %>%
        filter(exonerated == input$f) %>%
        ggplot(aes_string("state")) + geom_bar(aes_string(fill = "crime_type")) + theme_minimal() +
        ggtitle("Number of Exonerations Secured by Conviction Integrity Units in Prosecutorial Offices:") +
        labs(subtitle = "Data can be viewed in 5 year increments, beginning in 2003 when CIU exonerations are first seen.") +
        xlab("") + ylab("") + labs(fill = "Crime Type") + coord_flip() + theme(legend.position = "bottom")
      
      if (input$county == TRUE) {
        
        by_crime3 %>%
          mutate(tags = str_extract(tags, "CIU")) %>%
          filter(tags == "CIU") %>%
          select(-basis) %>%
          filter(exonerated == input$f) %>%
          ggplot(aes_string("state")) + geom_bar(aes_string(fill = "county")) + theme_minimal() +
          ggtitle("Number of Exonerations Secured by Conviction Integrity Units in Prosecutorial Offices:") +
          labs(subtitle = "Data can be viewed in 5 year increments, beginning in 2003 when CIU exonerations are first seen.") +
          xlab("") + ylab("") + labs(fill = "County") + coord_flip()  + theme(legend.position = "bottom", legend.key.size = unit(0.32, "cm"))
        
      }
      
      else {
        
        by_crime3 %>%
          mutate(tags = str_extract(tags, "CIU")) %>%
          filter(tags == "CIU") %>%
         select(-basis) %>%
          filter(exonerated == input$f) %>%
          ggplot(aes_string("state")) + geom_bar(aes_string(fill = "crime_type")) + theme_minimal() +
          ggtitle("Number of Exonerations Secured by Conviction Integrity Units in Prosecutorial Offices:") +
          labs(subtitle = "Data can be viewed in 5 year increments, beginning in 2003 when CIU exonerations are first seen.") +
          xlab("") + ylab("") + labs(fill = "Crime Type") + coord_flip() + theme(legend.position = "bottom")
        
      }
      

    }
    
    else {
    
    by_crime1 %>%
      mutate(tags = str_extract(tags, "CIU")) %>%
      filter(tags == "CIU") %>% 
      group_by(exonerated, state) %>%
      count(tags) %>%
      ggplot(aes_string("exonerated")) + geom_line(aes_string(y = "n", color = "state"), size = 1.5, alpha = 0.8) + theme_minimal() +
        ggtitle("Number of Exonerations Secured by Conviction Integrity Units in Prosecutorial Offices Over Time:") +
        xlab("") + ylab("") + labs(color = "State")
    
    }
    
    
    
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)


library(tidyverse)
library(shiny)
library(ggplot2)
library(shinythemes)
library(stringr)
library(janitor)
library(plotly)
library(styler) 

#I used the Styler addin to format my code in accordance with the tidyverse
#styleguide.

### IMPORTING DATA USING AN .RDS FILE: ###

by_crime1 <- read_rds("by_crime.rds")

### ADDITIONAL DATA CLEANING/MANIPULATING: ###

by_crime1$crime_type <- as.factor(by_crime1$crime_type)
by_crime1$race <- as.factor(by_crime1$race)

by_crime1 <- by_crime1 %>%
  mutate(race = fct_relevel(race, c("Other", "Native American", "Asian", "Hispanic", "Black", "White")))

# Adding the crime types that — because of their unique nature/what I'm trying
# to explore — have not been added to the violent/non-violent/vary-by-case
# categories, to a new 'Other' category for cleaner visualisation in subsetted
# plots:

by_crime2 <- by_crime1 %>%
  mutate(crime_type2 = case_when(
    crime_type %in% c("Drug Possession or Sale", "Weapon Possession or Sale", "Immigration", "Military Justice Offense", "Other") ~ "Other",
    crime_type %in% "Violent Crimes (Non-Sexual)" ~ "Violent Crimes (Non-Sexual)",
    crime_type %in% "Violent Crimes (Sexual)" ~ "Violent Crimes (Sexual)",
    crime_type %in% "Non-Violent Crimes" ~ "Non-Violent Crimes",
    crime_type %in% "Vary-by-Case Crimes" ~ "Vary-by-Case Crimes"
  ))

by_crime2$crime_type2 <- as.factor(by_crime2$crime_type2)

# Creating a new dataframe with the names of basis-of-exoneration tags as
# drop-down options I'll need later, which will map onto the same tag names in
# the main dataframe (the preparation for which is the re-naming of the tags
# from the dataframe by_crime1 to the new dataframe by_crime_new): I do this in
# order to get the tag names in the drop-down at all, because in many cases
# multiple tags apply to a single observation, and I choose to look at them
# individually. Using the main dataframe to do so would require a reactive
# function because each tag must be filtered individually, which is not possible
# in the input section of the ui. Therefore a new dataframe with just the tag
# names I need must be created:

x <- c("DNA", "FC", "MWID", "F/MFE", "P/FA", "OM", "ILD")
new <- as.data.frame(x)
new$x <- as.factor(new$x)

new1 <- new %>%
  mutate(x = case_when(
    x == "DNA" ~ "DNA Evidence",
    x == "FC" ~ "False Confession",
    x == "MWID" ~ "Mistaken Witness ID",
    x == "F/MFE" ~ "False or Misleading Forensic Evidence",
    x == "P/FA" ~ "Perjury or False Accusation",
    x == "OM" ~ "Official Misconduct",
    x == "ILD" ~ "Insufficient Legal Defense"
  ))

by_crime_new <- by_crime1 %>%
  mutate(basis = str_replace_all(basis, c(
    "DNA" = "DNA Evidence",
    "FC" = "False Confession",
    "MWID" = "Mistaken Witness ID",
    "F/MFE" = "False or Misleading Forensic Evidence",
    "P/FA" = "Perjury or False Accusation",
    "OM" = "Official Misconduct",
    "ILD" = "Insufficient Legal Defense"
  )))

new1$x <- as.factor(new1$x)
by_crime_new$basis <- as.factor(by_crime_new$basis)

# Here I create the 3 time periods I will use to subset my later plot on CIUs:

by_crime3 <- by_crime2 %>%
  mutate(exonerated = case_when(
    exonerated >= 2013 & exonerated <= 2018 ~ "2013 - Present",
    exonerated >= 2008 & exonerated <= 2012 ~ "2008 - 2012",
    exonerated >= 2003 & exonerated <= 2007 ~ "2003 - 2007"
  ))

by_crime3$exonerated <- as.factor(by_crime3$exonerated)
by_crime3$county <- as.factor(by_crime3$county)
by_crime3$race <- as.factor(by_crime3$race)


### APP CODE: ###

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  
  br(),
  
  # Using html to format the header, add a favicon, and add and format a
  # background image for the 'About this Project' tab:

  tags$head(
    tags$style(
      ".title {margin: auto; width: 600px; text-align:center; font-size : 30px}"
    )
  ),

  tags$div(class = "title", titlePanel(strong("EXPLORING U.S. EXONERATION DATA"), windowTitle = "Exonerations in the U.S.")),
  h4("TANYA ROHATGI", align = "center"),
  
  tags$head(
    tags$link(rel = "shortcut icon", href = "https://s3.amazonaws.com/user-media.venngage.com/416105-88b87a96f427878faa56ce057443dd56.png")),

  tags$head(
    tags$style(HTML(".background{ width: 100%; background-size: 100% 116%; text-align:center; position: absolute; left: 0;
                                  background-image: url(https://cdn-images-1.medium.com/max/2000/1*N36dU-o02IRwRoqQVZqqdg.jpeg);
                                  }"))
  ),
  
  # Adding tabs and content:

  tabsetPanel(
    tabPanel(
      "About This Project",
      column(
        10,
        class = "background",
        br(),
        br(),
        h4(strong("What does this app do?")),
        h4("This app uses data from", tags$a(href = "http://www.law.umich.edu/special/exoneration/Pages/about.aspx", "The National Registry of Exonerations,"), "a project by the University of Michigan Law School, to analyse 2,280 exonerations between the years 1989 and 2018 in the United States. I look at type of crime, exoneree demographic characteristics, prosecutorial offices involved, and factors related to exoneration, to graphically demonstrate my findings of the following:"),
        h4("1) Over the last 4-5 years, there has been a massive increase in exonerations for those falsely convicted of drug-related offences, which has corresponded with a large spike in total exonerations. 2) These exonerations have largely been facilitated by the Conviction Integrity Unit in the prosecution office of Texas' Harris County, and I explore why. 3) Official misconduct and false accusations played a greater role in the convictions of exonerees than factors such as erroneous DNA evidence or inadequate legal defense, although both the latter were present. Mistaken witness ID played a far greater role in sending black exonerees to prison than it did for any other race."),
        br(),
        h4(strong("Take a look under the hood:")),
        h4("Access the code for this app", tags$a(href = "https://github.com/tanyarohatgi/exoneration", "here."), "Browse my other work this past semester for Harvard College's 'GOV 1005: Data'", tags$a(href = "https://github.com/tanyarohatgi", "here.")),
        br(), br(), br(), br(), br(), br(), br(), br(),
        br(), br(), br(), br(), br(), br(), br(), br()
      )
    ),

    tabPanel(
      "Plots and Findings",
      br(),
      tabsetPanel(
        type = "pills", 
        tabPanel(
          "Introductory Plots",

          br(),

          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "a",
                label = "Select Variable to Examine Exoneration Through the Years:",
                choices = c(
                  "Type of Crime" = "by_crime1$crime_type", "Race of Exoneree" = "by_crime1$race",
                  "Gender of Exoneree" = "by_crime1$sex"
                )
              ),

              helpText("NOTES:"),
              helpText("1. The type-of-crime chart details only the worst crime the exoneree was convicted for, and does not include any other crimes/misdemeanors they may have committed."),
              helpText("2. The gender of the exoneree refers to their gender at the time of conviction."),
              helpText("3. Vary-by-Case crimes are those that, depending on the individual circumstances of the case, could be either violent or non-violent. Examples include harrassment and supporting terrorism.")
            ),

            mainPanel(
              h4(strong("I. Exonerations in the United States between 1989 and 2018:")),
              h5("Breakdowns by type of crime and demograhic characteristics."),
              plotlyOutput("PlotA"),
              p(""),
              p("Type of Crime: While we see the consistent presence of violent offense convictions through the years, there has been a recent spike in the percentage of exonerations for non-violent and drug-related offence convictions.
                            This is somewhat surprising; it would be reasonable to expect that scare re-consideration resources would be spent on high-profile cases — such as of those on death row — where the crimes committed were most likely violent.",
                style = "font-size : 11pt"
              ),
              p("Race: We can see that the number of exonerations in the U.S. has increased steadily over time, and sharply in the last few years. 
                            The percentages of African-Americans and Caucasions who have been exonerated in this period have ranged from 20% to 56% and 21% to 44% respectively.
                            Despite constituting around 12% of the country's population, African-Americans", tags$a(href = "http://www.pewresearch.org/fact-tank/2018/01/12/shrinking-gap-between-number-of-blacks-and-whites-in-prison/", "form"), "34% of the incarcerated population, contrasted against the nearly 64% majority Caucasian population's 30%.
                            The exoneration parity between the two races, then, means that while the existing prison population percentage remains steady, the racial disparity in incarceration remains as well.",
                style = "font-size : 11pt"
              ),
              p("Gender: The percentages of exonerees per gender are closely", tags$a(href = "https://www.bop.gov/about/statistics/statistics_inmate_gender.jsp", "in line with"), "the incarceration percentages per gender."),
              p("In this project, I will attempt to explore the first two phenomena in greater detail.",
                style = "font-size : 11pt"
              ),
              br()
            )
          )
        ),

        tabPanel(
          "Type of Crime, State, and Race",

          br(),
          
          # When there is more than one unique plot (as in, with its own
          # distinct sidebarPanel) in a tab, I position the text containing
          # findings in between them, as opposed to after both plots. This
          # allows for less scrolling through the page when interacting with the
          # many plots while reading the text.

          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "b",
                label = "A By-State Look at the Data: Select Type of Crime:",
                choices = levels(by_crime2$crime_type2)
              ),

              helpText("Since there are too many states for a legend to be helpful or visually significant, hover over the barplot to identify state and exoneration count instead."),
              helpText("NOTE: For federal cases, the state name is in the form of 'Fed-XX' where 'XX' is the postal abbreviation for the state in which the crime was committed. I have not collapsed federal and non-federal crimes per state because they were in all likelihood prosecuted by different offices and tried in different tiers of courts.")
            ),


            mainPanel(
              h4(strong("II. The Worst Crimes Exonerees Were Convicted For, by State:")),
              h5("This graphic shows both convictions and exonerations by crime and state."),
              plotlyOutput("PlotB"),
              p("In Graph II the most significant thing we see is that Texas has exonerated the greatest number of exonerees charged with drug crimes, which after murder and sexual assault is the third-most represented kind of conviction.",
                style = "font-size : 11pt"
              ),
              p("It is also a crime for which, to our knowledge, a greater number of African-Americans have been falsely accused — as we see in Graph III below. This leads us to the question: is this a proportionate or disproportionate number? To put it another way, is it reflective of racial disparities in drug-crime related incarceration? The answer seems to be in the", tags$a(href = "https://www.naacp.org/criminal-justice-fact-sheet/", "affirmative."),
                style = "font-size : 11pt"
              ),
              p("Why, however, is Texas leaps and bounds ahead of its fellow states when in comes to overturning narcotics convictions? I explore this in the next section.",
                style = "font-size : 11pt"
              )
            )
          ),

          br(),

          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "c",
                label = "A By-Race Look at the Data: Select Type of Crime:",
                choices = levels(by_crime2$crime_type2)
              ),

              helpText("Hover over the barplot to identify race and exoneration count.")
            ),


            mainPanel(
              h4(strong("III. The Worst Crimes Exonerees Were Convicted For, by Race:")),
              h5("This graphic shows both convictions and exonerations by crime and state."),
              plotlyOutput("PlotC"),
              br()
            )
          )
        ),


        tabPanel(
          "Conviction Integrity Units",

          br(),

          sidebarLayout(
            sidebarPanel(
              helpText("A Conviction Integrity Unit (CIU) is a division of a prosecutorial office that works to prevent, identify, and remedy false convictions."),
              radioButtons(
                inputId = "total", label = "Select Plot:",
                choices = c("Total CIU Exonerations", "CIU Exonerations by State")
              ),
              helpText("Hover for details.")
            ),


            mainPanel(
              h4(strong("IV. Number of Exonerations Secured by Conviction Integrity Units in Prosecutorial Offices Over Time:")),
              plotlyOutput("PlotD"),
              p("As we see above, CIUs play no definitive role in exonerations before 2003. Given this we can conclude that they only began to come into existence around this time. As we can see in both Gaph IV above and Graph V below, their involvement has grown rapidly in the years following; from only 2 exonerations secured between 2003 and 2007, to 21 between 2008 and 2012, to 266 between 2013 and now. This indicates a growing (but by no means sufficient, because only 12 states have CIUs that have resulted in even a single exoneration) degree of comprehension in prosecutorial offices that wrongful convictions are a serious and frequent problem, one that the office itself must play a role in remedying.",
                style = "font-size : 11pt"
              ),
              p("Graph IV also shows us that Texas has had the highest number of CIU exonerations by far, and then we see in Graph V that the maximum number of exonerations come from the state's Harris County, and that they are overwhelmingly for cases of drug posession or sale. The only two other states with CIUs that have resulted in significant numbers of exonerations secured are New York and Illinois, the majority of their successes being for exonerees convicted of violent crimes.",
                style = "font-size : 11pt"
              ),
              p("A CIU that dedicates resources to possible miscarriages of justice in non-violent/less severe crimes can be explanation enough for the focus on drug crimes that the data indicates. However, what is it that pushed Harris County, in particular, to do so? According to an", tags$a(href = "https://www.nytimes.com/2016/07/10/magazine/how-a-2-roadside-drug-test-sends-innocent-people-to-jail.html", "extensive investigation by The New York Times,"), "until 2014, defendents were asked for their pleas based on results of faulty field-test kits that would often yeild false positives — before lab results on the substances were returned, and before trial. And 99.5% of drug convictions in the county resulted from plea deals (the national average is an appalling 90%). Harris County's crime lab did express a commitment to testing the substances even after a guilty plea, unlike a whopping 62% of crime labs in the United States, which is an ethos that corresponds with the county's comparatively stellar record of narcotics exonerations. The problem was, however, that they often didn't get to doing so till years after the plea was recorded and the convict sentenced, which caught the attention of reporters and individual prosecutors.
                             Because of their efforts, Harris county is now one of the only prosecutorial offices in the entire country that has systematized its overview of drug convictions in light of these procedural challenges.",
                style = "font-size : 11pt"
              )
            )
          ),

          br(),

          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "e",
                label = "Select Time Period:",
                choices = levels(by_crime3$exonerated)
              ),
              helpText("Hover for details."),
              br(),

              checkboxInput("county", label = "Fill by county instead of crime type."),
              helpText("For the by-county plot: Since there are too many counties (especially 2013 onwards) for a legend to be helpful or visually significant, hover over the barplot to identify county and exoneration count instead.")
            ),


            mainPanel(
              h4(strong("V. Number of Exonerations Secured by Conviction Integrity Units in Prosecutorial Offices:")),
              h5("Data can be viewed in 5 year increments, beginning in 2003 when CIU exonerations are first seen."),
              plotlyOutput("PlotE"),
              br()
            )
          )
        ),

        tabPanel(
          "Exoneration Basis",

          br(),

          sidebarLayout(
            sidebarPanel(
              selectInput(
                inputId = "f",
                label = "Select Basis for Exoneration:",
                choices = levels(new1$x)
              ),
              helpText("Hover for details.")
            ),

            mainPanel(
              h4(strong("VI. Basis for Exonerations, by Race:")),
              h5("Comparative view of total exonerations and those based, at least in part, on specific mistakes/misconduct during trial."),
              plotlyOutput("PlotF"),
              p("Some key take-aways from this plot: official misconduct, wherein police, prosecutors, or other officials abused their power or the workings of the judicial process, was a contributing factor in over half the convictions of exonerees of all races but Asian and Other.
                             This is also true for perjury or false accusations. However mistaken witness ID, wherein one or more witness confirmed — mistakenly — that they saw the exoneree commit the crime, sent more black exonerees to jail than those of any other race.",
                style = "font-size : 11pt"
              )
            )
          )
        )
      )
    )
  )
)



server <- function(input, output) {
  output$PlotA <- renderPlotly({
    
    # In all my plots, I have done away with axis labels and legend labels for a
    # neater look. I can do this without impacting the effectiveness of the
    # visualisation because 1) often the varaibles on the axis are self-evident,
    # such as counts in barplots, and 2) they are always made explicit both in
    # the plot title and subtitle, and in the details/instructions in the
    # sidebarPanels.
    
    # I have also removed the legend formatting from the ggplot objects, because
    # plotly doesn't recognise them. Instead, I have formatted the legends using
    # plotly functions and arguments.
    
    ply0 <- by_crime1 %>%
      ggplot(aes_string("exonerated")) + geom_bar(aes_string(fill = input$a), width = 0.9) + 
      theme_minimal() +
      xlab("") + ylab("") + labs(fill = "")
    
    # In the plots where I keep the legend, I make them hozizonal and position
    # them at the bottom of the plots using plotly functions and arguments, such
    # as below:
    
    ply0 %>%
      ggplotly(tooltip = "count") %>%
      layout(legend = list(orientation = "h",  
                           xanchor = "center",
                           x = 0.5,
                           y = -0.1, 
                           traceorder = "reversed",
                           font = list(family = "sans-serif", size = 11))
             ) %>%
      style(legendgroup = NULL)
    
    # The 'legendgroup = NULL' argument allows the elements of the now
    # horizontal legend to wrap into multiple rows if needed, instead of showing
    # only the number that fit in one row. There is no dedicated argument to
    # manually set rows yet.
    
  })

  output$PlotB <- renderPlotly({
    
    # In many of the plots I mutate variable names to title cases ("variable" to
    # "Variable"), for the purpose of tooltip display:
    
    ply <- by_crime2 %>%
      mutate(crime = worst_crime_display) %>%
      filter(crime_type2 == input$b) %>%
      mutate(State = state) %>%
      ggplot(aes_string("crime")) + geom_bar(aes_string(fill = "State")) +
      theme(legend.position = "bottom") + theme_minimal() + 
      xlab("") + ylab("") + labs(fill = "") +
      coord_flip() 
    
    # I remove the legend because there are too many states displayed, and so a
    # plotly hover by itself works better:

    ply %>%
      ggplotly(tooltip = c("State", "count")) %>%
      layout(showlegend = F)
    
  })

  output$PlotC <- renderPlotly({
    
    ply2 <- by_crime2 %>%
      mutate(crime = worst_crime_display) %>%
      filter(crime_type2 == input$c) %>%
      mutate(Race = race) %>%
      ggplot(aes_string("crime")) + geom_bar(aes_string(fill = "Race")) +
      theme(legend.position = "bottom") + theme_minimal() + 
      xlab("") + ylab("") + labs(fill = "") +
      coord_flip() 

    ply2 %>%
      ggplotly(tooltip = c("Race", "count")) %>%
      layout(legend = list(orientation = "h",  
                           xanchor = "center",
                           x = 0.5,
                           y = -0.1, 
                           traceorder = "reversed",
                           font = list(family = "sans-serif", size = 11)))
      
  })


  output$PlotD <- renderPlotly({
    
    # In this tab, I filter specifically for 'CIU's, or Conviction Integrity
    # Units, because of the findings of previous plots that lead me to examine
    # the role of county-specific factors in specific kinds of exonerations —
    # in this case, drug-related crime convicts were exonerated at much higher
    # rates in Harris County, Texas, than anywhere else.
    
    if (input$total == "Total CIU Exonerations") {
      
      ply3 <- by_crime1 %>%
        mutate(tags = str_extract(tags, "CIU")) %>%
        filter(tags == "CIU") %>%
        group_by(exonerated, state) %>%
        count(tags) %>%
        mutate(Count = n) %>%
        mutate(Exonerated = exonerated) %>%
        ggplot(aes_string("Exonerated")) + geom_line(aes_string(y = "Count"), size = 0.8, alpha = 0.8, color = "orchid") +
        theme_minimal() +
        xlab("") + ylab("")
      
      ply3 %>%
        ggplotly(tooltip = c("Exonerated", "Count")) 
    }

    else {
      
      ply4 <- by_crime1 %>%
        mutate(tags = str_extract(tags, "CIU")) %>%
        filter(tags == "CIU") %>%
        group_by(exonerated, state) %>%
        count(tags) %>%
        mutate(Count = n) %>%
        mutate(Exonerated = exonerated, State = state) %>%
        ggplot(aes_string("Exonerated")) + geom_line(aes_string(y = "Count", color = "State"), size = 0.8, alpha = 0.8) + 
        theme_minimal() +
        xlab("") + ylab("") + labs(color = "")
      
      ply4 %>%
      ggplotly(tooltip = c("State", "Exonerated", "Count")) %>%
        layout(legend = list(font = list(family = "sans-serif", size = 11)))
      
    }
  })

  output$PlotE <- renderPlotly({
    
    if (input$county == TRUE) {
      
      ply5 <- by_crime3 %>%
        mutate(tags = str_extract(tags, "CIU")) %>%
        filter(tags == "CIU") %>%
        select(-basis) %>%
        filter(exonerated == input$e) %>%
        mutate(County = county) %>%
        ggplot(aes_string("state")) + geom_bar(aes_string(fill = "County")) + 
        theme_minimal() +
        xlab("") + ylab("") + labs(fill = "County") + 
        coord_flip() 
       
      # Once again, removing legend because there are too many counties for a
      # legend to be helpful. Using only the plotly hover instead:
      
      ply5 %>% 
        ggplotly(tooltip = c("County", "count")) %>%
        layout(showlegend = F)

    }

    else {
      
      ply6 <- by_crime3 %>%
        mutate(tags = str_extract(tags, "CIU")) %>%
        filter(tags == "CIU") %>%
        select(-basis) %>%
        filter(exonerated == input$e) %>%
        mutate(Category = crime_type) %>%
        ggplot(aes_string("state")) + geom_bar(aes_string(fill = "Category")) + 
        theme_minimal() +
        xlab("") + ylab("") + labs(fill = "Crime Type") + 
        coord_flip() 
      
      ply6 %>% 
        ggplotly(tooltip = c("Category", "count")) %>%
        layout(legend = list(orientation = "h",  
                             xanchor = "center",
                             x = 0.5,
                             y = -0.1, 
                             font = list(family = "sans-serif", size = 10)))
    }
  })

  output$PlotF <- renderPlotly({
    
    # For this plot, I use multiple reactive functions in order to first create
    # a reactive object wherein the dataframe is filtered for individual
    # basis-of-exoneration tags, and then to work with this object (which can
    # only be done in a reactive context):
    
    t4 <- reactive({
      
      by_crime_new %>%
        mutate(b = ifelse(str_detect(basis, input$f), T, F)) %>%
        filter(b == T) %>%
        count(race)
    })

    t5 <- by_crime1 %>%
      count(race)


    t7 <- reactive({
      
      left_join(t5, t4(), by = "race") %>%
        transmute(race = race, Total = n.x, Based = n.y) %>%
        gather("count.type", "Value", Total, Based)
      
    })


    ply7 <- reactive({ 
      
      t7() %>%
      mutate_if(is.numeric, round, 0) %>%
      ggplot(aes(race, Value)) + geom_bar(aes(fill = count.type), stat = "identity", position = "dodge") +
      xlab("") + ylab("") + theme_minimal() + 
      scale_fill_manual("", values = c("Total" = "royalblue1", "Based" = "orchid1")) +
      coord_flip()
      
    })
    
      ply7() %>%
      ggplotly(tooltip = c("Value")) %>%
      style(hoverinfo = "x") %>%
      layout(legend = list(orientation = "h",  
                           xanchor = "center",
                           x = 0.5,
                           y = -0.1,
                           font = list(family = "sans-serif", size = 11)),
             xaxis = list("Value", hoverformat = '.0f')
             )
  })
}


shinyApp(ui = ui, server = server)

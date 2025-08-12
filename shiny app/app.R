library(shiny)
library(ggplot2)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(grid)
library(ggtext)

# Build ETL pipeline for processing the data

## Extract data
extract_data <- function(path){
  df <- read.csv(path,
                 header = TRUE,
                 sep = "\t",      
                 dec = ".",          
                 quote = "\"",        
                 comment.char = "",   
                 na.strings = "NA",  
                 stringsAsFactors = FALSE)
  
  # Check the file name and add the column accordingly
  if (grepl("10-24", path)) {
    df$Five.Year.Age.Groups.Code <- "10-24"
  } else if (grepl("10-19", path)) {
    df$Five.Year.Age.Groups.Code <- "10-19"
  }
  
  return(df)
}

## Transform data, make the raw data organized and readable
transform_data <-function(df){
  # make uniform columns
  required_cols <- c(
    "Multiple.Cause.of.death",
    "Multiple.Cause.of.death.Code",
    "UCD...Drug.Alcohol.Induced.Cause",
    "UCD...Drug.Alcohol.Induced.Cause.Code"
  )
  
  if ("Hispanic.Origin" %in% colnames(df) & "Hispanic.Origin.Code" %in% colnames(df)) {
    df <- df %>%
      rename(Race = Hispanic.Origin, Race.Code = Hispanic.Origin.Code)
  } else if ("Single.Race.6" %in% colnames(df) & "Single.Race.6.Code" %in% colnames(df)){
    df <- df %>%
      rename(Race = Single.Race.6, Race.Code = Single.Race.6.Code)
  }else {
    required_cols <- c(required_cols, "Race", "Race.Code")
  }
  
  missing_cols <- setdiff(required_cols, colnames(df))
  df[missing_cols] <- "ALL"
  
  # make all data in the same format
  df <- df %>%
    filter(Notes == "") %>%
    mutate(Deaths = as.numeric(ifelse(Deaths == "Suppressed", "0", Deaths)),
           Death_Rate = Deaths/Population*1e5,
           Age = coalesce(!!!select(., any_of(c("Five.Year.Age.Groups.Code", "Ten.Year.Age.Groups.Code"))))
             ) %>%
    select(Year = Year.Code, 
           Age, 
           MCD = Multiple.Cause.of.death,
           MCD.Code = Multiple.Cause.of.death.Code,
           UCD = UCD...Drug.Alcohol.Induced.Cause, UCD.Code = UCD...Drug.Alcohol.Induced.Cause.Code, 
           Deaths, Population, Death_Rate, Race, Race.Code)
  
  # need to aggregate races after 2021
  if (any(df$Year >= 2021)){
    df <- df %>%
      mutate(Race = ifelse(Race == "Asian" | Race == "Native Hawaiian or Other Pacific Islander",
                           "Asian or Pacific Islander", Race),
             Race.Code = ifelse(Race == "Asian or Pacific Islander", "A-PI", Race.Code)) %>%
      group_by(Year, Age, MCD, MCD.Code, UCD, UCD.Code, Race, Race.Code) %>%
      summarise(Deaths = sum(Deaths),
                Population = sum(Population)) %>%
      mutate(Death_Rate = Deaths/Population*1e5) %>%
      select(Year, Age, MCD, MCD.Code, UCD,
             UCD.Code, Deaths, Population, Death_Rate, Race, Race.Code)
  }
  
  #rename necessary info
  df <- df %>%
    mutate(MCD = ifelse(MCD == "Other synthetic narcotics",
                        "Synthetic opioids excluding methadone", MCD),
           MCD = ifelse(MCD == "Other opioids",
                        "Natural and semisynthetic opioids", MCD),
           UCD = case_when(
             UCD == "Drug poisonings (overdose) Unintentional (X40-X44)" ~ "Unintentional",
             UCD == "Drug poisonings (overdose) Suicide (X60-X64)" ~ "Suicide",
             UCD == "Drug poisonings (overdose) Homicide (X85)" ~ "Homicide",
             UCD == "Drug poisonings (overdose) Undetermined (Y10-Y14)" ~ "Undetermined",
             TRUE ~ "ALL"),
           Race = ifelse(Race == "Hispanic or Latino",
                         "Hispanic/Latino", Race))

  return(df)
}

aggregate_age <- function(df){
  df <- df %>%
    filter(Age == "10-14" | Age == "20-24") %>%
    group_by(Year, MCD, MCD.Code, UCD, UCD.Code, Race, Race.Code) %>%
    summarise(Deaths = sum(Deaths),
              Population = sum(Population)) %>%
    ungroup() %>%
    mutate(Age = "10-14, 20-24",
           Death_Rate = Deaths/Population*1e5) %>%
    select(Year, Age, MCD, MCD.Code, UCD, UCD.Code, 
           Deaths, Population, Death_Rate, Race, Race.Code)
}

## I also need a function of aggregation, I think I also need to add 10-24 data(or maybe not)

## merge and aggregate all the files and save
process_all <- function(file_paths) {
  cleaned_dfs <- lapply(file_paths, function(file) {
    raw_data <- extract_data(file)
    transform_data(raw_data)
  })
  
  # Combine all cleaned datasets
  final_df <- bind_rows(cleaned_dfs)
  
  return(final_df)
}

file_list <- list.files(path = "sample data", full.names = TRUE)

all <- process_all(file_list)
age2 <- aggregate_age(all)
all <- rbind(all, age2)


ui <- fluidPage(
  div(h2("Drug overdose mortality among youth aged 10-24 in the United States", style = "text-align: center;"),
      style = "font-size: 17px; font-family: 'Arial, sans-serif';"),
  titlePanel(div(HTML("Data Sourced from Centers for Disease Control and Prevention. CDC WONDER Database. Accessed Mar 6, 2025."),
                 style = "font-size: 13px; font-family: 'Arial, sans-serif';")),
  sidebarLayout(
    sidebarPanel(
      div(
        style = "font-family: 'Arial, sans-serif';",
        radioButtons("Result",
                     h4("Outcome"),
                     choices = list("original", "log", "sqrt"),
                     selected = "original",
                     inline = TRUE),
        fluidRow(
          column(8,
                 checkboxGroupInput("Substances", 
                                    h4("Available Substances"), 
                                    choices = list("ALL", 
                                                   "Heroin", 
                                                   "Methadone", 
                                                   "Cocaine",
                                                   "Natural and Semisynthetic Opioids" = "Natural and semisynthetic opioids",
                                                   "Illicitly Manufactured Fentanyls" = "Synthetic opioids excluding methadone", 
                                                   "Psychostimulants with Abuse Potential" = "Psychostimulants with abuse potential"),
                                    selected = "Synthetic opioids excluding methadone")
          ),
          column(4,
                 div(
                   style = "display: flex; align-items: center;
                   font-size: 12px; margin-top: 30px;",
                   prettySwitch(
                     inputId = "Aggregate1",
                     label = "Aggregate",
                     fill = TRUE,
                     inline = FALSE
                   )
                 )
          )
        ), 
        fluidRow(
          column(8,
                 checkboxGroupInput("UCD", 
                                    h4("Mechanism of Overdose Fatality"), 
                                    choices = list("ALL", 
                                                   "Unintentional", 
                                                   "Suicide",
                                                   "Homicide",
                                                   "Undetermined"),
                                    selected = c("ALL"))
          ),
          column(4,
                 div(
                   style = "display: flex; align-items: center;
                   font-size: 12px; margin-top: 30px;",
                   prettySwitch(
                     inputId = "Aggregate2",
                     label = "Aggregate",
                     fill = TRUE,
                     inline = FALSE
                   )
                 ))
        ),
        fluidRow(
          column(8,
                 checkboxGroupInput("Age_group", 
                                    h4("Age"), 
                                    choices = list("10-14", 
                                                   "15-19", 
                                                   "20-24"
                                    ),
                                    selected = c("10-14", "15-19", "20-24"),
                                    inline = TRUE)
          ),
          column(4,
                 div(
                   style = "display: flex; align-items: center;
                   font-size: 12px; margin-top: 30px;",
                   prettySwitch(
                     inputId = "Aggregate3",
                     label = "Aggregate",
                     fill = TRUE,
                     inline = FALSE)
                 )
          )
        ),
        fluidRow(
          column(8,
                 checkboxGroupInput("Race_group", 
                                    h4("Race/Ethnicity"), 
                                    choices = list("ALL" = "ALL",
                                                   "American Indian or Alaska Native, not Hispanic/Latino" = "1002-5", 
                                                   "Asian or Pacific Islander, not Hispanic/Latino" = "A-PI", 
                                                   "Black or African American, not Hispanic/Latino" = "2054-5",
                                                   "Hispanic/Latino" = "2135-2",
                                                   "White, not Hispanic/Latino" = "2106-3"
                                    ),
                                    selected = "ALL")
          ),
          column(4,
                 div(
                   style = "display: flex; align-items: center;
                   font-size: 12px; margin-top: 30px;",
                   prettySwitch(
                     inputId = "Aggregate4",
                     label = "Aggregate",
                     fill = TRUE,
                     inline = FALSE
                   )
                 )
          )
        ), 
        sliderInput("date_range", h4("Year Range"),
                    min = 1999, max = 2023, value = c(1999, 2023),
                    step = 1,
                    sep = ""),
        selectInput("death", h4("Measurement"),
                    choices = list("Death Rate" = "Death_Rate",
                                   "Death Amount" = "Deaths"),
                    selected = "Death_Rate")
      ),
      width = 3,
    ),
    mainPanel(
      uiOutput("warning_of_aggregate"),
      plotlyOutput("Trend_plot"), 
      div(
        style = "margin-top: 30px;",
        fluidRow(column(5,
                        radioButtons("plot_out", h5("Download Format"),
                                     choices = list("png", "svg", "jpeg", "webp"),
                                     selected = "png",
                                     inline = TRUE)),
                 column(5, 
                        textInput("file_name", h5("Name the Plot Manually"), 
                                  value = "Trend Plot for Drug Overdose Deaths")))
      ),
      div(HTML("Suggested citation: Visualization created by Chen, Wang & Bell, 2025"),
          style = "font-size: 13px; font-family: 'Arial, sans-serif';")
    )
  )
)

server <- function(input, output, session){
  
  # this is the warning when data is aggregated
  
  output$warning_of_aggregate <- renderUI({
    if (input$Aggregate1) {
      p(style = "color: red;
        font-size: 15px;
        font-family: 'Arial, sans-serif'; 
        font-style: italic;",
        "One death certificate may contain multiple substances as multiple causes of death.")
    }
  })
  
  # can't choose all with other options when data is aggregated
  
  observe({
    if ("ALL" %in% input$Substances && length(input$Substances) > 1
        && input$Aggregate1) {
      showModal(modalDialog(
        title = "Warning",
        "You cannot select 'ALL' with other options. Only 'ALL' will be selected.",
        easyClose = TRUE,
        footer = NULL
      ))
      
      updateCheckboxGroupInput(session, "Substances", selected = "ALL")
    }
  })
  
  observe({
    if ("ALL" %in% input$UCD && length(input$UCD) > 1
        && input$Aggregate2) {
      showModal(modalDialog(
        title = "Warning",
        "You cannot select 'ALL' with other options. Only 'ALL' will be selected.",
        easyClose = TRUE,
        footer = NULL
      ))
      
      updateCheckboxGroupInput(session, "UCD", selected = "ALL")
    }
  })
  
  observe({
    if ("ALL" %in% input$Race_group && length(input$Race_group) > 1
        && input$Aggregate4) {
      showModal(modalDialog(
        title = "Warning",
        "You cannot select 'ALL' with other options. Only 'ALL' will be selected.",
        easyClose = TRUE,
        footer = NULL
      ))
      
      updateCheckboxGroupInput(session, "Race_group", selected = "ALL")
    }
  })
  
  outdata <- reactive({
    
    if(input$Aggregate3 &
       setequal(input$Age_group, c("10-14", "15-19", "20-24"))){
      data <- all %>%
        filter(Age == "10-24", 
               Year >= input$date_range[1], Year <= input$date_range[2],
               MCD %in% input$Substances,
               UCD %in% input$UCD,
               Race.Code %in% input$Race_group)
    } else if(input$Aggregate3 &
              setequal(input$Age_group, c("15-19", "20-24"))){
      data <- all %>%
        filter(Age == "15-24",
               Year >= input$date_range[1], Year <= input$date_range[2],
               MCD %in% input$Substances,
               UCD %in% input$UCD,
               Race.Code %in% input$Race_group)
    } else if(input$Aggregate3 &
              setequal(input$Age_group, c("10-14", "20-24"))){
      data <- all %>%
        filter(Age == "10-14, 20-24",
               Year >= input$date_range[1], Year <= input$date_range[2],
               MCD %in% input$Substances,
               UCD %in% input$UCD,
               Race.Code %in% input$Race_group)
    }
    else if(input$Aggregate3 &
             setequal(input$Age_group, c("10-14", "15-19"))){
      data <- all %>%
        filter(Age == "10-19",
               Year >= input$date_range[1], Year <= input$date_range[2],
               MCD %in% input$Substances,
               UCD %in% input$UCD,
               Race.Code %in% input$Race_group)
    }
    else {
      data <- all %>%
        filter(Year >= input$date_range[1], Year <= input$date_range[2],
               Age %in% input$Age_group,
               MCD %in% input$Substances,
               UCD %in% input$UCD,
               Race.Code %in% input$Race_group)
    }
    
    if (all(!input$Aggregate1, !input$Aggregate2, 
            !input$Aggregate3, !input$Aggregate4)) {
      return(data)
    }
    
    if(input$Aggregate1){
      data <- data %>%
        filter(case_when(
          "ALL" %in% input$Substances ~ MCD == "ALL",
          TRUE ~ TRUE,
        )) %>% 
        group_by(Year, Age, UCD, Race, Race.Code) %>%
        summarise(Deaths = sum(Deaths),
                  Population = first(Population)) %>%
        mutate(Death_Rate = Deaths/Population*1e5,
               MCD = "",
               MCD.Code = "ALL"
        )
    }
    
    if(input$Aggregate2){
      data <- data %>%
        filter(case_when(
          "ALL" %in% input$UCD ~ UCD == "ALL",
          TRUE ~ TRUE,
        )) %>% 
        group_by(Year, Age, MCD, MCD.Code, Race, Race.Code) %>%
        summarise(Deaths = sum(Deaths),
                  Population = first(Population)) %>%
        mutate(Death_Rate = Deaths/Population*1e5,
               UCD = "")
    }
    
    if(input$Aggregate4){
      data <- data %>%
        filter(case_when(
          "ALL" %in% input$Race.Code ~ Race.Code == "ALL",
          TRUE ~ TRUE,
        )) %>%
        group_by(Year, Age, MCD, MCD.Code, UCD) %>%
        summarise(Deaths = sum(Deaths),
                  Population = sum(Population)) %>%
        mutate(Death_Rate = Deaths/Population*1e5,
               Race = "",
               Race.Code = "NULL")
    }
    
    data
    
  })
  
  output$Trend_plot <- renderPlotly({
    
    plot_data <- outdata()
    
    selected_out <- input$death
    yName <- case_when(
      selected_out == "Death_Rate" & input$Result == "original" ~ "Death Rate per 100 000",
      selected_out == "Death_Rate" & input$Result == "log" ~ "log(Death Rate per 100 000 + 1)",
      selected_out == "Death_Rate" & input$Result == "sqrt" ~ "sqrt(Death Rate per 100 000)",
      selected_out == "Deaths" & input$Result == "original" ~ "Deaths",
      selected_out == "Deaths" & input$Result == "log" ~ "log(Deaths + 1)",
      selected_out == "Deaths" & input$Result == "sqrt" ~ "Square Root of Deaths)"
    )
    yTitle <- case_when(
      selected_out == "Death_Rate" & input$Result == "original" ~ "Death Rate",
      selected_out == "Death_Rate" & input$Result == "log" ~ "log(Death Rate + 1)",
      selected_out == "Death_Rate" & input$Result == "sqrt" ~ "Square Root of Death Rate",
      selected_out == "Deaths" & input$Result == "original" ~ "Deaths",
      selected_out == "Deaths" & input$Result == "log" ~ "log(Deaths + 1)",
      selected_out == "Deaths" & input$Result == "sqrt" ~ "Square Root of Deaths)"
    )
    yTitle <- ifelse(input$file_name == "Trend Plot for Drug Overdose Deaths", yTitle, input$file_name)
    plot_data$transformed_out <- switch(input$Result, 
                                        original = plot_data[[selected_out]],
                                        log = log(plot_data[[selected_out]] + 1),
                                        sqrt = sqrt(plot_data[[selected_out]]))
    
    y_max <- max(plot_data$transformed_out)
    y_breaks <- seq(0, round(y_max), by = round(y_max) / 10)
    y_minor_breaks <- seq(0, round(y_max), by = round(y_max) / 50)
    
    plot_data <- plot_data %>%
      mutate(Race = ifelse(Race %in% c("Hispanic/Latino", "ALL", ""), Race, paste0(Race, ", not Hispanic/Latino")),
             Label =  paste0(Age, " ",
                             ifelse(MCD == "ALL", "", MCD), 
                             ifelse(MCD.Code == "ALL", "", paste0(" (", MCD.Code, ")")), 
                             ifelse(UCD == "ALL", "", paste(" ", UCD)), 
                             ifelse(Race == "ALL", "", Race)),
             hover_txt = case_when(input$Result == "original" ~ paste0(ifelse(input$death == "Death_Rate", "Death Rate", "Deaths"), ": ", round(transformed_out, 2)),
                                   input$Result == "log" ~ paste0("log(", ifelse(input$death == "Death_Rate", "Death Rate", "Deaths"), "+1): ", round(transformed_out, 2)),
                                   input$Result == "sqrt" ~ paste0("sqrt(", ifelse(input$death == "Death_Rate", "Death Rate", "Deaths"), "): ", round(transformed_out, 2))
             )
      )
    
    p <- ggplot(plot_data, aes_string(x = "Year", y = "transformed_out",
                                      group = "Label",
                                      color = "Label",
                                      text = "hover_txt")) +
      geom_line(size = 1) +
      geom_point(size = 2) + 
      labs(x = "Year", 
           y = yName,  
           title = ifelse(input$file_name == "Trend Plot for Drug Overdose Deaths", paste(
             "Drug overdose", yTitle , "of United States adolescents and young adults"
           ), yTitle),
           color = ""
      ) +
      scale_x_continuous(breaks = seq(input$date_range[1],
                                      input$date_range[2], by = 1)) + 
      scale_y_continuous(
        breaks = y_breaks,
        minor_breaks = y_minor_breaks
      ) + 
      theme_minimal()
    
    ggplotly(p, tooltip = "text") %>% plotly::config(
      modeBarButtonsToRemove = list("sendDataToCloud", "lasso2d"),
      toImageButtonOptions = list(
        format = input$plot_out, 
        filename = input$file_name,
        height = 800, # Height in pixels
        width = 1200, # Width in pixels
        scale = 4 # Multiply title/legend/axis/canvas sizes by this factor
      )
    ) %>%
      plotly::layout(
        legend = list(orientation = "v", y = 1, x = 0),
        xaxis = list(title = "<span style='font-size:15pt;'>Year</span>",
                     color ="black", tickangle = 45, 
                     standoff = 15),
        annotations = list(text = 'Visualization created by Chen, Wang & Bell, 2025',
                           color = 'grey',
                           font = list(size = 10),
                           showarrow = FALSE,
                           xref = 'paper', x = -0.02,
                           yref = 'paper', y = -0.15)
      )
  })
} 

shinyApp(ui = ui, server = server)

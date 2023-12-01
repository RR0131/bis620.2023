# Shuangrui Chen (sc3237)
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# 0. Midterm scheduling

# 1. Clean up the table column names X
# 2. Allow multiple brief title keywords X
# 3. Create a histogram of the phase (almost)
# 4. Organize files.
# 5. Fix the Phase plot
# 6. Plot the concurrent studies (adding a feature/capability).

# Steps to adding a feature:
# 1. Specify the feature.
#   - What does it do?
#   - What will be shown?
# 2. Specify the interface
#   - Where should it go?
#   - Does it enhance the user experience?
# 3. Implement the feature without the UI
# 4. Integrate the feature.

source("ct-util.R")
max_num_studies = 1000

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with a text input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"),


  ##Problem3: Add a drop-down so that queries can be subsetted on sponsor type
  ##Interpretation: There are two types of sponsors: lead or collaborators.
  ##By default, the drop-down menu will provide all the trials which include both sponsor types.
  ##If we click on "Lead", only trials sponsored by a lead will show up.
  ##If we click on "Collaborator", only trials sponsored by collaborators will show up.

      selectInput(
        inputId = "sponsor_type",
        label = 'Choose sponsor type:',
        choices = c('All' = 'all', 'Lead' = 'lead', 'Collaborator' = 'collaborator')),

   textInput("bins", "Number of Bins for Condition Plot"),

  #Problem 4
  # Feature (1)	The Date Sorting Drop-down Menu
  # Add five different labels in the menu

  selectInput(
    inputId = 'date_order',
    label = 'Sort Trial Description by Dates: ',
    choices = c(
      'Default' = 'all',
      'Sort Starting Date in Ascending Order' = 'start_asc',
      'Sort Starting Date in Descending Order' = 'start_desc',
      'Sort End Date in Ascending Order' = 'end_asc',
      'Sort End Date in Descending Order' = 'end_desc'
    )
  )

    ),


    mainPanel(
      tabsetPanel(
         type = "tabs",
         tabPanel("Phase", plotOutput("phase_plot")),
         tabPanel("Concurrent", plotOutput("concurrent_plot")),
         tabPanel("Condition",plotOutput("condition_plot")),
       ),
      tabsetPanel(
        type = "tabs",
        tabPanel("Trials Description", dataTableOutput("trial_table")),
        tabPanel("Condition of the Trials",dataTableOutput("condition_table")),
      ),
      textOutput("num_unique_trials")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  get_studies = reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
           strsplit(",") |>
           unlist() |>
           trimws()
      ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
    } else {
      ret = studies
    }
    print(input$date_order)

    # Problem 3
    # Add a drop-down so that queries can be subsetted on sponsor type
    final_ret <- head(ret, max_num_studies) |>
      collect()

    if (input$sponsor_type == 'lead') {
      type_sel <- sponsors |>
        filter(lead_or_collaborator == 'lead') |>
        select(nct_id) |>
        distinct() |>
        pull(nct_id)
      final_ret <- final_ret |>
        filter(nct_id %in% type_sel)
    } else if (input$sponsor_type == 'collaborator') {
      type_sel <- sponsors |>
        filter(lead_or_collaborator == 'collaborator') |>
        select(nct_id) |>
        distinct() |>
        pull(nct_id)
      final_ret <- final_ret |>
        filter(nct_id %in% type_sel)
    }

    final_ret



  })

  output$phase_plot = renderPlot({
    get_studies() |>
      plot_phase_histogram()
  })

  output$concurrent_plot = renderPlot({
    get_studies() |>
      select(start_date, completion_date) |>
      get_concurrent_trials() |>
      ggplot(aes(x = date, y = count)) +
        geom_line() +
        xlab("Date") +
        ylab("Count") +
        theme_bw()
  })

  output$condition_plot = renderPlot({
    get_studies() |>
      plot_condition_histogram(input$bins)
  })

  # Problem 4
  # Feature (2)	Trial Condition Table and Bin number controller


  output$trial_table = renderDataTable({
    ret <- get_studies() |>
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)

    #Problem 4
    #Feature (1)	The Date Sorting Drop-down Menu
    if (input$date_order == 'start_asc') {
      ret <- arrange(ret, `Start Date`)
    } else if (input$date_order == 'start_desc') {
      ret <- arrange(ret, desc(`Start Date`))
    } else if (input$date_order == 'end_asc') {
      ret <- arrange(ret, `Completion Date`)
    } else if (input$date_order == 'end_desc') {
      ret <- arrange(ret, desc(`Completion Date`))
    }

    ret
  })

  output$condition_table = renderDataTable({
    trail_sel <- get_studies() |>
      select(nct_id) |> distinct() |> pull(nct_id)
    res_out = conditions |>
      select(nct_id, name) |>
      filter(nct_id %in% trail_sel) |>
      rename(`Condition` = name) |>
      collect()
    res_out
  })


}


# Run the application
shinyApp(ui = ui, server = server)

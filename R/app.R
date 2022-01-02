# library(shiny)
# library(shinydashboard)
# library(adpain)
# library(targets)
# library(multinma)
# library(tidyverse)
# library(kableExtra)

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

adpain_app <- function(...) {

withr::with_dir(here::here(), {
  tar_load(oti_results)
  tar_load(o_nma)
  tar_load(oti_sof)
  tar_load(oti_pw)
})

# scripts
outcome_choices <-
  outcome_key$outcome_label

intervention_choices <-
  oti_sof %>%
  arrange(desc(participants)) %>%
  select(intervention) %>%
  distinct()

# eventually port to own project

ui <- dashboardPage(
  skin = "black",


  # header ------------------------------------------------------------------


  dashboardHeader(title = "Antidepressants... todo",
                  # puts sidebar toggle on right
                  titleWidth = "calc(100% - 44px)"),

  # sidebar -----------------------------------------------------------------

  dashboardSidebar(


# selectors ---------------------------------------------------------------


    shiny::selectInput(
      inputId = "outcome",
      label = "Outcome",
      selected = outcome_choices[1],
      choices = outcome_choices
    ),

selectInput(
  inputId = "int",
  label = "Intervention",
  choices = intervention_choices,
  selected = "duloxetine"
),


    shiny::selectInput(
      inputId = "tp",
      label = "Timepoint",
      selected = "post_int",
      choices = c("post_int", "change_score", "follow_up")
    ),

    shiny::uiOutput("tp"),


# tabs --------------------------------------------------------------------


    shinydashboard::menuItem(
      "Summary of findings",
      tabName = "sof",
      icon = shiny::icon("project-diagram")
    )
# ,
#     shinydashboard::menuItem(
#       "Intervention meta-analyses",
#       tabName = "pw",
#       icon = shiny::icon("arrow-right")
#     )

  ),
  # body --------------------------------------------------------------------
  dashboardBody(
    shinyjs::useShinyjs(),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "sof",
                              shiny::column(4,
                                            shiny::tableOutput("pico"),
                                            shiny::plotOutput("net")),
                              shiny::column(8,
                                            shiny::tableOutput("sof"),
                                            column(6, plotOutput("ma_forest")),
                                            column(6, plotOutput("ma_funnel"))
                                            )
                              )
      # ,
      # shinydashboard::tabItem(tabName = "pw",
      #                         column(6, plotOutput("ma_forest")),
      #                         column(6, plotOutput("ma_funnel"))
      #                         )
      )

  )
)

server <- function(input, output, session) {
  # selectors ---------------------------------------------------------------

  this_outcome <- reactive({
    outcome_key %>%
      filter(outcome_label == input$outcome) %>%
      pull(outcome)
  })

  outcome_results <- reactive({
    oti_results %>%
      left_join(outcome_key) %>%
      filter(outcome_label == input$outcome,
             timepoint == input$tp
             )   })

  output$tp_choices <- shiny::renderUI({
    timepoints <-
      outcome_results %>%
      pull(timepoint) %>%
      unique()

      shiny::selectInput(
      inputId = "tp",
      label = "Timepoint",
      choices = timepoints,

    )
  })

# model -------------------------------------------------------------------

  this_mod <- reactive({
    index <-
      outcome_results() %>%
      pull(model_index) %>% unique()

    # assert here that model index is lenght 1?

    o_nma %>% pluck(index, "result")

  })


  # sof tab -----------------------------------------------------------------
  output$net <- renderPlot(plot(this_mod() %>% pluck("network")))

  output$pico <- render_gt({
    sof_pico(this_mod())
  })

  output$sof <- render_gt({
    oti_sof %>%
      filter(
        outcome == this_outcome(),
        timepoint == input$tp
      ) %>%
      arrange(desc(n)) %>%
      filter(participants > 200) %>%
      select(-outcome, -timepoint) %>%
      mutate(
        n = round(n,2),
        studies = round(studies,2)
      )
  })

# pw tab ------------------------------------------------------------------

  this_pw <- reactive({
    oti_pw %>%
      filter(outcome == this_outcome(),
             timepoint == input$tp,
             intervention == input$int
             ) %>%
      pull(pw_rma) %>%
      pluck(1, "rma_mv")

  })

  output$ma_forest <- renderPlot({
    forest(this_pw())
  })

  output$ma_funnel <- renderPlot({
    funnel(this_pw())
  })

}

shinyApp(ui, server, ...)
}

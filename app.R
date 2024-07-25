library(shiny)
library(shinydashboard)
library(DT)
library(fontawesome)

# Function to determine grade based on mark
markToGrade <- function(mark) {
  if (mark >= 70) return('First')
  else if (mark >= 60) return('Upper Second (2:1)')
  else if (mark >= 50) return('Lower Second (2:2)')
  else if (mark >= 40) return('Third')
  else return('Fail')
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Goldsmiths Psychology Undergraduate Grade Calculator"),
  dashboardSidebar(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=Atkinson+Hyperlegible:ital,wght@0,400;0,700;1,400;1,700&display=swap"),
      tags$style(HTML("
        body, .main-sidebar .sidebar { font-family: 'Atkinson Hyperlegible', sans-serif; }
        .main-sidebar .sidebar { font-size: 18px; line-height: 1.6; }
      "))
    ),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Year 2", tabName = "year2", icon = icon("2")),
      menuItem("Year 3", tabName = "year3", icon = icon("3"))
    ),
    tags$div(
      style = "padding: 15px; color: #fff; font-size: 12px;",
      HTML(paste0(
        "Content ", fa("creative-commons"), " 2024 by Gordon Wright<br>",
        "All content licensed under a ", fa("creative-commons"), " ",
        fa("creative-commons-by"), " ", fa("creative-commons-nc"), " ",
        '<a href="https://creativecommons.org/licenses/by-nc/4.0/" target="_blank" style="color: #fff;">',
        "Creative Commons Attribution-NonCommercial 4.0 International license (CC BY-NC 4.0)</a>"
      )),
      tags$hr(),
      HTML(paste0(
        'Made with <a href="https://github.com/posit-dev/positron/" target="_blank" style="color: #fff;">Positron,</a> ',
        fa("r-project"), ', and <a href="https://quarto.org/" target="_blank" style="color: #fff;">Quarto</a><br>',
        '<a href="https://github.com/LittleMonkeyLab/GoldPsychGradeCalc" target="_blank" style="color: #fff;"><br>',
        "View the source at ", fa("github"), " GitHub</a>"
      )),
      tags$hr(),
      tags$img(src = "LMLLOGO.png", height = "50px", style = "margin-bottom: 10px;"),
      HTML('<br>a <a href="https://littlemonkeylab.com" target="_blank" style="color: #fff;">LittleMonkeyLab</a> caper')
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Calmer color scheme */
        body { font-family: 'Atkinson Hyperlegible', sans-serif; }
        .skin-blue .main-header .logo { background-color: #0c6892; }
        .skin-blue .main-header .navbar { background-color: #0c6892; }
        .skin-blue .main-sidebar { background-color: #0c6892; }
        .content-wrapper { background-color: #ecf0f5; }
        .box.box-solid.box-primary { border-color: #0c6892; }
        .box.box-solid.box-primary > .box-header { background-color: #0c6892; }
      "))
    ),
    tabItems(
      # Year 2 tab
      tabItem(tabName = "year2",
        fluidRow(
          box(
            title = "Year 2 Courses",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            numericInput("populate_all_y2", "Populate all fields with:", value = NA, min = 0, max = 100),
            lapply(1:6, function(i) {
              fluidRow(
                column(3, h5(switch(i,
                  "Biological (15 credits)",
                  "Developmental (15 credits)",
                  "Social (15 credits)",
                  "Personality (15 credits)",
                  "Cognitive (15 credits)",
                  "Design & Analysis (15 credits)"
                ))),
                column(3, numericInput(paste0("y2_coursework_", i), "Coursework (15%)", value = NA, min = 0, max = 100)),
                column(3, numericInput(paste0("y2_exam_", i), "Exam (85%)", value = NA, min = 0, max = 100)),
                column(3, uiOutput(paste0("y2_total_", i)))
              )
            }),
            fluidRow(
              column(12, h5("Research Methods (30 credits)")),
              column(3, numericInput("y2_critical_7", "Critical Proposal (15%)", value = NA, min = 0, max = 100)),
              column(3, numericInput("y2_dissertation_7", "Mini-Dissertation (70%)", value = NA, min = 0, max = 100)),
              column(3, numericInput("y2_log_7", "CHIP Log (15%)", value = NA, min = 0, max = 100)),
              column(3, uiOutput("y2_total_7"))
            )
          )
        )
      ),
      
      # Year 3 tab
      tabItem(tabName = "year3",
        fluidRow(
          box(
            title = "Year 3 Courses",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            numericInput("populate_all_y3", "Populate all fields with:", value = NA, min = 0, max = 100),
            lapply(1:5, function(i) {
              fluidRow(
                column(3, h5(paste("Course", i, "(15 credits)"))),
                column(3, numericInput(paste0("y3_coursework_", i), "Coursework", value = NA, min = 0, max = 100)),
                column(3, numericInput(paste0("y3_exam_", i), "Exam", value = NA, min = 0, max = 100)),
                column(3, sliderInput(paste0("y3_weight_", i), "Coursework Weight", value = 50, min = 0, max = 100))
              )
            }),
            fluidRow(
              column(6, h5("Final Year Project (45 credits)")),
              column(6, numericInput("y3_project", "Mark", value = NA, min = 0, max = 100))
            )
          )
        )
      ),
      
      # Overview tab
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "Final Overview",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("overview_content")
          )
        ),
        fluidRow(
          box(
            title = "Course Summary",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("course_summary")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Populate all Year 2 fields
  observe({
    if (!is.na(input$populate_all_y2)) {
      for (i in 1:6) {
        updateNumericInput(session, paste0("y2_coursework_", i), value = input$populate_all_y2)
        updateNumericInput(session, paste0("y2_exam_", i), value = input$populate_all_y2)
      }
      updateNumericInput(session, "y2_critical_7", value = input$populate_all_y2)
      updateNumericInput(session, "y2_dissertation_7", value = input$populate_all_y2)
      updateNumericInput(session, "y2_log_7", value = input$populate_all_y2)
    }
  })
  
  # Populate all Year 3 fields
  observe({
    if (!is.na(input$populate_all_y3)) {
      for (i in 1:5) {
        updateNumericInput(session, paste0("y3_coursework_", i), value = input$populate_all_y3)
        updateNumericInput(session, paste0("y3_exam_", i), value = input$populate_all_y3)
      }
      updateNumericInput(session, "y3_project", value = input$populate_all_y3)
    }
  })
  
  # Calculate and display total for each Year 2 course
  lapply(1:6, function(i) {
    output[[paste0("y2_total_", i)]] <- renderUI({
      coursework <- input[[paste0("y2_coursework_", i)]]
      exam <- input[[paste0("y2_exam_", i)]]
      if (!is.na(coursework) && !is.na(exam)) {
        total <- 0.15 * coursework + 0.85 * exam
        h5(paste("Total:", round(total, 2), "%"))
      } else {
        h5("Total: N/A")
      }
    })
  })
  
  # Calculate and display total for Year 2 Research Methods
  output$y2_total_7 <- renderUI({
    critical <- input$y2_critical_7
    dissertation <- input$y2_dissertation_7
    log <- input$y2_log_7
    if (!is.na(critical) && !is.na(dissertation) && !is.na(log)) {
      total <- 0.15 * critical + 0.70 * dissertation + 0.15 * log
      h5(paste("Total:", round(total, 2), "%"))
    } else {
      h5("Total: N/A")
    }
  })
  
  # Calculate final results
  calculate_results <- reactive({
    # Year 2 calculations
    y2_marks <- c()
    for (i in 1:6) {
      coursework <- input[[paste0("y2_coursework_", i)]]
      exam <- input[[paste0("y2_exam_", i)]]
      if (!is.na(coursework) && !is.na(exam)) {
        y2_marks <- c(y2_marks, 0.15 * coursework + 0.85 * exam)
      }
    }
    
    # Research Methods (Course 7)
    critical7 <- input$y2_critical_7
    dissertation7 <- input$y2_dissertation_7
    log7 <- input$y2_log_7
    if (!is.na(critical7) && !is.na(dissertation7) && !is.na(log7)) {
      research_methods <- 0.15 * critical7 + 0.70 * dissertation7 + 0.15 * log7
      y2_marks <- c(y2_marks, research_methods)
    }
    
    y2_average <- if (length(y2_marks) > 0) mean(y2_marks) else NA
    y2_best_105 <- if (length(y2_marks) >= 7) {
      best_6 <- sort(y2_marks[1:6], decreasing = TRUE)[1:5]
      (sum(best_6) * 15 + y2_marks[7] * 30) / 105
    } else NA
    
    # Year 3 calculations
    y3_marks <- c()
    for (i in 1:5) {
      coursework <- input[[paste0("y3_coursework_", i)]]
      exam <- input[[paste0("y3_exam_", i)]]
      weight <- input[[paste0("y3_weight_", i)]] / 100
      if (!is.na(coursework) && !is.na(exam) && !is.na(weight)) {
        y3_marks <- c(y3_marks, weight * coursework + (1 - weight) * exam)
      }
    }
    
    project <- input$y3_project
    if (!is.na(project)) {
      y3_marks <- c(y3_marks, project)
    }
    
    y3_average <- if (length(y3_marks) > 0) mean(y3_marks) else NA
    y3_best_105 <- if (length(y3_marks) == 6) {
      best_4 <- sort(y3_marks[1:5], decreasing = TRUE)[1:4]
      (sum(best_4) * 15 + y3_marks[6] * 45) / 105
    } else NA
    
    # Final weighted average
    if (!is.na(y2_best_105) && !is.na(y3_best_105)) {
      final_average <- (3/8 * y2_best_105) + (5/8 * y3_best_105)
    } else if (!is.na(y2_best_105)) {
      final_average <- y2_best_105  # Use Y2 average for both if only Y2 is complete
    } else {
      final_average <- NA
    }
    
    final_grade <- if (!is.na(final_average)) markToGrade(final_average) else NA
    
    list(
      y2_marks = y2_marks,
      y2_average = y2_average,
      y2_best_105 = y2_best_105,
      y3_marks = y3_marks,
      y3_average = y3_average,
      y3_best_105 = y3_best_105,
      final_average = final_average,
      final_grade = final_grade
    )
  })
  
  # Create course summary
  create_course_summary <- reactive({
    y2_courses <- c("Biological", "Developmental", "Social", "Personality", "Cognitive", "Design & Analysis", "Research Methods")
    y3_courses <- c("Course 1", "Course 2", "Course 3", "Course 4", "Course 5", "Final Year Project")
    
    summary_data <- data.frame(
      Year = c(rep("Year 2", 7), rep("Year 3", 6)),
      Course = c(y2_courses, y3_courses),
      Element1 = c(rep("Coursework", 6), "Critical Proposal", rep("Coursework", 5), "Project"),
      Weight1 = c(rep("15%", 6), "15%", rep("Varies", 5), "100%"),
      Element2 = c(rep("Exam", 6), "Mini-Dissertation", rep("Exam", 5), NA),
      Weight2 = c(rep("85%", 6), "70%", rep("Varies", 5), NA),
      Element3 = c(rep(NA, 6), "CHIP Log", rep(NA, 6)),
      Weight3 = c(rep(NA, 6), "15%", rep(NA, 6)),
      Total = NA,
      Included = "Yes"
    )
    
    results <- calculate_results()
    
    # Check if any marks have been entered
    if (length(results$y2_marks) > 0 || length(results$y3_marks) > 0) {
      # Fill in the total marks
      if (length(results$y2_marks) > 0) {
        for (i in 1:min(6, length(results$y2_marks))) {
          summary_data$Total[i] <- results$y2_marks[i]
        }
        if (length(results$y2_marks) == 7) {
          summary_data$Total[7] <- results$y2_marks[7]
        }
      }
      
      if (length(results$y3_marks) > 0) {
        for (i in 1:min(5, length(results$y3_marks))) {
          summary_data$Total[i+7] <- results$y3_marks[i]
        }
        if (length(results$y3_marks) == 6) {
          summary_data$Total[13] <- results$y3_marks[6]
        }
      }
      
      # Determine which courses are excluded
      if (length(results$y2_marks) >= 6) {
        y2_excluded <- which.min(summary_data$Total[1:6])
        summary_data$Included[y2_excluded] <- "No"
      }
      if (length(results$y3_marks) >= 5) {
        y3_excluded <- which.min(summary_data$Total[8:12])
        summary_data$Included[y3_excluded + 7] <- "No"
      }
    }
    
    return(summary_data)
  })
  
  # Display course summary table
  output$course_summary <- renderDT({
    summary_data <- create_course_summary()
    
    if (all(is.na(summary_data$Total))) {
      return(NULL)  # Return NULL if no data is entered
    }
    
    datatable(summary_data, options = list(pageLength = 13, dom = 't')) %>%
      formatStyle('Included',
                  target = 'row',
                  backgroundColor = styleEqual(c("No", "Yes"), c('pink', 'white'))) %>%
      formatStyle('Total',
                  target = 'cell',
                  backgroundColor = styleInterval(c(40, 50, 60, 70), 
                                                  c('red', 'orange', 'yellow', 'lightgreen', 'green')),
                  formatter = "function(data, type, row, meta) {
                    return data === null ? 'N/A' : data.toFixed(2);
                  }")
  })
  
  # Display final overview
  output$overview_content <- renderUI({
    results <- calculate_results()
    
    if (is.na(results$y2_average) && is.na(results$y3_average)) {
      return(
        div(
          h4("Welcome to the Goldsmiths Psychology Undergraduate Grade Calculator!"),
          p("This tool will help you calculate your grades based on your coursework and exam results."),
          tags$ul(
            tags$li("Use the 'Year 2' tab to enter your second-year marks."),
            tags$li("Use the 'Year 3' tab to enter your third-year marks."),
            tags$li("Once you've entered your marks, return to this 'Overview' tab to see your results.")
          ),
          p("Remember, your final grade is calculated as follows:"),
          tags$ul(
            tags$li("Year 2 contributes 3/8 (37.5%) to the final grade"),
            tags$li("Year 3 contributes 5/8 (62.5%) to the final grade"),
            tags$li("For both years, only the best 105 credits are considered"),
            tags$li("Research Methods (30 credits) is always included in Year 2 calculation"),
            tags$li("Final Year Project (45 credits) is always included in Year 3 calculation")
          ),
          p("Start by entering your marks in the Year 2 and Year 3 tabs!")
        )
      )
    } else {
      tagList(
        fluidRow(
          column(6,
            h4("Year 2 Results"),
            if (!is.na(results$y2_average)) {
              tagList(
                p(strong("Average grade:"), round(results$y2_average, 2)),
                p(strong("Best 105 credits average:"), round(results$y2_best_105, 2))
              )
            } else {
              p("No data entered for Year 2")
            }
          ),
          column(6,
            h4("Year 3 Results"),
            if (!is.na(results$y3_average)) {
              tagList(
                p(strong("Average grade:"), round(results$y3_average, 2)),
                p(strong("Best 105 credits average:"), round(results$y3_best_105, 2))
              )
            } else {
              p("No data entered for Year 3")
            }
          )
        ),
        hr(),
        h4("Final Results"),
        if (!is.na(results$final_average)) {
          tagList(
            p(strong("Weighted average:"), round(results$final_average, 2)),
            p(strong("Final grade:"), results$final_grade)
          )
        } else {
          p("Insufficient data to calculate final grade")
        }
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
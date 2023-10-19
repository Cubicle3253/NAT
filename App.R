library(bslib)
library(dplyr)
library(ggplot2)
library(gt)
library(pins)
library(shiny)
library(shinyWidgets)
library(srvyr)
library(viridis)
library(waiter)

board <- board_connect()
nhanes <- board |> pin_read("ajo0/nhanes_a")

a_svy <- nhanes |>
  as_survey_design(id = SDMVPSU, strata = SDMVSTRA, nest = TRUE, weights = mec_wt)

by_choices <- c("Survey Period" = "SDDSRVYR",
  "Gender" = "RIAGENDR",
  "Age Group" = "age_group",
  "Hispanic Status & Race" = "RIDRETH1")
surveys <- nhanes |> distinct(SDDSRVYR) |> arrange(SDDSRVYR) |> pull()
genders <- c("Both" = "M|F",
  "Females Only" = "F",
  "Males Only" = "M")
a_select <- c("Blood Pressure - Diastolic (mm Hg)" = "bp_dia",
  "Blood Pressure - Systolic (mm Hg)" = "bp_sys",
  "Body Mass Index (kg/m^2)" = "BMXBMI",
  "Cholesterol - HDL (mg/dL)" = "LBDHDD",
  "Cholesterol - Total (mg/dL)" = "LBXTC",
  "Glycohemoglobin (%)" = "LBXGH",
  "Height (cm)" = "BMXHT",
  "Pulse" = "pulse",
  "Waist Circumference (cm)" = "BMXWAIST",
  "Weight (kg)" = "BMXWT")

ui <- page_navbar(title = "NAT - NHANES Analysis Tool", bg = "#31688e",
  nav_panel(title = "About",
    HTML("<p style='margin-top: 10%; text-align: center; font-size: 36px;'>"),
    HTML("Welcome to NAT!<BR>The NHANES Analysis Tool</p>")
  ),
  nav_panel("Adult Data", 
    layout_sidebar(
      autoWaiter(html = spin_3circles(), color = "#ffffff"),
      sidebar = sidebar(width = "25%",
        selectInput("a_var", "Select Variable:", choices = a_select),
        radioButtons("a_examine", "Examine:",
          choices = c("Means and Percentiles" = "S", "Prevalence" = "P")),
        conditionalPanel(
          condition = "input.a_examine == 'P'",
            sliderInput("a_value", "Values Equal or Greater Than:", min = 10, max = 90, value = 50)
        ),
        HTML("<hr>"),
        selectInput("a_by", "By:", choices = by_choices),
        sliderTextInput("a_survey", "For Surveys:",
          choices = surveys, grid = TRUE, selected = c(min(surveys), max(surveys))),
        radioButtons("a_gender", "Gender:", choices = genders),
        sliderInput("a_age", "Age Range:", min = 18, max = 80, value = c(18, 80)),
        HTML("<hr>")
      ),
      navset_card_tab(
        nav_panel("Graph", plotOutput("a_graph")),
        nav_panel("Data Table", gt_output("a_table"))
      )
    )
  ),
  nav_panel("Youth Data",
    HTML("<p style='margin-top: 10%; text-align: center; font-size: 24px;'>"),
    HTML("Only data for adults (18+) is currently available.<BR>Please check back later.</p>")
  ),
  nav_spacer(),
  nav_panel(HTML("<i>From Cubicle 3253</i>"))
)

server <- function(input, output, session) {

  a_svy2 <- reactive({
    a_svy |>
      filter(!is.na(.data[[input$a_var]]),
        !is.na(.data[[input$a_by]]),
        SDDSRVYR >= input$a_survey[1],
        SDDSRVYR <= input$a_survey[2],
        grepl(input$a_gender, RIAGENDR),
        RIDAGEYR %in% input$a_age[1]:input$a_age[2])
  })  

  a_summary <- reactive({ 
    a_svy2() |>
      group_by("Group" = .data[[input$a_by]]) |>
      summarize(
        n = n(),
        m = survey_mean(.data[[input$a_var]], vartype = "ci"),
        q = survey_quantile(.data[[input$a_var]],
          quantiles = c(0.10, 0.25, 0.50, 0.75, 0.90), vartype = NULL),
        sb_min = min(.data[[input$a_var]]),
        sb_max = max(.data[[input$a_var]])
      ) |>
      rowwise() |>
      mutate(
        IQR = q_q75 - q_q25,
        q_lf = max(sb_min, q_q25 - IQR*1.5),
        q_uf = min(sb_max, q_q75 + IQR*1.5)
      )
  })

  a_prev <- reactive({ 
    a_svy2() |>
      group_by("Group" = .data[[input$a_by]]) |>
      summarize(
        n = n(),
        p = survey_mean(.data[[input$a_var]] >= input$a_value, 
          proportion = TRUE, prop_method = "beta", vartype = "ci") * 100)
  })

  observeEvent(input$a_var, {
    v_df <- nhanes |>
      summarize(
        p05 = quantile(.data[[input$a_var]], probs = 0.05, names = FALSE, na.rm = TRUE),
        p50 = quantile(.data[[input$a_var]], probs = 0.50, names = FALSE, na.rm = TRUE),
        p95 = quantile(.data[[input$a_var]], probs = 0.95, names = FALSE, na.rm = TRUE)
      ) |>
      mutate(
        v_range = p95 - p05,
        p05 = if_else(v_range < 20, round(p05, 1), round(p05)), 
        p50 = if_else(v_range < 20, round(p50, 1), round(p50)),
        p95 = if_else(v_range < 20, round(p95, 1), round(p95)),
        step = if_else(v_range < 20, 0.1, 1)
      )        
    updateSliderInput(session, "a_value",
      min = v_df$p05, max = v_df$p95, value = v_df$p50, step = v_df$step)
  })

  output$a_graph <- renderPlot({
    if (input$a_examine == "S") {
      g <- ggplot(a_summary(),
        aes(x = Group, fill = Group)) +
        geom_boxplot(aes(ymin = q_lf, lower = q_q25, middle = q_q50, upper = q_q75, ymax = q_uf), 
          stat = "identity", alpha = 0.6, width = 0.85) +
        geom_point(aes(y = m), size = 6) +
        coord_flip() +
        labs(title = names(a_select)[a_select == input$a_var],
          x = element_blank(),
          y = element_blank(),
          caption = "Note: Estimates weighted using exam weights.")
    } else {
      g <- ggplot(a_prev(),
        aes(x = p, y = Group, fill = Group)) +
        geom_col(alpha = 0.6) +
        geom_errorbar(aes(xmin = p_low, xmax = p_upp), width = 0.25) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.025))) +
        labs(title = names(a_select)[a_select == input$a_var],
          subtitle = paste0("Prevalence of values ≥", input$a_value),
          x = "Percent",
          y = element_blank(),
          caption = "Note: Estimates weighted using exam weights.")
    }
    g +
      scale_fill_viridis_d() +
      theme_bw(base_size = 24) +
      theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 16),
        legend.position = "none")
  })

  output$a_table <- render_gt({
    if (input$a_examine == "S") {
      t <- a_summary() |>
        mutate(m_ci = sprintf("%.1f (%.1f-%.1f)", m, m_low, m_upp)) |>
        select(Group, n, m_ci, q_q10:q_q90) |>
        gt() |>
        cols_label(Group = "", n = "Sample Size", 
          m_ci = "Mean (95% CI)",
          q_q10 = md("10^th^"),
          q_q25 = md("25^th^"),
          q_q50 = md("50^th^"),
          q_q75 = md("75^th^"),
          q_q90 = md("90^th^")) |>
        tab_spanner(label = "Percentiles", columns = starts_with("q")) |>
        cols_align(columns = 2:8, align = "center") |>
        fmt_integer(columns = 2) |>
        fmt_number(columns = 4:8, decimals = 1) |>
        tab_header(title = names(a_select)[a_select == input$a_var])
    } else {      
      t <- a_prev() |>
        mutate(p_ci = sprintf("%.1f (%.1f-%.1f)", p, p_low, p_upp)) |>
        select(Group, n, p_ci) |>
        gt() |>
        cols_label(Group = "", n = "Sample Size", p_ci = "Percent (95% CI)") |>
        cols_align(columns = 2:3, align = "center") |>
        fmt_integer(columns = 2) |>
        tab_header(title = names(a_select)[a_select == input$a_var],
          subtitle = paste0("Prevalence of values ≥", input$a_value))
    }
    t |>
      tab_footnote(footnote = "Note: Estimates weighted using exam weights.",
        placement = "left") |>
      opt_row_striping(row_striping = TRUE) |>
      opt_horizontal_padding(scale = 3) |>
      tab_options(data_row.padding = px(2), table.font.size = 24)
  })

}

shinyApp(ui, server)

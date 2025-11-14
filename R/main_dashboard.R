library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(tidymodels)

# Load data from CSV file
if (file.exists('data/band_scores_with_frl.csv')) {
    csv_path <- 'data/band_scores_with_frl.csv'
} else if (file.exists('../data/band_scores_with_frl.csv')) {
    csv_path <- '../data/band_scores_with_frl.csv'
} else {
    stop("Cannot find data/band_scores_with_frl.csv. Please ensure you're running from the project directory.")
}

# Read and prepare data
df <- read.csv(csv_path, stringsAsFactors = FALSE) %>%
    mutate(
        # Convert Event_Date to Date type
        Event_Date = as.Date(Event_Date),
        # Convert Event_Class to factor with proper ordering
        Event_Class = factor(Event_Class, levels = c("1A", "2A", "3A", "4A"))
    )

# ===== Build FRL Prediction Model =====
# Prepare model data
model_data <- df %>%
    filter(!is.na(Final_Total),
           !is.na(Event_Class),
           !is.na(FRL_Percent),
           !is.na(School_Size)) %>%
    select(Final_Total, Event_Class, FRL_Percent, School_Size, School, Competition_Name) %>%
    mutate(Event_Class = factor(Event_Class))

# Split data
set.seed(123)
data_split <- initial_split(model_data, prop = 0.75, strata = Event_Class)
train_data <- training(data_split)
test_data <- testing(data_split)

# Build model
lm_spec <- linear_reg() %>%
    set_engine("lm") %>%
    set_mode("regression")

model_recipe <- recipe(Final_Total ~ Event_Class + FRL_Percent + School_Size,
                       data = train_data) %>%
    step_dummy(Event_Class)

model_workflow <- workflow() %>%
    add_model(lm_spec) %>%
    add_recipe(model_recipe)

fitted_model <- model_workflow %>%
    fit(data = train_data)

# Get predictions
test_predictions <- fitted_model %>%
    predict(new_data = test_data) %>%
    bind_cols(test_data)

train_predictions <- fitted_model %>%
    predict(new_data = train_data) %>%
    bind_cols(train_data)

# Calculate metrics
test_metrics <- test_predictions %>%
    metrics(truth = Final_Total, estimate = .pred)

train_metrics <- train_predictions %>%
    metrics(truth = Final_Total, estimate = .pred)

test_rsq <- cor(test_predictions$Final_Total, test_predictions$.pred)^2
train_rsq <- cor(train_predictions$Final_Total, train_predictions$.pred)^2

# Extract model coefficients
fitted_lm <- fitted_model %>%
    extract_fit_engine()

model_coefs <- tidy(fitted_lm) %>%
    mutate(
        significant = p.value < 0.05,
        sig_marker = ifelse(p.value < 0.001, "***",
                            ifelse(p.value < 0.01, "**",
                                   ifelse(p.value < 0.05, "*", "")))
    )

# ===== Setup for Dashboard v2 =====
year_choices_v2 <- df %>%
    mutate(Year = format(Event_Date, "%Y")) %>%
    distinct(Year) %>%
    arrange(desc(Year)) %>%
    pull(Year)

initial_competitions_v2 <- df %>%
    mutate(Year = format(Event_Date, "%Y")) %>%
    filter(Year == "2024") %>%
    distinct(Competition_Name) %>%
    arrange(Competition_Name) %>%
    pull(Competition_Name)

initial_dates_v2 <- df %>%
    filter(Competition_Name == "CBA 2A-4A Metro Regional 2024") %>%
    distinct(Event_Date) %>%
    arrange(desc(Event_Date)) %>%
    pull(Event_Date)

class_choices_v2 <- c("All Classes", "1A", "2A", "3A", "4A")

# ===== Setup for Scatter Dashboard =====
numeric_cols_scatter <- df %>%
    select(where(is.numeric)) %>%
    names()

categorical_cols_scatter <- df %>%
    select(where(~ is.character(.) || is.factor(.))) %>%
    select(-File) %>%
    names()

# ===== Setup for Correlation Dashboard =====
numeric_cols_corr <- df %>%
    select(where(is.numeric)) %>%
    names()

var_labels_corr <- setNames(
    numeric_cols_corr,
    gsub("_", " ", numeric_cols_corr)
)

default_vars_corr <- c("Music_Total", "Visual_Total", "General_Effect_Total", "Final_Total")

# ===== UI =====
ui <- navbarPage(
    title = "CBA Score Analysis Dashboard",

    # ===== Tab 1: Cover Page =====
    tabPanel("Home",
        fluidPage(
            style = "padding: 40px;",
            h1("Competition Band Assessment (CBA) Score Analysis Dashboard",
               style = "text-align: center; color: #2c3e50; margin-bottom: 30px;"),

            hr(),

            div(style = "max-width: 900px; margin: 0 auto;",
                h2("Welcome", style = "color: #34495e;"),
                p("This interactive dashboard provides comprehensive analysis tools for Colorado Band Association (CBA) competition scores.",
                  style = "font-size: 16px; line-height: 1.6;"),

                br(),

                h3("Dashboard Features:", style = "color: #34495e;"),

                tags$ul(style = "font-size: 16px; line-height: 1.8;",
                    tags$li(tags$b("Score Comparison:"), " Compare schools across competitions with interactive bar charts. Filter by year, competition, date, and class."),
                    tags$li(tags$b("Scatter Analysis:"), " Explore relationships between different score components with customizable scatter plots. Highlight specific schools and add trend lines."),
                    tags$li(tags$b("Correlation Matrix:"), " Identify patterns and relationships between score variables with an interactive correlation heatmap.")
                ),

                br(),

                h3("Data Summary:", style = "color: #34495e;"),

                div(style = "background-color: #ecf0f1; padding: 20px; border-radius: 8px;",
                    p(tags$b("Total Valid Records:"), nrow(df)),
                    p(tags$b("Schools:"), n_distinct(df$School)),
                    p(tags$b("Competitions:"), n_distinct(df$Competition_Name, na.rm = TRUE)),
                    p(tags$b("Date Range:"),
                      paste(format(min(df$Event_Date, na.rm = TRUE), "%Y-%m-%d"),
                            "to",
                            format(max(df$Event_Date, na.rm = TRUE), "%Y-%m-%d")))
                ),

                br(),

                h3("Getting Started:", style = "color: #34495e;"),
                p("Use the navigation bar above to explore different analysis tools. Each dashboard provides interactive controls to customize your view.",
                  style = "font-size: 16px; line-height: 1.6;"),

                br(),
                br(),

                p("Data source: PDF scoresheets processed via read_file.R",
                  style = "text-align: center; color: #7f8c8d; font-style: italic;")
            )
        )
    ),

    # ===== Tab 2: Score Comparison (dashboard_v2) =====
    tabPanel("Event Summary",
        fluidPage(
            sidebarLayout(
                sidebarPanel(
                    h4("Filter Data:"),

                    selectInput("year_v2",
                                "Year:",
                                choices = year_choices_v2,
                                selected = "2024"),

                    selectInput("competition_name_v2",
                                "Competition Name:",
                                choices = initial_competitions_v2,
                                selected = "CBA 2A-4A Metro Regional 2024"),

                    selectInput("event_date_v2",
                                "Event Date:",
                                choices = initial_dates_v2,
                                selected = initial_dates_v2[1]),

                    selectInput("event_class_v2",
                                "Event Class:",
                                choices = class_choices_v2,
                                selected = "All Classes"),

                    hr(),

                    h4("Instructions:"),
                    p("Click on a category name in the plot to order schools by that category."),

                    hr(),

                    h4("Currently ordering by:"),
                    textOutput("current_order_v2"),

                    hr(),

                    radioButtons("sort_direction_v2",
                                 "Sort Direction:",
                                 choices = c("Highest First" = "desc",
                                             "Lowest First" = "asc"),
                                 selected = "desc"),

                    checkboxInput("show_values_v2", "Show Score Values", FALSE),

                    width = 3
                ),

                mainPanel(
                    plotlyOutput("score_plot_v2", height = "600px"),
                    hr(),
                    h4("Data Table"),
                    DTOutput("data_table_v2")
                )
            )
        )
    ),

    # ===== Tab 3: Scatter Analysis =====
    tabPanel("Scatter Analysis",
        fluidPage(
            sidebarLayout(
                sidebarPanel(
                    h4("Plot Controls"),

                    selectInput("x_var_scatter",
                                "X-axis:",
                                choices = numeric_cols_scatter,
                                selected = "Music_Total"),

                    selectInput("y_var_scatter",
                                "Y-axis:",
                                choices = numeric_cols_scatter,
                                selected = "Visual_Total"),

                    hr(),

                    selectInput("color_var_scatter",
                                "Color by:",
                                choices = c("None" = "none", categorical_cols_scatter),
                                selected = "Event_Class"),

                    selectInput("shape_var_scatter",
                                "Shape by:",
                                choices = c("None" = "none", categorical_cols_scatter),
                                selected = "none"),

                    hr(),

                    h4("Highlight"),

                    selectInput("highlight_school_scatter",
                                "Highlight School:",
                                choices = c("None" = "none"),
                                selected = "none"),

                    hr(),

                    h4("Filters"),

                    selectInput("year_filter_scatter",
                                "Year:",
                                choices = c("All Years" = "all"),
                                selected = "all"),

                    selectInput("event_type_filter_scatter",
                                "Event Type:",
                                choices = c("All Types" = "all"),
                                selected = "all"),

                    hr(),

                    checkboxInput("show_trend_scatter",
                                  "Show Trend Line",
                                  value = TRUE),

                    checkboxInput("show_one_to_one_scatter",
                                  "Show 1:1 Line",
                                  value = TRUE),

                    hr(),

                    h4("Data Summary"),
                    textOutput("data_summary_scatter"),

                    width = 3
                ),

                mainPanel(
                    plotlyOutput("scatter_plot", height = "700px"),
                    hr(),
                    h4("Selected Points"),
                    verbatimTextOutput("click_info_scatter")
                )
            )
        )
    ),

    # ===== Tab 4: Correlation Matrix =====
    tabPanel("Correlation Matrix",
        fluidPage(
            sidebarLayout(
                sidebarPanel(
                    h4("Select Variables:"),
                    p("Choose at least 2 variables to display correlations."),

                    checkboxGroupInput("selected_vars_corr",
                                       "Score Variables:",
                                       choices = var_labels_corr,
                                       selected = default_vars_corr),

                    hr(),

                    actionButton("select_all_corr", "Select All"),
                    actionButton("select_none_corr", "Clear All"),

                    hr(),

                    h4("Display Options:"),

                    sliderInput("text_size_corr",
                                "Text Size:",
                                min = 2,
                                max = 5,
                                value = 3,
                                step = 0.5),

                    checkboxInput("show_values_corr", "Show Correlation Values", TRUE),

                    width = 3
                ),

                mainPanel(
                    plotOutput("correlation_plot", height = "700px"),
                    hr(),
                    h4("Strongest Correlations"),
                    p("Top 10 strongest correlations (excluding self-correlations):"),
                    tableOutput("top_correlations_corr"),
                    hr(),
                    textOutput("record_count_corr")
                )
            )
        )
    ),

    # ===== Tab 5: Class Comparison Boxplots =====
    tabPanel("Class Comparison",
        fluidPage(
            sidebarLayout(
                sidebarPanel(
                    h4("Display Options:"),

                    checkboxInput("show_points_box", "Show Individual Points", TRUE),

                    checkboxInput("show_mean_box", "Show Mean Values", TRUE),

                    hr(),

                    h4("Summary Statistics:"),
                    tableOutput("summary_stats_box"),

                    hr(),

                    textOutput("record_count_box"),

                    width = 3
                ),

                mainPanel(
                    plotlyOutput("boxplot_class", height = "600px"),
                    hr(),
                    h4("Data Distribution by Class"),
                    DTOutput("class_data_table")
                )
            )
        )
    ),

    # ===== Tab 6: FRL Prediction Model =====
    tabPanel("FRL Prediction Model",
        fluidPage(
            h2("Predicting Final Total Score from School Demographics"),
            p("This model predicts Final_Total using Event_Class, FRL_Percent (Free/Reduced Lunch %), and School_Size"),

            hr(),

            fluidRow(
                column(4,
                    div(style = "background-color: #ecf0f1; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                        h4("Model Performance", style = "margin-top: 0;"),
                        tableOutput("model_metrics_frl")
                    ),

                    div(style = "background-color: #ecf0f1; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                        h4("Dataset Summary", style = "margin-top: 0;"),
                        tableOutput("data_summary_frl")
                    ),

                    div(style = "background-color: #fff3cd; padding: 20px; border-radius: 8px;",
                        h4("Interactive Prediction", style = "margin-top: 0;"),
                        selectInput("pred_class_frl",
                                    "Event Class:",
                                    choices = c("1A", "2A", "3A", "4A"),
                                    selected = "3A"),

                        sliderInput("pred_frl_frl",
                                    "FRL Percent:",
                                    min = 0,
                                    max = 100,
                                    value = 30,
                                    step = 5),

                        sliderInput("pred_size_frl",
                                    "School Size:",
                                    min = 100,
                                    max = 3000,
                                    value = 1000,
                                    step = 100),

                        hr(),

                        h4("Predicted Final Total:", style = "color: #2c3e50;"),
                        verbatimTextOutput("prediction_result_frl")
                    )
                ),

                column(8,
                    tabsetPanel(
                        tabPanel("Predicted vs Actual",
                            plotlyOutput("pred_vs_actual_frl", height = "500px")
                        ),

                        tabPanel("Residuals",
                            plotlyOutput("residuals_plot_frl", height = "500px")
                        ),

                        tabPanel("Coefficients",
                            h4("Model Coefficients"),
                            p("Significance codes: *** p < 0.001, ** p < 0.01, * p < 0.05"),
                            tableOutput("coefficients_table_frl"),
                            hr(),
                            plotlyOutput("coefficients_plot_frl", height = "400px")
                        ),

                        tabPanel("Data Table",
                            h4("Test Set Predictions"),
                            DTOutput("predictions_table_frl")
                        )
                    )
                )
            )
        )
    )
)

# ===== SERVER =====
server <- function(input, output, session) {

    # ========================================
    # Tab 2: Score Comparison (dashboard_v2)
    # ========================================

    order_by_v2 <- reactiveVal("Music_Total")

    # Update competition_name choices when year changes
    shiny::observeEvent(input$year_v2, {
        available_competitions <- df %>%
            mutate(Year = format(Event_Date, "%Y")) %>%
            filter(Year == input$year_v2) %>%
            distinct(Competition_Name) %>%
            arrange(Competition_Name) %>%
            pull(Competition_Name)

        # Set default to CBA 2A-4A Metro Regional 2024 if it exists, otherwise use first competition
        default_competition <- if("CBA 2A-4A Metro Regional 2024" %in% available_competitions) {
            "CBA 2A-4A Metro Regional 2024"
        } else {
            available_competitions[1]
        }

        updateSelectInput(session, "competition_name_v2",
                          choices = available_competitions,
                          selected = default_competition)
    }, ignoreNULL = FALSE)

    # Update event_date choices when competition_name changes
    shiny::observeEvent(input$competition_name_v2, {
        available_dates <- df %>%
            filter(Competition_Name == input$competition_name_v2) %>%
            distinct(Event_Date) %>%
            arrange(desc(Event_Date)) %>%
            pull(Event_Date)

        updateSelectInput(session, "event_date_v2",
                          choices = available_dates,
                          selected = available_dates[1])
    }, ignoreNULL = FALSE)

    output$current_order_v2 <- renderText({
        gsub("_", " ", order_by_v2())
    })

    tmp_v2 <- reactive({
        filtered_data <- df %>%
            filter(Competition_Name == input$competition_name_v2 &
                       Event_Date == input$event_date_v2)

        if(input$event_class_v2 != "All Classes") {
            filtered_data <- filtered_data %>%
                filter(Event_Class == input$event_class_v2)
        }

        filtered_data %>%
            select(School, Music_Total, Visual_Total, Visual_Eff_Tot, Music_Eff_Avg_Total)
    })

    tmp_long_v2 <- reactive({
        tmp_v2() %>%
            pivot_longer(cols = c(Music_Total, Visual_Total, Visual_Eff_Tot, Music_Eff_Avg_Total),
                         names_to = "Category",
                         values_to = "Score")
    })

    ordered_data_v2 <- reactive({
        if(input$sort_direction_v2 == "desc") {
            tmp_v2() %>% arrange(desc(.data[[order_by_v2()]]))
        } else {
            tmp_v2() %>% arrange(.data[[order_by_v2()]])
        }
    })

    output$score_plot_v2 <- renderPlotly({
        school_order <- ordered_data_v2() %>% pull(School)

        plot_data <- tmp_long_v2() %>%
            mutate(School = factor(School, levels = school_order),
                   Category_Display = gsub("_", " ", Category))

        p <- plot_ly(height = 600)

        categories <- unique(plot_data$Category)
        colors <- c("Music_Total" = "#3b82f6",
                    "Visual_Total" = "#10b981",
                    "Visual_Eff_Tot" = "#f59e0b",
                    "Music_Eff_Avg_Total" = "#8b5cf6")

        for(cat in categories) {
            cat_data <- plot_data %>% filter(Category == cat)

            p <- p %>%
                add_trace(
                    data = cat_data,
                    x = ~Score,
                    y = ~School,
                    type = 'bar',
                    orientation = 'h',
                    name = gsub("_", " ", cat),
                    marker = list(color = colors[cat]),
                    text = ~paste0(School, "<br>", gsub("_", " ", Category),
                                   ": ", round(Score, 2)),
                    hoverinfo = 'text',
                    customdata = cat
                )
        }

        p <- p %>%
            layout(
                title = paste("Schools Ordered by", gsub("_", " ", order_by_v2())),
                xaxis = list(title = "Score"),
                yaxis = list(title = ""),
                barmode = 'group',
                showlegend = TRUE,
                legend = list(
                    orientation = "h",
                    x = 0.5,
                    xanchor = "center",
                    y = -0.1
                )
            )

        p
    })

    shiny::observeEvent(event_data("plotly_click"), {
        click_data <- event_data("plotly_click")
        if(!is.null(click_data)) {
            trace_num <- click_data$curveNumber + 1
            categories <- c("Music_Total", "Visual_Total", "Visual_Eff_Tot", "Music_Eff_Avg_Total")
            if(trace_num <= length(categories)) {
                order_by_v2(categories[trace_num])
            }
        }
    })

    output$data_table_v2 <- renderDT({
        ordered_data_v2() %>%
            datatable(options = list(pageLength = 10, scrollX = TRUE),
                      rownames = FALSE) %>%
            formatRound(columns = c("Music_Total", "Visual_Total", "Visual_Eff_Tot", "Music_Eff_Avg_Total"),
                        digits = 2)
    })

    # ========================================
    # Tab 3: Scatter Analysis
    # ========================================

    # Populate year filter dynamically
    shiny::observe({
        years <- df %>%
            mutate(Year = format(Event_Date, "%Y")) %>%
            pull(Year) %>%
            unique() %>%
            sort(decreasing = TRUE)

        year_choices <- c("All Years" = "all", setNames(years, years))
        updateSelectInput(session, "year_filter_scatter", choices = year_choices)
    })

    # Populate event type filter dynamically
    shiny::observe({
        event_types <- df %>%
            pull(Event_Type) %>%
            unique() %>%
            sort()

        event_type_choices <- c("All Types" = "all", setNames(event_types, event_types))
        updateSelectInput(session, "event_type_filter_scatter", choices = event_type_choices)
    })

    # Populate school highlight dropdown dynamically
    shiny::observe({
        schools <- df %>%
            pull(School) %>%
            unique() %>%
            sort()

        school_choices <- c("None" = "none", setNames(schools, schools))
        updateSelectInput(session, "highlight_school_scatter", choices = school_choices)
    })

    # Reactive dataset with selected variables and filters
    plot_data_scatter <- reactive({
        cols_to_select <- c("School", "Competition_Name", "Event_Date", "Event_Type",
                            input$x_var_scatter, input$y_var_scatter)

        if (input$color_var_scatter != "none") {
            cols_to_select <- c(cols_to_select, input$color_var_scatter)
        }
        if (input$shape_var_scatter != "none" && input$shape_var_scatter != input$color_var_scatter) {
            cols_to_select <- c(cols_to_select, input$shape_var_scatter)
        }

        cols_to_select <- unique(cols_to_select)

        data <- df %>%
            mutate(Year = format(Event_Date, "%Y")) %>%
            select(all_of(cols_to_select), Year) %>%
            rename(x = all_of(input$x_var_scatter),
                   y = all_of(input$y_var_scatter))

        if (input$year_filter_scatter != "all") {
            data <- data %>% filter(Year == input$year_filter_scatter)
        }

        if (input$event_type_filter_scatter != "all") {
            data <- data %>% filter(Event_Type == input$event_type_filter_scatter)
        }

        if (input$color_var_scatter != "none") {
            data <- data %>%
                mutate(color_group = as.factor(.data[[input$color_var_scatter]]))
        } else {
            data <- data %>%
                mutate(color_group = factor("All"))
        }

        if (input$shape_var_scatter != "none") {
            data <- data %>%
                mutate(shape_group = as.factor(.data[[input$shape_var_scatter]]))
        } else {
            data <- data %>%
                mutate(shape_group = factor("All"))
        }

        data %>% filter(!is.na(x) & !is.na(y))
    })

    output$scatter_plot <- renderPlotly({
        data <- plot_data_scatter()

        data <- data %>%
            mutate(hover_text = paste0(
                "<b>", School, "</b><br>",
                "Competition: ", Competition_Name, "<br>",
                "Date: ", Event_Date, "<br>",
                input$x_var_scatter, ": ", round(x, 2), "<br>",
                input$y_var_scatter, ": ", round(y, 2),
                if (input$color_var_scatter != "none") paste0("<br>", input$color_var_scatter, ": ", color_group) else "",
                if (input$shape_var_scatter != "none") paste0("<br>", input$shape_var_scatter, ": ", shape_group) else ""
            ))

        p <- plot_ly(data,
                     x = ~x,
                     y = ~y,
                     type = 'scatter',
                     mode = 'markers',
                     color = ~color_group,
                     symbol = ~shape_group,
                     symbols = c('circle', 'square', 'diamond', 'cross', 'x',
                                 'triangle-up', 'triangle-down', 'pentagon', 'hexagon'),
                     text = ~hover_text,
                     hoverinfo = 'text',
                     marker = list(
                         size = 10,
                         opacity = 0.7,
                         line = list(width = 1, color = 'white')
                     ))

        if (input$highlight_school_scatter != "none") {
            highlighted_data <- data %>%
                filter(School == input$highlight_school_scatter)

            if (nrow(highlighted_data) > 0) {
                highlighted_data <- highlighted_data %>%
                    mutate(highlight_hover = paste0(
                        "<b>*** ", School, " ***</b><br>",
                        "Competition: ", Competition_Name, "<br>",
                        "Date: ", Event_Date, "<br>",
                        input$x_var_scatter, ": ", round(x, 2), "<br>",
                        input$y_var_scatter, ": ", round(y, 2),
                        if (input$color_var_scatter != "none") paste0("<br>", input$color_var_scatter, ": ", color_group) else "",
                        if (input$shape_var_scatter != "none") paste0("<br>", input$shape_var_scatter, ": ", shape_group) else ""
                    ))

                p <- p %>%
                    add_trace(
                        data = highlighted_data,
                        x = ~x,
                        y = ~y,
                        type = 'scatter',
                        mode = 'markers',
                        name = paste0("★ ", input$highlight_school_scatter),
                        text = ~highlight_hover,
                        hoverinfo = 'text',
                        marker = list(
                            size = 16,
                            color = 'gold',
                            opacity = 1,
                            line = list(width = 3, color = 'red')
                        ),
                        showlegend = TRUE,
                        inherit = FALSE
                    )
            }
        }

        if (input$show_one_to_one_scatter) {
            all_values <- c(data$x, data$y)
            min_val <- min(all_values, na.rm = TRUE)
            max_val <- max(all_values, na.rm = TRUE)

            p <- p %>%
                add_trace(
                    x = c(min_val, max_val),
                    y = c(min_val, max_val),
                    type = 'scatter',
                    mode = 'lines',
                    name = '1:1 Line',
                    line = list(color = 'gray', dash = 'dot', width = 2),
                    hoverinfo = 'skip',
                    showlegend = TRUE,
                    inherit = FALSE
                )
        }

        if (input$show_trend_scatter) {
            fit <- lm(y ~ x, data = data)
            x_range <- seq(min(data$x, na.rm = TRUE),
                           max(data$x, na.rm = TRUE),
                           length.out = 100)
            y_pred <- predict(fit, newdata = data.frame(x = x_range))

            r_squared <- summary(fit)$r.squared

            p <- p %>%
                add_trace(
                    x = x_range,
                    y = y_pred,
                    type = 'scatter',
                    mode = 'lines',
                    name = paste0('Trend (R² = ', round(r_squared, 3), ')'),
                    line = list(color = 'red', dash = 'dash'),
                    hoverinfo = 'skip',
                    showlegend = TRUE,
                    inherit = FALSE
                )
        }

        p <- p %>%
            layout(
                title = paste(input$y_var_scatter, "vs", input$x_var_scatter),
                xaxis = list(title = gsub("_", " ", input$x_var_scatter)),
                yaxis = list(title = gsub("_", " ", input$y_var_scatter)),
                legend = list(
                    orientation = "v",
                    x = 1.02,
                    y = 1,
                    xanchor = "left"
                ),
                hovermode = 'closest'
            )

        p
    })

    output$data_summary_scatter <- renderText({
        data <- plot_data_scatter()

        filter_text <- ""
        if (input$year_filter_scatter != "all") {
            filter_text <- paste0(filter_text, "Year: ", input$year_filter_scatter, "\n")
        }
        if (input$event_type_filter_scatter != "all") {
            filter_text <- paste0(filter_text, "Type: ", input$event_type_filter_scatter, "\n")
        }

        paste0(
            filter_text,
            "Points: ", nrow(data), "\n",
            "Schools: ", n_distinct(data$School), "\n",
            "X range: ", round(min(data$x, na.rm = TRUE), 2), " - ",
            round(max(data$x, na.rm = TRUE), 2), "\n",
            "Y range: ", round(min(data$y, na.rm = TRUE), 2), " - ",
            round(max(data$y, na.rm = TRUE), 2)
        )
    })

    output$click_info_scatter <- renderPrint({
        click_data <- event_data("plotly_click")
        if (is.null(click_data)) {
            cat("Click on a point to see details")
        } else {
            data <- plot_data_scatter()
            point_idx <- click_data$pointNumber + 1
            selected_point <- data[point_idx, ]

            cat("School:", selected_point$School, "\n")
            cat("Competition:", selected_point$Competition_Name, "\n")
            cat("Date:", as.character(selected_point$Event_Date), "\n")
            cat(input$x_var_scatter, ":", round(selected_point$x, 2), "\n")
            cat(input$y_var_scatter, ":", round(selected_point$y, 2), "\n")
            if (input$color_var_scatter != "none") {
                cat(input$color_var_scatter, ":", as.character(selected_point$color_group), "\n")
            }
            if (input$shape_var_scatter != "none") {
                cat(input$shape_var_scatter, ":", as.character(selected_point$shape_group), "\n")
            }
        }
    })

    # ========================================
    # Tab 4: Correlation Matrix
    # ========================================

    shiny::observeEvent(input$select_all_corr, {
        updateCheckboxGroupInput(session, "selected_vars_corr",
                                  selected = numeric_cols_corr)
    })

    shiny::observeEvent(input$select_none_corr, {
        updateCheckboxGroupInput(session, "selected_vars_corr",
                                  selected = character(0))
    })

    cor_data_corr <- reactive({
        req(length(input$selected_vars_corr) >= 2)

        cor_matrix <- df %>%
            select(all_of(input$selected_vars_corr)) %>%
            cor(use = "complete.obs")

        cor_long <- cor_matrix %>%
            as.data.frame() %>%
            mutate(Var1 = rownames(.)) %>%
            pivot_longer(cols = -Var1, names_to = "Var2", values_to = "Correlation")

        var_order <- input$selected_vars_corr
        cor_long <- cor_long %>%
            mutate(
                Var1 = factor(Var1, levels = var_order),
                Var2 = factor(Var2, levels = rev(var_order))
            )

        cor_long
    })

    output$correlation_plot <- renderPlot({
        req(length(input$selected_vars_corr) >= 2)

        cor_long <- cor_data_corr()

        p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
            geom_tile(color = "white", linewidth = 0.5) +
            scale_fill_gradient2(
                low = "#3B4CC0",
                mid = "#F7F7F7",
                high = "#B40426",
                midpoint = 0,
                limit = c(-1, 1),
                name = "Correlation"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
                axis.text.y = element_text(size = 10),
                axis.title = element_blank(),
                panel.grid = element_blank(),
                legend.position = "right",
                plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 10, hjust = 0.5)
            ) +
            labs(
                title = "Correlation Matrix of Band Competition Scores",
                subtitle = paste0("Based on ", nrow(df), " valid records")
            ) +
            coord_fixed()

        if(input$show_values_corr) {
            p <- p + geom_text(aes(label = sprintf("%.2f", Correlation)),
                               color = "black", size = input$text_size_corr)
        }

        p
    })

    output$top_correlations_corr <- renderTable({
        req(length(input$selected_vars_corr) >= 2)

        cor_long <- cor_data_corr()

        cor_long %>%
            filter(Var1 != Var2) %>%
            filter(as.character(Var1) < as.character(Var2)) %>%
            arrange(desc(abs(Correlation))) %>%
            head(10) %>%
            mutate(
                Correlation = round(Correlation, 3),
                Abs_Correlation = round(abs(Correlation), 3)
            ) %>%
            select(Variable_1 = Var1, Variable_2 = Var2,
                   Correlation, Abs_Correlation)
    })

    output$record_count_corr <- renderText({
        paste("Total valid records used:", nrow(df))
    })

    # ========================================
    # Tab 5: Class Comparison Boxplots
    # ========================================

    # Filter data to only include records with Final_Total
    boxplot_data <- reactive({
        df %>%
            filter(!is.na(Final_Total) & !is.na(Event_Class))
    })

    output$boxplot_class <- renderPlotly({
        data <- boxplot_data()

        # Create base boxplot
        p <- plot_ly(data,
                     y = ~Final_Total,
                     x = ~Event_Class,
                     type = "box",
                     name = "Distribution",
                     boxpoints = if(input$show_points_box) "all" else FALSE,
                     jitter = 0.3,
                     pointpos = -1.8,
                     marker = list(size = 4, opacity = 0.5),
                     line = list(width = 2),
                     fillcolor = "rgba(56, 108, 176, 0.5)",
                     text = ~paste0(
                         "<b>School:</b> ", School, "<br>",
                         "<b>Final Total:</b> ", round(Final_Total, 2), "<br>",
                         "<b>Competition:</b> ", Competition_Name, "<br>",
                         "<b>Date:</b> ", Event_Date
                     ),
                     hoverinfo = "text")

        # Add mean values if requested
        if (input$show_mean_box) {
            mean_data <- data %>%
                group_by(Event_Class) %>%
                summarise(Mean_Score = mean(Final_Total, na.rm = TRUE), .groups = 'drop')

            p <- p %>%
                add_trace(
                    data = mean_data,
                    x = ~Event_Class,
                    y = ~Mean_Score,
                    type = 'scatter',
                    mode = 'markers',
                    name = 'Mean',
                    marker = list(
                        size = 12,
                        color = 'red',
                        symbol = 'diamond',
                        line = list(width = 2, color = 'darkred')
                    ),
                    text = ~paste0("<b>Mean Final Total:</b> ", round(Mean_Score, 2)),
                    hoverinfo = 'text',
                    showlegend = TRUE
                )
        }

        p <- p %>%
            layout(
                title = "Final Total Score Distribution by Event Class",
                xaxis = list(title = "Event Class"),
                yaxis = list(title = "Final Total Score"),
                showlegend = TRUE,
                legend = list(
                    orientation = "h",
                    x = 0.5,
                    xanchor = "center",
                    y = -0.15
                ),
                hovermode = 'closest'
            )

        p
    })

    output$summary_stats_box <- renderTable({
        data <- boxplot_data()

        data %>%
            group_by(Event_Class) %>%
            summarise(
                Count = n(),
                Mean = round(mean(Final_Total, na.rm = TRUE), 2),
                Median = round(median(Final_Total, na.rm = TRUE), 2),
                SD = round(sd(Final_Total, na.rm = TRUE), 2),
                Min = round(min(Final_Total, na.rm = TRUE), 2),
                Max = round(max(Final_Total, na.rm = TRUE), 2),
                .groups = 'drop'
            )
    }, striped = TRUE, hover = TRUE)

    output$record_count_box <- renderText({
        data <- boxplot_data()
        total_records <- nrow(df)
        valid_records <- nrow(data)
        dropped_records <- total_records - valid_records

        paste0(
            "Valid records with Final_Total: ", valid_records, "\n",
            "Records dropped (missing Final_Total or Event_Class): ", dropped_records
        )
    })

    output$class_data_table <- renderDT({
        boxplot_data() %>%
            select(School, Event_Class, Competition_Name, Event_Date, Final_Total) %>%
            arrange(Event_Class, desc(Final_Total)) %>%
            datatable(
                options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    order = list(list(1, 'asc'), list(4, 'desc'))
                ),
                rownames = FALSE
            ) %>%
            formatRound('Final_Total', digits = 2)
    })

    # ========================================
    # Tab 6: FRL Prediction Model
    # ========================================

    output$model_metrics_frl <- renderTable({
        test_rmse <- test_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)
        train_rmse <- train_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)

        tibble(
            Metric = c("R² (Training)", "R² (Test)", "RMSE (Training)", "RMSE (Test)"),
            Value = c(
                round(train_rsq, 4),
                round(test_rsq, 4),
                round(train_rmse, 2),
                round(test_rmse, 2)
            )
        )
    }, striped = TRUE, hover = TRUE)

    output$data_summary_frl <- renderTable({
        tibble(
            Item = c("Total Records", "Training Set", "Test Set", "Predictors"),
            Value = c(
                nrow(model_data),
                nrow(train_data),
                nrow(test_data),
                "Event_Class, FRL_Percent, School_Size"
            )
        )
    }, striped = TRUE)

    output$pred_vs_actual_frl <- renderPlotly({
        all_preds <- bind_rows(
            train_predictions %>% mutate(Set = "Training"),
            test_predictions %>% mutate(Set = "Test")
        )

        p <- plot_ly(all_preds,
                     x = ~Final_Total,
                     y = ~.pred,
                     color = ~Set,
                     colors = c("Training" = "#3b82f6", "Test" = "#ef4444"),
                     type = 'scatter',
                     mode = 'markers',
                     text = ~paste0(
                         "<b>School:</b> ", School, "<br>",
                         "<b>Actual:</b> ", round(Final_Total, 2), "<br>",
                         "<b>Predicted:</b> ", round(.pred, 2), "<br>",
                         "<b>Event Class:</b> ", Event_Class, "<br>",
                         "<b>FRL%:</b> ", round(FRL_Percent, 1), "<br>",
                         "<b>School Size:</b> ", School_Size
                     ),
                     hoverinfo = 'text',
                     marker = list(size = 8, opacity = 0.6))

        # Add 1:1 line
        range_vals <- range(c(all_preds$Final_Total, all_preds$.pred), na.rm = TRUE)
        p <- p %>%
            add_trace(
                x = range_vals,
                y = range_vals,
                type = 'scatter',
                mode = 'lines',
                name = 'Perfect Prediction',
                line = list(color = 'gray', dash = 'dash', width = 2),
                hoverinfo = 'skip',
                showlegend = TRUE
            )

        p %>%
            layout(
                title = "Predicted vs Actual Final Total",
                xaxis = list(title = "Actual Final Total"),
                yaxis = list(title = "Predicted Final Total"),
                hovermode = 'closest'
            )
    })

    output$residuals_plot_frl <- renderPlotly({
        all_preds <- bind_rows(
            train_predictions %>% mutate(Set = "Training"),
            test_predictions %>% mutate(Set = "Test")
        ) %>%
            mutate(Residual = Final_Total - .pred)

        p <- plot_ly(all_preds,
                     x = ~.pred,
                     y = ~Residual,
                     color = ~Set,
                     colors = c("Training" = "#3b82f6", "Test" = "#ef4444"),
                     type = 'scatter',
                     mode = 'markers',
                     text = ~paste0(
                         "<b>School:</b> ", School, "<br>",
                         "<b>Predicted:</b> ", round(.pred, 2), "<br>",
                         "<b>Residual:</b> ", round(Residual, 2), "<br>",
                         "<b>Event Class:</b> ", Event_Class
                     ),
                     hoverinfo = 'text',
                     marker = list(size = 8, opacity = 0.6))

        # Add zero line
        p <- p %>%
            add_trace(
                x = range(all_preds$.pred, na.rm = TRUE),
                y = c(0, 0),
                type = 'scatter',
                mode = 'lines',
                name = 'Zero',
                line = list(color = 'gray', dash = 'dash', width = 2),
                hoverinfo = 'skip',
                showlegend = FALSE
            )

        p %>%
            layout(
                title = "Residual Plot (Actual - Predicted)",
                xaxis = list(title = "Predicted Final Total"),
                yaxis = list(title = "Residual"),
                hovermode = 'closest'
            )
    })

    output$coefficients_table_frl <- renderTable({
        model_coefs %>%
            mutate(
                term = gsub("Event_Class_X", "", term),
                Estimate = round(estimate, 4),
                `Std Error` = round(std.error, 4),
                `t value` = round(statistic, 3),
                `p value` = ifelse(p.value < 0.001, "< 0.001", round(p.value, 4)),
                Significance = sig_marker
            ) %>%
            select(Term = term, Estimate, `Std Error`, `t value`, `p value`, Significance)
    }, striped = TRUE, hover = TRUE)

    output$coefficients_plot_frl <- renderPlotly({
        coef_plot_data <- model_coefs %>%
            filter(term != "(Intercept)") %>%
            mutate(
                term = gsub("Event_Class_X", "", term),
                term = factor(term, levels = term[order(abs(estimate))])
            )

        plot_ly(coef_plot_data,
                y = ~term,
                x = ~estimate,
                type = 'bar',
                orientation = 'h',
                marker = list(
                    color = ~ifelse(significant, "#3b82f6", "#94a3b8")
                ),
                text = ~paste0(
                    "<b>", term, "</b><br>",
                    "Coefficient: ", round(estimate, 4), "<br>",
                    "p-value: ", ifelse(p.value < 0.001, "< 0.001", round(p.value, 4))
                ),
                hoverinfo = 'text') %>%
            layout(
                title = "Coefficient Magnitudes (Blue = Significant)",
                xaxis = list(title = "Coefficient Value"),
                yaxis = list(title = ""),
                showlegend = FALSE
            )
    })

    output$predictions_table_frl <- renderDT({
        test_predictions %>%
            mutate(
                Residual = Final_Total - .pred,
                Abs_Error = abs(Residual)
            ) %>%
            select(School, Competition_Name, Event_Class, FRL_Percent, School_Size,
                   Actual = Final_Total, Predicted = .pred, Residual, Abs_Error) %>%
            arrange(desc(Abs_Error)) %>%
            datatable(
                options = list(
                    pageLength = 15,
                    scrollX = TRUE
                ),
                rownames = FALSE
            ) %>%
            formatRound(c('FRL_Percent', 'Actual', 'Predicted', 'Residual', 'Abs_Error'), digits = 2)
    })

    output$prediction_result_frl <- renderText({
        new_data <- tibble(
            Event_Class = factor(input$pred_class_frl, levels = levels(model_data$Event_Class)),
            FRL_Percent = input$pred_frl_frl,
            School_Size = input$pred_size_frl
        )

        prediction <- fitted_model %>%
            predict(new_data = new_data) %>%
            pull(.pred)

        paste0(round(prediction, 2), " points")
    })
}

shinyApp(ui = ui, server = server)

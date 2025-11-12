library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)

# Load data from read_file.R
# Determine correct path based on working directory
if (file.exists('read_file.R')) {
    source('read_file.R')  # Running from R/ directory
} else if (file.exists('R/read_file.R')) {
    source('R/read_file.R')  # Running from project root
} else {
    stop("Cannot find read_file.R. Please ensure you're running from the project directory.")
}

# Use the validated dataset
df <- scores_labeled_df_valid

# Get numeric columns (excluding File column)
numeric_cols <- df %>%
    select(where(is.numeric)) %>%
    names()

# Get categorical columns
categorical_cols <- df %>%
    select(where(~ is.character(.) || is.factor(.))) %>%
    select(-File) %>%  # Exclude File column (too many unique values)
    names()

ui <- fluidPage(
    titlePanel("CBA Scores - Interactive Scatter Plot"),

    sidebarLayout(
        sidebarPanel(
            h4("Plot Controls"),

            selectInput("x_var",
                        "X-axis:",
                        choices = numeric_cols,
                        selected = "Music_Total"),

            selectInput("y_var",
                        "Y-axis:",
                        choices = numeric_cols,
                        selected = "Visual_Total"),

            hr(),

            selectInput("color_var",
                        "Color by:",
                        choices = c("None" = "none", categorical_cols),
                        selected = "Event_Class"),

            selectInput("shape_var",
                        "Shape by:",
                        choices = c("None" = "none", categorical_cols),
                        selected = "Event_Type"),

            hr(),

            h4("Highlight"),

            selectInput("highlight_school",
                        "Highlight School:",
                        choices = c("None" = "none"),
                        selected = "none"),

            hr(),

            h4("Filters"),

            selectInput("year_filter",
                        "Year:",
                        choices = c("All Years" = "all"),
                        selected = "all"),

            selectInput("event_type_filter",
                        "Event Type:",
                        choices = c("All Types" = "all"),
                        selected = "all"),

            hr(),

            checkboxInput("show_trend",
                          "Show Trend Line",
                          value = FALSE),

            checkboxInput("show_one_to_one",
                          "Show 1:1 Line",
                          value = FALSE),

            hr(),

            h4("Data Summary"),
            textOutput("data_summary"),

            width = 3
        ),

        mainPanel(
            plotlyOutput("scatter_plot", height = "700px"),
            hr(),
            h4("Selected Points"),
            verbatimTextOutput("click_info")
        )
    )
)

server <- function(input, output, session) {

    # Populate year filter dynamically
    observe({
        years <- df %>%
            mutate(Year = format(Event_Date, "%Y")) %>%
            pull(Year) %>%
            unique() %>%
            sort(decreasing = TRUE)

        year_choices <- c("All Years" = "all", setNames(years, years))
        updateSelectInput(session, "year_filter", choices = year_choices)
    })

    # Populate event type filter dynamically
    observe({
        event_types <- df %>%
            pull(Event_Type) %>%
            unique() %>%
            sort()

        event_type_choices <- c("All Types" = "all", setNames(event_types, event_types))
        updateSelectInput(session, "event_type_filter", choices = event_type_choices)
    })

    # Populate school highlight dropdown dynamically
    observe({
        schools <- df %>%
            pull(School) %>%
            unique() %>%
            sort()

        school_choices <- c("None" = "none", setNames(schools, schools))
        updateSelectInput(session, "highlight_school", choices = school_choices)
    })

    # Reactive dataset with selected variables and filters
    plot_data <- reactive({
        # Start with full dataset and add needed columns
        cols_to_select <- c("School", "Competition_Name", "Event_Date", "Event_Type",
                           input$x_var, input$y_var)

        # Add color and shape variables if selected
        if (input$color_var != "none") {
            cols_to_select <- c(cols_to_select, input$color_var)
        }
        if (input$shape_var != "none" && input$shape_var != input$color_var) {
            cols_to_select <- c(cols_to_select, input$shape_var)
        }

        # Select unique columns
        cols_to_select <- unique(cols_to_select)

        # Select and prepare data
        data <- df %>%
            mutate(Year = format(Event_Date, "%Y")) %>%
            select(all_of(cols_to_select), Year) %>%
            rename(x = all_of(input$x_var),
                   y = all_of(input$y_var))

        # Apply year filter
        if (input$year_filter != "all") {
            data <- data %>% filter(Year == input$year_filter)
        }

        # Apply event type filter
        if (input$event_type_filter != "all") {
            data <- data %>% filter(Event_Type == input$event_type_filter)
        }

        # Create color grouping variable
        if (input$color_var != "none") {
            data <- data %>%
                mutate(color_group = as.factor(.data[[input$color_var]]))
        } else {
            data <- data %>%
                mutate(color_group = factor("All"))
        }

        # Create shape grouping variable
        if (input$shape_var != "none") {
            data <- data %>%
                mutate(shape_group = as.factor(.data[[input$shape_var]]))
        } else {
            data <- data %>%
                mutate(shape_group = factor("All"))
        }

        # Remove rows with NA in x or y
        data %>% filter(!is.na(x) & !is.na(y))
    })

    output$scatter_plot <- renderPlotly({
        data <- plot_data()

        # Create hover text
        data <- data %>%
            mutate(hover_text = paste0(
                "<b>", School, "</b><br>",
                "Competition: ", Competition_Name, "<br>",
                "Date: ", Event_Date, "<br>",
                input$x_var, ": ", round(x, 2), "<br>",
                input$y_var, ": ", round(y, 2),
                if (input$color_var != "none") paste0("<br>", input$color_var, ": ", color_group) else "",
                if (input$shape_var != "none") paste0("<br>", input$shape_var, ": ", shape_group) else ""
            ))

        # Create plotly scatter plot
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

        # Add highlighted school if selected
        if (input$highlight_school != "none") {
            highlighted_data <- data %>%
                filter(School == input$highlight_school)

            if (nrow(highlighted_data) > 0) {
                # Create hover text for highlighted points
                highlighted_data <- highlighted_data %>%
                    mutate(highlight_hover = paste0(
                        "<b>*** ", School, " ***</b><br>",
                        "Competition: ", Competition_Name, "<br>",
                        "Date: ", Event_Date, "<br>",
                        input$x_var, ": ", round(x, 2), "<br>",
                        input$y_var, ": ", round(y, 2),
                        if (input$color_var != "none") paste0("<br>", input$color_var, ": ", color_group) else "",
                        if (input$shape_var != "none") paste0("<br>", input$shape_var, ": ", shape_group) else ""
                    ))

                p <- p %>%
                    add_trace(
                        data = highlighted_data,
                        x = ~x,
                        y = ~y,
                        type = 'scatter',
                        mode = 'markers',
                        name = paste0("★ ", input$highlight_school),
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

        # Add 1:1 line if requested
        if (input$show_one_to_one) {
            # Calculate range for 1:1 line
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

        # Add trend line if requested
        if (input$show_trend) {
            # Fit linear model
            fit <- lm(y ~ x, data = data)
            x_range <- seq(min(data$x, na.rm = TRUE),
                          max(data$x, na.rm = TRUE),
                          length.out = 100)
            y_pred <- predict(fit, newdata = data.frame(x = x_range))

            # Calculate R-squared
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

        # Update layout
        p <- p %>%
            layout(
                title = paste(input$y_var, "vs", input$x_var),
                xaxis = list(title = gsub("_", " ", input$x_var)),
                yaxis = list(title = gsub("_", " ", input$y_var)),
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

    # Display data summary
    output$data_summary <- renderText({
        data <- plot_data()

        filter_text <- ""
        if (input$year_filter != "all") {
            filter_text <- paste0(filter_text, "Year: ", input$year_filter, "\n")
        }
        if (input$event_type_filter != "all") {
            filter_text <- paste0(filter_text, "Type: ", input$event_type_filter, "\n")
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

    # Show info when points are clicked
    output$click_info <- renderPrint({
        click_data <- event_data("plotly_click")
        if (is.null(click_data)) {
            cat("Click on a point to see details")
        } else {
            data <- plot_data()
            point_idx <- click_data$pointNumber + 1
            selected_point <- data[point_idx, ]

            cat("School:", selected_point$School, "\n")
            cat("Competition:", selected_point$Competition_Name, "\n")
            cat("Date:", as.character(selected_point$Event_Date), "\n")
            cat(input$x_var, ":", round(selected_point$x, 2), "\n")
            cat(input$y_var, ":", round(selected_point$y, 2), "\n")
            if (input$color_var != "none") {
                cat(input$color_var, ":", as.character(selected_point$color_group), "\n")
            }
            if (input$shape_var != "none") {
                cat(input$shape_var, ":", as.character(selected_point$shape_group), "\n")
            }
        }
    })
}

shinyApp(ui = ui, server = server)

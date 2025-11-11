library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(DT)

# Load data from read_file.R
# Determine correct path based on working directory
if (file.exists('read_file.R')) {
    source('read_file.R')  # Running from R/ directory
} else if (file.exists('R/read_file.R')) {
    source('R/read_file.R')  # Running from project root
} else {
    stop("Cannot find read_file.R. Please ensure you're running from the project directory.")
}

# Define tmp and tmp_long as in eda.R
tmp <- scores_labeled_df_valid %>%
    filter(Competition_Name == "CBA 2A-4A Metro Regional 2024" &
               Event_Class == "3A") %>%
    select(School, Music_Total, Visual_Total, Visual_Eff_Tot, Music_Eff_Avg_Total)

tmp_long <- tmp %>%
    pivot_longer(cols = c(Music_Total, Visual_Total, Visual_Eff_Tot, Music_Eff_Avg_Total),
                 names_to = "Category",
                 values_to = "Score")

ui <- fluidPage(
    titlePanel("CBA Competition Scores - Interactive Dashboard"),

    sidebarLayout(
        sidebarPanel(
            h4("Instructions:"),
            p("Click on a category name in the plot to order schools by that category."),

            hr(),

            h4("Currently ordering by:"),
            textOutput("current_order"),

            hr(),

            radioButtons("sort_direction",
                         "Sort Direction:",
                         choices = c("Highest First" = "desc",
                                     "Lowest First" = "asc"),
                         selected = "desc"),

            checkboxInput("show_values", "Show Score Values", FALSE),

            width = 3
        ),

        mainPanel(
            plotlyOutput("score_plot", height = "600px"),
            hr(),
            h4("Data Table"),
            DTOutput("data_table")
        )
    )
)

server <- function(input, output, session) {

    # Reactive value to store current ordering
    order_by <- reactiveVal("Music_Total")
    
    # Display current ordering
    output$current_order <- renderText({
        gsub("_", " ", order_by())
    })

    # Reactive data ordering
    ordered_data <- reactive({
        if(input$sort_direction == "desc") {
            tmp %>% arrange(desc(.data[[order_by()]]))
        } else {
            tmp %>% arrange(.data[[order_by()]])
        }
    })

    output$score_plot <- renderPlotly({
        school_order <- ordered_data() %>% pull(School)

        plot_data <- tmp_long %>%
            mutate(School = factor(School, levels = school_order),
                   Category_Display = gsub("_", " ", Category))

        # Create separate traces for each category to enable clicking
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
                    customdata = cat  # Store category for click detection
                )
        }

        p <- p %>%
            layout(
                title = paste("Schools Ordered by", gsub("_", " ", order_by())),
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

    # Handle click events
    observeEvent(event_data("plotly_click"), {
        click_data <- event_data("plotly_click")
        if(!is.null(click_data)) {
            # Get the category from the trace that was clicked
            trace_num <- click_data$curveNumber + 1
            categories <- c("Music_Total", "Visual_Total", "Visual_Eff_Tot", "Music_Eff_Avg_Total")
            if(trace_num <= length(categories)) {
                order_by(categories[trace_num])
            }
        }
    })

    output$data_table <- renderDT({
        ordered_data() %>%
            datatable(options = list(pageLength = 10, scrollX = TRUE),
                      rownames = FALSE) %>%
            formatRound(columns = c("Music_Total", "Visual_Total", "Visual_Eff_Tot", "Music_Eff_Avg_Total"),
                        digits = 2)
    })
}

shinyApp(ui = ui, server = server)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)

ui <- fluidPage(
    titlePanel("CBA Competition Scores - Interactive Dashboard"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("order_by", 
                        "Order Schools By:",
                        choices = c("Music Total" = "Music_Total", 
                                    "Visual Total" = "Visual_Total", 
                                    "Final Total" = "Final_Total"),
                        selected = "Final_Total"),
            
            hr(),
            
            h4("Display Categories:"),
            checkboxGroupInput("categories",
                               NULL,
                               choices = c("Music Total" = "Music_Total",
                                           "Visual Total" = "Visual_Total",
                                           "Final Total" = "Final_Total"),
                               selected = c("Music_Total", "Visual_Total", "Final_Total")),
            
            hr(),
            
            radioButtons("sort_direction",
                         "Sort Direction:",
                         choices = c("Highest First" = "desc",
                                     "Lowest First" = "asc"),
                         selected = "desc"),
            
            checkboxInput("show_values", "Show Score Values", TRUE),
            
            sliderInput("text_size",
                        "Text Size:",
                        min = 8, max = 16, value = 12, step = 1),
            
            hr(),
            
            h4("Color Scheme:"),
            selectInput("color_scheme",
                        "Choose Palette:",
                        choices = c("Default", "Viridis", "Blues", "Set2"),
                        selected = "Default"),
            
            width = 3
        ),
        
        mainPanel(
            plotOutput("score_plot", height = "600px"),
            hr(),
            h4("Data Table"),
            DTOutput("data_table")
        )
    )
)

server <- function(input, output) {
    
    # Reactive data ordering
    ordered_data <- reactive({
        if(input$sort_direction == "desc") {
            tmp %>% arrange(desc(.data[[input$order_by]]))
        } else {
            tmp %>% arrange(.data[[input$order_by]])
        }
    })
    
    output$score_plot <- renderPlot({
        # Validate at least one category is selected
        req(length(input$categories) > 0)
        
        # Get ordered schools
        school_order <- ordered_data() %>% pull(School)
        
        # Create plot data - filter by selected categories
        plot_data <- tmp_long %>%
            filter(Category %in% input$categories) %>%
            mutate(School = factor(School, levels = school_order),
                   Category = factor(gsub("_Total", "", Category),
                                     levels = gsub("_Total", "", input$categories)))
        
        # Color palette
        if(input$color_scheme == "Viridis") {
            colors <- scale_fill_viridis_d()
        } else if(input$color_scheme == "Blues") {
            colors <- scale_fill_brewer(palette = "Blues")
        } else if(input$color_scheme == "Set2") {
            colors <- scale_fill_brewer(palette = "Set2")
        } else {
            colors <- scale_fill_manual(values = c("Music" = "#3b82f6", 
                                                   "Visual" = "#10b981", 
                                                   "Final" = "#f59e0b"))
        }
        
        # Create plot
        p <- ggplot(plot_data, aes(x = School, y = Score, fill = Category)) +
            geom_col() +
            facet_grid(. ~ Category) +
            coord_flip() +
            colors +
            labs(title = paste("Schools Ordered by", gsub("_", " ", input$order_by)),
                 subtitle = paste("Displaying:", paste(gsub("_Total", "", input$categories), collapse = ", ")),
                 x = NULL, y = "Score") +
            theme_minimal(base_size = input$text_size) +
            theme(legend.position = "none",
                  strip.text = element_text(face = "bold"),
                  panel.grid.major.y = element_blank())
        
        # Add value labels
        if(input$show_values) {
            p <- p + geom_text(aes(label = round(Score, 1)), 
                               hjust = -0.2, size = input$text_size/3)
        }
        
        p
    })
    
    output$data_table <- renderDT({
        # Show only selected columns
        cols_to_show <- c("School", input$categories)
        
        ordered_data() %>%
            select(all_of(cols_to_show)) %>%
            datatable(options = list(pageLength = 10, scrollX = TRUE),
                      rownames = FALSE) %>%
            formatRound(columns = input$categories, digits = 2)
    })
}

shinyApp(ui = ui, server = server)
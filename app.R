library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(viridis)

# Load and clean the data
vgsales <- read.csv("vgsales.csv", stringsAsFactors = FALSE)

# Clean the data
vgsales <- vgsales %>%
  filter(!is.na(Genre), !is.na(Platform)) %>%
  mutate(
    Year = ifelse(is.na(Year), median(Year, na.rm = TRUE), Year),
    Year = as.numeric(Year)
  ) %>%
  filter(Year >= 1980 & Year <= 2020) %>%
  mutate(Decade = paste0(floor(Year/10)*10, "s"))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Video Game Sales Analysis Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Regional Analysis", tabName = "regional", icon = icon("globe")),
      menuItem("Time Trends", tabName = "trends", icon = icon("calendar")),
      menuItem("Platform Analysis", tabName = "platforms", icon = icon("gamepad")),
      menuItem("Top Performers", tabName = "top", icon = icon("trophy")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .navbar {
          background-color: #2c3e50 !important;
        }
        .skin-blue .main-header .logo {
          background-color: #34495e !important;
        }
        .content-wrapper, .right-side {
          background-color: #ecf0f1;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalGames"),
                valueBoxOutput("totalSales"),
                valueBoxOutput("topGenre")
              ),
              fluidRow(
                box(
                  title = "Global Sales by Genre", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("genreBarChart", height = "350px")
                ),
                box(
                  title = "Market Share by Region", status = "info", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("regionPieChart", height = "350px")
                )
              ),
              fluidRow(
                box(
                  title = "Sales Distribution Overview", status = "success", solidHeader = TRUE,
                  width = 12, height = 400,
                  plotlyOutput("salesDistribution", height = "350px")
                )
              )
      ),
      
      # Regional Analysis Tab
      tabItem(tabName = "regional",
              fluidRow(
                box(
                  title = "Regional Genre Preferences Heatmap", status = "primary", solidHeader = TRUE,
                  width = 12, height = 500,
                  plotlyOutput("genreHeatmap", height = "450px")
                )
              ),
              fluidRow(
                box(
                  title = "Regional Sales Comparison", status = "info", solidHeader = TRUE,
                  width = 6, height = 400,
                  selectInput("regionCompare", "Select Regions to Compare:",
                              choices = c("NA vs EU", "NA vs JP", "EU vs JP"),
                              selected = "NA vs EU"),
                  plotlyOutput("regionScatter", height = "300px")
                ),
                box(
                  title = "Genre Preferences by Region", status = "success", solidHeader = TRUE,
                  width = 6, height = 400,
                  selectInput("selectedGenre", "Select Genre:",
                              choices = unique(vgsales$Genre),
                              selected = "Action"),
                  plotlyOutput("genreRegionBar", height = "300px")
                )
              )
      ),
      
      # Time Trends Tab
      tabItem(tabName = "trends",
              fluidRow(
                box(
                  title = "Sales Trends Over Time", status = "primary", solidHeader = TRUE,
                  width = 12, height = 450,
                  checkboxGroupInput("regionsToShow", "Select Regions:",
                                     choices = c("North America" = "NA_Sales",
                                                 "Europe" = "EU_Sales", 
                                                 "Japan" = "JP_Sales",
                                                 "Other" = "Other_Sales"),
                                     selected = c("NA_Sales", "EU_Sales", "JP_Sales")),
                  plotlyOutput("timeTrends", height = "350px")
                )
              ),
              fluidRow(
                box(
                  title = "Genre Evolution Over Decades", status = "info", solidHeader = TRUE,
                  width = 12, height = 400,
                  plotlyOutput("genreEvolution", height = "350px")
                )
              )
      ),
      
      # Platform Analysis Tab
      tabItem(tabName = "platforms",
              fluidRow(
                box(
                  title = "Top Platforms by Sales", status = "primary", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("platformSales", height = "350px")
                ),
                box(
                  title = "Platform Market Share", status = "info", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("platformShare", height = "350px")
                )
              ),
              fluidRow(
                box(
                  title = "Platform Popularity Timeline", status = "success", solidHeader = TRUE,
                  width = 12, height = 400,
                  plotlyOutput("platformTimeline", height = "350px")
                )
              )
      ),
      
      # Top Performers Tab
      tabItem(tabName = "top",
              fluidRow(
                box(
                  title = "Top 20 Games by Global Sales", status = "primary", solidHeader = TRUE,
                  width = 12, height = 500,
                  plotlyOutput("topGames", height = "450px")
                )
              ),
              fluidRow(
                box(
                  title = "Top Publishers", status = "info", solidHeader = TRUE,
                  width = 6, height = 400,
                  plotlyOutput("topPublishers", height = "350px")
                ),
                box(
                  title = "Regional Champions", status = "success", solidHeader = TRUE,
                  width = 6, height = 400,
                  selectInput("championRegion", "Select Region:",
                              choices = c("North America" = "NA_Sales",
                                          "Europe" = "EU_Sales",
                                          "Japan" = "JP_Sales",
                                          "Other Regions" = "Other_Sales"),
                              selected = "NA_Sales"),
                  plotlyOutput("regionalChampions", height = "300px")
                )
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Data Explorer", status = "primary", solidHeader = TRUE,
                  width = 12,
                  DT::dataTableOutput("dataTable")
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Value Boxes
  output$totalGames <- renderValueBox({
    valueBox(
      value = formatC(nrow(vgsales), format = "d", big.mark = ","),
      subtitle = "Total Games",
      icon = icon("gamepad"),
      color = "blue"
    )
  })
  
  output$totalSales <- renderValueBox({
    valueBox(
      value = paste0(round(sum(vgsales$Global_Sales), 1), "M"),
      subtitle = "Total Sales (Units)",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$topGenre <- renderValueBox({
    top_genre <- vgsales %>%
      group_by(Genre) %>%
      summarise(Sales = sum(Global_Sales)) %>%
      arrange(desc(Sales)) %>%
      slice(1)
    
    valueBox(
      value = top_genre$Genre,
      subtitle = "Top Genre",
      icon = icon("trophy"),
      color = "yellow"
    )
  })
  
  # Genre Bar Chart
  output$genreBarChart <- renderPlotly({
    genre_sales <- vgsales %>%
      group_by(Genre) %>%
      summarise(Sales = sum(Global_Sales)) %>%
      arrange(desc(Sales))
    
    p <- ggplot(genre_sales, aes(x = reorder(Genre, Sales), y = Sales)) +
      geom_bar(stat = "identity", fill = "#3498db") +
      coord_flip() +
      labs(x = "Genre", y = "Global Sales (Millions)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Region Pie Chart
  output$regionPieChart <- renderPlotly({
    region_sales <- data.frame(
      Region = c("North America", "Europe", "Japan", "Other"),
      Sales = c(sum(vgsales$NA_Sales), sum(vgsales$EU_Sales), 
                sum(vgsales$JP_Sales), sum(vgsales$Other_Sales))
    )
    
    plot_ly(region_sales, labels = ~Region, values = ~Sales, type = 'pie',
            textposition = 'inside', textinfo = 'label+percent',
            marker = list(colors = c("#e74c3c", "#3498db", "#2ecc71", "#f39c12"))) %>%
      layout(showlegend = TRUE)
  })
  
  # Sales Distribution
  output$salesDistribution <- renderPlotly({
    p <- ggplot(vgsales, aes(x = Global_Sales)) +
      geom_histogram(bins = 50, fill = "#3498db", alpha = 0.7) +
      scale_x_log10() +
      labs(x = "Global Sales (Millions, log scale)", y = "Number of Games") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Genre Heatmap
  output$genreHeatmap <- renderPlotly({
    heatmap_data <- vgsales %>%
      group_by(Genre) %>%
      summarise(
        NA_Sales = sum(NA_Sales),
        EU_Sales = sum(EU_Sales),
        JP_Sales = sum(JP_Sales),
        Other_Sales = sum(Other_Sales)
      ) %>%
      mutate(
        NA_Pct = NA_Sales / sum(NA_Sales) * 100,
        EU_Pct = EU_Sales / sum(EU_Sales) * 100,
        JP_Pct = JP_Sales / sum(JP_Sales) * 100,
        Other_Pct = Other_Sales / sum(Other_Sales) * 100
      ) %>%
      select(Genre, NA_Pct, EU_Pct, JP_Pct, Other_Pct)
    
    heatmap_matrix <- as.matrix(heatmap_data[,-1])
    rownames(heatmap_matrix) <- heatmap_data$Genre
    
    plot_ly(z = heatmap_matrix, x = c("North America", "Europe", "Japan", "Other"),
            y = heatmap_data$Genre, type = "heatmap", colorscale = "Viridis") %>%
      layout(title = "Genre Preferences by Region (% of Regional Sales)")
  })
  
  # Regional Scatter Plot
  output$regionScatter <- renderPlotly({
    regions <- switch(input$regionCompare,
                      "NA vs EU" = c("NA_Sales", "EU_Sales"),
                      "NA vs JP" = c("NA_Sales", "JP_Sales"),
                      "EU vs JP" = c("EU_Sales", "JP_Sales"))
    
    p <- ggplot(vgsales, aes_string(x = regions[1], y = regions[2], color = "Genre")) +
      geom_point(alpha = 0.6) +
      scale_x_log10() + scale_y_log10() +
      labs(x = paste(regions[1], "(Millions, log scale)"),
           y = paste(regions[2], "(Millions, log scale)")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Genre Region Bar Chart
  output$genreRegionBar <- renderPlotly({
    genre_data <- vgsales %>%
      filter(Genre == input$selectedGenre) %>%
      summarise(
        `North America` = sum(NA_Sales),
        Europe = sum(EU_Sales),
        Japan = sum(JP_Sales),
        Other = sum(Other_Sales)
      ) %>%
      melt(variable.name = "Region", value.name = "Sales")
    
    p <- ggplot(genre_data, aes(x = Region, y = Sales, fill = Region)) +
      geom_bar(stat = "identity") +
      labs(y = "Sales (Millions)", title = paste("Sales for", input$selectedGenre, "Genre")) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(p)
  })
  
  # Time Trends
  output$timeTrends <- renderPlotly({
    trend_data <- vgsales %>%
      group_by(Year) %>%
      summarise(
        NA_Sales = sum(NA_Sales, na.rm = TRUE),
        EU_Sales = sum(EU_Sales, na.rm = TRUE),
        JP_Sales = sum(JP_Sales, na.rm = TRUE),
        Other_Sales = sum(Other_Sales, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      melt(id.vars = "Year", variable.name = "Region", value.name = "Sales") %>%
      filter(Region %in% input$regionsToShow)
    
    # Create region labels
    trend_data$Region_Label <- case_when(
      trend_data$Region == "NA_Sales" ~ "North America",
      trend_data$Region == "EU_Sales" ~ "Europe", 
      trend_data$Region == "JP_Sales" ~ "Japan",
      trend_data$Region == "Other_Sales" ~ "Other",
      TRUE ~ as.character(trend_data$Region)
    )
    
    p <- ggplot(trend_data, aes(x = Year, y = Sales, color = Region_Label)) +
      geom_line(size = 1.2) +
      geom_point(size = 0.8) +
      labs(y = "Sales (Millions)", color = "Region") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1") +
      scale_y_continuous(limits = c(0, NA))
    
    ggplotly(p)
  })
  
  # Genre Evolution
  output$genreEvolution <- renderPlotly({
    evolution_data <- vgsales %>%
      filter(!is.na(Year), Year >= 1980, Year <= 2020) %>%
      mutate(Decade = paste0(floor(Year/10)*10, "s")) %>%
      group_by(Decade, Genre) %>%
      summarise(Sales = sum(Global_Sales, na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(Genre))
    
    p <- ggplot(evolution_data, aes(x = Decade, y = Sales, fill = Genre)) +
      geom_col(position = "stack") +
      labs(y = "Global Sales (Millions)", x = "Decade") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_viridis_d()
    
    ggplotly(p)
  })
  
  # Platform Sales
  output$platformSales <- renderPlotly({
    platform_sales <- vgsales %>%
      group_by(Platform) %>%
      summarise(Sales = sum(Global_Sales)) %>%
      arrange(desc(Sales)) %>%
      slice(1:15)
    
    p <- ggplot(platform_sales, aes(x = reorder(Platform, Sales), y = Sales)) +
      geom_bar(stat = "identity", fill = "#e74c3c") +
      coord_flip() +
      labs(x = "Platform", y = "Global Sales (Millions)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Platform Share
  output$platformShare <- renderPlotly({
    platform_share <- vgsales %>%
      group_by(Platform) %>%
      summarise(Sales = sum(Global_Sales)) %>%
      arrange(desc(Sales)) %>%
      slice(1:10)
    
    plot_ly(platform_share, labels = ~Platform, values = ~Sales, type = 'pie',
            textposition = 'inside', textinfo = 'label+percent') %>%
      layout(showlegend = TRUE)
  })
  
  # Platform Timeline
  output$platformTimeline <- renderPlotly({
    # Get top platforms first
    top_platforms <- vgsales %>%
      group_by(Platform) %>%
      summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE), .groups = 'drop') %>%
      arrange(desc(Total_Sales)) %>%
      slice(1:10) %>%
      pull(Platform)
    
    timeline_data <- vgsales %>%
      filter(Platform %in% top_platforms) %>%
      group_by(Year, Platform) %>%
      summarise(Sales = sum(Global_Sales, na.rm = TRUE), .groups = 'drop')
    
    p <- ggplot(timeline_data, aes(x = Year, y = Sales, color = Platform)) +
      geom_line(alpha = 0.8, size = 1) +
      labs(y = "Annual Sales (Millions)", title = "Top 10 Platforms Over Time") +
      theme_minimal() +
      scale_color_viridis_d()
    
    ggplotly(p)
  })
  
  # Top Games
  output$topGames <- renderPlotly({
    top_games <- vgsales %>%
      arrange(desc(Global_Sales)) %>%
      slice(1:20)
    
    p <- ggplot(top_games, aes(x = reorder(Name, Global_Sales), y = Global_Sales, fill = Genre)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Game", y = "Global Sales (Millions)") +
      theme_minimal() +
      scale_fill_viridis_d()
    
    ggplotly(p)
  })
  
  # Top Publishers
  output$topPublishers <- renderPlotly({
    top_publishers <- vgsales %>%
      group_by(Publisher) %>%
      summarise(Sales = sum(Global_Sales)) %>%
      arrange(desc(Sales)) %>%
      slice(1:15)
    
    p <- ggplot(top_publishers, aes(x = reorder(Publisher, Sales), y = Sales)) +
      geom_bar(stat = "identity", fill = "#f39c12") +
      coord_flip() +
      labs(x = "Publisher", y = "Global Sales (Millions)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Regional Champions
  output$regionalChampions <- renderPlotly({
    region_col <- input$championRegion
    champions <- vgsales %>%
      arrange(desc(.data[[region_col]])) %>%
      slice(1:10)
    
    p <- ggplot(champions, aes(x = reorder(Name, .data[[region_col]]), 
                               y = .data[[region_col]], fill = Genre)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Game", y = "Regional Sales (Millions)") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3")
    
    ggplotly(p)
  })
  
  # Data Table
  output$dataTable <- DT::renderDataTable({
    DT::datatable(vgsales, options = list(scrollX = TRUE, pageLength = 15))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
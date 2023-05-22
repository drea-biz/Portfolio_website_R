library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

## Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Portfolio and Analysis Sample Dashboard"),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("About Me", tabName = "about_me"),
      menuItem("Sample Analysis", tabName = "sample_analysis") # tabname is the page handle
    )
  ),
  dashboardBody(
    tags$head( # setting default 
      tags$style(
        HTML(
          "
          .content-wrapper{
            width: 100%; /* Set the width to 100% */}
          .box {
            background-color: #ffff;
            border: 2px solid #dee2e6;
            padding: 15px;
            margin-bottom: 5px;
            width: 100%; /* Add this line to set the width to 100% */
            overflow-x: auto; /* Add this line to enable horizontal scrolling */
          }
          .box-title {
            font-weight: bold;
            margin-top: 15px;
            padding-bottom: 5px;
            border-bottom: 1px solid #dee2e6;
          }
          "
        )
      )
    ),
    tabItems(
      tabItem(
        tabName = "about_me",
        fluidRow(
          column(
            width = 6,
            h2("About Me"),
            p("Welcome to my portfolio!"),
            p("I am a data enthusiast passionate about data analysis and using data to make your life easier."),
            p("Feel free to explore the sample analysis section and contact me if you have any questions or opportunities for collaboration.")
          ),
          column(
            width = 6,
            h2("Contact Information"), # HTML BELOW
            
            p("Email: drea.datasci@gmail.com"),
            p("Phone: (858) 466-9040"),
            p("LinkedIn: [LinkedIn Profile](https://www.linkedin.com/in/m-drea)")
          )
        )
      ),
      tabItem(
        tabName = "sample_analysis",
        fluidRow(
          column(
            width = 10, # Adjust the width of the column and the boxes 
            h2("Price Analysis Dashboard"),
            box(
              h3(class = "box-title","Slice of Sample Data"),
              tableOutput("peak_df")
            ),
            
            box(
              h3(class = "box-title","Price distribution for products"),
              plotOutput("histogram")
            ),
            
            box(
              h3(class = "box-title","Filtered Product Transactions"),
              p("Note: This table is arranged by popularity and only includes the top 20 skew numbers.",br(n = 1)," This table is date and skew searchable."),
              div(
                style = "height: 300px; overflow: auto;",
                DT:: dataTableOutput("filter_popular_table") # scrollable table
              )
            ),
            
            box(
              h3(class = "box-title","Summary Statistics"),
              div(
                style = "height: 300px; overflow: auto;",
                tableOutput("summary_table") # scrollable table
              )
            )
          )
        )
      )
    )
  )
)

# Define server and this is where you put your defs or function-like code ie output$summary_table
server <- function(input, output) {
  # Perform analytics
  df <- read.csv("portfolio_Mrg_data.csv")
  df <- df |>
    select(-Price_bought) 
  
  # Render histogram
  output$histogram <- renderPlot({
    ggplot(df, aes(x = Price_sold, fill = "SKU")) +
      geom_histogram(fill = "purple", bins = 30) +
      labs(
        title = "Price distribution and the frequency of products",
        subtitle = "This table shows the range of prices and the frequency that they were bought",
        x = "Price",
        y = "Product Frequency"
      )
  })
  
# Listing the top 20 skews
output$filter_popular_table <- DT:: renderDataTable({
  # Count the frequency of each skew number
  freq_table <- table(df$SKU)
  
  # Sort the frequencies in descending order
  sorted_freq <- sort(freq_table, decreasing = TRUE)
  
  # Extract the top 20 skew numbers
  top_skews <- names(sorted_freq)[1:20]
  
  # Filter the dataframe based on the top 20 skew numbers
  filt <- df %>%
    filter(SKU %in% top_skews) 
    
  
  
  # Apply search filters
  if (!is.null(input$skew_input) && input$skew_input != "") {
    filt <- filt[filt$SKU == input$skew_input, ]
  }
  if (!is.null(input$date_input)) {
    filt <- filt[as.Date(filt$Date_Sold) == input$date_input, ]
  }
  filt
})

  
  
  # Render summary table
  output$summary_table <- renderTable({
    summary_df <- data.frame(
      Min_Price = min(df$Price_sold),
      Max_Price = max(df$Price_sold),
      Mean_Price = mean(df$Price_sold),
      Median_Price = median(df$Price_sold),
      SD_Price = sd(df$Price_sold)
    )
    summary_df
  })
  
  # Table 2 function: sales predictions
  output$peak_df <- renderTable({
    head(df, n = 6)
  })
  
  # Render filter notes
  output$filter_notes <- renderText({
    "This table is arranged by date and only includes the products that are most popular."
  })
}

# Run the app
shinyApp(ui, server)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(bslib)

## Define UI
ui <- dashboardPage(
  dashboardHeader(title = strong("Portfolio Webpage"), titleWidth = "300px" ),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("About Me", tabName = "about_me"),
      menuItem("Sample Analysis", tabName = "sample_analysis") # tabname is the page handle
    )
  ),
  dashboardBody(
    tags$head( # setting default styles and creating definitions for styles 
      
      tags$style(
        HTML("

          .custom-column {
            background-color: #F4F6F6
 /* Desired background color for the column */
          }
       
    
          body {
        background-color: #F5F5F5; /* background color */
            }
          .content-wrapper{
            width: 100%; /* Set the width to 100% */}
          .box {
            background-color:  #f8faff; /* Light blue accent shade */
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
          column( class = "custom-column",  # Assign a custom class to the column
            width = 6,
            h2("About Me"),
            h3("Welcome to my portfolio website!"),
            p(" I created this website by scratch and am adding more as time goes on! "),
            p("I am a data scientist passionate about  using data to create smart bussiness decisions. Click the side panel to see an example of one of the many things I can do with data."),
            p("Feel free to explore the sample analysis section and contact me if you have any questions or opportunities for collaboration."),
            h2("Resume"),

            h3("Andrea M. Mejia"),
            p("San Diego, CA | (858) 466-9040 "),
            
            h3("Summary"),
            p("Experienced Data Scientist skilled in Python, R, and SQL with expertise in predictive modeling, statistical analysis, data visualization, and manipulation. Holds a Bachelor's degree in Cognitive Science from UC San Diego with a strong foundation in research methodology, data science, and statistical concepts. Strong communication skills and passion for data-driven approaches to optimize business processes and innovation."),
            
            h3("Skills"),
            HTML("<ul>
           <li>Programming Languages: Python, R, HTML, SQL (MYSQL)</li>
           <li>Data Wrangling, cleaning, and Visualizations.</li>
           <li>ETL, Natural Language Processing (NLP), Sentimental Analysis, Relational Databases.</li>
           <li>Predictive Modeling and projections, PLM, Machine Learning, and Statistical Analysis.</li>
           <li>Management, Leadership, Customer Service, Time Management, Problem-solving.</li>
           <li>Word, Excel, Research, Presentations, and Report Writing.</li>
       </ul>"),
          
            h3("Education"),
            p(
              strong("University of California, San Diego - Cognitive Science: Specialization in Neuroscience B.S. (2017 - March 2023)"), br(),
              "Relevant classes include calculus, statistics, linear algebra, python, data science, R and Python, and research methods. Transferred from a junior college and completed pre-medicine classes in addition to my degree."
            ),
            
            h3("Experience"),
            p(
              strong("Data Engineer - Pacific Distribution Corporation, San Diego, CA (July 2022 - March 2023)"), br(),
              "Developed Python-based models for business analytics and projections with a fast completion rate.", br(),
              "Validated and cleaned data for analysis.", br(),
              "Utilized machine learning techniques for sale prediction.", br(),
              "Presented data visualizations to communicate insights clearly and effectively.", br()
            ),
            p(
              strong("Event Security Supervisor & Area Manager - Contemporary Service Corp., San Diego, CA (Sept. 2021 - Present)"), br(),
              "Managed security guards for high-profile events/venues such as Comic-Con and the Super Bowl.", br(),
              "Utilized de-escalation techniques, customer service, leadership, time management, and problem-solving abilities to ensure guest satisfaction while maintaining safety.", br(),
              "Recognized for producing a positive, tangible impact appreciated by clients.", br()
            ),
            p(
              strong("Sales/ Promotion - Levinson Group, San Diego, CA (July 2022 - April 2023)"), br(),
              "Engaged with large groups of strangers and created repeat clients.", br(),
              "Sold tickets for venues and was recognized for consistently improving sales.", br()
            ),
            p(
              strong("Emergency Medical Technician - Horizon & Symons Ambulance, Orange, CA (July 2018 - April 2020)"), br(),
              "Provided standby EMS for fast-paced large events, critical care, and BLS transports.", br(),
              "Trained EMTs on ambulance driving, patient care, lifting techniques, documentation, and medical practices.", br(),
              "Demonstrates high accuracy and professionalism in patient comfort and support in sensitive situations."
            )),
          
          
          column(
            width = 6,
            h2("Contact Information"),
            p("Email: drea.datasci@gmail.com"),
            p("Phone: (858) 466-9040"),
            h3("LinkedIn:"),
            div(
              class = "badge-base LI-profile-badge",
              "data-locale" = "en_US",
              "data-size" = "medium",
              "data-theme" = "light",
              "data-type" = "VERTICAL",
              "data-vanity" = "m-drea",
              "data-version" = "v1",
              tags$a(
                class = "badge-base__link LI-simple-link",
                href = "https://www.linkedin.com/in/m-drea?trk=profile-badge",
                "Andrea M."
              )
            )
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
              div(
                style = "height: 300px; overflow: auto;",
                tableOutput("peak_df")
              )
            ),
            box(
              h3(class = "box-title","Price distribution for products"),
              div(
                style = "height: 500px; overflow: auto;",
                plotOutput("histogram")
              )
            ),
            box(
              h3(class = "box-title","Filtered Product Transactions"),
              p("Note: This table is arranged by popularity and only includes the top 20 skew numbers.",br(n = 1)," This table is date and skew searchable."),
              div(
                style = "height: 300px; overflow: auto;",
                DT::dataTableOutput("filter_popular_table")
              )
            ),
            box(
              h3(class = "box-title","Summary Statistics"),
              div(
                style = "height: 300px; overflow: auto;",
                tableOutput("summary_table")
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
  library(stringr)
  
  output$histogram <- renderPlot({
    ggplot(df, aes(x = Price_sold, fill = "SKU")) +
      geom_histogram(fill = "purple", bins = 30) +
      labs(
        title = str_wrap("Price Distribution and Product Frequency:", width = 50),
        subtitle = str_wrap("This table shows the range of prices and the frequency at which they were bought", width = 40),
        x = str_wrap("Price", width = 20),
        y = str_wrap("Product Frequency", width = 20)
      ) +
      theme(
        plot.title = element_text(size = 13, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
      )
  })
  
  
  # Listing the top 20 skews
  output$filter_popular_table <- DT::renderDataTable({
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

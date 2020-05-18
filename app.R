library(tidyverse)
library(shiny)
library(scales)

theme_set(theme_minimal())

n_members <- 100
n_years <- 5
cta <- 100
initial_amt <- 100
renewal_rate <- 0.5
renewal_amt <- 100

calc_ltv <- function(n_years,
                     n_members,
                     cta,
                     initial_amt,
                     renewal_rate,
                     renewal_amt) {


    yr1_rev <- (n_members * initial_amt) - (cta * n_members)

    ppl <- accumulate(2:n_years, ~ .x * renewal_rate, .init = n_members)

    rev <- ppl[2:n_years] * renewal_amt

    rev <- c(yr1_rev, rev)

    tibble(years = 1:n_years, ppl, rev)
}

ui <- fluidPage(

  # App title ----
  titlePanel("Member LTV Simulation"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
       sliderInput(
            "n_years",
            "Number of Years",
            min = 1,
            max = 20,
            value = 5
       ),
        numericInput(
            "n_members",
            "Number of Members",
            value = 100
         ),
       numericInput(
            "cta",
            "Cost to Acquire",
            value = 100
        ),
       numericInput(
            "initial_amt",
            "Initial Transaction Amount",
            value = 100
        ),
        sliderInput(
            "renewal_rate",
            "Renewal Rate",
            min = 0,
            max = 100,
            value = 50,
            post = "%"
        ),
       numericInput(
            "renewal_amt",
            "Renewal Transaction Amount",
            value = 100
        )

    ),

    # Main panel for displaying outputs ----
    mainPanel(
        tableOutput("summary"),
        plotOutput("members"),
        plotOutput("revenue")
    )
  )
)

server <- function(input, output) {

   ret <- reactive({
        calc_ltv(
            n_years = input$n_years,
            n_members = input$n_members,
            cta = input$cta,
            initial_amt = input$initial_amt,
            renewal_rate = input$renewal_rate / 100,
            renewal_amt = input$renewal_amt
        )
   })

    output$summary <- renderTable({
        ret() %>%
        summarise(
            `Final members` = ppl[years == max(years)],
            `Net revenue` = dollar(sum(rev))
        )
    })

    output$members <- renderPlot({
        ret() %>%
        ggplot(aes(years, ppl)) + 
            geom_col(fill = "#e83a17") +
            geom_text(aes(label = round(ppl, 2)), vjust = -0.5) + 
            scale_y_continuous(labels = function(x) round(x, 0),
                               breaks = breaks_pretty()) +
            labs(title = "Number of members by year",
                y = "members") +
            theme(plot.title = element_text(size = 20),
                  axis.text = element_text(size = 12))
    })

    output$revenue <- renderPlot({
        ret() %>%
        ggplot(aes(years, rev)) + 
            geom_col(fill = "#e83a17") +
            geom_text(aes(label = dollar(rev)), 
                      vjust = -0.5) + 
            scale_y_continuous(labels = dollar_format(),
                               breaks = breaks_pretty()) +
            labs(title = "Net revenue by year",
                y = "revenue") +
            theme(plot.title = element_text(size = 20),
                  axis.text = element_text(size = 12))
    })
}

shinyApp(ui, server)


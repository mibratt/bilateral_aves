library(shiny)
library(tidyverse)
library(DT)
library(haven)
library(writexl)

# Load the data file
load("data/ave_results.Rdata")

# Country codes and names
country_corr <- readr::read_csv("data/Country list.csv")

# Data frame for app -----------------------------------------------------

aves_results <- result %>%
  left_join(country_corr %>% select(c(iso3, country)) %>% 
              rename(Exporter = country), 
            by = join_by(country == iso3)) %>% 
  left_join(country_corr %>% select(c(iso3, country)) %>% 
              rename(Importer = country), 
            by = join_by(partner == iso3)) %>% 
  mutate(
    ave = format(round(ave * 100, 2), nsmall = 2), 
    shareones = format(round(shareones * 100, 2), nsmall = 2), 
    weighted_aves = format(round(weighted_aves * 100, 2), nsmall = 2)) %>% 
  select(Exporter, Importer, weighted_aves, ave, shareones)


# App ---------------------------------------------------------------------

ui <- fluidPage(
  tags$h2("Estimates of bilateral AVEs of NTMs by exporter-importer pair", 
          style = "margin-top: 20px; margin-bottom: 20px; text-align: center;"),
  tags$details(
    style = "margin-bottom: 20px;",
    tags$summary("Click to view citation", style = "cursor: pointer; font-size: 16px; color: #007BFF;"),
    tags$p(
      "Bratt, M. (2017). ",
      tags$a(href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/roie.12297", 
             target = "_blank", 
             "Estimating the bilateral impact of nontariff measures on trade", 
             style = "text-decoration: underline; color: #007BFF;"),
      ". ",
      tags$em("Review of International Economics, 25(5), 1105–1129.")
    )
  ),
  div(style = "text-align: right; margin-bottom: 10px;",
      downloadButton("download", "Download AVEs (excel)")
      ),
  DTOutput("aves")
)

server <- function(input, output, session) {
  
  # Preprocess the table with renamed columns
  processed_data <- aves_results %>%
    rename(
      `Median AVE (%) for tariff lines where NTM = 1` = ave,
      `Frequency of tariff lines where NTM = 1 (%)` = shareones,
      `Median AVE (%) for all tariff lines` = weighted_aves
    )
  
  output$aves <- renderDT(
    processed_data,
    rownames = FALSE,
    options = list(
      searching = TRUE,
      paging = FALSE,
      columnDefs = list(
        list(className = 'dt-center', targets = 2:4)
      )
    )
  )
  
  
  output$download <- downloadHandler(
    filename = function() {
      "aves_results.xlsx"
    },
    content = function(file) {
      writexl::write_xlsx(processed_data, path = file)
    }
  )
}

shinyApp(ui, server)

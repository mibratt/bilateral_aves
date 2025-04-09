library(shiny)
library(tidyverse)
library(lubridate)
library(janitor)
library(reactable)
library(DT)
library(writexl)

# Functions ---------------------------------------------------------------

# Source for the computation
source("Bilateral AVEs per trading pair.R")

# ColorBrewer-inspired 3-color scale
BuYlRd <- function(x) rgb(colorRamp(c("#7fb7d7", "#ffffbf", "#fc8d59"))(x), 
                          maxColorValue = 255)

make_ui <- function(x, var) {
  gtci_rng <- round(range(x, na.rm = TRUE), digits = 2)
  sliderInput(var, var, min = gtci_rng[1], max = gtci_rng[2], value = 0)
}

# Values, vectors and data ------------------------------------------------

# Country codes and names
country_corr <- readr::read_csv("data/Country list.csv")

country_list <- country_corr %>%
  arrange(country) %>%
  pull(country)

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
    shareones = format(round(shareones * 100), nsmall = 2), 
    weighted_aves = format(round(weighted_aves * 100), nsmall = 2)) %>% 
  select(Exporter, Importer, weighted_aves, ave, shareones)


# App ---------------------------------------------------------------------

ui <- fluidPage(
  tags$h2("Estimates of bilateral AVEs of NTMs by exporter-importer pair", 
          style = "margin-top: 20px; margin-bottom: 20px; text-align: center;"),
  div(style = "text-align: right; margin-bottom: 10px;",
      downloadButton("download", "Download AVEs (excel)")
      ),
  DTOutput("aves")
)

server <- function(input, output, session) {
  
  # Preprocess the table with renamed columns
  processed_data <- aves_results %>%
    rename(
      `Median AVE (%) for tariff lines with NTMs` = ave,
      `Share of tariff lines with NTMs` = shareones,
      `Median AVE (%) for all tariff lines (with or without NTMs)` = weighted_aves
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

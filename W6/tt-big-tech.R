library(tidyverse)
library(reactablefmtr)
library(htmltools)
library(htmlwidgets)
library(lubridate)
library(glue)
#remotes::install_github("timelyportfolio/dataui")

#import data
df_sp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
df_comp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

#change IBM company name to short form
df_comp <- df_comp|>mutate(company = case_when(stock_symbol=="IBM" ~ "IBM Corporation", TRUE ~ company))

#look at the most recent date of stock values per company
sp_last_date = df_sp|>
  group_by(stock_symbol)|>
  summarise(last_date = max(date))

#minimum value of last date among all companies
last_date = min(sp_last_date$last_date)

#get dates for 1m, 6m ago
date_1m = last_date %m-% months(1) 
date_6m = last_date %m-% months(6) 
date_1y = last_date %m-% months(12) 

#get last close value of stock per company
df_last_sp <- df_sp|>
  filter(date == last_date)|>
  select(stock_symbol, close)|>
  rename(last=close)

#aggregate by company list of stock prices past month
df_trend_1m <- df_sp|>
  filter(date <= last_date & date >= date_1m)|>
  arrange(stock_symbol, date)|>
  group_by(stock_symbol)|>
  summarise(trend_1m = list(close))

#aggregate by company list of stock prices past 6 months
df_trend_6m <- df_sp|>
  filter(date <= last_date & date >= date_6m)|>
  group_by(stock_symbol)|>
  summarise(trend_6m = list(close))

#aggregate by company list of stock prices past year
df_trend_1y <- df_sp|>
  filter(date <= last_date & date >= date_1y)|>
  group_by(stock_symbol)|>
  summarise(trend_1y = list(close))

#get the stock prices per company 1 month ago, 6 months ago, and 1 year ago
df_values <- df_sp|>
  filter(date %in% c(date_1m, date_6m, date_1y))|>
  select(stock_symbol, date, close)|>
  mutate(close = round(close,2))|>
  pivot_wider(id_cols=stock_symbol, names_from = date, values_from = close)|>
  rename(value_1y = 2, value_6m = 3, value_1m = 4)


df_table<- df_last_sp|>
  #combine and merge all data sets - values & trends
  left_join(df_values, by="stock_symbol")|>
  left_join(df_trend_1m, by="stock_symbol")|>
  left_join(df_trend_6m, by="stock_symbol")|>
  left_join(df_trend_1y, by="stock_symbol")|>
  #add a color comp variable to change table fonts based on positive/negative values
  mutate(comp_1m = case_when(value_1m>last ~ "#F73131", TRUE ~ "#0BC157"),
         comp_6m = case_when(value_6m>last ~ "#F73131", TRUE ~ "#0BC157"),
         comp_1y = case_when(value_1y>last ~ "#F73131", TRUE ~ "#0BC157"),
         #create delta values
         delta_1m = (last - value_1m)/value_1m, 
         delta_6m = (last - value_6m)/value_6m,
         delta_1y = (last - value_1y)/value_1y
         )|>
  #rearrange order of variables before passing it into table
  select(stock_symbol, last, delta_1m, comp_1m, trend_1m, delta_6m, comp_6m, trend_6m, delta_1y, comp_1y, trend_1y)


#base url for where logo images are hosted
base_url = "https://raw.githubusercontent.com/tashapiro/TidyTuesday/master/2023/W6/logos/"

font="Archivo"

  
# Create table
reactable(
  data = df_table,
  fullWidth = FALSE,
  defaultPageSize = 14,
  # Core: Uniform center alignment for column headers, optimize cell style
  defaultColDef = colDef(
    vAlign = "center",
    align = "center",  # Cell content center alignment
    width = 100,
    style = list(
      padding = "8px 4px",
      "&&:hover" = list(backgroundColor = "#f1f1f1")
    ),
    # Column header (table header) center alignment
    headerStyle = list(
      textAlign = "center"  # Force column header text to center
    )
  ),
  theme = reactableTheme(
    headerStyle = list(
      backgroundColor = "#f8f9fa",
      fontWeight = "bold",
      borderBottom = "2px solid #ddd",
      padding = "10px 4px"
    )
  ),
  # Force center alignment for grouped column headers (override default left alignment if any)
  columnGroups = list(
    colGroup(
      name = "1 Month", 
      columns = c("delta_1m", "trend_1m"),
      headerStyle = list(textAlign = "center")  # Group header center alignment
    ),
    colGroup(
      name = "6 Months", 
      columns = c("delta_6m", "trend_6m"),
      headerStyle = list(textAlign = "center")
    ),
    colGroup(
      name = "1 Year", 
      columns = c("delta_1y", "trend_1y"),
      headerStyle = list(textAlign = "center")
    )
  ),
  columns = list(
    comp_1y = colDef(show = FALSE),
    comp_1m = colDef(show = FALSE),
    comp_6m = colDef(show = FALSE),
    
    # Company column (optimized layout, header forced to center separately)
    stock_symbol = colDef(
      name = "Company",
      align = "left",  # Cell content left alignment (for better text readability)
      width = 260,
      headerStyle = list(textAlign = "center"),  # Force "Company" column header to center separately
      cell = function(value) {
        img_url <- paste0(base_url, value, ".png")
        company_name <- df_comp$company[df_comp$stock_symbol == value]
        tagList(
          div(style = "display: inline-block; vertical-align: middle; margin-right: 12px;",
              img(src = img_url, style = "height: 30px; width: auto;")),
          div(style = "display: inline-block; vertical-align: middle; padding: 4px 0;",
              div(company_name, style = "font-weight: bold; font-size: 11pt; line-height: 1.3;"),
              div(paste0("(", value, ")"), style = "font-size: 9pt; color: #666; margin-top: 3px;"))
        )
      }
    ),
    
    last = colDef(
      name = "Latest Price",
      width = 100,
      cell = function(value) {
        sprintf("$%.2f", value)
      }
    ),
    
    delta_1m = colDef(
      name = "Change",
      cell = function(value, index) {
        color <- ifelse(value < 0, "#d32f2f", "#388e3c")
        perc <- sprintf("%.1f%%", value * 100)
        tagList(
          span(ifelse(value < 0, "↓", "↑"), style = glue("color: {color}; margin-right: 4px;")),
          span(perc, style = glue("color: {color};"))
        )
      }
    ),
    delta_6m = colDef(
      name = "Change",
      cell = function(value, index) {
        color <- ifelse(value < 0, "#d32f2f", "#388e3c")
        perc <- sprintf("%.1f%%", value * 100)
        tagList(
          span(ifelse(value < 0, "↓", "↑"), style = glue("color: {color}; margin-right: 4px;")),
          span(perc, style = glue("color: {color};"))
        )
      }
    ),
    delta_1y = colDef(
      name = "Change",
      cell = function(value, index) {
        color <- ifelse(value < 0, "#d32f2f", "#388e3c")
        perc <- sprintf("%.1f%%", value * 100)
        tagList(
          span(ifelse(value < 0, "↓", "↑"), style = glue("color: {color}; margin-right: 4px;")),
          span(perc, style = glue("color: {color};"))
        )
      }
    ),
    
    trend_1m = colDef(
      name = "Trend",
      cell = react_sparkline(
        data = df_table,
        line_curve = "linear",
        show_area = FALSE,
        line_width = 2,
        line_color_ref = "comp_1m"
      )
    ),
    trend_6m = colDef(
      name = "Trend",
      cell = react_sparkline(
        data = df_table,
        line_curve = "linear",
        show_area = FALSE,
        line_width = 2,
        line_color_ref = "comp_6m"
      )
    ),
    trend_1y = colDef(
      name = "Trend",
      cell = react_sparkline(
        data = df_table,
        line_curve = "linear",
        show_area = FALSE,
        line_width = 2,
        line_color_ref = "comp_1y"
      )
    )
  )
) %>%
  htmlwidgets::prependContent(
    tagList(
      h2("The Rise & Fall of Big Tech 2022", style = "margin-bottom: 8px;"),
      div("Big tech stock prices as of December 29th, 2022. All companies show negative 1-year trends except IBM.", 
          style = "color: #666; margin-bottom: 12px;")
    )
  ) %>%
  htmlwidgets::appendContent(
    div(
      "Source: Kaggle | Reproduced from: github.com/tashapiro/TidyTuesday/blob/master/2023/W6/big-tech-stocks.png | Reproduction Date: 2025/9/28", 
      style = "margin-top: 10px; padding-top: 8px; border-top: 1px solid #eee; color: #666; font-size: 9pt;"
    )
  )

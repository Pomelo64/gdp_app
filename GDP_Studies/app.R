


library(shiny)
# library(janitor)
suppressPackageStartupMessages(library(janitor))
# library(randomForest)
suppressPackageStartupMessages(library(randomForest))
library(shinyjs)
library(shinycssloaders)
library(imputeTS)
# library(tidyverse)
library(readxl)
# library(janitor)
library(fmsb)
# library(gganimate)
library(umap)
library(bslib)
suppressPackageStartupMessages(library(tidyverse))


# reading data for forecasting --------------------------------------------

readxl::read_xlsx(path = "GDP_Studies/data/Forecasting dataset.xlsx", sheet = 2) -> forecasting_gdp_tbl

forecasting_gdp_tbl <- 
  forecasting_gdp_tbl %>% 
  janitor::clean_names() %>%
  rename(date = x1)


readxl::read_xlsx(path = "GDP_Studies/data/Forecasting dataset.xlsx", sheet = 3) -> forecasting_pmi_tbl

forecasting_pmi_tbl <- 
  forecasting_pmi_tbl %>%
  janitor::clean_names() %>% 
  rename(date = x1)

readxl::read_xlsx(path = "GDP_Studies/data/Forecasting dataset.xlsx", sheet = 4) -> forecasting_bcc_tbl

forecasting_bcc_tbl <- 
  forecasting_bcc_tbl %>% 
  janitor::clean_names() %>% 
  rename(date = x1)

forecasting_tbl <- 
  forecasting_gdp_tbl %>% 
  left_join(forecasting_pmi_tbl) %>% 
  left_join(forecasting_bcc_tbl) %>%
  filter(!is.na(quarterly_gdp_growth))


# reading gdp data for vis --------------------------------------------------------

sheet1_tbl <- 
 readxl::read_xlsx(path = "GDP_Studies/data/Dashboard dataset.xlsx", sheet = 1, skip = 4) 

gdp_tbl <- 
  sheet1_tbl %>%
  slice(-1) %>% 
  janitor::clean_names() %>%
  separate(col = time_period_and_dataset_code_row, into = c("year","quarter"), remove = F) %>% 
  mutate(quarter = str_extract(quarter, pattern = "[:digit:]")) %>%
  mutate(across(.cols = -time_period_and_dataset_code_row, .fns = as.numeric)) %>%
  rename(hh = households_final_consumption_expenditure, 
         g = general_government_final_consumption_expenditure, 
         pi = gross_fixed_capital_formation_gross_capital_formation, 
         inv = changes_in_inventories_gross_capital_formation,
         tb = trade_balance, 
         gdp = gross_domestic_product_at_market_prices) %>%
  mutate(id = row_number() ) %>% 
  relocate(id)

gdp_components_perc_tbl <- 
  gdp_tbl %>% 
  select(hh:gdp) %>% 
  mutate(id = row_number()) %>% 
  relocate(id) %>% 
  group_by(id) %>% 
  mutate(across(.cols = everything(), .fns = ~100 * .x / gdp) %>% round(digits = 2)) %>% 
  ungroup() 

lagged_forecasting_list <- list()

for(i in 2:6){
  temp <- 
    forecasting_tbl %>% 
    select(-date) %>%
    mutate_all(.funs = ~lag(.x,n=i)) %>%
    rename_all(.funs = ~paste0(.x,"_",i,"lag"))
  
  lagged_forecasting_list[i] <- 
    list(temp)
}

lagged_forecasting_tbl <- bind_cols(lagged_forecasting_list)

forecasting_tbl_w_lag <- 
  forecasting_tbl %>%
  mutate(month = month(date)) %>% 
  bind_cols(lagged_forecasting_tbl) 

forecasting_tbl_train <- 
  forecasting_tbl_w_lag %>% 
  filter(date >= "2003-09-30") %>% 
  select(-date) %>%
  # complete(mice::mice(data= ., method = "cart"))
  mutate(across(.cols = starts_with("bcc"),.fns = ~imputeTS::na_interpolation(.x) ))


forecasting_tbl_train_for_rf <- 
  forecasting_tbl_train %>% 
  select(!forecasting_tbl %>%
           select(-quarterly_gdp_growth,-date) %>%
           colnames() )


test_dates <- c("2023-12-31","2024-03-31")
lagged_test_list <- list()

test_tbl <- 
  forecasting_tbl %>%
  arrange(date) %>%
  slice_tail(n = 10) %>%
  bind_rows(tibble(date = lubridate::ymd(test_dates)))

for(i in 2:6){
  temp <- 
    test_tbl %>% 
    select(-date) %>%
    mutate_all(.funs = ~lag(.x,n=i)) %>%
    rename_all(.funs = ~paste0(.x,"_",i,"lag"))
  
  lagged_test_list[i] <- 
    list(temp)
}

lagged_test_tbl <- bind_cols(lagged_test_list)

test_tbl_w_lag <- 
  test_tbl %>%
  mutate(month = month(date)) %>% 
  bind_cols(lagged_test_tbl) %>%
  slice_tail(n=2)


# ui ----------------------------------------------------------------------
# ui <- navbarPage(
#   
#   # Application title
#   titlePanel("GDP Study And Forecasting"),
#   
#   # Sidebar layout with a input and output definitions
#   sidebarLayout(
#     sidebarPanel(
#       conditionalPanel(
#         condition = "input.tabset1 == 'GDP Composition Path'",
#         selectInput("tabset1_data_level", "Data Level:", choices = c("Quarterly", "Annually"), selected = "Quarterly")
#       ),
#       conditionalPanel(
#         condition = "input.tabset1 == 'GDP Composition SpiderPlot'",
#         selectInput("tabset2_data_level", "Data Level:", choices = c("Quarterly", "Annually"), selected = "Quarterly"),
#         selectInput("tabset2_year", "Year:", choices = c(min(gdp_tbl$year):max(gdp_tbl$year)), selected = "2020"),
#         selectInput("tabset2_quarter", "Quarter:", choices = paste0(1:4), selected = "1")
#         
#       ),
#       conditionalPanel(
#         condition = "input.tabset1 == 'GDP Composition Comparison: Web'",
        # selectInput("tabset3_data_level", "Data Level:", choices = c("Quarterly", "Annually"), selected = "Quarterly"),
        # selectInput("tabset3_bench_year", "Benchmark Year:", choices = c(min(gdp_tbl$year):max(gdp_tbl$year)), selected = "2020"),
        # selectInput("tabset3_bench_quarter", "Quarter:", choices = paste0(1:4), selected = "1"),
        # selectInput("tabset3_select_year", "Year:", choices = c(min(gdp_tbl$year):max(gdp_tbl$year)), selected = "2020"),
        # selectInput("tabset3_select_quarter", "Quarter:", choices = paste0(1:4), selected = "1")
#         
#       ),
#       conditionalPanel(
#         condition = "input.tabset1 == 'GDP Composition Comparison: Arrows'",
        # selectInput("tabset4_data_level", "Data Level:", choices = c("Quarterly", "Annually"), selected = "Quarterly"),
        # selectInput("tabset4_bench_year", "Benchmark Year:", choices = c(min(gdp_tbl$year):max(gdp_tbl$year)), selected = "2020"),
        # selectInput("tabset4_bench_quarter", "Quarter:", choices = paste0(1:4), selected = "1"),
        # selectInput("tabset4_select_year", "Year:", choices = c(min(gdp_tbl$year):max(gdp_tbl$year)), selected = "2020"),
        # selectInput("tabset4_select_quarter", "Quarter:", choices = paste0(1:4), selected = "1")
#         
#       )
#     ),
#     
#     # Main panel for displaying outputs
#     mainPanel(
#       # Output: Tabset with multiple tab panels
#       tabsetPanel(
#         tabPanel("GDP Composition Path",
#                  h4("Tab 1 Content"),
#                  plotOutput("pca_biplot")
#         ),
#         tabPanel("GDP Composition SpiderPlot",
#                  h4("Tab 2 Content"),
#                  plotOutput("spider_plot1")
#         ),
#         tabPanel("GDP Composition Comparison: Web",
#                  plotOutput("spider_plot2")
#         ),
#         tabPanel("GDP Composition Comparison: Arrows",
#                  plotOutput("arrow_comparison_1"),
#                  plotOutput("arrow_comparison_2")
#         ),
#         id = "tabset1" # id for this tabset panel
#       )
#     )
#   )
# )

ui <- navbarPage(
  
  # Application title
  title = "GDP Study And Forecasting",
  
  # Tab panels for different sections
  tabPanel("GDP Composition Path",
           sidebarLayout(
             sidebarPanel(
               selectInput("tabset1_data_level", "Data Level:", choices = c("Quarterly", "Annually"), selected = "Quarterly")
             ),
             mainPanel(
               plotOutput("pca_biplot")
             )
           )
  ),
  
  tabPanel("GDP Composition SpiderPlot",
           sidebarLayout(
             sidebarPanel(
               selectInput("tabset2_data_level", "Data Level:", choices = c("Quarterly", "Annually"), selected = "Quarterly"),
               selectInput("tabset2_year", "Year:", choices = c(min(gdp_tbl$year):max(gdp_tbl$year)), selected = "2020"),
               selectInput("tabset2_quarter", "Quarter:", choices = paste0(1:4), selected = "1")
             ),
             mainPanel(
               plotOutput("spider_plot1")
             )
           )
  ),
  
  tabPanel("GDP Composition Comparison: Web",
           sidebarLayout(
             sidebarPanel(
               selectInput("tabset3_data_level", "Data Level:", choices = c("Quarterly", "Annually"), selected = "Quarterly"),
               selectInput("tabset3_bench_year", "Benchmark Year:", choices = c(min(gdp_tbl$year):max(gdp_tbl$year)), selected = "2020"),
               selectInput("tabset3_bench_quarter", "Quarter:", choices = paste0(1:4), selected = "1"),
               selectInput("tabset3_select_year", "Year:", choices = c(min(gdp_tbl$year):max(gdp_tbl$year)), selected = "2020"),
               selectInput("tabset3_select_quarter", "Quarter:", choices = paste0(1:4), selected = "1")
             ),
             mainPanel(
               plotOutput("spider_plot2")
             )
           )
  ),
  
  tabPanel("GDP Composition Comparison: Arrows",
           sidebarLayout(
             sidebarPanel(
               selectInput("tabset4_data_level", "Data Level:", choices = c("Quarterly", "Annually"), selected = "Quarterly"),
               selectInput("tabset4_bench_year", "Benchmark Year:", choices = c(min(gdp_tbl$year):max(gdp_tbl$year)), selected = "2020"),
               selectInput("tabset4_bench_quarter", "Quarter:", choices = paste0(1:4), selected = "1"),
               selectInput("tabset4_select_year", "Year:", choices = c(min(gdp_tbl$year):max(gdp_tbl$year)), selected = "2021"),
               selectInput("tabset4_select_quarter", "Quarter:", choices = paste0(1:4), selected = "1")
             ),
             mainPanel(
               plotOutput("arrow_plot1"),
               plotOutput("arrow_plot2")
             )
           )
  ),
  
  tabPanel("GDP Growth Barplot",
           sidebarLayout(
             sidebarPanel(
               selectInput("tabset5_data_level", "Data Level:", choices = c("Quarterly", "Annually"), selected = "Quarterly"),
               sliderInput(inputId = "tabset5_slider_year", label = "Year", min = min(gdp_tbl$year), max = max(gdp_tbl$year), value = c(2020,max(gdp_tbl$year)), step = 1)
             ),
             mainPanel(
               plotOutput("growth_stacked_plot1")
             )
           )
  ),
  
  tabPanel("Forecasting",
           sidebarLayout(
             sidebarPanel(
               # selectInput("tabset5_data_level", "Data Level:", choices = c("Quarterly", "Annually"), selected = "Quarterly"),
               sliderInput(inputId = "tabset6_slider_ntree", label = "nTree", min = 50, max = 500, value = 100, step = 50)
             ),
             mainPanel(
               plotOutput("rf_forecast_plot"),
               tableOutput("rf_performance_table")
             )
           )
  )
)


# server ------------------------------------------------------------------


server <- function(input, output) {


# server - biplot ---------------------------------------------------------
  # gdp components for pca biplot 
  gdp_components_perc_tbl_for_biplot <- reactive({
    if(input$tabset1_data_level == "Quarterly"){
      gdp_components_perc_tbl <- 
        gdp_tbl %>% 
        select(hh:gdp) %>% 
        mutate(id = row_number()) %>% 
        relocate(id) %>% 
        group_by(id) %>% 
        mutate(across(.cols = everything(), .fns = ~100 * .x / gdp) %>% round(digits = 2)) %>% 
        ungroup() 
    } else{
      gdp_components_perc_tbl <- 
        gdp_tbl %>%
        group_by(year) %>%
        summarise(across(.cols = hh:gdp, .fns = sum)) %>% 
        ungroup() %>% 
        select(hh:gdp) %>% 
        mutate(id = row_number()) %>% 
        relocate(id) %>% 
        group_by(id) %>% 
        mutate(across(.cols = everything(), .fns = ~100 * .x / gdp) %>% round(digits = 2)) %>% 
        ungroup() 
    }
    
    return(gdp_components_perc_tbl)
    
  })
  
  gdp_biplot <- reactive({
    if(input$tabset1_data_level == "Quarterly"){
        gdp_tbl <- 
          sheet1_tbl %>%
          slice(-1) %>% 
          janitor::clean_names() %>%
          separate(col = time_period_and_dataset_code_row, into = c("year","quarter"), remove = F) %>% 
          mutate(quarter = str_extract(quarter, pattern = "[:digit:]")) %>%
          mutate(across(.cols = -time_period_and_dataset_code_row, .fns = as.numeric)) %>%
          rename(hh = households_final_consumption_expenditure, 
                 g = general_government_final_consumption_expenditure, 
                 pi = gross_fixed_capital_formation_gross_capital_formation, 
                 inv = changes_in_inventories_gross_capital_formation,
                 tb = trade_balance, 
                 gdp = gross_domestic_product_at_market_prices) %>%
          mutate(id = row_number() ) %>% 
          relocate(id)
    } else{
      gdp_tbl <- 
        sheet1_tbl %>%
        slice(-1) %>% 
        janitor::clean_names() %>%
        separate(col = time_period_and_dataset_code_row, into = c("year","quarter"), remove = F) %>% 
        mutate(quarter = str_extract(quarter, pattern = "[:digit:]")) %>%
        mutate(across(.cols = -time_period_and_dataset_code_row, .fns = as.numeric)) %>%
        rename(hh = households_final_consumption_expenditure, 
               g = general_government_final_consumption_expenditure, 
               pi = gross_fixed_capital_formation_gross_capital_formation, 
               inv = changes_in_inventories_gross_capital_formation,
               tb = trade_balance, 
               gdp = gross_domestic_product_at_market_prices) %>%
        group_by(year) %>%
        summarise(across(.cols = hh:gdp, .fns = sum)) %>% 
        mutate(id = row_number() ) %>% 
        relocate(id) %>% 
        ungroup() 
    }
    
    return(gdp_tbl)
  })
  
  #pca biplot
  output$pca_biplot <- renderPlot({
    gdp_tbl <- gdp_biplot()
    
    pca_object <- 
      gdp_components_perc_tbl_for_biplot() %>% 
      select(-id,
             -gdp) %>%
      prcomp()
    
    pca_points_tbl <- 
      pca_object %>%
      broom::augment(gdp_tbl) %>% 
      filter(year > 2015) 
    
    vectors_tbl <- 
      pca_object %>% 
      broom::tidy(matrix = "rotation") %>%
      filter(PC < 3) %>% 
      pivot_wider(id_cols = column, names_from = PC,values_from = value, names_prefix =  ".fittedPC") 
    
    arrow_style <- arrow(
      angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
    )
    
    # plot rotation matrix
    centroid_pc1 <- pca_points_tbl %>% summarise(m = mean(.fittedPC1)) %>% pull(m)
    
    centrold_pc2  <- pca_points_tbl %>% summarise(m = mean(.fittedPC2)) %>% pull(m)
    
    pca_points_tbl <- 
      pca_points_tbl %>% 
      mutate(.fittedPC1_recentered = .fittedPC1 - centroid_pc1, .fittedPC2_recentered = .fittedPC2 - centrold_pc2)
    
    
    # dynamic using arrows (path) 
    a <- grid::arrow(type = "closed",
                     length = unit(0.05, "inches"))
    
    path_tbl <-   
      pca_points_tbl %>% 
      mutate(lag_fittedPC1 = lag(.fittedPC1_recentered), 
             lag_fittedPC2 = lag(.fittedPC2_recentered)) 
    
    if(input$tabset1_data_level == "Quarterly"){
      ggplot() + 
        # year points
        geom_point(data = pca_points_tbl, aes(.fittedPC1_recentered, .fittedPC2_recentered, alpha = year),color = "gold") + 
        # path 
        geom_segment(data = path_tbl,
                     arrow = a,
                     mapping = aes(xend = .fittedPC1_recentered,
                                   yend = .fittedPC2_recentered,
                                   x = lag_fittedPC1,
                                   y = lag_fittedPC2
                     ),  alpha = 0.5) + 
        theme_light() + 
        # year labels 
        geom_text(data = pca_points_tbl %>% mutate(period = paste0(year,"-Q", quarter)), aes(.fittedPC1_recentered, .fittedPC2_recentered, alpha = year, label = period), size = 2, color = "blue") + 
        # vectors
        geom_segment(data = vectors_tbl, xend = 0, yend = 0, arrow = arrow_style, mapping = aes(2*.fittedPC1, 2*.fittedPC2), color = "red") +
        # vector's text
        geom_text(data = vectors_tbl, 
                  aes(label = column, x = 2*.fittedPC1, y= 2*.fittedPC2),
                  hjust = 1, nudge_x = -0.02, 
                  color = "#904C2F"
        ) 
    }else{
      ggplot() + 
        # year points
        geom_point(data = pca_points_tbl, aes(.fittedPC1_recentered, .fittedPC2_recentered, alpha = year),color = "gold") + 
        # path 
        geom_segment(data = path_tbl,
                     arrow = a,
                     mapping = aes(xend = .fittedPC1_recentered,
                                   yend = .fittedPC2_recentered,
                                   x = lag_fittedPC1,
                                   y = lag_fittedPC2
                     ),  alpha = 0.5) + 
        theme_light() + 
        # year labels 
        geom_text(data = pca_points_tbl %>% mutate(period = paste0(year)), aes(.fittedPC1_recentered, .fittedPC2_recentered, alpha = year, label = period), size = 2, color = "blue") + 
        # vectors
        geom_segment(data = vectors_tbl, xend = 0, yend = 0, arrow = arrow_style, mapping = aes(2*.fittedPC1, 2*.fittedPC2), color = "red") +
        # vector's text
        geom_text(data = vectors_tbl, 
                  aes(label = column, x = 2*.fittedPC1, y= 2*.fittedPC2),
                  hjust = 1, nudge_x = -0.02, 
                  color = "#904C2F"
        ) 
    }
    
  })

# spiderplot --------------------------------------------------------------
spider_gdp <- reactive({
  if(input$tabset2_data_level == "Quarterly"){
    gdp_tbl <- 
      sheet1_tbl %>%
      slice(-1) %>% 
      janitor::clean_names() %>%
      separate(col = time_period_and_dataset_code_row, into = c("year","quarter"), remove = F) %>% 
      mutate(quarter = str_extract(quarter, pattern = "[:digit:]")) %>%
      mutate(across(.cols = -time_period_and_dataset_code_row, .fns = as.numeric)) %>%
      rename(hh = households_final_consumption_expenditure, 
             g = general_government_final_consumption_expenditure, 
             pi = gross_fixed_capital_formation_gross_capital_formation, 
             inv = changes_in_inventories_gross_capital_formation,
             tb = trade_balance, 
             gdp = gross_domestic_product_at_market_prices) %>%
      mutate(id = row_number() ) %>% 
      relocate(id)
  } else{
    gdp_tbl <- 
      sheet1_tbl %>%
      slice(-1) %>% 
      janitor::clean_names() %>%
      separate(col = time_period_and_dataset_code_row, into = c("year","quarter"), remove = F) %>% 
      mutate(quarter = str_extract(quarter, pattern = "[:digit:]")) %>%
      mutate(across(.cols = -time_period_and_dataset_code_row, .fns = as.numeric)) %>%
      rename(hh = households_final_consumption_expenditure, 
             g = general_government_final_consumption_expenditure, 
             pi = gross_fixed_capital_formation_gross_capital_formation, 
             inv = changes_in_inventories_gross_capital_formation,
             tb = trade_balance, 
             gdp = gross_domestic_product_at_market_prices) %>%
      group_by(year) %>%
      summarise(across(.cols = hh:gdp, .fns = sum)) %>% 
      mutate(id = row_number() ) %>% 
      relocate(id) %>% 
      ungroup() 
  }
  
  return(gdp_tbl)
})
  
  gdp_components_perc_tbl_for_spider <- reactive({
    if(input$tabset2_data_level == "Quarterly"){
      gdp_components_perc_tbl <- 
        gdp_tbl %>% 
        select(year:gdp) %>% 
        mutate(id = row_number()) %>% 
        relocate(id) %>% 
        group_by(id) %>% 
        mutate(across(.cols = hh:gdp, .fns = ~100 * .x / gdp) %>% round(digits = 2)) %>% 
        ungroup() 
    } else{
      gdp_components_perc_tbl <- 
        gdp_tbl %>%
        group_by(year) %>%
        summarise(across(.cols = hh:gdp, .fns = sum)) %>% 
        ungroup() %>% 
        select(year:gdp) %>% 
        mutate(id = row_number()) %>% 
        relocate(id) %>% 
        group_by(id) %>% 
        mutate(across(.cols = hh:gdp, .fns = ~100 * .x / gdp) %>% round(digits = 2)) %>% 
        ungroup() 
    }
    
    return(gdp_components_perc_tbl)
    
  })
  
  
output$spider_plot1 <- renderPlot({
  
    chosen_year <-  as.numeric(input$tabset2_year)
    # print(chosen_year)
    chosen_quarter <- as.numeric(input$tabset2_quarter)
    # print(chosen_quarter)
    
    gdp_tbl <- spider_gdp() 
    
    min_tbl <- gdp_tbl %>% select(hh:other) %>% slice(2) %>% mutate_all(.funs = ~ .x * 0)
    max_tbl <- gdp_tbl %>% select(hh:other) %>% slice(2) %>% mutate_all(.funs = ~ 100 * .x / .x )
    
    if(input$tabset2_data_level=="Quarterly" ){
      # print("qarterly")
      # gdp_components_perc_tbl_for_spider() %>%
      #   glimpse() 
      
      radar_tbl <- 
        max_tbl %>%
        bind_rows(min_tbl) %>%
        bind_rows(gdp_components_perc_tbl_for_spider() %>%
                    # slice(2) %>% 
                    filter(year == chosen_year & quarter == chosen_quarter) %>%
                    select(hh:other)
        )
    } else{
      # print("Annyally")
      radar_tbl <- 
        max_tbl %>%
        bind_rows(min_tbl) %>%
        bind_rows(gdp_components_perc_tbl_for_spider() %>%
                    # slice(2) %>% 
                    filter(year == chosen_year) %>%
                    select(hh:other)
        )
    }
    
    
    
    
    
    create_beautiful_radarchart <- function(data, color = "#00AFBB",vlabels = colnames(data), vlcex = 0.7,
                                            caxislabels = NULL,title = NULL, ...){
      radarchart(
        data, axistype = 1,
        # Customize the polygon
        pcol = color,
        pfcol = scales::alpha(color, 0.5),
        plwd = 2,
        plty = 1,
        # Customize the grid
        cglcol = "grey", cglty = 1, cglwd = 0.8,
        # Customize the axis
        axislabcol = "grey", 
        # Variable labels
        vlcex = vlcex, vlabels = vlabels,
        caxislabels = caxislabels, title = title, ...
      )
    }
    
    op <- par(mar = c(1, 2, 2, 1))
    create_beautiful_radarchart(radar_tbl, caxislabels = c("0%", "25%", "50%", "75%", "100%"))
    par(op)
})  

  



# spider comparison -------------------------------------------------------
tab3_spider_gdp <- reactive({
  if(input$tabset3_data_level == "Quarterly"){
    gdp_tbl <- 
      sheet1_tbl %>%
      slice(-1) %>% 
      janitor::clean_names() %>%
      separate(col = time_period_and_dataset_code_row, into = c("year","quarter"), remove = F) %>% 
      mutate(quarter = str_extract(quarter, pattern = "[:digit:]")) %>%
      mutate(across(.cols = -time_period_and_dataset_code_row, .fns = as.numeric)) %>%
      rename(hh = households_final_consumption_expenditure, 
             g = general_government_final_consumption_expenditure, 
             pi = gross_fixed_capital_formation_gross_capital_formation, 
             inv = changes_in_inventories_gross_capital_formation,
             tb = trade_balance, 
             gdp = gross_domestic_product_at_market_prices) %>%
      mutate(id = row_number() ) %>% 
      relocate(id)
  } else{
    gdp_tbl <- 
      sheet1_tbl %>%
      slice(-1) %>% 
      janitor::clean_names() %>%
      separate(col = time_period_and_dataset_code_row, into = c("year","quarter"), remove = F) %>% 
      mutate(quarter = str_extract(quarter, pattern = "[:digit:]")) %>%
      mutate(across(.cols = -time_period_and_dataset_code_row, .fns = as.numeric)) %>%
      rename(hh = households_final_consumption_expenditure, 
             g = general_government_final_consumption_expenditure, 
             pi = gross_fixed_capital_formation_gross_capital_formation, 
             inv = changes_in_inventories_gross_capital_formation,
             tb = trade_balance, 
             gdp = gross_domestic_product_at_market_prices) %>%
      group_by(year) %>%
      summarise(across(.cols = hh:gdp, .fns = sum)) %>% 
      mutate(id = row_number() ) %>% 
      relocate(id) %>% 
      ungroup() 
  }
  
  return(gdp_tbl)
})

tab3_gdp_components_perc_tbl_for_spider <- reactive({
  if(input$tabset3_data_level == "Quarterly"){
    gdp_components_perc_tbl <- 
      gdp_tbl %>% 
      select(year:gdp) %>% 
      mutate(id = row_number()) %>% 
      relocate(id) %>% 
      group_by(id) %>% 
      mutate(across(.cols = hh:gdp, .fns = ~100 * .x / gdp) %>% round(digits = 2)) %>% 
      ungroup() 
  } else{
    gdp_components_perc_tbl <- 
      gdp_tbl %>%
      group_by(year) %>%
      summarise(across(.cols = hh:gdp, .fns = sum)) %>% 
      ungroup() %>% 
      select(year:gdp) %>% 
      mutate(id = row_number()) %>% 
      relocate(id) %>% 
      group_by(id) %>% 
      mutate(across(.cols = hh:gdp, .fns = ~100 * .x / gdp) %>% round(digits = 2)) %>% 
      ungroup() 
  }
  
  return(gdp_components_perc_tbl)
  
})


output$spider_plot2 <- renderPlot({
  
  bench_year <-  as.numeric(input$tabset3_bench_year)
  select_year <- as.numeric(input$tabset3_select_year)
  # print(chosen_year)
  bench_quarter <- as.numeric(input$tabset3_bench_quarter)
  select_quarter <- as.numeric(input$tabset3_select_quarter)
  # print(chosen_quarter)
  
  gdp_tbl <- tab3_spider_gdp() 
  
  min_tbl <- gdp_tbl %>% select(hh:other) %>% slice(2) %>% mutate_all(.funs = ~ .x * 0)
  max_tbl <- gdp_tbl %>% select(hh:other) %>% slice(2) %>% mutate_all(.funs = ~ 100 * .x / .x )
  
  if(input$tabset3_data_level=="Quarterly" ){
    # print("qarterly")
    # tab3_gdp_components_perc_tbl_for_spider() %>%
    #   glimpse() 
    
    radar_tbl <- 
      max_tbl %>%
      bind_rows(min_tbl) %>%
      bind_rows(gdp_components_perc_tbl_for_spider() %>%
                  # slice(2) %>% 
                  filter(year == bench_year & quarter == bench_quarter) %>%
                  select(hh:other)
      ) %>% 
      bind_rows(gdp_components_perc_tbl_for_spider() %>%
                  # slice(2) %>% 
                  filter(year == select_year & quarter == select_quarter) %>%
                  select(hh:other)
      )
  } else{
    # print("Annyally")
    radar_tbl <- 
      max_tbl %>%
      bind_rows(min_tbl) %>%
      bind_rows(gdp_components_perc_tbl_for_spider() %>%
                  # slice(2) %>% 
                  filter(year == bench_year) %>%
                  select(hh:other)
      ) %>% 
      bind_rows(gdp_components_perc_tbl_for_spider() %>%
                  # slice(2) %>% 
                  filter(year == select_year) %>%
                  select(hh:other)
      )
  }
  
  
  
  
  
  create_beautiful_radarchart <- function(data, color = "#00AFBB",vlabels = colnames(data), vlcex = 0.7,
                                          caxislabels = NULL,title = NULL, ...){
    radarchart(
      data, axistype = 1,
      # Customize the polygon
      pcol = color,
      pfcol = scales::alpha(color, 0.5),
      plwd = 2,
      plty = 1,
      # Customize the grid
      cglcol = "grey", cglty = 1, cglwd = 0.8,
      # Customize the axis
      axislabcol = "grey", 
      # Variable labels
      vlcex = vlcex, vlabels = vlabels,
      caxislabels = caxislabels, title = title, ...
    )
  }
  
  op <- par(mar = c(1, 2, 2, 2))
  create_beautiful_radarchart(
    data = radar_tbl, caxislabels = c("0%", "25%", "50%", "75%", "100%"),
    color = c("#00AFBB", "#E7B800", "#FC4E07")
  ) 
  legend(
    x = "bottom", legend = 1:3, horiz = TRUE,
    bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
    text.col = "black", cex = 1, pt.cex = 1.5
  )
  par(op)
})  


# arrow -------------------------------------------------------------------
tab4_arrow_gdp <- reactive({
  if(input$tabset4_data_level == "Quarterly"){
    gdp_tbl <- 
      sheet1_tbl %>%
      slice(-1) %>% 
      janitor::clean_names() %>%
      separate(col = time_period_and_dataset_code_row, into = c("year","quarter"), remove = F) %>% 
      mutate(quarter = str_extract(quarter, pattern = "[:digit:]")) %>%
      mutate(across(.cols = -time_period_and_dataset_code_row, .fns = as.numeric)) %>%
      rename(hh = households_final_consumption_expenditure, 
             g = general_government_final_consumption_expenditure, 
             pi = gross_fixed_capital_formation_gross_capital_formation, 
             inv = changes_in_inventories_gross_capital_formation,
             tb = trade_balance, 
             gdp = gross_domestic_product_at_market_prices) %>%
      mutate(id = row_number() ) %>% 
      relocate(id)
  } else{
    gdp_tbl <- 
      sheet1_tbl %>%
      slice(-1) %>% 
      janitor::clean_names() %>%
      separate(col = time_period_and_dataset_code_row, into = c("year","quarter"), remove = F) %>% 
      mutate(quarter = str_extract(quarter, pattern = "[:digit:]")) %>%
      mutate(across(.cols = -time_period_and_dataset_code_row, .fns = as.numeric)) %>%
      rename(hh = households_final_consumption_expenditure, 
             g = general_government_final_consumption_expenditure, 
             pi = gross_fixed_capital_formation_gross_capital_formation, 
             inv = changes_in_inventories_gross_capital_formation,
             tb = trade_balance, 
             gdp = gross_domestic_product_at_market_prices) %>%
      group_by(year) %>%
      summarise(across(.cols = hh:gdp, .fns = sum)) %>% 
      mutate(id = row_number() ) %>% 
      relocate(id) %>% 
      ungroup() 
  }
  
  return(gdp_tbl)
})

comp_arrow_tbl <- reactive({
  
  bench_year <- input$tabset4_bench_year
  select_year <- input$tabset4_select_year
  bench_quarter <- input$tabset4_bench_quarter
  select_quarter <- input$tabset4_select_quarter
  
  if(input$tabset4_data_level == "Quarterly"){
    benchmark_period <- paste0(bench_year,"-Q",bench_quarter)
    selected_period <- paste0(select_year,"-Q",select_quarter)
    # gdp_tbl <- tab4_arrow_gdp()
  } else{
    benchmark_period <- bench_year
    selected_period <- select_year
    # gdp_tbl <- tab4_arrow_gdp()
  }
  
  if(input$tabset4_data_level == "Quarterly"){
    comp_arrow_tbl <- 
      gdp_tbl %>% 
      mutate(period = paste0(year,"-Q",quarter)) %>%
      filter(period %in% c(benchmark_period, selected_period)) %>%
      # I need percentage change from the benchmark
      mutate(period_mark = ifelse(period == benchmark_period, 0,1)) %>% 
      mutate(period_mark_2 = ifelse(period == benchmark_period,"benchmark","selected")) %>% 
      arrange(period_mark) %>% 
      mutate(hh_lag = lag(hh),
             g_lag = lag(g),
             pi_lag = lag(pi),
             inv_lag = lag(inv),
             tb_lag = lag(tb),
             other_lag  = lag(other),
             gdp_lag = lag(gdp)) %>%
      mutate(hh_delta = hh - hh_lag, 
             g_delta = g - g_lag, 
             pi_delta = pi - pi_lag, 
             inv_delta = inv - inv_lag,
             tb_delta = tb - tb_lag, 
             other_delta = other - other_lag,
             gdp_delta = gdp - gdp_lag) %>% 
      mutate(hh_growth = 100 * hh_delta / hh_lag, 
             g_growth = 100 * g_delta / g_lag, 
             pi_growth = 100 * pi_delta / pi_lag, 
             inv_growth = 100 * inv_delta / inv_lag, 
             tb_growth = 100 * tb_delta / tb_lag, 
             other_growth = 100 * other_delta / other_lag, 
             gdp_growth = 100 * gdp_delta / gdp_lag)
  } else{
    comp_arrow_tbl <- 
      gdp_tbl %>% 
      mutate(period = paste0(year)) %>%
      filter(period %in% c(benchmark_period, selected_period)) %>%
      # I need percentage change from the benchmark
      mutate(period_mark = ifelse(period == benchmark_period, 0,1)) %>% 
      mutate(period_mark_2 = ifelse(period == benchmark_period,"benchmark","selected")) %>% 
      arrange(period_mark) %>% 
      mutate(hh_lag = lag(hh),
             g_lag = lag(g),
             pi_lag = lag(pi),
             inv_lag = lag(inv),
             tb_lag = lag(tb),
             other_lag  = lag(other),
             gdp_lag = lag(gdp)) %>%
      mutate(hh_delta = hh - hh_lag, 
             g_delta = g - g_lag, 
             pi_delta = pi - pi_lag, 
             inv_delta = inv - inv_lag,
             tb_delta = tb - tb_lag, 
             other_delta = other - other_lag,
             gdp_delta = gdp - gdp_lag) %>% 
      mutate(hh_growth = 100 * hh_delta / hh_lag, 
             g_growth = 100 * g_delta / g_lag, 
             pi_growth = 100 * pi_delta / pi_lag, 
             inv_growth = 100 * inv_delta / inv_lag, 
             tb_growth = 100 * tb_delta / tb_lag, 
             other_growth = 100 * other_delta / other_lag, 
             gdp_growth = 100 * gdp_delta / gdp_lag)
  }
  
  
  
  
  return(comp_arrow_tbl)
})

output$arrow_plot1 <- renderPlot({
  bench_year <- input$tabset4_bench_year
  select_year <- input$tabset4_select_year
  bench_quarter <- input$tabset4_bench_quarter
  select_quarter <- input$tabset4_select_quarter
  
  if(input$tabset4_data_level == "Quarterly"){
    benchmark_period <- paste0(bench_year,"-Q",bench_quarter)
    selected_period <- paste0(select_year,"-Q",select_quarter)
    gdp_tbl <- tab4_arrow_gdp()
  } else{
    benchmark_period <- bench_year
    selected_period <- select_year
    gdp_tbl <- tab4_arrow_gdp()
  }
  
  
  comp_arrow_tbl <- comp_arrow_tbl()
 
  
  # benchmark must be the centeral point
  arrow_style <- arrow(
    angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
  )
  
  comp_arrow_segment_tbl <- 
    comp_arrow_tbl %>% 
    relocate(id,time_period_and_dataset_code_row, year, quarter, period, period_mark, period_mark_2 ) %>%
    select(id:gdp_lag) %>%
    filter(period_mark_2 == "selected") %>% 
    pivot_longer(cols = hh:gdp, values_to = "benchmark",names_to = "comp") %>%
    pivot_longer(cols = hh_lag:gdp_lag, values_to = "selected", names_to = "comp_lag") %>% 
    filter(comp_lag == paste0(comp,"_lag") )
  
  comp_arrow_tbl %>% 
    select(id,time_period_and_dataset_code_row, year, quarter, period, period_mark, period_mark_2, hh:gdp ) %>%
    pivot_longer(cols = hh: gdp, names_to = "comp", values_to = "comp_value") %>% 
    ggplot() + 
    geom_point(aes(y = comp, x = comp_value, color = period_mark_2)) + 
    geom_segment(data = comp_arrow_segment_tbl, 
                 arrow = arrow_style,
                 mapping = aes(xend = selected,
                               x= benchmark ,
                               yend= comp ,
                               y= comp ), alpha = 0.5) + 
    theme_light() +
    ggtitle("Changes in GDP and its component", "Absolute Values")
})


output$arrow_plot2 <- renderPlot({
  bench_year <- input$tabset4_bench_year
  select_year <- input$tabset4_select_year
  bench_quarter <- input$tabset4_bench_quarter
  select_quarter <- input$tabset4_select_quarter
  
  if(input$tabset4_data_level == "Quarterly"){
    benchmark_period <- paste0(bench_year,"-Q",bench_quarter)
    selected_period <- paste0(select_year,"-Q",select_quarter)
    gdp_tbl <- tab4_arrow_gdp()
  } else{
    benchmark_period <- bench_year
    selected_period <- select_year
    gdp_tbl <- tab4_arrow_gdp()
  }
  
  comp_arrow_tbl <- comp_arrow_tbl()
  
  # comp_arrow_tbl <- 
  #   gdp_tbl %>% 
  #   mutate(period = paste0(year,"-Q",quarter)) %>%
  #   filter(period %in% c(benchmark_period, selected_period)) %>%
  #   # I need percentage change from the benchmark
  #   mutate(period_mark = ifelse(period == benchmark_period, 0,1)) %>% 
  #   mutate(period_mark_2 = ifelse(period == benchmark_period,"benchmark","selected")) %>% 
  #   arrange(period_mark) %>% 
  #   mutate(hh_lag = lag(hh),
  #          g_lag = lag(g),
  #          pi_lag = lag(pi),
  #          inv_lag = lag(inv),
  #          tb_lag = lag(tb),
  #          other_lag  = lag(other),
  #          gdp_lag = lag(gdp)) %>%
  #   mutate(hh_delta = hh - hh_lag, 
  #          g_delta = g - g_lag, 
  #          pi_delta = pi - pi_lag, 
  #          inv_delta = inv - inv_lag,
  #          tb_delta = tb - tb_lag, 
  #          other_delta = other - other_lag,
  #          gdp_delta = gdp - gdp_lag) %>% 
  #   mutate(hh_growth = 100 * hh_delta / hh_lag, 
  #          g_growth = 100 * g_delta / g_lag, 
  #          pi_growth = 100 * pi_delta / pi_lag, 
  #          inv_growth = 100 * inv_delta / inv_lag, 
  #          tb_growth = 100 * tb_delta / tb_lag, 
  #          other_growth = 100 * other_delta / other_lag, 
  #          gdp_growth = 100 * gdp_delta / gdp_lag) 
  
  
  # benchmark must be the centeral point
  arrow_style <- arrow(
    angle = 20, ends = "first", type = "closed", length = grid::unit(5, "pt")
  )
 
  
  
  comp_arrow_tbl %>% 
    select(id,time_period_and_dataset_code_row,
           year, quarter, period, period_mark,
           period_mark_2, hh_growth:gdp_growth ) %>%
    pivot_longer(cols = hh_growth: gdp_growth,
                 names_to = "comp",
                 values_to = "comp_value") %>% 
    filter(!is.na(comp_value)) %>%
    mutate(change = ifelse(comp_value > 0,
                           "increase", "decrease")) %>% 
    ggplot() + 
    geom_point(aes(y = comp, x = comp_value)) + 
    geom_segment( 
      arrow = arrow_style,
      xend = 0, 
      mapping = aes(x = comp_value,
                    yend= comp ,
                    y = comp, 
                    color = change
      ), alpha = 0.8) + 
    theme_light() +
    ggtitle("Changes in GDP and its component", "Percentage Change from the benchmark") +
    scale_color_manual(values = c("red","darkgreen")) +
    xlab("%") + 
    ylab("")
})


# growth stacked ----------------------------------------------------------
growth_tbl <- reactive({
  if(input$tabset5_data_level == "Quarterly"){
    temp <- 
      
      gdp_tbl %>% 
      mutate(id = row_number()) %>% 
      relocate(id) %>% 
      arrange(year, quarter) %>% 
      
      mutate(hh_lag = lag(hh),
             g_lag = lag(g),
             pi_lag = lag(pi),
             inv_lag = lag(inv),
             tb_lag = lag(tb),
             other_lag  = lag(other),
             gdp_lag = lag(gdp)) %>%
      
      mutate(hh_delta = hh - hh_lag, 
             g_delta = g - g_lag, 
             pi_delta = pi - pi_lag, 
             inv_delta = inv - inv_lag,
             tb_delta = tb - tb_lag, 
             other_delta = other - other_lag,
             gdp_delta = gdp - gdp_lag) %>% 
      
      # weight in the lag gdp
      mutate(hh_weight = hh_lag/ gdp_lag, 
             g_weight = g_lag / gdp_lag, 
             pi_weight = pi_lag/ gdp_lag, 
             inv_weight = inv_lag / gdp_lag, 
             tb_weight = tb_lag / gdp_lag, 
             other_weight = other_lag / gdp_lag) %>% 
      
      mutate(hh_growth = 100 * hh_delta / hh_lag, 
             g_growth = 100 * g_delta / g_lag, 
             pi_growth = 100 * pi_delta / pi_lag, 
             inv_growth = 100 * inv_delta / inv_lag, 
             tb_growth = 100 * tb_delta / tb_lag, 
             other_growth = 100 * other_delta / other_lag, 
             gdp_growth = 100 * gdp_delta / gdp_lag) %>%
      
      mutate(across(.cols = ends_with("growth"), .fns = ~ round(.x, digits = 4) )) %>%
      
      # contributions 
      mutate(hh_cont = hh_weight * hh_growth, 
             g_cont = g_weight * g_growth, 
             pi_cont = pi_weight * pi_growth, 
             inv_cont = inv_weight * inv_growth, 
             tb_cont = tb_weight * tb_growth,
             other_cont = other_weight * other_growth) 
  } else{
    temp <- 
      
      gdp_tbl %>% 
      mutate(id = row_number()) %>% 
      relocate(id) %>% 
      arrange(year, quarter) %>% 
      
      # rename(hh = households_final_consumption_expenditure, 
      #        g = general_government_final_consumption_expenditure, 
      #        pi = gross_fixed_capital_formation_gross_capital_formation, 
      #        inv = changes_in_inventories_gross_capital_formation,
      #        tb = trade_balance, 
      #        gdp = gross_domestic_product_at_market_prices) %>% 
      
      mutate(hh_lag = lag(hh,4),
             g_lag = lag(g,4),
             pi_lag = lag(pi,4),
             inv_lag = lag(inv,4),
             tb_lag = lag(tb,4),
             other_lag  = lag(other,4),
             gdp_lag = lag(gdp,4)) %>%
      
      mutate(hh_delta = hh - hh_lag, 
             g_delta = g - g_lag, 
             pi_delta = pi - pi_lag, 
             inv_delta = inv - inv_lag,
             tb_delta = tb - tb_lag, 
             other_delta = other - other_lag,
             gdp_delta = gdp - gdp_lag) %>% 
      # there are different ways to calculate the weights
      # average weight 
      # mutate(hh_weight = (hh+hh_lag)/ (gdp+gdp_lag), 
      #        g_weight = (g + g_lag) / (gdp+gdp_lag), 
      #        pi_weight = (pi + pi_lag)/ (gdp+gdp_lag), 
      #        inv_weight = (inv + inv_lag) / (gdp+gdp_lag), 
      #        tb_weight = (tb + tb_lag) / (gdp+gdp_lag), 
      #        other_weight = (other + other_lag) / (gdp+gdp_lag)) %>% 
      # weight in the lag gdp
      mutate(hh_weight = hh_lag/ gdp_lag, 
             g_weight = g_lag / gdp_lag, 
             pi_weight = pi_lag/ gdp_lag, 
             inv_weight = inv_lag / gdp_lag, 
             tb_weight = tb_lag / gdp_lag, 
             other_weight = other_lag / gdp_lag) %>% 
      
      mutate(hh_growth = 100 * hh_delta / hh_lag, 
             g_growth = 100 * g_delta / g_lag, 
             pi_growth = 100 * pi_delta / pi_lag, 
             inv_growth = 100 * inv_delta / inv_lag, 
             tb_growth = 100 * tb_delta / tb_lag, 
             other_growth = 100 * other_delta / other_lag, 
             gdp_growth = 100 * gdp_delta / gdp_lag) %>%
      
      mutate(across(.cols = ends_with("growth"), .fns = ~ round(.x, digits = 4) )) %>%
      
      # contributions 
      mutate(hh_cont = hh_weight * hh_growth, 
             g_cont = g_weight * g_growth, 
             pi_cont = pi_weight * pi_growth, 
             inv_cont = inv_weight * inv_growth, 
             tb_cont = tb_weight * tb_growth,
             other_cont = other_weight * other_growth) 
  }
  
  
  return(temp)
})


last_period_growth_tbl <- reactive({
  lower_bound_year <- input$tabset5_slider_year[1] %>% as.numeric()
  upper_bound_year <- input$tabset5_slider_year[2] %>% as.numeric()
  
  growth_tbl() %>% 
    glimpse()
  
  growth_for_lineplot_tbl <- 
    growth_tbl() %>% 
    filter(year >= lower_bound_year & year <= upper_bound_year) %>% 
    select(year, quarter, gdp_growth) %>% 
    mutate(period = paste0(year,"-Q",quarter)) %>%
    mutate(gdp_color = gdp_growth > 0)
})

output$growth_stacked_plot1 <- renderPlot({
  # last_period_growth_tbl() %>% 
  #   glimpse()
  lower_bound_year <- input$tabset5_slider_year[1] %>% as.numeric()
  upper_bound_year <- input$tabset5_slider_year[2] %>% as.numeric()

  growth_tbl() %>% 
    filter(year >= lower_bound_year & year <= upper_bound_year) %>% 
    select_at(vars(ends_with("_cont")|"year"|"quarter")) %>% 
    pivot_longer(cols = ends_with("_cont"), names_to = "factor", values_to = "contribution") %>%
    mutate(period = paste0(year,"-Q",quarter)) %>%
    ggplot() + 
    geom_col(aes(x = period, y = contribution, fill = factor)) +
    geom_point(last_period_growth_tbl(), mapping = aes(x = period, y = gdp_growth)) + 
    geom_line(last_period_growth_tbl(), mapping = aes(x = period, y = gdp_growth, group = 1)) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
    xlab("") + 
    theme_light()
})


# forecasting rf ----------------------------------------------------------

rf_output <- reactive({
  ntree_choice <- input$tabset6_slider_ntree
  
  forest <- randomForest( quarterly_gdp_growth ~ .,
                          data = forecasting_tbl_train_for_rf,
                          ntree = ntree_choice, 
                          localImp = TRUE,
                          importance = TRUE)
  
  return(forest)
})

output$rf_forecast_plot <- renderPlot({
  forest <- rf_output()
  
  predicted_values <- predict(forest, newdata = test_tbl_w_lag)
  
  # visualization of horizon forecast
  forecasting_tbl_train_for_rf %>% 
    mutate(predicted = forest$predicted) %>% 
    mutate(date = forecasting_tbl_w_lag %>% 
             filter(date >= "2003-09-30") %>% pull(date)) %>%
    bind_rows(test_tbl_w_lag %>% mutate(predicted = predicted_values)) %>%
    ggplot() + 
    geom_point(aes(x = date, y = quarterly_gdp_growth), color = "darkblue") + 
    geom_line(aes(x = date, y = quarterly_gdp_growth), color = "darkblue", alpha = 0.5) + 
    geom_point(
      aes(x = date, y = predicted),
      color = "darkred") + 
    geom_line(
      aes(x = date, y = predicted),
      color = "darkred", alpha = 0.5)
})

output$rf_performance_table <- renderTable({
  forest <- rf_output()
  
  error_tbl <- 
    forecasting_tbl_train_for_rf %>% 
    mutate(predicted = forest$predicted) %>% 
    mutate(date = forecasting_tbl_w_lag %>% 
             filter(date >= "2003-09-30") %>% pull(date)) %>%
    select(date, quarterly_gdp_growth, predicted) %>% 
    mutate(error = quarterly_gdp_growth - predicted) 
  
  mae <- 
    error_tbl %>%
    mutate(abs_error = abs(error)) %>% 
    summarise(mae = mean(abs_error)) %>% 
    pull(mae)
  
  rmse <- 
    error_tbl %>%
    mutate(square_error = error ** 2) %>%
    summarise(rmse = sqrt(mean(square_error)))
  
  r2 <- 
    error_tbl %>% 
    mutate(mean_obs = mean(quarterly_gdp_growth)) %>%
    mutate(basemodel_error = quarterly_gdp_growth-mean_obs) %>% 
    summarise(tss = sum(basemodel_error**2), rss = sum(error**2)) %>% 
    mutate(r2 = 1 - (rss/tss)) %>% 
    pull(r2)
  
  tibble(MAE = mae, RMSE = rmse, R2 = r2)
})

} # server



# Run the application 
shinyApp(ui = ui, server = server)

# PSet 2 test script
# A Hart

# broken code ----
  library(tidyverse) 
  library(knitr)  # 1. load knitr for kable

# open my data
  gspace = read_csv('greenspace_data_share.csv') # 2. quote filename
  
# summarize average urban greenspace by region
  table =
    gspace |> 
    group_by(Major_Geo_Region) |> # 3. add pipe
    summarise(
      obs = n(), # 4. comma at end
      avg = mean(annual_avg_2020), #5. fix var name
      `weighted avg` = mean(annual_weight_avg_2020) # 6. fix output name
  )
# output as table
  kable(table, digits = 1) # 7. correct object is table

  
  
# load packages
  library(tidyverse)
  library(scales)
  library(knitr)
  
# data ----
## load  
  df = read_csv("https://dataverse.harvard.edu/api/access/datafile/6903364")

## clean
  gspace =
    df |>
    select(-1) |>
    mutate(
      ind_2021f =
        fct_relevel(
          indicator_2021,
          "Exceptionally Low",
          "Very Low",
          "Low",
          "Moderate",
          "High",
          "Very High"
        )
    ) 
    
    
    
## 3. how many urban areas
  nrow(gspace)

  
## 4. indicator in 2021
  gspace |>
    count(ind_2021f, name = "Freq") |>
    mutate(
      Per = Freq / sum(Freq) * 100
    ) |>
    rename(`Greenspace (2021)` = ind_2021f) |>
    kable(digits = 1L)
  
## 5. number of urban areas
### high in 2015
  gspace |>
    filter(indicator_2015 %in% c('High', 'Very High')) |>
    nrow()
  
### scored exceptionally low in any year
  gspace |>
    filter(
      if_any(starts_with('indicator'),  ~ .x == "Exceptionally Low")
    ) |>
    nrow()
 
### arid areas greener 2010 to 2020
  gspace |>
    filter(
      Climate_region == 'Arid',
      annual_weight_avg_2020 > annual_weight_avg_2010
    ) |>
    nrow()
  
## 6. Urban areas less green 2010 to 2021
  gt = gspace |>
    mutate(
      less_green = annual_avg_2010 > annual_avg_2021
    ) |>
    count(less_green, Major_Geo_Region) |>
    pivot_wider(
      names_from = less_green,
      values_from = n,
      values_fill = 0
    ) |>
    mutate(Percent = `TRUE` / (`FALSE` + `TRUE`) * 100) |>
    select(1, 3, 5) |>
    rename(
      Continent = Major_Geo_Region,
      Freq = `TRUE`
    ) 
  
  gt |>
    kable(digits = 1L)
  
  sum(gt$Freq)
  
## Histogram change 2010 to 2021
  gspace |>
    mutate(
      change = annual_avg_2021 - annual_avg_2010
    ) |>
    ggplot(aes(x = change, fill = change > 0)) +
    geom_histogram(
      color = 'white', 
      boundary = 0,
      closed = 'left'
    ) +
    labs(
      x = "Change in NDVI Greenspace (2010 to 2021)",
      y = "Urban areas"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_fill_manual(values = c('orange2','seagreen4')) +
    theme_classic() +
    theme(legend.position = 'none')
  
## plot of greenspace 2021 over 2010
  gspace |>
    ggplot(aes(x = annual_weight_avg_2010, y = annual_weight_avg_2021)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(
      aes(color = annual_weight_avg_2021 > annual_weight_avg_2010),
      alpha = .5
    ) +
    scale_y_continuous(limits = c(0, 0.75)) +
    scale_x_continuous(limits = c(0, 0.75)) +
    scale_color_manual(values = c('orange2','seagreen4')) +
    labs(
      y = 'NDVI 2021 (weighted)',
      x = 'NDVI 2010 (weighted)'
    ) +
    theme_classic() +
    theme(legend.position = 'none')

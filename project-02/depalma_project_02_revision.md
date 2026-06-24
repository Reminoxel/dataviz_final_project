---
title: "Data Visualization Mini-Project 2 Revision" 
author: "Joseph DePalma - JDePalma1039@floridapoly.edu" 
output:
  html_document:
    df_print: paged
    keep_md: true
---


``` r
library(tidyverse)
library(lubridate)
library(plotly)
library(htmlwidgets)
library(broom)
library(sf)
library(scales)
library(viridis)
library(maps)
library(grid)
```

<style type="text/css">
body {
  background-color: black;
}

.main-container {
  background-color: black;
  color: white;
}
</style>

I learned how to make the whole paper dark!

# Introduction

For this mini-project I used three datasets: `us_births_00_14.csv`, `atl-weather.csv`, and `Florida_Lakes.zip`. The births dataset shows daily birth totals in the United States from 2000 to 2014. The weather dataset shows daily weather conditions in Atlanta during 2019. The Florida Lakes dataset is a shapefile dataset used for the spatial visualization.

I picked the births and weather datasets because they both have time-based patterns, but in different ways. Births seem like they would be random at first, but the data shows that human behavior and scheduling probably matter a lot. Weather is more naturally seasonal, so it gives a different type of pattern to compare.

The main thing I wanted to look at was how births change by weekday and month, and how Atlanta weather changes across the year. I also wanted to include more than just simple count charts this time, so I added an interactive heatmap, a spatial visualization, and a model visualization.

The original charts I planned to make were:

1. A bar chart of average births by weekday
2. An interactive heatmap of births by month and weekday
3. A line chart of Atlanta temperature by month
4. A spatial map of Florida lakes using shapefile data
5. A coefficient plot from a weather model

# Load and Explore the Data

Read the datasets from the `data` folder.


``` r
births <- read_csv("../data/us_births_00_14.csv", col_types = cols())
weather <- read_csv("../data/atl-weather.csv", col_types = cols())
```

Use `glimpse()` to look at the dataset structures.


``` r
glimpse(births)
```

```
## Rows: 5,479
## Columns: 6
## $ year          <dbl> 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 20…
## $ month         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
## $ date_of_month <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
## $ date          <date> 2000-01-01, 2000-01-02, 2000-01-03, 2000-01-04, 2000-01…
## $ day_of_week   <chr> "Sat", "Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat…
## $ births        <dbl> 9083, 8006, 11363, 13032, 12558, 12466, 12516, 8934, 794…
```

``` r
glimpse(weather)
```

```
## Rows: 365
## Columns: 40
## $ time                        <dttm> 2019-01-01 05:00:00, 2019-01-02 05:00:00,…
## $ summary                     <chr> "Light rain in the morning and afternoon."…
## $ icon                        <chr> "rain", "rain", "rain", "rain", "partly-cl…
## $ sunriseTime                 <dttm> 2019-01-01 12:44:00, 2019-01-02 12:44:00,…
## $ sunsetTime                  <dttm> 2019-01-01 22:41:00, 2019-01-02 22:42:00,…
## $ moonPhase                   <dbl> 0.87, 0.91, 0.94, 0.97, 0.00, 0.03, 0.06, …
## $ precipIntensity             <dbl> 0.0190, 0.0150, 0.0124, 0.0480, 0.0001, 0.…
## $ precipIntensityMax          <dbl> 0.2586, 0.0787, 0.0845, 0.2716, 0.0003, 0.…
## $ precipIntensityMaxTime      <dttm> 2019-01-01 06:02:00, 2019-01-03 01:57:00,…
## $ precipProbability           <dbl> 0.99, 0.96, 0.99, 0.99, 0.05, 0.04, 0.01, …
## $ precipType                  <chr> "rain", "rain", "rain", "rain", "rain", "r…
## $ temperatureHigh             <dbl> 63.90, 57.37, 55.30, 64.98, 58.64, 69.61, …
## $ temperatureHighTime         <dbl> 1546374000, 1546454280, 1546552680, 154663…
## $ temperatureLow              <dbl> 50.58, 49.03, 53.08, 42.95, 42.52, 42.00, …
## $ temperatureLowTime          <dbl> 1546432500, 1546474680, 1546563780, 154668…
## $ apparentTemperatureHigh     <dbl> 63.40, 56.87, 54.80, 64.48, 58.14, 69.11, …
## $ apparentTemperatureHighTime <dbl> 1546374000, 1546454280, 1546552680, 154663…
## $ apparentTemperatureLow      <dbl> 51.07, 49.52, 53.57, 38.67, 41.13, 42.49, …
## $ apparentTemperatureLowTime  <dbl> 1546432500, 1546474680, 1546563780, 154668…
## $ dewPoint                    <dbl> 58.08, 48.94, 50.41, 49.99, 36.50, 38.65, …
## $ humidity                    <dbl> 0.89, 0.86, 0.92, 0.85, 0.63, 0.59, 0.63, …
## $ pressure                    <dbl> 1019.1, 1021.4, 1017.1, 1009.4, 1016.1, 10…
## $ windSpeed                   <dbl> 2.88, 2.79, 2.85, 6.94, 7.43, 3.46, 3.61, …
## $ windGust                    <dbl> 14.77, 8.09, 8.71, 23.05, 21.95, 10.39, 12…
## $ windGustTime                <dbl> 1546318800, 1546469340, 1546538880, 154663…
## $ windBearing                 <dbl> 316, 12, 355, 214, 291, 315, 146, 240, 320…
## $ cloudCover                  <dbl> 0.72, 0.75, 0.97, 0.79, 0.35, 0.04, 0.26, …
## $ uvIndex                     <dbl> 3, 3, 3, 3, 4, 4, 4, 3, 4, 4, 4, 3, 3, 3, …
## $ uvIndexTime                 <dbl> 1546363800, 1546451400, 1546537620, 154662…
## $ visibility                  <dbl> 5.998, 6.080, 5.418, 5.967, 8.409, 8.853, …
## $ ozone                       <dbl> 213.1, 220.6, 240.2, 256.7, 269.1, 226.4, …
## $ temperatureMin              <dbl> 55.94, 49.03, 50.05, 45.02, 42.95, 42.52, …
## $ temperatureMinTime          <dttm> 2019-01-02 04:00:00, 2019-01-03 00:18:00,…
## $ temperatureMax              <dbl> 65.76, 57.37, 55.30, 64.98, 58.64, 69.61, …
## $ temperatureMaxTime          <dttm> 2019-01-01 05:00:00, 2019-01-02 18:38:00,…
## $ apparentTemperatureMin      <dbl> 56.43, 49.52, 50.54, 41.26, 38.67, 41.13, …
## $ apparentTemperatureMinTime  <dttm> 2019-01-02 04:00:00, 2019-01-03 00:18:00,…
## $ apparentTemperatureMax      <dbl> 65.92, 56.87, 54.80, 64.48, 58.14, 69.11, …
## $ apparentTemperatureMaxTime  <dttm> 2019-01-01 05:00:00, 2019-01-02 18:38:00,…
## $ precipAccumulation          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
```

Check the dimensions of both datasets.


``` r
dim(births)
```

```
## [1] 5479    6
```

``` r
dim(weather)
```

```
## [1] 365  40
```

Look at the column names.


``` r
names(births)
```

```
## [1] "year"          "month"         "date_of_month" "date"         
## [5] "day_of_week"   "births"
```

``` r
names(weather)
```

```
##  [1] "time"                        "summary"                    
##  [3] "icon"                        "sunriseTime"                
##  [5] "sunsetTime"                  "moonPhase"                  
##  [7] "precipIntensity"             "precipIntensityMax"         
##  [9] "precipIntensityMaxTime"      "precipProbability"          
## [11] "precipType"                  "temperatureHigh"            
## [13] "temperatureHighTime"         "temperatureLow"             
## [15] "temperatureLowTime"          "apparentTemperatureHigh"    
## [17] "apparentTemperatureHighTime" "apparentTemperatureLow"     
## [19] "apparentTemperatureLowTime"  "dewPoint"                   
## [21] "humidity"                    "pressure"                   
## [23] "windSpeed"                   "windGust"                   
## [25] "windGustTime"                "windBearing"                
## [27] "cloudCover"                  "uvIndex"                    
## [29] "uvIndexTime"                 "visibility"                 
## [31] "ozone"                       "temperatureMin"             
## [33] "temperatureMinTime"          "temperatureMax"             
## [35] "temperatureMaxTime"          "apparentTemperatureMin"     
## [37] "apparentTemperatureMinTime"  "apparentTemperatureMax"     
## [39] "apparentTemperatureMaxTime"  "precipAccumulation"
```

Look at a few of the variables that will be used for the plots.


``` r
births %>%
  select(year, month, date, day_of_week, births) %>%
  head()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["year"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["month"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["date"],"name":[3],"type":["date"],"align":["right"]},{"label":["day_of_week"],"name":[4],"type":["chr"],"align":["left"]},{"label":["births"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"2000","2":"1","3":"2000-01-01","4":"Sat","5":"9083"},{"1":"2000","2":"1","3":"2000-01-02","4":"Sun","5":"8006"},{"1":"2000","2":"1","3":"2000-01-03","4":"Mon","5":"11363"},{"1":"2000","2":"1","3":"2000-01-04","4":"Tues","5":"13032"},{"1":"2000","2":"1","3":"2000-01-05","4":"Wed","5":"12558"},{"1":"2000","2":"1","3":"2000-01-06","4":"Thurs","5":"12466"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

``` r
weather %>%
  select(time, summary, icon, precipProbability, temperatureHigh, temperatureLow, humidity, cloudCover, windSpeed) %>%
  head()
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["time"],"name":[1],"type":["dttm"],"align":["right"]},{"label":["summary"],"name":[2],"type":["chr"],"align":["left"]},{"label":["icon"],"name":[3],"type":["chr"],"align":["left"]},{"label":["precipProbability"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["temperatureHigh"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["temperatureLow"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["humidity"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["cloudCover"],"name":[8],"type":["dbl"],"align":["right"]},{"label":["windSpeed"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"2019-01-01 05:00:00","2":"Light rain in the morning and afternoon.","3":"rain","4":"0.99","5":"63.90","6":"50.58","7":"0.89","8":"0.72","9":"2.88"},{"1":"2019-01-02 05:00:00","2":"Rain starting in the afternoon.","3":"rain","4":"0.96","5":"57.37","6":"49.03","7":"0.86","8":"0.75","9":"2.79"},{"1":"2019-01-03 05:00:00","2":"Rain throughout the day.","3":"rain","4":"0.99","5":"55.30","6":"53.08","7":"0.92","8":"0.97","9":"2.85"},{"1":"2019-01-04 05:00:00","2":"Heavy rain in the morning and afternoon.","3":"rain","4":"0.99","5":"64.98","6":"42.95","7":"0.85","8":"0.79","9":"6.94"},{"1":"2019-01-05 05:00:00","2":"Clear throughout the day.","3":"partly-cloudy-day","4":"0.05","5":"58.64","6":"42.52","7":"0.63","8":"0.35","9":"7.43"},{"1":"2019-01-06 05:00:00","2":"Clear throughout the day.","3":"clear-day","4":"0.04","5":"69.61","6":"42.00","7":"0.59","8":"0.04","9":"3.46"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

# Clean the Data

The datasets were already pretty usable, but I cleaned them so the dates, months, weekdays, and weather categories worked better in the charts.


``` r
births_clean <- births %>%
  mutate(
    date = ymd(date),
    month_name = factor(month.abb[month], levels = month.abb),
    day_of_week = factor(
      day_of_week,
      levels = c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
    ),
    day_type = if_else(day_of_week %in% c("Sat", "Sun"), "Weekend", "Weekday")
  )

weather_clean <- weather %>%
  mutate(
    date = as.Date(ymd_hms(time)),
    month = month(date),
    month_name = factor(month.abb[month], levels = month.abb),
    precipIntensity = replace_na(precipIntensity, 0),
    precipProbability = replace_na(precipProbability, 0),
    humidity = replace_na(humidity, 0),
    cloudCover = replace_na(cloudCover, 0),
    windSpeed = replace_na(windSpeed, 0),
    rainy_day = precipProbability >= 0.5 | icon == "rain",
    temperature_range = temperatureHigh - temperatureLow
  )
```

Check the cleaned datasets.


``` r
glimpse(births_clean)
```

```
## Rows: 5,479
## Columns: 8
## $ year          <dbl> 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 2000, 20…
## $ month         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
## $ date_of_month <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 1…
## $ date          <date> 2000-01-01, 2000-01-02, 2000-01-03, 2000-01-04, 2000-01…
## $ day_of_week   <fct> Sat, Sun, Mon, Tues, Wed, Thurs, Fri, Sat, Sun, Mon, Tue…
## $ births        <dbl> 9083, 8006, 11363, 13032, 12558, 12466, 12516, 8934, 794…
## $ month_name    <fct> Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, J…
## $ day_type      <chr> "Weekend", "Weekend", "Weekday", "Weekday", "Weekday", "…
```

``` r
glimpse(weather_clean)
```

```
## Rows: 365
## Columns: 45
## $ time                        <dttm> 2019-01-01 05:00:00, 2019-01-02 05:00:00,…
## $ summary                     <chr> "Light rain in the morning and afternoon."…
## $ icon                        <chr> "rain", "rain", "rain", "rain", "partly-cl…
## $ sunriseTime                 <dttm> 2019-01-01 12:44:00, 2019-01-02 12:44:00,…
## $ sunsetTime                  <dttm> 2019-01-01 22:41:00, 2019-01-02 22:42:00,…
## $ moonPhase                   <dbl> 0.87, 0.91, 0.94, 0.97, 0.00, 0.03, 0.06, …
## $ precipIntensity             <dbl> 0.0190, 0.0150, 0.0124, 0.0480, 0.0001, 0.…
## $ precipIntensityMax          <dbl> 0.2586, 0.0787, 0.0845, 0.2716, 0.0003, 0.…
## $ precipIntensityMaxTime      <dttm> 2019-01-01 06:02:00, 2019-01-03 01:57:00,…
## $ precipProbability           <dbl> 0.99, 0.96, 0.99, 0.99, 0.05, 0.04, 0.01, …
## $ precipType                  <chr> "rain", "rain", "rain", "rain", "rain", "r…
## $ temperatureHigh             <dbl> 63.90, 57.37, 55.30, 64.98, 58.64, 69.61, …
## $ temperatureHighTime         <dbl> 1546374000, 1546454280, 1546552680, 154663…
## $ temperatureLow              <dbl> 50.58, 49.03, 53.08, 42.95, 42.52, 42.00, …
## $ temperatureLowTime          <dbl> 1546432500, 1546474680, 1546563780, 154668…
## $ apparentTemperatureHigh     <dbl> 63.40, 56.87, 54.80, 64.48, 58.14, 69.11, …
## $ apparentTemperatureHighTime <dbl> 1546374000, 1546454280, 1546552680, 154663…
## $ apparentTemperatureLow      <dbl> 51.07, 49.52, 53.57, 38.67, 41.13, 42.49, …
## $ apparentTemperatureLowTime  <dbl> 1546432500, 1546474680, 1546563780, 154668…
## $ dewPoint                    <dbl> 58.08, 48.94, 50.41, 49.99, 36.50, 38.65, …
## $ humidity                    <dbl> 0.89, 0.86, 0.92, 0.85, 0.63, 0.59, 0.63, …
## $ pressure                    <dbl> 1019.1, 1021.4, 1017.1, 1009.4, 1016.1, 10…
## $ windSpeed                   <dbl> 2.88, 2.79, 2.85, 6.94, 7.43, 3.46, 3.61, …
## $ windGust                    <dbl> 14.77, 8.09, 8.71, 23.05, 21.95, 10.39, 12…
## $ windGustTime                <dbl> 1546318800, 1546469340, 1546538880, 154663…
## $ windBearing                 <dbl> 316, 12, 355, 214, 291, 315, 146, 240, 320…
## $ cloudCover                  <dbl> 0.72, 0.75, 0.97, 0.79, 0.35, 0.04, 0.26, …
## $ uvIndex                     <dbl> 3, 3, 3, 3, 4, 4, 4, 3, 4, 4, 4, 3, 3, 3, …
## $ uvIndexTime                 <dbl> 1546363800, 1546451400, 1546537620, 154662…
## $ visibility                  <dbl> 5.998, 6.080, 5.418, 5.967, 8.409, 8.853, …
## $ ozone                       <dbl> 213.1, 220.6, 240.2, 256.7, 269.1, 226.4, …
## $ temperatureMin              <dbl> 55.94, 49.03, 50.05, 45.02, 42.95, 42.52, …
## $ temperatureMinTime          <dttm> 2019-01-02 04:00:00, 2019-01-03 00:18:00,…
## $ temperatureMax              <dbl> 65.76, 57.37, 55.30, 64.98, 58.64, 69.61, …
## $ temperatureMaxTime          <dttm> 2019-01-01 05:00:00, 2019-01-02 18:38:00,…
## $ apparentTemperatureMin      <dbl> 56.43, 49.52, 50.54, 41.26, 38.67, 41.13, …
## $ apparentTemperatureMinTime  <dttm> 2019-01-02 04:00:00, 2019-01-03 00:18:00,…
## $ apparentTemperatureMax      <dbl> 65.92, 56.87, 54.80, 64.48, 58.14, 69.11, …
## $ apparentTemperatureMaxTime  <dttm> 2019-01-01 05:00:00, 2019-01-02 18:38:00,…
## $ precipAccumulation          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ date                        <date> 2019-01-01, 2019-01-02, 2019-01-03, 2019-…
## $ month                       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
## $ month_name                  <fct> Jan, Jan, Jan, Jan, Jan, Jan, Jan, Jan, Ja…
## $ rainy_day                   <lgl> TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALS…
## $ temperature_range           <dbl> 13.32, 8.34, 2.22, 22.03, 16.12, 27.61, 22…
```

# Missing Values

I checked missing values because missing data can affect charts and models.


``` r
births_clean %>%
  summarize(
    missing_date = sum(is.na(date)),
    missing_year = sum(is.na(year)),
    missing_month = sum(is.na(month)),
    missing_weekday = sum(is.na(day_of_week)),
    missing_births = sum(is.na(births))
  )
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["missing_date"],"name":[1],"type":["int"],"align":["right"]},{"label":["missing_year"],"name":[2],"type":["int"],"align":["right"]},{"label":["missing_month"],"name":[3],"type":["int"],"align":["right"]},{"label":["missing_weekday"],"name":[4],"type":["int"],"align":["right"]},{"label":["missing_births"],"name":[5],"type":["int"],"align":["right"]}],"data":[{"1":"0","2":"0","3":"0","4":"0","5":"0"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


``` r
weather_clean %>%
  summarize(
    missing_date = sum(is.na(date)),
    missing_summary = sum(is.na(summary)),
    missing_icon = sum(is.na(icon)),
    missing_high_temp = sum(is.na(temperatureHigh)),
    missing_low_temp = sum(is.na(temperatureLow)),
    missing_humidity = sum(is.na(humidity)),
    missing_cloud_cover = sum(is.na(cloudCover)),
    missing_wind_speed = sum(is.na(windSpeed))
  )
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["missing_date"],"name":[1],"type":["int"],"align":["right"]},{"label":["missing_summary"],"name":[2],"type":["int"],"align":["right"]},{"label":["missing_icon"],"name":[3],"type":["int"],"align":["right"]},{"label":["missing_high_temp"],"name":[4],"type":["int"],"align":["right"]},{"label":["missing_low_temp"],"name":[5],"type":["int"],"align":["right"]},{"label":["missing_humidity"],"name":[6],"type":["int"],"align":["right"]},{"label":["missing_cloud_cover"],"name":[7],"type":["int"],"align":["right"]},{"label":["missing_wind_speed"],"name":[8],"type":["int"],"align":["right"]}],"data":[{"1":"0","2":"0","3":"1","4":"0","5":"0","6":"0","7":"0","8":"0"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

# Basic Summary

Here is a basic summary of the births dataset.


``` r
births_clean %>%
  summarize(
    total_days = n(),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    total_births = sum(births, na.rm = TRUE),
    average_daily_births = round(mean(births, na.rm = TRUE), 1)
  )
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["total_days"],"name":[1],"type":["int"],"align":["right"]},{"label":["first_year"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["last_year"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["total_births"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["average_daily_births"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"5479","2":"2000","3":"2014","4":"62187024","5":"11350.1"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Here is a basic summary of the Atlanta weather dataset.


``` r
weather_clean %>%
  summarize(
    total_days = n(),
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    average_high_temp = round(mean(temperatureHigh, na.rm = TRUE), 1),
    average_low_temp = round(mean(temperatureLow, na.rm = TRUE), 1),
    rainy_days = sum(rainy_day, na.rm = TRUE)
  )
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["total_days"],"name":[1],"type":["int"],"align":["right"]},{"label":["first_date"],"name":[2],"type":["date"],"align":["right"]},{"label":["last_date"],"name":[3],"type":["date"],"align":["right"]},{"label":["average_high_temp"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["average_low_temp"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["rainy_days"],"name":[6],"type":["int"],"align":["right"]}],"data":[{"1":"365","2":"2019-01-01","3":"2019-12-31","4":"74.4","5":"56.9","6":"176"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

# Visualization 1: Average Births by Weekday

The first chart shows average births by weekday.


``` r
births_weekday <- births_clean %>%
  group_by(day_of_week, day_type) %>%
  summarize(
    average_births = mean(births, na.rm = TRUE),
    total_births = sum(births, na.rm = TRUE),
    .groups = "drop"
  )

births_weekday
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["day_of_week"],"name":[1],"type":["fct"],"align":["left"]},{"label":["day_type"],"name":[2],"type":["chr"],"align":["left"]},{"label":["average_births"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["total_births"],"name":[4],"type":["dbl"],"align":["right"]}],"data":[{"1":"Mon","2":"Weekday","3":"11897.830","4":"9316001"},{"1":"Tues","2":"Weekday","3":"13122.444","4":"10274874"},{"1":"Wed","2":"Weekday","3":"12910.766","4":"10109130"},{"1":"Thurs","2":"Weekday","3":"12845.826","4":"10045436"},{"1":"Fri","2":"Weekday","3":"12596.162","4":"9850199"},{"1":"Sat","2":"Weekend","3":"8562.573","4":"6704495"},{"1":"Sun","2":"Weekend","3":"7518.377","4":"5886889"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


``` r
highest_day <- births_weekday %>%
  slice_max(average_births, n = 1, with_ties = FALSE)

ggplot(births_weekday, aes(x = day_of_week, y = average_births, fill = day_type)) +
  geom_col(width = 0.75) +
  geom_text(
    aes(label = comma(round(average_births))),
    vjust = -0.45,
    size = 3.5,
    color = "white",
    fontface = "bold"
  ) +
  geom_label(
    data = highest_day,
    aes(
      x = day_of_week,
      y = average_births + 2300,
      label = "Highest average"
    ),
    inherit.aes = FALSE,
    fill = "black",
    color = "white",
    fontface = "bold",
    label.size = 0.25,
    size = 3.7
  ) +
  scale_fill_manual(values = c("Weekday" = "steelblue", "Weekend" = "darkorange")) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.28))
  ) +
  labs(
    title = "Average U.S. Births by Weekday",
    subtitle = "Births are higher on weekdays than weekends",
    x = "Day of the Week",
    y = "Average Daily Births",
    fill = "Day Type"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(fill = "black", color = NA),
  
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray25"),
  
    plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray85", hjust = 0.5),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white", margin = margin(r = 14)),
    axis.text = element_text(color = "gray85"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "gray85"),
    legend.position = "bottom",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 18)
)
```

```
## Warning: The `label.size` argument of `geom_label()` is deprecated as of ggplot2 3.5.0.
## ℹ Please use the `linewidth` argument instead.
## This warning is displayed once per session.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

<img src="depalma_project_02_revision_files/figure-html/average-births-weekday-1.png" alt="Bar chart showing average U.S. births by weekday, with weekdays higher than weekends and the highest average day labeled." width="100%" />

This chart shows that average births are higher during the week and lower on weekends. I used a bar chart because weekdays are categories. This also fixes one of the issues from my first project, where the weekday chart probably should have been a bar chart instead of a line chart.

The pattern is interesting because it probably does not only reflect when babies naturally arrive. It also likely reflects scheduled deliveries, hospital staffing, and other human systems.

# Visualization 2: Interactive Birth Heatmap

Next, I looked at births by both month and weekday.


``` r
births_month_weekday <- births_clean %>%
  group_by(month_name, day_of_week) %>%
  summarize(
    average_births = mean(births, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    tooltip = paste0(
      "Month: ", month_name,
      "<br>Day: ", day_of_week,
      "<br>Average births: ", comma(round(average_births))
    )
  )

births_month_weekday
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["month_name"],"name":[1],"type":["fct"],"align":["left"]},{"label":["day_of_week"],"name":[2],"type":["fct"],"align":["left"]},{"label":["average_births"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["tooltip"],"name":[4],"type":["chr"],"align":["left"]}],"data":[{"1":"Jan","2":"Mon","3":"11322.657","4":"Month: Jan<br>Day: Mon<br>Average births: 11,323"},{"1":"Jan","2":"Tues","3":"12384.493","4":"Month: Jan<br>Day: Tues<br>Average births: 12,384"},{"1":"Jan","2":"Wed","3":"12266.254","4":"Month: Jan<br>Day: Wed<br>Average births: 12,266"},{"1":"Jan","2":"Thurs","3":"12398.985","4":"Month: Jan<br>Day: Thurs<br>Average births: 12,399"},{"1":"Jan","2":"Fri","3":"12291.738","4":"Month: Jan<br>Day: Fri<br>Average births: 12,292"},{"1":"Jan","2":"Sat","3":"8332.515","4":"Month: Jan<br>Day: Sat<br>Average births: 8,333"},{"1":"Jan","2":"Sun","3":"7314.076","4":"Month: Jan<br>Day: Sun<br>Average births: 7,314"},{"1":"Feb","2":"Mon","3":"11674.583","4":"Month: Feb<br>Day: Mon<br>Average births: 11,675"},{"1":"Feb","2":"Tues","3":"12820.016","4":"Month: Feb<br>Day: Tues<br>Average births: 12,820"},{"1":"Feb","2":"Wed","3":"12568.869","4":"Month: Feb<br>Day: Wed<br>Average births: 12,569"},{"1":"Feb","2":"Thurs","3":"12646.867","4":"Month: Feb<br>Day: Thurs<br>Average births: 12,647"},{"1":"Feb","2":"Fri","3":"12472.148","4":"Month: Feb<br>Day: Fri<br>Average births: 12,472"},{"1":"Feb","2":"Sat","3":"8447.917","4":"Month: Feb<br>Day: Sat<br>Average births: 8,448"},{"1":"Feb","2":"Sun","3":"7377.180","4":"Month: Feb<br>Day: Sun<br>Average births: 7,377"},{"1":"Mar","2":"Mon","3":"11847.667","4":"Month: Mar<br>Day: Mon<br>Average births: 11,848"},{"1":"Mar","2":"Tues","3":"12833.600","4":"Month: Mar<br>Day: Tues<br>Average births: 12,834"},{"1":"Mar","2":"Wed","3":"12595.288","4":"Month: Mar<br>Day: Wed<br>Average births: 12,595"},{"1":"Mar","2":"Thurs","3":"12603.612","4":"Month: Mar<br>Day: Thurs<br>Average births: 12,604"},{"1":"Mar","2":"Fri","3":"12380.299","4":"Month: Mar<br>Day: Fri<br>Average births: 12,380"},{"1":"Mar","2":"Sat","3":"8386.603","4":"Month: Mar<br>Day: Sat<br>Average births: 8,387"},{"1":"Mar","2":"Sun","3":"7292.894","4":"Month: Mar<br>Day: Sun<br>Average births: 7,293"},{"1":"Apr","2":"Mon","3":"11753.585","4":"Month: Apr<br>Day: Mon<br>Average births: 11,754"},{"1":"Apr","2":"Tues","3":"12814.846","4":"Month: Apr<br>Day: Tues<br>Average births: 12,815"},{"1":"Apr","2":"Wed","3":"12566.531","4":"Month: Apr<br>Day: Wed<br>Average births: 12,567"},{"1":"Apr","2":"Thurs","3":"12556.254","4":"Month: Apr<br>Day: Thurs<br>Average births: 12,556"},{"1":"Apr","2":"Fri","3":"12120.719","4":"Month: Apr<br>Day: Fri<br>Average births: 12,121"},{"1":"Apr","2":"Sat","3":"8194.000","4":"Month: Apr<br>Day: Sat<br>Average births: 8,194"},{"1":"Apr","2":"Sun","3":"7205.508","4":"Month: Apr<br>Day: Sun<br>Average births: 7,206"},{"1":"May","2":"Mon","3":"11044.803","4":"Month: May<br>Day: Mon<br>Average births: 11,045"},{"1":"May","2":"Tues","3":"12957.567","4":"Month: May<br>Day: Tues<br>Average births: 12,958"},{"1":"May","2":"Wed","3":"12920.761","4":"Month: May<br>Day: Wed<br>Average births: 12,921"},{"1":"May","2":"Thurs","3":"12886.882","4":"Month: May<br>Day: Thurs<br>Average births: 12,887"},{"1":"May","2":"Fri","3":"12526.515","4":"Month: May<br>Day: Fri<br>Average births: 12,527"},{"1":"May","2":"Sat","3":"8371.439","4":"Month: May<br>Day: Sat<br>Average births: 8,371"},{"1":"May","2":"Sun","3":"7339.492","4":"Month: May<br>Day: Sun<br>Average births: 7,339"},{"1":"Jun","2":"Mon","3":"12191.312","4":"Month: Jun<br>Day: Mon<br>Average births: 12,191"},{"1":"Jun","2":"Tues","3":"13158.032","4":"Month: Jun<br>Day: Tues<br>Average births: 13,158"},{"1":"Jun","2":"Wed","3":"13053.000","4":"Month: Jun<br>Day: Wed<br>Average births: 13,053"},{"1":"Jun","2":"Thurs","3":"13112.609","4":"Month: Jun<br>Day: Thurs<br>Average births: 13,113"},{"1":"Jun","2":"Fri","3":"12763.369","4":"Month: Jun<br>Day: Fri<br>Average births: 12,763"},{"1":"Jun","2":"Sat","3":"8593.369","4":"Month: Jun<br>Day: Sat<br>Average births: 8,593"},{"1":"Jun","2":"Sun","3":"7559.585","4":"Month: Jun<br>Day: Sun<br>Average births: 7,560"},{"1":"Jul","2":"Mon","3":"12285.358","4":"Month: Jul<br>Day: Mon<br>Average births: 12,285"},{"1":"Jul","2":"Tues","3":"13562.618","4":"Month: Jul<br>Day: Tues<br>Average births: 13,563"},{"1":"Jul","2":"Wed","3":"13269.530","4":"Month: Jul<br>Day: Wed<br>Average births: 13,270"},{"1":"Jul","2":"Thurs","3":"13358.667","4":"Month: Jul<br>Day: Thurs<br>Average births: 13,359"},{"1":"Jul","2":"Fri","3":"12886.892","4":"Month: Jul<br>Day: Fri<br>Average births: 12,887"},{"1":"Jul","2":"Sat","3":"8889.061","4":"Month: Jul<br>Day: Sat<br>Average births: 8,889"},{"1":"Jul","2":"Sun","3":"7809.761","4":"Month: Jul<br>Day: Sun<br>Average births: 7,810"},{"1":"Aug","2":"Mon","3":"12551.600","4":"Month: Aug<br>Day: Mon<br>Average births: 12,552"},{"1":"Aug","2":"Tues","3":"13735.136","4":"Month: Aug<br>Day: Tues<br>Average births: 13,735"},{"1":"Aug","2":"Wed","3":"13468.940","4":"Month: Aug<br>Day: Wed<br>Average births: 13,469"},{"1":"Aug","2":"Thurs","3":"13506.194","4":"Month: Aug<br>Day: Thurs<br>Average births: 13,506"},{"1":"Aug","2":"Fri","3":"13247.588","4":"Month: Aug<br>Day: Fri<br>Average births: 13,248"},{"1":"Aug","2":"Sat","3":"8976.773","4":"Month: Aug<br>Day: Sat<br>Average births: 8,977"},{"1":"Aug","2":"Sun","3":"7835.758","4":"Month: Aug<br>Day: Sun<br>Average births: 7,836"},{"1":"Sep","2":"Mon","3":"11808.431","4":"Month: Sep<br>Day: Mon<br>Average births: 11,808"},{"1":"Sep","2":"Tues","3":"13726.406","4":"Month: Sep<br>Day: Tues<br>Average births: 13,726"},{"1":"Sep","2":"Wed","3":"13880.476","4":"Month: Sep<br>Day: Wed<br>Average births: 13,880"},{"1":"Sep","2":"Thurs","3":"13838.562","4":"Month: Sep<br>Day: Thurs<br>Average births: 13,839"},{"1":"Sep","2":"Fri","3":"13607.812","4":"Month: Sep<br>Day: Fri<br>Average births: 13,608"},{"1":"Sep","2":"Sat","3":"9216.015","4":"Month: Sep<br>Day: Sat<br>Average births: 9,216"},{"1":"Sep","2":"Sun","3":"8053.462","4":"Month: Sep<br>Day: Sun<br>Average births: 8,053"},{"1":"Oct","2":"Mon","3":"12063.119","4":"Month: Oct<br>Day: Mon<br>Average births: 12,063"},{"1":"Oct","2":"Tues","3":"13169.224","4":"Month: Oct<br>Day: Tues<br>Average births: 13,169"},{"1":"Oct","2":"Wed","3":"12886.221","4":"Month: Oct<br>Day: Wed<br>Average births: 12,886"},{"1":"Oct","2":"Thurs","3":"12927.167","4":"Month: Oct<br>Day: Thurs<br>Average births: 12,927"},{"1":"Oct","2":"Fri","3":"12582.818","4":"Month: Oct<br>Day: Fri<br>Average births: 12,583"},{"1":"Oct","2":"Sat","3":"8515.477","4":"Month: Oct<br>Day: Sat<br>Average births: 8,515"},{"1":"Oct","2":"Sun","3":"7558.636","4":"Month: Oct<br>Day: Sun<br>Average births: 7,559"},{"1":"Nov","2":"Mon","3":"12404.730","4":"Month: Nov<br>Day: Mon<br>Average births: 12,405"},{"1":"Nov","2":"Tues","3":"13390.141","4":"Month: Nov<br>Day: Tues<br>Average births: 13,390"},{"1":"Nov","2":"Wed","3":"12714.156","4":"Month: Nov<br>Day: Wed<br>Average births: 12,714"},{"1":"Nov","2":"Thurs","3":"11648.031","4":"Month: Nov<br>Day: Thurs<br>Average births: 11,648"},{"1":"Nov","2":"Fri","3":"11941.892","4":"Month: Nov<br>Day: Fri<br>Average births: 11,942"},{"1":"Nov","2":"Sat","3":"8379.692","4":"Month: Nov<br>Day: Sat<br>Average births: 8,380"},{"1":"Nov","2":"Sun","3":"7477.375","4":"Month: Nov<br>Day: Sun<br>Average births: 7,477"},{"1":"Dec","2":"Mon","3":"11846.029","4":"Month: Dec<br>Day: Mon<br>Average births: 11,846"},{"1":"Dec","2":"Tues","3":"12912.970","4":"Month: Dec<br>Day: Tues<br>Average births: 12,913"},{"1":"Dec","2":"Wed","3":"12747.167","4":"Month: Dec<br>Day: Wed<br>Average births: 12,747"},{"1":"Dec","2":"Thurs","3":"12651.092","4":"Month: Dec<br>Day: Thurs<br>Average births: 12,651"},{"1":"Dec","2":"Fri","3":"12314.955","4":"Month: Dec<br>Day: Fri<br>Average births: 12,315"},{"1":"Dec","2":"Sat","3":"8440.493","4":"Month: Dec<br>Day: Sat<br>Average births: 8,440"},{"1":"Dec","2":"Sun","3":"7383.761","4":"Month: Dec<br>Day: Sun<br>Average births: 7,384"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


``` r
births_heatmap <- ggplot(
  births_month_weekday,
  aes(x = day_of_week, y = month_name, fill = average_births, text = tooltip)
) +
  geom_tile(color = "gray15", linewidth = 0.4) +
  scale_fill_viridis_c(option = "plasma", labels = comma) +
  labs(
    title = "Births by Month and Weekday",
    subtitle = "This interactive heatmap compares two time patterns at once",
    x = "Day of the Week",
    y = "Month",
    fill = "Average Births"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(fill = "black", color = NA),

    panel.grid = element_blank(),

    plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray85", hjust = 0.5),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white", margin = margin(r = 12)),
    axis.text = element_text(color = "gray85"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "gray85"),
    legend.position = "right",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 18)
  )

births_interactive <- ggplotly(births_heatmap, tooltip = "text") %>%
  layout(
    paper_bgcolor = "black",
    plot_bgcolor = "black",
    font = list(color = "white"),
    legend = list(
      bgcolor = "black",
      font = list(color = "white")
    )
  ) %>%
  config(displayModeBar = FALSE)

births_interactive
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-88f709e12dbe4964e939" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-88f709e12dbe4964e939">{"x":{"data":[{"x":[1,2,3,4,5,6,7],"y":[1,2,3,4,5,6,7,8,9,10,11,12],"z":[[0.61680426285755452,0.77588154107794938,0.75816777868302709,0.77805271796326503,0.76198573380928281,0.16884086561857065,0.016264955452277425],[0.66952760035524994,0.84112886864941161,0.80350359130877425,0.8151887122541468,0.78901343880813712,0.18612956371252845,0.025718868277545295],[0.69545781012040819,0.84316387549043326,0.80746151655383136,0.80870857285273579,0.77525321903389832,0.17694394350967366,0.013091634381529169],[0.68136305427131849,0.84035429726411026,0.80315338704044581,0.80161371209683352,0.7363646823263601,0.14808943412445075,0],[0.57517804601606393,0.86173582294054019,0.85622179389915165,0.85114628813489679,0.79715843762670291,0.17467224031867928,0.020072696286338846],[0.74694057493459787,0.89176811176821702,0.87603294446959012,0.88496323006065414,0.83264236228029009,0.20792031285875656,0.053045482263186075],[0.76102988621464784,0.95238051782497812,0.90847209427077891,0.92182591963502136,0.85114777949041909,0.25221885529719773,0.09052529639472548],[0.80091648509789792,0.97822614041104383,0.9383463918845989,0.94392750157125971,0.90518487759818977,0.26535931000289242,0.094419903797720389],[0.68957974531056476,0.97691825204591376,1,0.9937207658002144,0.95915131426447853,0.30120107575928523,0.12703488359330992],[0.72773552594446411,0.89344484396078705,0.85104714688710448,0.85718141979679796,0.80559338834122363,0.19625099820750694,0.052903421405743632],[0.77891340878223592,0.92654114163823686,0.82526959628405594,0.66554966935679649,0.70957407764309322,0.17590863772717327,0.040729376890228576],[0.69521252732957417,0.85505452291318484,0.83021500039730733,0.81582177007708645,0.76546381522981,0.18501732934688744,0.0267047704825976]],"text":[["Month: Jan<br>Day: Mon<br>Average births: 11,323","Month: Jan<br>Day: Tues<br>Average births: 12,384","Month: Jan<br>Day: Wed<br>Average births: 12,266","Month: Jan<br>Day: Thurs<br>Average births: 12,399","Month: Jan<br>Day: Fri<br>Average births: 12,292","Month: Jan<br>Day: Sat<br>Average births: 8,333","Month: Jan<br>Day: Sun<br>Average births: 7,314"],["Month: Feb<br>Day: Mon<br>Average births: 11,675","Month: Feb<br>Day: Tues<br>Average births: 12,820","Month: Feb<br>Day: Wed<br>Average births: 12,569","Month: Feb<br>Day: Thurs<br>Average births: 12,647","Month: Feb<br>Day: Fri<br>Average births: 12,472","Month: Feb<br>Day: Sat<br>Average births: 8,448","Month: Feb<br>Day: Sun<br>Average births: 7,377"],["Month: Mar<br>Day: Mon<br>Average births: 11,848","Month: Mar<br>Day: Tues<br>Average births: 12,834","Month: Mar<br>Day: Wed<br>Average births: 12,595","Month: Mar<br>Day: Thurs<br>Average births: 12,604","Month: Mar<br>Day: Fri<br>Average births: 12,380","Month: Mar<br>Day: Sat<br>Average births: 8,387","Month: Mar<br>Day: Sun<br>Average births: 7,293"],["Month: Apr<br>Day: Mon<br>Average births: 11,754","Month: Apr<br>Day: Tues<br>Average births: 12,815","Month: Apr<br>Day: Wed<br>Average births: 12,567","Month: Apr<br>Day: Thurs<br>Average births: 12,556","Month: Apr<br>Day: Fri<br>Average births: 12,121","Month: Apr<br>Day: Sat<br>Average births: 8,194","Month: Apr<br>Day: Sun<br>Average births: 7,206"],["Month: May<br>Day: Mon<br>Average births: 11,045","Month: May<br>Day: Tues<br>Average births: 12,958","Month: May<br>Day: Wed<br>Average births: 12,921","Month: May<br>Day: Thurs<br>Average births: 12,887","Month: May<br>Day: Fri<br>Average births: 12,527","Month: May<br>Day: Sat<br>Average births: 8,371","Month: May<br>Day: Sun<br>Average births: 7,339"],["Month: Jun<br>Day: Mon<br>Average births: 12,191","Month: Jun<br>Day: Tues<br>Average births: 13,158","Month: Jun<br>Day: Wed<br>Average births: 13,053","Month: Jun<br>Day: Thurs<br>Average births: 13,113","Month: Jun<br>Day: Fri<br>Average births: 12,763","Month: Jun<br>Day: Sat<br>Average births: 8,593","Month: Jun<br>Day: Sun<br>Average births: 7,560"],["Month: Jul<br>Day: Mon<br>Average births: 12,285","Month: Jul<br>Day: Tues<br>Average births: 13,563","Month: Jul<br>Day: Wed<br>Average births: 13,270","Month: Jul<br>Day: Thurs<br>Average births: 13,359","Month: Jul<br>Day: Fri<br>Average births: 12,887","Month: Jul<br>Day: Sat<br>Average births: 8,889","Month: Jul<br>Day: Sun<br>Average births: 7,810"],["Month: Aug<br>Day: Mon<br>Average births: 12,552","Month: Aug<br>Day: Tues<br>Average births: 13,735","Month: Aug<br>Day: Wed<br>Average births: 13,469","Month: Aug<br>Day: Thurs<br>Average births: 13,506","Month: Aug<br>Day: Fri<br>Average births: 13,248","Month: Aug<br>Day: Sat<br>Average births: 8,977","Month: Aug<br>Day: Sun<br>Average births: 7,836"],["Month: Sep<br>Day: Mon<br>Average births: 11,808","Month: Sep<br>Day: Tues<br>Average births: 13,726","Month: Sep<br>Day: Wed<br>Average births: 13,880","Month: Sep<br>Day: Thurs<br>Average births: 13,839","Month: Sep<br>Day: Fri<br>Average births: 13,608","Month: Sep<br>Day: Sat<br>Average births: 9,216","Month: Sep<br>Day: Sun<br>Average births: 8,053"],["Month: Oct<br>Day: Mon<br>Average births: 12,063","Month: Oct<br>Day: Tues<br>Average births: 13,169","Month: Oct<br>Day: Wed<br>Average births: 12,886","Month: Oct<br>Day: Thurs<br>Average births: 12,927","Month: Oct<br>Day: Fri<br>Average births: 12,583","Month: Oct<br>Day: Sat<br>Average births: 8,515","Month: Oct<br>Day: Sun<br>Average births: 7,559"],["Month: Nov<br>Day: Mon<br>Average births: 12,405","Month: Nov<br>Day: Tues<br>Average births: 13,390","Month: Nov<br>Day: Wed<br>Average births: 12,714","Month: Nov<br>Day: Thurs<br>Average births: 11,648","Month: Nov<br>Day: Fri<br>Average births: 11,942","Month: Nov<br>Day: Sat<br>Average births: 8,380","Month: Nov<br>Day: Sun<br>Average births: 7,477"],["Month: Dec<br>Day: Mon<br>Average births: 11,846","Month: Dec<br>Day: Tues<br>Average births: 12,913","Month: Dec<br>Day: Wed<br>Average births: 12,747","Month: Dec<br>Day: Thurs<br>Average births: 12,651","Month: Dec<br>Day: Fri<br>Average births: 12,315","Month: Dec<br>Day: Sat<br>Average births: 8,440","Month: Dec<br>Day: Sun<br>Average births: 7,384"]],"colorscale":[[0,"#0D0887"],[0.013091634381529169,"#190889"],[0.016264955452277425,"#1B088A"],[0.020072696286338846,"#1E078A"],[0.025718868277545295,"#22078B"],[0.0267047704825976,"#22078B"],[0.040729376890228576,"#2A078E"],[0.052903421405743632,"#300690"],[0.053045482263186075,"#300690"],[0.09052529639472548,"#410596"],[0.094419903797720389,"#430596"],[0.12703488359330992,"#4F049C"],[0.14808943412445075,"#57039F"],[0.16884086561857065,"#5F02A3"],[0.17467224031867928,"#6101A4"],[0.17590863772717327,"#6101A4"],[0.17694394350967366,"#6201A4"],[0.18501732934688744,"#6501A5"],[0.18612956371252845,"#6501A6"],[0.19625099820750694,"#6900A7"],[0.20792031285875656,"#6D01A7"],[0.25221885529719773,"#7F0BA2"],[0.26535931000289242,"#840FA0"],[0.30120107575928523,"#91169C"],[0.57517804601606393,"#DC5D68"],[0.61680426285755452,"#E36A5F"],[0.66554966935679649,"#EB7A57"],[0.66952760035524994,"#EB7C56"],[0.68136305427131849,"#ED8054"],[0.68957974531056476,"#EE8252"],[0.69521252732957417,"#EF8451"],[0.69545781012040819,"#EF8451"],[0.70957407764309322,"#F1894E"],[0.72773552594446411,"#F38F4A"],[0.7363646823263601,"#F49248"],[0.74694057493459787,"#F59545"],[0.75816777868302709,"#F79942"],[0.76102988621464784,"#F79A42"],[0.76198573380928281,"#F79A41"],[0.76546381522981,"#F89B40"],[0.77525321903389832,"#F99E3E"],[0.77588154107794938,"#F99E3D"],[0.77805271796326503,"#F99F3D"],[0.77891340878223592,"#F99F3D"],[0.78901343880813712,"#FBA33A"],[0.79715843762670291,"#FCA537"],[0.80091648509789792,"#FCA636"],[0.80161371209683352,"#FCA736"],[0.80315338704044581,"#FCA736"],[0.80350359130877425,"#FCA836"],[0.80559338834122363,"#FCA836"],[0.80746151655383136,"#FCA936"],[0.80870857285273579,"#FCAA36"],[0.8151887122541468,"#FCAD35"],[0.81582177007708645,"#FCAD35"],[0.82526959628405594,"#FBB135"],[0.83021500039730733,"#FBB334"],[0.83264236228029009,"#FBB434"],[0.84035429726411026,"#FBB734"],[0.84112886864941161,"#FBB834"],[0.84316387549043326,"#FBB933"],[0.85104714688710448,"#FBBC33"],[0.85114628813489679,"#FBBC33"],[0.85114777949041909,"#FBBC33"],[0.85505452291318484,"#FABE33"],[0.85622179389915165,"#FABE32"],[0.85718141979679796,"#FABE32"],[0.86173582294054019,"#FAC032"],[0.87603294446959012,"#FAC631"],[0.88496323006065414,"#F9CA30"],[0.89176811176821702,"#F9CD30"],[0.89344484396078705,"#F9CE2F"],[0.90518487759818977,"#F8D22E"],[0.90847209427077891,"#F8D42E"],[0.92182591963502136,"#F7D92C"],[0.92654114163823686,"#F7DB2C"],[0.9383463918845989,"#F6E02B"],[0.94392750157125971,"#F5E22A"],[0.95238051782497812,"#F5E629"],[0.95915131426447853,"#F4E828"],[0.97691825204591376,"#F2F025"],[0.97822614041104383,"#F2F025"],[0.9937207658002144,"#F1F622"],[1,"#F0F921"]],"type":"heatmap","showscale":false,"autocolorscale":false,"showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1],"y":[1],"name":"a37055c47562de206d49ba90f4f5566f","type":"scatter","mode":"markers","opacity":0,"hoverinfo":"skip","showlegend":false,"marker":{"color":[0,1],"colorscale":[[0,"#0D0887"],[0.0033444816053512056,"#110888"],[0.0066889632107022751,"#140888"],[0.010033444816053481,"#170889"],[0.013377926421404687,"#190889"],[0.016722408026755894,"#1C088A"],[0.020066889632106961,"#1E078A"],[0.023411371237458168,"#20078B"],[0.026755852842809374,"#22078B"],[0.030100334448160578,"#24078C"],[0.033444816053511649,"#26078C"],[0.036789297658862852,"#28078D"],[0.040133779264214062,"#2A078E"],[0.043478260869565265,"#2C078E"],[0.046822742474916336,"#2D078F"],[0.050167224080267539,"#2F078F"],[0.053511705685618749,"#310690"],[0.056856187290969952,"#320690"],[0.060200668896321023,"#340691"],[0.063545150501672226,"#350691"],[0.066889632107023436,"#370692"],[0.070234113712374632,"#380692"],[0.073578595317725703,"#3A0693"],[0.076923076923076913,"#3B0694"],[0.080267558528428123,"#3D0694"],[0.083612040133779181,"#3E0595"],[0.086956521739130391,"#3F0595"],[0.090301003344481601,"#410596"],[0.093645484949832811,"#420596"],[0.096989966555184007,"#440597"],[0.10033444816053508,"#450597"],[0.10367892976588629,"#460598"],[0.1070234113712375,"#480499"],[0.11036789297658856,"#490499"],[0.11371237458193977,"#4A049A"],[0.11705685618729098,"#4C049A"],[0.12040133779264217,"#4D049B"],[0.12374581939799338,"#4E049B"],[0.12709030100334445,"#4F049C"],[0.13043478260869565,"#51039C"],[0.13377926421404687,"#52039D"],[0.13712374581939793,"#53039E"],[0.14046822742474913,"#54039E"],[0.14381270903010035,"#56039F"],[0.14715719063545155,"#57039F"],[0.15050167224080277,"#5803A0"],[0.15384615384615383,"#5902A0"],[0.15719063545150488,"#5B02A1"],[0.16053511705685625,"#5C02A1"],[0.1638795986622073,"#5D02A2"],[0.16722408026755836,"#5E02A3"],[0.17056856187290972,"#6002A3"],[0.17391304347826078,"#6101A4"],[0.17725752508361212,"#6201A4"],[0.1806020066889632,"#6301A5"],[0.18394648829431426,"#6401A5"],[0.18729096989966562,"#6601A6"],[0.19063545150501668,"#6701A6"],[0.19397993311036801,"#6800A7"],[0.1973244147157191,"#6900A8"],[0.20066889632107016,"#6A00A8"],[0.20401337792642149,"#6C01A8"],[0.20735785953177258,"#6D01A7"],[0.21070234113712363,"#6F02A7"],[0.214046822742475,"#7003A6"],[0.21739130434782605,"#7203A6"],[0.22073578595317711,"#7304A6"],[0.22408026755852847,"#7405A5"],[0.22742474916387953,"#7605A5"],[0.23076923076923087,"#7706A4"],[0.23411371237458195,"#7807A4"],[0.23745819397993301,"#7A08A4"],[0.24080267558528434,"#7B08A3"],[0.24414715719063543,"#7C09A3"],[0.24749163879598676,"#7E0AA2"],[0.25083612040133785,"#7F0BA2"],[0.2541806020066889,"#800CA2"],[0.25752508361204024,"#810DA1"],[0.2608695652173913,"#830DA1"],[0.26421404682274241,"#840EA0"],[0.26755852842809374,"#850FA0"],[0.2709030100334448,"#8610A0"],[0.27424749163879586,"#88119F"],[0.27759197324414719,"#89119F"],[0.28093645484949825,"#8A129E"],[0.28428093645484936,"#8B139E"],[0.2876254180602007,"#8C149E"],[0.29096989966555176,"#8E149D"],[0.29431438127090309,"#8F159D"],[0.29765886287625415,"#90169C"],[0.30100334448160526,"#91169C"],[0.3043478260869566,"#92179C"],[0.30769230769230765,"#93189B"],[0.31103678929765899,"#95199B"],[0.31438127090301005,"#96199A"],[0.3177257525083611,"#971A9A"],[0.32107023411371249,"#981B9A"],[0.32441471571906355,"#991B99"],[0.32775919732441461,"#9A1C99"],[0.33110367892976594,"#9B1D98"],[0.334448160535117,"#9C1D98"],[0.33779264214046811,"#9D1E98"],[0.34113712374581945,"#9F1F97"],[0.34448160535117051,"#A01F97"],[0.34782608695652156,"#A12096"],[0.3511705685618729,"#A22196"],[0.35451505016722401,"#A32196"],[0.35785953177257535,"#A42295"],[0.3612040133779264,"#A52395"],[0.36454849498327774,"#A62394"],[0.3678929765886288,"#A72494"],[0.37123745819397985,"#A82494"],[0.37458193979933124,"#A92593"],[0.3779264214046823,"#AA2693"],[0.38127090301003336,"#AB2692"],[0.38461538461538469,"#AC2792"],[0.38795986622073575,"#AD2891"],[0.39130434782608686,"#AE2891"],[0.3946488294314382,"#AF2991"],[0.39799331103678925,"#B02A90"],[0.40133779264214031,"#B12A90"],[0.40468227424749165,"#B22C8F"],[0.4080267558528427,"#B32D8E"],[0.41137123745819409,"#B42E8E"],[0.41471571906354515,"#B52F8D"],[0.41806020066889649,"#B6308C"],[0.42140468227424754,"#B7318B"],[0.4247491638795986,"#B8328B"],[0.42809364548494999,"#B9338A"],[0.43143812709030105,"#B93489"],[0.43478260869565211,"#BA3588"],[0.43812709030100344,"#BB3688"],[0.4414715719063545,"#BC3887"],[0.44481605351170556,"#BD3986"],[0.44816053511705695,"#BE3A86"],[0.451505016722408,"#BF3B85"],[0.45484949832775906,"#BF3C84"],[0.4581939799331104,"#C03D83"],[0.46153846153846145,"#C13E83"],[0.46488294314381257,"#C23F82"],[0.4682274247491639,"#C34081"],[0.47157190635451496,"#C44180"],[0.47491638795986629,"#C44280"],[0.47826086956521735,"#C5437F"],[0.48160535117056869,"#C6437E"],[0.4849498327759198,"#C7447D"],[0.48829431438127086,"#C8457D"],[0.49163879598662219,"#C9467C"],[0.49498327759197325,"#C9477B"],[0.4983277591973243,"#CA487A"],[0.5016722408026757,"#CB497A"],[0.50501672240802675,"#CC4A79"],[0.50836120401337781,"#CD4B78"],[0.5117056856187292,"#CD4C77"],[0.51505016722408026,"#CE4D77"],[0.51839464882943131,"#CF4E76"],[0.52173913043478259,"#D04F75"],[0.52508361204013365,"#D05074"],[0.52842809364548504,"#D15173"],[0.5317725752508361,"#D25273"],[0.53511705685618749,"#D35372"],[0.53846153846153855,"#D35371"],[0.5418060200668896,"#D45470"],[0.54515050167224088,"#D5556F"],[0.54849498327759205,"#D6566F"],[0.55183946488294311,"#D6576E"],[0.55518394648829439,"#D7586D"],[0.55852842809364545,"#D8596C"],[0.5618729096989965,"#D95A6B"],[0.56521739130434789,"#D95B6B"],[0.56856187290969895,"#DA5C6A"],[0.57190635451505001,"#DB5C69"],[0.5752508361204014,"#DC5D68"],[0.57859531772575246,"#DC5E67"],[0.58193979933110351,"#DD5F67"],[0.5852842809364549,"#DE6066"],[0.58862876254180596,"#DF6165"],[0.59197324414715724,"#DF6264"],[0.5953177257525083,"#E06363"],[0.59866220735785969,"#E16462"],[0.60200668896321075,"#E16562"],[0.6053511705685618,"#E26661"],[0.60869565217391319,"#E26761"],[0.61204013377926425,"#E36860"],[0.61538461538461531,"#E3695F"],[0.6187290969899667,"#E46B5F"],[0.62207357859531776,"#E46C5E"],[0.62541806020066881,"#E56D5E"],[0.62876254180602009,"#E56E5D"],[0.63210702341137115,"#E66F5D"],[0.63545150501672221,"#E6705C"],[0.6387959866220736,"#E7715C"],[0.64214046822742465,"#E7735B"],[0.64548494983277571,"#E8745A"],[0.6488294314381271,"#E8755A"],[0.65217391304347838,"#E97659"],[0.65551839464882922,"#E97759"],[0.65886287625418061,"#EA7858"],[0.66220735785953189,"#EA7957"],[0.66555183946488294,"#EB7A57"],[0.668896321070234,"#EB7C56"],[0.67224080267558539,"#EB7D55"],[0.67558528428093645,"#EC7E55"],[0.67892976588628751,"#EC7F54"],[0.6822742474916389,"#ED8053"],[0.68561872909698995,"#ED8153"],[0.68896321070234101,"#EE8252"],[0.6923076923076924,"#EE8351"],[0.69565217391304346,"#EF8451"],[0.69899665551839452,"#EF8650"],[0.7023411371237458,"#F0874F"],[0.70568561872909719,"#F0884F"],[0.70903010033444802,"#F0894E"],[0.7123745819397993,"#F18A4D"],[0.71571906354515069,"#F18B4D"],[0.71906354515050175,"#F28C4C"],[0.72240802675585281,"#F28D4B"],[0.72575250836120409,"#F38E4A"],[0.72909698996655525,"#F38F4A"],[0.73244147157190631,"#F49049"],[0.73578595317725759,"#F49148"],[0.73913043478260865,"#F49347"],[0.7424749163879597,"#F59446"],[0.7458193979933111,"#F59546"],[0.74916387959866215,"#F69645"],[0.75250836120401321,"#F69744"],[0.7558528428093646,"#F79843"],[0.75919732441471566,"#F79942"],[0.76254180602006671,"#F79A41"],[0.76588628762541811,"#F89B40"],[0.76923076923076938,"#F89C3F"],[0.77257525083612022,"#F99D3E"],[0.7759197324414715,"#F99E3D"],[0.77926421404682289,"#F99F3D"],[0.78260869565217395,"#FAA03C"],[0.785953177257525,"#FAA23B"],[0.78929765886287639,"#FBA339"],[0.79264214046822745,"#FBA438"],[0.79598662207357851,"#FCA537"],[0.79933110367892979,"#FCA636"],[0.80267558528428096,"#FCA736"],[0.80602006688963201,"#FCA936"],[0.80936454849498329,"#FCAA35"],[0.81270903010033435,"#FCAC35"],[0.81605351170568541,"#FCAD35"],[0.8193979933110368,"#FCAE35"],[0.82274247491638819,"#FBB035"],[0.82608695652173891,"#FBB135"],[0.8294314381270903,"#FBB334"],[0.83277591973244158,"#FBB434"],[0.83612040133779264,"#FBB634"],[0.83946488294314381,"#FBB734"],[0.84280936454849509,"#FBB833"],[0.84615384615384615,"#FBBA33"],[0.8494983277591972,"#FBBB33"],[0.85284280936454859,"#FBBD33"],[0.85618729096989965,"#FABE32"],[0.85953177257525071,"#FABF32"],[0.8628762541806021,"#FAC132"],[0.86622073578595316,"#FAC232"],[0.86956521739130421,"#FAC431"],[0.8729096989966556,"#FAC531"],[0.87625418060200666,"#FAC631"],[0.87959866220735772,"#F9C831"],[0.882943143812709,"#F9C930"],[0.88628762541806039,"#F9CB30"],[0.88963210702341111,"#F9CC30"],[0.8929765886287625,"#F9CD2F"],[0.89632107023411389,"#F8CF2F"],[0.89966555183946495,"#F8D02F"],[0.90301003344481601,"#F8D22E"],[0.90635451505016729,"#F8D32E"],[0.90969899665551845,"#F8D42E"],[0.91304347826086951,"#F7D62D"],[0.91638795986622079,"#F7D72D"],[0.91973244147157185,"#F7D82D"],[0.92307692307692291,"#F7DA2C"],[0.9264214046822743,"#F7DB2C"],[0.92976588628762535,"#F6DD2C"],[0.93311036789297641,"#F6DE2B"],[0.9364548494983278,"#F6DF2B"],[0.93979933110367908,"#F6E12A"],[0.94314381270902992,"#F5E22A"],[0.94648829431438131,"#F5E32A"],[0.94983277591973259,"#F5E529"],[0.95317725752508364,"#F4E629"],[0.9565217391304347,"#F4E728"],[0.95986622073578609,"#F4E928"],[0.96321070234113715,"#F4EA27"],[0.96655518394648821,"#F3EB27"],[0.9698996655518396,"#F3ED26"],[0.97324414715719065,"#F3EE26"],[0.97658862876254171,"#F2F025"],[0.97993311036789299,"#F2F125"],[0.98327759197324416,"#F2F224"],[0.98662207357859522,"#F1F423"],[0.98996655518394649,"#F1F523"],[0.99331103678929755,"#F1F622"],[0.99665551839464861,"#F0F822"],[1,"#F0F921"]],"colorbar":{"bgcolor":"rgba(0,0,0,1)","bordercolor":"transparent","borderwidth":1.8897637795275593,"thickness":23.039999999999996,"title":"Average Births","titlefont":{"color":"rgba(255,255,255,1)","family":"","size":14.611872146118724},"tickmode":"array","ticktext":["8,000","10,000","12,000"],"tickvals":[0.12029554112761266,0.41892366707012024,0.71755179301262784],"tickfont":{"color":"rgba(217,217,217,1)","family":"","size":11.68949771689498},"ticklen":2,"len":0.5}},"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":46.817766708177672,"r":13.283520132835205,"b":43.23785803237859,"l":59.709422997094251},"plot_bgcolor":"black","paper_bgcolor":"black","font":{"color":"white","family":"","size":14.611872146118724},"title":{"text":"<b> Births by Month and Weekday <\/b>","font":{"color":"rgba(255,255,255,1)","family":"","size":17.534246575342465},"x":0.5,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,7.5999999999999996],"tickmode":"array","ticktext":["Mon","Tues","Wed","Thurs","Fri","Sat","Sun"],"tickvals":[1,2,3,4.0000000000000009,5,6,7],"categoryorder":"array","categoryarray":["Mon","Tues","Wed","Thurs","Fri","Sat","Sun"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(217,217,217,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Day of the Week","font":{"color":"rgba(255,255,255,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,12.6],"tickmode":"array","ticktext":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"tickvals":[1,2,3,4.0000000000000009,5,6,7.0000000000000009,8,9,10,11,12],"categoryorder":"array","categoryarray":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(217,217,217,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"Month","font":{"color":"rgba(255,255,255,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[],"showlegend":false,"legend":{"bgcolor":"black","bordercolor":"transparent","borderwidth":1.8897637795275593,"font":{"color":"white","family":"","size":11.68949771689498},"title":{"text":"Average Births","font":{"color":"rgba(255,255,255,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"source":"A","attrs":{"cd81d2395f":{"x":{},"y":{},"fill":{},"text":{},"type":"heatmap"}},"cur_data":"cd81d2395f","visdat":{"cd81d2395f":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

``` r
saveWidget(
  births_interactive,
  file = "interactive_births_month_weekday.html",
  selfcontained = TRUE
)
```

This chart makes the pattern easier to explore because it does not just look at one variable at a time. The weekday chart already showed that births are higher during the week, but the heatmap shows whether that same pattern holds across different months. I also included this as the interactive plot because the assignment required at least one interactive visualization. The hover feature is useful because it lets someone check the exact average for a specific month and weekday without making the chart look cluttered.

# Visualization 3: Atlanta Weather by Month

Then I looked at how Atlanta temperatures changed across 2019.


``` r
weather_monthly <- weather_clean %>%
  group_by(month_name) %>%
  summarize(
    average_high = mean(temperatureHigh, na.rm = TRUE),
    average_low = mean(temperatureLow, na.rm = TRUE),
    rainy_days = sum(rainy_day, na.rm = TRUE),
    average_humidity = mean(humidity, na.rm = TRUE),
    .groups = "drop"
  )

weather_monthly
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["month_name"],"name":[1],"type":["fct"],"align":["left"]},{"label":["average_high"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["average_low"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["rainy_days"],"name":[4],"type":["int"],"align":["right"]},{"label":["average_humidity"],"name":[5],"type":["dbl"],"align":["right"]}],"data":[{"1":"Jan","2":"53.21516","3":"36.97419","4":"12","5":"0.6593548"},{"1":"Feb","2":"60.51500","3":"46.60679","4":"18","5":"0.7185714"},{"1":"Mar","2":"63.93516","3":"44.25323","4":"12","5":"0.5458065"},{"1":"Apr","2":"75.90400","3":"55.33000","4":"12","5":"0.6233333"},{"1":"May","2":"85.41323","3":"65.43226","4":"9","5":"0.6374194"},{"1":"Jun","2":"85.68400","3":"68.61967","4":"20","5":"0.7113333"},{"1":"Jul","2":"90.01065","3":"72.37677","4":"22","5":"0.6758065"},{"1":"Aug","2":"90.13129","3":"72.52000","4":"21","5":"0.6600000"},{"1":"Sep","2":"91.38767","3":"71.38033","4":"6","5":"0.5550000"},{"1":"Oct","2":"75.94323","3":"59.93774","4":"18","5":"0.6916129"},{"1":"Nov","2":"61.43000","3":"44.59333","4":"10","5":"0.6280000"},{"1":"Dec","2":"57.85323","3":"44.20387","4":"16","5":"0.6512903"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


``` r
hottest_month <- weather_monthly %>%
  slice_max(average_high, n = 1, with_ties = FALSE)

ggplot(weather_monthly, aes(x = month_name, group = 1)) +
  geom_line(aes(y = average_high), color = "firebrick2", linewidth = 1.1) +
  geom_point(aes(y = average_high, size = rainy_days), color = "firebrick2", alpha = 0.85) +
  geom_line(aes(y = average_low), color = "deepskyblue3", linewidth = 1.1) +
  geom_point(aes(y = average_low, size = rainy_days), color = "deepskyblue3", alpha = 0.85) +
  annotate(
    "label",
    x = hottest_month$month_name,
    y = hottest_month$average_high + 5,
    label = paste("Warmest month:", hottest_month$month_name),
    fill = "black",
    color = "white",
    fontface = "bold",
    label.size = 0.25,
    size = 3.7
  ) +
  scale_size_continuous(range = c(2.5, 7.5)) +
  scale_y_continuous(
    expand = expansion(mult = c(0.05, 0.18))
  ) +
  labs(
    title = "Atlanta Weather by Month",
    subtitle = "Average high and low temperatures follow a seasonal pattern",
    x = "Month",
    y = "Average Temperature (F)",
    size = "Rainy Days"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(fill = "black", color = NA),

    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray25"),

    plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray85", hjust = 0.5),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white", margin = margin(r = 14)),
    axis.text = element_text(color = "gray85"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "gray85"),
    legend.position = "bottom",
    plot.margin = margin(t = 10, r = 10, b = 10, l = 18)
  )
```

```
## Warning in annotate("label", x = hottest_month$month_name, y =
## hottest_month$average_high + : Ignoring unknown parameters: `label.size`
```

<img src="depalma_project_02_revision_files/figure-html/atlanta-weather-monthly-1.png" alt="Line chart showing Atlanta average high and low temperatures by month, with point size representing rainy days and the warmest month labeled." width="100%" />

This chart shows the seasonal pattern in Atlanta pretty clearly. The high and low temperatures both climb into the summer months and then start dropping again later in the year, which is about what I expected. I also used the point size to show rainy days because weather is not just about temperature. This gives the chart a little more context and lets the viewer see temperature and rain patterns in the same visual.

# Visualization 4: Spatial Visualization of Florida Lakes

I used the `Florida_Lakes.zip` shapefile dataset for the spatial visualization. This fits the assignment requirement because the map is based on shapefile data stored in the `data` folder.


``` r
if (!dir.exists("../data/Florida_Lakes")) {
  unzip("../data/Florida_Lakes.zip", exdir = "../data/Florida_Lakes")
}

lake_shp <- list.files(
  "../data/Florida_Lakes",
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)[1]

florida_lakes <- st_read(lake_shp, quiet = TRUE)

glimpse(florida_lakes)
```

```
## Rows: 4,243
## Columns: 7
## $ PERIMETER <dbl> 11082.2515, 2834.0741, 18768.2731, 493.2790, 5662.6876, 316.…
## $ NAME      <chr> "Lake Maitland", "Black Lake", "Lake Jackson", "Halfmoon Lak…
## $ COUNTY    <chr> "ORANGE", "ESCAMBIA", "HIGHLANDS", "ESCAMBIA", "ESCAMBIA", "…
## $ OBJECTID  <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 1…
## $ SHAPEAREA <dbl> 1818000.097, 31379.777, 13601177.118, 6337.482, 338242.234, …
## $ SHAPELEN  <dbl> 11082.2509, 2834.0741, 18768.2738, 493.2789, 5662.6887, 316.…
## $ geometry  <MULTIPOLYGON [°]> MULTIPOLYGON (((-81.34813 2..., MULTIPOLYGON ((…
```


``` r
florida_lakes_clean <- florida_lakes %>%
  st_transform(3086) %>%
  mutate(
    area_acres = as.numeric(st_area(geometry)) * 0.000247105
  )
```


``` r
florida_outline <- map_data("state", region = "florida")

florida_lakes_plot <- florida_lakes_clean %>%
  st_transform(4326)

ggplot() +
  geom_polygon(
    data = florida_outline,
    aes(x = long, y = lat, group = group),
    fill = "gray12",
    color = "gray85",
    linewidth = 0.5
  ) +
  geom_sf(
    data = florida_lakes_plot,
    aes(fill = area_acres),
    color = NA,
    alpha = 0.95,
    inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    labels = label_number(scale = 1 / 1000, suffix = "k"),
    trans = "sqrt"
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(5.5, "cm"),
      barheight = unit(0.35, "cm")
    )
  ) +
  coord_sf(
    xlim = c(-88.2, -79.5),
    ylim = c(24.0, 31.4),
    expand = FALSE,
    datum = NA
  ) +
  labs(
    title = "Florida Lakes by Estimated Area",
    subtitle = "Lake polygons shown inside the outline of Florida",
    fill = "Area Acres",
    caption = "Data: Florida_Lakes shapefile"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(fill = "black", color = NA),

    plot.title = element_text(color = "white", face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(color = "gray85", hjust = 0.5, size = 11),
    plot.caption = element_text(color = "gray70", hjust = 0.5, size = 9),

    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "gray85"),
    legend.position = "bottom",

    plot.margin = margin(t = 12, r = 24, b = 8, l = 24)
  )
```

<img src="depalma_project_02_revision_files/figure-html/florida-lakes-map-1.png" alt="Map of Florida showing lake polygons colored by estimated area, with the Florida outline used for geographic context." width="100%" style="display: block; margin: auto;" />

This map works better with the Florida outline included because the lake polygons by themselves did not clearly show the shape of the state. The outline gives the viewer geographic context, while the lake colors show estimated area. I used a sequential color scale because lake area is numeric, so smaller lakes appear darker and larger lakes appear brighter. This makes the map show both location and size instead of only placing shapes on a map.

# Visualization 5: Weather Model Coefficients

Finally, I made a model visualization using the Atlanta weather data. The model looks at how humidity, cloud cover, wind speed, and precipitation probability relate to daily high temperature.


``` r
weather_model_data <- weather_clean %>%
  select(
    temperatureHigh,
    humidity,
    cloudCover,
    windSpeed,
    precipProbability
  ) %>%
  drop_na()

weather_model <- lm(
  temperatureHigh ~ humidity + cloudCover + windSpeed + precipProbability,
  data = weather_model_data
)

summary(weather_model)
```

```
## 
## Call:
## lm(formula = temperatureHigh ~ humidity + cloudCover + windSpeed + 
##     precipProbability, data = weather_model_data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.727  -8.837   3.229   9.455  22.877 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        72.8165     4.7880  15.208  < 2e-16 ***
## humidity           37.6220     7.8279   4.806 2.26e-06 ***
## cloudCover        -35.7368     3.5171 -10.161  < 2e-16 ***
## windSpeed          -2.6245     0.4539  -5.782 1.60e-08 ***
## precipProbability   7.2977     2.9606   2.465   0.0142 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 12.6 on 360 degrees of freedom
## Multiple R-squared:  0.3353,	Adjusted R-squared:  0.3279 
## F-statistic:  45.4 on 4 and 360 DF,  p-value: < 2.2e-16
```


``` r
model_coefficients <- tidy(weather_model, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      humidity = "Humidity",
      cloudCover = "Cloud Cover",
      windSpeed = "Wind Speed",
      precipProbability = "Precipitation Probability"
    ),
    term = fct_reorder(term, estimate)
  )

model_coefficients
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["fct"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["conf.low"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["conf.high"],"name":[7],"type":["dbl"],"align":["right"]}],"data":[{"1":"Humidity","2":"37.622012","3":"7.8278539","4":"4.806172","5":"2.263219e-06","6":"22.227947","7":"53.016077"},{"1":"Cloud Cover","2":"-35.736792","3":"3.5171389","4":"-10.160756","5":"1.716222e-21","6":"-42.653511","7":"-28.820073"},{"1":"Wind Speed","2":"-2.624490","3":"0.4539273","4":"-5.781740","5":"1.604648e-08","6":"-3.517172","7":"-1.731808"},{"1":"Precipitation Probability","2":"7.297656","3":"2.9606189","4":"2.464909","5":"1.417092e-02","6":"1.475376","7":"13.119937"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

The first version below is a raw coefficient plot. It is useful, but it has one major weakness: the predictors are measured on different scales. For example, wind speed is measured differently from humidity or precipitation probability, so the raw estimates are not always the fairest direct comparison.


``` r
model_coefficients <- model_coefficients %>%
  mutate(
    direction = if_else(estimate >= 0, "Positive", "Negative")
  )

ggplot(model_coefficients, aes(x = estimate, y = term, color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.6) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high),
    linewidth = 0.8,
    size = 0.7
  ) +
  scale_color_manual(
    values = c("Positive" = "darkorange", "Negative" = "deepskyblue3")
  ) +
  scale_x_continuous(
    limits = c(-45, 58),
    breaks = c(-25, 0, 25, 50),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title = "Weather Model Coefficients",
    subtitle = "Positive and negative estimates show how variables relate to daily high temperature",
    x = "Estimated Change in Daily High Temperature",
    y = "",
    color = "Relationship"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(fill = "black", color = NA),

    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray25"),
    panel.grid.major.y = element_line(color = "gray18"),

    plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray85", hjust = 0.5),
    axis.title.x = element_text(color = "white", margin = margin(t = 10)),
    axis.text = element_text(color = "gray85"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "gray85"),
    legend.position = "bottom",

    plot.margin = margin(t = 12, r = 4, b = 12, l = 18)
  )
```

<img src="depalma_project_02_revision_files/figure-html/model-coefficient-plot-before-1.png" alt="Raw coefficient plot from an Atlanta weather model showing estimated relationships between weather variables and daily high temperature." width="100%" style="display: block; margin: auto;" />

This chart adds more analysis because it turns the model output into something easier to compare visually. Instead of only reading the regression summary, I can see which variables are estimated to have positive or negative relationships with daily high temperature. It is not meant to perfectly predict Atlanta weather, but it helps show which variables appear more connected to temperature in this dataset.

# Redesign: Weather Model Coefficients

For the required chart redesign, I focused on the weather model coefficient plot instead of adding a random new chart. The original coefficient plot was not terrible, but it had a real issue: it compared raw coefficients from predictors that are not measured on the same scale. A one-unit change in wind speed is not the same kind of change as a one-unit change in humidity or precipitation probability.

To fix that, I rebuilt the model using standardized variables. This makes the coefficients easier to compare because each predictor is measured in standard deviation units. The redesigned chart still keeps my same dark style and positive/negative color idea, but the comparison is more honest.


``` r
weather_model_scaled_data <- weather_clean %>%
  select(
    temperatureHigh,
    humidity,
    cloudCover,
    windSpeed,
    precipProbability
  ) %>%
  drop_na() %>%
  mutate(
    across(
      c(temperatureHigh, humidity, cloudCover, windSpeed, precipProbability),
      ~ as.numeric(scale(.x))
    )
  )

weather_model_scaled <- lm(
  temperatureHigh ~ humidity + cloudCover + windSpeed + precipProbability,
  data = weather_model_scaled_data
)

model_coefficients_scaled <- tidy(weather_model_scaled, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(
      term,
      humidity = "Humidity",
      cloudCover = "Cloud Cover",
      windSpeed = "Wind Speed",
      precipProbability = "Precipitation Probability"
    ),
    direction = if_else(estimate >= 0, "Positive", "Negative"),
    label = round(estimate, 2),
    term = fct_reorder(term, abs(estimate))
  )

model_coefficients_scaled
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["term"],"name":[1],"type":["fct"],"align":["left"]},{"label":["estimate"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["std.error"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["statistic"],"name":[4],"type":["dbl"],"align":["right"]},{"label":["p.value"],"name":[5],"type":["dbl"],"align":["right"]},{"label":["conf.low"],"name":[6],"type":["dbl"],"align":["right"]},{"label":["conf.high"],"name":[7],"type":["dbl"],"align":["right"]},{"label":["direction"],"name":[8],"type":["chr"],"align":["left"]},{"label":["label"],"name":[9],"type":["dbl"],"align":["right"]}],"data":[{"1":"Humidity","2":"0.3383477","3":"0.07039858","4":"4.806172","5":"2.263219e-06","6":"0.19990356","7":"0.4767918","8":"Positive","9":"0.34"},{"1":"Cloud Cover","2":"-0.6892309","3":"0.06783263","4":"-10.160756","5":"1.716222e-21","6":"-0.82262884","7":"-0.5558329","8":"Negative","9":"-0.69"},{"1":"Wind Speed","2":"-0.2642408","3":"0.04570264","4":"-5.781740","5":"1.604648e-08","6":"-0.35411848","7":"-0.1743631","8":"Negative","9":"-0.26"},{"1":"Precipitation Probability","2":"0.1694369","3":"0.06873962","4":"2.464909","5":"1.417092e-02","6":"0.03425526","7":"0.3046186","8":"Positive","9":"0.17"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>


``` r
ggplot(model_coefficients_scaled, aes(x = estimate, y = term, color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray70", linewidth = 0.6) +
  geom_segment(
    aes(x = conf.low, xend = conf.high, y = term, yend = term),
    linewidth = 1
  ) +
  geom_point(
    aes(shape = direction, fill = direction),
    size = 3.3,
    stroke = 0
  ) +
  geom_text(
    aes(label = label),
    color = "white",
    nudge_y = 0.22,
    size = 3.5,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c("Positive" = "darkorange", "Negative" = "deepskyblue3")
  ) +
  scale_fill_manual(
    values = c("Positive" = "darkorange", "Negative" = "deepskyblue3")
  ) +
  scale_shape_manual(
    values = c("Positive" = 17, "Negative" = 25)
  ) +
  labs(
    title = "Redesigned Weather Model Coefficients",
    subtitle = "Standardized coefficients make the weather variables easier to compare fairly",
    x = "Standardized Estimated Relationship with Daily High Temperature",
    y = "",
    color = "Relationship",
    shape = "Relationship",
    fill = "Relationship"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(fill = "black", color = NA),

    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray25"),
    panel.grid.major.y = element_line(color = "gray18"),

    plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
    plot.subtitle = element_text(color = "gray85", hjust = 0.5),
    axis.title.x = element_text(color = "white", margin = margin(t = 10)),
    axis.text = element_text(color = "gray85"),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "gray85"),
    legend.position = "bottom",

    plot.margin = margin(t = 12, r = 4, b = 12, l = 18)
  )
```

<img src="depalma_project_02_revision_files/figure-html/weather-model-coefficient-redesign-1.png" alt="Redesigned standardized coefficient plot showing comparable weather model effect sizes with confidence intervals and a zero reference line." width="100%" style="display: block; margin: auto;" />

The redesigned version is better because it fixes the way the coefficients are being compared, not just how the chart looks. The original raw coefficient plot showed the model results, but the variables were on different scales, so the coefficients were not as easy to compare directly. By using standardized coefficients, the redesigned chart makes it easier to see which weather variables have stronger relationships with daily high temperature. The zero line still makes it clear whether each relationship is positive or negative, and the confidence intervals show how certain or uncertain each estimate is.

# Findings

The biggest pattern in the births data is that births are not spread evenly across the week. Weekdays have higher average births than weekends, which was one of the clearest findings in the project. This was interesting because it shows the data probably reflects more than just when babies naturally arrive. It may also connect to hospital scheduling, planned deliveries, and other human decisions.

The interactive heatmap made that pattern easier to check across the year. Instead of only saying weekdays are higher overall, it shows that the weekday/weekend difference appears across different months too.

The Atlanta weather data showed a more expected pattern. High and low temperatures rise into the summer and then drop later in the year. I also included rainy days in the chart so it was not only showing temperature.

The Florida lakes map was mostly used for the spatial part of the assignment. At first, the lake polygons alone did not clearly look like Florida, so I added the state outline behind them. That made the map easier to understand and gave the lakes actual geographic context.

The model chart added another layer because it compared multiple weather variables at once. It is not meant to perfectly predict weather, but it helped show which variables had positive or negative relationships with daily high temperature. The redesigned standardized coefficient chart made that comparison stronger because the predictors were placed on the same scale before comparing them.

# Design Choices

For this project, I tried to keep the same general layout as Mini-Project 1, but improve the parts I lost points on. In the feedback from Mini-Project 1, one issue was that the charts were clear but a little too simple. So for this project, I still kept the visuals readable, but added more layered charts like the heatmap, the weather chart with rainy days, the spatial map, and the model coefficient plot.

I also changed how I handled weekdays. In Mini-Project 1, the weekday chart would have worked better as a bar chart because weekdays are categories. I used that feedback here and made the births-by-weekday chart a bar chart instead of a line chart.

I added annotations and labels where they helped, like marking the highest average birth day and the warmest month in the weather chart. I also tried to make the colors serve a purpose. For example, weekdays and weekends use different colors, high and low temperatures use warm/cool colors, and the model chart separates positive and negative estimates.

I used a black background on several charts because I liked the look, but I tried to keep the colors meaningful instead of just making everything flashy. For the coefficient redesign, I kept the same orange and blue colors, but improved the actual comparison by using standardized coefficients and adding a zero reference line, confidence intervals, labels, and shape differences. The goal was still readability first.

# Difficulties and Additional Approaches

One challenge was making the project feel connected because I used more than one dataset. The births data worked best for weekday and monthly patterns, the weather data worked better for trends and modeling, and the Florida Lakes shapefile was mainly there for the spatial requirement. I explained the shapefile section separately instead of forcing it to connect to births or weather.

Another challenge was the spatial map. The first version only showed lake polygons, so it was hard to tell that the map was Florida. Adding the state outline fixed that and made the visualization much clearer.

The black background also took some adjusting. Some labels, legends, and annotations were hard to read at first, so I had to adjust spacing, text color, and legend placement to make the charts look cleaner.

If I continued this project, I could make it stronger by using birth data with state or region information. That would connect the births analysis to a spatial map better. For the weather data, I could compare Atlanta to other cities or use multiple years to see if 2019 was normal or unusual.

# Accessibility and Interactivity Notes

For accessibility, I added alt text to the main figure chunks and avoided relying only on color when possible. The weather coefficient redesign uses both color and shape for positive and negative relationships, and the model estimates are also separated by position around the zero line. The interactive birth heatmap lets the reader hover over each month and weekday combination to see the exact average birth value without crowding the chart with labels.

# Conclusion

This project helped me build on Mini-Project 1 instead of just repeating the same type of charts. The births data showed that births are higher on weekdays than weekends, which probably connects to hospital scheduling and human systems. The Atlanta weather data showed a clear seasonal pattern, and the model chart gave another way to compare weather variables.

The biggest change from my first project was trying to go beyond simple counts. I added annotations, an interactive plot, a shapefile map, and a model visualization. Overall, I tried to make the project tell a stronger data story while still keeping the report organized and easy to follow.

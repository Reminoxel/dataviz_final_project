---
title: "Data Visualization Mini-Project 1" 
author: "Joseph DePalma - JDePalma1039@floridapoly.edu" 
output:
  html_document:
    keep_md: true
---


``` r
library(tidyverse)
library(plotly)
library(viridis)
library(scales)

knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  fig.width = 8,
  fig.height = 5
)
```

# Introduction

For this mini-project I decided to use the `rats_nyc.csv` dataset, which is about rat sightings in New York City from 2015 to 2017. I picked this dataset mostly because it is about rats, and rats are stupid. Not in a useless way though, because the dataset is actually pretty good for visualization. There are categories like borough, weekday, year, and location type, so there are a lot of ways to look at where and when people reported seeing rats.

The main thing I wanted to look at was where rat sightings happen the most and whether there are patterns by borough, weekday, or year. This dataset is not just about rats existing. It is also about how often people report them, so there is probably some human behavior mixed in too.

The original charts I planned to make were:

1.  A bar chart of rat sightings by borough
2.  A weekday chart showing rat sightings across the week
3.  A line chart of rat sightings by year and borough

I also ended up making an extra chart with location type because I thought it helped explain where sightings were being reported from. For the final project revision, I also added a small interactive version of one chart, alt text for accessibility, and a before/after redesign of the weekday chart.

# Load and Explore the Data

Read the dataset from the `data` folder.


``` r
rats <- read_csv("../data/rats_nyc.csv", col_types = cols())
```

Use `glimpse()` to look at the dataset structure.


``` r
glimpse(rats)
```

```
## Rows: 41,845
## Columns: 56
## $ unique_key                     <dbl> 31464015, 31464024, 31464025, 31464026,…
## $ created_date                   <dttm> 2015-09-04, 2015-09-04, 2015-09-04, 20…
## $ closed_date                    <chr> "09/18/2015 12:00:00 AM", "10/28/2015 1…
## $ agency                         <chr> "DOHMH", "DOHMH", "DOHMH", "DOHMH", "DO…
## $ agency_name                    <chr> "Department of Health and Mental Hygien…
## $ complaint_type                 <chr> "Rodent", "Rodent", "Rodent", "Rodent",…
## $ descriptor                     <chr> "Rat Sighting", "Rat Sighting", "Rat Si…
## $ location_type                  <chr> "3+ Family Mixed Use Building", "Commer…
## $ incident_zip                   <dbl> 10006, 10306, 10310, 11206, 10462, 1123…
## $ incident_address               <chr> NA, "2270 HYLAN BOULEVARD", "758 POST A…
## $ street_name                    <chr> NA, "HYLAN BOULEVARD", "POST AVENUE", "…
## $ cross_street_1                 <chr> NA, NA, "CARY AVENUE", "HUMBOLDT STREET…
## $ cross_street_2                 <chr> NA, NA, "GREENLEAF AVENUE", "BUSHWICK A…
## $ intersection_street_1          <chr> "TRINITY PLACE", NA, NA, NA, NA, NA, NA…
## $ intersection_street_2          <chr> "RECTOR STREET", NA, NA, NA, NA, NA, NA…
## $ address_type                   <chr> "INTERSECTION", "LATLONG", "ADDRESS", "…
## $ city                           <chr> "NEW YORK", "STATEN ISLAND", "STATEN IS…
## $ landmark                       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ facility_type                  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ status                         <chr> "Closed", "Closed", "Assigned", "Closed…
## $ due_date                       <chr> "10/04/2015 03:01:02 PM", "10/04/2015 1…
## $ resolution_action_updated_date <chr> "09/18/2015 12:00:00 AM", "10/28/2015 1…
## $ community_board                <chr> "01 MANHATTAN", "Unspecified STATEN ISL…
## $ borough                        <chr> "MANHATTAN", "STATEN ISLAND", "STATEN I…
## $ x_coordinate_state_plane       <dbl> 980656, 955207, 949033, 1000550, 102164…
## $ y_coordinate_state_plane       <dbl> 197137, 148858, 169278, 197585, 250489,…
## $ park_facility_name             <chr> "Unspecified", "Unspecified", "Unspecif…
## $ park_borough                   <chr> "MANHATTAN", "STATEN ISLAND", "STATEN I…
## $ school_name                    <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_number                  <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_region                  <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_code                    <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_phone_number            <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_address                 <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_city                    <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_state                   <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_zip                     <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_not_found               <chr> "N", "N", "N", "N", "N", "N", "N", "N",…
## $ school_or_citywide_complaint   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ vehicle_type                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ taxi_company_borough           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ taxi_pick_up_location          <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ bridge_highway_name            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ bridge_highway_direction       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ road_ramp                      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ bridge_highway_segment         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ garage_lot_name                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ ferry_direction                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ ferry_terminal_name            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ latitude                       <dbl> 40.70777, 40.57521, 40.63124, 40.70899,…
## $ longitude                      <dbl> -74.01296, -74.10455, -74.12688, -73.94…
## $ location                       <chr> "(40.70777155363643, -74.01296309970473…
## $ sighting_year                  <dbl> 2015, 2015, 2015, 2015, 2015, 2015, 201…
## $ sighting_month                 <dbl> 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, …
## $ sighting_day                   <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, …
## $ sighting_weekday               <chr> "Friday", "Friday", "Friday", "Friday",…
```

Check the dimensions of the dataset.


``` r
dim(rats)
```

```
## [1] 41845    56
```

Look at the column names.


``` r
names(rats)
```

```
##  [1] "unique_key"                     "created_date"                  
##  [3] "closed_date"                    "agency"                        
##  [5] "agency_name"                    "complaint_type"                
##  [7] "descriptor"                     "location_type"                 
##  [9] "incident_zip"                   "incident_address"              
## [11] "street_name"                    "cross_street_1"                
## [13] "cross_street_2"                 "intersection_street_1"         
## [15] "intersection_street_2"          "address_type"                  
## [17] "city"                           "landmark"                      
## [19] "facility_type"                  "status"                        
## [21] "due_date"                       "resolution_action_updated_date"
## [23] "community_board"                "borough"                       
## [25] "x_coordinate_state_plane"       "y_coordinate_state_plane"      
## [27] "park_facility_name"             "park_borough"                  
## [29] "school_name"                    "school_number"                 
## [31] "school_region"                  "school_code"                   
## [33] "school_phone_number"            "school_address"                
## [35] "school_city"                    "school_state"                  
## [37] "school_zip"                     "school_not_found"              
## [39] "school_or_citywide_complaint"   "vehicle_type"                  
## [41] "taxi_company_borough"           "taxi_pick_up_location"         
## [43] "bridge_highway_name"            "bridge_highway_direction"      
## [45] "road_ramp"                      "bridge_highway_segment"        
## [47] "garage_lot_name"                "ferry_direction"               
## [49] "ferry_terminal_name"            "latitude"                      
## [51] "longitude"                      "location"                      
## [53] "sighting_year"                  "sighting_month"                
## [55] "sighting_day"                   "sighting_weekday"
```

Look at a few of the variables that will be used for the plots.


``` r
rats %>%
  select(borough, sighting_year, sighting_month, sighting_day, sighting_weekday, location_type) %>%
  head()
```

```
## # A tibble: 6 × 6
##   borough       sighting_year sighting_month sighting_day sighting_weekday
##   <chr>                 <dbl>          <dbl>        <dbl> <chr>           
## 1 MANHATTAN              2015              9            4 Friday          
## 2 STATEN ISLAND          2015              9            4 Friday          
## 3 STATEN ISLAND          2015              9            4 Friday          
## 4 BROOKLYN               2015              9            4 Friday          
## 5 BRONX                  2015              9            4 Friday          
## 6 BROOKLYN               2015              9            4 Friday          
## # ℹ 1 more variable: location_type <chr>
```

# Clean the Data

The dataset was already pretty usable, but I cleaned it a little so the charts looked nicer. I changed the borough names from all caps to title case and put weekdays in the correct order.


``` r
rats_clean <- rats %>%
  mutate(
    borough = str_to_title(borough),
    sighting_weekday = factor(
      sighting_weekday,
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    ),
    sighting_month = factor(
      sighting_month,
      levels = 1:12,
      labels = month.name
    )
  )
```

Check the cleaned dataset.


``` r
glimpse(rats_clean)
```

```
## Rows: 41,845
## Columns: 56
## $ unique_key                     <dbl> 31464015, 31464024, 31464025, 31464026,…
## $ created_date                   <dttm> 2015-09-04, 2015-09-04, 2015-09-04, 20…
## $ closed_date                    <chr> "09/18/2015 12:00:00 AM", "10/28/2015 1…
## $ agency                         <chr> "DOHMH", "DOHMH", "DOHMH", "DOHMH", "DO…
## $ agency_name                    <chr> "Department of Health and Mental Hygien…
## $ complaint_type                 <chr> "Rodent", "Rodent", "Rodent", "Rodent",…
## $ descriptor                     <chr> "Rat Sighting", "Rat Sighting", "Rat Si…
## $ location_type                  <chr> "3+ Family Mixed Use Building", "Commer…
## $ incident_zip                   <dbl> 10006, 10306, 10310, 11206, 10462, 1123…
## $ incident_address               <chr> NA, "2270 HYLAN BOULEVARD", "758 POST A…
## $ street_name                    <chr> NA, "HYLAN BOULEVARD", "POST AVENUE", "…
## $ cross_street_1                 <chr> NA, NA, "CARY AVENUE", "HUMBOLDT STREET…
## $ cross_street_2                 <chr> NA, NA, "GREENLEAF AVENUE", "BUSHWICK A…
## $ intersection_street_1          <chr> "TRINITY PLACE", NA, NA, NA, NA, NA, NA…
## $ intersection_street_2          <chr> "RECTOR STREET", NA, NA, NA, NA, NA, NA…
## $ address_type                   <chr> "INTERSECTION", "LATLONG", "ADDRESS", "…
## $ city                           <chr> "NEW YORK", "STATEN ISLAND", "STATEN IS…
## $ landmark                       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ facility_type                  <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ status                         <chr> "Closed", "Closed", "Assigned", "Closed…
## $ due_date                       <chr> "10/04/2015 03:01:02 PM", "10/04/2015 1…
## $ resolution_action_updated_date <chr> "09/18/2015 12:00:00 AM", "10/28/2015 1…
## $ community_board                <chr> "01 MANHATTAN", "Unspecified STATEN ISL…
## $ borough                        <chr> "Manhattan", "Staten Island", "Staten I…
## $ x_coordinate_state_plane       <dbl> 980656, 955207, 949033, 1000550, 102164…
## $ y_coordinate_state_plane       <dbl> 197137, 148858, 169278, 197585, 250489,…
## $ park_facility_name             <chr> "Unspecified", "Unspecified", "Unspecif…
## $ park_borough                   <chr> "MANHATTAN", "STATEN ISLAND", "STATEN I…
## $ school_name                    <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_number                  <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_region                  <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_code                    <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_phone_number            <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_address                 <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_city                    <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_state                   <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_zip                     <chr> "Unspecified", "Unspecified", "Unspecif…
## $ school_not_found               <chr> "N", "N", "N", "N", "N", "N", "N", "N",…
## $ school_or_citywide_complaint   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ vehicle_type                   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ taxi_company_borough           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ taxi_pick_up_location          <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ bridge_highway_name            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ bridge_highway_direction       <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ road_ramp                      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ bridge_highway_segment         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ garage_lot_name                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ ferry_direction                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ ferry_terminal_name            <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
## $ latitude                       <dbl> 40.70777, 40.57521, 40.63124, 40.70899,…
## $ longitude                      <dbl> -74.01296, -74.10455, -74.12688, -73.94…
## $ location                       <chr> "(40.70777155363643, -74.01296309970473…
## $ sighting_year                  <dbl> 2015, 2015, 2015, 2015, 2015, 2015, 201…
## $ sighting_month                 <fct> September, September, September, Septem…
## $ sighting_day                   <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, …
## $ sighting_weekday               <fct> Friday, Friday, Friday, Friday, Friday,…
```

# Missing Values

I checked missing values because missing data can mess up graphs or make the results weird if there is a lot of it.


``` r
rats_clean %>%
  summarize(
    missing_borough = sum(is.na(borough)),
    missing_year = sum(is.na(sighting_year)),
    missing_month = sum(is.na(sighting_month)),
    missing_day = sum(is.na(sighting_day)),
    missing_weekday = sum(is.na(sighting_weekday)),
    missing_location = sum(is.na(location_type))
  )
```

```
## # A tibble: 1 × 6
##   missing_borough missing_year missing_month missing_day missing_weekday
##             <int>        <int>         <int>       <int>           <int>
## 1               0            0             0           0               0
## # ℹ 1 more variable: missing_location <int>
```

# Basic Summary

Here is a basic summary of the number of rat sightings and the years included.


``` r
rats_clean %>%
  summarize(
    total_rat_sightings = n(),
    first_year = min(sighting_year, na.rm = TRUE),
    last_year = max(sighting_year, na.rm = TRUE),
    number_of_boroughs = n_distinct(borough),
    number_of_location_types = n_distinct(location_type, na.rm = TRUE)
  )
```

```
## # A tibble: 1 × 5
##   total_rat_sightings first_year last_year number_of_boroughs
##                 <int>      <dbl>     <dbl>              <int>
## 1               41845       2015      2017                  5
## # ℹ 1 more variable: number_of_location_types <int>
```

This gives the report a quick starting point before the charts. The dataset covers multiple boroughs, multiple location types, and several years of reports, so it is useful for looking at location and time patterns instead of only one overall count.

# Visualization 1: Rat Sightings by Borough

The first chart shows the number of rat sightings by borough.


``` r
rats_borough <- rats_clean %>%
  count(borough, sort = TRUE)

rats_borough
```

```
## # A tibble: 5 × 2
##   borough           n
##   <chr>         <int>
## 1 Brooklyn      15008
## 2 Manhattan     10650
## 3 Bronx          8413
## 4 Queens         6016
## 5 Staten Island  1758
```


``` r
top_borough <- rats_borough %>% slice_max(n, n = 1)

ggplot(rats_borough, aes(x = fct_reorder(borough, n), y = n, fill = borough)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(n)), hjust = -0.1, size = 3.5) +
  annotate(
    "text",
    x = top_borough$borough,
    y = top_borough$n,
    label = paste("Most reports:", top_borough$borough),
    hjust = 1.1,
    vjust = -0.5,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Rat Sightings by NYC Borough",
    subtitle = "Some boroughs reported a lot more rat sightings than others",
    x = "",
    y = "Number of Rat Sightings"
  ) +
  theme_minimal()
```

<img src="depalma_project_01_revision_files/figure-html/borough-chart-1.png" alt="Horizontal bar chart showing rat sightings by New York City borough. The bars are ordered from the borough with the most reports to the borough with the fewest reports."  />

This chart is probably the easiest one to understand. It compares the boroughs directly, and ordering the bars makes it easier to see which boroughs had the most reports. I used a horizontal bar chart because the borough names are categories and this layout is easier to read. I also added the count labels so the main comparison is easier to read without needing to guess from the axis.

## Interactive Version

I added a small interactive version of the borough chart for the final project requirement. It keeps the same basic idea as the original chart, but the reader can hover over each bar to see the exact number of reports.


``` r
borough_plot <- ggplot(
  rats_borough,
  aes(
    x = fct_reorder(borough, n),
    y = n,
    fill = borough,
    text = paste0("Borough: ", borough, "<br>Reports: ", comma(n))
  )
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Interactive Rat Sightings by NYC Borough",
    x = "",
    y = "Number of Rat Sightings"
  ) +
  theme_minimal()

ggplotly(borough_plot, tooltip = "text") %>%
  config(displayModeBar = FALSE)
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-4401b2488ff66248ac00" style="width:768px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-4401b2488ff66248ac00">{"x":{"data":[{"orientation":"h","width":0.90000000000000036,"base":0,"x":[8413],"y":[3],"text":"Borough: Bronx<br>Reports: 8,413","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(13,8,135,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"Bronx","legendgroup":"Bronx","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":0.90000000000000036,"base":0,"x":[15008],"y":[5],"text":"Borough: Brooklyn<br>Reports: 15,008","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(126,3,168,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"Brooklyn","legendgroup":"Brooklyn","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":0.90000000000000036,"base":0,"x":[10650],"y":[4],"text":"Borough: Manhattan<br>Reports: 10,650","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(204,70,120,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"Manhattan","legendgroup":"Manhattan","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":0.90000000000000013,"base":0,"x":[6016],"y":[2],"text":"Borough: Queens<br>Reports: 6,016","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(248,148,65,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"Queens","legendgroup":"Queens","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"orientation":"h","width":0.89999999999999991,"base":0,"x":[1758],"y":[1],"text":"Borough: Staten Island<br>Reports: 1,758","type":"bar","textposition":"none","marker":{"autocolorscale":false,"color":"rgba(240,249,33,1)","line":{"width":1.8897637795275593,"color":"transparent"}},"name":"Staten Island","legendgroup":"Staten Island","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":40.840182648401829,"r":7.3059360730593621,"b":37.260273972602747,"l":86.940639269406432},"paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724},"title":{"text":"Interactive Rat Sightings by NYC Borough","font":{"color":"rgba(0,0,0,1)","family":"","size":17.534246575342465},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-750.40000000000009,15758.4],"tickmode":"array","ticktext":["0","5,000","10,000","15,000"],"tickvals":[0,5000,10000,15000],"categoryorder":"array","categoryarray":["0","5,000","10,000","15,000"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.68949771689498},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"y","title":{"text":"Number of Rat Sightings","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.40000000000000002,5.5999999999999996],"tickmode":"array","ticktext":["Staten Island","Queens","Bronx","Manhattan","Brooklyn"],"tickvals":[1,2,3,4,5],"categoryorder":"array","categoryarray":["Staten Island","Queens","Bronx","Manhattan","Brooklyn"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.6529680365296811,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716894984},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176002,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}},"hoverformat":".2f"},"shapes":[],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.68949771689498},"title":{"text":"","font":{"color":"rgba(0,0,0,1)","family":"","size":14.611872146118724}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"source":"A","attrs":{"29944d9214a5":{"x":{},"y":{},"fill":{},"text":{},"type":"bar"}},"cur_data":"29944d9214a5","visdat":{"29944d9214a5":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

A static chart shows the overall pattern, but the interactive version lets the reader hover for exact values without crowding the chart with too many labels.

# Visualization 2: Rat Sightings by Weekday

Next, I looked at sightings by weekday.


``` r
rats_weekday <- rats_clean %>%
  count(sighting_weekday)

rats_weekday
```

```
## # A tibble: 7 × 2
##   sighting_weekday     n
##   <fct>            <int>
## 1 Monday            6989
## 2 Tuesday           6962
## 3 Wednesday         7233
## 4 Thursday          6855
## 5 Friday            5824
## 6 Saturday          3997
## 7 Sunday            3985
```

## Before: Original Weekday Chart

This was my original weekday chart. It worked, but it was not the best choice because weekdays are categories. A line chart can make it look like there is a continuous path from one day to the next, which is not really the point here.


``` r
ggplot(rats_weekday, aes(x = sighting_weekday, y = n)) +
  geom_point(color = "darkred", size = 3) +
  geom_line(aes(group = 1), color = "darkred", linewidth = 1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Rat Sightings by Weekday",
    subtitle = "Original version: the line makes the weekdays look more continuous than they really are",
    x = "Day of the Week",
    y = "Number of Rat Sightings"
  ) +
  theme_minimal()
```

<img src="depalma_project_01_revision_files/figure-html/weekday-before-line-chart-1.png" alt="Original line chart showing rat sightings by weekday. The chart connects weekday categories with a line, which can imply more continuity than the data actually has."  />

## After: Revised Weekday Chart

For the revision, I changed this to a bar chart. This keeps the weekday order but makes the comparison more appropriate for discrete categories.


``` r
top_day <- rats_weekday %>% slice_max(n, n = 1)

ggplot(rats_weekday, aes(x = sighting_weekday, y = n, fill = sighting_weekday)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(n)), vjust = -0.4, size = 3.2) +
  annotate(
    "text",
    x = top_day$sighting_weekday,
    y = top_day$n,
    label = "highest",
    vjust = -1.6,
    size = 3.5
  ) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Rat Sightings by Weekday",
    subtitle = "Reports are not perfectly even across the week",
    x = "Day of the Week",
    y = "Number of Rat Sightings"
  ) +
  theme_minimal()
```

<img src="depalma_project_01_revision_files/figure-html/weekday-after-bar-chart-1.png" alt="Revised bar chart showing rat sightings by weekday from Monday through Sunday. The chart uses bars because weekdays are discrete categories."  />

This chart shows how rat sightings are reported across the week. I kept the weekday order from Monday to Sunday because sorting it by count would make the days feel out of order. The revised bar chart fits the data better because each weekday is its own category.

# Visualization 3: Rat Sightings by Year and Borough

Then I looked at how sightings changed by year for each borough.


``` r
rats_year_borough <- rats_clean %>%
  group_by(sighting_year, borough) %>%
  summarize(total = n(), .groups = "drop")

rats_year_borough
```

```
## # A tibble: 15 × 3
##    sighting_year borough       total
##            <dbl> <chr>         <int>
##  1          2015 Bronx          2125
##  2          2015 Brooklyn       3536
##  3          2015 Manhattan      2714
##  4          2015 Queens         1407
##  5          2015 Staten Island   408
##  6          2016 Bronx          3497
##  7          2016 Brooklyn       5979
##  8          2016 Manhattan      4610
##  9          2016 Queens         2410
## 10          2016 Staten Island   734
## 11          2017 Bronx          2791
## 12          2017 Brooklyn       5493
## 13          2017 Manhattan      3326
## 14          2017 Queens         2199
## 15          2017 Staten Island   616
```


``` r
ggplot(rats_year_borough, aes(x = sighting_year, y = total, color = borough, group = borough)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = c(2015, 2016, 2017)) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Rat Sightings by Year and Borough",
    subtitle = "The number of reports changed over time, and the boroughs did not all look the same",
    x = "Year",
    y = "Number of Rat Sightings",
    color = "Borough"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
```

<img src="depalma_project_01_revision_files/figure-html/year-borough-chart-1.png" alt="Line chart showing rat sightings by year and borough from 2015 to 2017. Each borough has its own line so the yearly trend can be compared across boroughs."  />

This chart helps show the time part of the data. A line chart makes sense here because year is ordered. This also makes it easier to compare whether boroughs followed a similar pattern or if some boroughs changed more than others. I kept the borough colors colorblind-friendly and moved the legend to the bottom so it does not crowd the plot.

# Visualization 4: Rat Sightings by Location Type

I made one extra chart because location type seemed useful. It shows what kinds of places rat sightings were reported from most often.


``` r
rats_location <- rats_clean %>%
  filter(!is.na(location_type)) %>%
  count(location_type, sort = TRUE) %>%
  slice_head(n = 10)

rats_location
```

```
## # A tibble: 10 × 2
##    location_type                     n
##    <chr>                         <int>
##  1 3+ Family Apt. Building       17652
##  2 1-2 Family Dwelling            7669
##  3 Other (Explain Below)          6093
##  4 3+ Family Mixed Use Building   2742
##  5 Commercial Building            2351
##  6 Vacant Lot                     1345
##  7 Construction Site              1205
##  8 Vacant Building                 704
##  9 1-2 Family Mixed Use Building   676
## 10 Catch Basin/Sewer               423
```


``` r
ggplot(rats_location, aes(x = fct_reorder(location_type, n), y = n, fill = location_type)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(n)), hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_viridis_d(option = "plasma") +
  labs(
    title = "Top Location Types for Rat Sightings",
    subtitle = "The most common reported locations give more context for where the rat problem shows up",
    x = "",
    y = "Number of Rat Sightings"
  ) +
  theme_minimal()
```

<img src="depalma_project_01_revision_files/figure-html/location-chart-1.png" alt="Horizontal bar chart showing the top ten location types for rat sightings. The bars are ordered by number of reports."  />

I added this one because borough and weekday felt like they did not tell the whole story and it shows what type of places people were reporting rats from. I only used the top 10 location types because showing every location type would make the chart too crowded and annoying to read.

# Findings

The main story I can tell from these plots is that rat sightings in New York City are not evenly spread out. Some boroughs have way more reports than others. The borough chart makes this pretty obvious because the bars are not close to equal.

The weekday chart shows that reports also change depending on the day of the week. This does not automatically mean rats are only more active on certain days. It could also mean people are more likely to report sightings on certain days. That is one thing I had to keep in mind while looking at this dataset. The dataset is about rat sightings, but it is also about people reporting rat sightings.

The year and borough chart shows that the pattern changes over time. It is useful because it does not only show one big total. It separates the totals by borough and year, which makes the trend easier to compare.

The location type chart adds another part to the story. It helps show where sightings were being reported from, not just what borough they happened in. I think this chart is useful because rats are not just a general city problem. They are connected to specific places and environments.

Overall, the data shows that rat sightings depend on location, time, and reporting behavior. The rats are stupid, but the patterns are actually kind of interesting.

# Design Choices

For the design part, I mostly tried not to make the charts a mess. I used bar charts for categories like borough, weekday, and location type because those are good for comparing counts. For weekday, I originally used a line chart, but I changed it to a bar chart because weekdays are discrete categories. I still kept the normal weekday order because the order of the week matters more than ranking the days from largest to smallest.

I also used a line chart for year because years are ordered, and line charts are better for showing change over time. For the year and borough chart, color helps separate the boroughs, but the points and legend also help the reader follow the groups instead of relying only on color.

I also tried not to overload each chart with too much information. For example, the location type chart only shows the top 10 location types. If I included every location type, the chart would be harder to read and less useful.

I made sure to use clear titles and subtitles so the reader could understand what each chart was supposed to show without needing to stare at it for too long. I also used `theme_minimal()` because it removes extra clutter and keeps the focus on the data. For the final project revision, I used viridis color palettes because they are more accessible and colorblind-friendly.

For the borough chart and location type chart, I reordered the bars by count. That makes it easier to compare the largest and smallest categories. For weekdays, I did not reorder the bars because the normal weekday order matters more.

# Final Project Requirements

For the final project version, I made a few specific updates without changing the whole project.

- **Interactive chart:** I added an interactive borough chart using `plotly`. The reader can hover over each borough to see the exact number of reports.
- **Accessibility:** I added alt text to the main figures and used viridis color palettes instead of relying on less accessible colors.
- **Redesign:** I included the original weekday line chart and the revised weekday bar chart. The bar chart is better because weekdays are categories, not a continuous variable.
- **Annotations and summary statistics:** I added count labels and small annotations to draw attention to the highest categories instead of making the reader figure everything out from the axis alone.

# Conclusion

This project helped me explore rat sightings in New York City using different types of visualizations. The dataset had enough information to compare sightings by borough, weekday, year, and location type. The biggest takeaway is that rat sightings are not evenly distributed. Some boroughs and location types have more reports, and the number of reports changes across time.

The visuals made the dataset easier to understand because the raw data had thousands of rows. Instead of trying to read the table directly, the charts summarized the main patterns. Also, again, I picked the dataset because it is about rats. Aside from the unfortunate dead rat possibly in a wall at work, rats are genuinely just one of those creatures that have a funny side to them. Nonetheless, this ended up being a good dataset for practicing data visualization, and it made the project more entertaining rather than tedious.

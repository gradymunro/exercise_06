---
title: 'Weekly Exercises #6'
author: "Grady Munro"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: true
---





```r
library(tidyverse)     # for data cleaning and plotting
library(gardenR)       # for Lisa's garden data
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(gifski)        # for creating the gif (don't need to load this library every time,but need it installed)
library(transformr)    # for "tweening" (gganimate)
library(shiny)         # for creating interactive apps
library(patchwork)     # for nicely combining ggplot2 graphs  
library(gt)            # for creating nice tables
library(rvest)         # for scraping data
library(robotstxt)     # for checking if you can scrape data
theme_set(theme_minimal())
```


```r
# Lisa's garden data
data("garden_harvest")

#COVID-19 data from the New York Times
covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
```

## Put your homework on GitHub!

Go [here](https://github.com/llendway/github_for_collaboration/blob/master/github_for_collaboration.md) or to previous homework to remind yourself how to get set up. 

Once your repository is created, you should always open your **project** rather than just opening an .Rmd file. You can do that by either clicking on the .Rproj file in your repository folder on your computer. Or, by going to the upper right hand corner in R Studio and clicking the arrow next to where it says Project: (None). You should see your project come up in that list if you've used it recently. You could also go to File --> Open Project and navigate to your .Rproj file. 

## Instructions

* Put your name at the top of the document. 

* **For ALL graphs, you should include appropriate labels.** 

* Feel free to change the default theme, which I currently have set to `theme_minimal()`. 

* Use good coding practice. Read the short sections on good code with [pipes](https://style.tidyverse.org/pipes.html) and [ggplot2](https://style.tidyverse.org/ggplot2.html). **This is part of your grade!**

* **NEW!!** With animated graphs, add `eval=FALSE` to the code chunk that creates the animation and saves it using `anim_save()`. Add another code chunk to reread the gif back into the file. See the [tutorial](https://animation-and-interactivity-in-r.netlify.app/) for help. 

* When you are finished with ALL the exercises, uncomment the options at the top so your document looks nicer. Don't do it before then, or else you might miss some important warnings and messages.


## Warm-up exercises from tutorial

1. Read in the fake garden harvest data. Find the data [here](https://github.com/llendway/scraping_etc/blob/main/2020_harvest.csv) and click on the `Raw` button to get a direct link to the data. After reading in the data, do one of the quick checks mentioned in the tutorial.


```r
X2020_harvest <- read_csv("https://raw.githubusercontent.com/llendway/scraping_etc/main/2020_harvest.csv", 
    col_types = cols(...1 = col_skip(),
                     date = col_date(format = "%m/%d/%y"),
                     weight = col_number()),
    na = "MISSING", 
    skip = 2)

X2020_harvest %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  summary()
```

```
##      vegetable              variety         date                weight    
##  tomatoes :232   grape          : 37   Min.   :2020-06-06   Min.   :   2  
##  lettuce  : 68   Romanesco      : 34   1st Qu.:2020-07-21   1st Qu.:  87  
##  beans    : 38   pickling       : 32   Median :2020-08-09   Median : 252  
##  zucchini : 34   Lettuce Mixture: 28   Mean   :2020-08-08   Mean   : 504  
##  cucumbers: 32   Bonny Best     : 26   3rd Qu.:2020-08-26   3rd Qu.: 599  
##  peas     : 27   (Other)        :525   Max.   :2020-10-03   Max.   :7350  
##  (Other)  :254   NA's           :  3                        NA's   :4     
##    units    
##  grams:685  
##             
##             
##             
##             
##             
## 
```

  
2. Read in this [data](https://www.kaggle.com/heeraldedhia/groceries-dataset) from the kaggle website. You will need to download the data first. Save it to your project/repo folder. Do some quick checks of the data to assure it has been read in appropriately.


```r
groceries_dataset <- read_csv("Groceries_dataset.csv")

groceries_dataset %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  summary()
```

```
##  Member_number          Date               itemDescription 
##  Min.   :1000   21-01-2015:   96   whole milk      : 2502  
##  1st Qu.:2002   21-07-2015:   93   other vegetables: 1898  
##  Median :3005   08-08-2015:   92   rolls/buns      : 1716  
##  Mean   :3004   29-11-2015:   92   soda            : 1514  
##  3rd Qu.:4007   30-04-2015:   91   yogurt          : 1334  
##  Max.   :5000   26-03-2015:   88   root vegetables : 1071  
##                 (Other)   :38213   (Other)         :28730
```


3. Create a table using `gt` with data from your project or from the `garden_harvest` data if your project data aren't ready. Use at least 3 `gt()` functions.


```r
garden_harvest %>% 
  group_by(vegetable, variety) %>% 
  summarize(sum_weight = sum(weight)) %>% 
  gt(rowname_col = "variety",
     groupname_col = "vegetable") %>% 
  tab_header(title = "Garden Harvests",
             subtitle = "Total Harvests (in grams) from Lisa's Garden in 2020") %>% 
  summary_rows(columns = c(sum_weight),
               groups = TRUE,
               fns = list(avg = ~mean(., na.rm = TRUE),
                          sum = ~sum(.,na.rm = TRUE)))
```

```{=html}
<div id="epmpspritn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#epmpspritn .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#epmpspritn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#epmpspritn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#epmpspritn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#epmpspritn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#epmpspritn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#epmpspritn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#epmpspritn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#epmpspritn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#epmpspritn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#epmpspritn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#epmpspritn .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#epmpspritn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#epmpspritn .gt_from_md > :first-child {
  margin-top: 0;
}

#epmpspritn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#epmpspritn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#epmpspritn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#epmpspritn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#epmpspritn .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#epmpspritn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#epmpspritn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#epmpspritn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#epmpspritn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#epmpspritn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#epmpspritn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#epmpspritn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#epmpspritn .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#epmpspritn .gt_left {
  text-align: left;
}

#epmpspritn .gt_center {
  text-align: center;
}

#epmpspritn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#epmpspritn .gt_font_normal {
  font-weight: normal;
}

#epmpspritn .gt_font_bold {
  font-weight: bold;
}

#epmpspritn .gt_font_italic {
  font-style: italic;
}

#epmpspritn .gt_super {
  font-size: 65%;
}

#epmpspritn .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="2" class="gt_heading gt_title gt_font_normal" style>Garden Harvests</th>
    </tr>
    <tr>
      <th colspan="2" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style>Total Harvests (in grams) from Lisa's Garden in 2020</th>
    </tr>
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sum_weight</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">apple</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">unknown</td>
<td class="gt_row gt_right">156</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">156.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">156.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">asparagus</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">asparagus</td>
<td class="gt_row gt_right">20</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">20.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">20.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">basil</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Isle of Naxos</td>
<td class="gt_row gt_right">490</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">490.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">490.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">beans</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Bush Bush Slender</td>
<td class="gt_row gt_right">10038</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Chinese Red Noodle</td>
<td class="gt_row gt_right">356</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Classic Slenderette</td>
<td class="gt_row gt_right">1635</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">4,009.67</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">12,029.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">beets</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Gourmet Golden</td>
<td class="gt_row gt_right">3185</td></tr>
    <tr><td class="gt_row gt_left gt_stub">leaves</td>
<td class="gt_row gt_right">101</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Sweet Merlin</td>
<td class="gt_row gt_right">2897</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">2,061.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">6,183.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">broccoli</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Main Crop Bravado</td>
<td class="gt_row gt_right">967</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Yod Fah</td>
<td class="gt_row gt_right">372</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">669.50</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">1,339.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">carrots</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Bolero</td>
<td class="gt_row gt_right">3761</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Dragon</td>
<td class="gt_row gt_right">1862</td></tr>
    <tr><td class="gt_row gt_left gt_stub">greens</td>
<td class="gt_row gt_right">169</td></tr>
    <tr><td class="gt_row gt_left gt_stub">King Midas</td>
<td class="gt_row gt_right">1858</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">1,912.50</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">7,650.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">chives</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">perrenial</td>
<td class="gt_row gt_right">8</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">8.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">8.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">cilantro</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">cilantro</td>
<td class="gt_row gt_right">52</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">52.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">52.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">corn</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Dorinny Sweet</td>
<td class="gt_row gt_right">5174</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Golden Bantam</td>
<td class="gt_row gt_right">727</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">2,950.50</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">5,901.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">cucumbers</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">pickling</td>
<td class="gt_row gt_right">19781</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">19,781.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">19,781.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">edamame</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">edamame</td>
<td class="gt_row gt_right">2763</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">2,763.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">2,763.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">hot peppers</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">thai</td>
<td class="gt_row gt_right">67</td></tr>
    <tr><td class="gt_row gt_left gt_stub">variety</td>
<td class="gt_row gt_right">599</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">333.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">666.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">jalapeño</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">giant</td>
<td class="gt_row gt_right">4478</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">4,478.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">4,478.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">kale</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Heirloom Lacinto</td>
<td class="gt_row gt_right">2697</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">2,697.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">2,697.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">kohlrabi</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Crispy Colors Duo</td>
<td class="gt_row gt_right">191</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">191.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">191.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">lettuce</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Farmer's Market Blend</td>
<td class="gt_row gt_right">1725</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Lettuce Mixture</td>
<td class="gt_row gt_right">2154</td></tr>
    <tr><td class="gt_row gt_left gt_stub">mustard greens</td>
<td class="gt_row gt_right">23</td></tr>
    <tr><td class="gt_row gt_left gt_stub">reseed</td>
<td class="gt_row gt_right">45</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Tatsoi</td>
<td class="gt_row gt_right">1313</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">1,052.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">5,260.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">onions</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Delicious Duo</td>
<td class="gt_row gt_right">342</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Long Keeping Rainbow</td>
<td class="gt_row gt_right">1502</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">922.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">1,844.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">peas</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Magnolia Blossom</td>
<td class="gt_row gt_right">3383</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Super Sugar Snap</td>
<td class="gt_row gt_right">4340</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">3,861.50</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">7,723.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">peppers</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">green</td>
<td class="gt_row gt_right">2582</td></tr>
    <tr><td class="gt_row gt_left gt_stub">variety</td>
<td class="gt_row gt_right">1656</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">2,119.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">4,238.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">potatoes</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">purple</td>
<td class="gt_row gt_right">1365</td></tr>
    <tr><td class="gt_row gt_left gt_stub">red</td>
<td class="gt_row gt_right">2011</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Russet</td>
<td class="gt_row gt_right">4124</td></tr>
    <tr><td class="gt_row gt_left gt_stub">yellow</td>
<td class="gt_row gt_right">3357</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">2,714.25</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">10,857.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">pumpkins</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Cinderella's Carraige</td>
<td class="gt_row gt_right">14911</td></tr>
    <tr><td class="gt_row gt_left gt_stub">New England Sugar</td>
<td class="gt_row gt_right">20348</td></tr>
    <tr><td class="gt_row gt_left gt_stub">saved</td>
<td class="gt_row gt_right">34896</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">23,385.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">70,155.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">radish</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Garden Party Mix</td>
<td class="gt_row gt_right">429</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">429.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">429.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">raspberries</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">perrenial</td>
<td class="gt_row gt_right">843</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">843.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">843.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">rutabaga</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Improved Helenor</td>
<td class="gt_row gt_right">13490</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">13,490.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">13,490.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">spinach</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Catalina</td>
<td class="gt_row gt_right">923</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">923.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">923.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">squash</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Blue (saved)</td>
<td class="gt_row gt_right">18835</td></tr>
    <tr><td class="gt_row gt_left gt_stub">delicata</td>
<td class="gt_row gt_right">4762</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Red Kuri</td>
<td class="gt_row gt_right">10311</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Waltham Butternut</td>
<td class="gt_row gt_right">11009</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">11,229.25</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">44,917.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">strawberries</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">perrenial</td>
<td class="gt_row gt_right">592</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">592.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">592.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">Swiss chard</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Neon Glow</td>
<td class="gt_row gt_right">3122</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">3,122.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">3,122.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">tomatoes</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Amish Paste</td>
<td class="gt_row gt_right">29789</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Better Boy</td>
<td class="gt_row gt_right">15426</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Big Beef</td>
<td class="gt_row gt_right">11337</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Black Krim</td>
<td class="gt_row gt_right">7170</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Bonny Best</td>
<td class="gt_row gt_right">11305</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Brandywine</td>
<td class="gt_row gt_right">7097</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Cherokee Purple</td>
<td class="gt_row gt_right">7127</td></tr>
    <tr><td class="gt_row gt_left gt_stub">grape</td>
<td class="gt_row gt_right">14694</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Jet Star</td>
<td class="gt_row gt_right">6815</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Mortgage Lifter</td>
<td class="gt_row gt_right">11941</td></tr>
    <tr><td class="gt_row gt_left gt_stub">Old German</td>
<td class="gt_row gt_right">12119</td></tr>
    <tr><td class="gt_row gt_left gt_stub">volunteers</td>
<td class="gt_row gt_right">23411</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">13,185.92</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">158,231.00</td>
    </tr>
    <tr class="gt_group_heading_row">
      <td colspan="2" class="gt_group_heading">zucchini</td>
    </tr>
    <tr><td class="gt_row gt_left gt_stub">Romanesco</td>
<td class="gt_row gt_right">45227</td></tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row gt_first_summary_row">avg</td>
      <td class="gt_row gt_right gt_summary_row gt_first_summary_row">45,227.00</td>
    </tr>
    <tr>
      <td class="gt_row gt_stub gt_right gt_summary_row">sum</td>
      <td class="gt_row gt_right gt_summary_row">45,227.00</td>
    </tr>
  </tbody>
  
  
</table>
</div>
```


4. CHALLENGE (not graded): Write code to replicate the table shown below (open the .html file to see it) created from the `garden_harvest` data as best as you can. When you get to coloring the cells, I used the following line of code for the `colors` argument:
  

```r
colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "RColorBrewer::YlGn"
      ) %>% as.character()
```


  
5. Use `patchwork` operators and functions to combine at least two graphs using your project data or `garden_harvest` data if your project data aren't read.


```r
lettuce_histogram <- garden_harvest %>% 
  filter(vegetable %in% c("lettuce")) %>% 
  ggplot(aes(x = weight)) +
  geom_histogram(colour = "white", 
                 fill = "darkgreen") +
  labs(title = "Distribution of Lettuce Harvests",
       y = "Frequency",
       x = "Harvest Weight (grams)")+
  geom_vline(aes(xintercept = mean(weight, na.rm = TRUE)), 
             colour = "red")

lettuce_bars <- garden_harvest %>% 
  filter(vegetable %in% c("lettuce")) %>% 
  group_by(variety) %>% 
  ggplot(aes(y = fct_rev(fct_infreq(variety)))) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Harvests of Lettuce Varieties",
       x = "Number of Harvests",
       y = "")

lettuce_histogram +
  lettuce_bars +
  plot_annotation(title = "Analysis of Lettuce Harvests from Lisa's Garden in 2020")
```

![](06_exercises_GradyMunro_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

  
## Webscraping exercise (also from tutorial)

Use the data from the [Macalester Registrar's Fall 2017 Class Schedule](https://www.macalester.edu/registrar/schedules/2017fall/class-schedule/#crs10008) to complete all these exercises.

6. Find the correct selectors for the following fields. Make sure that each matches 762 results:

  * Course Number
  * Course Name
  * Day
  * Time
  * Room
  * Instructor
  * Avail. / Max
  * General Education Requirements (make sure you only match 762; beware of the Mac copyright banner at the bottom of the page!)
  * Description

Then, put all this information into one dataset (tibble or data.frame) Do not include any extraneous information like "Instructor: ".
  

```r
fall2017 <- read_html("https://www.macalester.edu/registrar/schedules/2017fall/class-schedule/#crs10008")

course_nums <- 
  fall2017 %>%
  html_elements(".class-schedule-course-number") %>%
  html_text2()

course_names <- 
  fall2017 %>%
  html_elements(".class-schedule-course-title") %>%
  html_text2()

course_days <- fall2017 %>%
  html_elements(".class-schedule-course-title+ .class-schedule-label") %>%
  html_text2() %>% 
  str_sub(start = 7)

course_times <-
  fall2017 %>% 
  html_elements(".class-schedule-label:nth-child(4)") %>%
  html_text2() %>% 
  str_sub(start = 7)

course_rooms <- 
  fall2017 %>%
  html_elements(".class-schedule-label:nth-child(5)") %>%
  html_text2() %>% 
  str_sub(start = 7)

course_intructors <- 
  fall2017 %>%
  html_elements(".class-schedule-label:nth-child(6)") %>%
  html_text2() %>% 
  str_sub(start = 13)

course_avail <- 
  fall2017 %>%
  html_elements(".class-schedule-label:nth-child(7)") %>%
  html_text2() %>% 
  str_sub(start = 14)

course_requirements <-
  fall2017 %>% 
  html_elements("#content p:nth-child(2)") %>% 
  html_text2() %>% 
  str_sub(start = 35)
head(course_requirements)
```

```
## [1] "U.S. Identities and Differences\n"            
## [2] "U.S. Identities and Differences\nWriting WA\n"
## [3] ""                                             
## [4] "U.S. Identities and Differences\nWriting WP\n"
## [5] "U.S. Identities and Differences\n"            
## [6] ""
```

```r
course_descriptions <-
  fall2017 %>% 
  html_elements(".collapsed p:nth-child(1)") %>% 
  html_text2() %>% 
  str_sub(start = 3)

course_df <- tibble(number = course_nums,
                    name = course_names,
                    days = course_days,
                    time = course_times,
                    room = course_rooms,
                    instructor = course_intructors,
                    availability = course_avail,
                    requirements = course_requirements,
                    description = course_descriptions)
```


7. Create a graph that shows the number of sections offered per department. Hint: The department is a substring of the course number - there are `str_XXX()` functions that can help. Yes, COMP and MATH are the same department, but for this exercise you can just show the results by four letter department code, e.g., with COMP and MATH separate.


```r
course_df %>% 
  mutate(department = substr(number,
                            1,
                            4)) %>% 
  group_by(department) %>% 
  ggplot(aes(y = fct_rev(fct_infreq(department)))) +
  geom_bar() +
  labs(title = "Harvests of Sections Offered by Department",
       x = "Number of Sections",
       y = "")
```

![](06_exercises_GradyMunro_files/figure-html/unnamed-chunk-7-1.png)<!-- -->



8. Analyze the typical length of course names by department. To do so, create a new data table based on your courses data table, with the following changes:
  
  * New columns for the length of the title of a course and the length of the description of the course. Hint: `str_length`.  
  * Remove departments that have fewer than 10 sections of courses. To do so, group by department, then remove observations in groups with fewer than 10 sections (Hint: use filter with n()). Then `ungroup()` the data.  
  * Create a visualization of the differences across groups in lengths of course names or course descriptions. Think carefully about the visualization you should be using!


```r
course_df_lengths <- course_df %>% 
  mutate(name_length = str_length(name)) %>% 
  mutate(description_length = str_length(description)) %>% 
  mutate(department = substr(number,
                            1,
                            4)) %>% 
  group_by(department) %>% 
  filter(n() >= 10) %>% 
  ungroup()

name_plot <- course_df_lengths %>% 
  ggplot(aes(x = name_length,
             y = department)) +
  geom_col(fill = "red") +
  labs(title = "Combined Length of All Course \nNames for each Department",
       x = "Characters",
       y = "")

description_plot <- course_df_lengths %>% 
  ggplot(aes(x = description_length,
             y = department)) +
  geom_col(fill = "blue") +
  labs(title = "Combined Length of All Course \nDescriptions for each Department",
       x = "Characters",
       y = "")

name_plot + description_plot
```

![](06_exercises_GradyMunro_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

  

**DID YOU REMEMBER TO UNCOMMENT THE OPTIONS AT THE TOP?**

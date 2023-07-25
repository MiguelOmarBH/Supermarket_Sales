Supermarket Sales
================
Miguel Omar
2023-07-24

## Introduction

Hello! Welcome to this notebook. We’re going to explore a sales dataset
of supermarkets in 3 different cities. The sales data refers to a period
of three months. In this notebook we’ll learn:

- Cleaning column name formats
- Convert data values to weekdays to explore which day has the highest
  incomes
- Visualize income data over time
- Plotting revenue grouping by city and gender to explore differences
- Determine the preferred payment method by customers

The dataset was obtained from Kaggle’s [Aung
Pyae](https://www.kaggle.com/datasets/aungpyaeap/supermarket-sales)
user. Thanks to him and the beautiful data community for their
contributions!

With that, we are able to start.

## Uploading libraries & dataset

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggthemes) # Optional
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library(lubridate)
supermarket_sales <- read_csv("supermarket_sales.csv")
```

    ## Rows: 1000 Columns: 17
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): Invoice ID, Branch, City, Customer type, Gender, Product line, Dat...
    ## dbl  (8): Unit price, Quantity, Tax 5%, Total, cogs, gross margin percentage...
    ## time (1): Time
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Data transforming

Let’s have a quick look to our dataset:

``` r
head(supermarket_sales)
```

    ## # A tibble: 6 × 17
    ##   `Invoice ID` Branch City    `Customer type` Gender `Product line` `Unit price`
    ##   <chr>        <chr>  <chr>   <chr>           <chr>  <chr>                 <dbl>
    ## 1 750-67-8428  A      Yangon  Member          Female Health and be…         74.7
    ## 2 226-31-3081  C      Naypyi… Normal          Female Electronic ac…         15.3
    ## 3 631-41-3108  A      Yangon  Normal          Male   Home and life…         46.3
    ## 4 123-19-1176  A      Yangon  Member          Male   Health and be…         58.2
    ## 5 373-73-7910  A      Yangon  Normal          Male   Sports and tr…         86.3
    ## 6 699-14-3026  C      Naypyi… Normal          Male   Electronic ac…         85.4
    ## # ℹ 10 more variables: Quantity <dbl>, `Tax 5%` <dbl>, Total <dbl>, Date <chr>,
    ## #   Time <time>, Payment <chr>, cogs <dbl>, `gross margin percentage` <dbl>,
    ## #   `gross income` <dbl>, Rating <dbl>

``` r
colnames(supermarket_sales)
```

    ##  [1] "Invoice ID"              "Branch"                 
    ##  [3] "City"                    "Customer type"          
    ##  [5] "Gender"                  "Product line"           
    ##  [7] "Unit price"              "Quantity"               
    ##  [9] "Tax 5%"                  "Total"                  
    ## [11] "Date"                    "Time"                   
    ## [13] "Payment"                 "cogs"                   
    ## [15] "gross margin percentage" "gross income"           
    ## [17] "Rating"

As we can see, we have different variables regarding the supermarket
city, product lines, dates, membership, revenue, gross income… We can
also notice that the some column names have spaces. To avoid problems,
let’s make a few transformations:

``` r
supermarket_sales <- clean_names(supermarket_sales) # Clean all column names to avoid problems while working
supermarket_sales$date <- mdy(supermarket_sales$date) # Tell R the date format the dataset has
supermarket_sales$gender <- factor(supermarket_sales$gender)
supermarket_sales$city <- factor(supermarket_sales$city)
supermarket_sales$payment <- factor(supermarket_sales$payment) # Converting few variables to a factor type
```

By now, this is what we have:

``` r
head(supermarket_sales)
```

    ## # A tibble: 6 × 17
    ##   invoice_id  branch city  customer_type gender product_line unit_price quantity
    ##   <chr>       <chr>  <fct> <chr>         <fct>  <chr>             <dbl>    <dbl>
    ## 1 750-67-8428 A      Yang… Member        Female Health and …       74.7        7
    ## 2 226-31-3081 C      Nayp… Normal        Female Electronic …       15.3        5
    ## 3 631-41-3108 A      Yang… Normal        Male   Home and li…       46.3        7
    ## 4 123-19-1176 A      Yang… Member        Male   Health and …       58.2        8
    ## 5 373-73-7910 A      Yang… Normal        Male   Sports and …       86.3        7
    ## 6 699-14-3026 C      Nayp… Normal        Male   Electronic …       85.4        7
    ## # ℹ 9 more variables: tax_5_percent <dbl>, total <dbl>, date <date>,
    ## #   time <time>, payment <fct>, cogs <dbl>, gross_margin_percentage <dbl>,
    ## #   gross_income <dbl>, rating <dbl>

All our variables are correctly formatted. Column names are lowercase
and avoiding spaces. Notice that we have variables such as the total
income and the gross income (that is, removing the production cost to
the total income), so in that case we don’t need to create a new
variable to calculate it.

## Data insights

At this point, we are able to dive deeper into our data. We can quickly
summarize it:

``` r
summary <- supermarket_sales %>% 
  group_by(city) %>% 
  summarize("Total Income" = sum(total), "Total Revenue" = sum(gross_income))
head(summary)
```

    ## # A tibble: 3 × 3
    ##   city      `Total Income` `Total Revenue`
    ##   <fct>              <dbl>           <dbl>
    ## 1 Mandalay         106198.           5057.
    ## 2 Naypyitaw        110569.           5265.
    ## 3 Yangon           106200.           5057.

Now we have an overall idea of our supermarkets performance. While
Mandalay and Yangon are almost identical, Naypyitaw is the market with
most revenue during the last months.

IMPORTANT: notice that in this dataset the gross margin percentage
applied is the same for every product line.

### Income evolution

We can create a graphical representation of the income evolution over
time. As each date could have many sales, first we have to summarize:

``` r
library(plyr)
```

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

``` r
supermarket_summary <- ddply(supermarket_sales, c("date"), summarize, total = sum(total))
detach(package:plyr)
```

After summarizing, time to plot:

``` r
ggplot(data = supermarket_summary, mapping = aes(x= date, y= total)) +
  geom_line(color="#0099f9", size=1) +
  labs(title = "Income evolution over time",
       x= "Date",
       y= "Daily income") +
  theme_bw()
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](supermarket_sales_notebook_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

We can see noticeable changes over time, so we could argue that in some
moments of the week the sales are higher than in others. Let’s try to
figure it out:

IMPORTANT: weekdays in the next plot are in Spanish due that my OS
language is set in that language.

``` r
supermarket_summary %>% 
  mutate(day = weekdays(date)) %>% # Tranforming the dates to weekdays (Monday, Tuesday...)
  ggplot(mapping = aes(x = day, y = total, fill= ifelse(day == "sábado", "highlighted", "normal"))) +
  geom_col(show.legend= FALSE) +
  scale_fill_manual("legend", values= c("highlighted"= "red", "normal"= "#0099f9")) +
  labs(title= "Saturday is the most revenuable day",
       subtitle = "Average income by day of the week",
       x= "Day",
       y= "Average income") + # A bar plot to represent the mean income by day of the week
  theme_economist()
```

![](supermarket_sales_notebook_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

Observing the plot we clearly observe that during Saturdays the mean
income is higher than the rest of the week. We also notice that the
opposite happens with Mondays.

### Going into details

Before we’ve analyzed which market has the highest revenue and the
evolution of the income over time and between weekdays. Now, we can
obtain valuable information going further into details.

#### Which is the favorite payment method?

That could be an interesting question. As we have just 3 payment methods
(Cash, Ewallet and Credit Card), a pie chart will give us this
information at a glance:

``` r
ggplot(data = supermarket_sales, mapping = aes(x="", y= payment, fill = payment)) +
  geom_bar(stat="identity", width=1) + # Starting from a bar chart
  coord_polar("y", start=0) + # Converting the bar into a 360º circle
  labs(title="Electronic methods are preferred by customers") +
  theme_void() # Theme to remove the background
```

![](supermarket_sales_notebook_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Now, we know that customers prefer to pay with card or Ewallet.

#### Males vs. Females: who buys the most?

It’s also important to observe differences between gender. Let’s dive
into the mean waste made by males and females:

``` r
ggplot(data = supermarket_sales, mapping = aes(x= city, y= total, fill = gender)) +
  geom_col(position="dodge", width = .5) +
  labs(title= "Avergae spend per sale made by men & women",
       x= "City",
       y= "Average waste")
```

![](supermarket_sales_notebook_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Noticeable differences in waste per gender appear in Mandalay and Yangon
markets. We can perform statistical methods such as ANOVA or T-test
(depending on the sample characteristics) to see if this differences are
significant, but not in this tutorial. In general, females tended to
waste a bit more in this time period:

``` r
supermarket_sales %>% 
  group_by(gender) %>%
  summarise("Total income" = sum(total))
```

    ## # A tibble: 2 × 2
    ##   gender `Total income`
    ##   <fct>           <dbl>
    ## 1 Female        167883.
    ## 2 Male          155084.

Now, let’s try to observe data by product line:

``` r
ggplot(data= supermarket_sales, mapping = aes(y= product_line, x= sum(quantity), fill = gender)) +
  geom_col(mapping = aes(width = .4)) +
  labs(title= "Units sold by product line",
       x= "Units Sold",
       y= "Product line") +
  theme_minimal()
```

    ## Warning in geom_col(mapping = aes(width = 0.4)): Ignoring unknown aesthetics:
    ## width

![](supermarket_sales_notebook_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

We can label each product line with the gender percentage by summarizing
and calculating the %, but we can observe that males have more sales in
Health and beauty. In general, Food and beverages and Fashions
accesories are the most selled lines.

## Conclusion

Here, we’ve showed a small sales report to gain some insights about
supermarkets in 3 different cities during 3 months. We could have go
further into more analysis, looking for patterns regarding membership
types for example. We could also have built a regression model to
predict sales given time variables. The purpose of this notebook is to
show a simple exploratory analysis. In next notebook, we’ll explore more
in depth methods.

Thank you for reading!

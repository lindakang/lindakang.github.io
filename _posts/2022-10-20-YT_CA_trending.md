---
layout: post
title: YouTube trending video data (2) - Canada
tag: youtube, kaggle, data analysis
categories: data
---

## US, Canada & Japan

### Background:

This exercise is for practice and entertainment purposes only, and will
only focus on YouTube channels from the US, Canada, and Japan. It will
be divided into three parts to ensure it is not too lengthy.

### Dataset

Dataset link: [YouTube Trending Video Dataset (updated
daily)](https://www.kaggle.com/datasets/rsrishav/youtube-trending-video-dataset)<br>

List of YouTube videos that have been on daily trending list from
2020-08-12 to 2022-10-02.

### Want to know:

1.  Channels & videos that on trending (top 50 \# of days on trending)
2.  Channels & videos with most views (top 100)
3.  Paid from views (top 20)

### YouTube Channel category

ID - Category name<br> 1 - Film & Animation<br> 2 - Autos<br> 10 -
Music<br> 15 - Pets & Animals<br> 17 - Sports<br> 19 - Travel
&Events<br> 20 - Gaming<br> 21 - Vblogging<br> 22 - Blogs<br> 23 -
Comedy<br> 24 - Entertainment<br> 25 - News & Politics<br> 26 - Howto &
Style<br> 27 - Education<br> 28 - Science & Technology<br> 29 -
Nonprofits & Activism<br> 43 - Shows<br>

## Methods

### 1. Data preparation

1-1. Load libraries

``` r
library(tidyr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ dplyr   1.0.10
    ## ✔ tibble  3.1.8      ✔ stringr 1.4.1
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2
    ## ✔ purrr   0.3.4      
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(tibble)
library(dbplyr)
```

    ##
    ## Attaching package: 'dbplyr'
    ##
    ## The following objects are masked from 'package:dplyr':
    ##
    ##     ident, sql

``` r
library(utils)
library(lubridate)
```

    ##
    ## Attaching package: 'lubridate'
    ##
    ## The following objects are masked from 'package:base':
    ##
    ##     date, intersect, setdiff, union

1-2. Import datasets

``` r
CA <- read_csv("/Users/Linda/Desktop/RStudio/Kaggle/YouTube/CA_youtube_trending_data.csv")
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 182143 Columns: 16
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): video_id, title, channelId, channelTitle, tags, thumbnail_link, de...
    ## dbl  (5): categoryId, view_count, likes, dislikes, comment_count
    ## lgl  (2): comments_disabled, ratings_disabled
    ## dttm (2): publishedAt, trending_date
    ##
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

### 2. Data cleaning

2-1. Check for duplicates

``` r
sum(duplicated(CA))  # 83 duplicate
```

    ## [1] 109

2-2. Remove duplicates

``` r
# use distinct() to remove duplicates
CA <- distinct(CA)

# check if it's removed
sum(duplicated(CA))
```

    ## [1] 0

2.3 - Extract columns we need

We will only look at these columns: title, channel_title, category_id,
trending_date, tags, views, likes, dislikes, comment_count

``` r
# select columns we need for analysis
# add a "category" column
CA_data <- CA %>%
  select(title, channelTitle, categoryId, trending_date, tags, view_count, likes, dislikes, comment_count) %>%
  mutate("category" = as.factor(categoryId)) %>%
  mutate(category = recode(category, "1" = "Film", "2" = "Autos", "10" = "Music", "15" = "Pets",
                           "17" = "Sports", "19" = "Travel", "20" = "Gaming", "22" = "Blogs",
                           "23" = "Comedy", "24" = "Entertainment", "25" = "News",
                           "26" = "HowToD", "27" = "Education", "28" = "Science", "29" = "NPO"))

# convert trending date to the date format
CA_data$trending_date<- as.Date(CA_data$trending_date, format = "%y-%m-%d")

# create a "trending _year" column
CA_data <- CA_data %>%
  mutate(trending_year = format(trending_date, format = '%Y'))

glimpse(CA_data)
```

    ## Rows: 182,034
    ## Columns: 11
    ## $ title         <chr> "Diljit Dosanjh: CLASH (Official) Music Video | G.O.A.T.…
    ## $ channelTitle  <chr> "Diljit Dosanjh", "jacksepticeye", "Apex Legends", "Braw…
    ## $ categoryId    <dbl> 10, 24, 20, 22, 26, 27, 17, 17, 22, 24, 24, 10, 24, 10, …
    ## $ trending_date <date> 2020-08-12, 2020-08-12, 2020-08-12, 2020-08-12, 2020-08…
    ## $ tags          <chr> "clash diljit dosanjh|diljit dosanjh|diljit dosanjh goat…
    ## $ view_count    <dbl> 9140911, 2038853, 2381688, 1514614, 1123889, 1050143, 75…
    ## $ likes         <dbl> 296541, 353797, 146740, 156914, 45803, 89192, 8278, 1655…
    ## $ dislikes      <dbl> 6180, 2628, 2794, 5857, 964, 855, 331, 4198, 1860, 5759,…
    ## $ comment_count <dbl> 30059, 40222, 16549, 35331, 2198, 6455, 2441, 15777, 705…
    ## $ category      <fct> Music, Entertainment, Gaming, Blogs, HowToD, Education, …
    ## $ trending_year <chr> "2020", "2020", "2020", "2020", "2020", "2020", "2020", …

2-4. Check for NAs

There’s no need to check for NAs at the beginning of the analysis if we
don’t plan on using certain columns. We can check for NAs later on when
we actually need to use the data in those columns.

``` r
sum(is.na(CA_data)) # 0 NAs
```

    ## [1] 0

No NAs in the columns in our final version of dataset.

## 3. Data analysis

### 3-1. All categories

To get a general trend from 2020 to present, we start by analyzing all
categories and videos.

#### 3-1-1. Number times of trending

Which category is on trending most frequently each year between 2020 and
present?

``` r
# All category and all videos
# no. times of trending of each category
CA_channel_trending <- CA_data %>%
  group_by(trending_year, category) %>%
  summarise(chtrending_n = n(),
            ch_views_sum = sum(view_count),
            ch_views_avg = ch_views_sum / chtrending_n) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'trending_year'. You can override using the
    ## `.groups` argument.

``` r
# Fig 01 - No. times of trending & total views
ggplot(CA_channel_trending, aes(y = category, x = chtrending_n, fill = ch_views_sum)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = 'No. times of trending of each category (total views)',
       x = 'No. of times on Trending',
       y = 'Category')
```

![Figure 1](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEiypAdbUCa7zTGl-AiH08lEU77LOtqpxFSDncA_ZMMwEUIMRFnjkuakDREtR7wH6XAjnTi3sTx-1blYzWFO3THuZjVCWRDh6Gvrc1fX9HwktgmBVoka1jNRPKgAORCzxYapdm8rJGfqB4RzIzoQuvbt5wqRpaKB24Xk3aMNza1t96931RjX7OUDllSp/s16000/p01_trending_category_total_views.png)<!-- -->

In 2021, Entertainment and Gaming videos have become increasingly
popular, as they appear most frequently on the trending list and have
higher total views. Although Music is the third most popular category on
the trending list, it has either higher or comparable total views to
Entertainment and Gaming.

What are the channels on trending for \>200 times?

``` r
# no. times of trending of each channel
CA_channel_trending <- CA_data %>%
  group_by(trending_year, channelTitle) %>%
  summarise(chtrending_n = n(),
            ch_views_sum = sum(view_count)) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'trending_year'. You can override using the
    ## `.groups` argument.

``` r
# Channels on Trending for >200 times
CA_channel_200trending <- CA_channel_trending %>%
  arrange(desc(chtrending_n)) %>%
  filter(chtrending_n > 200)

# Fig 02 - Trending times do not correlate with # views
ggplot(CA_channel_200trending, aes(y = channelTitle, x = chtrending_n, fill = ch_views_sum)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = 'Channels on trending >200 times',
       x = 'No. of times on trending',
       y = 'YouTube Channels')
```

![Figure 2](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEg7qcF-x8bp06qxsWwVBm3IZHeKL1j7-taMk_Yu399kc6mHJmeY63z-fPlq2XRdC3oVw5QaEMPwnWqcwf6eLIgzVYQKdbFp7lNTkkB_mrTU1KER16-1uvLmDv6bGPosxde66-BUwEEk7R0XiPcYRNPWeTPu2GkSzDFu6d20SgnkC3pQH_52ztb_AOLx/s16000/p02_trending_channels.png)<!-- -->

The NBA channel had a high frequency of appearing on the trending list
between 2020 and 2022, possibly due to the frequent games held every
year.

In 2021, MrBeast’s channels “MrBeast” and “MrBeast Gaming” dominated the
trending list, appearing more than 200 times and continuing to grow in
popularity.

What are the videos that were on trending for more than 30 days?

``` r
# Videos on Trending for >30 days
CAvideo_trending <- CA_data %>%
  group_by(title) %>%
  summarise(vtrending_n = n(),
            video_views_sum = sum(view_count),
            category = first(category),
            channelTitle = last(channelTitle)) %>%
  as.data.frame()

CA_video_30trending <- CAvideo_trending %>%
  arrange(desc(vtrending_n)) %>%
  filter(vtrending_n > 30)

CA_video_30trending
```

    ##              title vtrending_n video_views_sum category channelTitle
    ## 1 Starlink Mission         114        84822822  Science       SpaceX

The SpaceX Starlink Mission is the only video that remained on the
trending list for more than a month, actually staying on the list for 3
months.

#### 3-1-2. Highest views

Now we have narrowed down to the top 100 videos with the highest views.

``` r
# rank Canada videos with top 100 highest views from 2020 to 2022
# total 100 videos
CA_channel_top100 <- CA_data %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# rank Canada channels with top 100 highest views from 2020 to 2022
# total 300 videos = 100 videos per year
CA_channel_top100_year <- CA_data %>%
  group_by(trending_year) %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# Category of highest views
CA_category_views <- CA_channel_top100_year %>%
  group_by(category, trending_year) %>%
  summarise(n_cat_views = n(),
            avg_views = sum(view_count) / n_cat_views) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'category'. You can override using the
    ## `.groups` argument.

``` r
# Fig 03 - Top 100 views videos are Music and Entertainment
ggplot(CA_category_views, aes(y = category, x = n_cat_views, fill = avg_views)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = 'Category of videos with top 100 views (2020 - present)',
       x = '# of trending videos in top 100 views',
       y = 'YouTube Category',
       color = "Avg views")
```

![Figure 3](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEhaHFi1FjRHjzOaslw_TCOTpjUR05opJumvc6gfAbTRu4QGa9HJMCDAbW8z4F8gB4R_0f1c6RlSrJbqXCwQajIH4X7E06ujd4CDXP-UXeuZjuFSCZO9a6A4M5P5fcsGQ1wcC6H8M49RkiBN0T4jG_DNTe4qCTYbWMLnH0UiNxKPlIWROmIAd7rQgck1/s16000/p03_channels_top100views.png)<!-- -->

In 2020, the most popular videos were music, followed by a shift to
Entertainment in 2021. Music regained its popularity in 2022 and 2023.

``` r
# Fig 04 - Channels with videos of top 100 views: from 2020 to present
ggplot(CA_channel_top100, aes(y = channelTitle, x = view_count, alpha = likes)) +
  geom_point(color = "blue") +
  facet_grid(. ~trending_year) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = 'Canadian YouTube Channels with top 100 highest views',
       x = '# of viewss',
       y = 'YouTube Channels')
```

![Figure 4](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEhc0exoIB0bL-oHET6tgqIP_cNB3CJJ-XOUiCVd3GQUSvwrdDUV3S43-gqnXpWmyCkQLLmAKOVZq7BnFHVP_ruJgIrAdbTBm0kpXt22Lwk6IIdCjWsfchYlZrHfQIPSaom4kWg4n4N6lGUPpu6uNIg2hnNkixiWJX9ATpwERfsYUNoozhacHBS4kltt/s16000/p04_channels_top100views.png)<!-- -->

BLACKPINK has had the highest views from 2020 to 2022. The top 100
viewed channels were mostly from 2021 and were more diverse. Bizarrap,
which is a personal channel of an Argentine DJ and record producer, is a
new popular channel that has risen in 2023.

Note that Big Hit Labels was the former name of HYBE LABELS, which is
the company that BTS belongs to.”

``` r
# Fig 05 - Top 100 viewed videos: Higher views do not correlate with more likes
ggplot(CA_channel_top100, aes(y = title, x = view_count, alpha = likes)) +
  geom_point(color = "blue") +
  theme_linedraw(base_family = "NanumGothic") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = 'Top 100 Canadian YouTube videos with higest views',
       x = '# of views',
       y = 'YouTube videos')
```

![Figure 5](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgTHq-hNg6Gvf6LKbEiUPWiYMz8Jt5LBaDnyz7kNlFU_rd8q76DxY2JdoF20-EeudDSBbw7FgtSzIn92ztO0yImh4KNrQYymtD86gVKYNZaATdkRVGGIuaDCFLEK-kBRZ7V_FdLtsdyf8_QhrGpapxGqKxm7r1vDA9irqqEqksgag9ogOG3-rwR-J3b/s16000/p05_videos_top100views.png)<!-- -->

The most popular videos in the top 100 are mostly music videos and
shorts.

### 3-1-3. Paid by ad views

Based on the information that only about half of the total views are
monetized and the average revenue earned per 1,000 views across all
industries is around \$5-7, we can estimate the revenue sharing for the
top 10 channels with the highest total views on YouTube.

Ref: [How Much Money Do You Get Per View on YouTube? (2022
Stats)](https://www.thinkific.com/blog/youtube-money-per-view/)

``` r
## Paid by views of each channel
# Fig 06 - All category top 10 of each year
CAchannel_pay <- CA_data %>%
  group_by(trending_year, channelTitle) %>%
  summarise(view_sum = sum(view_count),
            USD_pay_in_k = (view_sum /1000 * 5)/100 ) %>%
  arrange(desc(USD_pay_in_k)) %>%
  slice(1:10)
```

    ## `summarise()` has grouped output by 'trending_year'. You can override using the
    ## `.groups` argument.

``` r
ggplot(CAchannel_pay, aes(y = channelTitle, x = USD_pay_in_k, fill = view_sum)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = 'Amount of top view channel got paid',
       x = 'USD (per 1000 dollars)',
       y = 'Category')
```

![Figure 6](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgLxls1NteJJUClE8oGH7lfXguTZfXN8DBIdUcNzuwpo3QzSLPBFulD0S1t43bT_7fgSGfls3ZaSFJf4SfA4oWnagAojSy942cs7HBt-zNwAYybmpLRdw87JMNJblCHhDC-T8ZSNHYqExLP_CBZh6O9NOhcBZy5qCmCE7FQ7Rfzxx3ijidMWljzo8_P/s16000/p06_channels_paid.png)<!-- -->

As expected, higher views are generally correlated with higher revenue
sharing.

### 3-2. Channels with excluded categories

Since channels with high views are typically related to music, movies,
sports, and TV shows, we may exclude those channels to see which types
of channels are more likely to have higher views or appear on the
trending list.

``` r
# list of commercials to be excluded
comm_ch_list <- c("HYBE LABELS", "starshipTV", "Stone Music Entertainment",
                  "NPR Music", "Navrattan Music", "Strange Music Inc", "Sony Music India",
                  "Zee Music Company", "Desi Music Factory", "MTV",
               "Paramount Pictures", "Warner Bros. Pictures", "Sony Pictures Entertainment",
               "Universal Pictures", "Magnolia Pictures & Magnet Releasing",
               "Orion Pictures", "Marvel Entertainment", "JYP Entertainment", "Big Hit Labels",
               "Apple", "amazon",  "T-Mobile", "Samsung Mobile USA", "Google")

comm_title_list <- c("Music Video", "Official Video", "Official Music Video", "Official Lyric Video",
                  "Official Audio", "Oficial", "Official MV", "Official Teaser", "Shorts", "shorts",
                  "SHORTS", "Trailer", "TRAILER", "Teaser Video", "TEASER", "M/V", "MV")

## Exclude music, films, sports, etc
CA_nocomm <- CA_data %>%
  filter(categoryId != 2 & categoryId != 17 & categoryId != 25 & categoryId != 43) %>%
  filter(!grepl(paste(comm_title_list, collapse = "|"), title)) %>%
  filter(!grepl(paste(comm_ch_list, collapse = "|"), channelTitle)) %>%
  filter(!grepl('VEVO|Vevo', channelTitle))
```

### 3-2-1. Highest view

After excluding commercial videos, which categories have the highest
number of views?

``` r
# Non-coomm videos of top 100 videos of each year
# total = 300 videos
CA_nocomm_top100_year <- CA_nocomm %>%
  group_by(trending_year) %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# Fig 07 - Category of highest views
CA_nocomm_top100_catyear <- CA_nocomm_top100_year %>%
  group_by(category, trending_year) %>%
  summarise(n_per_cat_views = n(),
            avg_percat_views = sum(view_count) / n_per_cat_views) %>%
  as.data.frame()
```

    ## `summarise()` has grouped output by 'category'. You can override using the
    ## `.groups` argument.

``` r
ggplot(CA_nocomm_top100_catyear, aes(y = category, x = n_per_cat_views, fill = avg_percat_views)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = 'No. times of trending of Canadian YouTube videos with top 100 higest views',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = 'No. times on Trending',
       y = 'Category',
       fill = "Avg views")
```

![Figure 7](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEjt4xv97bKo4kpkmu11eas866j4XXXZTNTVWNyTb7c8lZ029m5mKsVnisKLMB2UDRtnowUsKiubbYcN-LOUigpEJy3mehC5LQkPTLH_b4y6Op5ZkpBnjecqGdkk5__FAa0hYW2nsfmRueB4F6qaYWI5YklWBpxH51GHIVkz3NYLcudYDFhtWxrpibiC/s16000/p07_ex-category-top100views.png)<!-- -->

Entertainment has consistently had the highest views from 2020 to the
present.

We have selected the top 100 viewed videos from 2020 to the present and
examined their distribution across different channels and years.

``` r
# Top 100 highest views of total excl. videos
# total = 100 videos in 3 years
CA_nocomm_top100_view <-CA_nocomm %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# Fig 08 - Channels of top 100 views videos
# exclude shorts & commercials
ggplot(CA_nocomm_top100_view, aes(y = channelTitle, x = view_count, alpha = likes, color = trending_year)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_grid(. ~trending_year) +
  theme_linedraw(base_family = "NanumGothic") +
  labs(title = 'Top 100 Canadian YouTube Channels with highest views',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = '# of views',
       y = 'YouTube channels',
       color = "Trending year")
```

![Figure 8](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEjzavJq5O3FlreV9LhAkB2puTrOY3xyCJiSxcei8RQSUXZUuHA-Nxbh-nPOt0J7A-aOCDmONEmbpv_sIcCMeOpKh8THUN5jdS3pGvMeFdIN-MfuW-1T-yHehbxAY7_d6TbMVzTBFdvmsm76vnVuhWHqmekXU5JQv3P5X80Tou6RNlLkrkeRJKAy_pCm/s16000/p08_ex-channel-top100views.png)<!-- -->

The top 100 most viewed videos from 2020 to the present are mainly from
four channels: YRF, MrBeast, Klem Family, and Bizrrap. YRF’s popularity
increased in 2022, and Bizrrap has seen a significant rise this year in
both Canada and ths States. However, Klem Family, despite being very
popular in 2021, has fallen out of the top 100 since 2022.

Next, let’s examine the top 45 most viewed videos for each year.

``` r
# Fig 09 -  Non-comm videos of top 45 highest views (separated by year)
# total = 135 videos # exclude shorts & commercials
CA_nocomm_top50_year <- CA_nocomm %>%
  group_by(trending_year) %>%
  arrange(desc(view_count)) %>%
  slice(1:45)

# exclude shorts & commercials
ggplot(CA_nocomm_top50_year, aes(y = channelTitle, x = view_count, alpha = likes, color = trending_year)) +
  geom_point() +
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  facet_grid(. ~trending_year) +
  theme_linedraw(base_family = "NanumGothic") +
  labs(title = 'Top 45 Canadian YouTube Channels with highest views of each year',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = '# of views',
       y = 'YouTube channels',
       color = "Trending year")
```

![Figure 9](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEiuKFrkDwbngu61esllrYF0vsfPIz6q02H1bZtpG1UfLZy3NcA6IoxTSwEeYuAwe4cav63Xm-HG2hvnzRBY19N9m3_uS7TL90K7upPQhlbEISGKWpiLK1lghj4Ryu6ZstfzzMUK_4uK_wXF941T8riOeNs629WyCO5TRxxv9XGwsWgRtmD3UIXZ5fSA/s16000/p09_ex-video-top45views.png)<!-- -->

Popular channels, except for MrBeast, have a tendency to change each
year, with most experiencing a surge in popularity one year followed by
a decline in the next.

Next, we have selected the top 100 videos with the highest views and
sorted them based on the number of times they appeared on the trending
list.

``` r
# Top 100 highest views of total excl. videos
# total = 100 videos in 3 years
CA_nocomm_top100_view <-CA_nocomm %>%
  arrange(desc(view_count)) %>%
  slice(1:100)

# Videos of top 100 highest views
# No. times on trending
CA_nocomm_top100_view_ntrending <- CA_nocomm_top100_view %>%
  group_by(title) %>%
  summarise(channelTitle = first(channelTitle),
            v_ntrending = n(),
            sum_view = sum(view_count)) %>%
  arrange(desc(v_ntrending))

head(CA_nocomm_top100_view_ntrending)
```

    ## # A tibble: 6 × 4
    ##   title                                                  chann…¹ v_ntr…² sum_v…³
    ##   <chr>                                                  <chr>     <int>   <dbl>
    ## 1 I Survived 50 Hours In Antarctica                      MrBeast       9  5.54e8
    ## 2 Beach Money Ball!!💵🌊💵                               Klem F…       7  8.60e8
    ## 3 Jhoome Jo Pathaan Song | Shah Rukh Khan, Deepika | Vi… YRF           7  4.77e8
    ## 4 SHAKIRA || BZRP Music Sessions #53                     Bizarr…       7  8.13e8
    ## 5 $456,000 Squid Game In Real Life!                      MrBeast       6  6.29e8
    ## 6 so long nerds                                          Techno…       6  3.65e8
    ## # … with abbreviated variable names ¹​channelTitle, ²​v_ntrending, ³​sum_view

We can see that the number of views does not necessarily correlate with
the frequency of appearing on the trending list.

#### 3-2-3. Paid by ad views

Lastly, let’s examine the revenue sharing for the top 10 channels with
the highest total views on YouTube.

``` r
## Paid by views of each channel
# Fig 10 - excludes commercials
CA_nocomm_paytop20 <- CA_nocomm %>%
  group_by(trending_year, channelTitle) %>%
  summarise(view_sum = sum(view_count),
            USD_pay_in_k = (view_sum /1000 * 5)/100 ) %>%
  arrange(desc(USD_pay_in_k)) %>%
  slice(1:10)
```

    ## `summarise()` has grouped output by 'trending_year'. You can override using the
    ## `.groups` argument.

``` r
ggplot(CA_nocomm_paytop20, aes(y = channelTitle, x = USD_pay_in_k, fill = view_sum)) +
  geom_col() +
  facet_grid(. ~trending_year) +
  scale_fill_gradient(low = "light blue", high = "dark blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(title = 'Canadian YouTube channels earned (per 1000 views)',
       subtitle = 'Film, music, TV channels, commercials, shorts are excluded',
       x = 'USD (per 1000 dollars)',
       y = 'Category',
       fill = "Total views")
```

![Figure 10](https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEjQyASHmFu1l-vDsq4jje3aYr1jlLKwuTo4DJQzg_P-qUj8B5n0I79eYUGSsH9QbcjfkuJcsGwPHmWzRdSCTvPZjngEZEl9Enw8f394KWpkKa3uTX-0W3ofGwVZfeeBlyfviYK-R9I2FzuAEXbexq5k4etdADoF-UP1GsVb6JLkwthS_x4nKhbO1aY6/s16000/p10_ex-channels-paid.png)<!-- -->

Same as in the US, MrBeast has had the highest revenue sharing in Canada
since 2020. Apart from that, the top 10 channels with the highest
revenue sharing vary for each year.

### Summary

The most popular channels and videos on YouTube are mainly sports and
commercial ones, such as official music videos and movie trailers, as
well as the rising trend of shorts, which have gained a lot of attention
in recent years. It’s worth noting that most shorts are from TikTok.

Once we exclude sports channels, commercial videos, and shorts, we can
see more diverse personal channels rising in popularity. MrBeast has
been a popular content creator in both Canada and the United States for
several years, but consistently staying on the trending list like
MrBeast is a challenging feat. We can observe that many channels appear
on the trending list one year, but disappear the next.

Overall, the YouTube trending lists in Canada and the US are quite
similar. However, the popular channels and creators that appear on these
lists may be more diverse in Canada.

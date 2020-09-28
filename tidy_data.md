Tidy Data
================

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
options(tibble.print_min = 5)
```

Loading and cleaning the dataset.

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names()
pulse_data
```

    ## # A tibble: 1,087 x 7
    ##      id   age sex   bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##   <dbl> <dbl> <chr>        <dbl>         <dbl>         <dbl>         <dbl>
    ## 1 10003  48.0 male             7             1             2             0
    ## 2 10015  72.5 male             6            NA            NA            NA
    ## 3 10022  58.5 male            14             3             8            NA
    ## 4 10026  72.7 male            20             6            18            16
    ## 5 10035  60.4 male             4             0             1             2
    ## # ... with 1,082 more rows

Using pivot longer: wide-to-long.

``` r
pulse_data = 
  haven::read_sas("./data/public_pulse_data.sas7bdat") %>%
  janitor::clean_names() %>%
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit", 
    names_prefix = "bdi_score_",
    values_to = "bdi") %>%
  relocate(visit) %>%
  mutate(
    visit = replace(visit, visit == "bl", "00m"),
    visit = factor(visit, levels = str_c(c("00", "01", "06", "12"), "m"))) %>%
  arrange(id, visit)

print(pulse_data, n = 12)
```

    ## # A tibble: 4,348 x 5
    ##    visit    id   age sex     bdi
    ##    <fct> <dbl> <dbl> <chr> <dbl>
    ##  1 00m   10003  48.0 male      7
    ##  2 01m   10003  48.0 male      1
    ##  3 06m   10003  48.0 male      2
    ##  4 12m   10003  48.0 male      0
    ##  5 00m   10015  72.5 male      6
    ##  6 01m   10015  72.5 male     NA
    ##  7 06m   10015  72.5 male     NA
    ##  8 12m   10015  72.5 male     NA
    ##  9 00m   10022  58.5 male     14
    ## 10 01m   10022  58.5 male      3
    ## 11 06m   10022  58.5 male      8
    ## 12 12m   10022  58.5 male     NA
    ## # ... with 4,336 more rows

``` r
litters_data = read_csv("./data/FAS_litters.csv") %>% 
  janitor::clean_names() %>% 
  select(litter_number, gd0_weight, gd18_weight) %>% pivot_longer(
    gd0_weight:gd18_weight,
    names_to = "gd",
    values_to = "weight") %>% 
  relocate(gd, weight) %>% 
  mutate(gd = recode(gd, "gd0_weight" = 0, "gd18_weight" = 18))
```

    ## Parsed with column specification:
    ## cols(
    ##   Group = col_character(),
    ##   `Litter Number` = col_character(),
    ##   `GD0 weight` = col_double(),
    ##   `GD18 weight` = col_double(),
    ##   `GD of Birth` = col_double(),
    ##   `Pups born alive` = col_double(),
    ##   `Pups dead @ birth` = col_double(),
    ##   `Pups survive` = col_double()
    ## )

Tidy to un-tidy.

``` r
analysis_result = tibble(
  group = c("treatment", "treatment", "placebo", "placebo"),
  time = c("pre", "post", "pre", "post"),
  mean = c(4, 8, 3.5, 4)
)

pivot_wider(
  analysis_result, 
  names_from = "time", 
  values_from = "mean")
```

    ## # A tibble: 2 x 3
    ##   group       pre  post
    ##   <chr>     <dbl> <dbl>
    ## 1 treatment   4       8
    ## 2 placebo     3.5     4

``` r
fellowship_ring = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")

two_towers = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>%
  mutate(movie = "two_towers")

return_king = 
  readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>%
  mutate(movie = "return_king")
```

``` r
lotr_tidy = 
  bind_rows(fellowship_ring, two_towers, return_king) %>%
  janitor::clean_names() %>%
  pivot_longer(
    female:male,
    names_to = "gender", 
    values_to = "words") %>%
  mutate(race = str_to_lower(race)) %>% 
  select(movie, everything()) 

lotr_tidy
```

    ## # A tibble: 18 x 4
    ##    movie           race   gender words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring elf    male     971
    ##  3 fellowship_ring hobbit female    14
    ##  4 fellowship_ring hobbit male    3644
    ##  5 fellowship_ring man    female     0
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      elf    male     513
    ##  9 two_towers      hobbit female     0
    ## 10 two_towers      hobbit male    2463
    ## 11 two_towers      man    female   401
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     elf    male     510
    ## 15 return_king     hobbit female     2
    ## 16 return_king     hobbit male    2673
    ## 17 return_king     man    female   268
    ## 18 return_king     man    male    2459

Joining pups and litter data.

``` r
pup_data = 
  read_csv("./data/FAS_pups.csv", col_types = "ciiiii") %>%
  janitor::clean_names() %>%
  mutate(sex = recode(sex, `1` = "male", `2` = "female")) 

litter_data = 
  read_csv("./data/FAS_litters.csv", col_types = "ccddiiii") %>%
  janitor::clean_names() %>%
  separate(group, into = c("dose", "day_of_tx"), sep = 3) %>%
  relocate(litter_number) %>%
  mutate(
    wt_gain = gd18_weight - gd0_weight,
    dose = str_to_lower(dose))

fas_data = 
  left_join(pup_data, litter_data, by = "litter_number")

fas_data
```

    ## # A tibble: 313 x 15
    ##   litter_number sex   pd_ears pd_eyes pd_pivot pd_walk dose  day_of_tx
    ##   <chr>         <chr>   <int>   <int>    <int>   <int> <chr> <chr>    
    ## 1 #85           male        4      13        7      11 con   7        
    ## 2 #85           male        4      13        7      12 con   7        
    ## 3 #1/2/95/2     male        5      13        7       9 con   7        
    ## 4 #1/2/95/2     male        5      13        8      10 con   7        
    ## 5 #5/5/3/83/3-3 male        5      13        8      10 con   7        
    ## # ... with 308 more rows, and 7 more variables: gd0_weight <dbl>,
    ## #   gd18_weight <dbl>, gd_of_birth <int>, pups_born_alive <int>,
    ## #   pups_dead_birth <int>, pups_survive <int>, wt_gain <dbl>

Learning Assessment

``` r
survey_os = 
  read_csv("./data_1/surv_os.csv") %>% 
  janitor::clean_names() %>% 
  rename(
    uni = what_is_your_uni, 
    os = what_operating_system_do_you_use)
```

    ## Parsed with column specification:
    ## cols(
    ##   `What is your UNI?` = col_character(),
    ##   `What operating system do you use?` = col_character()
    ## )

``` r
survey_os
```

    ## # A tibble: 173 x 2
    ##   uni         os        
    ##   <chr>       <chr>     
    ## 1 student_87  <NA>      
    ## 2 student_106 Windows 10
    ## 3 student_66  Mac OS X  
    ## 4 student_93  Windows 10
    ## 5 student_99  Mac OS X  
    ## # ... with 168 more rows

``` r
survey_pr_git = read_csv("data_1/surv_program_git.csv") %>% 
  janitor::clean_names() %>% 
  rename(
    uni = what_is_your_uni, 
    degree = what_is_your_degree_program,
    git = which_most_accurately_describes_your_experience_with_git)
```

    ## Parsed with column specification:
    ## cols(
    ##   `What is your UNI?` = col_character(),
    ##   `What is your degree program?` = col_character(),
    ##   `Which most accurately describes your experience with Git?` = col_character()
    ## )

``` r
survey_pr_git
```

    ## # A tibble: 135 x 3
    ##   uni        degree git                                                         
    ##   <chr>      <chr>  <chr>                                                       
    ## 1 student_1~ MS     Pretty smooth: needed some work to connect Git, GitHub, and~
    ## 2 student_32 MS     Not smooth: I don't like git, I don't like GitHub, and I do~
    ## 3 <NA>       MPH    Pretty smooth: needed some work to connect Git, GitHub, and~
    ## 4 student_1~ MPH    Not smooth: I don't like git, I don't like GitHub, and I do~
    ## 5 student_17 PhD    Pretty smooth: needed some work to connect Git, GitHub, and~
    ## # ... with 130 more rows

``` r
left_join(survey_os, survey_pr_git, by = "uni")
```

    ## # A tibble: 175 x 4
    ##   uni       os        degree git                                                
    ##   <chr>     <chr>     <chr>  <chr>                                              
    ## 1 student_~ <NA>      MS     Pretty smooth: needed some work to connect Git, Gi~
    ## 2 student_~ Windows ~ Other  Pretty smooth: needed some work to connect Git, Gi~
    ## 3 student_~ Mac OS X  MPH    Smooth: installation and connection with GitHub wa~
    ## 4 student_~ Windows ~ MS     Smooth: installation and connection with GitHub wa~
    ## 5 student_~ Mac OS X  MS     Smooth: installation and connection with GitHub wa~
    ## # ... with 170 more rows

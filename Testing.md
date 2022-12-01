Testing
================

``` r
walkability_df = read_csv("./data/EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv")
```

    ## Rows: 220740 Columns: 117
    ## ── Column specification ─────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): CSA_Name, CBSA_Name
    ## dbl (115): OBJECTID, GEOID10, GEOID20, STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
walkability_new = walkability_df %>% 
  janitor::clean_names() %>% 
  mutate(statefp = str_pad(statefp, width = 2, pad = "0"),
         countyfp = str_pad(countyfp, width = 3, pad = "0"),
         tractce = str_pad(tractce, width = 6, pad = "0"),
         fips = str_c(statefp, countyfp, tractce, blkgrpce, sep = "")) %>% 
  select(fips, everything(), -objectid)
  
walkability_clean = walkability_new %>% 
  select(fips, statefp, countyfp, tractce, blkgrpce, nat_walk_ind, csa_name) %>% 
  mutate(fips_noblk = str_c(statefp, countyfp, tractce, sep = ""))
```

``` r
dep_df = read_csv("./data/PLACES__Local_Data_for_Better_Health__Census_Tract_Data_2021_release.csv")
```

    ## Rows: 2161543 Columns: 23
    ## ── Column specification ─────────────────────────────────────────
    ## Delimiter: ","
    ## chr (16): StateAbbr, StateDesc, CountyName, CountyFIPS, LocationName, DataSo...
    ## dbl  (4): Year, Data_Value, Low_Confidence_Limit, High_Confidence_Limit
    ## lgl  (2): Data_Value_Footnote_Symbol, Data_Value_Footnote
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
dep_new = dep_df %>% 
  janitor::clean_names() %>% 
  mutate(location_name = str_pad(location_name, width = 11, pad = "0")) %>%
  select(location_name, state_desc, county_name, data_value_type, data_value)
```

``` r
try_merge_withdep = merge(x = walkability_clean, y = dep_new, by.x = "fips_noblk", by.y = "location_name")

try_merge_withdep %>% 
  lm(data_value ~ nat_walk_ind, data = .) %>% 
  broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term         estimate std.error statistic p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>   <dbl>
    ## 1 (Intercept)    33.1     0.0251     1319.        0
    ## 2 nat_walk_ind   -0.166   0.00237     -69.9       0

``` r
copd_df = read_csv("./data/County_COPD_prevalence.csv")
```

    ## Rows: 3144 Columns: 7
    ## ── Column specification ─────────────────────────────────────────
    ## Delimiter: ","
    ## chr (6): LocationID, Public_Health_Jurisdiction, StateDesc, County, 95% Conf...
    ## dbl (1): Percent_COPD
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
longFIPS <- dep_df %>% select(CountyFIPS, LocationID)

copd_new <- merge(x = longFIPS, y = copd_df, by.x = "CountyFIPS", by.y = "LocationID") %>% 
  mutate(fips_noblk = LocationID) %>% 
  select (-c(CountyFIPS, Public_Health_Jurisdiction, LocationID))

try_merge_withcopd <- merge(x = walkability_clean, y = copd_new, by = "fips_noblk", all.x=TRUE, all.y=TRUE)
```

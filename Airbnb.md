
``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.1     v purrr   0.3.2
    ## v tibble  2.1.3     v dplyr   0.8.3
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)
library(readr)
```

``` r
HongKong = read_csv('Hongkong.csv')
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   thumbnail_url = col_logical(),
    ##   medium_url = col_logical(),
    ##   xl_picture_url = col_logical(),
    ##   host_id = col_double(),
    ##   host_is_superhost = col_logical(),
    ##   host_listings_count = col_double(),
    ##   host_total_listings_count = col_double(),
    ##   host_has_profile_pic = col_logical(),
    ##   host_identity_verified = col_logical(),
    ##   neighbourhood_group_cleansed = col_logical(),
    ##   latitude = col_double(),
    ##   longitude = col_double(),
    ##   is_location_exact = col_logical(),
    ##   accommodates = col_double(),
    ##   bathrooms = col_double(),
    ##   bedrooms = col_double(),
    ##   beds = col_double(),
    ##   square_feet = col_double(),
    ##   guests_included = col_double(),
    ##   minimum_nights = col_double()
    ##   # ... with 33 more columns
    ## )

    ## See spec(...) for full column specifications.

    ## Warning: 2 parsing failures.
    ##   row     col           expected    actual           file
    ##  8738 license 1/0/T/F/TRUE/FALSE wwhostel  'Hongkong.csv'
    ## 10253 license 1/0/T/F/TRUE/FALSE 18-618219 'Hongkong.csv'

``` r
names(HongKong)
```

    ##   [1] "name"                                        
    ##   [2] "summary"                                     
    ##   [3] "space"                                       
    ##   [4] "description"                                 
    ##   [5] "experiences_offered"                         
    ##   [6] "neighborhood_overview"                       
    ##   [7] "notes"                                       
    ##   [8] "transit"                                     
    ##   [9] "access"                                      
    ##  [10] "interaction"                                 
    ##  [11] "house_rules"                                 
    ##  [12] "thumbnail_url"                               
    ##  [13] "medium_url"                                  
    ##  [14] "picture_url"                                 
    ##  [15] "xl_picture_url"                              
    ##  [16] "host_id"                                     
    ##  [17] "host_url"                                    
    ##  [18] "host_name"                                   
    ##  [19] "host_since"                                  
    ##  [20] "host_location"                               
    ##  [21] "host_about"                                  
    ##  [22] "host_response_time"                          
    ##  [23] "host_response_rate"                          
    ##  [24] "host_acceptance_rate"                        
    ##  [25] "host_is_superhost"                           
    ##  [26] "host_thumbnail_url"                          
    ##  [27] "host_picture_url"                            
    ##  [28] "host_neighbourhood"                          
    ##  [29] "host_listings_count"                         
    ##  [30] "host_total_listings_count"                   
    ##  [31] "host_verifications"                          
    ##  [32] "host_has_profile_pic"                        
    ##  [33] "host_identity_verified"                      
    ##  [34] "street"                                      
    ##  [35] "neighbourhood"                               
    ##  [36] "neighbourhood_cleansed"                      
    ##  [37] "neighbourhood_group_cleansed"                
    ##  [38] "city"                                        
    ##  [39] "state"                                       
    ##  [40] "zipcode"                                     
    ##  [41] "market"                                      
    ##  [42] "smart_location"                              
    ##  [43] "country_code"                                
    ##  [44] "country"                                     
    ##  [45] "latitude"                                    
    ##  [46] "longitude"                                   
    ##  [47] "is_location_exact"                           
    ##  [48] "property_type"                               
    ##  [49] "room_type"                                   
    ##  [50] "accommodates"                                
    ##  [51] "bathrooms"                                   
    ##  [52] "bedrooms"                                    
    ##  [53] "beds"                                        
    ##  [54] "bed_type"                                    
    ##  [55] "amenities"                                   
    ##  [56] "square_feet"                                 
    ##  [57] "price"                                       
    ##  [58] "weekly_price"                                
    ##  [59] "monthly_price"                               
    ##  [60] "security_deposit"                            
    ##  [61] "cleaning_fee"                                
    ##  [62] "guests_included"                             
    ##  [63] "extra_people"                                
    ##  [64] "minimum_nights"                              
    ##  [65] "maximum_nights"                              
    ##  [66] "minimum_minimum_nights"                      
    ##  [67] "maximum_minimum_nights"                      
    ##  [68] "minimum_maximum_nights"                      
    ##  [69] "maximum_maximum_nights"                      
    ##  [70] "minimum_nights_avg_ntm"                      
    ##  [71] "maximum_nights_avg_ntm"                      
    ##  [72] "calendar_updated"                            
    ##  [73] "has_availability"                            
    ##  [74] "availability_30"                             
    ##  [75] "availability_60"                             
    ##  [76] "availability_90"                             
    ##  [77] "availability_365"                            
    ##  [78] "calendar_last_scraped"                       
    ##  [79] "number_of_reviews"                           
    ##  [80] "number_of_reviews_ltm"                       
    ##  [81] "first_review"                                
    ##  [82] "last_review"                                 
    ##  [83] "review_scores_rating"                        
    ##  [84] "review_scores_accuracy"                      
    ##  [85] "review_scores_cleanliness"                   
    ##  [86] "review_scores_checkin"                       
    ##  [87] "review_scores_communication"                 
    ##  [88] "review_scores_location"                      
    ##  [89] "review_scores_value"                         
    ##  [90] "requires_license"                            
    ##  [91] "license"                                     
    ##  [92] "jurisdiction_names"                          
    ##  [93] "instant_bookable"                            
    ##  [94] "is_business_travel_ready"                    
    ##  [95] "cancellation_policy"                         
    ##  [96] "require_guest_profile_picture"               
    ##  [97] "require_guest_phone_verification"            
    ##  [98] "calculated_host_listings_count"              
    ##  [99] "calculated_host_listings_count_entire_homes" 
    ## [100] "calculated_host_listings_count_private_rooms"
    ## [101] "calculated_host_listings_count_shared_rooms" 
    ## [102] "reviews_per_month"

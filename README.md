# Roche_RVA_assessment

This is Isaac Kuk's submission for the Roche RVA Specialist Coding Assessment. For Questions 1 and 2, the folders contain both an R script to generate the desired output, as well as an example of the output file. For Question 3, the folder contains the entire Shiny app.

## Repo structure
├── question_1
│   ├── question_1.R
│   └── sample_output_1.html
├── question_2
│   ├── question_2.R
│   └── sample_output_2.png
├── question_3
│   └── question_3.R

## Required R packages:
- `pharmaverseadam`
- `tidyverse`
- `gtsummary`
- `ggplot2`
- `shiny`

## Tested R and package versions:

All code was tested on the following R version using the following package versions:

```
R version 4.5.2 (2025-10-31)
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.04.4 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0

locale:
 [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8        LC_COLLATE=C.UTF-8    
 [5] LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8    LC_PAPER=C.UTF-8       LC_NAME=C             
 [9] LC_ADDRESS=C           LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] lubridate_1.9.5       forcats_1.0.1         stringr_1.6.0         dplyr_1.2.0           purrr_1.2.1          
 [6] readr_2.2.0           tidyr_1.3.2           tibble_3.3.1          ggplot2_4.0.2         tidyverse_2.0.0      
[11] pharmaverseadam_1.3.0 shiny_1.13.0         

loaded via a namespace (and not attached):
 [1] sass_0.4.10        generics_0.1.4     stringi_1.8.7      hms_1.1.4          digest_0.6.39     
 [6] magrittr_2.0.4     grid_4.5.2         timechange_0.4.0   RColorBrewer_1.1-3 fastmap_1.2.0     
[11] jsonlite_2.0.0     promises_1.5.0     scales_1.4.0       textshaping_1.0.5  jquerylib_0.1.4   
[16] cli_3.6.5          rlang_1.1.7        withr_3.0.2        cachem_1.1.0       otel_0.2.0        
[21] tools_4.5.2        tzdb_0.5.0         memoise_2.0.1      httpuv_1.6.16      vctrs_0.7.1       
[26] R6_2.6.1           mime_0.13          lifecycle_1.0.5    ragg_1.5.1         pkgconfig_2.0.3
```
[31] pillar_1.11.1      bslib_0.10.0       later_1.4.8        gtable_0.3.6       glue_1.8.0        
[36] Rcpp_1.1.1         systemfonts_1.3.2  tidyselect_1.2.1   rstudioapi_0.18.0  farver_2.1.2      
[41] xtable_1.8-8       htmltools_0.5.9    labeling_0.4.3     compiler_4.5.2     S7_0.2.1  

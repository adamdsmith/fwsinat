
[![Build Status](https://travis-ci.org/adamdsmith/fwsinat.png)](https://travis-ci.org/adamdsmith/fwsinat)

    ## Warning: package 'dplyr' was built under R version 3.4.1

USFWS Disclaimer
================

This United States Fish & Wildlife Service (USFWS) code is provided on an "as is" basis and the user assumes responsibility for its use. USFWS has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recomendation or favoring by USFWS. The USFWS seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by USFWS or the United States Government.

Installing `fwsinat`
====================

The `fwsinat` package requires you to have [R](https://www.r-project.org/) (&gt;= 3.3) installed on your computer as well as [Rtools](https://cran.r-project.org/bin/windows/Rtools/). Both will require administrative priveleges but the installation of packages after this initial install will not.

With R and Rtools installed, it's simple to install and load the `fwsinat` package to access its functionality. If you receive an SSL or CA Certificate error, you may need to take the extra step documented below.

    # If devtools package is not installed
    install.packages("devtools", dependencies = TRUE)

    # Now install and load fwsinat
    devtools::install_github("adamdsmith/fwsinat")
    library("fwsinat")

    # If you receive a SSL or CA Certificate error
    install.packages("httr")
    library("httr")
    set_config(config(ssl_verifypeer = 0L))
    devtools::install_github("adamdsmith/fwsinat")
    library("fwsinat")

The `fwsinat` package
=====================

This packages currently contains functions to:

1.  retrieve iNaturalist observations for any number of available USFWS properties (`retrieve_inat`);
2.  update previously retrievals of observations (`update`);
3.  export the observations to separate spreadsheets by USFWS property for distribution (`export_inat`); and
4.  harvest observations on USFWS properties into the [USFWS NWRS iNaturalist project](https://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system) (`harvest_inat`)

Using `fwsinat`
===============

The first step to using `fwsinat` is to generate a list of USFWS properties from which you'd like to retrieve observations. This can be done using the `find_refuges` function, which accepts as input a vector of strings you expect will return your desired refuge(s). The `find_refuges` function is case-insensitive and works with partial matches. For example, if we're interested in retrieving observations on Merritt Island, Loxahatchee, and Chassahowitzka NWRs, we can input a vector of strings we think are adequate to return those refuges:

``` r
refs <- c("merritt", "lox", "chass")
(refs <- find_refuges(refs))
```

    ## [1] "MERRITT ISLAND NATIONAL WILDLIFE REFUGE"                
    ## [2] "ARTHUR R. MARSHALL LOXAHATCHEE NATIONAL WILDLIFE REFUGE"
    ## [3] "CHASSAHOWITZKA NATIONAL WILDLIFE REFUGE"

Success! Note that some searches will return multiple matches that you may not be seeking. If we're interested in Hatchie NWR, for example:

``` r
(hatchie <- find_refuges("hatchie"))
```

    ## [1] "HATCHIE NATIONAL WILDLIFE REFUGE"      
    ## [2] "LOWER HATCHIE NATIONAL WILDLIFE REFUGE"
    ## [3] "TALLAHATCHIE NATIONAL WILDLIFE REFUGE"

Three matches! No worries! We can either select the one we want *ex post facto* or use regular expressions to narrow the matches:

``` r
## Narrow the search after the fact
(hatchie <- hatchie[1])
```

    ## [1] "HATCHIE NATIONAL WILDLIFE REFUGE"

``` r
# Narrow the search with regular expressions
# In this case, require the property to start with "Hatchie"
(hatchie <- find_refuges("^hatchie"))
```

    ## [1] "HATCHIE NATIONAL WILDLIFE REFUGE"

There are a couple more options to the `find_refuges` function to help you narrow your search; use `?find_refuges` for more information. By default, `find_refuges` returns **ALL** available USFWS properties.

Now let's pick a refuge and retrieve the observations from iNaturalist... `retrieve_inat` is our friend. Before we do so, however, we need to decide on a few options:

1.  Do we want all iNaturalist observations on the property or only those from a specific project?

By default, `retrieve_inat` returns only observations associated with the [USFWS National Wildlife Refuge System iNaturalist project](https://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system). If you want a different project, you'll need to specify which one with the `inat_proj` argument (see `?retrieve_inat` for guidance on what's expected). If you want **ALL** observations, use `inat_proj = NULL` (see below).

1.  Do we want to limit the dates of those observations?

By default, `retrieve_inat` does not limit retrievals by date range. See `?retrieve_inat` for guidance if this is of interest.

1.  Do we want helpful messages during the retrieval?

Generally these are useful to track progress, so the default is to print these messages. If they annoy you, you can reduce them to a minumum by passing `verbose = FALSE` to `retrieve_inat`.

``` r
musc <- find_refuges("musc")

# Get observations on Muscatatuck NWR only from the USFWS NWRS project
muscatatuck <- retrieve_inat(musc)
```

    ## Processing Muscatatuck National Wildlife Refuge within the
    ## usfws-national-wildlife-refuge-system project.

    ## Retrieving 12 records.

    ## Records retrieved: 
    ##   0-12

``` r
# Get all observations on Muscatatuck NWR
musc_all <- retrieve_inat(musc, inat_proj = NULL)
```

    ## Processing Muscatatuck National Wildlife Refuge across all
    ## iNaturalist projects.

    ## Retrieving 25 records.

    ## Records retrieved: 
    ##   0-25

In this case, we've retrieved observations only for a single refuge but we could just as easily retrieved from multiple refuges:

``` r
in_ky <- find_refuges(c("patoka river", "musc", "big oaks", "clarks river"))

# Get all observations from Indiana and Kentucky refuges
in_ky <- retrieve_inat(in_ky, inat_proj = NULL)
```

From our Muscatatuck example, we notice there are 13 observations on the refuge that do not belong to the USFWS NWRS project. We want those observations! We can try and harvest them with `harvest_inat`, although this will require you to have registered with []() and to pass your username and password. It may be best to leave this to USFWS NWRS project administrators. We illustrate it here with hidden credentials to prove the point, however.

``` r
out <- harvest_inat(musc, user = Sys.getenv("user"), pw = Sys.getenv("pw"), interactive = FALSE)
```

    ## 13 observations available for harvest on Muscatatuck National Wildlife Refuge.

Occasionally observations cannot be harvested because the user restricts their observations. If this happens, you will recieve a message indicating how many records could not be harvested, and you can explore details of those observations by printing the object you just created. In this case, that object was called `out` and it is empty because all 13 observations were successfully harvested.

``` r
out
```

    ## [1] orgname        observation_id http_error     error_msg     
    ## [5] user          
    ## <0 rows> (or 0-length row.names)

``` r
# USFWS NWRS project now contains all observations
muscatatuck <- retrieve_inat(musc)
```

    ## Processing Muscatatuck National Wildlife Refuge within the
    ## usfws-national-wildlife-refuge-system project.

    ## Retrieving 25 records.

    ## Records retrieved: 
    ##   0-25

It would be handy to update these observations periodically, and the `update` function provides this option. For example, to update our Muscatuck records we would run `update` on the created `muscatuck` object:

``` r
muscatatuck <- update(muscatatuck)
```

    ## No updates available.

In this case, no observations had been updated (e.g., another iNaturalist user had suggested an identification) and no new observation had been added. This isn't surprising given the short period between retrieval and update. Normally you'll want to save the record of observations locally and then update them some time later. For example:

``` r
saveRDS(muscatatuck, file = "SOME/PATH/TO/muscatatuck.rds")

# Wait a few weeks or months and...
muscatatuck <- readRDS("SOME/PATH/TO/muscatatuck.rds")
muscatatuck <- update(muscatatuck)
```

Lastly, at least in Region 4, we want to generate refuge-specific spreadsheets for distribution to the refuges so they have an updated record of biota observed on the property and, if so desired, they can suggest identifications for observations that may not have the desired level of specificity. We generate these spreadsheets with the `export_inat` function. You specify the output directory and `export_inat` generates an output spreadsheet there for each property contained in the input `fwsinat` object.

``` r
export_inat(muscatatuck, dir = "C:/temp/test_export")
```

    ## Processing Muscatatuck National Wildlife Refuge...  Export successful.

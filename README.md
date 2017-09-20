
[![Build Status](https://travis-ci.org/adamdsmith/fwsinat.png)](https://travis-ci.org/adamdsmith/fwsinat)

USFWS Disclaimer
================

This United States Fish & Wildlife Service (USFWS) code is provided on an "as is" basis and the user assumes responsibility for its use. USFWS has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recomendation or favoring by USFWS. The USFWS seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by USFWS or the United States Government.

Installing `fwsinat`
====================

The `fwsinat` package requires you to have [R](https://www.r-project.org/) (&gt;= 3.3) installed on your computer as well as [Rtools](https://cran.r-project.org/bin/windows/Rtools/). Both will require administrative priveleges but the installation of packages after this initial install will not.

With R and Rtools installed, it's simple to install and load the `fwsinat` package to access its functionality.

**NOTE**: If you receive a SSL or CA Certificate error, you may need to take the extra step documented below.

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

1.  retrieve iNaturalist observations for any number of available USFWS properties (`inat_retrieve`), the valid names of which can be obtained with the `find_refuges` function;
2.  update previous retrievals of observations (`inat_update`);
3.  export the observations to separate spreadsheets by USFWS property for distribution (`inat_export`); and
4.  harvest observations on USFWS properties into the [USFWS NWRS iNaturalist project](https://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system) (`inat_harvest`)

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

Now let's pick a refuge and retrieve the observations from iNaturalist... `inat_retrieve` is our friend. Before we do so, however, we need to decide on a few options:

1.  Do we want all iNaturalist observations on the property or only those from a specific project?

    By default, `inat_retrieve` returns only observations associated with the [USFWS National Wildlife Refuge System iNaturalist project](https://www.inaturalist.org/projects/usfws-national-wildlife-refuge-system). If you want a different project, you'll need to specify which one with the `inat_proj` argument (see `?inat_retrieve` for guidance on what's expected). If you want **ALL** observations, use `inat_proj = NULL` (see below).

2.  Do we want to limit the dates of those observations?

    By default, `inat_retrieve` does not limit retrievals by date range. See `?inat_retrieve` for guidance if this is of interest.

3.  Do we want helpful messages during the retrieval?

    Generally these are useful to track progress, so the default is to print these messages. If they annoy you, you can reduce them to a minimum by passing `verbose = FALSE` to `inat_retrieve`.

``` r
dis <- find_refuges("dismal")

# Get observations on Great Dismal Swamp NWR only from the USFWS NWRS project
dismal <- inat_retrieve(dis)
```

    ## Processing Great Dismal Swamp National Wildlife Refuge within the
    ## usfws-national-wildlife-refuge-system project.

    ## Retrieving 32 records.

    ## Records retrieved: 
    ##   0-32

``` r
# Get all observations on Great Dismal Swamp NWR
dismal_all <- inat_retrieve(dis, inat_proj = NULL)
```

    ## Processing Great Dismal Swamp National Wildlife Refuge across all
    ## iNaturalist projects.

    ## Retrieving 321 records.

    ## Records retrieved: 
    ##   0-200-321

**A BRIEF ASIDE**: In our Great Dismal Swamp example, we've retrieved observations only for a single refuge. We can just as easily retrieve observations from multiple refuges:

``` r
# Get all observations from a few refuges
multi <- find_refuges(c("dismal", "mackay", "currituck", "back bay"))
multi <- inat_retrieve(multi, inat_proj = NULL)

# Get USFWS NWRS project observation from all Region 4 (Southeast) refuges
r4 <- find_refuges(region = 4)
r4 <- inat_retrieve(r4)
```

Returning to our Great Dismal Swamp example, we notice there are **nearly 300** observations on the refuge that do not belong to the USFWS NWRS project. We want those observations! We can try and harvest them with `inat_harvest`, although this will require you to have an [iNaturalist](http://www.inaturalist.org/) account and to pass your username and password. It may be best to leave this to USFWS NWRS project administrators. We illustrate it here with hidden credentials to prove the point, however. Some useful information on options for storing and using your webservice credentials in R is available [here](http://blog.revolutionanalytics.com/2015/11/how-to-store-and-use-authentication-details-with-r.html).

``` r
out <- inat_harvest(dis, user = Sys.getenv("user"), pw = Sys.getenv("pw"), interactive = FALSE)
```

    ## 289 observations available for harvest on Great Dismal Swamp National Wildlife Refuge.

    ## Failed to harvest 21 observations due to errors.

Occasionally observations cannot be harvested because the user restricts their observations. If this happens, you will receive a message indicating how many records could not be harvested (21 in the case of Great Dismal Swamp), and you can explore details of those observations by printing the object you just created. In this case, that object was called `out` and it looks like these 21 observations are restricted by the users.

``` r
# See first 6 harvest errors
head(out)
```

    ##                                       orgname observation_id http_error
    ## 1 GREAT DISMAL SWAMP NATIONAL WILDLIFE REFUGE        2091064       TRUE
    ## 2 GREAT DISMAL SWAMP NATIONAL WILDLIFE REFUGE        2091063       TRUE
    ## 3 GREAT DISMAL SWAMP NATIONAL WILDLIFE REFUGE        2091051       TRUE
    ## 4 GREAT DISMAL SWAMP NATIONAL WILDLIFE REFUGE        2091089       TRUE
    ## 5 GREAT DISMAL SWAMP NATIONAL WILDLIFE REFUGE        2091092       TRUE
    ## 6 GREAT DISMAL SWAMP NATIONAL WILDLIFE REFUGE        2091084       TRUE
    ##                                                           error_msg
    ## 1 Submitter does not allow addition to projects they haven't joined
    ## 2 Submitter does not allow addition to projects they haven't joined
    ## 3 Submitter does not allow addition to projects they haven't joined
    ## 4 Submitter does not allow addition to projects they haven't joined
    ## 5 Submitter does not allow addition to projects they haven't joined
    ## 6 Submitter does not allow addition to projects they haven't joined
    ##         user
    ## 1 botanygirl
    ## 2 botanygirl
    ## 3 botanygirl
    ## 4 botanygirl
    ## 5 botanygirl
    ## 6 botanygirl

``` r
# USFWS NWRS project now contains all currently harvestable observations
dismal <- inat_retrieve(dis)
```

    ## Processing Great Dismal Swamp National Wildlife Refuge within the
    ## usfws-national-wildlife-refuge-system project.

    ## Retrieving 300 records.

    ## Records retrieved: 
    ##   0-200-300

It would be handy to update these observations periodically, and the `inat_update` function provides this option. For example, to update our Great Dismal Swamp records we would run `inat_update` on the created `dismal` object:

``` r
dismal <- inat_update(dismal)
```

    ## No updates available.

In this case, no observations had been updated (e.g., another iNaturalist user had suggested an identification) and no new observation had been added. This isn't surprising given the short period between retrieval and update. Normally you'll want to save the record of observations locally and then update them some time later. For example:

``` r
saveRDS(dismal, file = "SOME/PATH/TO/great_dismal.rds")

# Wait a few weeks or months and...
dismal <- readRDS("SOME/PATH/TO/great_dismal.rds")
dismal <- inat_update(dismal)
```

Lastly, at least in Region 4, we want to generate refuge-specific spreadsheets for distribution to the refuges so they have an updated record of biota observed on the property and, if so desired, they can suggest identifications for observations that may not have the desired level of specificity. We generate these spreadsheets with the `inat_export` function. You specify the output directory and `inat_export` generates an output spreadsheet there for each property contained in the input `fwsinat` object.

``` r
inat_export(dismal, dir = "C:/temp/test_export")
```

    ## Processing Great Dismal Swamp National Wildlife Refuge...  Export successful.

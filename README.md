
[![Build Status](https://travis-ci.org/adamdsmith/fwsinat.png)](https://travis-ci.org/adamdsmith/fwsinat)

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

1.  download iNaturalist observations for any number of iNaturalist projects (`retrieve_inat`);
2.  assign those observation to the USFWS property in which they occur (`assign_inat`); and
3.  export the observations to separate spreadsheets by USFWS property for distribution (`export_inat_xl`);

Using `fwsinat`
===============

First, lets specify some iNaturalist projects (sometimes called 'slugs') from which to retrieve data...

``` r
slugs <- c("bon-secour-national-wildlife-refuge-bioblitz",
           "2012-bioblitz-at-don-edwards-san-francisco-bay-national-wildlife-refuge",
           "antioch-dunes-national-wildlife-refuge-bioblitz")
```

Now let's retrieve the observations from iNaturalist...

``` r
ex <- retrieve_inat(slugs)
```

    ## Processing iNaturalist project: bon-secour-national-wildlife-refuge-bioblitz

    ## Retrieving 188 records.

    ## Records retrieved: 
    ##   0-188

    ## Processing iNaturalist project: 2012-bioblitz-at-don-edwards-san-francisco-bay-national-wildlife-refuge

    ## Retrieving 283 records.

    ## Records retrieved: 
    ##   0-200-283

    ## Processing iNaturalist project: antioch-dunes-national-wildlife-refuge-bioblitz

    ## Retrieving 159 records.

    ## Records retrieved: 
    ##   0-159

    ## Retained 568 georeferenced iNaturalist records.

and assign them to a refuge based on their geographic location...

Some (occasionally many) observations may not fall within a USFWS property boundary because the location information is obscured, either by the user or by [iNaturalist (for IUCN Red List 'near-threatened' and higher risk)](http://www.inaturalist.org/pages/help#obscured). These we assume (perhaps incorrectly?) belong to the nearest USFWS property. Other observations falling outside property boundaries and *not* indicated to have obscured coordinates are dropped.

``` r
ex <- assign_inat(ex, progress = FALSE)
```

    ## 513 observations successfully assigned to an USFWS property.

    ## Retrieving nearest USFWS property for 4 observations with obscured locational coordinates.

    ## 51 observations have been discarded.

This object contains observations for potentially many refuges. We want to distribute these observations to USFWS property-specific spreadsheets for distribution to those properties. First, we create a list of properties with observations and then loop through them...

``` r
orgs <- unique(ex$orgname)
for (org in orgs) {
  tmp_ex <- dplyr::filter(ex, orgname == org)
  export_inat(tmp_ex, dir = "./Output", orgname = org)
}
```

    ## Processing ANTIOCH DUNES NWR...  ANTIOCH_DUNES_NWR.xlsx successfully created.
    ## Processing BON SECOUR NWR...  BON_SECOUR_NWR.xlsx successfully created.
    ## Processing DON EDWARDS SAN FRANCISCO BAY NWR...  DON_EDWARDS_SAN_FRANCISCO_BAY_NWR.xlsx successfully created.

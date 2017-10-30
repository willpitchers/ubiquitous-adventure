---
title: 'Data Incubator Challenge - Question 3: Project pitch'
author: "Will Pitchers"
date: "Saturday, 28 October 2017"
output:
  html_document:
    df_print: paged
---


```r
require( tidyverse )
require( data.table )
require( maps )
require( zipcode )
require( lme4 )
```

This dataset covers the period of 2000--2017 and includes 62560 records of fines levied against corporations relating to violations of regulations, from cases initiated by 43 federal regulatory agencies. I downloaded the data from the Good Jobs First ["Violation Tracker"](https://www.goodjobsfirst.org/violation-tracker), using the search GUI with all the options set to `<any>`.



```r
viol <- tbl_df( fread( "/Users/willpitchers/Documents/=Job_Applications_etc/Data_Incubator_2017/violation_tracker_export.csv" ))

names( viol ) <- gsub( " ", "_", names( viol ))

viol <- viol %>% mutate( Year=as.integer( Year ), Industry_code=factor( Industry_in_Record ), Civ_Crim=factor( `Civil/Criminal` ) ) %>%
                  mutate( HQ_State_of_Parent=factor( HQ_State_of_Parent ), HQ_Country_of_Parent=factor( HQ_Country_of_Parent ) ) %>%
                  mutate( Primary_Offense=factor( Primary_Offense ), Penalty_Amount=as.numeric( gsub( "[$,]", "", Penalty_Amount ) ) ) %>% 
                  mutate( Penalty_Adj=as.numeric( gsub( "[$,]", "", Penalty_Amount_Adjusted_For_Eliminating_Multiple_Counting )) ) %>% 
                  mutate( Subtraction_From_Penalty=as.numeric( "[$,]", "", Subtraction_From_Penalty ) ) %>% 
                  mutate( Agency=factor( Agency ), Secondary_Offense=factor( Secondary_Offense ), Ownership_Structure=factor( Ownership_Structure ) ) %>% 
                  mutate( Major_Industry_of_Parent=factor( Major_Industry_of_Parent ), Zip=factor( Zip ), Facility_State=factor( Facility_State ) )
```

```
## Warning in evalq(as.numeric("[$,]", "", Subtraction_From_Penalty),
## <environment>): NAs introduced by coercion
```

```r
viol <- viol %>% mutate( Civ_Crim_bin=factor( ifelse( grepl( "civil and criminal", `Civil/Criminal` )=="TRUE", "both", 
                                              ifelse( grepl( "civil", `Civil/Criminal` )=="TRUE", "civil", "criminal" ))))

# str( viol )
# summary( viol$Year )
viol
```

```
## # A tibble: 62,560 x 28
##                                                               Company
##                                                                 <chr>
##  1                                                 1 800 PACK RAT LLC
##  2 1) Smithfield Foods, Inc. and  2) Smithfield Packing Company, Inc.
##  3                                             1366 TECHNOLOGIES INC.
##  4                                             1366 TECHNOLOGIES, INC
##  5                                                    180 Connect Inc
##  6                                                  180 Connect, Inc.
##  7                                             1st Financial Bank USA
##  8                                             1ST FINANCIAL BANK USA
##  9                           20TH CENTURY FOX STUDIOS (THREE STOOGES)
## 10                                           21ST CENTURY MEDIA, INC.
## # ... with 62,550 more rows, and 27 more variables: Parent_Company <chr>,
## #   Facility_State <fctr>, City <chr>, Address <chr>, Zip <fctr>,
## #   Description <chr>, Industry_in_Record <chr>, Year <int>,
## #   Record_Date <chr>, Penalty_Amount <dbl>,
## #   Subtraction_From_Penalty <dbl>,
## #   Penalty_Amount_Adjusted_For_Eliminating_Multiple_Counting <chr>,
## #   Agency <fctr>, Primary_Offense <fctr>, Secondary_Offense <fctr>,
## #   Source_of_Data <chr>, Notes <chr>, HQ_State_of_Parent <fctr>,
## #   HQ_Country_of_Parent <fctr>, Ownership_Structure <fctr>,
## #   Major_Industry_of_Parent <fctr>, `Civil/Criminal` <chr>,
## #   Prosecution_Agreement <chr>, Industry_code <fctr>, Civ_Crim <fctr>,
## #   Penalty_Adj <dbl>, Civ_Crim_bin <fctr>
```

## The Size of Penalties

This dataset contains fields for both `Penalty_Amount` and `Penalty_Amount_Adjusted_For_Eliminating_Multiple_Counting`, but is clear that these adjustments make very little difference to the data as a whole, as these two variables are correlation at r=0.994. I have thus elected to use the adjusted penalty values for all these analyses.

The first pattern to note is that (perhaps predictably) the penalties imposed via *criminal* proceedings tend to be larger than those imposed via *civil* proceedings, and larger still when both civil *and* criminal proceedings have been brought.


```r
viol %>% filter( Penalty_Adj > 0 ) %>% ggplot( aes( Penalty_Adj )) + 
          geom_density( aes( col=Civ_Crim_bin, fill=Civ_Crim_bin ), alpha=.5 ) + 
          scale_x_log10() + 
          xlab( "Penalty (log10 $'s)" ) + 
          scale_fill_discrete( name="Type of\ncase brought") + 
          scale_colour_discrete( name="Type of\ncase brought" )
```

![plot of chunk penalty civ vs. crim](figure/penalty civ vs. crim-1.png)

The way this pattern is built up is somewhat non-intuitive, as the linear model and boxplot below make clear. The groups mean are well-separated, and easy to distinguish statistically (small p-values), but there is so much variation within groups that the predictive value of the mean differences is small (low R^2^ value). The vast majority of penalties arise from civil actions -- 99.3% of those recorded -- and the mean value of these penalties is comparatively small.


```r
summary( lm( Penalty_Adj ~ Civ_Crim_bin, data=viol ) )
```

```
## 
## Call:
## lm(formula = Penalty_Adj ~ Civ_Crim_bin, data = viol)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -5.913e+08 -4.644e+06 -4.640e+06 -4.627e+06  2.080e+10 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           596240136   24921654   23.93   <2e-16 ***
## Civ_Crim_bincivil    -591589764   24931682  -23.73   <2e-16 ***
## Civ_Crim_bincriminal -464209918   26451988  -17.55   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 176200000 on 62557 degrees of freedom
## Multiple R-squared:  0.01211,	Adjusted R-squared:  0.01207 
## F-statistic: 383.3 on 2 and 62557 DF,  p-value: < 2.2e-16
```

```r
viol %>% filter( Penalty_Adj > 0 ) %>% ggplot( aes( Civ_Crim_bin, Penalty_Adj )) + 
                                        geom_point( colour="blue", alpha=0.3, position="jitter" ) + 
                                        geom_boxplot( outlier.size=0, alpha=0 ) + 
                                        coord_flip() + 
                                        xlab( "Type of case brought" ) +
                                        scale_y_log10() +
                                        ylab( "Penalty (log10 $'s)" )
```

![plot of chunk no.s crim vs. civ](figure/no.s crim vs. civ-1.png)

## Trends over Time

Modelling the relationship between penalty values and the year the penalty was imposed reveals an increasing trend, with the coefficent for year indicating an increase of ~$950k per year. However, the R^2^ value for this model indicates that it accounts for only 0.05% of the variation in the data. 

This makes sense with reference to the scatter plot, where we can see that the trend for annual increase (represented by the dashed black line) is pretty modest when compared to *huge* variation in the size of penalties (note log^10^ scale).


```r
summary( lm( Penalty_Adj ~ Year, data=viol ) )
```

```
## 
## Call:
## lm(formula = Penalty_Adj ~ Year, data = viol)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -1.323e+07 -9.358e+06 -6.516e+06 -2.701e+06  2.079e+10 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.915e+09  3.175e+08  -6.032 1.63e-09 ***
## Year         9.562e+05  1.580e+05   6.051 1.45e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 177200000 on 62558 degrees of freedom
## Multiple R-squared:  0.0005849,	Adjusted R-squared:  0.0005689 
## F-statistic: 36.61 on 1 and 62558 DF,  p-value: 1.449e-09
```

```r
viol %>% filter( Penalty_Adj > 0 ) %>% ggplot( aes( x=Year, y=Penalty_Adj ) ) + 
                                        geom_jitter( aes( col=Civ_Crim_bin ), alpha=0.5, width=.2, height=0 ) +
                                        ylab( "Penalty amount (log10 $'s)" ) +
                                        geom_smooth( method="lm", col="black", lty=2 ) + 
                                        scale_colour_discrete( name="Type of\ncase brought" ) +
                                        scale_y_log10()
```

![plot of chunk penalty by year](figure/penalty by year-1.png)

## Where are Violations Occurring?

Taking a broad-strokes view of the geographical data, we can see that there seems not to be a visually apparent trend in the locations where penalties are levied -- this data appears to track pretty well with the locations of population centres, thought here may be more subtle patterns that are not visible at the nation-wide scale.


```r
data( zipcode )
zipcode <- zipcode %>% mutate( zip=factor( zip ), region=substr( zip, 1, 1) )

full_join( viol, zipcode, by=c( "Zip" = "zip" ) ) %>% mutate( Zip=factor( Zip ) ) %>% filter( Civ_Crim_bin=="civil" ) %>% 
    ggplot() + geom_point( aes( x=longitude, y=latitude, col=Year ), cex=.5 ) + 
        theme_bw() + scale_x_continuous(limits = c(-125,-66), breaks = NULL ) + 
        scale_y_continuous(limits = c(25,50), breaks = NULL ) + 
        labs(x=NULL, y=NULL)
```

![plot of chunk penalty map](figure/penalty map-1.png)

However, if we look the number of penalties paid in each state/territory over the course of this 18-yr dataset, we can see that there are *many* more violations in some states that others, with West Virginia being responsible for 15.22% of all penalties levied. 


```r
viol %>% filter( Facility_State != "" ) %>% ggplot( aes( reorder( Facility_State, Facility_State, function(x)-length(x) ) )) + 
                                              geom_bar( aes( fill=Major_Industry_of_Parent )) + 
                                              coord_flip() +
                                              theme( axis.text.y=element_text( hjust=0, size=9 ) ) +
                                              ylab( "no. penalties" ) + 
                                              xlab( "" ) + 
                                              scale_fill_discrete( name="Industrial Sector" )
```

![plot of chunk violations by state](figure/violations by state-1.png)

The bars are colored by the industrial sector of the parent corporation found liable for the penalty -- the blue that occupies 93.43% of the West Virginia bar represents corporations classified as "mining and minerals". It is apparent that WV is unusual in both the number of violations *and* the number of those violations related to mining.

## What are Corporations being Fined For?

Across the 49 industrial sectors represented, it is immediately clear from this barplot the extent to which mining & mineral corporations are over-represented (25.24% of all penalties). It is also clear that the pink areas -- indicating 'workplace safety or health' violations -- comprise the majority of violations in most sectors, but particularly so in mining.


```r
viol %>% filter( Facility_State != "" ) %>% ggplot( aes( reorder( Major_Industry_of_Parent, Major_Industry_of_Parent, function(x)-length(x) ) )) + 
                                              geom_bar( aes( fill=Primary_Offense )) + 
                                              coord_flip() +
                                              theme( axis.text.y=element_text( hjust=0, size=9 ) ) +
                                              ylab( "no. penalties" ) + 
                                              xlab( "" ) + 
                                              scale_fill_discrete( name="Violation Categories" )
```

![plot of chunk violations by offense](figure/violations by offense-1.png)

## Take-Home Message

From these analyses so far, my preliminary recommendation would be that the primary focus of labor-safety lobbying efforts ought to be the West Virginian mining industry. This would also be a potentially fruitful focus for legislators and regulators, as the apparent abundance of regulatory violations would potentially allow for high statistical power in any empirical tests for the effects of regulatory/enforcement policy changes.

The over-representation of workplace health and safety violations is also noteworthy, and not limited to the mining sector, nor to West Virginia. Given the generality of this pattern, a potential ethical investor should be encouraged to investigate a corporation's history of health & safety compliance as a priority.

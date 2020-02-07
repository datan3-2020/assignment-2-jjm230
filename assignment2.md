Data analysis assignment 2
================
jjm230; 680003775
05/02/2020

In this assignment you will work with relational data, i.e. data coming from different data tables that you can combine using keys. Please read ch.13 from R for Data Science before completing this assignment -- <https://r4ds.had.co.nz/relational-data.html>.

Read data
---------

We will work with three different tables: household roster from wave 8 (*h\_egoalt*), stable characteristics of individuals (*xwavedat*), and household data from wave 8 (*h\_hhresp*).

``` r
library(tidyverse)
# You need to complete the paths to these files on your computer.
Egoalt8 <- read_tsv("~/Documents/Exeter/Q-Step/POL2094 Data Analysis in Social Science III/Data III Project/Data III Project/data/UKDA-6614-tab/tab/ukhls_w8/h_egoalt.tab")
Stable <- read_tsv("~/Documents/Exeter/Q-Step/POL2094 Data Analysis in Social Science III/Data III Project/Data III Project/data/UKDA-6614-tab/tab/ukhls_wx/xwavedat.tab")
Hh8 <- read_tsv("~/Documents/Exeter/Q-Step/POL2094 Data Analysis in Social Science III/Data III Project/Data III Project/data/UKDA-6614-tab/tab/ukhls_w8/h_hhresp.tab")
```

Filter household roster data (10 points)
----------------------------------------

The **egoalt8** data table contains data on the kin and other relationships between people in the same household. In each row in this table you will have a pair of individuals in the same household: ego (identified by *pidp*) and alter (identified by *apidp*). *h\_relationship\_dv* shows the type of relationship between ego and alter. You can check the codes in the Understanding Society codebooks here -- <https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation>.

First we want to select only pairs of individuals who are husbands and wives or cohabiting partners (codes 1 and 2). For convenience, we also want to keep only the variables *pidp*, *apidp*, *h\_hidp* (household identifier), *h\_relationship\_dv*, *h\_esex* (ego's sex), and *h\_asex* (alter's sex).

``` r
Partners8 <- Egoalt8 %>%                                              # Create Partners8 from Egoalt8, by
        filter(h_relationship_dv==1 | h_relationship_dv==2) %>%       # filtering for obs. whose h_relationship_dv values are 1 or 2, and then
        select(pidp, apidp, h_hidp, h_relationship_dv, h_sex, h_asex) # selecting only the listed variables. N.b. h_esex seems to have been updated to be just h_sex.
```

Each couple now appears in the data twice: 1) with one partner as ego and the other as alter, 2) the other way round. Now we will only focus on heterosexual couples, and keep one observation per couple with women as egos and men as their alters.

``` r
Hetero8 <- Partners8 %>%              # Create Hetero8 from Partners8, by
        # filter out same-sex couples                                 
        filter(h_sex!=h_asex) %>%     # filtering for obs. for whose h_sex and h_asex values are different, and then
        # keep only one observation per couple with women as egos
        filter(h_sex==2)              # filtering for obs. whose h_sex value is 2.
```

Recode data on ethnicity (10 points)
------------------------------------

In this assignment we will explore ethnic endogamy, i.e. marriages and partnerships within the same ethnic group. First, let us a create a version of the table with stable individual characteristics with two variables only: *pidp* and *racel\_dv* (ethnicity).

``` r
Stable2 <- Stable %>%          # Create Stable2 from Stable, by
        select(pidp, racel_dv) # selecting only the listed variables.
```

Let's code missing values on ethnicity (-9) as NA.

``` r
Stable2 <- Stable2 %>%                                       # Create Stable2 from Stable2, by
        mutate(racel_dv = recode(racel_dv, `-9` = NA_real_)) # creating the var. 'racel_dv', defined as a recoding of 'racel_dv' wherein '-9' becomes NA.
```

Now let us recode the variable on ethnicity into a new binary variable with the following values: "White" (codes 1 to 4) and "non-White" (all other codes).

``` r
Stable2 <- Stable2 %>%                                                # Create Stable2 from Stable2, and then
        mutate(race = ifelse(racel_dv == 1 |                          # create a new variable 'race', defined as: if racel_dv is 1, or
                               racel_dv == 2 |                        # 2, or
                               racel_dv == 3 |                        # 3, or
                               racel_dv == 4, "White", "non-White"))  # 4, then obs. take "White", else obs. take "non-White"
Stable2$race <- as.factor(Stable2$race)                               # Convert Stable2 into a factor variable 
```

Join data (30 points)
---------------------

Now we want to join data from the household roster (*Hetero8*) and the data table with ethnicity (*Stable2*). First let us merge in the data on ego's ethnicity. We want to keep all the observations we have in *Hetero8*, but we don't want to add any other individuals from *Stable2*.

``` r
JoinedEthn <- Hetero8 %>%            # Create JoinedEthn from Hetero8, and then
    left_join(Stable2, by = "pidp")  # incorporate vars. from Stable2 and its obs. that correspond to an ob. within Hetero8 only, by pidp.
```

Let us rename the variables for ethnicity to clearly indicate that they refer to egos.

``` r
JoinedEthn <- JoinedEthn %>%                # Create JoinedEthn from JoinedEthn, and then
        rename(egoRacel_dv = racel_dv) %>%  # rename racel_dv to egoRacel_dv, and then
        rename(egoRace = race)              # rename race to egoRace
```

Now let us merge in the data on alter's ethnicity. Note that in this case the key variables have different names in two data tables; please refer to the documentation for your join function (or the relevant section from R for Data Science) to check the solution for this problem.

``` r
JoinedEthn <- JoinedEthn %>%                         # Create JoinedEthn from JoinedEthn, and then
        left_join(Stable2, by = c("apidp" = "pidp")) # incorporate vars. from Stable2 and its obs. that correspond to an ob. within Hetero8 only, by apidp (that is named pidp within Stable2).
```

Renaming the variables for alters.

``` r
JoinedEthn <- JoinedEthn %>%                  # Create JoinedEthn from JoinedEthn, and then
        rename(alterRacel_dv = racel_dv) %>%  # rename racel_dv to alterRacel_dv, and then
        rename(alterRace = race)              # rename race to alterRace
```

Explore probabilities of racial endogamy (20 points)
----------------------------------------------------

Let us start by looking at the joint distribution of race (White vs. non-White) of both partners.

``` r
TableRace <- JoinedEthn %>%                          # Create TableRace from JoinedEthn, and then
        # filter out observations with missing data
        drop_na() %>%                                # Remove all obs. containing NAs, and then
        count(egoRace, alterRace)                    # Tabulate obs. by egoRace and alterRace
TableRace                                            # Print TableRace
```

    ## # A tibble: 4 x 3
    ##   egoRace   alterRace     n
    ##   <fct>     <fct>     <int>
    ## 1 non-White non-White  1790
    ## 2 non-White White       326
    ## 3 White     non-White   266
    ## 4 White     White      9694

Now calculate the following probabilities: 1) for a White woman to have a White partner, 2) for a White woman to have a non-White partner, 3) for a non-White woman to have a White partner, 4) for a non-White woman to have a non-White partner.

Of course, you can simply calculate these numbers manually. However, the code will not be reproducible: if the data change the code will need to be changed, too. Your task is to write reproducible code producing a table with the required four probabilities.

``` r
TableRace %>%                            # Pull TableRace, and then
        # group by ego's race to calculate sums
        group_by(egoRace) %>%            # group by var. egoRace, and then
        # create a new variable with the total number of women by race
        mutate(sumWomen = sum(n)) %>%    # create new var. 'sumWomen', defined as the sum of var. n (implicit by race, inherited from above), and then
        # create a new variable with the required probabilities 
        mutate(Prob = (n/sumWomen)*100)  # create new var. 'Prob', defined as the proportion of n, multiplied by 100.
```

    ## # A tibble: 4 x 5
    ## # Groups:   egoRace [2]
    ##   egoRace   alterRace     n sumWomen  Prob
    ##   <fct>     <fct>     <int>    <int> <dbl>
    ## 1 non-White non-White  1790     2116 84.6 
    ## 2 non-White White       326     2116 15.4 
    ## 3 White     non-White   266     9960  2.67
    ## 4 White     White      9694     9960 97.3

Join with household data and calculate mean and median number of children by ethnic group (30 points)
-----------------------------------------------------------------------------------------------------

1.  Join the individual-level file with the household-level data from wave 8 (specifically, we want the variable for the number of children in the household).
2.  Select only couples that are ethnically endogamous (i.e. partners come from the same ethnic group) for the following groups: White British, Indian, and Pakistani.
3.  Produce a table showing the mean and median number of children in these households by ethnic group (make sure the table has meaningful labels for ethnic groups, not just numerical codes).
4.  Write a short interpretation of your results. What could affect your findings?

``` r
Hh8.h_nkids_dv <- Hh8 %>%                                                       # Create Hh8.h_nkids_dv from Hh8, by
  select(h_hidp, h_nkids_dv)                                                    # selecting only listed vars.

JoinedEthn <- JoinedEthn %>%                                                    # Create JoinedEthn from JoinedEthn, and then
  left_join(Hh8.h_nkids_dv, by = "h_hidp")                                      # incorporate vars. from Hh8.h_nkids_dv and its obs. that correspond to an ob. within JoinedEthn only, by h_hidp.

JoinedEthn2 <- subset(JoinedEthn,                                               # Create JoinedEthn2 as a subset of JoinedEthn,
                      subset = egoRacel_dv == 1 |                               # including only obs. where egoRacel_dv equals 1, or
                               egoRacel_dv == 9 |                               # egoRacel_dv equals 9, or
                               egoRacel_dv == 10) %>%                           # egoRacel_dv equals 10, and then
  filter(egoRacel_dv == alterRacel_dv) %>%                                      # filter for obs. where egoRacel_dv and alterRacel_dv are equal, and then
  mutate(egoRace = ifelse(egoRacel_dv == 1, "White British",                    # create var. 'egoRace', defined as: if egoRacel_dv is 1, take "White British", else
                               ifelse(egoRacel_dv == 9, "Indian",               # if egoRacel_dv is 9, take "Indian", else                 
                               ifelse(egoRacel_dv == 10, "Pakistani", "other"   # if egoRacel_dv is 10, take "Pakistani", else take "other",
                                      )))) %>%                                  # and then
  filter(egoRace != "other")                                                    # filter for obs. that do not equal "other" within var. egoRace

JoinedEthn2$egoRace <- as.factor(JoinedEthn2$egoRace)                           # Compel egoRace to factor

TableRace2 <- JoinedEthn2 %>%                                                   # Create TableRace2 from JoinedEthn2, and then
  drop_na() %>%                                                                 # removing all missing values, and then
  group_by(egoRace) %>%                                                         # grouping by egoRace, and then
  summarise(meanChild = mean(h_nkids_dv),                                       # summarising as meanChild, defined as the mean of h_nkids_dv, and
            medianChild = median(h_nkids_dv))                                   # as medianChild, defined as the median of h_nkids_dv (implicit by race, inherited from above)
print(TableRace2) 
```

    ## # A tibble: 3 x 3
    ##   egoRace       meanChild medianChild
    ##   <fct>             <dbl>       <dbl>
    ## 1 Indian            0.955           1
    ## 2 Pakistani         1.81            2
    ## 3 White British     0.565           0

Interpretation: It is clear that there are significant differences between the number of children per household when stratified by race (focussing on the three races filtered for here). In particular over 50% of Pakistani housholds within the survey reported having at least two children, while the same proportion of Indian households reported at least one child, and over 50% of White British households reported none. This trend continues with the mean; Pakistani households have an average of almost two children, Indian ones almost one, and White British ones almost half that. Explanations for these trends are numerous: fertility is known to be negatively correlated with household income, which may expected to be lower for families of immigrant origins (n.b. not all racially non-White British respondents will be immigrants, however their families will have immigrated at some point in the past). Thus there might be fewer opportunities for economic advancement, which itself offers opportunities to limit fertility in ways that poorer households do not have access to. For example, the cost of birth control. However, this analysis did not determine the economic capacity of the respondents and so this interpretation cannot be substantiated without conducting further analysis.


# Purpose

This README captures all my code which I had used in the creation of my
thesis for 2025.

# Running initial OLS on data

- An OLS model was created to test if the variables are somewhat
  significant in the first place.
- It helps to inform the general direction of the project and to
  establish any relationships from early on.

## Pull Data into R

``` r
# pull W1 data from the adult questionnaires
download.file(
    'https://www.dropbox.com/scl/fi/3s45c3flubpx0ngbz4qjq/Adult_W1_Anon_V7.0.0.dta?rlkey=oa17mxoimb3gqdo7x7jh90elu&st=dpzn40ah&dl=1',
    './data/w1_adult_anon.dta',
    mode = "wb"
)

w1 <- read_dta('./data/w1_adult_anon.dta')

# pull W4 data from adult questionnaires
download.file(
    'https://www.dropbox.com/scl/fi/3ggfkjhj55z09i11e40ft/Adult_W4_Anon_V2.0.0.dta?rlkey=wjrlq33ggrohe5o8z3mpjsrda&st=ul2l4ay9&dl=1',
    './data/w4_adult_anon.dta'
)

w4 <- read_dta('./data/w4_adult_anon.dta')
```

## Clean and wrangle data

- Obtain W1 data and clean to include only variables which proxy for
  conscientiousness and other relevant variables

``` r
clean_w1 <- w1 %>% 
    select(pid, w1_a_dob_m, w1_a_dob_y, w1_a_gen, w1_a_unemmn, w1_a_hllfexer, w1_a_emohope, w1_a_aspen, w1_a_edter, w1_a_hllfsmk, w1_a_edintter, w1_a_wbsat, w1_a_hldifmon) %>% # selecting our questions which proxied for measures of conscientiousness
    mutate(Age = 2008 - w1_a_dob_y) %>% # creating age variable 
    filter(Age >= 14 & Age <=23) %>% # filter for those aged 14-23 at time of interview
    select(-c(w1_a_dob_m, w1_a_dob_y)) # remove unnecessary data
```

- Now want to clean W4 data to obtain W4 wages, employment, and
  schooling

``` r
clean_w4 <- w4 %>% 
    select(pid, w4_a_gen, w4_a_dob_y, w4_a_em1, w4_a_em1pay, w4_a_edschgrd) # collecting columns with info on employment status, wages and schooling level
```

Tips:

- Include common sense test
- Include dummy variable for missing values of conscientiousness, start
  with 4/5 most promising then add more if needed.
- Use PCA/MCA for constructing measure. Do some research on this

## Joining W1 and W4

``` r
joined_w1_w4 <- clean_w1 %>% 
    left_join(clean_w4, by = "pid")
```

## Add a common sense test

This will help catch if there are errors in the data, e.g. does reported
gender change from W1 to W4.

``` r
test1 <- joined_w1_w4 %>% 
    filter(!is.na(w1_a_gen) & !is.na(w4_a_gen)) %>% # removes entries where gender was missing in either w1 or in w4
    mutate(match = w1_a_gen == w4_a_gen) # returns TRUE/FALSE from logical test to see if the genders in W1 and W4 match for each observation

unique(test1$match) # checking if we have only TRUE values
```

    ## [1] TRUE

Test shows that data is not flawed!! :)

(WITH OLD GENDER VARIABLE) 3 077 entries were successfully joined and
had passed the common sense test. This is different from the
`joined_w1_w4` sample size (4 148 observations) due to the exclusion of
those entries with missing variables. 1 071 variables drop out because
of a missing gender variable in either W1 or W4. Therefore must try with
the `best_gen` variable as this provides a better measure for test.

## Trying common sense test with best gender

``` r
download.file(
    'https://www.dropbox.com/scl/fi/etluqr514bxyzigroy7cy/indderived_W1_Anon_V7.0.0.dta?rlkey=y79rxp6euqe82pj3tlo7y8xmh&st=pamh08ef&dl=1',
    './data/indderived_w1.dta',
    mode = "wb"
)

w1_best <- read_dta('./data/indderived_w1.dta') # pull in data containing best gen variable in W1

w1_best_gender <- w1_best %>%
    filter(w1_quest_typ == 1) %>% # only want those who did adult questionnaire
    select(pid, w1_best_gen)

download.file(
    'https://www.dropbox.com/scl/fi/vkxbocpglujiix6fyd2m0/indderived_W4_Anon_V2.0.0.dta?rlkey=xwetrj2fmp75oozqh8bgo6xgz&st=0meb21px&dl=1',
    './data/indderived_w4.dta',
    mode = "wb"
)
w4_best <- read_dta('./data/indderived_w4.dta') # pull in data containing best gen variable in W4

w4_best_gender_empl <- w4_best %>%
    filter(w4_quest_typ == 1) %>% # only want those who did adult questionnaire
    select(pid, w4_best_gen, w4_empl_stat, w4_best_race)

# now want to merge the best gender and best employment status columns into the main data

clean_w1_best <- clean_w1 %>%
    left_join(w1_best_gender, by = "pid")

clean_w4_best <- clean_w4 %>%
    left_join(w4_best_gender_empl, by = "pid")

joined_w1_w4_best <- clean_w1_best %>%
    left_join(clean_w4_best, by = "pid")
```

``` r
test_best <- joined_w1_w4_best %>% 
    filter(!is.na(w1_best_gen) & !is.na(w4_best_gen)) %>% # removes entries where gender was missing in either w1 or in w4
    mutate(match = w1_best_gen == w4_best_gen) # returns TRUE/FALSE from logical test to see if the genders in W1 and W4 match for each observation

unique(test_best$match)
```

    ## [1] TRUE

(WITH BEST GENDER VARIABLE) Now have 3 768 entries which were
successfully joined. Once again the genders match. Now have only *380*
missing observations for gender as opposed to *1 071*. W1 had no missing
values reported for best gender, whereas *W4 had all the 380 missing
values*. This might be explained by attrition post-W1.

## Create dummy variables coding missings for conscientiousness proxies

Now working towards running the OLS.

First, need to narrow down scope of proxies to 4 of the most important
potential proxies. These are the 4 I’ve chosen:

- J21: “How regularly do you exercise”. Speaks to hard-working nature
  and self-discipline of individual. *(46/4 148 missing)*

- H7: “Have you successfully completed any diplomas, certificates or
  degrees outside of school’’. Can show resilience, ambition, and
  dutifulness. Would, however, have to control for income level. *(50/
  4148 missing)*

- K5: “I felt hopeful about the future”. Speaks to ambitiousness of
  person. *(47/ 4148 missing)*

### New potential questions:

- H33: “Taking everything into account, do you intend to continue
  studying after matric, that is, after leaving school?”. *(2046/ 4148
  missing)* Too many missings and potential collinearity with question
  H7 makes it not usable.

- J26: “Do you smoke cigarettes”. *(31/ 4148 missing)* Measurement error
  and age group (14-23) makes this an uncertain measure.

- M5: “Using a scale of 1 to 10 where 1 means “Very dissatisfied” and 10
  means “Very satisfied”, how do you feel about your life as a whole
  right now?“. *(498/ 4148 missing)*. Very weak correlate of
  conscientiousness I think.

- J24.7: “Indicate the level of difficulty you have with managing money
  (if you had to)”

- Maybe something to do with income such as investing or being in a
  stokvel. Would be tricky due to effect of level of income on this.

Now can create dummy variable coding missing values as 1 and valid
entries as 0.

``` r
# start by only selecting variables which we have narrowed study down to
clean_w1 <- clean_w1 %>% 
    select(pid, w1_a_hllfexer, w1_a_emohope, w1_a_edter, w1_a_edintter, w1_a_hllfsmk, w1_a_wbsat, w1_a_hldifmon) %>% 
    # create conscientiousness dummy
    consc_dummy(c("w1_a_hllfexer", "w1_a_emohope", "w1_a_edter", "w1_a_edintter", "w1_a_hllfsmk", "w1_a_wbsat", "w1_a_hldifmon")) %>% 
    # now want to make sure that all these values get coded as missing in original columns
    nids_miss(c("w1_a_hllfexer", "w1_a_emohope", "w1_a_edter", "w1_a_edintter", "w1_a_hllfsmk", "w1_a_wbsat", "w1_a_hldifmon"))%>% 
    #also need to make sure that all variables are increasing in conscientiousness in same direction
    mutate(w1_a_edter_flipped = 3 - w1_a_edter)
```

## Create conscientiousness measure with PCA

- Will need to use the `prcomp` function for this

- Transforming data:

  - Convert entries such that all go in same direction, i.e. that higher
    value indicates higher conscientiousness
  - Some entries coded as -3 or 25 (these mean something). In this case,
    should I code as NA and then include them in my missing dummy.

``` r
consc <- prcomp(na.omit(clean_w1[, c("w1_a_hllfexer",
                                     "w1_a_emohope",
                                     "w1_a_edter_flipped",
                                     "w1_a_hllfsmk")]), 
                center = T, 
                scale. = T)

# Create a vector of NAs with original length
pca_scores <- rep(NA, nrow(clean_w1))

# Get indices of complete cases used in PCA
complete_rows <- complete.cases(clean_w1[, c("w1_a_hllfexer",
                                             "w1_a_emohope",
                                             "w1_a_edter_flipped",
                                             "w1_a_hllfsmk")])

# Assign PCA scores only to complete cases
pca_scores[complete_rows] <- consc$x[, 1]

# Now assign back to original dataframe
clean_w1$w1_a_consc <- pca_scores
```

# Run preliminary OLS for proposal

This section runs preliminary OLS models for inclusion into my research
proposal.

``` r
joined_w1_w4_best <- joined_w1_w4_best %>%
    mutate(w4_empl_stat = ifelse(w4_empl_stat == -8, NA, w4_empl_stat)) %>%  # code those who refused to answer as missing (15 people)
    mutate(w4_employed = ifelse(w4_empl_stat == 3, 1, 0)) %>% 
    mutate(w4_best_gen = as.factor(w4_best_gen)) %>%  # coding gender as a categorical variable
    mutate(w4_best_race = as.factor(w4_best_race)) # coding race as a categorical variable

joined_w1_w4_best$w1_a_consc <- pca_scores
```

# Continue work on OLS

The following chunks are my post-proposal code and serve to continue the
work to getting to an OLS model with additional controls.

## Create wealth measure (asset index) with MCA

Use MCA to construct a measure of wealth of individuals. This measure
can then be used as a control in the model.

``` r
# pull variables into df which give us info on asset ownership
clean_w1 <- clean_w1 %>%  
    left_join(select(w1, pid, w1_a_ownrad, w1_a_ownvehpri, w1_a_ownmot, w1_a_owncom, w1_a_owncel), by = "pid") %>% 
    nids_miss(c("w1_a_ownrad", "w1_a_ownvehpri", "w1_a_ownmot", "w1_a_owncom", "w1_a_owncel"))

# testing to see if my mutate command worked to code any `2`, i.e. `No`, responses as 0 and leaving 1 = yes     
test_mut <- clean_w1 %>% 
    mutate(across(c("w1_a_ownrad", "w1_a_ownvehpri", "w1_a_ownmot", "w1_a_owncom", "w1_a_owncel"), ~ ifelse(.x == 2, 0, .x)))

# Liam's MCA code
# binary_matrix <-
#   round7[, c("nrad","bradb","NOrad", "ntv","btvb","NOtv", "nveh","bvehb","NOveh", "ncom","bcomb", "NOcom", "npho", "bphob","NOpho",  "nban",     "bbanb", "NOban")]
# #creating MCA
# mca_result <- MCA(binary_matrix,graph = TRUE)
# round7$assdex <-
#   mca_result$ind$coord[, 1]
# round7$assdex  <-
#   (round7$assdex  + abs(min(round7$assdex)))# no negative
# print(mca_result$var$coord[, 1])
# remove(mca_result)
# remove(binary_matrix)

# Select the asset ownership columns
asset_vars <- clean_w1 %>%
  select(w1_a_ownrad, w1_a_ownvehpri, w1_a_ownmot, w1_a_owncom, w1_a_owncel)

######Stopped here ##########
# Select the asset ownership columns
# asset_vars <- df %>%
#   select(car, radio, cellphone, computer, fridge, tv)
# 
# # Convert asset vars to factors (MCA requires categorical variables)
# asset_factors <- asset_vars %>%
#   mutate(across(everything(), as.factor))
# 
# # Run MCA
# mca_result <- MCA(asset_factors, graph = FALSE)
# 
# # Extract the coordinates of individuals on the first dimension
# asset_index <- mca_result$ind$coord[, 1]
# 
# # Add the asset index as a new column to your original dataframe
# df <- df %>%
#   mutate(asset_index = asset_index)
```

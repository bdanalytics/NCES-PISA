# <Project name>: readingScore <regression/classification>
bdanalytics  

**  **    
**Date: (Sun) Mar 22, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/pisa2009train.csv  
    New:        https://courses.edx.org/c4x/MITx/15.071x_2/asset/pisa2009test.csv  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(plyr))

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_predict_dataset <- TRUE    # or TRUE
glb_predct_var <- "readingScore"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- NULL                # or c("<Id var>")

glb_exclude_vars_as_features <- NULL                      
# List chrs converted into factors 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("raceeth")     # or NULL
                                      )
# List feats that shd be excluded due to known causation by prediction variable
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("<col_name>")     # or NULL
#                                       )

glb_is_regression <- TRUE; glb_is_classification <- FALSE

glb_mdl <- glb_sel_mdl <- NULL
glb_models_df <- data.frame()

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/pisa2009train.csv", 
    comment="glb_entity_df", force_header=TRUE, print_diagn=TRUE)
```

```
## [1] "Reading file ./data/pisa2009train.csv..."
## [1] "dimensions of data in ./data/pisa2009train.csv: 3,663 rows x 24 cols"
##   grade male  raceeth preschool expectBachelors motherHS motherBachelors
## 1    11    1     <NA>        NA               0       NA              NA
## 2    11    1    White         0               0        1               1
## 3     9    1    White         1               1        1               1
## 4    10    0    Black         1               1        0               0
## 5    10    1 Hispanic         1               0        1               0
## 6    10    1    Black         1               1       NA              NA
##   motherWork fatherHS fatherBachelors fatherWork selfBornUS motherBornUS
## 1          1       NA              NA          1          1            0
## 2          1        1               0          1          1            1
## 3          1        1              NA          1          1            1
## 4          1        1               0          1          1            1
## 5          1        1               0          0          1            1
## 6          1        1               0          1          1            1
##   fatherBornUS englishAtHome computerForSchoolwork read30MinsADay
## 1            0             0                     1              0
## 2            1             1                     1              1
## 3            1             1                     1              0
## 4            1             1                     1              1
## 5            0             1                     1              1
## 6            1             1                     1              0
##   minutesPerWeekEnglish studentsInEnglish schoolHasLibrary publicSchool
## 1                   225                NA                1            1
## 2                   450                25                1            1
## 3                   250                28                1            1
## 4                   200                23                1            1
## 5                   250                35                1            1
## 6                   300                20                1            1
##   urban schoolSize readingScore
## 1     1        673       476.00
## 2     0       1173       575.01
## 3     0       1233       554.81
## 4     1       2640       458.11
## 5     1       1095       613.89
## 6     0        227       490.59
##      grade male raceeth preschool expectBachelors motherHS motherBachelors
## 609     10    1   Black         1               0        1               0
## 1671    10    1   Black         1               0        1               0
## 2641    10    0   Black         1               1        1               0
## 2786    10    0   Black         1               1        1               0
## 3208    10    0   White         0               1        1               0
## 3244     9    1   White         1               1        1               1
##      motherWork fatherHS fatherBachelors fatherWork selfBornUS
## 609           1        0               0          0          1
## 1671          0        1              NA          1          1
## 2641          1        1               0          0          1
## 2786          1        1               0          1          1
## 3208          1        1               0          1          1
## 3244          1        1              NA          0          1
##      motherBornUS fatherBornUS englishAtHome computerForSchoolwork
## 609             1            0             1                     1
## 1671            1            1             1                     1
## 2641            1            1             1                     1
## 2786            1            1             1                     1
## 3208            1            1             1                     1
## 3244            1            1             1                     1
##      read30MinsADay minutesPerWeekEnglish studentsInEnglish
## 609               0                    45                NA
## 1671              0                   240                15
## 2641              0                   250                NA
## 2786              0                    60                30
## 3208              0                   350                30
## 3244              0                   270                NA
##      schoolHasLibrary publicSchool urban schoolSize readingScore
## 609                 1            1     0        507       417.88
## 1671                1            1     0       2361       401.05
## 2641                1            1     0        762       472.41
## 2786                1            1     1       1400       453.82
## 3208                1            1     0        712       562.50
## 3244                1            1     0       2569       399.12
##      grade male            raceeth preschool expectBachelors motherHS
## 3658    10    1 More than one race         1               1        1
## 3659     9    1              White         0               1        1
## 3660     9    1              White         0               0        1
## 3661    10    1           Hispanic         1               1        1
## 3662    11    1              Black         0               0        1
## 3663    10    0              White         0               1        1
##      motherBachelors motherWork fatherHS fatherBachelors fatherWork
## 3658               0          1        1               0          1
## 3659              NA          0        1               1          1
## 3660               0          1        1               0          1
## 3661               0          1        1               0          1
## 3662               0         NA       NA               0          1
## 3663               0          1        1               0          1
##      selfBornUS motherBornUS fatherBornUS englishAtHome
## 3658          1            1            1             1
## 3659          1            1            1             1
## 3660          1            1            1             1
## 3661          1            1            1             1
## 3662          1            1            1             1
## 3663          1            1            1             1
##      computerForSchoolwork read30MinsADay minutesPerWeekEnglish
## 3658                     1              0                   270
## 3659                     1              0                   250
## 3660                     0              1                   450
## 3661                     1              0                   225
## 3662                     1              0                    54
## 3663                     1              1                   235
##      studentsInEnglish schoolHasLibrary publicSchool urban schoolSize
## 3658                24                1            1     0       1471
## 3659                20                1            1     0        421
## 3660                16                1            1     0       1317
## 3661                16                1            1     1        539
## 3662                36                1            1     1         NA
## 3663                25                1            1     0        227
##      readingScore
## 3658       492.76
## 3659       509.99
## 3660       444.90
## 3661       476.89
## 3662       363.61
## 3663       551.85
## 'data.frame':	3663 obs. of  20 variables:
##  $ grade                : int  11 11 9 10 10 10 10 10 9 10 ...
##  $ male                 : int  1 1 1 0 1 1 0 0 0 1 ...
##  $ raceeth              : chr  NA "White" "White" "Black" ...
##  $ preschool            : int  NA 0 1 1 1 1 0 1 1 1 ...
##  $ expectBachelors      : int  0 0 1 1 0 1 1 1 0 1 ...
##  $ motherHS             : int  NA 1 1 0 1 NA 1 1 1 1 ...
##  $ motherBachelors      : int  NA 1 1 0 0 NA 0 0 NA 1 ...
##  $ motherWork           : int  1 1 1 1 1 1 1 0 1 1 ...
##  $ fatherHS             : int  NA 1 1 1 1 1 NA 1 0 0 ...
##  $ fatherBachelors      : int  NA 0 NA 0 0 0 NA 0 NA 0 ...
##  $ fatherWork           : int  1 1 1 1 0 1 NA 1 1 1 ...
##  $ selfBornUS           : int  1 1 1 1 1 1 0 1 1 1 ...
##  $ motherBornUS         : int  0 1 1 1 1 1 1 1 1 1 ...
##  $ fatherBornUS         : int  0 1 1 1 0 1 NA 1 1 1 ...
##  $ englishAtHome        : int  0 1 1 1 1 1 1 1 1 1 ...
##  $ computerForSchoolwork: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ read30MinsADay       : int  0 1 0 1 1 0 0 1 0 0 ...
##  $ minutesPerWeekEnglish: int  225 450 250 200 250 300 250 300 378 294 ...
##  $ studentsInEnglish    : int  NA 25 28 23 35 20 28 30 20 24 ...
##  $ schoolHasLibrary     : int  1 1 1 1 1 1 1 1 0 1 ...
## NULL
## 'data.frame':	3663 obs. of  21 variables:
##  $ preschool            : int  NA 0 1 1 1 1 0 1 1 1 ...
##  $ expectBachelors      : int  0 0 1 1 0 1 1 1 0 1 ...
##  $ motherHS             : int  NA 1 1 0 1 NA 1 1 1 1 ...
##  $ motherBachelors      : int  NA 1 1 0 0 NA 0 0 NA 1 ...
##  $ motherWork           : int  1 1 1 1 1 1 1 0 1 1 ...
##  $ fatherHS             : int  NA 1 1 1 1 1 NA 1 0 0 ...
##  $ fatherBachelors      : int  NA 0 NA 0 0 0 NA 0 NA 0 ...
##  $ fatherWork           : int  1 1 1 1 0 1 NA 1 1 1 ...
##  $ selfBornUS           : int  1 1 1 1 1 1 0 1 1 1 ...
##  $ motherBornUS         : int  0 1 1 1 1 1 1 1 1 1 ...
##  $ fatherBornUS         : int  0 1 1 1 0 1 NA 1 1 1 ...
##  $ englishAtHome        : int  0 1 1 1 1 1 1 1 1 1 ...
##  $ computerForSchoolwork: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ read30MinsADay       : int  0 1 0 1 1 0 0 1 0 0 ...
##  $ minutesPerWeekEnglish: int  225 450 250 200 250 300 250 300 378 294 ...
##  $ studentsInEnglish    : int  NA 25 28 23 35 20 28 30 20 24 ...
##  $ schoolHasLibrary     : int  1 1 1 1 1 1 1 1 0 1 ...
##  $ publicSchool         : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ urban                : int  1 0 0 1 1 0 1 0 1 0 ...
##  $ schoolSize           : int  673 1173 1233 2640 1095 227 2080 1913 502 899 ...
##  $ readingScore         : num  476 575 555 458 614 ...
## NULL
```

```
## Warning in myprint_str_df(df): [list output truncated]
```

```r
if (glb_is_separate_predict_dataset) {
    glb_predct_df <- myimport_data(
        url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/pisa2009test.csv", 
        comment="glb_predct_df", force_header=TRUE, print_diagn=TRUE)
} else {
#     glb_predct_df <- subset(glb_entity_df, <condition>)
    glb_predct_df <- glb_entity_df[sample(1:nrow(glb_entity_df), nrow(glb_entity_df) / 1000),]
    comment(glb_predct_df) <- "glb_predct_df"
    myprint_df(glb_predct_df)
    str(glb_predct_df)
}         
```

```
## [1] "Reading file ./data/pisa2009test.csv..."
## [1] "dimensions of data in ./data/pisa2009test.csv: 1,570 rows x 24 cols"
##   grade male raceeth preschool expectBachelors motherHS motherBachelors
## 1    10    0   White         1               0        1               1
## 2    10    1   White         0               0        1               0
## 3    10    0   White         1               0        1               0
## 4    10    0   White         1               0        1               1
## 5    10    0   White         1               1        1               0
## 6    10    0   White         0               0        1               0
##   motherWork fatherHS fatherBachelors fatherWork selfBornUS motherBornUS
## 1          1        1               0          0          1            1
## 2          1        1               0          1          1            1
## 3          1        1               0          1          1            1
## 4          1        1               0          0          1            1
## 5          0        1               1          1          1            1
## 6          1        1               0          1          1            1
##   fatherBornUS englishAtHome computerForSchoolwork read30MinsADay
## 1            1             1                     1              0
## 2            1             1                     1              0
## 3            1             1                     1              0
## 4            1             1                     1              0
## 5            1             1                     1              0
## 6            1             1                     0              1
##   minutesPerWeekEnglish studentsInEnglish schoolHasLibrary publicSchool
## 1                   240                30                1            1
## 2                   255                NA                1            1
## 3                    NA                30                1            1
## 4                   160                30               NA            1
## 5                   240                30                1            1
## 6                   200                NA                1            1
##   urban schoolSize readingScore
## 1     0        808       355.24
## 2     0        808       385.57
## 3     0        808       522.62
## 4     0        808       406.24
## 5     0        808       453.50
## 6     0        808       437.78
##      grade male  raceeth preschool expectBachelors motherHS
## 55      11    0 Hispanic         1               0        1
## 239     10    1    White         1               1        1
## 511     10    1    White        NA               0        1
## 799     10    1 Hispanic         0               0        0
## 1142    11    0    White         0               1        1
## 1551    10    1 Hispanic         1               1        1
##      motherBachelors motherWork fatherHS fatherBachelors fatherWork
## 55                 1          1        0              NA          0
## 239                0          0        1               0          1
## 511               NA          1        1              NA          1
## 799                0          0        0               0          1
## 1142               1          1        1               1          1
## 1551               1          1        1               1          1
##      selfBornUS motherBornUS fatherBornUS englishAtHome
## 55            1            0            0             1
## 239           1            1            1             1
## 511           1            1            1             1
## 799           1            0            0             0
## 1142          1            1            1             1
## 1551          1            0            1             1
##      computerForSchoolwork read30MinsADay minutesPerWeekEnglish
## 55                       1              0                   280
## 239                      1              0                   300
## 511                      1              0                    NA
## 799                      1              0                   280
## 1142                     1              1                     0
## 1551                     1              0                   270
##      studentsInEnglish schoolHasLibrary publicSchool urban schoolSize
## 55                  25                1            1     1       3592
## 239                 30                1            1     0       1913
## 511                 NA                1            1     0       1750
## 799                 17                1            1     0        754
## 1142                20                1            1     0        865
## 1551                30                0            1     1       2999
##      readingScore
## 55         441.75
## 239        428.78
## 511        363.69
## 799        415.72
## 1142       434.04
## 1551       593.05
##      grade male  raceeth preschool expectBachelors motherHS
## 1565    10    0    White         1               1        1
## 1566     9    1    White         1               1        1
## 1567    11    0    White         1               0        1
## 1568    10    0 Hispanic         1               1        1
## 1569    10    0    White         1               1        1
## 1570    10    0    White         1               1        1
##      motherBachelors motherWork fatherHS fatherBachelors fatherWork
## 1565               1          1        1              NA          1
## 1566               0          1        0               0          1
## 1567               0         NA        1               0          1
## 1568              NA          1       NA              NA          1
## 1569               1          1        1               1          1
## 1570               0          1        1               1          1
##      selfBornUS motherBornUS fatherBornUS englishAtHome
## 1565          1            1            1             1
## 1566          1            1            1             1
## 1567          1            1            1             1
## 1568          1            1            1             1
## 1569          1            1            1             1
## 1570          1            1            1             1
##      computerForSchoolwork read30MinsADay minutesPerWeekEnglish
## 1565                     1              0                   450
## 1566                     1              0                   300
## 1567                     1              0                   450
## 1568                     1              0                    NA
## 1569                     1              1                   450
## 1570                     1              0                   450
##      studentsInEnglish schoolHasLibrary publicSchool urban schoolSize
## 1565                20                1            1     0        987
## 1566                20                1            1     0        987
## 1567                25                1            1     0        987
## 1568                NA                1            1     0        987
## 1569                20                1            1     0        987
## 1570                20                1            1     0        987
##      readingScore
## 1565       597.21
## 1566       465.58
## 1567       380.18
## 1568       324.10
## 1569       596.34
## 1570       577.43
## 'data.frame':	1570 obs. of  20 variables:
##  $ grade                : int  10 10 10 10 10 10 10 10 11 10 ...
##  $ male                 : int  0 1 0 0 0 0 0 0 0 1 ...
##  $ raceeth              : chr  "White" "White" "White" "White" ...
##  $ preschool            : int  1 0 1 1 1 0 1 1 0 1 ...
##  $ expectBachelors      : int  0 0 0 0 1 0 0 0 0 1 ...
##  $ motherHS             : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ motherBachelors      : int  1 0 0 1 0 0 0 0 1 1 ...
##  $ motherWork           : int  1 1 1 1 0 1 0 1 1 1 ...
##  $ fatherHS             : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ fatherBachelors      : int  0 0 0 0 1 0 0 0 1 0 ...
##  $ fatherWork           : int  0 1 1 0 1 1 0 1 1 1 ...
##  $ selfBornUS           : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ motherBornUS         : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ fatherBornUS         : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ englishAtHome        : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ computerForSchoolwork: int  1 1 1 1 1 0 1 1 1 1 ...
##  $ read30MinsADay       : int  0 0 0 0 0 1 1 1 1 1 ...
##  $ minutesPerWeekEnglish: int  240 255 NA 160 240 200 240 270 270 350 ...
##  $ studentsInEnglish    : int  30 NA 30 30 30 NA 30 35 30 25 ...
##  $ schoolHasLibrary     : int  1 1 1 NA 1 1 1 1 1 1 ...
## NULL
## 'data.frame':	1570 obs. of  21 variables:
##  $ preschool            : int  1 0 1 1 1 0 1 1 0 1 ...
##  $ expectBachelors      : int  0 0 0 0 1 0 0 0 0 1 ...
##  $ motherHS             : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ motherBachelors      : int  1 0 0 1 0 0 0 0 1 1 ...
##  $ motherWork           : int  1 1 1 1 0 1 0 1 1 1 ...
##  $ fatherHS             : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ fatherBachelors      : int  0 0 0 0 1 0 0 0 1 0 ...
##  $ fatherWork           : int  0 1 1 0 1 1 0 1 1 1 ...
##  $ selfBornUS           : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ motherBornUS         : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ fatherBornUS         : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ englishAtHome        : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ computerForSchoolwork: int  1 1 1 1 1 0 1 1 1 1 ...
##  $ read30MinsADay       : int  0 0 0 0 0 1 1 1 1 1 ...
##  $ minutesPerWeekEnglish: int  240 255 NA 160 240 200 240 270 270 350 ...
##  $ studentsInEnglish    : int  30 NA 30 30 30 NA 30 35 30 25 ...
##  $ schoolHasLibrary     : int  1 1 1 NA 1 1 1 1 1 1 ...
##  $ publicSchool         : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ urban                : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ schoolSize           : int  808 808 808 808 808 808 808 808 808 899 ...
##  $ readingScore         : num  355 386 523 406 454 ...
## NULL
```

```
## Warning in myprint_str_df(df): [list output truncated]
```

```r
# glb_entity_df <- subset(glb_entity_df, !<condition>)
# print(dim(glb_entity_df))

script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# grade: The grade in school of the student (most 15-year-olds in America are in 10th grade)
# male: Whether the student is male (1/0)
# raceeth: The race/ethnicity composite of the student
# preschool: Whether the student attended preschool (1/0)
# expectBachelors: Whether the student expects to obtain a bachelor's degree (1/0)
# motherHS: Whether the student's mother completed high school (1/0)
# motherBachelors: Whether the student's mother obtained a bachelor's degree (1/0)
# motherWork: Whether the student's mother has part-time or full-time work (1/0)
# fatherHS: Whether the student's father completed high school (1/0)
# fatherBachelors: Whether the student's father obtained a bachelor's degree (1/0)
# fatherWork: Whether the student's father has part-time or full-time work (1/0)
# selfBornUS: Whether the student was born in the United States of America (1/0)
# motherBornUS: Whether the student's mother was born in the United States of America (1/0)
# fatherBornUS: Whether the student's father was born in the United States of America (1/0)
# englishAtHome: Whether the student speaks English at home (1/0)
# computerForSchoolwork: Whether the student has access to a computer for schoolwork (1/0)
# read30MinsADay: Whether the student reads for pleasure for 30 minutes/day (1/0)
# minutesPerWeekEnglish: The number of minutes per week the student spend in English class
# studentsInEnglish: The number of students in this student's English class at school
# schoolHasLibrary: Whether this student's school has a library (1/0)
# publicSchool: Whether this student attends a public school (1/0)
# urban: Whether this student's school is in an urban area (1/0)
# schoolSize: The number of students in this student's school
# readingScore: The student's reading score, on a 1000-point scale

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, obs_twin_df) {
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>)

        raceeth.fctr=relevel(factor(raceeth, 
                    as.factor(union(obs_df$raceeth, obs_twin_df$raceeth))), 
                    "White") 

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)
        
                        )

    # If levels of a factor are different across obs_df & glb_predct_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df, glb_predct_df)
```

```
##      grade            male          raceeth            preschool     
##  Min.   : 8.00   Min.   :0.0000   Length:3663        Min.   :0.0000  
##  1st Qu.:10.00   1st Qu.:0.0000   Class :character   1st Qu.:0.0000  
##  Median :10.00   Median :1.0000   Mode  :character   Median :1.0000  
##  Mean   :10.09   Mean   :0.5111                      Mean   :0.7228  
##  3rd Qu.:10.00   3rd Qu.:1.0000                      3rd Qu.:1.0000  
##  Max.   :12.00   Max.   :1.0000                      Max.   :1.0000  
##                                                      NA's   :56      
##  expectBachelors     motherHS    motherBachelors    motherWork    
##  Min.   :0.0000   Min.   :0.00   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:1.0000   1st Qu.:1.00   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :1.0000   Median :1.00   Median :0.0000   Median :1.0000  
##  Mean   :0.7859   Mean   :0.88   Mean   :0.3481   Mean   :0.7345  
##  3rd Qu.:1.0000   3rd Qu.:1.00   3rd Qu.:1.0000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :1.00   Max.   :1.0000   Max.   :1.0000  
##  NA's   :62       NA's   :97     NA's   :397      NA's   :93      
##     fatherHS      fatherBachelors    fatherWork       selfBornUS    
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:1.0000  
##  Median :1.0000   Median :0.0000   Median :1.0000   Median :1.0000  
##  Mean   :0.8593   Mean   :0.3319   Mean   :0.8531   Mean   :0.9313  
##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##  NA's   :245      NA's   :569      NA's   :233      NA's   :69      
##   motherBornUS     fatherBornUS    englishAtHome    computerForSchoolwork
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000       
##  1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:1.0000       
##  Median :1.0000   Median :1.0000   Median :1.0000   Median :1.0000       
##  Mean   :0.7725   Mean   :0.7668   Mean   :0.8717   Mean   :0.8994       
##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000       
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000       
##  NA's   :71       NA's   :113      NA's   :71       NA's   :65           
##  read30MinsADay   minutesPerWeekEnglish studentsInEnglish schoolHasLibrary
##  Min.   :0.0000   Min.   :   0.0        Min.   : 1.0      Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.: 225.0        1st Qu.:20.0      1st Qu.:1.0000  
##  Median :0.0000   Median : 250.0        Median :25.0      Median :1.0000  
##  Mean   :0.2899   Mean   : 266.2        Mean   :24.5      Mean   :0.9676  
##  3rd Qu.:1.0000   3rd Qu.: 300.0        3rd Qu.:30.0      3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :2400.0        Max.   :75.0      Max.   :1.0000  
##  NA's   :34       NA's   :186           NA's   :249       NA's   :143     
##   publicSchool        urban          schoolSize    readingScore  
##  Min.   :0.0000   Min.   :0.0000   Min.   : 100   Min.   :168.6  
##  1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.: 712   1st Qu.:431.7  
##  Median :1.0000   Median :0.0000   Median :1212   Median :499.7  
##  Mean   :0.9339   Mean   :0.3849   Mean   :1369   Mean   :497.9  
##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1900   3rd Qu.:566.2  
##  Max.   :1.0000   Max.   :1.0000   Max.   :6694   Max.   :746.0  
##                                    NA's   :162                   
##              raceeth.fctr 
##  White             :2015  
##  Hispanic          : 834  
##  Black             : 444  
##  Asian             : 143  
##  More than one race: 124  
##  (Other)           :  68  
##  NA's              :  35  
##                 grade                  male               raceeth 
##                     0                     0                    35 
##             preschool       expectBachelors              motherHS 
##                    56                    62                    97 
##       motherBachelors            motherWork              fatherHS 
##                   397                    93                   245 
##       fatherBachelors            fatherWork            selfBornUS 
##                   569                   233                    69 
##          motherBornUS          fatherBornUS         englishAtHome 
##                    71                   113                    71 
## computerForSchoolwork        read30MinsADay minutesPerWeekEnglish 
##                    65                    34                   186 
##     studentsInEnglish      schoolHasLibrary          publicSchool 
##                   249                   143                     0 
##                 urban            schoolSize          readingScore 
##                     0                   162                     0 
##          raceeth.fctr 
##                    35
```

```r
glb_predct_df <- add_new_diag_feats(glb_predct_df, glb_entity_df)
```

```
##      grade            male          raceeth            preschool     
##  Min.   : 9.00   Min.   :0.0000   Length:1570        Min.   :0.0000  
##  1st Qu.:10.00   1st Qu.:0.0000   Class :character   1st Qu.:0.0000  
##  Median :10.00   Median :1.0000   Mode  :character   Median :1.0000  
##  Mean   :10.09   Mean   :0.5191                      Mean   :0.7108  
##  3rd Qu.:10.00   3rd Qu.:1.0000                      3rd Qu.:1.0000  
##  Max.   :12.00   Max.   :1.0000                      Max.   :1.0000  
##                                                      NA's   :21      
##  expectBachelors     motherHS      motherBachelors    motherWork   
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000  
##  1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.000  
##  Median :1.0000   Median :1.0000   Median :0.0000   Median :1.000  
##  Mean   :0.7673   Mean   :0.8682   Mean   :0.3307   Mean   :0.719  
##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.000  
##  NA's   :23       NA's   :45       NA's   :188      NA's   :36     
##     fatherHS      fatherBachelors    fatherWork       selfBornUS    
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:1.0000   1st Qu.:1.0000  
##  Median :1.0000   Median :0.0000   Median :1.0000   Median :1.0000  
##  Mean   :0.8484   Mean   :0.3253   Mean   :0.8435   Mean   :0.9127  
##  3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##  NA's   :125      NA's   :288      NA's   :113      NA's   :24      
##   motherBornUS    fatherBornUS    englishAtHome    computerForSchoolwork
##  Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000       
##  1st Qu.:1.000   1st Qu.:1.0000   1st Qu.:1.0000   1st Qu.:1.0000       
##  Median :1.000   Median :1.0000   Median :1.0000   Median :1.0000       
##  Mean   :0.766   Mean   :0.7659   Mean   :0.8652   Mean   :0.8981       
##  3rd Qu.:1.000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000       
##  Max.   :1.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000       
##  NA's   :23      NA's   :58       NA's   :27       NA's   :30           
##  read30MinsADay   minutesPerWeekEnglish studentsInEnglish schoolHasLibrary
##  Min.   :0.0000   Min.   :   0.0        Min.   : 1.0      Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.: 225.0        1st Qu.:20.0      1st Qu.:1.0000  
##  Median :0.0000   Median : 250.0        Median :25.0      Median :1.0000  
##  Mean   :0.2828   Mean   : 264.6        Mean   :24.7      Mean   :0.9623  
##  3rd Qu.:1.0000   3rd Qu.: 300.0        3rd Qu.:30.0      3rd Qu.:1.0000  
##  Max.   :1.0000   Max.   :2025.0        Max.   :90.0      Max.   :1.0000  
##  NA's   :21       NA's   :103           NA's   :114       NA's   :58      
##   publicSchool        urban         schoolSize    readingScore  
##  Min.   :0.0000   Min.   :0.000   Min.   : 100   Min.   :156.4  
##  1st Qu.:1.0000   1st Qu.:0.000   1st Qu.: 762   1st Qu.:430.5  
##  Median :1.0000   Median :0.000   Median :1273   Median :499.5  
##  Mean   :0.9344   Mean   :0.379   Mean   :1386   Mean   :496.8  
##  3rd Qu.:1.0000   3rd Qu.:1.000   3rd Qu.:1900   3rd Qu.:562.7  
##  Max.   :1.0000   Max.   :1.000   Max.   :6694   Max.   :772.5  
##                                   NA's   :69                    
##              raceeth.fctr
##  White             :879  
##  Hispanic          :350  
##  Black             :191  
##  Asian             : 61  
##  More than one race: 53  
##  (Other)           : 23  
##  NA's              : 13  
##                 grade                  male               raceeth 
##                     0                     0                    13 
##             preschool       expectBachelors              motherHS 
##                    21                    23                    45 
##       motherBachelors            motherWork              fatherHS 
##                   188                    36                   125 
##       fatherBachelors            fatherWork            selfBornUS 
##                   288                   113                    24 
##          motherBornUS          fatherBornUS         englishAtHome 
##                    23                    58                    27 
## computerForSchoolwork        read30MinsADay minutesPerWeekEnglish 
##                    30                    21                   103 
##     studentsInEnglish      schoolHasLibrary          publicSchool 
##                   114                    58                     0 
##                 urban            schoolSize          readingScore 
##                     0                    69                     0 
##          raceeth.fctr 
##                    13
```

```r
#pairs(subset(glb_entity_df, select=-c(col_symbol)))

#   Histogram of predictor in glb_entity_df & glb_predct_df
# Check for glb_predct_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(xtabs(~ <col1_name>, glb_entity_df))
# print(xtabs(~ <col1_name> + <col2_name>, glb_entity_df))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

print(readingScore_mean_entity_arr <- 
   sort(tapply(glb_entity_df$readingScore, glb_entity_df$male, mean, na.rm=TRUE)))
```

```
##        1        0 
## 483.5325 512.9406
```

```r
# print(readingScore_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>.NA, glb_entity_df$<col2_name>, mean, na.rm=TRUE)))


# Other plots:
# print(myplot_histogram(glb_entity_df, "<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>"))

script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
glb_entity_df <- na.omit(glb_entity_df)
glb_predct_df <- na.omit(glb_predct_df)

script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_predct_df <- mymap_codes(glb_predct_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_entity_df$<col_name>.fctr <- factor(glb_entity_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_predct_df$<col_name>)))
# glb_predct_df$<col_name>.fctr <- factor(glb_predct_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_predct_df$<col_name>)))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
```

## Step `3`: extract features

```r
# Create new features that help prediction
# glb_entity_df <- mutate(glb_entity_df,
#     <new_col_name>=
#                     )

# glb_predct_df <- mutate(glb_predct_df,
#     <new_col_name>=
#                     )

# print(summary(glb_entity_df))
# print(summary(glb_predct_df))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
## 7      select_features                4                0
```

## Step `4`: select features

```r
print(glb_feats_df <- myselect_features())
```

```
##                                          id       cor.y  cor.y.abs
## expectBachelors             expectBachelors  0.34430376 0.34430376
## fatherBachelors             fatherBachelors  0.27697819 0.27697819
## grade                                 grade  0.23868401 0.23868401
## read30MinsADay               read30MinsADay  0.23832990 0.23832990
## motherBachelors             motherBachelors  0.22676151 0.22676151
## computerForSchoolwork computerForSchoolwork  0.18120243 0.18120243
## fatherHS                           fatherHS  0.16882677 0.16882677
## motherHS                           motherHS  0.13898290 0.13898290
## male                                   male -0.12845812 0.12845812
## publicSchool                   publicSchool -0.11166900 0.11166900
## englishAtHome                 englishAtHome  0.10774742 0.10774742
## fatherWork                       fatherWork  0.07623819 0.07623819
## fatherBornUS                   fatherBornUS  0.06503407 0.06503407
## motherBornUS                   motherBornUS  0.05145971 0.05145971
## preschool                         preschool  0.05128936 0.05128936
## minutesPerWeekEnglish minutesPerWeekEnglish  0.03646560 0.03646560
## motherWork                       motherWork  0.02446121 0.02446121
## schoolHasLibrary           schoolHasLibrary  0.02355761 0.02355761
## schoolSize                       schoolSize  0.02050924 0.02050924
## urban                                 urban -0.01601687 0.01601687
## selfBornUS                       selfBornUS  0.01344333 0.01344333
## studentsInEnglish         studentsInEnglish  0.01025221 0.01025221
```

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, 
                    merge(glb_feats_df, mydelete_cor_features(), all.x=TRUE)))
```

```
## Loading required package: reshape2
```

```
##                       expectBachelors fatherBachelors        grade
## expectBachelors           1.000000000     0.201927970  0.111908245
## fatherBachelors           0.201927970     1.000000000  0.056086980
## grade                     0.111908245     0.056086980  1.000000000
## read30MinsADay            0.127765509     0.046179088  0.039723965
## motherBachelors           0.158609084     0.551442896  0.029744703
## computerForSchoolwork     0.148996604     0.165888481  0.067693475
## fatherHS                  0.159938986     0.267060004  0.027234796
## motherHS                  0.110998866     0.226294277 -0.005934161
## male                     -0.092480475     0.080779286 -0.094844648
## publicSchool             -0.109273432    -0.179153876 -0.042782297
## englishAtHome             0.053815253     0.138923281 -0.029076437
## fatherWork                0.015386750     0.104814574  0.005859578
## fatherBornUS             -0.018543095     0.098400961 -0.082089212
## motherBornUS             -0.008232966     0.084592050 -0.083493202
## preschool                 0.084994244     0.152760694  0.005577343
## minutesPerWeekEnglish     0.017679381    -0.002485598  0.017256498
## motherWork                0.076515530     0.059682214  0.004685733
## schoolHasLibrary          0.030533986     0.005418711 -0.034610260
## schoolSize                0.032910372     0.009340037  0.073231222
## urban                     0.021209172    -0.018416000  0.081234839
## selfBornUS               -0.016037463     0.027220115 -0.043886766
## studentsInEnglish         0.037771094    -0.033021777  0.065091947
##                       read30MinsADay motherBachelors computerForSchoolwork
## expectBachelors         0.1277655089     0.158609084          0.1489966037
## fatherBachelors         0.0461790881     0.551442896          0.1658884814
## grade                   0.0397239650     0.029744703          0.0676934748
## read30MinsADay          1.0000000000     0.024800603         -0.0177796343
## motherBachelors         0.0248006031     1.000000000          0.1399247526
## computerForSchoolwork  -0.0177796343     0.139924753          1.0000000000
## fatherHS                0.0208902447     0.183158194          0.1674833486
## motherHS                0.0109271086     0.246264547          0.1648666540
## male                   -0.2128472594     0.082509576          0.0007551518
## publicSchool            0.0098892970    -0.167887725         -0.0748209642
## englishAtHome           0.0006988334     0.157266462          0.0637397500
## fatherWork              0.0310592270     0.074994105          0.0972139867
## fatherBornUS            0.0092682679     0.112014223          0.0008175299
## motherBornUS           -0.0002258194     0.136148617          0.0005664754
## preschool              -0.0153331398     0.157263805          0.1016585555
## minutesPerWeekEnglish   0.0314942353     0.019152825         -0.0212173493
## motherWork             -0.0667223254     0.138744176          0.0779677192
## schoolHasLibrary        0.0097970041     0.005664362         -0.0163649650
## schoolSize             -0.0158591506    -0.007007003          0.0753204765
## urban                   0.0184361496    -0.024376314          0.0465480932
## selfBornUS             -0.0020593777     0.042323409          0.0121024202
## studentsInEnglish       0.0078286598    -0.044909467          0.0563925175
##                          fatherHS     motherHS          male publicSchool
## expectBachelors        0.15993899  0.110998866 -0.0924804746 -0.109273432
## fatherBachelors        0.26706000  0.226294277  0.0807792859 -0.179153876
## grade                  0.02723480 -0.005934161 -0.0948446476 -0.042782297
## read30MinsADay         0.02089024  0.010927109 -0.2128472594  0.009889297
## motherBachelors        0.18315819  0.246264547  0.0825095756 -0.167887725
## computerForSchoolwork  0.16748335  0.164866654  0.0007551518 -0.074820964
## fatherHS               1.00000000  0.496570370  0.0384014319 -0.072912901
## motherHS               0.49657037  1.000000000  0.0320617406 -0.062627880
## male                   0.03840143  0.032061741  1.0000000000 -0.091134322
## publicSchool          -0.07291290 -0.062627880 -0.0911343223  1.000000000
## englishAtHome          0.34379500  0.387462047 -0.0067798193 -0.049294970
## fatherWork             0.07689286  0.004374140  0.0424415788 -0.027716820
## fatherBornUS           0.33384721  0.337663426  0.0093716409  0.023107896
## motherBornUS           0.30087149  0.357459473  0.0165366121  0.008153869
## preschool              0.11534892  0.138932025  0.0108245460 -0.098916076
## minutesPerWeekEnglish  0.01175479 -0.001194904 -0.0075756313  0.065916099
## motherWork             0.10945534  0.159007741 -0.0135413380 -0.039602868
## schoolHasLibrary       0.02480741  0.006724287  0.0476608142  0.075132838
## schoolSize            -0.07646839 -0.069267119  0.0111585056  0.261285888
## urban                 -0.08487069 -0.107020782  0.0463699479 -0.334509892
## selfBornUS             0.16148067  0.188745981  0.0345501475 -0.047426843
## studentsInEnglish     -0.02380552 -0.035084104 -0.0327939896  0.106491004
##                       englishAtHome   fatherWork  fatherBornUS
## expectBachelors        0.0538152532  0.015386750 -0.0185430946
## fatherBachelors        0.1389232809  0.104814574  0.0984009608
## grade                 -0.0290764366  0.005859578 -0.0820892124
## read30MinsADay         0.0006988334  0.031059227  0.0092682679
## motherBachelors        0.1572664618  0.074994105  0.1120142233
## computerForSchoolwork  0.0637397500  0.097213987  0.0008175299
## fatherHS               0.3437950031  0.076892860  0.3338472106
## motherHS               0.3874620474  0.004374140  0.3376634262
## male                  -0.0067798193  0.042441579  0.0093716409
## publicSchool          -0.0492949697 -0.027716820  0.0231078964
## englishAtHome          1.0000000000  0.018773770  0.6264400692
## fatherWork             0.0187737699  1.000000000 -0.0058546302
## fatherBornUS           0.6264400692 -0.005854630  1.0000000000
## motherBornUS           0.6543533106 -0.007144167  0.7709450509
## preschool              0.1210285360  0.071665352  0.0992669549
## minutesPerWeekEnglish -0.0177885115  0.007235913  0.0078386569
## motherWork             0.1378257380  0.053199294  0.0826022203
## schoolHasLibrary      -0.0090373568 -0.020323743 -0.0109372081
## schoolSize            -0.2114827171 -0.012036739 -0.2625429923
## urban                 -0.2031800241 -0.029059917 -0.2812637184
## selfBornUS             0.4760179049 -0.019416960  0.4539953250
## studentsInEnglish     -0.0656052474 -0.005077876 -0.0960273188
##                        motherBornUS    preschool minutesPerWeekEnglish
## expectBachelors       -0.0082329663  0.084994244           0.017679381
## fatherBachelors        0.0845920499  0.152760694          -0.002485598
## grade                 -0.0834932018  0.005577343           0.017256498
## read30MinsADay        -0.0002258194 -0.015333140           0.031494235
## motherBachelors        0.1361486166  0.157263805           0.019152825
## computerForSchoolwork  0.0005664754  0.101658555          -0.021217349
## fatherHS               0.3008714912  0.115348917           0.011754789
## motherHS               0.3574594729  0.138932025          -0.001194904
## male                   0.0165366121  0.010824546          -0.007575631
## publicSchool           0.0081538691 -0.098916076           0.065916099
## englishAtHome          0.6543533106  0.121028536          -0.017788512
## fatherWork            -0.0071441671  0.071665352           0.007235913
## fatherBornUS           0.7709450509  0.099266955           0.007838657
## motherBornUS           1.0000000000  0.104612307           0.010906882
## preschool              0.1046123073  1.000000000          -0.030280961
## minutesPerWeekEnglish  0.0109068821 -0.030280961           1.000000000
## motherWork             0.0968764560  0.076153628           0.019266113
## schoolHasLibrary      -0.0091043265  0.012239077           0.001533587
## schoolSize            -0.2537631141 -0.026636887           0.004807711
## urban                 -0.2559637739 -0.019779009          -0.054023025
## selfBornUS             0.4604887273  0.091448862          -0.011278415
## studentsInEnglish     -0.1025868080 -0.025684806           0.047537258
##                         motherWork schoolHasLibrary   schoolSize
## expectBachelors        0.076515530      0.030533986  0.032910372
## fatherBachelors        0.059682214      0.005418711  0.009340037
## grade                  0.004685733     -0.034610260  0.073231222
## read30MinsADay        -0.066722325      0.009797004 -0.015859151
## motherBachelors        0.138744176      0.005664362 -0.007007003
## computerForSchoolwork  0.077967719     -0.016364965  0.075320476
## fatherHS               0.109455343      0.024807414 -0.076468388
## motherHS               0.159007741      0.006724287 -0.069267119
## male                  -0.013541338      0.047660814  0.011158506
## publicSchool          -0.039602868      0.075132838  0.261285888
## englishAtHome          0.137825738     -0.009037357 -0.211482717
## fatherWork             0.053199294     -0.020323743 -0.012036739
## fatherBornUS           0.082602220     -0.010937208 -0.262542992
## motherBornUS           0.096876456     -0.009104327 -0.253763114
## preschool              0.076153628      0.012239077 -0.026636887
## minutesPerWeekEnglish  0.019266113      0.001533587  0.004807711
## motherWork             1.000000000     -0.012606807 -0.049533585
## schoolHasLibrary      -0.012606807      1.000000000  0.079554898
## schoolSize            -0.049533585      0.079554898  1.000000000
## urban                 -0.008753962     -0.134225029  0.317388020
## selfBornUS             0.058810711     -0.004087532 -0.118650683
## studentsInEnglish     -0.035975554      0.096309537  0.296303095
##                              urban   selfBornUS studentsInEnglish
## expectBachelors        0.021209172 -0.016037463       0.037771094
## fatherBachelors       -0.018416000  0.027220115      -0.033021777
## grade                  0.081234839 -0.043886766       0.065091947
## read30MinsADay         0.018436150 -0.002059378       0.007828660
## motherBachelors       -0.024376314  0.042323409      -0.044909467
## computerForSchoolwork  0.046548093  0.012102420       0.056392517
## fatherHS              -0.084870694  0.161480674      -0.023805522
## motherHS              -0.107020782  0.188745981      -0.035084104
## male                   0.046369948  0.034550147      -0.032793990
## publicSchool          -0.334509892 -0.047426843       0.106491004
## englishAtHome         -0.203180024  0.476017905      -0.065605247
## fatherWork            -0.029059917 -0.019416960      -0.005077876
## fatherBornUS          -0.281263718  0.453995325      -0.096027319
## motherBornUS          -0.255963774  0.460488727      -0.102586808
## preschool             -0.019779009  0.091448862      -0.025684806
## minutesPerWeekEnglish -0.054023025 -0.011278415       0.047537258
## motherWork            -0.008753962  0.058810711      -0.035975554
## schoolHasLibrary      -0.134225029 -0.004087532       0.096309537
## schoolSize             0.317388020 -0.118650683       0.296303095
## urban                  1.000000000 -0.113217557       0.154293355
## selfBornUS            -0.113217557  1.000000000      -0.013601249
## studentsInEnglish      0.154293355 -0.013601249       1.000000000
##                       expectBachelors fatherBachelors       grade
## expectBachelors           0.000000000     0.201927970 0.111908245
## fatherBachelors           0.201927970     0.000000000 0.056086980
## grade                     0.111908245     0.056086980 0.000000000
## read30MinsADay            0.127765509     0.046179088 0.039723965
## motherBachelors           0.158609084     0.551442896 0.029744703
## computerForSchoolwork     0.148996604     0.165888481 0.067693475
## fatherHS                  0.159938986     0.267060004 0.027234796
## motherHS                  0.110998866     0.226294277 0.005934161
## male                      0.092480475     0.080779286 0.094844648
## publicSchool              0.109273432     0.179153876 0.042782297
## englishAtHome             0.053815253     0.138923281 0.029076437
## fatherWork                0.015386750     0.104814574 0.005859578
## fatherBornUS              0.018543095     0.098400961 0.082089212
## motherBornUS              0.008232966     0.084592050 0.083493202
## preschool                 0.084994244     0.152760694 0.005577343
## minutesPerWeekEnglish     0.017679381     0.002485598 0.017256498
## motherWork                0.076515530     0.059682214 0.004685733
## schoolHasLibrary          0.030533986     0.005418711 0.034610260
## schoolSize                0.032910372     0.009340037 0.073231222
## urban                     0.021209172     0.018416000 0.081234839
## selfBornUS                0.016037463     0.027220115 0.043886766
## studentsInEnglish         0.037771094     0.033021777 0.065091947
##                       read30MinsADay motherBachelors computerForSchoolwork
## expectBachelors         0.1277655089     0.158609084          0.1489966037
## fatherBachelors         0.0461790881     0.551442896          0.1658884814
## grade                   0.0397239650     0.029744703          0.0676934748
## read30MinsADay          0.0000000000     0.024800603          0.0177796343
## motherBachelors         0.0248006031     0.000000000          0.1399247526
## computerForSchoolwork   0.0177796343     0.139924753          0.0000000000
## fatherHS                0.0208902447     0.183158194          0.1674833486
## motherHS                0.0109271086     0.246264547          0.1648666540
## male                    0.2128472594     0.082509576          0.0007551518
## publicSchool            0.0098892970     0.167887725          0.0748209642
## englishAtHome           0.0006988334     0.157266462          0.0637397500
## fatherWork              0.0310592270     0.074994105          0.0972139867
## fatherBornUS            0.0092682679     0.112014223          0.0008175299
## motherBornUS            0.0002258194     0.136148617          0.0005664754
## preschool               0.0153331398     0.157263805          0.1016585555
## minutesPerWeekEnglish   0.0314942353     0.019152825          0.0212173493
## motherWork              0.0667223254     0.138744176          0.0779677192
## schoolHasLibrary        0.0097970041     0.005664362          0.0163649650
## schoolSize              0.0158591506     0.007007003          0.0753204765
## urban                   0.0184361496     0.024376314          0.0465480932
## selfBornUS              0.0020593777     0.042323409          0.0121024202
## studentsInEnglish       0.0078286598     0.044909467          0.0563925175
##                         fatherHS    motherHS         male publicSchool
## expectBachelors       0.15993899 0.110998866 0.0924804746  0.109273432
## fatherBachelors       0.26706000 0.226294277 0.0807792859  0.179153876
## grade                 0.02723480 0.005934161 0.0948446476  0.042782297
## read30MinsADay        0.02089024 0.010927109 0.2128472594  0.009889297
## motherBachelors       0.18315819 0.246264547 0.0825095756  0.167887725
## computerForSchoolwork 0.16748335 0.164866654 0.0007551518  0.074820964
## fatherHS              0.00000000 0.496570370 0.0384014319  0.072912901
## motherHS              0.49657037 0.000000000 0.0320617406  0.062627880
## male                  0.03840143 0.032061741 0.0000000000  0.091134322
## publicSchool          0.07291290 0.062627880 0.0911343223  0.000000000
## englishAtHome         0.34379500 0.387462047 0.0067798193  0.049294970
## fatherWork            0.07689286 0.004374140 0.0424415788  0.027716820
## fatherBornUS          0.33384721 0.337663426 0.0093716409  0.023107896
## motherBornUS          0.30087149 0.357459473 0.0165366121  0.008153869
## preschool             0.11534892 0.138932025 0.0108245460  0.098916076
## minutesPerWeekEnglish 0.01175479 0.001194904 0.0075756313  0.065916099
## motherWork            0.10945534 0.159007741 0.0135413380  0.039602868
## schoolHasLibrary      0.02480741 0.006724287 0.0476608142  0.075132838
## schoolSize            0.07646839 0.069267119 0.0111585056  0.261285888
## urban                 0.08487069 0.107020782 0.0463699479  0.334509892
## selfBornUS            0.16148067 0.188745981 0.0345501475  0.047426843
## studentsInEnglish     0.02380552 0.035084104 0.0327939896  0.106491004
##                       englishAtHome  fatherWork fatherBornUS motherBornUS
## expectBachelors        0.0538152532 0.015386750 0.0185430946 0.0082329663
## fatherBachelors        0.1389232809 0.104814574 0.0984009608 0.0845920499
## grade                  0.0290764366 0.005859578 0.0820892124 0.0834932018
## read30MinsADay         0.0006988334 0.031059227 0.0092682679 0.0002258194
## motherBachelors        0.1572664618 0.074994105 0.1120142233 0.1361486166
## computerForSchoolwork  0.0637397500 0.097213987 0.0008175299 0.0005664754
## fatherHS               0.3437950031 0.076892860 0.3338472106 0.3008714912
## motherHS               0.3874620474 0.004374140 0.3376634262 0.3574594729
## male                   0.0067798193 0.042441579 0.0093716409 0.0165366121
## publicSchool           0.0492949697 0.027716820 0.0231078964 0.0081538691
## englishAtHome          0.0000000000 0.018773770 0.6264400692 0.6543533106
## fatherWork             0.0187737699 0.000000000 0.0058546302 0.0071441671
## fatherBornUS           0.6264400692 0.005854630 0.0000000000 0.7709450509
## motherBornUS           0.6543533106 0.007144167 0.7709450509 0.0000000000
## preschool              0.1210285360 0.071665352 0.0992669549 0.1046123073
## minutesPerWeekEnglish  0.0177885115 0.007235913 0.0078386569 0.0109068821
## motherWork             0.1378257380 0.053199294 0.0826022203 0.0968764560
## schoolHasLibrary       0.0090373568 0.020323743 0.0109372081 0.0091043265
## schoolSize             0.2114827171 0.012036739 0.2625429923 0.2537631141
## urban                  0.2031800241 0.029059917 0.2812637184 0.2559637739
## selfBornUS             0.4760179049 0.019416960 0.4539953250 0.4604887273
## studentsInEnglish      0.0656052474 0.005077876 0.0960273188 0.1025868080
##                         preschool minutesPerWeekEnglish  motherWork
## expectBachelors       0.084994244           0.017679381 0.076515530
## fatherBachelors       0.152760694           0.002485598 0.059682214
## grade                 0.005577343           0.017256498 0.004685733
## read30MinsADay        0.015333140           0.031494235 0.066722325
## motherBachelors       0.157263805           0.019152825 0.138744176
## computerForSchoolwork 0.101658555           0.021217349 0.077967719
## fatherHS              0.115348917           0.011754789 0.109455343
## motherHS              0.138932025           0.001194904 0.159007741
## male                  0.010824546           0.007575631 0.013541338
## publicSchool          0.098916076           0.065916099 0.039602868
## englishAtHome         0.121028536           0.017788512 0.137825738
## fatherWork            0.071665352           0.007235913 0.053199294
## fatherBornUS          0.099266955           0.007838657 0.082602220
## motherBornUS          0.104612307           0.010906882 0.096876456
## preschool             0.000000000           0.030280961 0.076153628
## minutesPerWeekEnglish 0.030280961           0.000000000 0.019266113
## motherWork            0.076153628           0.019266113 0.000000000
## schoolHasLibrary      0.012239077           0.001533587 0.012606807
## schoolSize            0.026636887           0.004807711 0.049533585
## urban                 0.019779009           0.054023025 0.008753962
## selfBornUS            0.091448862           0.011278415 0.058810711
## studentsInEnglish     0.025684806           0.047537258 0.035975554
##                       schoolHasLibrary  schoolSize       urban  selfBornUS
## expectBachelors            0.030533986 0.032910372 0.021209172 0.016037463
## fatherBachelors            0.005418711 0.009340037 0.018416000 0.027220115
## grade                      0.034610260 0.073231222 0.081234839 0.043886766
## read30MinsADay             0.009797004 0.015859151 0.018436150 0.002059378
## motherBachelors            0.005664362 0.007007003 0.024376314 0.042323409
## computerForSchoolwork      0.016364965 0.075320476 0.046548093 0.012102420
## fatherHS                   0.024807414 0.076468388 0.084870694 0.161480674
## motherHS                   0.006724287 0.069267119 0.107020782 0.188745981
## male                       0.047660814 0.011158506 0.046369948 0.034550147
## publicSchool               0.075132838 0.261285888 0.334509892 0.047426843
## englishAtHome              0.009037357 0.211482717 0.203180024 0.476017905
## fatherWork                 0.020323743 0.012036739 0.029059917 0.019416960
## fatherBornUS               0.010937208 0.262542992 0.281263718 0.453995325
## motherBornUS               0.009104327 0.253763114 0.255963774 0.460488727
## preschool                  0.012239077 0.026636887 0.019779009 0.091448862
## minutesPerWeekEnglish      0.001533587 0.004807711 0.054023025 0.011278415
## motherWork                 0.012606807 0.049533585 0.008753962 0.058810711
## schoolHasLibrary           0.000000000 0.079554898 0.134225029 0.004087532
## schoolSize                 0.079554898 0.000000000 0.317388020 0.118650683
## urban                      0.134225029 0.317388020 0.000000000 0.113217557
## selfBornUS                 0.004087532 0.118650683 0.113217557 0.000000000
## studentsInEnglish          0.096309537 0.296303095 0.154293355 0.013601249
##                       studentsInEnglish
## expectBachelors             0.037771094
## fatherBachelors             0.033021777
## grade                       0.065091947
## read30MinsADay              0.007828660
## motherBachelors             0.044909467
## computerForSchoolwork       0.056392517
## fatherHS                    0.023805522
## motherHS                    0.035084104
## male                        0.032793990
## publicSchool                0.106491004
## englishAtHome               0.065605247
## fatherWork                  0.005077876
## fatherBornUS                0.096027319
## motherBornUS                0.102586808
## preschool                   0.025684806
## minutesPerWeekEnglish       0.047537258
## motherWork                  0.035975554
## schoolHasLibrary            0.096309537
## schoolSize                  0.296303095
## urban                       0.154293355
## selfBornUS                  0.013601249
## studentsInEnglish           0.000000000
## [1] "cor(fatherBornUS, motherBornUS)=0.7709"
```

![](NCES_PISA_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(readingScore, fatherBornUS)=0.0650"
## [1] "cor(readingScore, motherBornUS)=0.0515"
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

```
## Warning in mydelete_cor_features(): Dropping motherBornUS as a feature
```

![](NCES_PISA_files/figure-html/remove_correlated_features-2.png) 

```
##                                          id       cor.y  cor.y.abs
## expectBachelors             expectBachelors  0.34430376 0.34430376
## fatherBachelors             fatherBachelors  0.27697819 0.27697819
## grade                                 grade  0.23868401 0.23868401
## read30MinsADay               read30MinsADay  0.23832990 0.23832990
## motherBachelors             motherBachelors  0.22676151 0.22676151
## computerForSchoolwork computerForSchoolwork  0.18120243 0.18120243
## fatherHS                           fatherHS  0.16882677 0.16882677
## motherHS                           motherHS  0.13898290 0.13898290
## male                                   male -0.12845812 0.12845812
## publicSchool                   publicSchool -0.11166900 0.11166900
## englishAtHome                 englishAtHome  0.10774742 0.10774742
## fatherWork                       fatherWork  0.07623819 0.07623819
## fatherBornUS                   fatherBornUS  0.06503407 0.06503407
## preschool                         preschool  0.05128936 0.05128936
## minutesPerWeekEnglish minutesPerWeekEnglish  0.03646560 0.03646560
## motherWork                       motherWork  0.02446121 0.02446121
## schoolHasLibrary           schoolHasLibrary  0.02355761 0.02355761
## schoolSize                       schoolSize  0.02050924 0.02050924
## urban                                 urban -0.01601687 0.01601687
## selfBornUS                       selfBornUS  0.01344333 0.01344333
## studentsInEnglish         studentsInEnglish  0.01025221 0.01025221
##                       expectBachelors fatherBachelors        grade
## expectBachelors            1.00000000     0.201927970  0.111908245
## fatherBachelors            0.20192797     1.000000000  0.056086980
## grade                      0.11190824     0.056086980  1.000000000
## read30MinsADay             0.12776551     0.046179088  0.039723965
## motherBachelors            0.15860908     0.551442896  0.029744703
## computerForSchoolwork      0.14899660     0.165888481  0.067693475
## fatherHS                   0.15993899     0.267060004  0.027234796
## motherHS                   0.11099887     0.226294277 -0.005934161
## male                      -0.09248047     0.080779286 -0.094844648
## publicSchool              -0.10927343    -0.179153876 -0.042782297
## englishAtHome              0.05381525     0.138923281 -0.029076437
## fatherWork                 0.01538675     0.104814574  0.005859578
## fatherBornUS              -0.01854309     0.098400961 -0.082089212
## preschool                  0.08499424     0.152760694  0.005577343
## minutesPerWeekEnglish      0.01767938    -0.002485598  0.017256498
## motherWork                 0.07651553     0.059682214  0.004685733
## schoolHasLibrary           0.03053399     0.005418711 -0.034610260
## schoolSize                 0.03291037     0.009340037  0.073231222
## urban                      0.02120917    -0.018416000  0.081234839
## selfBornUS                -0.01603746     0.027220115 -0.043886766
## studentsInEnglish          0.03777109    -0.033021777  0.065091947
##                       read30MinsADay motherBachelors computerForSchoolwork
## expectBachelors         0.1277655089     0.158609084          0.1489966037
## fatherBachelors         0.0461790881     0.551442896          0.1658884814
## grade                   0.0397239650     0.029744703          0.0676934748
## read30MinsADay          1.0000000000     0.024800603         -0.0177796343
## motherBachelors         0.0248006031     1.000000000          0.1399247526
## computerForSchoolwork  -0.0177796343     0.139924753          1.0000000000
## fatherHS                0.0208902447     0.183158194          0.1674833486
## motherHS                0.0109271086     0.246264547          0.1648666540
## male                   -0.2128472594     0.082509576          0.0007551518
## publicSchool            0.0098892970    -0.167887725         -0.0748209642
## englishAtHome           0.0006988334     0.157266462          0.0637397500
## fatherWork              0.0310592270     0.074994105          0.0972139867
## fatherBornUS            0.0092682679     0.112014223          0.0008175299
## preschool              -0.0153331398     0.157263805          0.1016585555
## minutesPerWeekEnglish   0.0314942353     0.019152825         -0.0212173493
## motherWork             -0.0667223254     0.138744176          0.0779677192
## schoolHasLibrary        0.0097970041     0.005664362         -0.0163649650
## schoolSize             -0.0158591506    -0.007007003          0.0753204765
## urban                   0.0184361496    -0.024376314          0.0465480932
## selfBornUS             -0.0020593777     0.042323409          0.0121024202
## studentsInEnglish       0.0078286598    -0.044909467          0.0563925175
##                          fatherHS     motherHS          male publicSchool
## expectBachelors        0.15993899  0.110998866 -0.0924804746 -0.109273432
## fatherBachelors        0.26706000  0.226294277  0.0807792859 -0.179153876
## grade                  0.02723480 -0.005934161 -0.0948446476 -0.042782297
## read30MinsADay         0.02089024  0.010927109 -0.2128472594  0.009889297
## motherBachelors        0.18315819  0.246264547  0.0825095756 -0.167887725
## computerForSchoolwork  0.16748335  0.164866654  0.0007551518 -0.074820964
## fatherHS               1.00000000  0.496570370  0.0384014319 -0.072912901
## motherHS               0.49657037  1.000000000  0.0320617406 -0.062627880
## male                   0.03840143  0.032061741  1.0000000000 -0.091134322
## publicSchool          -0.07291290 -0.062627880 -0.0911343223  1.000000000
## englishAtHome          0.34379500  0.387462047 -0.0067798193 -0.049294970
## fatherWork             0.07689286  0.004374140  0.0424415788 -0.027716820
## fatherBornUS           0.33384721  0.337663426  0.0093716409  0.023107896
## preschool              0.11534892  0.138932025  0.0108245460 -0.098916076
## minutesPerWeekEnglish  0.01175479 -0.001194904 -0.0075756313  0.065916099
## motherWork             0.10945534  0.159007741 -0.0135413380 -0.039602868
## schoolHasLibrary       0.02480741  0.006724287  0.0476608142  0.075132838
## schoolSize            -0.07646839 -0.069267119  0.0111585056  0.261285888
## urban                 -0.08487069 -0.107020782  0.0463699479 -0.334509892
## selfBornUS             0.16148067  0.188745981  0.0345501475 -0.047426843
## studentsInEnglish     -0.02380552 -0.035084104 -0.0327939896  0.106491004
##                       englishAtHome   fatherWork  fatherBornUS
## expectBachelors        0.0538152532  0.015386750 -0.0185430946
## fatherBachelors        0.1389232809  0.104814574  0.0984009608
## grade                 -0.0290764366  0.005859578 -0.0820892124
## read30MinsADay         0.0006988334  0.031059227  0.0092682679
## motherBachelors        0.1572664618  0.074994105  0.1120142233
## computerForSchoolwork  0.0637397500  0.097213987  0.0008175299
## fatherHS               0.3437950031  0.076892860  0.3338472106
## motherHS               0.3874620474  0.004374140  0.3376634262
## male                  -0.0067798193  0.042441579  0.0093716409
## publicSchool          -0.0492949697 -0.027716820  0.0231078964
## englishAtHome          1.0000000000  0.018773770  0.6264400692
## fatherWork             0.0187737699  1.000000000 -0.0058546302
## fatherBornUS           0.6264400692 -0.005854630  1.0000000000
## preschool              0.1210285360  0.071665352  0.0992669549
## minutesPerWeekEnglish -0.0177885115  0.007235913  0.0078386569
## motherWork             0.1378257380  0.053199294  0.0826022203
## schoolHasLibrary      -0.0090373568 -0.020323743 -0.0109372081
## schoolSize            -0.2114827171 -0.012036739 -0.2625429923
## urban                 -0.2031800241 -0.029059917 -0.2812637184
## selfBornUS             0.4760179049 -0.019416960  0.4539953250
## studentsInEnglish     -0.0656052474 -0.005077876 -0.0960273188
##                          preschool minutesPerWeekEnglish   motherWork
## expectBachelors        0.084994244           0.017679381  0.076515530
## fatherBachelors        0.152760694          -0.002485598  0.059682214
## grade                  0.005577343           0.017256498  0.004685733
## read30MinsADay        -0.015333140           0.031494235 -0.066722325
## motherBachelors        0.157263805           0.019152825  0.138744176
## computerForSchoolwork  0.101658555          -0.021217349  0.077967719
## fatherHS               0.115348917           0.011754789  0.109455343
## motherHS               0.138932025          -0.001194904  0.159007741
## male                   0.010824546          -0.007575631 -0.013541338
## publicSchool          -0.098916076           0.065916099 -0.039602868
## englishAtHome          0.121028536          -0.017788512  0.137825738
## fatherWork             0.071665352           0.007235913  0.053199294
## fatherBornUS           0.099266955           0.007838657  0.082602220
## preschool              1.000000000          -0.030280961  0.076153628
## minutesPerWeekEnglish -0.030280961           1.000000000  0.019266113
## motherWork             0.076153628           0.019266113  1.000000000
## schoolHasLibrary       0.012239077           0.001533587 -0.012606807
## schoolSize            -0.026636887           0.004807711 -0.049533585
## urban                 -0.019779009          -0.054023025 -0.008753962
## selfBornUS             0.091448862          -0.011278415  0.058810711
## studentsInEnglish     -0.025684806           0.047537258 -0.035975554
##                       schoolHasLibrary   schoolSize        urban
## expectBachelors            0.030533986  0.032910372  0.021209172
## fatherBachelors            0.005418711  0.009340037 -0.018416000
## grade                     -0.034610260  0.073231222  0.081234839
## read30MinsADay             0.009797004 -0.015859151  0.018436150
## motherBachelors            0.005664362 -0.007007003 -0.024376314
## computerForSchoolwork     -0.016364965  0.075320476  0.046548093
## fatherHS                   0.024807414 -0.076468388 -0.084870694
## motherHS                   0.006724287 -0.069267119 -0.107020782
## male                       0.047660814  0.011158506  0.046369948
## publicSchool               0.075132838  0.261285888 -0.334509892
## englishAtHome             -0.009037357 -0.211482717 -0.203180024
## fatherWork                -0.020323743 -0.012036739 -0.029059917
## fatherBornUS              -0.010937208 -0.262542992 -0.281263718
## preschool                  0.012239077 -0.026636887 -0.019779009
## minutesPerWeekEnglish      0.001533587  0.004807711 -0.054023025
## motherWork                -0.012606807 -0.049533585 -0.008753962
## schoolHasLibrary           1.000000000  0.079554898 -0.134225029
## schoolSize                 0.079554898  1.000000000  0.317388020
## urban                     -0.134225029  0.317388020  1.000000000
## selfBornUS                -0.004087532 -0.118650683 -0.113217557
## studentsInEnglish          0.096309537  0.296303095  0.154293355
##                         selfBornUS studentsInEnglish
## expectBachelors       -0.016037463       0.037771094
## fatherBachelors        0.027220115      -0.033021777
## grade                 -0.043886766       0.065091947
## read30MinsADay        -0.002059378       0.007828660
## motherBachelors        0.042323409      -0.044909467
## computerForSchoolwork  0.012102420       0.056392517
## fatherHS               0.161480674      -0.023805522
## motherHS               0.188745981      -0.035084104
## male                   0.034550147      -0.032793990
## publicSchool          -0.047426843       0.106491004
## englishAtHome          0.476017905      -0.065605247
## fatherWork            -0.019416960      -0.005077876
## fatherBornUS           0.453995325      -0.096027319
## preschool              0.091448862      -0.025684806
## minutesPerWeekEnglish -0.011278415       0.047537258
## motherWork             0.058810711      -0.035975554
## schoolHasLibrary      -0.004087532       0.096309537
## schoolSize            -0.118650683       0.296303095
## urban                 -0.113217557       0.154293355
## selfBornUS             1.000000000      -0.013601249
## studentsInEnglish     -0.013601249       1.000000000
##                       expectBachelors fatherBachelors       grade
## expectBachelors            0.00000000     0.201927970 0.111908245
## fatherBachelors            0.20192797     0.000000000 0.056086980
## grade                      0.11190824     0.056086980 0.000000000
## read30MinsADay             0.12776551     0.046179088 0.039723965
## motherBachelors            0.15860908     0.551442896 0.029744703
## computerForSchoolwork      0.14899660     0.165888481 0.067693475
## fatherHS                   0.15993899     0.267060004 0.027234796
## motherHS                   0.11099887     0.226294277 0.005934161
## male                       0.09248047     0.080779286 0.094844648
## publicSchool               0.10927343     0.179153876 0.042782297
## englishAtHome              0.05381525     0.138923281 0.029076437
## fatherWork                 0.01538675     0.104814574 0.005859578
## fatherBornUS               0.01854309     0.098400961 0.082089212
## preschool                  0.08499424     0.152760694 0.005577343
## minutesPerWeekEnglish      0.01767938     0.002485598 0.017256498
## motherWork                 0.07651553     0.059682214 0.004685733
## schoolHasLibrary           0.03053399     0.005418711 0.034610260
## schoolSize                 0.03291037     0.009340037 0.073231222
## urban                      0.02120917     0.018416000 0.081234839
## selfBornUS                 0.01603746     0.027220115 0.043886766
## studentsInEnglish          0.03777109     0.033021777 0.065091947
##                       read30MinsADay motherBachelors computerForSchoolwork
## expectBachelors         0.1277655089     0.158609084          0.1489966037
## fatherBachelors         0.0461790881     0.551442896          0.1658884814
## grade                   0.0397239650     0.029744703          0.0676934748
## read30MinsADay          0.0000000000     0.024800603          0.0177796343
## motherBachelors         0.0248006031     0.000000000          0.1399247526
## computerForSchoolwork   0.0177796343     0.139924753          0.0000000000
## fatherHS                0.0208902447     0.183158194          0.1674833486
## motherHS                0.0109271086     0.246264547          0.1648666540
## male                    0.2128472594     0.082509576          0.0007551518
## publicSchool            0.0098892970     0.167887725          0.0748209642
## englishAtHome           0.0006988334     0.157266462          0.0637397500
## fatherWork              0.0310592270     0.074994105          0.0972139867
## fatherBornUS            0.0092682679     0.112014223          0.0008175299
## preschool               0.0153331398     0.157263805          0.1016585555
## minutesPerWeekEnglish   0.0314942353     0.019152825          0.0212173493
## motherWork              0.0667223254     0.138744176          0.0779677192
## schoolHasLibrary        0.0097970041     0.005664362          0.0163649650
## schoolSize              0.0158591506     0.007007003          0.0753204765
## urban                   0.0184361496     0.024376314          0.0465480932
## selfBornUS              0.0020593777     0.042323409          0.0121024202
## studentsInEnglish       0.0078286598     0.044909467          0.0563925175
##                         fatherHS    motherHS         male publicSchool
## expectBachelors       0.15993899 0.110998866 0.0924804746  0.109273432
## fatherBachelors       0.26706000 0.226294277 0.0807792859  0.179153876
## grade                 0.02723480 0.005934161 0.0948446476  0.042782297
## read30MinsADay        0.02089024 0.010927109 0.2128472594  0.009889297
## motherBachelors       0.18315819 0.246264547 0.0825095756  0.167887725
## computerForSchoolwork 0.16748335 0.164866654 0.0007551518  0.074820964
## fatherHS              0.00000000 0.496570370 0.0384014319  0.072912901
## motherHS              0.49657037 0.000000000 0.0320617406  0.062627880
## male                  0.03840143 0.032061741 0.0000000000  0.091134322
## publicSchool          0.07291290 0.062627880 0.0911343223  0.000000000
## englishAtHome         0.34379500 0.387462047 0.0067798193  0.049294970
## fatherWork            0.07689286 0.004374140 0.0424415788  0.027716820
## fatherBornUS          0.33384721 0.337663426 0.0093716409  0.023107896
## preschool             0.11534892 0.138932025 0.0108245460  0.098916076
## minutesPerWeekEnglish 0.01175479 0.001194904 0.0075756313  0.065916099
## motherWork            0.10945534 0.159007741 0.0135413380  0.039602868
## schoolHasLibrary      0.02480741 0.006724287 0.0476608142  0.075132838
## schoolSize            0.07646839 0.069267119 0.0111585056  0.261285888
## urban                 0.08487069 0.107020782 0.0463699479  0.334509892
## selfBornUS            0.16148067 0.188745981 0.0345501475  0.047426843
## studentsInEnglish     0.02380552 0.035084104 0.0327939896  0.106491004
##                       englishAtHome  fatherWork fatherBornUS   preschool
## expectBachelors        0.0538152532 0.015386750 0.0185430946 0.084994244
## fatherBachelors        0.1389232809 0.104814574 0.0984009608 0.152760694
## grade                  0.0290764366 0.005859578 0.0820892124 0.005577343
## read30MinsADay         0.0006988334 0.031059227 0.0092682679 0.015333140
## motherBachelors        0.1572664618 0.074994105 0.1120142233 0.157263805
## computerForSchoolwork  0.0637397500 0.097213987 0.0008175299 0.101658555
## fatherHS               0.3437950031 0.076892860 0.3338472106 0.115348917
## motherHS               0.3874620474 0.004374140 0.3376634262 0.138932025
## male                   0.0067798193 0.042441579 0.0093716409 0.010824546
## publicSchool           0.0492949697 0.027716820 0.0231078964 0.098916076
## englishAtHome          0.0000000000 0.018773770 0.6264400692 0.121028536
## fatherWork             0.0187737699 0.000000000 0.0058546302 0.071665352
## fatherBornUS           0.6264400692 0.005854630 0.0000000000 0.099266955
## preschool              0.1210285360 0.071665352 0.0992669549 0.000000000
## minutesPerWeekEnglish  0.0177885115 0.007235913 0.0078386569 0.030280961
## motherWork             0.1378257380 0.053199294 0.0826022203 0.076153628
## schoolHasLibrary       0.0090373568 0.020323743 0.0109372081 0.012239077
## schoolSize             0.2114827171 0.012036739 0.2625429923 0.026636887
## urban                  0.2031800241 0.029059917 0.2812637184 0.019779009
## selfBornUS             0.4760179049 0.019416960 0.4539953250 0.091448862
## studentsInEnglish      0.0656052474 0.005077876 0.0960273188 0.025684806
##                       minutesPerWeekEnglish  motherWork schoolHasLibrary
## expectBachelors                 0.017679381 0.076515530      0.030533986
## fatherBachelors                 0.002485598 0.059682214      0.005418711
## grade                           0.017256498 0.004685733      0.034610260
## read30MinsADay                  0.031494235 0.066722325      0.009797004
## motherBachelors                 0.019152825 0.138744176      0.005664362
## computerForSchoolwork           0.021217349 0.077967719      0.016364965
## fatherHS                        0.011754789 0.109455343      0.024807414
## motherHS                        0.001194904 0.159007741      0.006724287
## male                            0.007575631 0.013541338      0.047660814
## publicSchool                    0.065916099 0.039602868      0.075132838
## englishAtHome                   0.017788512 0.137825738      0.009037357
## fatherWork                      0.007235913 0.053199294      0.020323743
## fatherBornUS                    0.007838657 0.082602220      0.010937208
## preschool                       0.030280961 0.076153628      0.012239077
## minutesPerWeekEnglish           0.000000000 0.019266113      0.001533587
## motherWork                      0.019266113 0.000000000      0.012606807
## schoolHasLibrary                0.001533587 0.012606807      0.000000000
## schoolSize                      0.004807711 0.049533585      0.079554898
## urban                           0.054023025 0.008753962      0.134225029
## selfBornUS                      0.011278415 0.058810711      0.004087532
## studentsInEnglish               0.047537258 0.035975554      0.096309537
##                        schoolSize       urban  selfBornUS
## expectBachelors       0.032910372 0.021209172 0.016037463
## fatherBachelors       0.009340037 0.018416000 0.027220115
## grade                 0.073231222 0.081234839 0.043886766
## read30MinsADay        0.015859151 0.018436150 0.002059378
## motherBachelors       0.007007003 0.024376314 0.042323409
## computerForSchoolwork 0.075320476 0.046548093 0.012102420
## fatherHS              0.076468388 0.084870694 0.161480674
## motherHS              0.069267119 0.107020782 0.188745981
## male                  0.011158506 0.046369948 0.034550147
## publicSchool          0.261285888 0.334509892 0.047426843
## englishAtHome         0.211482717 0.203180024 0.476017905
## fatherWork            0.012036739 0.029059917 0.019416960
## fatherBornUS          0.262542992 0.281263718 0.453995325
## preschool             0.026636887 0.019779009 0.091448862
## minutesPerWeekEnglish 0.004807711 0.054023025 0.011278415
## motherWork            0.049533585 0.008753962 0.058810711
## schoolHasLibrary      0.079554898 0.134225029 0.004087532
## schoolSize            0.000000000 0.317388020 0.118650683
## urban                 0.317388020 0.000000000 0.113217557
## selfBornUS            0.118650683 0.113217557 0.000000000
## studentsInEnglish     0.296303095 0.154293355 0.013601249
##                       studentsInEnglish
## expectBachelors             0.037771094
## fatherBachelors             0.033021777
## grade                       0.065091947
## read30MinsADay              0.007828660
## motherBachelors             0.044909467
## computerForSchoolwork       0.056392517
## fatherHS                    0.023805522
## motherHS                    0.035084104
## male                        0.032793990
## publicSchool                0.106491004
## englishAtHome               0.065605247
## fatherWork                  0.005077876
## fatherBornUS                0.096027319
## preschool                   0.025684806
## minutesPerWeekEnglish       0.047537258
## motherWork                  0.035975554
## schoolHasLibrary            0.096309537
## schoolSize                  0.296303095
## urban                       0.154293355
## selfBornUS                  0.013601249
## studentsInEnglish           0.000000000
##                       id       cor.y  cor.y.abs cor.low
## 3        expectBachelors  0.34430376 0.34430376       1
## 4        fatherBachelors  0.27697819 0.27697819       1
## 8                  grade  0.23868401 0.23868401       1
## 17        read30MinsADay  0.23832990 0.23832990       1
## 11       motherBachelors  0.22676151 0.22676151       1
## 1  computerForSchoolwork  0.18120243 0.18120243       1
## 6               fatherHS  0.16882677 0.16882677       1
## 13              motherHS  0.13898290 0.13898290       1
## 2          englishAtHome  0.10774742 0.10774742       1
## 7             fatherWork  0.07623819 0.07623819       1
## 5           fatherBornUS  0.06503407 0.06503407       1
## 12          motherBornUS  0.05145971 0.05145971      NA
## 15             preschool  0.05128936 0.05128936       1
## 10 minutesPerWeekEnglish  0.03646560 0.03646560       1
## 14            motherWork  0.02446121 0.02446121       1
## 18      schoolHasLibrary  0.02355761 0.02355761       1
## 19            schoolSize  0.02050924 0.02050924       1
## 20            selfBornUS  0.01344333 0.01344333       1
## 21     studentsInEnglish  0.01025221 0.01025221       1
## 22                 urban -0.01601687 0.01601687       1
## 16          publicSchool -0.11166900 0.11166900       1
## 9                   male -0.12845812 0.12845812       1
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
## 9                 run_models                5                0
```

## Step `5`: run models

```r
max_cor_y_x_var <- subset(glb_feats_df, cor.low == 1)[1, "id"]

#   Regression:
if (glb_is_regression) {
    #   Linear:
    myrun_mdl_fn <- myrun_mdl_lm
}    

#   Classification:
if (glb_is_classification) {
    #   Logit Regression:
    myrun_mdl_fn <- myrun_mdl_glm
}    
    
# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 6900267
## [1] 0.1156172
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -262.096  -58.929    2.529   57.719  251.674 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      448.966      4.194  107.05   <2e-16 ***
## expectBachelors   82.700      4.592   18.01   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 83.88 on 2412 degrees of freedom
## Multiple R-squared:  0.1185,	Adjusted R-squared:  0.1182 
## F-statistic: 324.4 on 1 and 2412 DF,  p-value: < 2.2e-16
## 
##             feats n.fit  R.sq.fit  R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB
## 1 expectBachelors  2414 0.1185451 0.1156172    0.1185451 16971109 6900267
##   f.score.OOB
## 1          NA
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
    paste(max_cor_y_x_var, subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)    
```

```
## [1] 6821962
## [1] 0.1256534
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -265.193  -58.548    2.362   57.845  251.674 
## 
## Coefficients:
##                              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   448.966      4.186 107.253  < 2e-16 ***
## expectBachelors                71.156      5.829  12.207  < 2e-16 ***
## expectBachelors:motherBornUS   14.641      4.568   3.205  0.00137 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 83.72 on 2411 degrees of freedom
## Multiple R-squared:  0.1223,	Adjusted R-squared:  0.1216 
## F-statistic:   168 on 2 and 2411 DF,  p-value: < 2.2e-16
## 
##                                           feats n.fit  R.sq.fit  R.sq.OOB
## 2 expectBachelors, expectBachelors:motherBornUS  2414 0.1222845 0.1256534
## 1                               expectBachelors  2414 0.1185451 0.1156172
##   Adj.R.sq.fit  SSE.fit SSE.OOB f.score.OOB
## 2    0.1222845 16899111 6821962          NA
## 1    0.1185451 16971109 6900267          NA
```

```r
# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, cor.low == 1)[, "id"],
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 6095961
## [1] 0.2187023
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -247.633  -50.362    1.901   52.085  225.710 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            96.542015  34.755741   2.778 0.005517 ** 
## expectBachelors        54.801648   4.457533  12.294  < 2e-16 ***
## fatherBachelors        24.105930   4.099645   5.880 4.68e-09 ***
## grade                  31.377447   3.045464  10.303  < 2e-16 ***
## read30MinsADay         34.321146   3.533725   9.712  < 2e-16 ***
## motherBachelors        13.873908   4.000525   3.468 0.000534 ***
## computerForSchoolwork  29.406740   5.881602   5.000 6.16e-07 ***
## fatherHS                7.845913   5.726670   1.370 0.170795    
## motherHS                4.297301   6.276378   0.685 0.493613    
## englishAtHome          10.166441   6.737708   1.509 0.131460    
## fatherWork              9.841769   4.537860   2.169 0.030195 *  
## fatherBornUS            8.372982   5.290439   1.583 0.113631    
## preschool              -5.244515   3.616901  -1.450 0.147189    
## minutesPerWeekEnglish   0.016141   0.011120   1.452 0.146764    
## motherWork             -4.554387   3.651040  -1.247 0.212365    
## schoolHasLibrary       11.613093   9.620332   1.207 0.227497    
## schoolSize              0.005268   0.002262   2.329 0.019943 *  
## selfBornUS             -7.612850   7.509987  -1.014 0.310830    
## studentsInEnglish      -0.101844   0.235569  -0.432 0.665540    
## urban                  -9.534492   4.042192  -2.359 0.018417 *  
## publicSchool          -25.135258   6.934501  -3.625 0.000295 ***
## male                  -13.929515   3.270298  -4.259 2.13e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 76.67 on 2392 degrees of freedom
## Multiple R-squared:  0.2697,	Adjusted R-squared:  0.2633 
## F-statistic: 42.06 on 21 and 2392 DF,  p-value: < 2.2e-16
## 
##                                                                                                                                                                                                                                                                                                feats
## 3 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male
## 2                                                                                                                                                                                                                                                      expectBachelors, expectBachelors:motherBornUS
## 1                                                                                                                                                                                                                                                                                    expectBachelors
##   n.fit  R.sq.fit  R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB f.score.OOB
## 3  2414 0.2696846 0.2187023    0.2696846 14061140 6095961          NA
## 2  2414 0.1222845 0.1256534    0.1222845 16899111 6821962          NA
## 1  2414 0.1185451 0.1156172    0.1185451 16971109 6900267          NA
```

```r
# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(setdiff(names(glb_entity_df),
                                                        glb_predct_var),
                                                glb_exclude_vars_as_features),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 5762082
## [1] 0.2614944
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -247.44  -48.86    1.86   49.77  217.18 
## 
## Coefficients:
##                                                      Estimate Std. Error
## (Intercept)                                        143.766333  33.841226
## grade                                               29.542707   2.937399
## male                                               -14.521653   3.155926
## preschool                                           -4.463670   3.486055
## expectBachelors                                     55.267080   4.293893
## motherHS                                             6.058774   6.091423
## motherBachelors                                     12.638068   3.861457
## motherWork                                          -2.809101   3.521827
## fatherHS                                             4.018214   5.579269
## fatherBachelors                                     16.929755   3.995253
## fatherWork                                           5.842798   4.395978
## selfBornUS                                          -3.806278   7.323718
## motherBornUS                                        -8.798153   6.587621
## fatherBornUS                                         4.306994   6.263875
## englishAtHome                                        8.035685   6.859492
## computerForSchoolwork                               22.500232   5.702562
## read30MinsADay                                      34.871924   3.408447
## minutesPerWeekEnglish                                0.012788   0.010712
## studentsInEnglish                                   -0.286631   0.227819
## schoolHasLibrary                                    12.215085   9.264884
## publicSchool                                       -16.857475   6.725614
## urban                                               -0.110132   3.962724
## schoolSize                                           0.006540   0.002197
## raceeth.fctrBlack                                  -67.012347   5.460883
## raceeth.fctrHispanic                               -38.975486   5.177743
## raceeth.fctrAsian                                   -4.110325   9.220071
## raceeth.fctrMore than one race                     -16.922522   8.496268
## raceeth.fctrAmerican Indian/Alaska Native          -67.277327  16.786935
## raceeth.fctrNative Hawaiian/Other Pacific Islander  -5.101601  17.005696
##                                                    t value Pr(>|t|)    
## (Intercept)                                          4.248 2.24e-05 ***
## grade                                               10.057  < 2e-16 ***
## male                                                -4.601 4.42e-06 ***
## preschool                                           -1.280  0.20052    
## expectBachelors                                     12.871  < 2e-16 ***
## motherHS                                             0.995  0.32001    
## motherBachelors                                      3.273  0.00108 ** 
## motherWork                                          -0.798  0.42517    
## fatherHS                                             0.720  0.47147    
## fatherBachelors                                      4.237 2.35e-05 ***
## fatherWork                                           1.329  0.18393    
## selfBornUS                                          -0.520  0.60331    
## motherBornUS                                        -1.336  0.18182    
## fatherBornUS                                         0.688  0.49178    
## englishAtHome                                        1.171  0.24153    
## computerForSchoolwork                                3.946 8.19e-05 ***
## read30MinsADay                                      10.231  < 2e-16 ***
## minutesPerWeekEnglish                                1.194  0.23264    
## studentsInEnglish                                   -1.258  0.20846    
## schoolHasLibrary                                     1.318  0.18749    
## publicSchool                                        -2.506  0.01226 *  
## urban                                               -0.028  0.97783    
## schoolSize                                           2.977  0.00294 ** 
## raceeth.fctrBlack                                  -12.271  < 2e-16 ***
## raceeth.fctrHispanic                                -7.528 7.29e-14 ***
## raceeth.fctrAsian                                   -0.446  0.65578    
## raceeth.fctrMore than one race                      -1.992  0.04651 *  
## raceeth.fctrAmerican Indian/Alaska Native           -4.008 6.32e-05 ***
## raceeth.fctrNative Hawaiian/Other Pacific Islander  -0.300  0.76421    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 73.81 on 2385 degrees of freedom
## Multiple R-squared:  0.3251,	Adjusted R-squared:  0.3172 
## F-statistic: 41.04 on 28 and 2385 DF,  p-value: < 2.2e-16
## 
##                                                                                                                                                                                                                                                                                                                            feats
## 4 grade, male, preschool, expectBachelors, motherHS, motherBachelors, motherWork, fatherHS, fatherBachelors, fatherWork, selfBornUS, motherBornUS, fatherBornUS, englishAtHome, computerForSchoolwork, read30MinsADay, minutesPerWeekEnglish, studentsInEnglish, schoolHasLibrary, publicSchool, urban, schoolSize, raceeth.fctr
## 3                             expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male
## 2                                                                                                                                                                                                                                                                                  expectBachelors, expectBachelors:motherBornUS
## 1                                                                                                                                                                                                                                                                                                                expectBachelors
##   n.fit  R.sq.fit  R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB f.score.OOB
## 4  2414 0.3251434 0.2614944    0.3251434 12993365 5762082          NA
## 3  2414 0.2696846 0.2187023    0.2696846 14061140 6095961          NA
## 2  2414 0.1222845 0.1256534    0.1222845 16899111 6821962          NA
## 1  2414 0.1185451 0.1156172    0.1185451 16971109 6900267          NA
```

```r
glb_sel_mdl <- glb_mdl                        

# User specified
# ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("<feat1_name>", "<feat2_name>"),
#                         fit_df=glb_entity_df, OOB_df=glb_predct_df)


# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)


if (glb_is_regression)
    print(myplot_scatter(glb_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats), data=glb_models_df, color="NavyBlue", 
                    size=3.5))
```

![](NCES_PISA_files/figure-html/run_models-1.png) 

```r
if (glb_is_classification) {
    plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 20))
    print(myplot_hbar(df=plot_models_df, xcol_name="feats.label", 
                      ycol_names="f.score.OOB"))
}

script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_training.all", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
```

## Step `6`: fit training.all

```r
# Needs to handle dummy variables
print(mdl_feats_df <- myextract_mdl_feats())
```

```
##                                          id         Pr.z
## expectBachelors             expectBachelors 1.059226e-36
## raceeth.fctr                   raceeth.fctr 1.303096e-33
## read30MinsADay               read30MinsADay 4.489941e-24
## grade                                 grade 2.468118e-23
## male                                   male 4.416601e-06
## fatherBachelors             fatherBachelors 2.346477e-05
## computerForSchoolwork computerForSchoolwork 8.188776e-05
## motherBachelors             motherBachelors 1.079791e-03
## schoolSize                       schoolSize 2.941535e-03
## publicSchool                   publicSchool 1.226078e-02
## motherBornUS                   motherBornUS 1.818211e-01
## fatherWork                       fatherWork 1.839343e-01
## schoolHasLibrary           schoolHasLibrary 1.874869e-01
## preschool                         preschool 2.005164e-01
## studentsInEnglish         studentsInEnglish 2.084598e-01
## minutesPerWeekEnglish minutesPerWeekEnglish 2.326444e-01
## englishAtHome                 englishAtHome 2.415272e-01
## motherHS                           motherHS 3.200122e-01
## motherWork                       motherWork 4.251670e-01
## fatherHS                           fatherHS 4.714698e-01
## fatherBornUS                   fatherBornUS 4.917762e-01
## selfBornUS                       selfBornUS 6.033074e-01
## urban                                 urban 9.778304e-01
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl    
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))    
}    
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -247.44  -48.86    1.86   49.77  217.18 
## 
## Coefficients:
##                                                      Estimate Std. Error
## (Intercept)                                        143.766333  33.841226
## expectBachelors                                     55.267080   4.293893
## raceeth.fctrBlack                                  -67.012347   5.460883
## raceeth.fctrHispanic                               -38.975486   5.177743
## raceeth.fctrAsian                                   -4.110325   9.220071
## raceeth.fctrMore than one race                     -16.922522   8.496268
## raceeth.fctrAmerican Indian/Alaska Native          -67.277327  16.786935
## raceeth.fctrNative Hawaiian/Other Pacific Islander  -5.101601  17.005696
## read30MinsADay                                      34.871924   3.408447
## grade                                               29.542707   2.937399
## male                                               -14.521653   3.155926
## fatherBachelors                                     16.929755   3.995253
## computerForSchoolwork                               22.500232   5.702562
## motherBachelors                                     12.638068   3.861457
## schoolSize                                           0.006540   0.002197
## publicSchool                                       -16.857475   6.725614
## motherBornUS                                        -8.798153   6.587621
## fatherWork                                           5.842798   4.395978
## schoolHasLibrary                                    12.215085   9.264884
## preschool                                           -4.463670   3.486055
## studentsInEnglish                                   -0.286631   0.227819
## minutesPerWeekEnglish                                0.012788   0.010712
## englishAtHome                                        8.035685   6.859492
## motherHS                                             6.058774   6.091423
## motherWork                                          -2.809101   3.521827
## fatherHS                                             4.018214   5.579269
## fatherBornUS                                         4.306994   6.263875
## selfBornUS                                          -3.806278   7.323718
## urban                                               -0.110132   3.962724
##                                                    t value Pr(>|t|)    
## (Intercept)                                          4.248 2.24e-05 ***
## expectBachelors                                     12.871  < 2e-16 ***
## raceeth.fctrBlack                                  -12.271  < 2e-16 ***
## raceeth.fctrHispanic                                -7.528 7.29e-14 ***
## raceeth.fctrAsian                                   -0.446  0.65578    
## raceeth.fctrMore than one race                      -1.992  0.04651 *  
## raceeth.fctrAmerican Indian/Alaska Native           -4.008 6.32e-05 ***
## raceeth.fctrNative Hawaiian/Other Pacific Islander  -0.300  0.76421    
## read30MinsADay                                      10.231  < 2e-16 ***
## grade                                               10.057  < 2e-16 ***
## male                                                -4.601 4.42e-06 ***
## fatherBachelors                                      4.237 2.35e-05 ***
## computerForSchoolwork                                3.946 8.19e-05 ***
## motherBachelors                                      3.273  0.00108 ** 
## schoolSize                                           2.977  0.00294 ** 
## publicSchool                                        -2.506  0.01226 *  
## motherBornUS                                        -1.336  0.18182    
## fatherWork                                           1.329  0.18393    
## schoolHasLibrary                                     1.318  0.18749    
## preschool                                           -1.280  0.20052    
## studentsInEnglish                                   -1.258  0.20846    
## minutesPerWeekEnglish                                1.194  0.23264    
## englishAtHome                                        1.171  0.24153    
## motherHS                                             0.995  0.32001    
## motherWork                                          -0.798  0.42517    
## fatherHS                                             0.720  0.47147    
## fatherBornUS                                         0.688  0.49178    
## selfBornUS                                          -0.520  0.60331    
## urban                                               -0.028  0.97783    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 73.81 on 2385 degrees of freedom
## Multiple R-squared:  0.3251,	Adjusted R-squared:  0.3172 
## F-statistic: 41.04 on 28 and 2385 DF,  p-value: < 2.2e-16
## 
##                                                                                                                                                                                                                                                                                                                            feats
## 4 grade, male, preschool, expectBachelors, motherHS, motherBachelors, motherWork, fatherHS, fatherBachelors, fatherWork, selfBornUS, motherBornUS, fatherBornUS, englishAtHome, computerForSchoolwork, read30MinsADay, minutesPerWeekEnglish, studentsInEnglish, schoolHasLibrary, publicSchool, urban, schoolSize, raceeth.fctr
## 3                             expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male
## 2                                                                                                                                                                                                                                                                                  expectBachelors, expectBachelors:motherBornUS
## 1                                                                                                                                                                                                                                                                                                                expectBachelors
## 5 expectBachelors, raceeth.fctr, read30MinsADay, grade, male, fatherBachelors, computerForSchoolwork, motherBachelors, schoolSize, publicSchool, motherBornUS, fatherWork, schoolHasLibrary, preschool, studentsInEnglish, minutesPerWeekEnglish, englishAtHome, motherHS, motherWork, fatherHS, fatherBornUS, selfBornUS, urban
##   n.fit  R.sq.fit  R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB f.score.OOB
## 4  2414 0.3251434 0.2614944    0.3251434 12993365 5762082          NA
## 3  2414 0.2696846 0.2187023    0.2696846 14061140 6095961          NA
## 2  2414 0.1222845 0.1256534    0.1222845 16899111 6821962          NA
## 1  2414 0.1185451 0.1156172    0.1185451 16971109 6900267          NA
## 5  2414 0.3251434        NA    0.3251434 12993365      NA          NA
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

![](NCES_PISA_files/figure-html/fit_training.all-1.png) 

```r
if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl        
    glb_entity_df[, glb_predct_var_name] <- (predict(glb_sel_mdl, 
                        newdata=glb_entity_df, type="response") >= 0.5) * 1.0
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_entity_df))                        
}    

print(glb_feats_df <- mymerge_feats_Pr.z())
```

```
##                       id       cor.y  cor.y.abs cor.low         Pr.z
## 3        expectBachelors  0.34430376 0.34430376       1 1.059226e-36
## 17          raceeth.fctr          NA         NA      NA 1.303096e-33
## 18        read30MinsADay  0.23832990 0.23832990       1 4.489941e-24
## 8                  grade  0.23868401 0.23868401       1 2.468118e-23
## 9                   male -0.12845812 0.12845812       1 4.416601e-06
## 4        fatherBachelors  0.27697819 0.27697819       1 2.346477e-05
## 1  computerForSchoolwork  0.18120243 0.18120243       1 8.188776e-05
## 11       motherBachelors  0.22676151 0.22676151       1 1.079791e-03
## 20            schoolSize  0.02050924 0.02050924       1 2.941535e-03
## 16          publicSchool -0.11166900 0.11166900       1 1.226078e-02
## 12          motherBornUS  0.05145971 0.05145971      NA 1.818211e-01
## 7             fatherWork  0.07623819 0.07623819       1 1.839343e-01
## 19      schoolHasLibrary  0.02355761 0.02355761       1 1.874869e-01
## 15             preschool  0.05128936 0.05128936       1 2.005164e-01
## 22     studentsInEnglish  0.01025221 0.01025221       1 2.084598e-01
## 10 minutesPerWeekEnglish  0.03646560 0.03646560       1 2.326444e-01
## 2          englishAtHome  0.10774742 0.10774742       1 2.415272e-01
## 13              motherHS  0.13898290 0.13898290       1 3.200122e-01
## 14            motherWork  0.02446121 0.02446121       1 4.251670e-01
## 6               fatherHS  0.16882677 0.16882677       1 4.714698e-01
## 5           fatherBornUS  0.06503407 0.06503407       1 4.917762e-01
## 21            selfBornUS  0.01344333 0.01344333       1 6.033074e-01
## 23                 urban -0.01601687 0.01601687       1 9.778304e-01
```

```r
# Most of this code is used again in predict_newdata chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))
    }
    
    if (glb_is_regression) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_regression(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                           plot_vars_df$id[1])
#               + geom_point(aes_string(color="<col_name>.fctr"))
              )
    }    
    
    if (glb_is_classification) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_classification(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"),
                                               plot_vars_df$id[1])
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(glb_entity_df)
```

![](NCES_PISA_files/figure-html/fit_training.all-2.png) ![](NCES_PISA_files/figure-html/fit_training.all-3.png) ![](NCES_PISA_files/figure-html/fit_training.all-4.png) ![](NCES_PISA_files/figure-html/fit_training.all-5.png) ![](NCES_PISA_files/figure-html/fit_training.all-6.png) ![](NCES_PISA_files/figure-html/fit_training.all-7.png) ![](NCES_PISA_files/figure-html/fit_training.all-8.png) ![](NCES_PISA_files/figure-html/fit_training.all-9.png) ![](NCES_PISA_files/figure-html/fit_training.all-10.png) ![](NCES_PISA_files/figure-html/fit_training.all-11.png) 

```
##      grade male raceeth preschool expectBachelors motherHS motherBachelors
## 1759    10    1   White         0               1        1               0
## 105     10    1   White         1               1        1               0
## 3099    10    0   White         0               1        1               1
## 3436    10    1   White         0               1        1               0
## 2460    10    0   White         0               1        0               0
##      motherWork fatherHS fatherBachelors fatherWork selfBornUS
## 1759          0        1               0          1          1
## 105           1        1               1          0          1
## 3099          1        0               0          1          1
## 3436          1        1               1          1          1
## 2460          0        1               1          1          1
##      motherBornUS fatherBornUS englishAtHome computerForSchoolwork
## 1759            1            1             1                     1
## 105             1            1             1                     1
## 3099            1            1             1                     1
## 3436            1            1             1                     1
## 2460            1            0             1                     1
##      read30MinsADay minutesPerWeekEnglish studentsInEnglish
## 1759              0                   300                30
## 105               1                   205                30
## 3099              0                   235                21
## 3436              0                   270                20
## 2460              1                   275                22
##      schoolHasLibrary publicSchool urban schoolSize readingScore
## 1759                1            1     1       3592       284.64
## 105                 1            1     0       1828       316.63
## 3099                1            1     0       1828       311.15
## 3436                1            1     0        261       295.79
## 2460                1            1     1       1276       356.75
##      raceeth.fctr readingScore.predict readingScore.predict.err .label
## 1759        White             532.0752                 247.4352       
## 105         White             558.1193                 241.4893       
## 3099        White             542.7290                 231.5790       
## 3436        White             527.0027                 231.2127       
## 2460        White             574.8586                 218.1086
```

![](NCES_PISA_files/figure-html/fit_training.all-12.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
## 11            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
if (glb_is_regression)
    glb_predct_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_predct_df, type="response")

if (glb_is_classification)
    glb_predct_df[, glb_predct_var_name] <- (predict(glb_sel_mdl, 
                        newdata=glb_predct_df, type="response") >= 0.5) * 1.0
    
myprint_df(glb_predct_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##    readingScore readingScore.predict
## 1        355.24             471.9867
## 5        453.50             540.1974
## 7        405.13             429.7523
## 8        665.05             499.0139
## 9        604.84             564.0212
## 10       626.08             517.9064
##      readingScore readingScore.predict
## 5          453.50             540.1974
## 599        675.94             508.8671
## 604        615.49             571.4667
## 620        498.63             450.0453
## 719        454.14             477.0301
## 1131       587.92             558.0869
##      readingScore readingScore.predict
## 1562       611.91             560.3228
## 1563       667.20             566.0630
## 1564       541.76             548.7232
## 1566       465.58             477.1803
## 1569       596.34             591.6209
## 1570       577.43             544.1109
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_predct_df[, glb_predct_var_name] - 
                        glb_predct_df[, glb_predct_var]) ^ 2)))
    print(myplot_scatter(glb_predct_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
}                         
```

```
## [1] "Total SSE: 5762082.3711"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](NCES_PISA_files/figure-html/predict_newdata-1.png) 

```r
if (glb_is_classification)
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_predct_df))
    
glb_analytics_diag_plots(glb_predct_df)
```

![](NCES_PISA_files/figure-html/predict_newdata-2.png) ![](NCES_PISA_files/figure-html/predict_newdata-3.png) ![](NCES_PISA_files/figure-html/predict_newdata-4.png) ![](NCES_PISA_files/figure-html/predict_newdata-5.png) ![](NCES_PISA_files/figure-html/predict_newdata-6.png) ![](NCES_PISA_files/figure-html/predict_newdata-7.png) ![](NCES_PISA_files/figure-html/predict_newdata-8.png) ![](NCES_PISA_files/figure-html/predict_newdata-9.png) ![](NCES_PISA_files/figure-html/predict_newdata-10.png) ![](NCES_PISA_files/figure-html/predict_newdata-11.png) 

```
##      grade male  raceeth preschool expectBachelors motherHS
## 937     10    0    White         1               1        1
## 1185    10    0    Asian         1               1        1
## 1361    11    1 Hispanic         1               1        1
## 406     10    0 Hispanic         0               1        1
## 817     10    0    White         1               1        1
##      motherBachelors motherWork fatherHS fatherBachelors fatherWork
## 937                0          1        1               0          0
## 1185               0          1        1               1          1
## 1361               0          1        0               0          1
## 406                0          1        1               1          1
## 817                0          0        1               0          0
##      selfBornUS motherBornUS fatherBornUS englishAtHome
## 937           1            1            1             1
## 1185          1            0            0             0
## 1361          1            0            0             0
## 406           1            1            1             1
## 817           1            1            1             1
##      computerForSchoolwork read30MinsADay minutesPerWeekEnglish
## 937                      1              0                   180
## 1185                     1              1                   330
## 1361                     1              0                   350
## 406                      1              0                   285
## 817                      1              1                   300
##      studentsInEnglish schoolHasLibrary publicSchool urban schoolSize
## 937                 14                1            1     0        887
## 1185                21                1            1     1       2149
## 1361                20                1            1     0       2183
## 406                 20                1            1     1       1378
## 817                 20                1            1     0       1250
##      readingScore raceeth.fctr readingScore.predict
## 937        772.46        White             518.9512
## 1185       335.22        Asian             576.9965
## 1361       735.91     Hispanic             502.2074
## 406        740.44     Hispanic             509.9361
## 817        339.35        White             558.8212
##      readingScore.predict.err .label
## 937                  253.5088       
## 1185                 241.7765       
## 1361                 233.7026       
## 406                  230.5039       
## 817                  219.4712
```

![](NCES_PISA_files/figure-html/predict_newdata-12.png) 

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] mgcv_1.8-5      nlme_3.1-120    reshape2_1.4.1  plyr_1.8.1     
## [5] doBy_4.5-13     survival_2.38-1 ggplot2_1.0.0  
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-5 digest_0.6.8     evaluate_0.5.5   formatR_1.0     
##  [5] grid_3.1.2       gtable_0.1.2     htmltools_0.2.6  knitr_1.9       
##  [9] labeling_0.3     lattice_0.20-30  MASS_7.3-39      Matrix_1.1-5    
## [13] munsell_0.4.2    proto_0.3-10     Rcpp_0.11.4      rmarkdown_0.5.1 
## [17] scales_0.2.4     splines_3.1.2    stringr_0.6.2    tcltk_3.1.2     
## [21] tools_3.1.2      yaml_2.1.13
```

# PISA: NCES: readingScore regression:: template2
bdanalytics  

**  **    
**Date: (Sun) Jun 21, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/pisa2009train.csv  
    New:        https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/pisa2009test.csv  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

Regression results:
First run:
    <glb_sel_mdl_id>: 
        OOB_RMSE=<0.4f>; new_RMSE=<0.4f>; <feat1>=<imp>; <feat2>=<imp>

Classification results:
First run:
    <glb_sel_mdl_id>: Leaderboard: <accuracy>
        newobs_tbl=[0=, 1=]; submit_filename=
        OOB_conf_mtrx=[YN=, NY=]=; max.Accuracy.OOB=; opt.prob.threshold.OOB=
            <feat1>=<imp>; <feat1>=<imp>; <feat1>=<imp>; 
            <txt.feat1>=<imp>; <txt.feat1>=<imp>; <txt.feat1>=<imp>; 

### Prediction Accuracy Enhancement Options:
- import.data chunk:
    - which obs should be in fit vs. OOB (currently dirty.0 vs .1 is split 50%)
    
- inspect.data chunk:
    - For date variables
        - Appropriate factors ?
        - Different / More last* features ?
        
- scrub.data chunk:        
- transform.data chunk:
    - derive features from multiple features
    
- manage.missing.data chunk:
    - Not fill missing vars
    - Fill missing numerics with a different algorithm
    - Fill missing chars with data based on clusters 
    
- extract.features chunk:
    - Text variables: move to date extraction chunk ???
        - Mine acronyms
        - Mine places

- Review set_global_options chunk after features are finalized

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")
#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/pisa2009train.csv"
glb_newdt_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/pisa2009test.csv"
glb_out_pfx <- "template2_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newobs_dataset <- TRUE    # or TRUE
    glb_split_entity_newobs_datasets <- TRUE   # or FALSE
    glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
    glb_split_newdata_condition <- NULL # or "is.na(<var>)"; "<var> <condition_operator> <value>"
    glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
    glb_split_sample.seed <- 123               # or any integer

glb_max_fitobs <- NULL # or any integer                         
glb_is_regression <- TRUE; glb_is_classification <- !glb_is_regression; 
    glb_is_binomial <- NULL # or TRUE or FALSE

glb_rsp_var_raw <- "readingScore"

# for classification, the response variable has to be a factor
glb_rsp_var <- glb_rsp_var_raw # or "readingScore.fctr"

# if the response factor is based on numbers/logicals e.g (0/1 OR TRUE/FALSE vs. "A"/"B"), 
#   or contains spaces (e.g. "Not in Labor Force")
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- NULL # or function(raw) {
#     ret_vals <- rep_len(NA, length(raw)); ret_vals[!is.na(raw)] <- ifelse(raw[!is.na(raw)] == 1, "Y", "N"); return(relevel(as.factor(ret_vals), ref="N"))
#     #as.factor(paste0("B", raw))
#     #as.factor(gsub(" ", "\\.", raw))    
# }
# glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0))

glb_map_rsp_var_to_raw <- NULL # or function(var) {
#     as.numeric(var) - 1
#     #as.numeric(var)
#     #gsub("\\.", " ", levels(var)[as.numeric(var)])
#     c("<=50K", " >50K")[as.numeric(var)]
#     #c(FALSE, TRUE)[as.numeric(var)]
# }
# glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0)))

if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# grade: The grade in school of the student (most 15-year-olds in America are in 10th grade)
# 
# male: Whether the student is male (1/0)
# 
# raceeth: The race/ethnicity composite of the student
# 
# preschool: Whether the student attended preschool (1/0)
# 
# expectBachelors: Whether the student expects to obtain a bachelor's degree (1/0)
# 
# motherHS: Whether the student's mother completed high school (1/0)
# 
# motherBachelors: Whether the student's mother obtained a bachelor's degree (1/0)
# 
# motherWork: Whether the student's mother has part-time or full-time work (1/0)
# 
# fatherHS: Whether the student's father completed high school (1/0)
# 
# fatherBachelors: Whether the student's father obtained a bachelor's degree (1/0)
# 
# fatherWork: Whether the student's father has part-time or full-time work (1/0)
# 
# selfBornUS: Whether the student was born in the United States of America (1/0)
# 
# motherBornUS: Whether the student's mother was born in the United States of America (1/0)
# 
# fatherBornUS: Whether the student's father was born in the United States of America (1/0)
# 
# englishAtHome: Whether the student speaks English at home (1/0)
# 
# computerForSchoolwork: Whether the student has access to a computer for schoolwork (1/0)
# 
# read30MinsADay: Whether the student reads for pleasure for 30 minutes/day (1/0)
# 
# minutesPerWeekEnglish: The number of minutes per week the student spend in English class
# 
# studentsInEnglish: The number of students in this student's English class at school
# 
# schoolHasLibrary: Whether this student's school has a library (1/0)
# 
# publicSchool: Whether this student attends a public school (1/0)
# 
# urban: Whether this student's school is in an urban area (1/0)
# 
# schoolSize: The number of students in this student's school
# 
# readingScore: The student's reading score, on a 1000-point scale

# If multiple vars are parts of id, consider concatenating them to create one id var
# If glb_id_var == NULL, ".rownames <- row.names()" is the default
glb_id_var <- NULL # or c("<var1>")
glb_category_vars <- NULL # or c("<var1>", "<var2>")
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_map_vars <- NULL # or c("<var1>", "<var2>")
glb_map_urls <- list();
# glb_map_urls[["<var1>"]] <- "<var1.url>"

glb_assign_pairs_lst <- NULL; 
# glb_assign_pairs_lst[["<var1>"]] <- list(from=c(NA),
#                                            to=c("NA.my"))
glb_assign_vars <- names(glb_assign_pairs_lst)

glb_transform_lst <- NULL;
# glb_transform_lst[["<var>"]] <- list(
#     mapfn=function(raw) { tfr_raw <- as.character(cut(raw, 5)); 
#                           tfr_raw[is.na(tfr_raw)] <- "NA.my";
#                           return(as.factor(tfr_raw)) }
#     , sfx=".my.fctr")
#     mapfn=function(raw) { mod_raw <- raw;
#         mod_raw <- gsub("&#[[:digit:]]{3};", " ", mod_raw);
#         # Modifications for this exercise only
#         mod_raw <- gsub("\\bgoodIn ", "good In", mod_raw);
#                           return(mod_raw)
#                         }
#     , sfx=".my")
# mapfn(glb_allobs_df$review[739])
# mapfn(glb_allobs_df$review[740])
# ret_lst <- regexec("&#[[:digit:]]{3};", raw, ignore.case=FALSE); ret_lst <- regmatches(raw, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
# ret_lst <- gregexpr("&#[[:digit:]]{3};", raw, ignore.case=FALSE); print(ret_lst); print(length(ret_lst[[1]]))
# mapfn(glb_allobs_df$review)
# mapfn(glb_allobs_df$review[739:740])

# mapfn(glb_allobs_df$<var>)
# glb_transform_lst[["<var1>"]] <- glb_transform_lst[["<var2>"]]
# Add logs of numerics that are not distributed normally ->  do automatically ???
glb_transform_vars <- names(glb_transform_lst)

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- NULL # or c("<txt_var1>", "<txt_var2>")   
#Sys.setlocale("LC_ALL", "C") # For english

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms 

glb_sprs_thresholds <- NULL # or c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

# Derived features (consolidate this with transform features ???)
glb_derive_lst <- NULL;
# glb_derive_lst[["PTS.diff"]] <- list(
#     mapfn=function(PTS, oppPTS) { return(PTS - oppPTS) }
#     , args=c("PTS", "oppPTS"))
# glb_derive_lst[["<txt_var>.niso8859.log"]] <- list(
#     mapfn=function(<txt_var>) { match_lst <- gregexpr("&#[[:digit:]]{3};", <txt_var>)
#                         match_num_vctr <- unlist(lapply(match_lst, 
#                                                         function(elem) length(elem)))
#                         return(log(1 + match_num_vctr)) }
#     , args=c("<txt_var>"))

# args_lst <- NULL; for (arg in glb_derive_lst[["PTS.diff"]]$args) args_lst[[arg]] <- glb_allobs_df[, arg]; do.call(mapfn, args_lst)

# glb_derive_lst[["<var1>"]] <- glb_derive_lst[["<var2>"]]

#         # Create user-specified pattern vectors 
# #sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
#         if (txt_var %in% c("Snippet", "Abstract")) {
#             txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
#                 as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
#                                                    glb_allobs_df[, txt_var]))
#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])
glb_derive_vars <- names(glb_derive_lst)

# User-specified exclusions  
glb_exclude_vars_as_features <- NULL # or c("<var_name>") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- FALSE # or TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](PISA_template2_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 8.165  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
#glb_chunks_df <- myadd_chunk(NULL, "import.data")

glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
                                force_header=TRUE)
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
##      grade male                                raceeth preschool
## 127     11    1                                  White         1
## 558     10    0 Native Hawaiian/Other Pacific Islander         1
## 1866    10    0                                  White         1
## 2665    10    1                                  Black         0
## 2692    10    0                                  Black         1
## 3624    10    0                                  White         1
##      expectBachelors motherHS motherBachelors motherWork fatherHS
## 127                1        1               1          1        1
## 558                1        1               0          1        1
## 1866               1        1               1          1        1
## 2665               1        1               0          0       NA
## 2692               1        1              NA          1        1
## 3624               0        1               1          1        1
##      fatherBachelors fatherWork selfBornUS motherBornUS fatherBornUS
## 127               NA          0          1            1            1
## 558                0          1          1            0            0
## 1866              NA          1          1            1            1
## 2665              NA         NA          1            1            1
## 2692              NA          1          1            1            1
## 3624               1          1          1            1            1
##      englishAtHome computerForSchoolwork read30MinsADay
## 127              1                     1              1
## 558              0                     1              0
## 1866             1                     1              0
## 2665             1                     1              0
## 2692             1                     1              0
## 3624             1                     1              0
##      minutesPerWeekEnglish studentsInEnglish schoolHasLibrary publicSchool
## 127                    300                25                1            1
## 558                    365                25                1            1
## 1866                   270                28                1            1
## 2665                     0                25                1            1
## 2692                   100                20                1            1
## 3624                   261                30                1            1
##      urban schoolSize readingScore
## 127      1       1988       534.45
## 558      0       2183       477.97
## 1866     1       1378       637.09
## 2665     0       1411       601.06
## 2692     0        868       393.05
## 3624     1       1734       445.40
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
# glb_trnobs_df <- read.delim("data/hygiene.txt", header=TRUE, fill=TRUE, sep="\t",
#                             fileEncoding='iso-8859-1')
# glb_trnobs_df <- read.table("data/hygiene.dat.labels", col.names=c("dirty"),
#                             na.strings="[none]")
# glb_trnobs_df$review <- readLines("data/hygiene.dat", n =-1)
# comment(glb_trnobs_df) <- "glb_trnobs_df"                                

# glb_trnobs_df <- data.frame()
# for (symbol in c("Boeing", "CocaCola", "GE", "IBM", "ProcterGamble")) {
#     sym_trnobs_df <- 
#         myimport_data(url=gsub("IBM", symbol, glb_trnng_url), comment="glb_trnobs_df", 
#                                     force_header=TRUE)
#     sym_trnobs_df$Symbol <- symbol
#     glb_trnobs_df <- myrbind_df(glb_trnobs_df, sym_trnobs_df)
# }
                                
# glb_trnobs_df <- 
#     glb_trnobs_df %>% dplyr::filter(Year >= 1999)
                                
if (glb_is_separate_newobs_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newobs_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newobs_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
    }
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
##     grade male  raceeth preschool expectBachelors motherHS motherBachelors
## 2      10    1    White         0               0        1               0
## 281    10    1 Hispanic         1               0        1               0
## 609    11    1    White         1               1        1               0
## 614    10    1    Black         0               0        1               0
## 631    10    1    White         0               1        1               0
## 726    10    1 Hispanic         1               1        0               0
##     motherWork fatherHS fatherBachelors fatherWork selfBornUS motherBornUS
## 2            1        1               0          1          1            1
## 281          1        0              NA          1          0            0
## 609          0        1               1          1          1            1
## 614          1        0               0          1          1            1
## 631          0        1               0          0          1            1
## 726          1        0               0          1          1            0
##     fatherBornUS englishAtHome computerForSchoolwork read30MinsADay
## 2              1             1                     1              0
## 281            0             0                     1              0
## 609            1             1                     1              1
## 614            1             1                    NA              0
## 631            1             1                     1              1
## 726            0             0                     1              1
##     minutesPerWeekEnglish studentsInEnglish schoolHasLibrary publicSchool
## 2                     255                NA                1            1
## 281                    90                30                1            1
## 609                   450                25                1            1
## 614                   450                20                1            1
## 631                   225                13                1            1
## 726                   120                20                1            1
##     urban schoolSize readingScore
## 2       0        808       385.57
## 281     1       1400       296.35
## 609     0       1387       474.40
## 614     0       1387       481.05
## 631     1       1062       672.33
## 726     1       1763       327.29
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
if ((num_nas <- sum(is.na(glb_trnobs_df[, glb_rsp_var_raw]))) > 0)
    stop("glb_trnobs_df$", glb_rsp_var_raw, " contains NAs for ", num_nas, " obs")

if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Combine trnent & newobs into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"

# Check for duplicates in glb_id_var
if (length(glb_id_var) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_trnobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Train"))
    glb_newobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Test"))    
    glb_id_var <- ".rownames"
}
```

```
## Warning: using .rownames as identifiers for observations
```

```r
if (sum(duplicated(glb_allobs_df[, glb_id_var, FALSE])) > 0)
    stop(glb_id_var, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_var)

glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 8.165 8.748   0.583
## 2 inspect.data          2          0 8.748    NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    mycheck_problem_data(glb_allobs_df)
}
glb_chk_data()
```

![](PISA_template2_files/figure-html/inspect.data-1.png) 

```
## [1] "numeric data missing in glb_allobs_df: "
##             preschool       expectBachelors              motherHS 
##                    77                    85                   142 
##       motherBachelors            motherWork              fatherHS 
##                   585                   129                   370 
##       fatherBachelors            fatherWork            selfBornUS 
##                   857                   346                    93 
##          motherBornUS          fatherBornUS         englishAtHome 
##                    94                   171                    98 
## computerForSchoolwork        read30MinsADay minutesPerWeekEnglish 
##                    95                    55                   289 
##     studentsInEnglish      schoolHasLibrary            schoolSize 
##                   363                   201                   231 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                  male             preschool       expectBachelors 
##                  2546                  1448                  1131 
##              motherHS       motherBachelors            motherWork 
##                   629                  3054                  1379 
##              fatherHS       fatherBachelors            fatherWork 
##                   700                  2932                   732 
##            selfBornUS          motherBornUS          fatherBornUS 
##                   382                  1179                  1182 
##         englishAtHome computerForSchoolwork        read30MinsADay 
##                   669                   519                  3688 
## minutesPerWeekEnglish      schoolHasLibrary          publicSchool 
##                    69                   171                   345 
##                 urban 
##                  3228 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##   raceeth .rownames 
##        NA         0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}

#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

myextract_dates_df <- function(df, vars, id_vars, rsp_var) {
    keep_feats <- c(NULL)
    for (var in vars) {
        dates_df            <- df[, id_vars, FALSE]        
        dates_df[, rsp_var] <- df[, rsp_var, FALSE]
        #dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df <- cbind(dates_df, data.frame(.date=strptime(df[, var], 
            glb_date_fmts[[var]], tz=glb_date_tzs[[var]])))
#         print(dates_df[is.na(dates_df$.date), c("ID", "Arrest.fctr", ".date")])
#         print(glb_allobs_df[is.na(dates_df$.date), c("ID", "Arrest.fctr", "Date")])     
#         print(head(glb_allobs_df[grepl("4/7/02 .:..", glb_allobs_df$Date), c("ID", "Arrest.fctr", "Date")]))
#         print(head(strptime(glb_allobs_df[grepl("4/7/02 .:..", glb_allobs_df$Date), "Date"], "%m/%e/%y %H:%M"))
        # Wrong data during EST->EDT transition
#         tmp <- strptime("4/7/02 2:00","%m/%e/%y %H:%M:%S"); print(tmp); print(is.na(tmp))
#         dates_df[dates_df$ID == 2068197, .date] <- tmp
#         grep("(.*?) 2:(.*)", glb_allobs_df[is.na(dates_df$.date), "Date"], value=TRUE)
#         dates_df[is.na(dates_df$.date), ".date"] <- 
#             data.frame(.date=strptime(gsub("(.*?) 2:(.*)", "\\1 3:\\2",
#                 glb_allobs_df[is.na(dates_df$.date), "Date"]), "%m/%e/%y %H:%M"))$.date
        if (sum(is.na(dates_df$.date)) > 0) {
            stop("NA POSIX dates for ", var)
            print(df[is.na(dates_df$.date), c(id_vars, rsp_var, var)])
        }    
        
        .date <- dates_df$.date
        dates_df[, paste0(var, ".POSIX")] <- .date
        dates_df[, paste0(var, ".year")] <- as.numeric(format(.date, "%Y"))
        dates_df[, paste0(var, ".year.fctr")] <- as.factor(format(.date, "%Y")) 
        dates_df[, paste0(var, ".month")] <- as.numeric(format(.date, "%m"))
        dates_df[, paste0(var, ".month.fctr")] <- as.factor(format(.date, "%m"))
        dates_df[, paste0(var, ".date")] <- as.numeric(format(.date, "%d"))
        dates_df[, paste0(var, ".date.fctr")] <- 
            cut(as.numeric(format(.date, "%d")), 5) # by month week  
        dates_df[, paste0(var, ".juliandate")] <- as.numeric(format(.date, "%j"))        
        
        # wkday Sun=0; Mon=1; ...; Sat=6
        dates_df[, paste0(var, ".wkday")] <- as.numeric(format(.date, "%w"))
        dates_df[, paste0(var, ".wkday.fctr")] <- as.factor(format(.date, "%w"))
        
        # Get US Federal Holidays for relevant years
        require(XML)
        doc.html = htmlTreeParse('http://about.usps.com/news/events-calendar/2012-federal-holidays.htm', useInternal = TRUE)
        
#         # Extract all the paragraphs (HTML tag is p, starting at
#         # the root of the document). Unlist flattens the list to
#         # create a character vector.
#         doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
#         # Replace all \n by spaces
#         doc.text = gsub('\\n', ' ', doc.text)
#         # Join all the elements of the character vector into a single
#         # character string, separated by spaces
#         doc.text = paste(doc.text, collapse = ' ')
        
        # parse the tree by tables
        txt <- unlist(strsplit(xpathSApply(doc.html, "//*/table", xmlValue), "\n"))
        # do some clean up with regular expressions
        txt <- grep("day, ", txt, value=TRUE)
        txt <- trimws(gsub("(.*?)day, (.*)", "\\2", txt))
#         txt <- gsub("\t","",txt)
#         txt <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", txt, perl=TRUE)
#         txt <- txt[!(txt %in% c("", "|"))]
        hldays <- strptime(paste(txt, ", 2012", sep=""), "%B %e, %Y")
        dates_df[, paste0(var, ".hlday")] <- 
            ifelse(format(.date, "%Y-%m-%d") %in% hldays, 1, 0)
        
        # NYState holidays 1.9., 13.10., 11.11., 27.11., 25.12.
        
        dates_df[, paste0(var, ".wkend")] <- as.numeric(
            (dates_df[, paste0(var, ".wkday")] %in% c(0, 6)) | 
            dates_df[, paste0(var, ".hlday")] )
        
        dates_df[, paste0(var, ".hour")] <- as.numeric(format(.date, "%H"))
        dates_df[, paste0(var, ".hour.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%H")))) <= 1)
                   vals else cut(vals, 3) # by work-shift    
        dates_df[, paste0(var, ".minute")] <- as.numeric(format(.date, "%M")) 
        dates_df[, paste0(var, ".minute.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%M")))) <= 1)
                   vals else cut(vals, 4) # by quarter-hours    
        dates_df[, paste0(var, ".second")] <- as.numeric(format(.date, "%S")) 
        dates_df[, paste0(var, ".second.fctr")] <- 
            if (length(unique(vals <- as.numeric(format(.date, "%S")))) <= 1)
                   vals else cut(vals, 4) # by quarter-minutes

        dates_df[, paste0(var, ".day.minutes")] <- 
            60 * dates_df[, paste0(var, ".hour")] + 
                 dates_df[, paste0(var, ".minute")]
        if ((unq_vals_n <- length(unique(dates_df[, paste0(var, ".day.minutes")]))) > 1) {
            max_degree <- min(unq_vals_n, 5)
            dates_df[, paste0(var, ".day.minutes.poly.", 1:max_degree)] <- 
                as.matrix(poly(dates_df[, paste0(var, ".day.minutes")], max_degree))
        } else max_degree <- 0   
        
#         print(gp <- myplot_box(df=dates_df, ycol_names="PubDate.day.minutes", 
#                                xcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name=".rownames", 
#                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes.poly.1", colorcol_name=rsp_var))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.day.minutes", 
#                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var))
# 
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes", colorcol_name=rsp_var, smooth=TRUE))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name="PubDate.day.minutes.poly.4", colorcol_name=rsp_var, smooth=TRUE))
#         print(gp <- myplot_scatter(df=dates_df, xcol_name="PubDate.juliandate", 
#                         ycol_name=c("PubDate.day.minutes", "PubDate.day.minutes.poly.4"), 
#                         colorcol_name=rsp_var))
        
#         print(gp <- myplot_scatter(df=subset(dates_df, Popular.fctr=="Y"), 
#                                    xcol_name=paste0(var, ".juliandate"), 
#                         ycol_name=paste0(var, ".day.minutes", colorcol_name=rsp_var))
#         print(gp <- myplot_box(df=dates_df, ycol_names=paste0(var, ".hour"), 
#                                xcol_name=rsp_var))
#         print(gp <- myplot_bar(df=dates_df, ycol_names=paste0(var, ".hour.fctr"), 
#                                xcol_name=rsp_var, 
#                                colorcol_name=paste0(var, ".hour.fctr")))                
        keep_feats <- paste(var, 
            c(".POSIX", ".year.fctr", ".month.fctr", ".date.fctr", ".wkday.fctr", 
              ".wkend", ".hour.fctr", ".minute.fctr", ".second.fctr"), sep="")
        if (max_degree > 0)
            keep_feats <- union(keep_feats, paste(var, 
              paste0(".day.minutes.poly.", 1:max_degree), sep=""))
        keep_feats <- intersect(keep_feats, names(dates_df))        
    }
    #myprint_df(dates_df)
    return(dates_df[, keep_feats])
}

if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, 
                           id_vars=glb_id_var, rsp_var=glb_rsp_var))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          paste(glb_date_vars, c("", ".POSIX"), sep=""))

    for (feat in glb_date_vars) {
        glb_allobs_df <- orderBy(reformulate(paste0(feat, ".POSIX")), glb_allobs_df)
#         print(myplot_scatter(glb_allobs_df, xcol_name=paste0(feat, ".POSIX"),
#                              ycol_name=glb_rsp_var, colorcol_name=glb_rsp_var))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[, paste0(feat, ".POSIX")] >=
                                               strptime("2012-12-01", "%Y-%m-%d"), ], 
                             xcol_name=paste0(feat, ".POSIX"),
                             ycol_name=glb_rsp_var, colorcol_name=paste0(feat, ".wkend")))

        # Create features that measure the gap between previous timestamp in the data
        require(zoo)
        z <- zoo(as.numeric(as.POSIXlt(glb_allobs_df[, paste0(feat, ".POSIX")])))
        glb_allobs_df[, paste0(feat, ".zoo")] <- z
        print(head(glb_allobs_df[, c(glb_id_var, feat, paste0(feat, ".zoo"))]))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[,  paste0(feat, ".POSIX")] >
                                            strptime("2012-10-01", "%Y-%m-%d"), ], 
                            xcol_name=paste0(feat, ".zoo"), ycol_name=glb_rsp_var,
                            colorcol_name=glb_rsp_var))
        b <- zoo(, seq(nrow(glb_allobs_df)))
        
        last1 <- as.numeric(merge(z-lag(z, -1), b, all=TRUE)); last1[is.na(last1)] <- 0
        glb_allobs_df[, paste0(feat, ".last1.log")] <- log(1 + last1)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last1.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last1.log"), 
                               xcol_name=glb_rsp_var))
        
        last10 <- as.numeric(merge(z-lag(z, -10), b, all=TRUE)); last10[is.na(last10)] <- 0
        glb_allobs_df[, paste0(feat, ".last10.log")] <- log(1 + last10)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last10.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last10.log"), 
                               xcol_name=glb_rsp_var))
        
        last100 <- as.numeric(merge(z-lag(z, -100), b, all=TRUE)); last100[is.na(last100)] <- 0
        glb_allobs_df[, paste0(feat, ".last100.log")] <- log(1 + last100)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last100.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last100.log"), 
                               xcol_name=glb_rsp_var))
        
        glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                                c(paste0(feat, ".zoo")))
        # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
        # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
        # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
        # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
        # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
        # 
        # 
        # # order table
        # all2 = all2[order(all2$id),]
        # 
        # ## fill in NAs
        # # count averages
        # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
        #     last1=mean(last1, na.rm=TRUE),
        #     last3=mean(last3, na.rm=TRUE),
        #     last5=mean(last5, na.rm=TRUE),
        #     last10=mean(last10, na.rm=TRUE),
        #     last20=mean(last20, na.rm=TRUE),
        #     last50=mean(last50, na.rm=TRUE)
        # )
        # 
        # # fill in averages
        # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
        # na.merge = na.merge[order(na.merge$id),]
        # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
        #     y = paste0(i, ".y")
        #     idx = is.na(all2[[i]])
        #     all2[idx,][[i]] <- na.merge[idx,][[y]]
        # }
        # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
    }
}

# check distribution of all numeric data
dsp_numeric_feats_dstrb <- function(feats_vctr) {
    for (feat in feats_vctr) {
        print(sprintf("feat: %s", feat))
        if (glb_is_regression)
            gp <- myplot_scatter(df=glb_allobs_df, ycol_name=glb_rsp_var, xcol_name=feat,
                                 smooth=TRUE)
        if (glb_is_classification)
            gp <- myplot_box(df=glb_allobs_df, ycol_names=feat, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, feat], "factor"))
            gp <- gp + facet_wrap(reformulate(feat))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

#         <col_name> = trunc(<col2_name> / 100),

        .rnorm = rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Merge some <descriptor>
# glb_allobs_df$<descriptor>.my <- glb_allobs_df$<descriptor>
# glb_allobs_df[grepl("\\bAIRPORT\\b", glb_allobs_df$<descriptor>.my),
#               "<descriptor>.my"] <- "AIRPORT"
# glb_allobs_df$<descriptor>.my <-
#     plyr::revalue(glb_allobs_df$<descriptor>.my, c(
#         "ABANDONED BUILDING" = "OTHER",
#         "##"                      = "##"
#     ))
# print(<descriptor>_freq_df <- mycreate_sqlxtab_df(glb_allobs_df, c("<descriptor>.my")))
# # print(dplyr::filter(<descriptor>_freq_df, grepl("(MEDICAL|DENTAL|OFFICE)", <descriptor>.my)))
# # print(dplyr::filter(dplyr::select(glb_allobs_df, -<var.zoo>), 
# #                     grepl("STORE", <descriptor>.my)))
# glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, "<descriptor>")

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_feats_dstrb(feats_vctr=setdiff(names(glb_allobs_df), 
        c(myfind_chr_cols_df(glb_allobs_df), glb_rsp_var_raw, glb_rsp_var, 
          glb_exclude_vars_as_features)))
```

```
## [1] "feat: grade"
```

![](PISA_template2_files/figure-html/inspect.data-2.png) 

```
## [1] "feat: male"
```

![](PISA_template2_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: preschool"
```

```
## Warning in loop_apply(n, do.ply): Removed 77 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 77 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 77 rows containing missing values
## (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: expectBachelors"
```

```
## Warning in loop_apply(n, do.ply): Removed 85 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 85 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 85 rows containing missing values
## (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-5.png) 

```
## [1] "feat: motherHS"
```

```
## Warning in loop_apply(n, do.ply): Removed 142 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 142 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 142 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-6.png) 

```
## [1] "feat: motherBachelors"
```

```
## Warning in loop_apply(n, do.ply): Removed 585 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 585 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 585 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-7.png) 

```
## [1] "feat: motherWork"
```

```
## Warning in loop_apply(n, do.ply): Removed 129 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 129 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 129 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-8.png) 

```
## [1] "feat: fatherHS"
```

```
## Warning in loop_apply(n, do.ply): Removed 370 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 370 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 370 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-9.png) 

```
## [1] "feat: fatherBachelors"
```

```
## Warning in loop_apply(n, do.ply): Removed 857 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 857 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 857 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-10.png) 

```
## [1] "feat: fatherWork"
```

```
## Warning in loop_apply(n, do.ply): Removed 346 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 346 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 346 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-11.png) 

```
## [1] "feat: selfBornUS"
```

```
## Warning in loop_apply(n, do.ply): Removed 93 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 93 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 93 rows containing missing values
## (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-12.png) 

```
## [1] "feat: motherBornUS"
```

```
## Warning in loop_apply(n, do.ply): Removed 94 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 94 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 94 rows containing missing values
## (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-13.png) 

```
## [1] "feat: fatherBornUS"
```

```
## Warning in loop_apply(n, do.ply): Removed 171 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 171 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 171 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-14.png) 

```
## [1] "feat: englishAtHome"
```

```
## Warning in loop_apply(n, do.ply): Removed 98 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 98 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 98 rows containing missing values
## (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-15.png) 

```
## [1] "feat: computerForSchoolwork"
```

```
## Warning in loop_apply(n, do.ply): Removed 95 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 95 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 95 rows containing missing values
## (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-16.png) 

```
## [1] "feat: read30MinsADay"
```

```
## Warning in loop_apply(n, do.ply): Removed 55 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 55 rows containing missing values
## (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 55 rows containing missing values
## (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-17.png) 

```
## [1] "feat: minutesPerWeekEnglish"
```

```
## Warning in loop_apply(n, do.ply): Removed 289 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 289 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 289 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-18.png) 

```
## [1] "feat: studentsInEnglish"
```

```
## Warning in loop_apply(n, do.ply): Removed 363 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 363 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 363 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-19.png) 

```
## [1] "feat: schoolHasLibrary"
```

```
## Warning in loop_apply(n, do.ply): Removed 201 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 201 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 201 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-20.png) 

```
## [1] "feat: publicSchool"
```

![](PISA_template2_files/figure-html/inspect.data-21.png) 

```
## [1] "feat: urban"
```

![](PISA_template2_files/figure-html/inspect.data-22.png) 

```
## [1] "feat: schoolSize"
```

```
## Warning in loop_apply(n, do.ply): Removed 231 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 231 rows containing missing
## values (stat_smooth).
```

```
## Warning in loop_apply(n, do.ply): Removed 231 rows containing missing
## values (geom_point).
```

![](PISA_template2_files/figure-html/inspect.data-23.png) 

```
## [1] "feat: .rnorm"
```

![](PISA_template2_files/figure-html/inspect.data-24.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 
# print(mycreate_sqlxtab_df(glb_allobs_df, c("<col1_name>", "<col2_name>")))

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("CocaCola", "ProcterGamble")), 
#                   "Date.POSIX", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.POSIXlt("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1983-01-01")))        
#         )
# print(myplot_line(subset(glb_trnobs_df, Date.POSIX > as.POSIXct("2004-01-01")), 
#                   "Date.POSIX", "StockPrice") +
#     geom_line(aes(color=Symbol)) + 
#     coord_cartesian(xlim=c(as.POSIXct("1990-01-01"),
#                            as.POSIXct("2000-01-01"))) +     
#     coord_cartesian(ylim=c(0, 250)) +     
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-09-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-11-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5) +
#         geom_vline(xintercept=84))

rm(last1, last10, last100)
```

```
## Warning in rm(last1, last10, last100): object 'last1' not found
```

```
## Warning in rm(last1, last10, last100): object 'last10' not found
```

```
## Warning in rm(last1, last10, last100): object 'last100' not found
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "scrub.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0  8.748 23.186  14.438
## 3   scrub.data          2          1 23.187     NA      NA
```

### Step `2.1: scrub data`

```r
mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##             preschool       expectBachelors              motherHS 
##                    77                    85                   142 
##       motherBachelors            motherWork              fatherHS 
##                   585                   129                   370 
##       fatherBachelors            fatherWork            selfBornUS 
##                   857                   346                    93 
##          motherBornUS          fatherBornUS         englishAtHome 
##                    94                   171                    98 
## computerForSchoolwork        read30MinsADay minutesPerWeekEnglish 
##                    95                    55                   289 
##     studentsInEnglish      schoolHasLibrary            schoolSize 
##                   363                   201                   231 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                  male             preschool       expectBachelors 
##                  2546                  1448                  1131 
##              motherHS       motherBachelors            motherWork 
##                   629                  3054                  1379 
##              fatherHS       fatherBachelors            fatherWork 
##                   700                  2932                   732 
##            selfBornUS          motherBornUS          fatherBornUS 
##                   382                  1179                  1182 
##         englishAtHome computerForSchoolwork        read30MinsADay 
##                   669                   519                  3688 
## minutesPerWeekEnglish      schoolHasLibrary          publicSchool 
##                    69                   171                   345 
##                 urban 
##                  3228 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##   raceeth .rownames 
##        NA         0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

# sel_obs <- function(Popular=NULL, 
#                     NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
#         Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
#         Headline.pfx=NULL, NewsDesk.nb=NULL, .clusterid=NULL, myCategory=NULL,
#         perl=FALSE) {
sel_obs <- function(vars_lst) {
    tmp_df <- glb_allobs_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
            tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_df <- tmp_df[tmp_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_df <- tmp_df[tmp_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_df <- 
            tmp_df[grep(Headline.contains, tmp_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_df <- 
            tmp_df[grep(Snippet.contains, tmp_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_df <- 
            tmp_df[grep(Abstract.contains, tmp_df$Abstract, perl=perl), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_df), fixed=TRUE, value=TRUE))
            > 0) tmp_df <- 
                tmp_df[tmp_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_allobs_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_allobs_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    if (!is.null(.clusterid)) {
        if (any(grepl(".clusterid", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$clusterid == clusterid, ] else
        warning("glb_allobs_df does not contain clusterid; ignoring that filter")                       }
    if (!is.null(myCategory)) {    
        if (!(myCategory %in% names(glb_allobs_df)))
            tmp_df <-
                tmp_df[tmp_df$myCategory == myCategory, ] else
        warning("glb_allobs_df does not contain myCategory; ignoring that filter")                    
    }    
    
    return(glb_allobs_df$UniqueID %in% tmp_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_var, "Popular")],
                    sel_df[, c(glb_id_var, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 23.187 28.877    5.69
## 4 transform.data          2          2 28.878     NA      NA
```

```r
### Mapping dictionary
#sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_map_vars)) {
    for (feat in glb_map_vars) {
        map_df <- myimport_data(url=glb_map_urls[[feat]], 
                                            comment="map_df", 
                                           print_diagn=TRUE)
        glb_allobs_df <- mymap_codes(glb_allobs_df, feat, names(map_df)[2], 
                                     map_df, map_join_col_name=names(map_df)[1], 
                                     map_tgt_col_name=names(map_df)[2])
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_map_vars)
}

### Forced Assignments
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_assign_vars) {
    new_feat <- paste0(feat, ".my")
    print(sprintf("Forced Assignments for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_allobs_df[, feat]
    
    pairs <- glb_assign_pairs_lst[[feat]]
    for (pair_ix in 1:length(pairs$from)) {
        if (is.na(pairs$from[pair_ix]))
            nobs <- nrow(filter(glb_allobs_df, 
                                is.na(eval(parse(text=feat),
                                            envir=glb_allobs_df)))) else
            nobs <- sum(glb_allobs_df[, feat] == pairs$from[pair_ix])
        #nobs <- nrow(filter(glb_allobs_df, is.na(Married.fctr)))    ; print(nobs)
        
        if ((is.na(pairs$from[pair_ix])) && (is.na(pairs$to[pair_ix])))
            stop("what are you trying to do ???")
        if (is.na(pairs$from[pair_ix]))
            glb_allobs_df[is.na(glb_allobs_df[, feat]), new_feat] <- 
                pairs$to[pair_ix] else
            glb_allobs_df[glb_allobs_df[, feat] == pairs$from[pair_ix], new_feat] <- 
                pairs$to[pair_ix]
                    
        print(sprintf("    %s -> %s for %s obs", 
                      pairs$from[pair_ix], pairs$to[pair_ix], format(nobs, big.mark=",")))
    }

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_assign_vars)
}

### Transformations using mapping functions
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_transform_vars) {
    new_feat <- paste0(feat, glb_transform_lst[[feat]]$sfx)
    print(sprintf("Applying mapping function for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_transform_lst[[feat]]$mapfn(glb_allobs_df[, feat])

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_transform_vars)
}

### Derivations using mapping functions
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (new_feat in glb_derive_vars) {
    print(sprintf("Creating new feature: %s...", new_feat))
    args_lst <- NULL 
    for (arg in glb_derive_lst[[new_feat]]$args) 
        args_lst[[arg]] <- glb_allobs_df[, arg]
    glb_allobs_df[, new_feat] <- do.call(glb_derive_lst[[new_feat]]$mapfn, args_lst)
}
```

### Step `2.2: transform data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn   end elapsed
## 4      transform.data          2          2 28.878 28.91   0.032
## 5 manage.missing.data          2          3 28.910    NA      NA
```

```r
# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
##             preschool       expectBachelors              motherHS 
##                    77                    85                   142 
##       motherBachelors            motherWork              fatherHS 
##                   585                   129                   370 
##       fatherBachelors            fatherWork            selfBornUS 
##                   857                   346                    93 
##          motherBornUS          fatherBornUS         englishAtHome 
##                    94                   171                    98 
## computerForSchoolwork        read30MinsADay minutesPerWeekEnglish 
##                    95                    55                   289 
##     studentsInEnglish      schoolHasLibrary            schoolSize 
##                   363                   201                   231 
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                  male             preschool       expectBachelors 
##                  2546                  1448                  1131 
##              motherHS       motherBachelors            motherWork 
##                   629                  3054                  1379 
##              fatherHS       fatherBachelors            fatherWork 
##                   700                  2932                   732 
##            selfBornUS          motherBornUS          fatherBornUS 
##                   382                  1179                  1182 
##         englishAtHome computerForSchoolwork        read30MinsADay 
##                   669                   519                  3688 
## minutesPerWeekEnglish      schoolHasLibrary          publicSchool 
##                    69                   171                   345 
##                 urban 
##                  3228 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##   raceeth .rownames 
##        NA         0
```

```r
glb_allobs_df <- na.omit(glb_allobs_df)

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    # complete(mice()) changes attributes of factors even though values don't change
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col], inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (length(myfind_numerics_missing(glb_allobs_df)) > 0) &&
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
    
mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##                  male             preschool       expectBachelors 
##                  1707                   933                   587 
##              motherHS       motherBachelors            motherWork 
##                   376                  2198                   916 
##              fatherHS       fatherBachelors            fatherWork 
##                   452                  2243                   493 
##            selfBornUS          motherBornUS          fatherBornUS 
##                   238                   735                   743 
##         englishAtHome computerForSchoolwork        read30MinsADay 
##                   420                   282                  2383 
## minutesPerWeekEnglish      schoolHasLibrary          publicSchool 
##                    27                   103                   285 
##                 urban 
##                  2178 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##   raceeth .rownames 
##         0         0
```

## Step `2.3: manage missing data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 5 manage.missing.data          2          3 28.910 29.007   0.097
## 6    extract.features          3          0 29.007     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 29.013  NA      NA
```

```r
# Options:
#   Select Tf, log(1 + Tf), Tf-IDF or BM25Tf-IDf

# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 1                extract.features_bgn          1          0 29.013 29.021
## 2 extract.features_factorize.str.vars          2          0 29.021     NA
##   elapsed
## 1   0.008
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##     raceeth        .src   .rownames 
##   "raceeth"      ".src" ".rownames"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               c(glb_exclude_vars_as_features, glb_txt_vars))) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- 
            relevel(factor(glb_allobs_df[, var]),
                    names(which.max(table(glb_allobs_df[, var], useNA = "ifany"))))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}
```

```
## Warning: Creating factors of string variable: raceeth: # of unique values:
## 7
```

```r
if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(rex_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(rex_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }

#     match_lst <- gregexpr("\\bok(?!ay)", txt_vctr[746], ignore.case = FALSE, perl=TRUE); print(match_lst)
    dsp_pattern <- function(rex_str, ignore.case=TRUE, print.all=TRUE) {
        match_lst <- gregexpr(rex_str, txt_vctr, ignore.case = ignore.case, perl=TRUE)
        match_lst <- regmatches(txt_vctr, match_lst)
        match_df <- data.frame(matches=sapply(match_lst, 
                                              function (elems) paste(elems, collapse="#")))
        match_df <- subset(match_df, matches != "")
        if (print.all)
            print(match_df)
        return(match_df)
    }
    
    dsp_matches <- function(rex_str, ix) {
        print(match_pos <- gregexpr(rex_str, txt_vctr[ix], perl=TRUE))
        print(str_sub(txt_vctr[ix], (match_pos[[1]] / 100) *  99 +   0, 
                                    (match_pos[[1]] / 100) * 100 + 100))        
    }

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], 
                        glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#chk.equal( 1, 100)
#dsp.equal(86, 90)
    
    glb_txt_map_df <- read.csv("mytxt_map.csv", comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[163, "rex_str"])
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rpl_str == "versus", "rex_str"])             
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining OK in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "(?<!(BO|HO|LO))OK(?!(E\\!|ED|IE|IN|S ))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "Ok(?!(a\\.|ay|in|ra|um))", ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "(?<!( b| B| c| C| g| G| j| M| p| P| w| W| r| Z|\\(b|ar|bo|Bo|co|Co|Ew|gk|go|ho|ig|jo|kb|ke|Ke|ki|lo|Lo|mo|mt|no|No|po|ra|ro|sm|Sm|Sp|to|To))ok(?!(ay|bo|e |e\\)|e,|e\\.|eb|ed|el|en|er|es|ey|i |ie|in|it|ka|ke|ki|ly|on|oy|ra|st|u |uc|uy|yl|yo))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))
    }    
    # txt_vctr <- glb_txt_lst[[glb_txt_vars[1]]]
    # print(chk_pattern_freq(rex_str <- "(?<!( b| c| C| p|\\(b|bo|co|lo|Lo|Sp|to|To))ok(?!(ay|e |e\\)|e,|e\\.|ed|el|en|es|ey|ie|in|on|ra))", ignore.case=FALSE))
    # print(chk_pattern_freq(rex_str <- "ok(?!(ay|el|on|ra))", ignore.case=FALSE))
    # dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
    # dsp_matches(rex_str, ix=8)
    # substr(txt_vctr[86], 5613, 5620)
    # substr(glb_allobs_df[301, "review"], 550, 650)

#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "([[:upper:]]\\.( *)){2,}", ignore.case=FALSE))
        
        # Check for names
        print(subset(chk_pattern_freq(rex_str <- "(([[:upper:]]+)\\.( *)){1}",
                                      ignore.case=FALSE),
                     .n > 1))
        # dsp_pattern(rex_str="(OK\\.( *)){1}", ignore.case=FALSE)
        # dsp_matches(rex_str="(OK\\.( *)){1}", ix=557)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)(\\B)", ix=461)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)", ix=461)        
        #print(str_sub(txt_vctr[676], 10100, 10200))
        #print(str_sub(txt_vctr[74], 1, -1))        
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, 
                             preserve_intra_word_dashes=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        filter_df <- read.csv("mytxt_compound.csv", comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, tolower) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english"))) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#sav_allobs_df <- glb_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)
        txt_X_df[is.nan(txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")]),
                 paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 0

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_var, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
    rm(log_X_df, txt_X_df)
}

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_DTM' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_full_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_sprs_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_corpus' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 29.021 29.039
## 3                extract.features_end          3          0 29.039     NA
##   elapsed
## 2   0.018
## 3      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 29.021 29.039
## 1                extract.features_bgn          1          0 29.013 29.021
##   elapsed duration
## 2   0.018    0.018
## 1   0.008    0.008
## [1] "Total Elapsed Time: 29.039 secs"
```

![](PISA_template2_files/figure-html/extract.features-1.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](PISA_template2_files/figure-html/extract.features-2.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 6 extract.features          3          0 29.007 30.134   1.127
## 7     cluster.data          4          0 30.135     NA      NA
```

## Step `4.0: cluster data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##             label step_major step_minor    bgn    end elapsed
## 7    cluster.data          4          0 30.135 30.408   0.274
## 8 select.features          5          0 30.409     NA      NA
```

## Step `5.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##                                          id        cor.y exclude.as.feat
## expectBachelors             expectBachelors  0.344303760               0
## fatherBachelors             fatherBachelors  0.276978192               0
## raceeth.fctr                   raceeth.fctr -0.245787319               0
## grade                                 grade  0.238684014               0
## read30MinsADay               read30MinsADay  0.238329895               0
## motherBachelors             motherBachelors  0.226761511               0
## computerForSchoolwork computerForSchoolwork  0.181202433               0
## fatherHS                           fatherHS  0.168826773               0
## motherHS                           motherHS  0.138982897               0
## male                                   male -0.128458124               0
## publicSchool                   publicSchool -0.111669005               0
## englishAtHome                 englishAtHome  0.107747418               0
## fatherWork                       fatherWork  0.076238189               0
## fatherBornUS                   fatherBornUS  0.065034070               0
## motherBornUS                   motherBornUS  0.051459712               0
## preschool                         preschool  0.051289362               0
## minutesPerWeekEnglish minutesPerWeekEnglish  0.036465599               0
## motherWork                       motherWork  0.024461211               0
## schoolHasLibrary           schoolHasLibrary  0.023557613               0
## schoolSize                       schoolSize  0.020509236               0
## urban                                 urban -0.016016868               0
## selfBornUS                       selfBornUS  0.013443329               0
## studentsInEnglish         studentsInEnglish  0.010252212               0
## .rnorm                               .rnorm -0.003734208               0
##                         cor.y.abs
## expectBachelors       0.344303760
## fatherBachelors       0.276978192
## raceeth.fctr          0.245787319
## grade                 0.238684014
## read30MinsADay        0.238329895
## motherBachelors       0.226761511
## computerForSchoolwork 0.181202433
## fatherHS              0.168826773
## motherHS              0.138982897
## male                  0.128458124
## publicSchool          0.111669005
## englishAtHome         0.107747418
## fatherWork            0.076238189
## fatherBornUS          0.065034070
## motherBornUS          0.051459712
## preschool             0.051289362
## minutesPerWeekEnglish 0.036465599
## motherWork            0.024461211
## schoolHasLibrary      0.023557613
## schoolSize            0.020509236
## urban                 0.016016868
## selfBornUS            0.013443329
## studentsInEnglish     0.010252212
## .rnorm                0.003734208
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## Loading required package: reshape2
```

```
## [1] "cor(fatherBornUS, motherBornUS)=0.7709"
## [1] "cor(readingScore, fatherBornUS)=0.0650"
## [1] "cor(readingScore, motherBornUS)=0.0515"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified motherBornUS as highly correlated with
## fatherBornUS
```

```
##                       id        cor.y exclude.as.feat   cor.y.abs
## 4        expectBachelors  0.344303760               0 0.344303760
## 5        fatherBachelors  0.276978192               0 0.276978192
## 9                  grade  0.238684014               0 0.238684014
## 19        read30MinsADay  0.238329895               0 0.238329895
## 12       motherBachelors  0.226761511               0 0.226761511
## 2  computerForSchoolwork  0.181202433               0 0.181202433
## 7               fatherHS  0.168826773               0 0.168826773
## 14              motherHS  0.138982897               0 0.138982897
## 3          englishAtHome  0.107747418               0 0.107747418
## 8             fatherWork  0.076238189               0 0.076238189
## 6           fatherBornUS  0.065034070               0 0.065034070
## 13          motherBornUS  0.051459712               0 0.051459712
## 16             preschool  0.051289362               0 0.051289362
## 11 minutesPerWeekEnglish  0.036465599               0 0.036465599
## 15            motherWork  0.024461211               0 0.024461211
## 20      schoolHasLibrary  0.023557613               0 0.023557613
## 21            schoolSize  0.020509236               0 0.020509236
## 22            selfBornUS  0.013443329               0 0.013443329
## 23     studentsInEnglish  0.010252212               0 0.010252212
## 1                 .rnorm -0.003734208               0 0.003734208
## 24                 urban -0.016016868               0 0.016016868
## 17          publicSchool -0.111669005               0 0.111669005
## 10                  male -0.128458124               0 0.128458124
## 18          raceeth.fctr -0.245787319               0 0.245787319
##      cor.high.X freqRatio percentUnique zeroVar   nzv myNearZV
## 4          <NA>  5.035000    0.08285004   FALSE FALSE    FALSE
## 5          <NA>  1.870392    0.08285004   FALSE FALSE    FALSE
## 9          <NA>  3.523422    0.20712510   FALSE FALSE    FALSE
## 19         <NA>  2.315934    0.08285004   FALSE FALSE    FALSE
## 12         <NA>  1.749431    0.08285004   FALSE FALSE    FALSE
## 2          <NA> 10.833333    0.08285004   FALSE FALSE    FALSE
## 7          <NA>  6.940789    0.08285004   FALSE FALSE    FALSE
## 14         <NA>  8.617530    0.08285004   FALSE FALSE    FALSE
## 3          <NA>  7.440559    0.08285004   FALSE FALSE    FALSE
## 8          <NA>  5.997101    0.08285004   FALSE FALSE    FALSE
## 6          <NA>  3.660232    0.08285004   FALSE FALSE    FALSE
## 13 fatherBornUS  3.761341    0.08285004   FALSE FALSE    FALSE
## 16         <NA>  2.668693    0.08285004   FALSE FALSE    FALSE
## 11         <NA>  1.142349    5.88235294   FALSE FALSE    FALSE
## 15         <NA>  2.783699    0.08285004   FALSE FALSE    FALSE
## 20         <NA> 33.985507    0.08285004   FALSE  TRUE    FALSE
## 21         <NA>  1.076923    6.13090307   FALSE FALSE    FALSE
## 22         <NA> 14.675325    0.08285004   FALSE FALSE    FALSE
## 23         <NA>  1.061364    2.11267606   FALSE FALSE    FALSE
## 1          <NA>  1.000000  100.00000000   FALSE FALSE    FALSE
## 24         <NA>  1.755708    0.08285004   FALSE FALSE    FALSE
## 17         <NA> 11.130653    0.08285004   FALSE FALSE    FALSE
## 10         <NA>  1.004983    0.08285004   FALSE FALSE    FALSE
## 18         <NA>  2.940000    0.28997514   FALSE FALSE    FALSE
##    is.cor.y.abs.low
## 4             FALSE
## 5             FALSE
## 9             FALSE
## 19            FALSE
## 12            FALSE
## 2             FALSE
## 7             FALSE
## 14            FALSE
## 3             FALSE
## 8             FALSE
## 6             FALSE
## 13            FALSE
## 16            FALSE
## 11            FALSE
## 15            FALSE
## 20            FALSE
## 21            FALSE
## 22            FALSE
## 23            FALSE
## 1             FALSE
## 24            FALSE
## 17            FALSE
## 10            FALSE
## 18            FALSE
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 1 rows containing missing values
## (geom_point).
```

![](PISA_template2_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##  [1] id               cor.y            exclude.as.feat  cor.y.abs       
##  [5] cor.high.X       freqRatio        percentUnique    zeroVar         
##  [9] nzv              myNearZV         is.cor.y.abs.low
## <0 rows> (or 0-length row.names)
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
## named integer(0)
## [1] "numeric data w/ 0s in : "
##                  male             preschool       expectBachelors 
##                  1707                   933                   587 
##              motherHS       motherBachelors            motherWork 
##                   376                  2198                   916 
##              fatherHS       fatherBachelors            fatherWork 
##                   452                  2243                   493 
##            selfBornUS          motherBornUS          fatherBornUS 
##                   238                   735                   743 
##         englishAtHome computerForSchoolwork        read30MinsADay 
##                   420                   282                  2383 
## minutesPerWeekEnglish      schoolHasLibrary          publicSchool 
##                    27                   103                   285 
##                 urban 
##                  2178 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
##   raceeth .rownames 
##         0         0
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 30.409 31.157   0.749
## 9 partition.data.training          6          0 31.158     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newobs_df) * 1.1 / nrow(glb_trnobs_df)))
    glb_fitobs_df <- glb_trnobs_df[split, ] 
    glb_OOBobs_df <- glb_trnobs_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## [1] "Newdata contains non-NA data for readingScore; setting OOB to Newdata"
```

```r
if (!is.null(glb_max_fitobs) && (nrow(glb_fitobs_df) > glb_max_fitobs)) {
    warning("glb_fitobs_df restricted to glb_max_fitobs: ", 
            format(glb_max_fitobs, big.mark=","))
    org_fitobs_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitobs_df[split <- sample.split(org_fitobs_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitobs), ]
    org_fitobs_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_fitobs_df[, glb_id_var], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_OOBobs_df[, glb_id_var], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_vars)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_vars)
    glb_ctgry_df <- merge(newobs_ctgry_df, OOBobs_ctgry_df, by=glb_category_vars
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 24 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_var) && glb_id_var != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_var, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                        id exclude.as.feat rsp_var
## readingScore readingScore            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
if (glb_id_var != ".rownames")
    print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var)) else
    print(subset(glb_feats_df, rsp_var_raw | rsp_var))    
```

```
##                        id cor.y exclude.as.feat cor.y.abs cor.high.X
## readingScore readingScore    NA            TRUE        NA       <NA>
##              freqRatio percentUnique zeroVar nzv myNearZV is.cor.y.abs.low
## readingScore        NA            NA      NA  NA       NA               NA
##              interaction.feat rsp_var_raw rsp_var
## readingScore               NA          NA    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
## character(0)
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 3404   29
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 2414   28
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 2414   28
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 990  28
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 990  28
```

```r
# # Does not handle NULL or length(glb_id_var) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_trnobs_df[, glb_id_var], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_fitobs_df[, glb_id_var], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_OOBobs_df[, glb_id_var], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_newobs_df[, glb_id_var], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)
```

```
## Warning in rm(split): object 'split' not found
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 31.158 31.446   0.288
## 10              fit.models          7          0 31.446     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.lm"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
```

![](PISA_template2_files/figure-html/fit.models_0-1.png) ![](PISA_template2_files/figure-html/fit.models_0-2.png) ![](PISA_template2_files/figure-html/fit.models_0-3.png) ![](PISA_template2_files/figure-html/fit.models_0-4.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -273.029  -62.199    2.119   63.642  228.151 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 517.9716     1.8190 284.749   <2e-16 ***
## .rnorm       -0.3351     1.8271  -0.183    0.855    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 89.34 on 2412 degrees of freedom
## Multiple R-squared:  1.394e-05,	Adjusted R-squared:  -0.0004006 
## F-statistic: 0.03363 on 1 and 2412 DF,  p-value: 0.8545
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##   model_id model_method  feats max.nTuningRuns min.elapsedtime.everything
## 1   MFO.lm           lm .rnorm               0                      0.472
##   min.elapsedtime.final max.R.sq.fit min.RMSE.fit  max.R.sq.OOB
## 1                 0.003 1.394431e-05     89.30657 -0.0003464439
##   min.RMSE.OOB max.Adj.R.sq.fit
## 1     88.79131    -0.0004006436
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: expectBachelors, fatherBachelors"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.119 on full training set
```

```
## Loading required package: rpart.plot
```

![](PISA_template2_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 2414 
## 
##          CP nsplit rel error
## 1 0.1185451      0         1
## 
## Node number 1: 2414 observations
##   mean=517.9629, MSE=7975.774 
## 
## n= 2414 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 2414 19253520 517.9629 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##               model_id model_method                            feats
## 1 Max.cor.Y.cv.0.rpart        rpart expectBachelors, fatherBachelors
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.581                 0.038
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1            0     89.30719            0     88.77593
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: expectBachelors, fatherBachelors"
## Fitting cp = 0 on full training set
```

![](PISA_template2_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 2414 
## 
##             CP nsplit rel error
## 1 0.1185450792      0 1.0000000
## 2 0.0505567098      1 0.8814549
## 3 0.0001010536      2 0.8308982
## 4 0.0000000000      3 0.8307972
## 
## Variable importance
## expectBachelors fatherBachelors 
##              70              30 
## 
## Node number 1: 2414 observations,    complexity param=0.1185451
##   mean=517.9629, MSE=7975.774 
##   left son=2 (400 obs) right son=3 (2014 obs)
##   Primary splits:
##       expectBachelors < 0.5 to the left,  improve=0.11854510, (0 missing)
##       fatherBachelors < 0.5 to the left,  improve=0.07671692, (0 missing)
## 
## Node number 2: 400 observations,    complexity param=0.0001010536
##   mean=448.9663, MSE=6279.129 
##   left son=4 (53 obs) right son=5 (347 obs)
##   Primary splits:
##       fatherBachelors < 0.5 to the right, improve=0.0007746449, (0 missing)
## 
## Node number 3: 2014 observations,    complexity param=0.05055671
##   mean=531.6663, MSE=7179.472 
##   left son=6 (1226 obs) right son=7 (788 obs)
##   Primary splits:
##       fatherBachelors < 0.5 to the left,  improve=0.06731889, (0 missing)
## 
## Node number 4: 53 observations
##   mean=443.323, MSE=7918.368 
## 
## Node number 5: 347 observations
##   mean=449.8282, MSE=6023.148 
## 
## Node number 6: 1226 observations
##   mean=514.0412, MSE=6491.536 
## 
## Node number 7: 788 observations
##   mean=559.0881, MSE=7014.517 
## 
## n= 2414 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 2414 19253520.0 517.9629  
##   2) expectBachelors< 0.5 400  2511651.0 448.9663  
##     4) fatherBachelors>=0.5 53   419673.5 443.3230 *
##     5) fatherBachelors< 0.5 347  2090032.0 449.8282 *
##   3) expectBachelors>=0.5 2014 14459460.0 531.6663  
##     6) fatherBachelors< 0.5 1226  7958623.0 514.0412 *
##     7) fatherBachelors>=0.5 788  5527440.0 559.0881 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##                    model_id model_method                            feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart expectBachelors, fatherBachelors
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.462                 0.036
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1    0.1692028     81.40178    0.1529903     81.70327
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: expectBachelors, fatherBachelors"
```

```
## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
## trainInfo, : There were missing values in resampled performance measures.
```

```
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.000101 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](PISA_template2_files/figure-html/fit.models_0-7.png) ![](PISA_template2_files/figure-html/fit.models_0-8.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 2414 
## 
##             CP nsplit rel error
## 1 0.1185450792      0 1.0000000
## 2 0.0505567098      1 0.8814549
## 3 0.0001010536      2 0.8308982
## 
## Variable importance
## expectBachelors fatherBachelors 
##              70              30 
## 
## Node number 1: 2414 observations,    complexity param=0.1185451
##   mean=517.9629, MSE=7975.774 
##   left son=2 (400 obs) right son=3 (2014 obs)
##   Primary splits:
##       expectBachelors < 0.5 to the left,  improve=0.11854510, (0 missing)
##       fatherBachelors < 0.5 to the left,  improve=0.07671692, (0 missing)
## 
## Node number 2: 400 observations
##   mean=448.9663, MSE=6279.129 
## 
## Node number 3: 2014 observations,    complexity param=0.05055671
##   mean=531.6663, MSE=7179.472 
##   left son=6 (1226 obs) right son=7 (788 obs)
##   Primary splits:
##       fatherBachelors < 0.5 to the left,  improve=0.06731889, (0 missing)
## 
## Node number 6: 1226 observations
##   mean=514.0412, MSE=6491.536 
## 
## Node number 7: 788 observations
##   mean=559.0881, MSE=7014.517 
## 
## n= 2414 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 2414 19253520 517.9629  
##   2) expectBachelors< 0.5 400  2511651 448.9663 *
##   3) expectBachelors>=0.5 2014 14459460 531.6663  
##     6) fatherBachelors< 0.5 1226  7958623 514.0412 *
##     7) fatherBachelors>=0.5 788  5527440 559.0881 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##          model_id model_method                            feats
## 1 Max.cor.Y.rpart        rpart expectBachelors, fatherBachelors
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      0.961                 0.039
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.1691018     81.58425    0.1536399     81.67193         0.165659
##   min.RMSESD.fit max.RsquaredSD.fit
## 1       1.066654        0.009491393
```

```r
# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.lm"
## [1] "    indep_vars: expectBachelors, fatherBachelors"
## Aggregating results
## Fitting final model on full training set
```

![](PISA_template2_files/figure-html/fit.models_0-9.png) ![](PISA_template2_files/figure-html/fit.models_0-10.png) ![](PISA_template2_files/figure-html/fit.models_0-11.png) ![](PISA_template2_files/figure-html/fit.models_0-12.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -264.243  -54.232    3.101   56.783  257.045 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      443.595      4.114  107.83   <2e-16 ***
## expectBachelors   72.210      4.568   15.81   <2e-16 ***
## fatherBachelors   40.538      3.565   11.37   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 81.74 on 2411 degrees of freedom
## Multiple R-squared:  0.1634,	Adjusted R-squared:  0.1627 
## F-statistic: 235.5 on 2 and 2411 DF,  p-value: < 2.2e-16
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##       model_id model_method                            feats
## 1 Max.cor.Y.lm           lm expectBachelors, fatherBachelors
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                       0.84                 0.004
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.1634115     81.76012    0.1551885     81.59718        0.1627175
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.1618022       1.348194        0.006007268
```

```r
if (!is.null(glb_date_vars) && 
    (sum(grepl(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
               names(glb_allobs_df))) > 0)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
            grep(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
                        names(glb_allobs_df), value=TRUE)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.lm"
## [1] "    indep_vars: expectBachelors, fatherBachelors, expectBachelors:fatherBornUS"
## Aggregating results
## Fitting final model on full training set
```

![](PISA_template2_files/figure-html/fit.models_0-13.png) ![](PISA_template2_files/figure-html/fit.models_0-14.png) ![](PISA_template2_files/figure-html/fit.models_0-15.png) ![](PISA_template2_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -266.353  -54.189    2.471   56.501  256.914 
## 
## Coefficients:
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     443.726      4.108 108.005  < 2e-16 ***
## expectBachelors                  62.737      5.672  11.061  < 2e-16 ***
## fatherBachelors                  39.550      3.577  11.056  < 2e-16 ***
## `expectBachelors:fatherBornUS`   12.441      4.426   2.811  0.00499 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 81.62 on 2410 degrees of freedom
## Multiple R-squared:  0.1661,	Adjusted R-squared:  0.1651 
## F-statistic: 160.1 on 3 and 2410 DF,  p-value: < 2.2e-16
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##                 model_id model_method
## 1 Interact.High.cor.Y.lm           lm
##                                                            feats
## 1 expectBachelors, fatherBachelors, expectBachelors:fatherBornUS
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.056                 0.004
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.1661447     81.67066    0.1634496     81.19724        0.1651067
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.1635641       1.506315        0.002570215
```

```r
# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.lm"
## [1] "    indep_vars: expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](PISA_template2_files/figure-html/fit.models_0-17.png) ![](PISA_template2_files/figure-html/fit.models_0-18.png) ![](PISA_template2_files/figure-html/fit.models_0-19.png) ![](PISA_template2_files/figure-html/fit.models_0-20.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -247.638  -48.603    1.705   49.605  217.742 
## 
## Coefficients:
##                                                        Estimate Std. Error
## (Intercept)                                          141.939535  33.825387
## expectBachelors                                       55.324559   4.295293
## fatherBachelors                                       17.293499   3.988301
## grade                                                 29.617975   2.938390
## read30MinsADay                                        34.854649   3.409616
## motherBachelors                                       12.394256   3.858521
## computerForSchoolwork                                 22.736991   5.702167
## fatherHS                                               3.935410   5.583585
## motherHS                                               5.472251   6.077343
## englishAtHome                                          5.775566   6.642361
## fatherWork                                             6.037465   4.396661
## fatherBornUS                                           0.469190   5.557531
## preschool                                             -4.458012   3.490484
## minutesPerWeekEnglish                                  0.012516   0.010714
## motherWork                                            -2.785850   3.523637
## schoolHasLibrary                                      12.263412   9.268609
## schoolSize                                             0.006665   0.002196
## selfBornUS                                            -5.034699   7.273307
## studentsInEnglish                                     -0.285694   0.228023
## .rnorm                                                -0.536410   1.517952
## urban                                                 -0.135816   3.964467
## publicSchool                                         -17.073779   6.725862
## male                                                 -14.662299   3.159010
## `raceeth.fctrAmerican Indian/Alaska Native`          -67.123697  16.793828
## raceeth.fctrAsian                                     -1.018221   8.933450
## raceeth.fctrBlack                                    -66.687070   5.459324
## raceeth.fctrHispanic                                 -37.539511   5.062326
## `raceeth.fctrMore than one race`                     -16.001968   8.472434
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -1.949664  16.858844
##                                                      t value Pr(>|t|)    
## (Intercept)                                            4.196 2.81e-05 ***
## expectBachelors                                       12.880  < 2e-16 ***
## fatherBachelors                                        4.336 1.51e-05 ***
## grade                                                 10.080  < 2e-16 ***
## read30MinsADay                                        10.222  < 2e-16 ***
## motherBachelors                                        3.212  0.00133 ** 
## computerForSchoolwork                                  3.987 6.88e-05 ***
## fatherHS                                               0.705  0.48099    
## motherHS                                               0.900  0.36798    
## englishAtHome                                          0.870  0.38466    
## fatherWork                                             1.373  0.16982    
## fatherBornUS                                           0.084  0.93273    
## preschool                                             -1.277  0.20166    
## minutesPerWeekEnglish                                  1.168  0.24284    
## motherWork                                            -0.791  0.42925    
## schoolHasLibrary                                       1.323  0.18592    
## schoolSize                                             3.035  0.00244 ** 
## selfBornUS                                            -0.692  0.48887    
## studentsInEnglish                                     -1.253  0.21036    
## .rnorm                                                -0.353  0.72384    
## urban                                                 -0.034  0.97267    
## publicSchool                                          -2.539  0.01120 *  
## male                                                  -4.641 3.65e-06 ***
## `raceeth.fctrAmerican Indian/Alaska Native`           -3.997 6.61e-05 ***
## raceeth.fctrAsian                                     -0.114  0.90926    
## raceeth.fctrBlack                                    -12.215  < 2e-16 ***
## raceeth.fctrHispanic                                  -7.415 1.67e-13 ***
## `raceeth.fctrMore than one race`                      -1.889  0.05905 .  
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -0.116  0.90794    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 73.84 on 2385 degrees of freedom
## Multiple R-squared:  0.3247,	Adjusted R-squared:  0.3167 
## F-statistic: 40.95 on 28 and 2385 DF,  p-value: < 2.2e-16
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##       model_id model_method
## 1 Low.cor.X.lm           lm
##                                                                                                                                                                                                                                                                                                                      feats
## 1 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.871                  0.02
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1     0.324674     74.25722    0.2610226     76.31516        0.3167456
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.3094934       1.358007         0.01737114
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 31.446 46.769  15.323
## 11 fit.models          7          1 46.770     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor    bgn end elapsed
## 1 fit.models_1_bgn          1          0 49.473  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 49.473 49.486   0.014
## 2  fit.models_1_lm          2          0 49.487     NA      NA
## [1] "fitting model: All.X.lm"
## [1] "    indep_vars: expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](PISA_template2_files/figure-html/fit.models_1-1.png) ![](PISA_template2_files/figure-html/fit.models_1-2.png) ![](PISA_template2_files/figure-html/fit.models_1-3.png) ![](PISA_template2_files/figure-html/fit.models_1-4.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -247.354  -48.784    1.998   49.494  216.666 
## 
## Coefficients:
##                                                        Estimate Std. Error
## (Intercept)                                          144.069645  33.857003
## expectBachelors                                       55.248729   4.294947
## fatherBachelors                                       16.943817   3.996149
## grade                                                 29.514109   2.938919
## read30MinsADay                                        34.876591   3.409084
## motherBachelors                                       12.637969   3.862153
## computerForSchoolwork                                 22.512116   5.703678
## fatherHS                                               3.956898   5.582673
## motherHS                                               6.068686   6.092579
## englishAtHome                                          8.093471   6.862462
## fatherWork                                             5.874774   4.397598
## fatherBornUS                                           4.353791   6.266248
## motherBornUS                                          -8.837430   6.589643
## preschool                                             -4.405668   3.490118
## minutesPerWeekEnglish                                  0.012780   0.010714
## motherWork                                            -2.837106   3.523254
## schoolHasLibrary                                      12.251454   9.267062
## schoolSize                                             0.006552   0.002198
## selfBornUS                                            -3.846790   7.325835
## studentsInEnglish                                     -0.289668   0.228004
## .rnorm                                                -0.568790   1.517890
## urban                                                 -0.085567   3.963981
## publicSchool                                         -16.835127   6.727090
## male                                                 -14.570801   3.159219
## `raceeth.fctrAmerican Indian/Alaska Native`          -67.203320  16.791122
## raceeth.fctrAsian                                     -4.094679   9.221827
## raceeth.fctrBlack                                    -66.976887   5.462687
## raceeth.fctrHispanic                                 -39.015744   5.179790
## `raceeth.fctrMore than one race`                     -16.907649   8.497892
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -5.015339  17.010319
##                                                      t value Pr(>|t|)    
## (Intercept)                                            4.255 2.17e-05 ***
## expectBachelors                                       12.864  < 2e-16 ***
## fatherBachelors                                        4.240 2.32e-05 ***
## grade                                                 10.043  < 2e-16 ***
## read30MinsADay                                        10.230  < 2e-16 ***
## motherBachelors                                        3.272  0.00108 ** 
## computerForSchoolwork                                  3.947 8.14e-05 ***
## fatherHS                                               0.709  0.47853    
## motherHS                                               0.996  0.31931    
## englishAtHome                                          1.179  0.23836    
## fatherWork                                             1.336  0.18171    
## fatherBornUS                                           0.695  0.48725    
## motherBornUS                                          -1.341  0.18001    
## preschool                                             -1.262  0.20695    
## minutesPerWeekEnglish                                  1.193  0.23304    
## motherWork                                            -0.805  0.42075    
## schoolHasLibrary                                       1.322  0.18628    
## schoolSize                                             2.981  0.00290 ** 
## selfBornUS                                            -0.525  0.59956    
## studentsInEnglish                                     -1.270  0.20405    
## .rnorm                                                -0.375  0.70790    
## urban                                                 -0.022  0.98278    
## publicSchool                                          -2.503  0.01240 *  
## male                                                  -4.612 4.20e-06 ***
## `raceeth.fctrAmerican Indian/Alaska Native`           -4.002 6.46e-05 ***
## raceeth.fctrAsian                                     -0.444  0.65707    
## raceeth.fctrBlack                                    -12.261  < 2e-16 ***
## raceeth.fctrHispanic                                  -7.532 7.03e-14 ***
## `raceeth.fctrMore than one race`                      -1.990  0.04675 *  
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -0.295  0.76814    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 73.82 on 2384 degrees of freedom
## Multiple R-squared:  0.3252,	Adjusted R-squared:  0.317 
## F-statistic: 39.61 on 29 and 2384 DF,  p-value: < 2.2e-16
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##   model_id model_method
## 1 All.X.lm           lm
##                                                                                                                                                                                                                                                                                                                                    feats
## 1 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.912                  0.02
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.3251831     74.29604    0.2611282     76.30971        0.3169743
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1         0.308882        1.43952         0.01890421
##              label step_major step_minor    bgn    end elapsed
## 2  fit.models_1_lm          2          0 49.487 52.209   2.722
## 3 fit.models_1_glm          3          0 52.209     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](PISA_template2_files/figure-html/fit.models_1-5.png) ![](PISA_template2_files/figure-html/fit.models_1-6.png) ![](PISA_template2_files/figure-html/fit.models_1-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -247.354   -48.784     1.998    49.494   216.666  
## 
## Coefficients:
##                                                        Estimate Std. Error
## (Intercept)                                          144.069645  33.857003
## expectBachelors                                       55.248729   4.294947
## fatherBachelors                                       16.943817   3.996149
## grade                                                 29.514109   2.938919
## read30MinsADay                                        34.876591   3.409084
## motherBachelors                                       12.637969   3.862153
## computerForSchoolwork                                 22.512116   5.703678
## fatherHS                                               3.956898   5.582673
## motherHS                                               6.068686   6.092579
## englishAtHome                                          8.093471   6.862462
## fatherWork                                             5.874774   4.397598
## fatherBornUS                                           4.353791   6.266248
## motherBornUS                                          -8.837430   6.589643
## preschool                                             -4.405668   3.490118
## minutesPerWeekEnglish                                  0.012780   0.010714
## motherWork                                            -2.837106   3.523254
## schoolHasLibrary                                      12.251454   9.267062
## schoolSize                                             0.006552   0.002198
## selfBornUS                                            -3.846790   7.325835
## studentsInEnglish                                     -0.289668   0.228004
## .rnorm                                                -0.568790   1.517890
## urban                                                 -0.085567   3.963981
## publicSchool                                         -16.835127   6.727090
## male                                                 -14.570801   3.159219
## `raceeth.fctrAmerican Indian/Alaska Native`          -67.203320  16.791122
## raceeth.fctrAsian                                     -4.094679   9.221827
## raceeth.fctrBlack                                    -66.976887   5.462687
## raceeth.fctrHispanic                                 -39.015744   5.179790
## `raceeth.fctrMore than one race`                     -16.907649   8.497892
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -5.015339  17.010319
##                                                      t value Pr(>|t|)    
## (Intercept)                                            4.255 2.17e-05 ***
## expectBachelors                                       12.864  < 2e-16 ***
## fatherBachelors                                        4.240 2.32e-05 ***
## grade                                                 10.043  < 2e-16 ***
## read30MinsADay                                        10.230  < 2e-16 ***
## motherBachelors                                        3.272  0.00108 ** 
## computerForSchoolwork                                  3.947 8.14e-05 ***
## fatherHS                                               0.709  0.47853    
## motherHS                                               0.996  0.31931    
## englishAtHome                                          1.179  0.23836    
## fatherWork                                             1.336  0.18171    
## fatherBornUS                                           0.695  0.48725    
## motherBornUS                                          -1.341  0.18001    
## preschool                                             -1.262  0.20695    
## minutesPerWeekEnglish                                  1.193  0.23304    
## motherWork                                            -0.805  0.42075    
## schoolHasLibrary                                       1.322  0.18628    
## schoolSize                                             2.981  0.00290 ** 
## selfBornUS                                            -0.525  0.59956    
## studentsInEnglish                                     -1.270  0.20405    
## .rnorm                                                -0.375  0.70790    
## urban                                                 -0.022  0.98278    
## publicSchool                                          -2.503  0.01240 *  
## male                                                  -4.612 4.20e-06 ***
## `raceeth.fctrAmerican Indian/Alaska Native`           -4.002 6.46e-05 ***
## raceeth.fctrAsian                                     -0.444  0.65707    
## raceeth.fctrBlack                                    -12.261  < 2e-16 ***
## raceeth.fctrHispanic                                  -7.532 7.03e-14 ***
## `raceeth.fctrMore than one race`                      -1.990  0.04675 *  
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -0.295  0.76814    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 5449.916)
## 
##     Null deviance: 19253519  on 2413  degrees of freedom
## Residual deviance: 12992600  on 2384  degrees of freedom
## AIC: 27651
## 
## Number of Fisher Scoring iterations: 2
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                    feats
## 1 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.261                   0.2
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB min.aic.fit
## 1    0.3251831     74.29604    0.2611282     76.30971    27650.95
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1         0.308882        1.43952         0.01890421
##                   label step_major step_minor    bgn    end elapsed
## 3      fit.models_1_glm          3          0 52.209 55.629    3.42
## 4 fit.models_1_bayesglm          4          0 55.629     NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr"
```

```
## Loading required package: arm
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: Matrix
## Loading required package: lme4
## Loading required package: Rcpp
## 
## Attaching package: 'lme4'
## 
## The following object is masked from 'package:nlme':
## 
##     lmList
## 
## 
## arm (Version 1.8-5, built: 2015-05-13)
## 
## Working directory is /Users/bbalaji-2012/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Assignments/HW2_NCES_PISA
```

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -247.342   -48.774     1.991    49.483   216.659  
## 
## Coefficients:
##                                                        Estimate Std. Error
## (Intercept)                                          144.071019  33.853764
## expectBachelors                                       55.241333   4.294521
## fatherBachelors                                       16.947477   3.995705
## grade                                                 29.512992   2.938783
## read30MinsADay                                        34.872068   3.408859
## motherBachelors                                       12.638954   3.861771
## computerForSchoolwork                                 22.514188   5.702652
## fatherHS                                               3.961390   5.581578
## motherHS                                               6.066237   6.091273
## englishAtHome                                          8.091704   6.860534
## fatherWork                                             5.876484   4.397120
## fatherBornUS                                           4.352692   6.264524
## motherBornUS                                          -8.826775   6.587394
## preschool                                             -4.404604   3.489891
## minutesPerWeekEnglish                                  0.012780   0.010714
## motherWork                                            -2.836185   3.523013
## schoolHasLibrary                                      12.241730   9.263045
## schoolSize                                             0.006552   0.002198
## selfBornUS                                            -3.846658   7.323687
## studentsInEnglish                                     -0.289535   0.228001
## .rnorm                                                -0.569010   1.517871
## urban                                                 -0.088734   3.963390
## publicSchool                                         -16.834483   6.725385
## male                                                 -14.571419   3.159024
## `raceeth.fctrAmerican Indian/Alaska Native`          -67.015025  16.767927
## raceeth.fctrAsian                                     -4.074785   9.217311
## raceeth.fctrBlack                                    -66.951397   5.461706
## raceeth.fctrHispanic                                 -38.996945   5.178430
## `raceeth.fctrMore than one race`                     -16.886560   8.494717
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -4.988177  16.985472
##                                                      t value Pr(>|t|)    
## (Intercept)                                            4.256 2.16e-05 ***
## expectBachelors                                       12.863  < 2e-16 ***
## fatherBachelors                                        4.241 2.31e-05 ***
## grade                                                 10.043  < 2e-16 ***
## read30MinsADay                                        10.230  < 2e-16 ***
## motherBachelors                                        3.273  0.00108 ** 
## computerForSchoolwork                                  3.948 8.11e-05 ***
## fatherHS                                               0.710  0.47794    
## motherHS                                               0.996  0.31940    
## englishAtHome                                          1.179  0.23833    
## fatherWork                                             1.336  0.18153    
## fatherBornUS                                           0.695  0.48724    
## motherBornUS                                          -1.340  0.18039    
## preschool                                             -1.262  0.20703    
## minutesPerWeekEnglish                                  1.193  0.23304    
## motherWork                                            -0.805  0.42087    
## schoolHasLibrary                                       1.322  0.18644    
## schoolSize                                             2.981  0.00290 ** 
## selfBornUS                                            -0.525  0.59947    
## studentsInEnglish                                     -1.270  0.20425    
## .rnorm                                                -0.375  0.70779    
## urban                                                 -0.022  0.98214    
## publicSchool                                          -2.503  0.01238 *  
## male                                                  -4.613 4.19e-06 ***
## `raceeth.fctrAmerican Indian/Alaska Native`           -3.997 6.62e-05 ***
## raceeth.fctrAsian                                     -0.442  0.65847    
## raceeth.fctrBlack                                    -12.258  < 2e-16 ***
## raceeth.fctrHispanic                                  -7.531 7.12e-14 ***
## `raceeth.fctrMore than one race`                      -1.988  0.04694 *  
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -0.294  0.76903    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 5449.916)
## 
##     Null deviance: 19253519  on 2413  degrees of freedom
## Residual deviance: 12992601  on 2384  degrees of freedom
## AIC: 27651
## 
## Number of Fisher Scoring iterations: 6
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                                                                                                                                                                                    feats
## 1 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.835                 0.293
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB min.aic.fit
## 1    0.3251831     74.29555    0.2611458     76.30881    27650.95
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.3088866       1.439702         0.01890536
##                   label step_major step_minor    bgn    end elapsed
## 4 fit.models_1_bayesglm          4          0 55.629 58.679    3.05
## 5    fit.models_1_rpart          5          0 58.680     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr"
```

```
## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info =
## trainInfo, : There were missing values in resampled performance measures.
```

![](PISA_template2_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.023 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](PISA_template2_files/figure-html/fit.models_1-9.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 2414 
## 
##           CP nsplit rel error
## 1 0.11854508      0 1.0000000
## 2 0.05055671      1 0.8814549
## 3 0.02298671      2 0.8308982
## 
## Variable importance
## expectBachelors fatherBachelors motherBachelors    publicSchool 
##              60              26              12               2 
## 
## Node number 1: 2414 observations,    complexity param=0.1185451
##   mean=517.9629, MSE=7975.774 
##   left son=2 (400 obs) right son=3 (2014 obs)
##   Primary splits:
##       expectBachelors < 0.5   to the left,  improve=0.11854510, (0 missing)
##       fatherBachelors < 0.5   to the left,  improve=0.07671692, (0 missing)
##       grade           < 9.5   to the left,  improve=0.06346444, (0 missing)
##       read30MinsADay  < 0.5   to the left,  improve=0.05680114, (0 missing)
##       motherBachelors < 0.5   to the left,  improve=0.05142078, (0 missing)
##   Surrogate splits:
##       schoolSize        < 124.5 to the left,  agree=0.835, adj=0.005, (0 split)
##       studentsInEnglish < 5.5   to the left,  agree=0.835, adj=0.003, (0 split)
## 
## Node number 2: 400 observations
##   mean=448.9663, MSE=6279.129 
## 
## Node number 3: 2014 observations,    complexity param=0.05055671
##   mean=531.6663, MSE=7179.472 
##   left son=6 (1226 obs) right son=7 (788 obs)
##   Primary splits:
##       fatherBachelors   < 0.5   to the left,  improve=0.06731889, (0 missing)
##       motherBachelors   < 0.5   to the left,  improve=0.05174876, (0 missing)
##       grade             < 9.5   to the left,  improve=0.05137033, (0 missing)
##       raceeth.fctrBlack < 0.5   to the right, improve=0.05095610, (0 missing)
##       read30MinsADay    < 0.5   to the left,  improve=0.04238281, (0 missing)
##   Surrogate splits:
##       motherBachelors   < 0.5   to the left,  agree=0.789, adj=0.461, (0 split)
##       publicSchool      < 0.5   to the right, agree=0.635, adj=0.067, (0 split)
##       grade             < 11.5  to the left,  agree=0.609, adj=0.001, (0 split)
##       studentsInEnglish < 69    to the left,  agree=0.609, adj=0.001, (0 split)
## 
## Node number 6: 1226 observations
##   mean=514.0412, MSE=6491.536 
## 
## Node number 7: 788 observations
##   mean=559.0881, MSE=7014.517 
## 
## n= 2414 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 2414 19253520 517.9629  
##   2) expectBachelors< 0.5 400  2511651 448.9663 *
##   3) expectBachelors>=0.5 2014 14459460 531.6663  
##     6) fatherBachelors< 0.5 1226  7958623 514.0412 *
##     7) fatherBachelors>=0.5 788  5527440 559.0881 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                                                                                                                                                                                            feats
## 1 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.511                 0.209
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.1691018     80.87644    0.1536399     81.67193        0.1808361
##   min.RMSESD.fit max.RsquaredSD.fit
## 1       1.633299        0.009641447
##                label step_major step_minor    bgn    end elapsed
## 5 fit.models_1_rpart          5          0 58.680 62.348   3.669
## 6    fit.models_1_rf          6          0 62.349     NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](PISA_template2_files/figure-html/fit.models_1-10.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 15 on full training set
```

![](PISA_template2_files/figure-html/fit.models_1-11.png) ![](PISA_template2_files/figure-html/fit.models_1-12.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted       2414   -none-     numeric  
## mse              500   -none-     numeric  
## rsq              500   -none-     numeric  
## oob.times       2414   -none-     numeric  
## importance        28   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            11   -none-     list     
## coefs              0   -none-     NULL     
## y               2414   -none-     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            28   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          1   -none-     logical  
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                            feats
## 1 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     43.785                15.557
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.8400191      75.0717    0.2599311     76.36408        0.2965709
##   min.RMSESD.fit max.RsquaredSD.fit
## 1       1.978098         0.02749742
```

```r
# User specified
#   Ensure at least 2 vars in each regression; else varImp crashes
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df

    # easier to exclude features
#model_id_pfx <- "";
# indep_vars_vctr <- setdiff(names(glb_fitobs_df), 
#                         union(union(glb_rsp_var, glb_exclude_vars_as_features), 
#                                 c("<feat1_name>", "<feat2_name>")))
# method <- ""                                

    # easier to include features
model_id <- "All.X.no.rnorm"; 
# indep_vars_vctr <- c(NULL
#    ,"<feat1>"
#    ,"<feat1>*<feat2>"
#    ,"<feat1>:<feat2>"
#                                            )
indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
indep_vars_vctr <- setdiff(indep_vars_vctr, ".rnorm")
for (method in c("lm")) {
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    csm_mdl_id <- paste0(model_id, ".", method)
    csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
}
```

```
## [1] "fitting model: All.X.no.rnorm.lm"
## [1] "    indep_vars: expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](PISA_template2_files/figure-html/fit.models_1-13.png) ![](PISA_template2_files/figure-html/fit.models_1-14.png) ![](PISA_template2_files/figure-html/fit.models_1-15.png) ![](PISA_template2_files/figure-html/fit.models_1-16.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -247.44  -48.86    1.86   49.77  217.18 
## 
## Coefficients:
##                                                        Estimate Std. Error
## (Intercept)                                          143.766333  33.841226
## expectBachelors                                       55.267080   4.293893
## fatherBachelors                                       16.929755   3.995253
## grade                                                 29.542707   2.937399
## read30MinsADay                                        34.871924   3.408447
## motherBachelors                                       12.638068   3.861457
## computerForSchoolwork                                 22.500232   5.702562
## fatherHS                                               4.018214   5.579269
## motherHS                                               6.058774   6.091423
## englishAtHome                                          8.035685   6.859492
## fatherWork                                             5.842798   4.395978
## fatherBornUS                                           4.306994   6.263875
## motherBornUS                                          -8.798153   6.587621
## preschool                                             -4.463670   3.486055
## minutesPerWeekEnglish                                  0.012788   0.010712
## motherWork                                            -2.809101   3.521827
## schoolHasLibrary                                      12.215085   9.264884
## schoolSize                                             0.006540   0.002197
## selfBornUS                                            -3.806278   7.323718
## studentsInEnglish                                     -0.286631   0.227819
## urban                                                 -0.110132   3.962724
## publicSchool                                         -16.857475   6.725614
## male                                                 -14.521653   3.155926
## `raceeth.fctrAmerican Indian/Alaska Native`          -67.277327  16.786935
## raceeth.fctrAsian                                     -4.110325   9.220071
## raceeth.fctrBlack                                    -67.012347   5.460883
## raceeth.fctrHispanic                                 -38.975486   5.177743
## `raceeth.fctrMore than one race`                     -16.922522   8.496268
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -5.101601  17.005696
##                                                      t value Pr(>|t|)    
## (Intercept)                                            4.248 2.24e-05 ***
## expectBachelors                                       12.871  < 2e-16 ***
## fatherBachelors                                        4.237 2.35e-05 ***
## grade                                                 10.057  < 2e-16 ***
## read30MinsADay                                        10.231  < 2e-16 ***
## motherBachelors                                        3.273  0.00108 ** 
## computerForSchoolwork                                  3.946 8.19e-05 ***
## fatherHS                                               0.720  0.47147    
## motherHS                                               0.995  0.32001    
## englishAtHome                                          1.171  0.24153    
## fatherWork                                             1.329  0.18393    
## fatherBornUS                                           0.688  0.49178    
## motherBornUS                                          -1.336  0.18182    
## preschool                                             -1.280  0.20052    
## minutesPerWeekEnglish                                  1.194  0.23264    
## motherWork                                            -0.798  0.42517    
## schoolHasLibrary                                       1.318  0.18749    
## schoolSize                                             2.977  0.00294 ** 
## selfBornUS                                            -0.520  0.60331    
## studentsInEnglish                                     -1.258  0.20846    
## urban                                                 -0.028  0.97783    
## publicSchool                                          -2.506  0.01226 *  
## male                                                  -4.601 4.42e-06 ***
## `raceeth.fctrAmerican Indian/Alaska Native`           -4.008 6.32e-05 ***
## raceeth.fctrAsian                                     -0.446  0.65578    
## raceeth.fctrBlack                                    -12.271  < 2e-16 ***
## raceeth.fctrHispanic                                  -7.528 7.29e-14 ***
## `raceeth.fctrMore than one race`                      -1.992  0.04651 *  
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -0.300  0.76421    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 73.81 on 2385 degrees of freedom
## Multiple R-squared:  0.3251,	Adjusted R-squared:  0.3172 
## F-statistic: 41.04 on 28 and 2385 DF,  p-value: < 2.2e-16
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##            model_id model_method
## 1 All.X.no.rnorm.lm           lm
##                                                                                                                                                                                                                                                                                                                            feats
## 1 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                       0.92                 0.022
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.3251434     74.18381    0.2614944     76.29079        0.3172205
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.3108972       1.311752         0.01678333
##                      importance
## expectBachelors       100.00000
## raceeth.fctrBlack      95.33026
## read30MinsADay         79.44408
## grade                  78.09246
## raceeth.fctrHispanic   58.39399
## male                   35.61080
```

```r
# Ntv.1.lm <- lm(reformulate(indep_vars_vctr, glb_rsp_var), glb_trnobs_df); print(summary(Ntv.1.lm))

#print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id model_method
## MFO.lm                                       MFO.lm           lm
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart        rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart        rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart        rpart
## Max.cor.Y.lm                           Max.cor.Y.lm           lm
## Interact.High.cor.Y.lm       Interact.High.cor.Y.lm           lm
## Low.cor.X.lm                           Low.cor.X.lm           lm
## All.X.lm                                   All.X.lm           lm
## All.X.glm                                 All.X.glm          glm
## All.X.bayesglm                       All.X.bayesglm     bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart        rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf           rf
## All.X.no.rnorm.lm                 All.X.no.rnorm.lm           lm
##                                                                                                                                                                                                                                                                                                                                                            feats
## MFO.lm                                                                                                                                                                                                                                                                                                                                                    .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                            expectBachelors, fatherBachelors
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                       expectBachelors, fatherBachelors
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                 expectBachelors, fatherBachelors
## Max.cor.Y.lm                                                                                                                                                                                                                                                                                                                    expectBachelors, fatherBachelors
## Interact.High.cor.Y.lm                                                                                                                                                                                                                                                                            expectBachelors, fatherBachelors, expectBachelors:fatherBornUS
## Low.cor.X.lm                            expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
## All.X.lm                  expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
## All.X.glm                 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
## All.X.bayesglm            expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
## All.X.no.rnorm.rpart              expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr
## All.X.no.rnorm.rf                 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr
## All.X.no.rnorm.lm                 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.lm                                  0                      0.472
## Max.cor.Y.cv.0.rpart                    0                      0.581
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.462
## Max.cor.Y.rpart                         3                      0.961
## Max.cor.Y.lm                            1                      0.840
## Interact.High.cor.Y.lm                  1                      1.056
## Low.cor.X.lm                            1                      0.871
## All.X.lm                                1                      0.912
## All.X.glm                               1                      1.261
## All.X.bayesglm                          1                      1.835
## All.X.no.rnorm.rpart                    3                      1.511
## All.X.no.rnorm.rf                       3                     43.785
## All.X.no.rnorm.lm                       1                      0.920
##                           min.elapsedtime.final max.R.sq.fit min.RMSE.fit
## MFO.lm                                    0.003 1.394431e-05     89.30657
## Max.cor.Y.cv.0.rpart                      0.038 0.000000e+00     89.30719
## Max.cor.Y.cv.0.cp.0.rpart                 0.036 1.692028e-01     81.40178
## Max.cor.Y.rpart                           0.039 1.691018e-01     81.58425
## Max.cor.Y.lm                              0.004 1.634115e-01     81.76012
## Interact.High.cor.Y.lm                    0.004 1.661447e-01     81.67066
## Low.cor.X.lm                              0.020 3.246740e-01     74.25722
## All.X.lm                                  0.020 3.251831e-01     74.29604
## All.X.glm                                 0.200 3.251831e-01     74.29604
## All.X.bayesglm                            0.293 3.251831e-01     74.29555
## All.X.no.rnorm.rpart                      0.209 1.691018e-01     80.87644
## All.X.no.rnorm.rf                        15.557 8.400191e-01     75.07170
## All.X.no.rnorm.lm                         0.022 3.251434e-01     74.18381
##                            max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## MFO.lm                    -0.0003464439     88.79131    -0.0004006436
## Max.cor.Y.cv.0.rpart       0.0000000000     88.77593               NA
## Max.cor.Y.cv.0.cp.0.rpart  0.1529902959     81.70327               NA
## Max.cor.Y.rpart            0.1536399403     81.67193               NA
## Max.cor.Y.lm               0.1551885090     81.59718     0.1627175370
## Interact.High.cor.Y.lm     0.1634496441     81.19724     0.1651066958
## Low.cor.X.lm               0.2610225537     76.31516     0.3167456414
## All.X.lm                   0.2611281994     76.30971     0.3169743402
## All.X.glm                  0.2611281994     76.30971               NA
## All.X.bayesglm             0.2611457687     76.30881               NA
## All.X.no.rnorm.rpart       0.1536399403     81.67193               NA
## All.X.no.rnorm.rf          0.2599311190     76.36408               NA
## All.X.no.rnorm.lm          0.2614943754     76.29079     0.3172205106
##                           max.Rsquared.fit min.RMSESD.fit
## MFO.lm                                  NA             NA
## Max.cor.Y.cv.0.rpart                    NA             NA
## Max.cor.Y.cv.0.cp.0.rpart               NA             NA
## Max.cor.Y.rpart                  0.1656590       1.066654
## Max.cor.Y.lm                     0.1618022       1.348194
## Interact.High.cor.Y.lm           0.1635641       1.506315
## Low.cor.X.lm                     0.3094934       1.358007
## All.X.lm                         0.3088820       1.439520
## All.X.glm                        0.3088820       1.439520
## All.X.bayesglm                   0.3088866       1.439702
## All.X.no.rnorm.rpart             0.1808361       1.633299
## All.X.no.rnorm.rf                0.2965709       1.978098
## All.X.no.rnorm.lm                0.3108972       1.311752
##                           max.RsquaredSD.fit min.aic.fit
## MFO.lm                                    NA          NA
## Max.cor.Y.cv.0.rpart                      NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA          NA
## Max.cor.Y.rpart                  0.009491393          NA
## Max.cor.Y.lm                     0.006007268          NA
## Interact.High.cor.Y.lm           0.002570215          NA
## Low.cor.X.lm                     0.017371143          NA
## All.X.lm                         0.018904206          NA
## All.X.glm                        0.018904206    27650.95
## All.X.bayesglm                   0.018905357    27650.95
## All.X.no.rnorm.rpart             0.009641447          NA
## All.X.no.rnorm.rf                0.027497421          NA
## All.X.no.rnorm.lm                0.016783326          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 6  fit.models_1_rf          6          0  62.349 111.507  49.159
## 7 fit.models_1_end          7          0 111.508      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1  46.770 111.514  64.744
## 12 fit.models          7          2 111.514      NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id",
                                    grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df),
                                    grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                                            model_id model_method
## MFO.lm                                       MFO.lm           lm
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart        rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart        rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart        rpart
## Max.cor.Y.lm                           Max.cor.Y.lm           lm
## Interact.High.cor.Y.lm       Interact.High.cor.Y.lm           lm
## Low.cor.X.lm                           Low.cor.X.lm           lm
## All.X.lm                                   All.X.lm           lm
## All.X.glm                                 All.X.glm          glm
## All.X.bayesglm                       All.X.bayesglm     bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart        rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf           rf
## All.X.no.rnorm.lm                 All.X.no.rnorm.lm           lm
##                                                                                                                                                                                                                                                                                                                                                            feats
## MFO.lm                                                                                                                                                                                                                                                                                                                                                    .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                                                                                                                                                                            expectBachelors, fatherBachelors
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                                                                                                                                                                                       expectBachelors, fatherBachelors
## Max.cor.Y.rpart                                                                                                                                                                                                                                                                                                                 expectBachelors, fatherBachelors
## Max.cor.Y.lm                                                                                                                                                                                                                                                                                                                    expectBachelors, fatherBachelors
## Interact.High.cor.Y.lm                                                                                                                                                                                                                                                                            expectBachelors, fatherBachelors, expectBachelors:fatherBornUS
## Low.cor.X.lm                            expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
## All.X.lm                  expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
## All.X.glm                 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
## All.X.bayesglm            expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, .rnorm, urban, publicSchool, male, raceeth.fctr
## All.X.no.rnorm.rpart              expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr
## All.X.no.rnorm.rf                 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr
## All.X.no.rnorm.lm                 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr
##                           max.nTuningRuns max.R.sq.fit  max.R.sq.OOB
## MFO.lm                                  0 1.394431e-05 -0.0003464439
## Max.cor.Y.cv.0.rpart                    0 0.000000e+00  0.0000000000
## Max.cor.Y.cv.0.cp.0.rpart               0 1.692028e-01  0.1529902959
## Max.cor.Y.rpart                         3 1.691018e-01  0.1536399403
## Max.cor.Y.lm                            1 1.634115e-01  0.1551885090
## Interact.High.cor.Y.lm                  1 1.661447e-01  0.1634496441
## Low.cor.X.lm                            1 3.246740e-01  0.2610225537
## All.X.lm                                1 3.251831e-01  0.2611281994
## All.X.glm                               1 3.251831e-01  0.2611281994
## All.X.bayesglm                          1 3.251831e-01  0.2611457687
## All.X.no.rnorm.rpart                    3 1.691018e-01  0.1536399403
## All.X.no.rnorm.rf                       3 8.400191e-01  0.2599311190
## All.X.no.rnorm.lm                       1 3.251434e-01  0.2614943754
##                           max.Adj.R.sq.fit max.Rsquared.fit
## MFO.lm                       -0.0004006436               NA
## Max.cor.Y.cv.0.rpart                    NA               NA
## Max.cor.Y.cv.0.cp.0.rpart               NA               NA
## Max.cor.Y.rpart                         NA        0.1656590
## Max.cor.Y.lm                  0.1627175370        0.1618022
## Interact.High.cor.Y.lm        0.1651066958        0.1635641
## Low.cor.X.lm                  0.3167456414        0.3094934
## All.X.lm                      0.3169743402        0.3088820
## All.X.glm                               NA        0.3088820
## All.X.bayesglm                          NA        0.3088866
## All.X.no.rnorm.rpart                    NA        0.1808361
## All.X.no.rnorm.rf                       NA        0.2965709
## All.X.no.rnorm.lm             0.3172205106        0.3108972
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.lm                                    2.11864407          333.33333333
## Max.cor.Y.cv.0.rpart                      1.72117040           26.31578947
## Max.cor.Y.cv.0.cp.0.rpart                 2.16450216           27.77777778
## Max.cor.Y.rpart                           1.04058273           25.64102564
## Max.cor.Y.lm                              1.19047619          250.00000000
## Interact.High.cor.Y.lm                    0.94696970          250.00000000
## Low.cor.X.lm                              1.14810563           50.00000000
## All.X.lm                                  1.09649123           50.00000000
## All.X.glm                                 0.79302141            5.00000000
## All.X.bayesglm                            0.54495913            3.41296928
## All.X.no.rnorm.rpart                      0.66181337            4.78468900
## All.X.no.rnorm.rf                         0.02283887            0.06427975
## All.X.no.rnorm.lm                         1.08695652           45.45454545
##                           inv.RMSE.fit inv.RMSE.OOB  inv.aic.fit
## MFO.lm                      0.01119738   0.01126236           NA
## Max.cor.Y.cv.0.rpart        0.01119731   0.01126431           NA
## Max.cor.Y.cv.0.cp.0.rpart   0.01228474   0.01223941           NA
## Max.cor.Y.rpart             0.01225727   0.01224411           NA
## Max.cor.Y.lm                0.01223090   0.01225533           NA
## Interact.High.cor.Y.lm      0.01224430   0.01231569           NA
## Low.cor.X.lm                0.01346670   0.01310356           NA
## All.X.lm                    0.01345967   0.01310449           NA
## All.X.glm                   0.01345967   0.01310449 3.616513e-05
## All.X.bayesglm              0.01345976   0.01310465 3.616513e-05
## All.X.no.rnorm.rpart        0.01236454   0.01224411           NA
## All.X.no.rnorm.rf           0.01332060   0.01309516           NA
## All.X.no.rnorm.lm           0.01348003   0.01310774           NA
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 7 rows containing missing values
## (geom_path).
```

```
## Warning in loop_apply(n, do.ply): Removed 78 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 21 rows containing missing values
## (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

![](PISA_template2_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## [1] "var:min.RMSESD.fit"
## [1] "var:max.RsquaredSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (position_stack).
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (position_stack).
```

![](PISA_template2_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                     model_id min.RMSE.OOB  max.R.sq.OOB max.Adj.R.sq.fit
## 13         All.X.no.rnorm.lm     76.29079  0.2614943754     0.3172205106
## 10            All.X.bayesglm     76.30881  0.2611457687               NA
## 8                   All.X.lm     76.30971  0.2611281994     0.3169743402
## 9                  All.X.glm     76.30971  0.2611281994               NA
## 7               Low.cor.X.lm     76.31516  0.2610225537     0.3167456414
## 12         All.X.no.rnorm.rf     76.36408  0.2599311190               NA
## 6     Interact.High.cor.Y.lm     81.19724  0.1634496441     0.1651066958
## 5               Max.cor.Y.lm     81.59718  0.1551885090     0.1627175370
## 4            Max.cor.Y.rpart     81.67193  0.1536399403               NA
## 11      All.X.no.rnorm.rpart     81.67193  0.1536399403               NA
## 3  Max.cor.Y.cv.0.cp.0.rpart     81.70327  0.1529902959               NA
## 2       Max.cor.Y.cv.0.rpart     88.77593  0.0000000000               NA
## 1                     MFO.lm     88.79131 -0.0003464439    -0.0004006436
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 5 rows containing missing values
## (geom_path).
```

```
## Warning in loop_apply(n, do.ply): Removed 29 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 7 rows containing missing values
## (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

![](PISA_template2_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~+min.RMSE.OOB - max.R.sq.OOB - max.Adj.R.sq.fit
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: All.X.no.rnorm.lm"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
    if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
        warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
        glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
    }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](PISA_template2_files/figure-html/fit.models_2-4.png) ![](PISA_template2_files/figure-html/fit.models_2-5.png) ![](PISA_template2_files/figure-html/fit.models_2-6.png) ![](PISA_template2_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -247.44  -48.86    1.86   49.77  217.18 
## 
## Coefficients:
##                                                        Estimate Std. Error
## (Intercept)                                          143.766333  33.841226
## expectBachelors                                       55.267080   4.293893
## fatherBachelors                                       16.929755   3.995253
## grade                                                 29.542707   2.937399
## read30MinsADay                                        34.871924   3.408447
## motherBachelors                                       12.638068   3.861457
## computerForSchoolwork                                 22.500232   5.702562
## fatherHS                                               4.018214   5.579269
## motherHS                                               6.058774   6.091423
## englishAtHome                                          8.035685   6.859492
## fatherWork                                             5.842798   4.395978
## fatherBornUS                                           4.306994   6.263875
## motherBornUS                                          -8.798153   6.587621
## preschool                                             -4.463670   3.486055
## minutesPerWeekEnglish                                  0.012788   0.010712
## motherWork                                            -2.809101   3.521827
## schoolHasLibrary                                      12.215085   9.264884
## schoolSize                                             0.006540   0.002197
## selfBornUS                                            -3.806278   7.323718
## studentsInEnglish                                     -0.286631   0.227819
## urban                                                 -0.110132   3.962724
## publicSchool                                         -16.857475   6.725614
## male                                                 -14.521653   3.155926
## `raceeth.fctrAmerican Indian/Alaska Native`          -67.277327  16.786935
## raceeth.fctrAsian                                     -4.110325   9.220071
## raceeth.fctrBlack                                    -67.012347   5.460883
## raceeth.fctrHispanic                                 -38.975486   5.177743
## `raceeth.fctrMore than one race`                     -16.922522   8.496268
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -5.101601  17.005696
##                                                      t value Pr(>|t|)    
## (Intercept)                                            4.248 2.24e-05 ***
## expectBachelors                                       12.871  < 2e-16 ***
## fatherBachelors                                        4.237 2.35e-05 ***
## grade                                                 10.057  < 2e-16 ***
## read30MinsADay                                        10.231  < 2e-16 ***
## motherBachelors                                        3.273  0.00108 ** 
## computerForSchoolwork                                  3.946 8.19e-05 ***
## fatherHS                                               0.720  0.47147    
## motherHS                                               0.995  0.32001    
## englishAtHome                                          1.171  0.24153    
## fatherWork                                             1.329  0.18393    
## fatherBornUS                                           0.688  0.49178    
## motherBornUS                                          -1.336  0.18182    
## preschool                                             -1.280  0.20052    
## minutesPerWeekEnglish                                  1.194  0.23264    
## motherWork                                            -0.798  0.42517    
## schoolHasLibrary                                       1.318  0.18749    
## schoolSize                                             2.977  0.00294 ** 
## selfBornUS                                            -0.520  0.60331    
## studentsInEnglish                                     -1.258  0.20846    
## urban                                                 -0.028  0.97783    
## publicSchool                                          -2.506  0.01226 *  
## male                                                  -4.601 4.42e-06 ***
## `raceeth.fctrAmerican Indian/Alaska Native`           -4.008 6.32e-05 ***
## raceeth.fctrAsian                                     -0.446  0.65578    
## raceeth.fctrBlack                                    -12.271  < 2e-16 ***
## raceeth.fctrHispanic                                  -7.528 7.29e-14 ***
## `raceeth.fctrMore than one race`                      -1.992  0.04651 *  
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -0.300  0.76421    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 73.81 on 2385 degrees of freedom
## Multiple R-squared:  0.3251,	Adjusted R-squared:  0.3172 
## F-statistic: 41.04 on 28 and 2385 DF,  p-value: < 2.2e-16
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
```

![](PISA_template2_files/figure-html/fit.models_2-8.png) 

```
##      grade male  raceeth preschool expectBachelors motherHS
## 4600    10    0    White         1               1        1
## 4848    10    0    Asian         1               1        1
## 5024    11    1 Hispanic         1               1        1
## 4069    10    0 Hispanic         0               1        1
## 4480    10    0    White         1               1        1
## 3720    10    1 Hispanic         1               1        0
##      motherBachelors motherWork fatherHS fatherBachelors fatherWork
## 4600               0          1        1               0          0
## 4848               0          1        1               1          1
## 5024               0          1        0               0          1
## 4069               0          1        1               1          1
## 4480               0          0        1               0          0
## 3720               0          1        0               0          1
##      selfBornUS motherBornUS fatherBornUS englishAtHome
## 4600          1            1            1             1
## 4848          1            0            0             0
## 5024          1            0            0             0
## 4069          1            1            1             1
## 4480          1            1            1             1
## 3720          1            0            0             1
##      computerForSchoolwork read30MinsADay minutesPerWeekEnglish
## 4600                     1              0                   180
## 4848                     1              1                   330
## 5024                     1              0                   350
## 4069                     1              0                   285
## 4480                     1              1                   300
## 3720                     1              0                   300
##      studentsInEnglish schoolHasLibrary publicSchool urban schoolSize
## 4600                14                1            1     0        887
## 4848                21                1            1     1       2149
## 5024                20                1            1     0       2183
## 4069                20                1            1     1       1378
## 4480                20                1            1     0       1250
## 3720                30                1            1     1       3592
##      readingScore .src .rownames     .rnorm raceeth.fctr
## 4600       772.46 Test      4600 -0.7060728        White
## 4848       335.22 Test      4848 -0.7190565        Asian
## 5024       735.91 Test      5024 -1.2390576     Hispanic
## 4069       740.44 Test      4069  1.6573888     Hispanic
## 4480       339.35 Test      4480 -1.6626492        White
## 3720       694.46 Test      3720 -0.7674848     Hispanic
##      readingScore.predict.All.X.no.rnorm.lm
## 4600                               518.9512
## 4848                               576.9965
## 5024                               502.2074
## 4069                               509.9361
## 4480                               558.8212
## 3720                               480.2412
##      readingScore.predict.All.X.no.rnorm.lm.err
## 4600                                   253.5088
## 4848                                   241.7765
## 5024                                   233.7026
## 4069                                   230.5039
## 4480                                   219.4712
## 3720                                   214.2188
```

```r
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

#stop(here"); #sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                                                      importance
## expectBachelors                                      100.000000
## raceeth.fctrBlack                                     95.330262
## read30MinsADay                                        79.444077
## grade                                                 78.092463
## raceeth.fctrHispanic                                  58.393993
## male                                                  35.610799
## fatherBachelors                                       32.777221
## `raceeth.fctrAmerican Indian/Alaska Native`           30.988366
## computerForSchoolwork                                 30.504971
## motherBachelors                                       25.266748
## schoolSize                                            22.961876
## publicSchool                                          19.299307
## `raceeth.fctrMore than one race`                      15.291772
## motherBornUS                                          10.182483
## fatherWork                                            10.132381
## schoolHasLibrary                                      10.049105
## preschool                                              9.753290
## studentsInEnglish                                      9.579786
## minutesPerWeekEnglish                                  9.079338
## englishAtHome                                          8.904859
## motherHS                                               7.528038
## motherWork                                             5.994054
## fatherHS                                               5.391235
## fatherBornUS                                           5.137316
## selfBornUS                                             3.830227
## raceeth.fctrAsian                                      3.254694
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`   2.119407
## urban                                                  0.000000
##                                                      All.X.no.rnorm.lm.importance
## expectBachelors                                                        100.000000
## raceeth.fctrBlack                                                       95.330262
## read30MinsADay                                                          79.444077
## grade                                                                   78.092463
## raceeth.fctrHispanic                                                    58.393993
## male                                                                    35.610799
## fatherBachelors                                                         32.777221
## `raceeth.fctrAmerican Indian/Alaska Native`                             30.988366
## computerForSchoolwork                                                   30.504971
## motherBachelors                                                         25.266748
## schoolSize                                                              22.961876
## publicSchool                                                            19.299307
## `raceeth.fctrMore than one race`                                        15.291772
## motherBornUS                                                            10.182483
## fatherWork                                                              10.132381
## schoolHasLibrary                                                        10.049105
## preschool                                                                9.753290
## studentsInEnglish                                                        9.579786
## minutesPerWeekEnglish                                                    9.079338
## englishAtHome                                                            8.904859
## motherHS                                                                 7.528038
## motherWork                                                               5.994054
## fatherHS                                                                 5.391235
## fatherBornUS                                                             5.137316
## selfBornUS                                                               3.830227
## raceeth.fctrAsian                                                        3.254694
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`                     2.119407
## urban                                                                    0.000000
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
    
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))

#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_var)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_var,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_OOBobs_df, mdl_id =
## glb_sel_mdl_id): Limiting important feature scatter plots to 5 out of 23
```

![](PISA_template2_files/figure-html/fit.models_2-9.png) ![](PISA_template2_files/figure-html/fit.models_2-10.png) ![](PISA_template2_files/figure-html/fit.models_2-11.png) ![](PISA_template2_files/figure-html/fit.models_2-12.png) ![](PISA_template2_files/figure-html/fit.models_2-13.png) 

```
##      grade male  raceeth preschool expectBachelors motherHS
## 4600    10    0    White         1               1        1
## 4848    10    0    Asian         1               1        1
## 5024    11    1 Hispanic         1               1        1
## 4069    10    0 Hispanic         0               1        1
## 4480    10    0    White         1               1        1
##      motherBachelors motherWork fatherHS fatherBachelors fatherWork
## 4600               0          1        1               0          0
## 4848               0          1        1               1          1
## 5024               0          1        0               0          1
## 4069               0          1        1               1          1
## 4480               0          0        1               0          0
##      selfBornUS motherBornUS fatherBornUS englishAtHome
## 4600          1            1            1             1
## 4848          1            0            0             0
## 5024          1            0            0             0
## 4069          1            1            1             1
## 4480          1            1            1             1
##      computerForSchoolwork read30MinsADay minutesPerWeekEnglish
## 4600                     1              0                   180
## 4848                     1              1                   330
## 5024                     1              0                   350
## 4069                     1              0                   285
## 4480                     1              1                   300
##      studentsInEnglish schoolHasLibrary publicSchool urban schoolSize
## 4600                14                1            1     0        887
## 4848                21                1            1     1       2149
## 5024                20                1            1     0       2183
## 4069                20                1            1     1       1378
## 4480                20                1            1     0       1250
##      readingScore .src .rownames     .rnorm raceeth.fctr
## 4600       772.46 Test      4600 -0.7060728        White
## 4848       335.22 Test      4848 -0.7190565        Asian
## 5024       735.91 Test      5024 -1.2390576     Hispanic
## 4069       740.44 Test      4069  1.6573888     Hispanic
## 4480       339.35 Test      4480 -1.6626492        White
##      readingScore.predict.All.X.no.rnorm.lm
## 4600                               518.9512
## 4848                               576.9965
## 5024                               502.2074
## 4069                               509.9361
## 4480                               558.8212
##      readingScore.predict.All.X.no.rnorm.lm.err
## 4600                                   253.5088
## 4848                                   241.7765
## 5024                                   233.7026
## 4069                                   230.5039
## 4480                                   219.4712
##      readingScore.predict.All.X.no.rnorm.lm.accurate .label
## 4600                                           FALSE   4600
## 4848                                           FALSE   4848
## 5024                                           FALSE   5024
## 4069                                           FALSE   4069
## 4480                                           FALSE   4480
```

![](PISA_template2_files/figure-html/fit.models_2-14.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
# FN_OOB_ids <- c(4721, 4020, 693, 92)
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_feats_df$id[1:5]])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
write.csv(glb_OOBobs_df[, c(glb_id_var, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

# write.csv(glb_chunks_df, paste0(glb_out_pfx, tail(glb_chunks_df, 1)$label, "_",
#                                 tail(glb_chunks_df, 1)$step_minor,  "_chunks1.csv"),
#           row.names=FALSE)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 12 fit.models          7          2 111.514 125.214    13.7
## 13 fit.models          7          3 125.214      NA      NA
```


```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "readingScore.predict.All.X.no.rnorm.lm"         
## [2] "readingScore.predict.All.X.no.rnorm.lm.err"     
## [3] "readingScore.predict.All.X.no.rnorm.lm.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](PISA_template2_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 125.214 130.812   5.598
## 14 fit.data.training          8          0 130.812      NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## [1] "fitting model: Final.lm"
## [1] "    indep_vars: expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr"
## Aggregating results
## Fitting final model on full training set
```

![](PISA_template2_files/figure-html/fit.data.training_0-1.png) ![](PISA_template2_files/figure-html/fit.data.training_0-2.png) ![](PISA_template2_files/figure-html/fit.data.training_0-3.png) ![](PISA_template2_files/figure-html/fit.data.training_0-4.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -247.44  -48.86    1.86   49.77  217.18 
## 
## Coefficients:
##                                                        Estimate Std. Error
## (Intercept)                                          143.766333  33.841226
## expectBachelors                                       55.267080   4.293893
## fatherBachelors                                       16.929755   3.995253
## grade                                                 29.542707   2.937399
## read30MinsADay                                        34.871924   3.408447
## motherBachelors                                       12.638068   3.861457
## computerForSchoolwork                                 22.500232   5.702562
## fatherHS                                               4.018214   5.579269
## motherHS                                               6.058774   6.091423
## englishAtHome                                          8.035685   6.859492
## fatherWork                                             5.842798   4.395978
## fatherBornUS                                           4.306994   6.263875
## motherBornUS                                          -8.798153   6.587621
## preschool                                             -4.463670   3.486055
## minutesPerWeekEnglish                                  0.012788   0.010712
## motherWork                                            -2.809101   3.521827
## schoolHasLibrary                                      12.215085   9.264884
## schoolSize                                             0.006540   0.002197
## selfBornUS                                            -3.806278   7.323718
## studentsInEnglish                                     -0.286631   0.227819
## urban                                                 -0.110132   3.962724
## publicSchool                                         -16.857475   6.725614
## male                                                 -14.521653   3.155926
## `raceeth.fctrAmerican Indian/Alaska Native`          -67.277327  16.786935
## raceeth.fctrAsian                                     -4.110325   9.220071
## raceeth.fctrBlack                                    -67.012347   5.460883
## raceeth.fctrHispanic                                 -38.975486   5.177743
## `raceeth.fctrMore than one race`                     -16.922522   8.496268
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -5.101601  17.005696
##                                                      t value Pr(>|t|)    
## (Intercept)                                            4.248 2.24e-05 ***
## expectBachelors                                       12.871  < 2e-16 ***
## fatherBachelors                                        4.237 2.35e-05 ***
## grade                                                 10.057  < 2e-16 ***
## read30MinsADay                                        10.231  < 2e-16 ***
## motherBachelors                                        3.273  0.00108 ** 
## computerForSchoolwork                                  3.946 8.19e-05 ***
## fatherHS                                               0.720  0.47147    
## motherHS                                               0.995  0.32001    
## englishAtHome                                          1.171  0.24153    
## fatherWork                                             1.329  0.18393    
## fatherBornUS                                           0.688  0.49178    
## motherBornUS                                          -1.336  0.18182    
## preschool                                             -1.280  0.20052    
## minutesPerWeekEnglish                                  1.194  0.23264    
## motherWork                                            -0.798  0.42517    
## schoolHasLibrary                                       1.318  0.18749    
## schoolSize                                             2.977  0.00294 ** 
## selfBornUS                                            -0.520  0.60331    
## studentsInEnglish                                     -1.258  0.20846    
## urban                                                 -0.028  0.97783    
## publicSchool                                          -2.506  0.01226 *  
## male                                                  -4.601 4.42e-06 ***
## `raceeth.fctrAmerican Indian/Alaska Native`           -4.008 6.32e-05 ***
## raceeth.fctrAsian                                     -0.446  0.65578    
## raceeth.fctrBlack                                    -12.271  < 2e-16 ***
## raceeth.fctrHispanic                                  -7.528 7.29e-14 ***
## `raceeth.fctrMore than one race`                      -1.992  0.04651 *  
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`  -0.300  0.76421    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 73.81 on 2385 degrees of freedom
## Multiple R-squared:  0.3251,	Adjusted R-squared:  0.3172 
## F-statistic: 41.04 on 28 and 2385 DF,  p-value: < 2.2e-16
## 
## [1] "    calling mypredict_mdl for fit:"
##   model_id model_method
## 1 Final.lm           lm
##                                                                                                                                                                                                                                                                                                                            feats
## 1 expectBachelors, fatherBachelors, grade, read30MinsADay, motherBachelors, computerForSchoolwork, fatherHS, motherHS, englishAtHome, fatherWork, fatherBornUS, motherBornUS, preschool, minutesPerWeekEnglish, motherWork, schoolHasLibrary, schoolSize, selfBornUS, studentsInEnglish, urban, publicSchool, male, raceeth.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.935                 0.022
##   max.R.sq.fit min.RMSE.fit max.Adj.R.sq.fit max.Rsquared.fit
## 1    0.3251434     74.18381        0.3172205        0.3108972
##   min.RMSESD.fit max.RsquaredSD.fit
## 1       1.311752         0.01678333
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 130.812 135.286   4.474
## 15 fit.data.training          8          1 135.286      NA      NA
```


```r
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

![](PISA_template2_files/figure-html/fit.data.training_1-1.png) 

```
##      grade male  raceeth preschool expectBachelors motherHS
## 1759    10    1    White         0               1        1
## 105     10    1    White         1               1        1
## 3099    10    0    White         0               1        1
## 3436    10    1    White         0               1        1
## 2460    10    0    White         0               1        0
## 2349    11    1 Hispanic         1               0        1
##      motherBachelors motherWork fatherHS fatherBachelors fatherWork
## 1759               0          0        1               0          1
## 105                0          1        1               1          0
## 3099               1          1        0               0          1
## 3436               0          1        1               1          1
## 2460               0          0        1               1          1
## 2349               1          1        1               1          1
##      selfBornUS motherBornUS fatherBornUS englishAtHome
## 1759          1            1            1             1
## 105           1            1            1             1
## 3099          1            1            1             1
## 3436          1            1            1             1
## 2460          1            1            0             1
## 2349          1            0            1             1
##      computerForSchoolwork read30MinsADay minutesPerWeekEnglish
## 1759                     1              0                   300
## 105                      1              1                   205
## 3099                     1              0                   235
## 3436                     1              0                   270
## 2460                     1              1                   275
## 2349                     1              0                   650
##      studentsInEnglish schoolHasLibrary publicSchool urban schoolSize
## 1759                30                1            1     1       3592
## 105                 30                1            1     0       1828
## 3099                21                1            1     0       1828
## 3436                20                1            1     0        261
## 2460                22                1            1     1       1276
## 2349                20                0            1     0        987
##      readingScore  .src .rownames    .rnorm raceeth.fctr
## 1759       284.64 Train      1759 0.1380485        White
## 105        316.63 Train       105 0.4211062        White
## 3099       311.15 Train      3099 1.4445030        White
## 3436       295.79 Train      3436 0.3055016        White
## 2460       356.75 Train      2460 1.1062739        White
## 2349       259.15 Train      2349 3.3972241     Hispanic
##      readingScore.predict.Final.lm readingScore.predict.Final.lm.err
## 1759                      532.0752                          247.4352
## 105                       558.1193                          241.4893
## 3099                      542.7290                          231.5790
## 3436                      527.0027                          231.2127
## 2460                      574.8586                          218.1086
## 2349                      476.6682                          217.5182
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##                                                      All.X.no.rnorm.lm.importance
## expectBachelors                                                        100.000000
## raceeth.fctrBlack                                                       95.330262
## read30MinsADay                                                          79.444077
## grade                                                                   78.092463
## raceeth.fctrHispanic                                                    58.393993
## male                                                                    35.610799
## fatherBachelors                                                         32.777221
## `raceeth.fctrAmerican Indian/Alaska Native`                             30.988366
## computerForSchoolwork                                                   30.504971
## motherBachelors                                                         25.266748
## schoolSize                                                              22.961876
## publicSchool                                                            19.299307
## `raceeth.fctrMore than one race`                                        15.291772
## motherBornUS                                                            10.182483
## fatherWork                                                              10.132381
## schoolHasLibrary                                                        10.049105
## preschool                                                                9.753290
## studentsInEnglish                                                        9.579786
## minutesPerWeekEnglish                                                    9.079338
## englishAtHome                                                            8.904859
## motherHS                                                                 7.528038
## motherWork                                                               5.994054
## fatherHS                                                                 5.391235
## fatherBornUS                                                             5.137316
## selfBornUS                                                               3.830227
## raceeth.fctrAsian                                                        3.254694
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`                     2.119407
## urban                                                                    0.000000
##                                                      importance
## expectBachelors                                      100.000000
## raceeth.fctrBlack                                     95.330262
## read30MinsADay                                        79.444077
## grade                                                 78.092463
## raceeth.fctrHispanic                                  58.393993
## male                                                  35.610799
## fatherBachelors                                       32.777221
## `raceeth.fctrAmerican Indian/Alaska Native`           30.988366
## computerForSchoolwork                                 30.504971
## motherBachelors                                       25.266748
## schoolSize                                            22.961876
## publicSchool                                          19.299307
## `raceeth.fctrMore than one race`                      15.291772
## motherBornUS                                          10.182483
## fatherWork                                            10.132381
## schoolHasLibrary                                      10.049105
## preschool                                              9.753290
## studentsInEnglish                                      9.579786
## minutesPerWeekEnglish                                  9.079338
## englishAtHome                                          8.904859
## motherHS                                               7.528038
## motherWork                                             5.994054
## fatherHS                                               5.391235
## fatherBornUS                                           5.137316
## selfBornUS                                             3.830227
## raceeth.fctrAsian                                      3.254694
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`   2.119407
## urban                                                  0.000000
##                                                      Final.lm.importance
## expectBachelors                                               100.000000
## raceeth.fctrBlack                                              95.330262
## read30MinsADay                                                 79.444077
## grade                                                          78.092463
## raceeth.fctrHispanic                                           58.393993
## male                                                           35.610799
## fatherBachelors                                                32.777221
## `raceeth.fctrAmerican Indian/Alaska Native`                    30.988366
## computerForSchoolwork                                          30.504971
## motherBachelors                                                25.266748
## schoolSize                                                     22.961876
## publicSchool                                                   19.299307
## `raceeth.fctrMore than one race`                               15.291772
## motherBornUS                                                   10.182483
## fatherWork                                                     10.132381
## schoolHasLibrary                                               10.049105
## preschool                                                       9.753290
## studentsInEnglish                                               9.579786
## minutesPerWeekEnglish                                           9.079338
## englishAtHome                                                   8.904859
## motherHS                                                        7.528038
## motherWork                                                      5.994054
## fatherHS                                                        5.391235
## fatherBornUS                                                    5.137316
## selfBornUS                                                      3.830227
## raceeth.fctrAsian                                               3.254694
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`            2.119407
## urban                                                           0.000000
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id): Limiting important feature scatter plots to 5 out of 23
```

![](PISA_template2_files/figure-html/fit.data.training_1-2.png) ![](PISA_template2_files/figure-html/fit.data.training_1-3.png) ![](PISA_template2_files/figure-html/fit.data.training_1-4.png) ![](PISA_template2_files/figure-html/fit.data.training_1-5.png) ![](PISA_template2_files/figure-html/fit.data.training_1-6.png) 

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
##      schoolHasLibrary publicSchool urban schoolSize readingScore  .src
## 1759                1            1     1       3592       284.64 Train
## 105                 1            1     0       1828       316.63 Train
## 3099                1            1     0       1828       311.15 Train
## 3436                1            1     0        261       295.79 Train
## 2460                1            1     1       1276       356.75 Train
##      .rownames    .rnorm raceeth.fctr readingScore.predict.Final.lm
## 1759      1759 0.1380485        White                      532.0752
## 105        105 0.4211062        White                      558.1193
## 3099      3099 1.4445030        White                      542.7290
## 3436      3436 0.3055016        White                      527.0027
## 2460      2460 1.1062739        White                      574.8586
##      readingScore.predict.Final.lm.err .label
## 1759                          247.4352   1759
## 105                           241.4893    105
## 3099                          231.5790   3099
## 3436                          231.2127   3436
## 2460                          218.1086   2460
```

![](PISA_template2_files/figure-html/fit.data.training_1-7.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

# print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])

print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "readingScore.predict.Final.lm"     "readingScore.predict.Final.lm.err"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](PISA_template2_files/figure-html/fit.data.training_1-8.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 135.286 143.124   7.838
## 16  predict.data.new          9          0 143.124      NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

![](PISA_template2_files/figure-html/predict.data.new-1.png) 

```
##      grade male  raceeth preschool expectBachelors motherHS
## 4600    10    0    White         1               1        1
## 4848    10    0    Asian         1               1        1
## 5024    11    1 Hispanic         1               1        1
## 4069    10    0 Hispanic         0               1        1
## 4480    10    0    White         1               1        1
## 3720    10    1 Hispanic         1               1        0
##      motherBachelors motherWork fatherHS fatherBachelors fatherWork
## 4600               0          1        1               0          0
## 4848               0          1        1               1          1
## 5024               0          1        0               0          1
## 4069               0          1        1               1          1
## 4480               0          0        1               0          0
## 3720               0          1        0               0          1
##      selfBornUS motherBornUS fatherBornUS englishAtHome
## 4600          1            1            1             1
## 4848          1            0            0             0
## 5024          1            0            0             0
## 4069          1            1            1             1
## 4480          1            1            1             1
## 3720          1            0            0             1
##      computerForSchoolwork read30MinsADay minutesPerWeekEnglish
## 4600                     1              0                   180
## 4848                     1              1                   330
## 5024                     1              0                   350
## 4069                     1              0                   285
## 4480                     1              1                   300
## 3720                     1              0                   300
##      studentsInEnglish schoolHasLibrary publicSchool urban schoolSize
## 4600                14                1            1     0        887
## 4848                21                1            1     1       2149
## 5024                20                1            1     0       2183
## 4069                20                1            1     1       1378
## 4480                20                1            1     0       1250
## 3720                30                1            1     1       3592
##      readingScore .src .rownames     .rnorm raceeth.fctr
## 4600       772.46 Test      4600 -0.7060728        White
## 4848       335.22 Test      4848 -0.7190565        Asian
## 5024       735.91 Test      5024 -1.2390576     Hispanic
## 4069       740.44 Test      4069  1.6573888     Hispanic
## 4480       339.35 Test      4480 -1.6626492        White
## 3720       694.46 Test      3720 -0.7674848     Hispanic
##      readingScore.predict.Final.lm readingScore.predict.Final.lm.err
## 4600                      518.9512                          253.5088
## 4848                      576.9965                          241.7765
## 5024                      502.2074                          233.7026
## 4069                      509.9361                          230.5039
## 4480                      558.8212                          219.4712
## 3720                      480.2412                          214.2188
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_newobs_df, mdl_id =
## glb_fin_mdl_id): Limiting important feature scatter plots to 5 out of 23
```

![](PISA_template2_files/figure-html/predict.data.new-2.png) ![](PISA_template2_files/figure-html/predict.data.new-3.png) ![](PISA_template2_files/figure-html/predict.data.new-4.png) ![](PISA_template2_files/figure-html/predict.data.new-5.png) ![](PISA_template2_files/figure-html/predict.data.new-6.png) 

```
##      grade male  raceeth preschool expectBachelors motherHS
## 4600    10    0    White         1               1        1
## 4848    10    0    Asian         1               1        1
## 5024    11    1 Hispanic         1               1        1
## 4069    10    0 Hispanic         0               1        1
## 4480    10    0    White         1               1        1
##      motherBachelors motherWork fatherHS fatherBachelors fatherWork
## 4600               0          1        1               0          0
## 4848               0          1        1               1          1
## 5024               0          1        0               0          1
## 4069               0          1        1               1          1
## 4480               0          0        1               0          0
##      selfBornUS motherBornUS fatherBornUS englishAtHome
## 4600          1            1            1             1
## 4848          1            0            0             0
## 5024          1            0            0             0
## 4069          1            1            1             1
## 4480          1            1            1             1
##      computerForSchoolwork read30MinsADay minutesPerWeekEnglish
## 4600                     1              0                   180
## 4848                     1              1                   330
## 5024                     1              0                   350
## 4069                     1              0                   285
## 4480                     1              1                   300
##      studentsInEnglish schoolHasLibrary publicSchool urban schoolSize
## 4600                14                1            1     0        887
## 4848                21                1            1     1       2149
## 5024                20                1            1     0       2183
## 4069                20                1            1     1       1378
## 4480                20                1            1     0       1250
##      readingScore .src .rownames     .rnorm raceeth.fctr
## 4600       772.46 Test      4600 -0.7060728        White
## 4848       335.22 Test      4848 -0.7190565        Asian
## 5024       735.91 Test      5024 -1.2390576     Hispanic
## 4069       740.44 Test      4069  1.6573888     Hispanic
## 4480       339.35 Test      4480 -1.6626492        White
##      readingScore.predict.Final.lm readingScore.predict.Final.lm.err
## 4600                      518.9512                          253.5088
## 4848                      576.9965                          241.7765
## 5024                      502.2074                          233.7026
## 4069                      509.9361                          230.5039
## 4480                      558.8212                          219.4712
##      .label
## 4600   4600
## 4848   4848
## 5024   5024
## 4069   4069
## 4480   4480
```

![](PISA_template2_files/figure-html/predict.data.new-7.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
#     submit_df <- glb_newobs_df[, c(paste0(glb_rsp_var_out, glb_fin_mdl_id)), FALSE]
#     names(submit_df)[1] <- "BDscience"
#     submit_df$BDscience <- as.numeric(submit_df$BDscience) - 1
#     #submit_df <-rbind(submit_df, data.frame(bdanalytics=c(" ")))
#     print("Submission Stats:")
#     print(table(submit_df$BDscience, useNA = "ifany"))
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
submit_fname <- paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
                    "_submit.csv")
write.csv(submit_df, submit_fname, quote=FALSE, row.names=FALSE)
#cat(" ", "\n", file=submit_fn, append=TRUE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: All.X.no.rnorm.lm"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.lm"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 2414   28
```

```r
print(dsp_models_df)
```

```
##                     model_id min.RMSE.OOB  max.R.sq.OOB max.Adj.R.sq.fit
## 13         All.X.no.rnorm.lm     76.29079  0.2614943754     0.3172205106
## 10            All.X.bayesglm     76.30881  0.2611457687               NA
## 8                   All.X.lm     76.30971  0.2611281994     0.3169743402
## 9                  All.X.glm     76.30971  0.2611281994               NA
## 7               Low.cor.X.lm     76.31516  0.2610225537     0.3167456414
## 12         All.X.no.rnorm.rf     76.36408  0.2599311190               NA
## 6     Interact.High.cor.Y.lm     81.19724  0.1634496441     0.1651066958
## 5               Max.cor.Y.lm     81.59718  0.1551885090     0.1627175370
## 4            Max.cor.Y.rpart     81.67193  0.1536399403               NA
## 11      All.X.no.rnorm.rpart     81.67193  0.1536399403               NA
## 3  Max.cor.Y.cv.0.cp.0.rpart     81.70327  0.1529902959               NA
## 2       Max.cor.Y.cv.0.rpart     88.77593  0.0000000000               NA
## 1                     MFO.lm     88.79131 -0.0003464439    -0.0004006436
```

```r
if (glb_is_regression) {
    print(sprintf("%s OOB RMSE: %0.4f", glb_sel_mdl_id,
                  glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "min.RMSE.OOB"]))

    if (!is.null(glb_category_vars)) {
        stop("not implemented yet")
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
            pred_stats_df <- 
                mypredict_mdl(mdl=glb_models_lst[[glb_fin_mdl_id]], 
                              df=glb_newobs_df, 
                              rsp_var=glb_rsp_var, 
                              rsp_var_out=glb_rsp_var_out, 
                              model_id_method=glb_fin_mdl_id, 
                              label="new",
						      model_summaryFunction=glb_sel_mdl$control$summaryFunction, 
						      model_metric=glb_sel_mdl$metric,
						      model_metric_maximize=glb_sel_mdl$maximize,
						      ret_type="stats")        
            print(sprintf("%s prediction stats for glb_newobs_df:", glb_fin_mdl_id))
            print(pred_stats_df)
    }    
}    
```

```
## [1] "All.X.no.rnorm.lm OOB RMSE: 76.2908"
## [1] "Final.lm prediction stats for glb_newobs_df:"
##   model_id max.R.sq.new min.RMSE.new
## 1 Final.lm    0.2614944     76.29079
```

```r
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    if (!is.null(glb_category_vars)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
        print(sprintf("%s new confusion matrix & accuracy: ", glb_fin_mdl_id))
        print(t(confusionMatrix(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)], 
                                glb_newobs_df[, glb_rsp_var])$table))
    }    

}    

dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_var]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

# if (glb_is_classification) {
#     print("FN_OOB_ids:")
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         glb_txt_vars])
#     print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
#                                 union(myfind_chr_cols_df(glb_OOBobs_df),
#                     grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
# }

dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##                                                      All.X.no.rnorm.lm.importance
## expectBachelors                                                        100.000000
## raceeth.fctrBlack                                                       95.330262
## read30MinsADay                                                          79.444077
## grade                                                                   78.092463
## raceeth.fctrHispanic                                                    58.393993
## male                                                                    35.610799
## fatherBachelors                                                         32.777221
## `raceeth.fctrAmerican Indian/Alaska Native`                             30.988366
## computerForSchoolwork                                                   30.504971
## motherBachelors                                                         25.266748
## schoolSize                                                              22.961876
## publicSchool                                                            19.299307
## `raceeth.fctrMore than one race`                                        15.291772
## motherBornUS                                                            10.182483
## fatherWork                                                              10.132381
## schoolHasLibrary                                                        10.049105
## preschool                                                                9.753290
## studentsInEnglish                                                        9.579786
## minutesPerWeekEnglish                                                    9.079338
## englishAtHome                                                            8.904859
## motherHS                                                                 7.528038
## motherWork                                                               5.994054
## fatherHS                                                                 5.391235
## fatherBornUS                                                             5.137316
## selfBornUS                                                               3.830227
## raceeth.fctrAsian                                                        3.254694
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`                     2.119407
## urban                                                                    0.000000
##                                                      importance
## expectBachelors                                      100.000000
## raceeth.fctrBlack                                     95.330262
## read30MinsADay                                        79.444077
## grade                                                 78.092463
## raceeth.fctrHispanic                                  58.393993
## male                                                  35.610799
## fatherBachelors                                       32.777221
## `raceeth.fctrAmerican Indian/Alaska Native`           30.988366
## computerForSchoolwork                                 30.504971
## motherBachelors                                       25.266748
## schoolSize                                            22.961876
## publicSchool                                          19.299307
## `raceeth.fctrMore than one race`                      15.291772
## motherBornUS                                          10.182483
## fatherWork                                            10.132381
## schoolHasLibrary                                      10.049105
## preschool                                              9.753290
## studentsInEnglish                                      9.579786
## minutesPerWeekEnglish                                  9.079338
## englishAtHome                                          8.904859
## motherHS                                               7.528038
## motherWork                                             5.994054
## fatherHS                                               5.391235
## fatherBornUS                                           5.137316
## selfBornUS                                             3.830227
## raceeth.fctrAsian                                      3.254694
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`   2.119407
## urban                                                  0.000000
##                                                      Final.lm.importance
## expectBachelors                                               100.000000
## raceeth.fctrBlack                                              95.330262
## read30MinsADay                                                 79.444077
## grade                                                          78.092463
## raceeth.fctrHispanic                                           58.393993
## male                                                           35.610799
## fatherBachelors                                                32.777221
## `raceeth.fctrAmerican Indian/Alaska Native`                    30.988366
## computerForSchoolwork                                          30.504971
## motherBachelors                                                25.266748
## schoolSize                                                     22.961876
## publicSchool                                                   19.299307
## `raceeth.fctrMore than one race`                               15.291772
## motherBornUS                                                   10.182483
## fatherWork                                                     10.132381
## schoolHasLibrary                                               10.049105
## preschool                                                       9.753290
## studentsInEnglish                                               9.579786
## minutesPerWeekEnglish                                           9.079338
## englishAtHome                                                   8.904859
## motherHS                                                        7.528038
## motherWork                                                      5.994054
## fatherHS                                                        5.391235
## fatherBornUS                                                    5.137316
## selfBornUS                                                      3.830227
## raceeth.fctrAsian                                               3.254694
## `raceeth.fctrNative Hawaiian/Other Pacific Islander`            2.119407
## urban                                                           0.000000
```

```r
# players_df <- data.frame(id=c("Chavez", "Giambi", "Menechino", "Myers", "Pena"),
#                          OBP=c(0.338, 0.391, 0.369, 0.313, 0.361),
#                          SLG=c(0.540, 0.450, 0.374, 0.447, 0.500),
#                         cost=c(1400000, 1065000, 295000, 800000, 300000))
# players_df$RS.predict <- predict(glb_models_lst[[csm_mdl_id]], players_df)
# print(orderBy(~ -RS.predict, players_df))

if (length(diff <- setdiff(names(glb_trnobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

if (length(diff <- setdiff(names(glb_fitobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
if (length(diff <- setdiff(names(glb_OOBobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
if (length(diff <- setdiff(names(glb_newobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)
```

```
## Warning in rm(submit_df, tmp_OOBobs_df): object 'tmp_OOBobs_df' not found
```

```r
# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor     bgn    end elapsed
## 16     predict.data.new          9          0 143.124 151.77   8.646
## 17 display.session.info         10          0 151.770     NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor     bgn     end elapsed
## 11              fit.models          7          1  46.770 111.514  64.744
## 10              fit.models          7          0  31.446  46.769  15.323
## 2             inspect.data          2          0   8.748  23.186  14.438
## 12              fit.models          7          2 111.514 125.214  13.700
## 16        predict.data.new          9          0 143.124 151.770   8.646
## 15       fit.data.training          8          1 135.286 143.124   7.838
## 3               scrub.data          2          1  23.187  28.877   5.690
## 13              fit.models          7          3 125.214 130.812   5.598
## 14       fit.data.training          8          0 130.812 135.286   4.474
## 6         extract.features          3          0  29.007  30.134   1.127
## 8          select.features          5          0  30.409  31.157   0.749
## 1              import.data          1          0   8.165   8.748   0.583
## 9  partition.data.training          6          0  31.158  31.446   0.288
## 7             cluster.data          4          0  30.135  30.408   0.274
## 5      manage.missing.data          2          3  28.910  29.007   0.097
## 4           transform.data          2          2  28.878  28.910   0.032
##    duration
## 11   64.744
## 10   15.323
## 2    14.438
## 12   13.700
## 16    8.646
## 15    7.838
## 3     5.690
## 13    5.598
## 14    4.474
## 6     1.127
## 8     0.748
## 1     0.583
## 9     0.288
## 7     0.273
## 5     0.097
## 4     0.032
## [1] "Total Elapsed Time: 151.77 secs"
```

![](PISA_template2_files/figure-html/display.session.info-1.png) 

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] grid      parallel  stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] gdata_2.16.1        randomForest_4.6-10 arm_1.8-5          
##  [4] lme4_1.1-7          Rcpp_0.11.6         Matrix_1.2-1       
##  [7] MASS_7.3-40         rpart.plot_1.5.2    rpart_4.1-9        
## [10] reshape2_1.4.1      mgcv_1.8-6          nlme_3.1-120       
## [13] dplyr_0.4.1         plyr_1.8.2          doMC_1.3.3         
## [16] iterators_1.0.7     foreach_1.4.2       doBy_4.5-13        
## [19] survival_2.38-1     caret_6.0-47        ggplot2_1.0.1      
## [22] lattice_0.20-31    
## 
## loaded via a namespace (and not attached):
##  [1] compiler_3.2.0      RColorBrewer_1.1-2  formatR_1.2        
##  [4] nloptr_1.0.4        tools_3.2.0         digest_0.6.8       
##  [7] evaluate_0.7        gtable_0.1.2        DBI_0.3.1          
## [10] yaml_2.1.13         brglm_0.5-9         SparseM_1.6        
## [13] proto_0.3-10        coda_0.17-1         BradleyTerry2_1.0-6
## [16] stringr_1.0.0       knitr_1.10.5        gtools_3.5.0       
## [19] nnet_7.3-9          rmarkdown_0.6.1     minqa_1.2.4        
## [22] car_2.0-25          magrittr_1.5        scales_0.2.4       
## [25] codetools_0.2-11    htmltools_0.2.6     splines_3.2.0      
## [28] abind_1.4-3         assertthat_0.1      pbkrtest_0.4-2     
## [31] colorspace_1.2-6    labeling_0.3        quantreg_5.11      
## [34] stringi_0.4-1       lazyeval_0.1.10     munsell_0.4.2
```

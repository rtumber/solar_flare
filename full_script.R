#Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
install.packages('devtools', repos = "http://cran.us.r-project.org")
devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.26/catboost-R-Windows-0.26.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(neuralnet)) install.packages("neuralnet", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(RSNNS)) install.packages("RSNNS", repos = "http://cran.us.r-project.org")
if(!require(earth)) install.packages("earth", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")

library(data.table)
library(DataExplorer)
library(gridExtra)
library(caTools)
library(kernlab)
library(ROSE)
library(Rborist)
library(catboost)
library(xgboost)
library(nnet)
library(randomForest)
library(neuralnet)
library(RSNNS)
library(caret)
library(earth)
library(MASS)
library(tidyverse)

#Download data description
names_file <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/solar-flare/flare.names", names_file)

length(readLines(names_file))
dataset_desc <- readLines(names_file)

rm(names_file)

#Examine and Tidy
print(dataset_desc)

#dataset_explanation
ds_explanation <- dataset_desc[12:18] %>% str_trim()
ds_explanation[1] <- paste(ds_explanation[1], ds_explanation[2], sep= " ")
ds_explanation <- ds_explanation[-2]
ds_explanation[2] <- paste(ds_explanation[2], ds_explanation[3], sep= " ")
ds_explanation <- ds_explanation[-3]
ds_explanation[3] <- paste(ds_explanation[3], ds_explanation[4], ds_explanation[5],  sep= " ")
ds_explanation <- ds_explanation[1:3]

ds_explanation <- str_replace(ds_explanation, "-- ", "")
print(ds_explanation)

#Dataset explanation directs us to use flare.data2 file.

#Attribute information
att_inf <- dataset_desc[25:48] %>% str_trim(side="left")
att_inf <- att_inf[att_inf != ""]
att_inf <- str_split(att_inf, "\\s\\s+", n = 2, simplify = TRUE)
att_inf[5,2] <- paste(att_inf[5,2], att_inf[6,1], sep = " ")
att_inf[7,2] <- paste(att_inf[7,2], att_inf[8,1], att_inf[9,1], sep = " ")
att_inf[11,1] <- paste(att_inf[11,1], att_inf[12,1], sep = " ")
att_inf[15,1] <- paste(att_inf[15,1], att_inf[16,1], sep = " ")
att_inf[17,1] <- paste(att_inf[17,1], att_inf[18,1], sep = " ")
att_inf[19,1] <- paste(att_inf[19,1], att_inf[20,1], sep = " ")
att_inf[21,1] <- paste(att_inf[21,1], att_inf[22,1], sep = " ")
att_inf <- att_inf[c(1:5, 7, 10:11, 13:15, 17, 19, 21), 1:2]
att_inf[,1] <- str_squish(att_inf[,1])
att_inf[,2] <- str_squish(att_inf[,2])
colnames(att_inf) <- c("Attribute", "Explanation")


#Download and import flare.data2 file
data_file <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/solar-flare/flare.data2", data_file)

length(readLines(data_file)) #number of records matches that described in description
flare_raw <- readLines(data_file)
rm(data_file)

#Arrange into something useful  
flare_data <- flare_raw
print(flare_data)
flare_data <- flare_data[-1]
flare_data <- str_split(flare_data, "\\s", simplify = TRUE)
colnames(flare_data) <- c(att_inf[1:10,1], att_inf[12:14,1]) 
#Some column names are a little wordy, abbreviate in att_inf and reassign.
att_inf <- cbind(att_inf, att_abb = c("Mod_Zur_Class_Code", "Lrgst_spot_size_code", "Spot_dist_code", "Activity", "Evolution", "Flare_activity_code", "Historically_complex", "Recent_historically_complex", "Area", "Lrgst_spot_area", "/" ,"C-class_flares_(common)", "M-class_flares_(moderate)", "X-class_flares_(severe)"))
colnames(flare_data) <- c(att_inf[1:10,3], att_inf[12:14,3]) 

#According to att_inf, columns 1-10 should be factors, and 11-13, numeric. Convert to data frame and adjust classes
flare_data <- as.data.frame(flare_data, stringsAsFactors = TRUE)
flare_data$`C-class_flares_(common)` <- as.numeric(levels(flare_data$`C-class_flares_(common)`)[flare_data$`C-class_flares_(common)`])
flare_data$`M-class_flares_(moderate)` <- as.numeric(levels(flare_data$`M-class_flares_(moderate)`)[flare_data$`M-class_flares_(moderate)`])
flare_data$`X-class_flares_(severe)` <- as.numeric(levels(flare_data$`X-class_flares_(severe)`)[flare_data$`X-class_flares_(severe)`])

#Examine data
introduce(flare_data)
str(flare_data)
plot_str(flare_data)
#No missing values
#The three numeric vectors are the target variables
#distribution of flare counts

c_class_dist <- flare_data %>% group_by(flare_data[11]) %>% summarize(n = n())
m_class_dist <- flare_data %>% group_by(flare_data[12]) %>% summarize(n = n())
x_class_dist <- flare_data %>% group_by(flare_data[13]) %>% summarize(n = n())


ggplot() +
  geom_bar(aes(c_class_dist$`C-class_flares_(common)`, c_class_dist$n), stat = "identity") +
  xlab("C-Class flare count") +
  ylab("n")

1 - c_class_dist$n[1]/sum(c_class_dist$n)
#Little over 17% of observations had subsequent activity

ggplot() +
  geom_bar(aes(m_class_dist$`M-class_flares_(moderate)`, m_class_dist$n), stat = "identity") +
  xlab("M-Class flare count") +
  ylab("n")

1 - m_class_dist$n[1]/sum(m_class_dist$n)
#Little over 3% of observations had subsequent activity

ggplot() +
  geom_bar(aes(x_class_dist$`X-class_flares_(severe)`, x_class_dist$n), stat = "identity") +
  xlab("X-Class flare count") +
  ylab("n")

1 - x_class_dist$n[1]/sum(x_class_dist$n)
#Less than 1% of observations had subsequent activity

#Examine the distribution of the ten data frame features in relation to three target variables

#determine max. flare count for Classes to standardise scales.
max(flare_data[11:13])
flare_data %>% filter(flare_data[11] > 0) %>% nrow()

feature_ind <- seq(1, 10, 1)
pred_ind <- seq(11, 13, 1)

class_plots <- lapply(feature_ind, function(f_ind){
  lapply(pred_ind, function(p_ind){
  flare_data %>%
    ggplot(aes(flare_data[,f_ind], flare_data[,p_ind])) +
    geom_jitter(width = 0.2, height = 0.1, alpha = 0.2, color = p_ind - 1) + #(-1 on p_ind in color for clarity until I add new palette)
    ggtitle(colnames(flare_data[p_ind])) +
    xlab(colnames(flare_data[f_ind])) + 
    ylab("no. flares") + 
    ylim(0, 8)
  })
})

#arrange by feature (1-10)
feature_grids <- lapply(feature_ind, function(f_ind){
  grid.arrange(grobs = class_plots[[f_ind]], ncol = 3)
})

#arrange by each flare class (1-3, c,m,x)
class_no <- seq(1,3,1) 
class_grids <- lapply(class_no, function(p_ind){
  grid.arrange(class_plots[[1]][[p_ind]], class_plots[[2]][[p_ind]], class_plots[[3]][[p_ind]], class_plots[[4]][[p_ind]], class_plots[[5]][[p_ind]], class_plots[[6]][[p_ind]], class_plots[[7]][[p_ind]], class_plots[[8]][[p_ind]], class_plots[[9]][[p_ind]], class_plots[[10]][[p_ind]], ncol = 5)
})

#The majority of the active regions on the Sun produced no solar flares of any class in the following 24 hours.
#Look at distribution where there is flare activity

#flare_data %>% group_by(`Mod_Zur_Class_Code`, `C-class_flares_(common)`) %>%
#  summarize(n = n()) %>%
#  ggplot(aes(`Mod_Zur_Class_Code`, n, fill = as_factor(`C-class_flares_(common)`))) +
#  geom_bar(position = position_dodge(width = 1, preserve = "single"), stat = "identity") +
#  guides(fill=guide_legend(title = "Flare Count")) +
#  xlab("Mod_Zur_Class_Code") +
#  ggtitle("C-class_flares_(common)")

#Replicate the above for all features and flare classes
dist_plots <- lapply(feature_ind, function(f_ind){
  lapply(pred_ind, function(p_ind){
  flare_data %>%
    mutate("feature_name" = flare_data[,f_ind], "flare_count" = flare_data[,p_ind]) %>%
    group_by(feature_name, flare_count) %>%
    summarize(n = n()) %>%
    ggplot(aes(feature_name, n, fill = as_factor(flare_count))) +
    geom_bar(position = position_dodge2(preserve = "single"), stat = "identity") +
    guides(fill=guide_legend(title = "Flare Count")) +
    xlab(colnames(flare_data[f_ind])) +
    ggtitle(colnames(flare_data[p_ind]))
  })
})

#arrange by feature (1-10)
feature_dist_grids <- lapply(feature_ind, function(f_ind){
  grid.arrange(grobs = dist_plots[[f_ind]], ncol = 3)
})  

#arrange by each flare class (1-3, c,m,x)
class_dist_grids <- lapply(class_no, function(p_ind){
  grid.arrange(dist_plots[[1]][[p_ind]], dist_plots[[2]][[p_ind]], dist_plots[[3]][[p_ind]], dist_plots[[4]][[p_ind]], dist_plots[[5]][[p_ind]], dist_plots[[6]][[p_ind]], dist_plots[[7]][[p_ind]], dist_plots[[8]][[p_ind]], dist_plots[[9]][[p_ind]], dist_plots[[10]][[p_ind]], ncol = 5)
})


#zero flares dominates observations, potentially obscuring relationships. Filter to remove and add y limits for comparisons
y_lim_calc <- lapply(feature_ind, function(f_ind){
  lapply(pred_ind, function(p_ind){
    flare_data %>%
    mutate("feature_name" = flare_data[,f_ind], "flare_count" = flare_data[,p_ind]) %>%
    filter(flare_count > 0) %>%
    group_by(feature_name, flare_count) %>%
    summarize(n = n()) %>% 
    summarize(max_flare_cnt_instance = max(n))
  })
}) %>% 
  unlist(recursive = FALSE) %>%
  map_df(~as.data.frame(.))
  max(y_lim_calc$max_flare_cnt_instance)

#maximum value 112, set y-lim to 120

dist_plots_filt <- lapply(feature_ind, function(f_ind){
  lapply(pred_ind, function(p_ind){
    flare_data %>%
      mutate("feature_name" = flare_data[,f_ind], "flare_count" = flare_data[,p_ind]) %>%
      filter(flare_count > 0) %>%
      group_by(feature_name, flare_count) %>%
      summarize(n = n()) %>%
      ggplot(aes(feature_name, n, fill = as_factor(flare_count))) +
      geom_bar(position = position_dodge2(preserve = "single"), stat = "identity") +
      guides(fill=guide_legend(title = "Flare Count")) +
      xlab(colnames(flare_data[f_ind])) +
      ggtitle(colnames(flare_data[p_ind])) +
      ylim(0, 120)
  })
})

#arrange by feature (1-10)
filt_feature_dist_grids <- lapply(feature_ind, function(f_ind){
  grid.arrange(grobs = dist_plots_filt[[f_ind]], ncol = 3)
})  

#arrange by each flare class (1-3, c,m,x)
filt_class_dist_grids <- lapply(class_no, function(p_ind){
  grid.arrange(dist_plots_filt[[1]][[p_ind]], dist_plots_filt[[2]][[p_ind]], dist_plots_filt[[3]][[p_ind]], dist_plots_filt[[4]][[p_ind]], dist_plots_filt[[5]][[p_ind]], dist_plots_filt[[6]][[p_ind]], dist_plots_filt[[7]][[p_ind]], dist_plots_filt[[8]][[p_ind]], dist_plots_filt[[9]][[p_ind]], dist_plots_filt[[10]][[p_ind]], ncol = 5)
})


#Flare activity is very low for m and x class flares so there is very little data that can be used to model. While
#C class is only ~17% there may be something that can be assembled but from here investigation is focused on model
#generation for c-class predictions
grid.arrange(class_dist_grids[[1]])
grid.arrange(filt_class_dist_grids[[1]])

#Where observations with zero flares in subsequent 24hours are removed reasonably clear relationships are observed. 
#To summarise

#1 Code for class (modified Zurich class)
#Class D is most likely
#2 Code for largest spot size
#S & A spots most likely
#3 Code for spot distribution
#I & O distribution most likely
#4 Activity
#Activity is typically reduced
#5 Evolution
#Growth or no-growth usually observed, decay is rare
#6 Previous 24 hour flare activity code
#Usually 1,  nothing as big as an M1
#7 Historically-complex
#Fairly even split, tending towards 2, not historically complex
#8 Did region become historically complex on this pass across the sun's disk
#Mostly not
#9 Area
#Area is mostly small
#10 Area of the largest spot
#Always <= 5

#Can these relationships be refined if we plot each variable against each other and flare count
feature_ind_1 <- seq(1, 10, 1)
feature_ind_2 <- seq(1, 10, 1)

variable_plots <- lapply(feature_ind_1, function(f_ind1){
  lapply(feature_ind_2, function(f_ind2){
    flare_data %>%
    mutate("feature_x" = flare_data[,f_ind1], "feature_y" = flare_data[,f_ind2]) %>%
    group_by(feature_x, feature_y) %>%
      summarise(n = n()) %>%
      ggplot(aes(feature_x, feature_y)) +
      geom_point(aes(size = n)) +
      scale_size(limits =c(0,1250)) +
      xlab(colnames(flare_data[f_ind1])) +
      ylab(colnames(flare_data[f_ind2]))
  })
})

#flare_data %>%
#  group_by(Flare_activity_code, x) %>%
#  summarise(n = n()) %>%
#  ggplot(aes(`Mod_Zur_Class_Code`, Lrgst_spot_size_code)) +
#  geom_point(aes(color = n, size = n))

#10x10 grid may make interpretation a little challenging, grid formed per variable
variable_grid <- lapply(feature_ind_1, function(f_ind1){
  grid.arrange(grobs = variable_plots[[f_ind1]], ncol = 5)
})
grid.arrange(variable_grid[[6]])
#Compare to flare_data filtered for events > 0
filt_variable_plots <- lapply(feature_ind_1, function(f_ind1){
  lapply(feature_ind_2, function(f_ind2){
    flare_data %>%
      mutate("feature_x" = flare_data[,f_ind1], "feature_y" = flare_data[,f_ind2]) %>%
      filter(`C-class_flares_(common)` > 0) %>%
      group_by(feature_x, feature_y) %>%
      summarise(n = n()) %>%
      ggplot(aes(feature_x, feature_y)) +
      geom_point(aes(size = n)) +
      scale_size(limits =c(0,1250)) +
      xlab(colnames(flare_data[f_ind1])) +
      ylab(colnames(flare_data[f_ind2]))
  })
})

filt_variable_grid <- lapply(feature_ind_1, function(f_ind1){
  grid.arrange(grobs = filt_variable_plots[[f_ind1]], ncol = 5)
})

#Far easier to compare filtered and unfiltered plots next to each other. Arrange
layout_matr <- rbind(c(1,2,3,4,5),
                     c(6,7,8,9,10))
comb_variable_grid <- lapply(feature_ind_1, function(f_ind1, f_ind2){
  marrangeGrob(c(variable_plots[[f_ind1]][1:5], filt_variable_plots[[f_ind1]][1:5],variable_plots[[f_ind1]][6:10], filt_variable_plots[[f_ind1]][6:10]), ncol = 5, nrow = 2, layout_matrix = layout_matr,  top="unfiltered", bottom="filtered")
})

#1 Code for class (modified Zurich class)
comb_variable_grid[[1]]
#2 Code for largest spot size
comb_variable_grid[[2]]
#3 Code for spot distribution
comb_variable_grid[[3]]
#4 Activity
comb_variable_grid[[4]]
#5 Evolution
comb_variable_grid[[5]]
#6 Previous 24 hour flare activity code
comb_variable_grid[[6]]
#7 Historically-complex
comb_variable_grid[[7]]
#8 Did region become historically complex on this pass across the sun's disk
comb_variable_grid[[8]]
#9 Area
comb_variable_grid[[9]]
#10 Area of the largest spot
comb_variable_grid[[10]]
#There appear to be very few differences in the distributions in the above plots and all of these could easily be
#explained by the lower sample size.
#Additionally, the same could be said of the distribution of the feature data with respect to c-class events. 

#To investigate the above we should return to the earlier observations, we should construct a model using the 
#following variables.
#Code for class (modified Zurich class)
#Code for largest spot size
#Code for spot distribution
#Activity
#Evolution
#Previous 24 hour flare activity code
#Did region become historically complex on this pass across the sun's disk
#Area

#Potentially Historically-complex variable, though the fairly even split indicate it is of low relevance 

#Need to return number of flares to categorical/discrete for predictions
flare_data$`C-class_flares_(common)` <- as.factor(flare_data$`C-class_flares_(common)`)


#Housekeeping
rm(y_lim_calc)
rm(class_no)
rm(feature_ind)
rm(feature_ind_1)
rm(feature_ind_2)
rm(pred_ind)

#straightforward split of data to training and testing set will be challenging due to imbalanced set.
#Need to create stratified split.
#createDataPartition in Caret packet can be utilised.
#70-30 Train-Test split seems reasonable considering the distribution of C-class flare counts.
#Further to this, looking at the distribution of target observations and the plots in the EDA it is highly likely 
#there is insufficient data to predict outcomes for values 4-8, and to leave them in will only negatively affect the 
#classification model.  
#These are filtered and the dataset split

flare_data <- rename(flare_data, C_class = `C-class_flares_(common)`)
flare_data_split <- filter(flare_data, !C_class %in% c("8", "6", "5", "4")) %>% droplevels()

set.seed(1, sample.kind="Rounding")
train_index <- createDataPartition(y = flare_data_split$C_class, times = 1, p = 0.7, list = FALSE)
flare_train <- flare_data_split[train_index,] %>% select(-Lrgst_spot_area, -`M-class_flares_(moderate)`, -`X-class_flares_(severe)`)
flare_test <- flare_data_split[-train_index,] %>% select(-Lrgst_spot_area, -`M-class_flares_(moderate)`, -`X-class_flares_(severe)`)

#Data will be up-sampled and down-sampled to attempt to identify the best method for addressing the imbalance in the
#data.10x5 repeated cross validation to be used with each sampling method.

#Resulting model used for prediction with the test set and the results compared.

#Evaluation metrics
#Evaluation metrics for imbalanced multiclass classification models can be challenging,
#accuracy and error rate unsuitable due to prevalence of zero c-class flares in subsequent 24 hours 
#balanced/weighted accuracy can assist with this.
#ROC or Precision-Recall AUC could be used if the output is binarized.
#F score can be used for imbalanced datasets, weighted F score can be better.

#Evaluation metric to be used is Macro-averaged F1 score,  This takes accounts for the imbalanced proportions of the
#different classes in the dataset. Value of one is defines perfect precision/recall.

#To begin, random forest model applied using ranger.
set.seed(1, sample.kind="Rounding")
base_ranger_fit_us <- train(C_class ~ ., method = "ranger", data = flare_train, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, sampling = "up", savePredictions = "final"))

base_ranger_fit_us_cm_matr <- vector("list", length(levels(flare_train$C_class)))
for (i in seq_along(base_ranger_fit_us_cm_matr)){
  positive.class <- levels(flare_train$C_class)[i]
  base_ranger_fit_us_cm_matr[[i]] <- confusionMatrix(base_ranger_fit_us[["pred"]][["pred"]], base_ranger_fit_us[["pred"]][["obs"]], positive = positive.class)
}

macro_f1_score <- function(base_ranger_fit_us_cm_matr){
  con_matr <- base_ranger_fit_us_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}

macro_f1_base_ranger_us <- macro_f1_score(base_ranger_fit_us_cm_matr)
macro_f1_base_ranger_us

set.seed(1, sample.kind="Rounding")
base_ranger_fit_ds <- train(C_class ~ ., method = "ranger", data = flare_train, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, sampling = "down", savePredictions = "final"))

base_ranger_fit_ds_cm_matr <- vector("list", length(levels(flare_train$C_class)))
for (i in seq_along(base_ranger_fit_ds_cm_matr)){
  positive.class <- levels(flare_train$C_class)[i]
  base_ranger_fit_ds_cm_matr[[i]] <- confusionMatrix(base_ranger_fit_ds[["pred"]][["pred"]], base_ranger_fit_ds[["pred"]][["obs"]], positive = positive.class)
}

macro_f1_score <- function(base_ranger_fit_ds_cm_matr){
  con_matr <- base_ranger_fit_ds_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}

macro_f1_base_ranger_ds <- macro_f1_score(base_ranger_fit_ds_cm_matr)
macro_f1_base_ranger_ds

#repeat with other model types,

#
#sampling_method <- c("down","up")
#control <- trainControl(method = "repeatedcv", number = 5, repeats = 10, sampling = "splmtd")
#ranger_fits <- lapply(sampling_method, function(splmtd){
#  print(splmtd)
#control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, sampling = splmtd)
# train(`C-class_flares_(common)` ~ ., method = "ranger", data = flare_train, trControl = control)
#})
#

#attempt again with other model types in caret with oversampling, eg. boosted, svm
#logitboost
#svmLinear

#LogitBoost
set.seed(1, sample.kind="Rounding")
base_boosted_fit_us <- train(C_class ~ ., method = "LogitBoost", data = flare_train, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, sampling = "up", savePredictions = "final"))

base_boosted_fit_us_cm_matr <- vector("list", length(levels(flare_train$C_class)))
for (i in seq_along(base_boosted_fit_us_cm_matr)){
  positive.class <- levels(flare_train$C_class)[i]
  base_boosted_fit_us_cm_matr[[i]] <- confusionMatrix(base_boosted_fit_us[["pred"]][["pred"]], base_boosted_fit_us[["pred"]][["obs"]], positive = positive.class)
}

macro_f1_score <- function(base_boosted_fit_us_cm_matr){
  con_matr <- base_boosted_fit_us_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}

macro_f1_base_boosted_us <- macro_f1_score(base_boosted_fit_us_cm_matr)
macro_f1_base_boosted_us

set.seed(1, sample.kind="Rounding")
base_boosted_fit_ds <- train(C_class ~ ., method = "LogitBoost", data = flare_train, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, sampling = "down", savePredictions = "final"))

base_boosted_fit_ds_cm_matr <- vector("list", length(levels(flare_train$C_class)))
for (i in seq_along(base_boosted_fit_ds_cm_matr)){
  positive.class <- levels(flare_train$C_class)[i]
  base_boosted_fit_ds_cm_matr[[i]] <- confusionMatrix(base_boosted_fit_ds[["pred"]][["pred"]], base_boosted_fit_ds[["pred"]][["obs"]], positive = positive.class)
}

macro_f1_score <- function(base_boosted_fit_ds_cm_matr){
  con_matr <- base_boosted_fit_ds_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}

macro_f1_base_boosted_ds <- macro_f1_score(base_boosted_fit_ds_cm_matr)
macro_f1_base_boosted_ds

#svmLinear
set.seed(1, sample.kind="Rounding")
base_svm_fit_us <- train(C_class ~ ., method = "svmLinear", data = flare_train, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, sampling = "up", savePredictions = "final"))

base_svm_fit_us_cm_matr <- vector("list", length(levels(flare_train$C_class)))
for (i in seq_along(base_svm_fit_us_cm_matr)){
  positive.class <- levels(flare_train$C_class)[i]
  base_svm_fit_us_cm_matr[[i]] <- confusionMatrix(base_svm_fit_us[["pred"]][["pred"]], base_svm_fit_us[["pred"]][["obs"]], positive = positive.class)
}

macro_f1_score <- function(base_svm_fit_us_cm_matr){
  con_matr <- base_svm_fit_us_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}

macro_f1_base_svm_us <- macro_f1_score(base_svm_fit_us_cm_matr)
macro_f1_base_svm_us


set.seed(1, sample.kind="Rounding")
base_svm_fit_ds <- train(C_class ~ ., method = "svmLinear", data = flare_train, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, sampling = "down", savePredictions = "final"))

base_svm_fit_ds_cm_matr <- vector("list", length(levels(flare_train$C_class)))
for (i in seq_along(base_svm_fit_ds_cm_matr)){
  positive.class <- levels(flare_train$C_class)[i]
  base_svm_fit_ds_cm_matr[[i]] <- confusionMatrix(base_svm_fit_ds[["pred"]][["pred"]], base_svm_fit_ds[["pred"]][["obs"]], positive = positive.class)
}

macro_f1_score <- function(base_svm_fit_ds_cm_matr){
  con_matr <- base_svm_fit_ds_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}

macro_f1_base_svm_ds <- macro_f1_score(base_svm_fit_ds_cm_matr)
macro_f1_base_svm_ds

#assemble results
base_methods_macroF1 <- as.data.frame(rbind(macro_f1_base_ranger_us, macro_f1_base_ranger_ds, macro_f1_base_boosted_us, macro_f1_base_boosted_ds, macro_f1_base_svm_us, macro_f1_base_svm_ds)) %>%
  mutate(model_name = c("ranger", "ranger", "LogitBoost", "LogitBoost", "svmLinear", " svmLinear"), sampling = c("up", "down", "up", "down", "up", "down")) %>%
  select(model_name, sampling, V1)
colnames(base_methods_macroF1) <- c("Model Name", "Sampling Method", "Macro F1 Score")
base_methods_macroF1

#Poor performance overall.

#Looking at the precision & recall for any model with any sampling method across the different classes and recalling
#the dominance of zero flare events in the EDA demonstrates the problem. Zero flare events appear to hinder 
#classification for 1+ flare events to the point where different models or sampling methods may also prove 
#ineffective.

#With this in mind better results may be forthcoming if the imbalance is addressed outside of caret, which provides
#additional options in terms of packages.
#While it could also facilitate a different approach to evaluation metrics since the dataset would be balanced, it is
#advantageous to continue using the current metric because it is highly likely the test set is imbalanced. 

#ROSE Package
#ROSE function generates a sample of synthetic data however only works for binary classification.
#This can be addressed by subsetting for each minority class, producing a dataset of majority and one minority class
#(binary), which can then be balanced using ROSE package, before joining the subsets together to produce a more 
#balanced dataset. It should be noted at this time the low instances of flare events 4+ could make synthesising data
#in this manner unreliable so they are still omitted.

flr_trn_C_0 <- flare_train %>% filter(C_class == 0) %>% droplevels()
flr_trn_C_1 <- flare_train %>% filter(C_class == 0 | C_class == 1) %>% droplevels()
flr_trn_C_2 <- flare_train %>% filter(C_class == 0 | C_class == 2) %>% droplevels()
flr_trn_C_3 <- flare_train %>% filter(C_class == 0 | C_class == 3) %>% droplevels()

class_1_data <- ROSE(C_class ~ ., data = flr_trn_C_1, p =1, seed = 1)$data
class_2_data <- ROSE(C_class ~ ., data = flr_trn_C_2, p =1, seed = 1)$data
class_3_data <- ROSE(C_class ~ ., data = flr_trn_C_3, p =1, seed = 1)$data

#examine minority class as formula, p closer to .75 or .5 (default), p = 1 may be too far, for recombination of just the three minor class data frames.

flare_train_balanced <- rbind(flr_trn_C_0, class_1_data, class_2_data, class_3_data)
#a quick look at c class flare distribution
c_class_dist_balanced <- flare_train_balanced %>% group_by(flare_train_balanced[10]) %>% summarize(n = n())
print(c_class_dist_balanced)

#dataset far more balanced.

#repeat modelling initial modelling using caret

set.seed(1, sample.kind="Rounding")
ranger_fit_balanced <- train(C_class ~ ., method = "ranger", data = flare_train_balanced, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"))
#base_ranger_fit_balanced_cm <- confusionMatrix(base_ranger_fit_balanced[["pred"]][["pred"]], base_ranger_fit_balanced[["pred"]][["obs"]], mode = "prec_recall")

#base_ranger_fit_balanced_cm_matr <- vector("list", length(levels(flare_train_balanced$C_class)))
#for (i in seq_along(base_ranger_fit_balanced_cm_matr)){
#  positive.class <- levels(flare_train_balanced$C_class)[i]
#  base_ranger_fit_balanced_cm_matr[[i]] <- confusionMatrix(base_ranger_fit_balanced[["pred"]][["pred"]], base_ranger_fit_balanced[["pred"]][["obs"]], positive = positive.class)
#}

#macro_f1_score <- function(base_ranger_fit_balanced_cm_matr){
#  con_matr <- base_ranger_fit_balanced_cm_matr[[1]]$byClass
#  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
#  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
#  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
#  return(mac_f1)
#}

#macro_f1_base_ranger_balanced <- macro_f1_score(base_ranger_fit_balanced_cm_matr)
#macro_f1_base_ranger_balanced

#(rewrite evaluation metric functions to single user friendly function)

model_fit_name <- ranger_fit_balanced
modeltype_cm_matr <- vector("list", length(levels(flare_train_balanced$C_class)))
macro_f1_score <- function(modeltype_cm_matr, model_fit_name){
  for (i in seq_along(modeltype_cm_matr)){
    positive.class <- levels(flare_train_balanced$C_class)[i]
    modeltype_cm_matr[[i]] <- confusionMatrix(model_fit_name[["pred"]][["pred"]], model_fit_name[["pred"]][["obs"]], positive = positive.class)
  }
  con_matr <- modeltype_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}
macro_f1_ranger_balanced <- macro_f1_score(modeltype_cm_matr, model_fit_name)
macro_f1_ranger_balanced
#This represents a vast improvement on the initial approach

#As before, repeat with LogitBoost and svmLinear
#LogitBoost
set.seed(1, sample.kind="Rounding")
LogitBoost_fit_balanced <- train(C_class ~ ., method = "LogitBoost", data = flare_train_balanced, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"))

model_fit_name <- LogitBoost_fit_balanced
modeltype_cm_matr <- vector("list", length(levels(flare_train_balanced$C_class)))
macro_f1_score <- function(modeltype_cm_matr, model_fit_name){
  for (i in seq_along(modeltype_cm_matr)){
    positive.class <- levels(flare_train_balanced$C_class)[i]
    modeltype_cm_matr[[i]] <- confusionMatrix(model_fit_name[["pred"]][["pred"]], model_fit_name[["pred"]][["obs"]], positive = positive.class)
  }
  con_matr <- modeltype_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}
macro_f1_LogitBoost_balanced <- macro_f1_score(modeltype_cm_matr, model_fit_name)
macro_f1_LogitBoost_balanced

#svmLinear
set.seed(1, sample.kind="Rounding")
svmLinear_fit_balanced <- train(C_class ~ ., method = "svmLinear", data = flare_train_balanced, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"))

model_fit_name <- svmLinear_fit_balanced
modeltype_cm_matr <- vector("list", length(levels(flare_train_balanced$C_class)))
macro_f1_score <- function(modeltype_cm_matr, model_fit_name){
  for (i in seq_along(modeltype_cm_matr)){
    positive.class <- levels(flare_train_balanced$C_class)[i]
    modeltype_cm_matr[[i]] <- confusionMatrix(model_fit_name[["pred"]][["pred"]], model_fit_name[["pred"]][["obs"]], positive = positive.class)
  }
  con_matr <- modeltype_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}
macro_f1_svmLinear_balanced <- macro_f1_score(modeltype_cm_matr, model_fit_name)
macro_f1_svmLinear_balanced


#Assemble results
balanced_data_methods_macroF1 <- as.data.frame(rbind(macro_f1_ranger_balanced, macro_f1_LogitBoost_balanced, macro_f1_svmLinear_balanced)) %>%
  mutate(model_name = c("ranger", "LogitBoost", "svmLinear")) %>%
  select(model_name, V1)
colnames(balanced_data_methods_macroF1) <- c("Model Name", "Macro F1 Score")
balanced_data_methods_macroF1

#Look at tuning the existing models before looking for alternatives.
#Before this, split the training set to training and validation sets to help prevent overtraining. 70/30 split.
set.seed(1, sample.kind="Rounding")
train_split_index <- createDataPartition(y = flare_train_balanced$C_class, times = 1, p = 0.7, list = FALSE)
flare_train_balanced_tuning <- flare_train_balanced[train_split_index,]
flare_train_balanced_validation <- flare_train_balanced[-train_split_index,]

#ranger fit
ranger_fit_balanced

ranger_tune_grid <- expand.grid(mtry = c(20, 21), splitrule = c("gini", "extratrees"), min.node.size = c(1,3,5,10))

set.seed(1, sample.kind="Rounding")
ranger_fit_balanced_tune <- train(C_class ~ ., method = "ranger", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = ranger_tune_grid)
ranger_fit_balanced_tune
#adjust grid
ranger_tune_grid <- expand.grid(mtry = c(18, 19, 20, 21), splitrule = c("gini", "extratrees"), min.node.size = c(1,3))
set.seed(1, sample.kind="Rounding")
ranger_fit_balanced_tune <- train(C_class ~ ., method = "ranger", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = ranger_tune_grid)
ranger_fit_balanced_tune
ranger_fit_balanced_tune$bestTune
ranger_fit_balanced_tune_pred <- predict(ranger_fit_balanced_tune, flare_train_balanced_validation)

model_pred_name <- ranger_fit_balanced_tune_pred
modelpred_cm_matr <- vector("list", length(levels(flare_train_balanced_validation$C_class)))
pred_macro_f1_score <- function(modelpred_cm_matr, model_pred_name){
  for (i in seq_along(modelpred_cm_matr)){
    positive.class <- levels(flare_train_balanced_validation$C_class)[i]
    modelpred_cm_matr[[i]] <- confusionMatrix(model_pred_name, flare_train_balanced_validation$C_class, positive = positive.class)
  }
  con_matr <- modelpred_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}
macro_f1_ranger_fit_balanced_tuned <- pred_macro_f1_score(modelpred_cm_matr, model_pred_name)
macro_f1_ranger_fit_balanced_tuned
#Small improvement in f1 score over untuned model.

#Repeat process for LogitBoost and svmLinear models

#LogitBoost
LogitBoost_fit_balanced

LogitBoost_tune_grid <- expand.grid(nIter = c(21, 31, 41))
set.seed(2, sample.kind="Rounding")
LogitBoost_fit_balanced_tune <- train(C_class ~ ., method = "LogitBoost", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = LogitBoost_tune_grid)
LogitBoost_fit_balanced_tune
#adjust grid
#LogitBoost_tune_grid <- expand.grid(nIter = c(25, 31, 35))
#set.seed(2, sample.kind="Rounding")
#LogitBoost_fit_balanced_tune <- train(C_class ~ ., method = "LogitBoost", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = LogitBoost_tune_grid)
#LogitBoost_fit_balanced_tune
#adjust grid
LogitBoost_tune_grid <- expand.grid(nIter = c(27, 28, 29, 30, 31, 32))
set.seed(2, sample.kind="Rounding")
LogitBoost_fit_balanced_tune <- train(C_class ~ ., method = "LogitBoost", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = LogitBoost_tune_grid)
LogitBoost_fit_balanced_tune
LogitBoost_fit_balanced_tune$bestTune

LogitBoost_fit_balanced_tune_pred <- predict(LogitBoost_fit_balanced_tune, flare_train_balanced_validation)

model_pred_name <- LogitBoost_fit_balanced_tune_pred
modelpred_cm_matr <- vector("list", length(levels(flare_train_balanced_validation$C_class)))
pred_macro_f1_score <- function(modelpred_cm_matr, model_pred_name){
  for (i in seq_along(modelpred_cm_matr)){
    positive.class <- levels(flare_train_balanced_validation$C_class)[i]
    modelpred_cm_matr[[i]] <- confusionMatrix(model_pred_name, flare_train_balanced_validation$C_class, positive = positive.class)
  }
  con_matr <- modelpred_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}
macro_f1_LogitBoost_fit_balanced_tuned <- pred_macro_f1_score(modelpred_cm_matr, model_pred_name)
macro_f1_LogitBoost_fit_balanced_tuned
#Much less accurate than before data split into training and validation but possibly more robust tuning to new data.

#svmLinear
svmLinear_fit_balanced

svmLinear_tune_grid <- expand.grid(C = c(1, 2, 3))
set.seed(1, sample.kind="Rounding")
svmLinear_fit_balanced_tune <- train(C_class ~ ., method = "svmLinear", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = svmLinear_tune_grid)
svmLinear_fit_balanced_tune
#Adjust grid
#svmLinear_tune_grid <- expand.grid(C = c(3, 5, 7, 9))
#set.seed(1, sample.kind="Rounding")
#svmLinear_fit_balanced_tune <- train(C_class ~ ., method = "svmLinear", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = svmLinear_tune_grid)
#svmLinear_fit_balanced_tune
#Adjust grid
svmLinear_tune_grid <- expand.grid(C = 7:15)
set.seed(1, sample.kind="Rounding")
svmLinear_fit_balanced_tune <- train(C_class ~ ., method = "svmLinear", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = svmLinear_tune_grid)
svmLinear_fit_balanced_tune
svmLinear_fit_balanced_tune$bestTune

svmLinear_fit_balanced_tune_pred <- predict(svmLinear_fit_balanced_tune, flare_train_balanced_validation)

model_pred_name <- svmLinear_fit_balanced_tune_pred
modelpred_cm_matr <- vector("list", length(levels(flare_train_balanced_validation$C_class)))
pred_macro_f1_score <- function(modelpred_cm_matr, model_pred_name){
  for (i in seq_along(modelpred_cm_matr)){
    positive.class <- levels(flare_train_balanced_validation$C_class)[i]
    modelpred_cm_matr[[i]] <- confusionMatrix(model_pred_name, flare_train_balanced_validation$C_class, positive = positive.class)
  }
  con_matr <- modelpred_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}
macro_f1_svmLinear_fit_balanced_tuned <- pred_macro_f1_score(modelpred_cm_matr, model_pred_name)
macro_f1_svmLinear_fit_balanced_tuned
#Improvement on untuned model.

#Assemble results
balanced_tuned_data_methods_macroF1 <- as.data.frame(rbind(macro_f1_ranger_fit_balanced_tuned, macro_f1_LogitBoost_fit_balanced_tuned, macro_f1_svmLinear_fit_balanced_tuned)) %>%
  mutate(model_name = c("ranger", "LogitBoost", "svmLinear")) %>%
  select(model_name, V1)
colnames(balanced_tuned_data_methods_macroF1) <- c("Model Name", "Macro F1 Score")
balanced_tuned_data_methods_macroF1

#Improvement in model performance over untuned models in all except LogitBoost.
#Would be useful to model other methods before writing off a whole method type

#Rborist
set.seed(1, sample.kind="Rounding")
Rborist_fit_balanced <- train(C_class ~ ., method = "Rborist", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"))
Rborist_fit_balanced

Rborist_tune_grid <- expand.grid(predFixed = 15:21, minNode = 1:5)
set.seed(1, sample.kind="Rounding")
Rborist_fit_balanced_tune <- train(C_class ~ ., method = "Rborist", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = Rborist_tune_grid)
Rborist_fit_balanced_tune
Rborist_fit_balanced_tune$bestTune

Rborist_fit_balanced_tune_pred <- predict(Rborist_fit_balanced_tune, flare_train_balanced_validation)

model_pred_name <- Rborist_fit_balanced_tune_pred
modelpred_cm_matr <- vector("list", length(levels(flare_train_balanced_validation$C_class)))
pred_macro_f1_score <- function(modelpred_cm_matr, model_pred_name){
  for (i in seq_along(modelpred_cm_matr)){
    positive.class <- levels(flare_train_balanced_validation$C_class)[i]
    modelpred_cm_matr[[i]] <- confusionMatrix(model_pred_name, flare_train_balanced_validation$C_class, positive = positive.class)
  }
  con_matr <- modelpred_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}
macro_f1_Rborist_fit_balanced_tuned <- pred_macro_f1_score(modelpred_cm_matr, model_pred_name)
macro_f1_Rborist_fit_balanced_tuned
#Improvement on tuned ranger


#Boosted model performed worst so far, try more models to determine if reflective of any boosting for classification
#on this dataset.

#catboost =^.^=
set.seed(1, sample.kind="Rounding")
catboost_fit_balanced <- train(y = flare_train_balanced_tuning$C_class, x = flare_train_balanced_tuning[,-10], method = catboost.caret, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"))
catboost_fit_balanced

#catboost_tune_grid <- expand.grid(depth = c(4, 6 ,8, 10), learning_rate = c(0.125, 0.13, 0.135, 0.14, 0.145), iterations = 100, l2_leaf_reg = c(0.000001, 0.001), rsm = 0.9, border_count = 255)
#set.seed(1, sample.kind="Rounding")
#catboost_fit_balanced_tune <- train(y = flare_train_balanced_tuning$C_class, x = flare_train_balanced_tuning[,-10], method = catboost.caret, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = catboost_tune_grid)
#catboost_fit_balanced_tune
#catboost_fit_balanced_tune$bestTune

catboost_tune_grid <- expand.grid(depth = 6, learning_rate = c(0.1353353, 0.136), iterations = 100, l2_leaf_reg = 0.001, rsm = 0.9, border_count = 255)
set.seed(1, sample.kind="Rounding")
catboost_fit_balanced_tune <- train(y = flare_train_balanced_tuning$C_class, x = flare_train_balanced_tuning[,-10], method = catboost.caret, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"), tuneGrid = catboost_tune_grid)
catboost_fit_balanced_tune
catboost_fit_balanced_tune$bestTune

catboost_fit_balanced_tune_pred <- predict(catboost_fit_balanced_tune, flare_train_balanced_validation)

model_pred_name <- catboost_fit_balanced_tune_pred
modelpred_cm_matr <- vector("list", length(levels(flare_train_balanced_validation$C_class)))
pred_macro_f1_score <- function(modelpred_cm_matr, model_pred_name){
  for (i in seq_along(modelpred_cm_matr)){
    positive.class <- levels(flare_train_balanced_validation$C_class)[i]
    modelpred_cm_matr[[i]] <- confusionMatrix(model_pred_name, flare_train_balanced_validation$C_class, positive = positive.class)
  }
  con_matr <- modelpred_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}
macro_f1_catboost_fit_balanced_tuned <- pred_macro_f1_score(modelpred_cm_matr, model_pred_name)
macro_f1_catboost_fit_balanced_tuned

#Significant improvement on Logitboost, comparable to tuned Rborist.

#xgbTree
#adjust trcontrol to allow parallel processing
set.seed(1, sample.kind="Rounding")
xgbTree_fit_balanced <- train(C_class ~ ., method = "xgbTree", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final", allowParallel = TRUE))
xgbTree_fit_balanced

#xgbTree_tune_grid <- expand.grid(eta = c(0.3, 0.4, 0.5), max_depth = 2:4, colsample_bytree = c(0.6, 0.7, 0.8, 0.9, 1), subsample = c(0.5, 0.75, 1), nrounds = c(100, 150, 200), gamma = 0, min_child_weight = 1)
#set.seed(1, sample.kind="Rounding")
#xgbTree_fit_balanced_tune <- train(C_class ~ ., method = "xgbTree", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final", allowParallel = TRUE), tuneGrid = xgbTree_tune_grid)
#xgbTree_fit_balanced_tune

xgbTree_tune_grid <- expand.grid(eta = c(0.3, 0.4, 0.5), max_depth = 3:9, colsample_bytree = 0.8, subsample = 1, nrounds = 150, gamma = 0, min_child_weight = 1)
set.seed(1, sample.kind="Rounding")
xgbTree_fit_balanced_tune <- train(C_class ~ ., method = "xgbTree", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final", allowParallel = TRUE), tuneGrid = xgbTree_tune_grid)
xgbTree_fit_balanced_tune
xgbTree_fit_balanced_tune$bestTune

xgbTree_fit_balanced_tune_pred <- predict(xgbTree_fit_balanced_tune, flare_train_balanced_validation)

model_pred_name <- xgbTree_fit_balanced_tune_pred
modelpred_cm_matr <- vector("list", length(levels(flare_train_balanced_validation$C_class)))
pred_macro_f1_score <- function(modelpred_cm_matr, model_pred_name){
  for (i in seq_along(modelpred_cm_matr)){
    positive.class <- levels(flare_train_balanced_validation$C_class)[i]
    modelpred_cm_matr[[i]] <- confusionMatrix(model_pred_name, flare_train_balanced_validation$C_class, positive = positive.class)
  }
  con_matr <- modelpred_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}
macro_f1_xgbTree_fit_balanced_tuned <- pred_macro_f1_score(modelpred_cm_matr, model_pred_name)
macro_f1_xgbTree_fit_balanced_tuned
#Outperforms all other models currently.

#svmRadial
set.seed(1, sample.kind="Rounding")
svmRadial_fit_balanced <- train(C_class ~ ., method = "svmRadial", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"))
svmRadial_fit_balanced

svmRad_sig <- svmRadial_fit_balanced$bestTune[["sigma"]]
#svmRadial_tune_grid <- expand.grid(sigma = svmRad_sig, C = c(0.5, 0.75, 1, 1.25, 1.5))
#set.seed(1, sample.kind="Rounding")
#svmRadial_fit_balanced_tune <- train(C_class ~ ., method = "svmRadial", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final", allowParallel = TRUE), tuneGrid = svmRadial_tune_grid)
#svmRadial_fit_balanced_tune

svmRadial_tune_grid <- expand.grid(sigma = svmRad_sig, C = c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0))
set.seed(1, sample.kind="Rounding")
svmRadial_fit_balanced_tune <- train(C_class ~ ., method = "svmRadial", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final", allowParallel = TRUE), tuneGrid = svmRadial_tune_grid)
svmRadial_fit_balanced_tune

#High cost increases reported accuracy but can lead to overtraining. Reported accuracy improvements get smaller with
#increased C. Limit to C = 1.2, define parameter and retrain.
svmRadial_tune_grid <- expand.grid(sigma = svmRad_sig, C = 1.2)
set.seed(1, sample.kind="Rounding")
svmRadial_fit_balanced_tune <- train(C_class ~ ., method = "svmRadial", data = flare_train_balanced_tuning, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final", allowParallel = TRUE), tuneGrid = svmRadial_tune_grid)
svmRadial_fit_balanced_tune

svmRadial_fit_balanced_tune_pred <- predict(svmRadial_fit_balanced_tune, flare_train_balanced_validation)

model_pred_name <- svmRadial_fit_balanced_tune_pred
modelpred_cm_matr <- vector("list", length(levels(flare_train_balanced_validation$C_class)))
pred_macro_f1_score <- function(modelpred_cm_matr, model_pred_name){
  for (i in seq_along(modelpred_cm_matr)){
    positive.class <- levels(flare_train_balanced_validation$C_class)[i]
    modelpred_cm_matr[[i]] <- confusionMatrix(model_pred_name, flare_train_balanced_validation$C_class, positive = positive.class)
  }
  con_matr <- modelpred_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}
macro_f1_svmRadial_fit_balanced_tuned <- pred_macro_f1_score(modelpred_cm_matr, model_pred_name)
macro_f1_svmRadial_fit_balanced_tuned
#more accurate than svmLinear

#Assemble results
balanced_tuned_data_methods_macroF1_upd <- rbind(balanced_tuned_data_methods_macroF1, macro_f1_Rborist_fit_balanced_tuned, macro_f1_catboost_fit_balanced_tuned, macro_f1_xgbTree_fit_balanced_tuned, macro_f1_svmRadial_fit_balanced_tuned) %>%
  select("Macro F1 Score")
rownames(balanced_tuned_data_methods_macroF1_upd) <- c("ranger", "LogitBoost", "svmLinear", "Rborist", "catboost", "xgbTree", "svmRadial")
balanced_tuned_data_methods_macroF1_upd <- balanced_tuned_data_methods_macroF1_upd %>% arrange(desc(`Macro F1 Score`))
balanced_tuned_data_methods_macroF1_upd

#Make predictions on test set for first five models and assess performance on unbalanced data.
model_names <- paste(rownames(balanced_tuned_data_methods_macroF1_upd)[1:5], "_fit_balanced_tune", sep = "")
model_predictions <- lapply(model_names, function(model){
  predict(get(model), flare_test)
  })

pred_test_macro_f1_score <- function(modelpred_cm_matr, model_pred_name){
  for (i in seq_along(modelpred_cm_matr)){
    positive.class <- levels(flare_test$C_class)[i]
    modelpred_cm_matr[[i]] <- confusionMatrix(model_pred_name, flare_test$C_class, positive = positive.class)
  }
  con_matr <- modelpred_cm_matr[[1]]$byClass
  recall <- sum(con_matr[,"Recall"]/nrow(con_matr))
  precision <- sum(con_matr[,"Precision"]/nrow(con_matr))
  mac_f1 <- 2 * ((recall*precision) / (recall + precision))
  return(mac_f1)
}

model_predictions_ind <- 1:5
macro_f1_test_scores <- lapply(model_predictions_ind, function(ind){
  model_pred_name <- model_predictions[[ind]]
  modelpred_cm_matr <- vector("list", length(levels(flare_test$C_class)))
  pred_test_macro_f1_score(modelpred_cm_matr, model_pred_name)
})

macro_f1_test_scores <- as.data.table(macro_f1_test_scores)
colnames(macro_f1_test_scores) <- rownames(balanced_tuned_data_methods_macroF1_upd)[1:5]
macro_f1_test_scores <- t(macro_f1_test_scores)
colnames(macro_f1_test_scores) <- "Macro F1 score"
macro_f1_test_scores <- macro_f1_test_scores %>% as.data.frame() %>% arrange(desc(`Macro F1 score`))
macro_f1_test_scores

#Overall very poor performance, comparable to that from initial imbalanced training set prior to use of ROSE package.
#This suggests one or more of the following.

#Insufficient data in the imbalanced training set to accurately synthesise new data points
#weak relationships between dataset features and the number of C class solar flares
#Overtrained models
#A categorisation exercise is not the correct approach


#Examination of synthesised data
#Data compared to original dataset, filtered to isolated synthesised factor.
class_1_flare_train <- flare_train %>% filter(C_class == 1)
class_1_data

train_feature_ind <- 1:9
class_1_comp_plots <- lapply(train_feature_ind, function(f_ind){
    class_1_flare_train$C_class <- as.numeric(levels(class_1_flare_train$C_class)[class_1_flare_train$C_class])
    flare_train_feat <- class_1_flare_train %>%
      mutate("feature_name" = class_1_flare_train[,f_ind], "flare_count" = 1) %>%
      group_by(feature_name, flare_count) %>%
      summarize(n = n())
    class_1_data$C_class <- as.numeric(levels(class_1_data$C_class)[class_1_data$C_class])
    class_1_data_feat <- class_1_data %>%
      mutate("feature_name" = class_1_data[,f_ind], "flare_count" = 1) %>%
      group_by(feature_name, flare_count) %>%
      summarize(n = n())
    ggplot() +
    geom_histogram(data = flare_train_feat, aes(feature_name, n), fill = "red", stat = "identity") +
    geom_histogram(data = class_1_data_feat, aes(feature_name, n), fill = "blue", alpha = 0.5, stat = "identity") +
    xlab(colnames(class_1_flare_train[f_ind]))
})

#arrange by feature (1-9)
class_1_comp_feature_grids <- lapply(train_feature_ind, function(f_ind){
  grid.arrange(grobs = class_1_comp_plots[f_ind], ncol = 3)
})
grid.arrange(class_1_comp_feature_grids[[1]],class_1_comp_feature_grids[[2]],class_1_comp_feature_grids[[3]],class_1_comp_feature_grids[[4]],class_1_comp_feature_grids[[5]],class_1_comp_feature_grids[[6]],class_1_comp_feature_grids[[7]],class_1_comp_feature_grids[[8]],class_1_comp_feature_grids[[9]])


#For 1 c-class flare event proportions have remained consistent between actual and synthesised data. There is no 
#reason to assume this would not be the case for other numbers of flare events and were we to look at the 
#distributions of the features of the synthesised data against each other in the same fashion as was performed in 
#the EDA it seems quite unlikely this would be the source of the inaccuracy.
#All this being the case, can we reasonably discount the idea that there was insufficient data from which to 
#artificially create new data points with which to balance the dataset, given the clear differences in accuracy 
#between training/validation and testing sets?

#This would be possible if there are only weak/missing relationships between the variables, something not really
#observed in EDA, or in the event of overtraining, something that would have become apparent in testing the tuned
#model performance.

#If indeed the above are all sound judgements, the remaining factor in the poor performance of the model is the
#approach taken in the first place.

#Before getting too involved, quick lm test on unbalanced training set to determine if reasonable to continue.

#dummy variables on full dataset before train/test split
#duplicate full data for new train/test to avoid conflict with categorisation

flare_data_reg <- flare_data
flare_data_reg <- rename(flare_data_reg, M_class = `M-class_flares_(moderate)`)
flare_data_reg <- rename(flare_data_reg, X_class = `X-class_flares_(severe)`)
flare_data_reg$C_class <- as.numeric(levels(flare_data_reg$C_class)[flare_data_reg$C_class])

str(flare_data_reg)

#multiple targets variables, need to ensure even split
flare_data_comb_var <- data.frame(c(flare_data_reg[11:13])) %>% mutate(comb_var = paste(C_class, M_class, X_class))

#split set
set.seed(1, sample.kind="Rounding")
train_index_reg <- createDataPartition(y = flare_data_comb_var$comb_var, times = 1, p = 0.7, list = FALSE)

flare_train_reg <- flare_data_reg[train_index_reg,]
flare_test_reg <- flare_data_reg[-train_index_reg,]


contrasts(flare_train_reg$Mod_Zur_Class_Code) = contr.treatment(6)
contrasts(flare_train_reg$Lrgst_spot_size_code) = contr.treatment(6)
contrasts(flare_train_reg$Spot_dist_code) = contr.treatment(4)
contrasts(flare_train_reg$Activity) = contr.treatment(2)
contrasts(flare_train_reg$Evolution) = contr.treatment(3)
contrasts(flare_train_reg$Flare_activity_code) = contr.treatment(3)
contrasts(flare_train_reg$Historically_complex) = contr.treatment(2)
contrasts(flare_train_reg$Recent_historically_complex) = contr.treatment(2)
contrasts(flare_train_reg$Area) = contr.treatment(2)
flare_train_reg$Lrgst_spot_area <- as.numeric(levels(flare_train_reg$Lrgst_spot_area)[flare_train_reg$Lrgst_spot_area])

contrasts(flare_test_reg$Mod_Zur_Class_Code) = contr.treatment(6)
contrasts(flare_test_reg$Lrgst_spot_size_code) = contr.treatment(6)
contrasts(flare_test_reg$Spot_dist_code) = contr.treatment(4)
contrasts(flare_test_reg$Activity) = contr.treatment(2)
contrasts(flare_test_reg$Evolution) = contr.treatment(3)
contrasts(flare_test_reg$Flare_activity_code) = contr.treatment(3)
contrasts(flare_test_reg$Historically_complex) = contr.treatment(2)
contrasts(flare_test_reg$Recent_historically_complex) = contr.treatment(2)
contrasts(flare_test_reg$Area) = contr.treatment(2)
flare_test_reg$Lrgst_spot_area <- as.numeric(levels(flare_test_reg$Lrgst_spot_area)[flare_test_reg$Lrgst_spot_area])

lm_fit_tst <- train(C_class ~ ., method = "lm", data = flare_train_reg, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"))
warnings()
summary(lm_fit_tst)

#For a very basic linear regression, C_class against everything, we are not expecting amazing results as it is 
#including M & X class flares as predictors but are instead looking for any indications from the summary that there
#is reason to continue a regression approach. Pr(>|t|) values indicate there may be a benefit in pursuing this.

#Recursive Feature Elimination
#Given the imbalance in the data and the very low proportion of non-zero flare events for M & X class types we will 
#omit these at this stage as before. Additionally, there is no variation in Largest Spot area value across both sets
#so this will also be omitted.
flare_train_reg_RFE <- flare_train_reg %>% select(-Lrgst_spot_area, -M_class, -X_class)
set.seed(1, sample.kind="Rounding")
rfe_control <- rfeControl(functions = rfFuncs, method = "repeatedcv", number = 10, repeats = 5)
reg_RFE_res <- rfe(flare_train_reg_RFE[,1:9], flare_train_reg_RFE$C_class, sizes = c(2:9), rfeControl = rfe_control)
predictors(reg_RFE_res)

#Using RMSE as an evaluation metric best performance was using below five predictors
reg_RFE_res[["optVariables"]]
#Though optimised, still very bad RMSE value. Is this down to the imbalance in the dataset and/or the model/method 
#used?
#Next steps should be to examine the performance of those four variables in C_class prediction in different models
#and if any show signs of success, tune models. Following this verify the RFE findings above using predictors 5 & 6 
#and compare performance.

#Following this, balance dataset as before and assess performance using top models for unbalanced data.

#RMSE will be used as an evaluation metric across all the regression models. Some models may overlap with those 
#already used for classification   
#Split flare_train_reg into train and validation sets.
set.seed(1, sample.kind="Rounding")
train_reg_split_index <- createDataPartition(y = flare_train_reg$C_class, times = 1, p = 0.7, list = FALSE)
flare_train_reg_trn <- flare_train_reg[train_reg_split_index,]
flare_train_reg_val <- flare_train_reg[-train_reg_split_index,]

#models
#ranger
#xgbtree
#pcaNNEt
#glm
model_types <- c("ranger", "xgbTree", "pcaNNet", "glm")
control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

set.seed(1, sample.kind="Rounding")
flare_train_reg_fits_2var <- lapply(model_types, function(model){
  print(model)
  train(C_class ~ Mod_Zur_Class_Code + Historically_complex, method = model, data = flare_train_reg_trn, trControl = control)
})

#RMSE gathering (minimum RMSE retained from models that record more than one since this is a comparative test and 
#does not represent the final model)
n_models <- seq(1:length(model_types))
model_rmses <- sapply(n_models, function(m_number){
  min(flare_train_reg_fits_2var[[m_number]][["results"]][["RMSE"]])
})

flare_train_reg_rmses_2var <- as.data.frame(model_rmses) %>%
  mutate(model_name = model_types)
colnames(flare_train_reg_rmses_2var) <- c("RMSE", "model_name")

flare_train_reg_rmses_2var

#Some improvement in RMSE for all models, add third variable
set.seed(1, sample.kind="Rounding")
flare_train_reg_fits_3var <- lapply(model_types, function(model){
  print(model)
  train(C_class ~ Mod_Zur_Class_Code + Historically_complex + Lrgst_spot_size_code, method = model, data = flare_train_reg_trn, trControl = control)
})

n_models <- seq(1:length(model_types))
model_rmses <- sapply(n_models, function(m_number){
  min(flare_train_reg_fits_3var[[m_number]][["results"]][["RMSE"]])
})

flare_train_reg_rmses_3var <- as.data.frame(model_rmses) %>%
  mutate(model_name = model_types)
colnames(flare_train_reg_rmses_3var) <- c("RMSE", "model_name")

flare_train_reg_rmses_3var

#Again improvement in RMSE. Add fourth variable
set.seed(1, sample.kind="Rounding")
flare_train_reg_fits_4var <- lapply(model_types, function(model){
  print(model)
  train(C_class ~ Mod_Zur_Class_Code + Historically_complex + Lrgst_spot_size_code + Area, method = model, data = flare_train_reg_trn, trControl = control)
})

n_models <- seq(1:length(model_types))
model_rmses <- sapply(n_models, function(m_number){
  min(flare_train_reg_fits_4var[[m_number]][["results"]][["RMSE"]])
})

flare_train_reg_rmses_4var <- as.data.frame(model_rmses) %>%
  mutate(model_name = model_types)
colnames(flare_train_reg_rmses_4var) <- c("RMSE", "model_name")

flare_train_reg_rmses_4var

#RMSE increases, replace fourth variable with fifth.
set.seed(1, sample.kind="Rounding")
flare_train_reg_fits_4var_rev <- lapply(model_types, function(model){
  print(model)
  train(C_class ~ Mod_Zur_Class_Code + Historically_complex + Lrgst_spot_size_code + Evolution, method = model, data = flare_train_reg_trn, trControl = control)
})

n_models <- seq(1:length(model_types))
model_rmses <- sapply(n_models, function(m_number){
  min(flare_train_reg_fits_4var_rev[[m_number]][["results"]][["RMSE"]])
})

flare_train_reg_rmses_4var_rev <- as.data.frame(model_rmses) %>%
  mutate(model_name = model_types)
colnames(flare_train_reg_rmses_4var_rev) <- c("RMSE", "model_name")

flare_train_reg_rmses_4var_rev

#Another increase in RMSE, continue for now with just the three variables, Mod_Zur_Class_Code, Historically_complex, Lrgst_spot_size_code
#Ranger & pcaNNet most successful.
#Tune models
#Ranger
flare_train_reg_fits_3var[[1]][["results"]]
ranger_reg_tune_grid <- expand.grid(mtry = c(1:5), splitrule = c("variance", "extratrees"), min.node.size = 5)
set.seed(1, sample.kind="Rounding")
ranger_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Historically_complex + Lrgst_spot_size_code, method = "ranger", data = flare_train_reg_trn, trControl = control, tuneGrid = ranger_reg_tune_grid)
ranger_fit_reg_tune
ranger_fit_reg_tune$bestTune
ranger_fit_reg_tune_pred <- predict(ranger_fit_reg_tune, flare_train_reg_val)
ranger_fit_reg_tune_RMSE <- RMSE(ranger_fit_reg_tune_pred, flare_train_reg_val$C_class)
ranger_fit_reg_tune_RMSE
#Again, poor performance, repeat for pcaNNEt
flare_train_reg_fits_3var[[3]][["results"]]

#pcaNNet
pcaNNet_reg_tune_grid <- expand.grid(size = c(1:3), decay = c(5e-3, 1e-4, 5e-4, 1e-5))
set.seed(1, sample.kind="Rounding")
pcaNNet_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Historically_complex + Lrgst_spot_size_code, method = "nnet", data = flare_train_reg_trn, maxit = 1000, trControl = control, tuneGrid = pcaNNet_reg_tune_grid)
pcaNNet_fit_reg_tune
pcaNNet_fit_reg_tune$bestTune
pcaNNet_fit_reg_tune_pred <- predict(pcaNNet_fit_reg_tune, flare_train_reg_val)
pcaNNet_fit_reg_tune_RMSE <- RMSE(pcaNNet_fit_reg_tune_pred, flare_train_reg_val$C_class)
pcaNNet_fit_reg_tune_RMSE
#Similarly poor performance.

#Are we missing any settings in caret. Refer documentation.
#Near Zero variance
near_zero_var <- nearZeroVar(flare_data_reg, saveMetrics = TRUE)
#As already identified, Largest Spot Area has no variance and has already been excluded from variables for 
#prediction. M & X class event prediction has also been excluded due to a shortage of data. Variables with near zero 
#variance have been retained as they may still have a use.
#Centering & Scaling
#center and scale will ignore any dummy variables


#Despite poor performance, still best performing so look at implementing other random forest/neural net models
#Rborist
#rf
#neuralnet
#mlpML

#Rborist
set.seed(1, sample.kind="Rounding")
Rborist_fit_reg <- train(C_class ~ Mod_Zur_Class_Code + Historically_complex + Lrgst_spot_size_code, method = "Rborist", data = flare_train_reg_trn, trControl = control)
Rborist_fit_reg
Rborist_fit_reg$bestTune
Rborist_fit_reg_pred <- predict(Rborist_fit_reg, flare_train_reg_val)
Rborist_fit_reg_RMSE <- RMSE(Rborist_fit_reg_pred, flare_train_reg_val$C_class)
Rborist_fit_reg_RMSE

#rf
set.seed(1, sample.kind="Rounding")
rf_fit_reg <- train(C_class ~ Mod_Zur_Class_Code + Historically_complex + Lrgst_spot_size_code, method = "rf", data = flare_train_reg_trn, trControl = control)
rf_fit_reg
rf_fit_reg$bestTune
rf_fit_reg_pred <- predict(rf_fit_reg, flare_train_reg_val)
rf_fit_reg_RMSE <- RMSE(rf_fit_reg_pred, flare_train_reg_val$C_class)
rf_fit_reg_RMSE

#neuralnet
set.seed(1, sample.kind="Rounding")
neuralnet_fit_reg <- train(C_class ~ Mod_Zur_Class_Code + Historically_complex + Lrgst_spot_size_code, method = "neuralnet", data = flare_train_reg_trn, trControl = control)
neuralnet_fit_reg
neuralnet_fit_reg$bestTune
neuralnet_fit_reg_pred <- predict(neuralnet_fit_reg, flare_train_reg_val)
neuralnet_fit_reg_RMSE <- RMSE(neuralnet_fit_reg_pred, flare_train_reg_val$C_class)
neuralnet_fit_reg_RMSE

#mlpML
set.seed(1, sample.kind="Rounding")
mlpML_fit_reg <- train(C_class ~ Mod_Zur_Class_Code + Historically_complex + Lrgst_spot_size_code, method = "mlpML", data = flare_train_reg_trn, trControl = control)
mlpML_fit_reg
mlpML_fit_reg$bestTune
mlpML_fit_reg_pred <- predict(mlpML_fit_reg, flare_train_reg_val)
mlpML_fit_reg_RMSE <- RMSE(mlpML_fit_reg_pred, flare_train_reg_val$C_class)
mlpML_fit_reg_RMSE


#Assemble fit results
reg_fit_RMSEs <- as.data.table(rbind(min(ranger_fit_reg_tune[["results"]][["RMSE"]]), min(pcaNNet_fit_reg_tune[["results"]][["RMSE"]]), min(Rborist_fit_reg[["results"]][["RMSE"]]), min(rf_fit_reg[["results"]][["RMSE"]]), min(neuralnet_fit_reg[["results"]][["RMSE"]]), min(mlpML_fit_reg[["results"]][["RMSE"]]))) %>% mutate(model_name = c("ranger tuned", "pcaNNet tuned", "Rborist", "rf", "neuralnet", "mlpML")) %>% select(model_name, V1)
colnames(reg_fit_RMSEs) <- c("model_name", "RMSE")
reg_fit_RMSEs <- rbind(flare_train_reg_rmses_3var, reg_fit_RMSEs) %>% select(model_name, RMSE) %>% arrange(RMSE)
reg_fit_RMSEs

#Looking at these results still suggests a neural net or random forest model could be the way forward.

#Checking once again the basic linear regression, we see the most significant features differ from those picked out
#in the recursive feature elimination.
set.seed(1, sample.kind="Rounding")
lm_fit_tst2 <- train(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Spot_dist_code + Activity + Evolution + Flare_activity_code + Historically_complex + Recent_historically_complex + Area + Lrgst_spot_area, method = "lm", data = flare_train_reg, trControl = trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = "final"))
summary(lm_fit_tst2)
#Mod_Zur_Class_code, Lrgst_spot_size_code, Activity, Area
#Does this indicate we should look once again at feature selection for fitting the data.
#The consistently poor results suggests this ought to be the next place to look

#1 Fit random forest model on the full set of variables and look at the variable importance 
set.seed(1, sample.kind="Rounding")
random_forest_vi_fit <- randomForest(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Spot_dist_code + Activity + Evolution + Flare_activity_code + Historically_complex + Recent_historically_complex + Area + Lrgst_spot_area, data = flare_train_reg, mtry = c(3:6), importance = TRUE, proximity = TRUE)
varImp(random_forest_vi_fit)
varImpPlot(random_forest_vi_fit)
#The variables, in decreasing order of significance, are identified as Historically_complex, Mod_Zur_Class_Code, 
#Lrgst_spot_size_code, Area, Evolution & Spot_dist_code. #The remaining variables are determined to be of no 
#significance.
#Once again this is quite some way removed from the recursive feature elimination. 

#2 Fit earth model and look at estimated variable importance.
set.seed(1, sample.kind="Rounding")
earth_vi_fit <- earth(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Spot_dist_code + Activity + Evolution + Flare_activity_code + Historically_complex + Recent_historically_complex + Area + Lrgst_spot_area, data = flare_train_reg)
evimp(earth_vi_fit)
#Variables determined to be important this time relate to the dummy variable for the variable in question.
#Mod_Zur_Class_Code, Activity, Lrgst_spot_size_code, Spot_dist_code, Area
#Here there is some commonality with the features from the random forest variable importance only loosely

#Step-wise Regression
set.seed(1, sample.kind="Rounding")
step_reg_basic <- lm(C_class ~ 1, data = flare_train_reg)
step_reg_full <- lm(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Spot_dist_code + Activity + Evolution + Flare_activity_code + Historically_complex + Recent_historically_complex + Area + Lrgst_spot_area, data = flare_train_reg)
step_reg_stepped <- stepAIC(step_reg_basic, scope = list(lower = step_reg_basic, upper = step_reg_full), direction = "both", trace = 0, steps = 1000)
step_variables <- as.data.table(step_reg_stepped[["coefficients"]], keep.rownames = TRUE)
colnames(step_variables) <- c("Coefficient", "Value")
step_variables <- step_variables[Coefficient != "(Intercept)"] %>% arrange(desc(Value))
step_variables
#Variables identified this time are Mod_Zur_Class_Code, Lrgst_spot_size_code, Activity & Area

#There are some variables common to all, however it would be unwise to ignore the differences.
#Model the variables identified above with the six better performing methods used previously, ranger, pcaNNEt, 
#Rborist, rf, neuralnet, mlpML, then tune.

#varimp variables
model_types <- c("ranger", "Rborist", "rf", "pcaNNet", "neuralnet", "mlpML")
set.seed(1, sample.kind="Rounding")
flare_train_reg_varimp_fits <- lapply(model_types, function(model){
  print(model)
  train(C_class ~ Historically_complex + Mod_Zur_Class_Code + Lrgst_spot_size_code + Area + Evolution + Spot_dist_code, method = model, data = flare_train_reg_trn, trControl = control)
})

n_models <- seq(1:length(model_types))
model_rmses <- sapply(n_models, function(m_number){
  min(flare_train_reg_varimp_fits[[m_number]][["results"]][["RMSE"]])
})

flare_train_reg_varimp_fit_rmses <- as.data.frame(model_rmses) %>%
  mutate(model_name = model_types)
colnames(flare_train_reg_varimp_fit_rmses) <- c("RMSE", "model_name")

flare_train_reg_varimp_fit_rmses

#earth variables
model_types <- c("ranger", "Rborist", "rf", "pcaNNet", "neuralnet", "mlpML")
set.seed(1, sample.kind="Rounding")
flare_train_reg_earth_fits <- lapply(model_types, function(model){
  print(model)
  train(C_class ~ Mod_Zur_Class_Code + Activity + Lrgst_spot_size_code + Spot_dist_code + Area, method = model, data = flare_train_reg_trn, trControl = control)
})

n_models <- seq(1:length(model_types))
model_rmses <- sapply(n_models, function(m_number){
  min(flare_train_reg_earth_fits[[m_number]][["results"]][["RMSE"]])
})

flare_train_reg_earth_fit_rmses <- as.data.frame(model_rmses) %>%
  mutate(model_name = model_types)
colnames(flare_train_reg_earth_fit_rmses) <- c("RMSE", "model_name")

flare_train_reg_earth_fit_rmses

#stepwise reg variables
model_types <- c("ranger", "Rborist", "rf", "pcaNNet", "neuralnet", "mlpML")
set.seed(1, sample.kind="Rounding")
flare_train_reg_step_fits <- lapply(model_types, function(model){
  print(model)
  train(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Activity + Area, method = model, data = flare_train_reg_trn, trControl = control)
})

n_models <- seq(1:length(model_types))
model_rmses <- sapply(n_models, function(m_number){
  min(flare_train_reg_step_fits[[m_number]][["results"]][["RMSE"]])
})

flare_train_reg_step_fit_rmses <- as.data.frame(model_rmses) %>%
  mutate(model_name = model_types)
colnames(flare_train_reg_step_fit_rmses) <- c("RMSE", "model_name")

flare_train_reg_step_fit_rmses

#compare all
flare_train_reg_varimp_fit_rmses
flare_train_reg_earth_fit_rmses
flare_train_reg_step_fit_rmses
#Variable performance to tuning
#Performance-wise, there are no major gains observed, however, based on prior tuned vs untuned performance, it looks
#like improvements could be made to all models by tuning the hyperparameters.

#varimp
flare_train_reg_varimp_fit_rmses
  #ranger
flare_train_reg_varimp_fits[[1]][["results"]]
ranger_reg_tune_grid <- expand.grid(mtry = c(1:5), splitrule = c("variance", "extratrees"), min.node.size = c(2:8))
set.seed(1, sample.kind="Rounding")
ranger_varimp_fit_reg_tune <- train(C_class ~ Historically_complex + Mod_Zur_Class_Code + Lrgst_spot_size_code + Area + Evolution + Spot_dist_code, method = "ranger", data = flare_train_reg_trn, trControl = control, tuneGrid = ranger_reg_tune_grid)
ranger_varimp_fit_reg_tune
ranger_varimp_fit_reg_tune$bestTune
ranger_varimp_fit_reg_tune_pred <- predict(ranger_varimp_fit_reg_tune, flare_train_reg_val)
ranger_varimp_fit_reg_tune_RMSE <- RMSE(ranger_varimp_fit_reg_tune_pred, flare_train_reg_val$C_class)
ranger_varimp_fit_reg_tune_RMSE

  #Rborist
flare_train_reg_varimp_fits[[2]][["results"]]
Rborist_reg_tune_grid <- expand.grid(predFixed = 2, minNode = c(2:10))
set.seed(1, sample.kind="Rounding")
Rborist_varimp_fit_reg_tune <- train(C_class ~ Historically_complex + Mod_Zur_Class_Code + Lrgst_spot_size_code + Area + Evolution + Spot_dist_code, method = "Rborist", data = flare_train_reg_trn, trControl = control, tuneGrid = Rborist_reg_tune_grid)
Rborist_varimp_fit_reg_tune
Rborist_varimp_fit_reg_tune$bestTune
Rborist_varimp_fit_reg_tune_pred <- predict(Rborist_varimp_fit_reg_tune, flare_train_reg_val)
Rborist_varimp_fit_reg_tune_RMSE <- RMSE(Rborist_varimp_fit_reg_tune_pred, flare_train_reg_val$C_class)
Rborist_varimp_fit_reg_tune_RMSE
  
  #rf
flare_train_reg_varimp_fits[[3]][["results"]]
rf_reg_tune_grid <- expand.grid(mtry = c(1:5))
set.seed(1, sample.kind="Rounding")
rf_varimp_fit_reg_tune <- train(C_class ~ Historically_complex + Mod_Zur_Class_Code + Lrgst_spot_size_code + Area + Evolution + Spot_dist_code, method = "rf", data = flare_train_reg_trn, trControl = control, tuneGrid = rf_reg_tune_grid)
rf_varimp_fit_reg_tune
rf_varimp_fit_reg_tune$bestTune
rf_varimp_fit_reg_tune_pred <- predict(rf_varimp_fit_reg_tune, flare_train_reg_val)
rf_varimp_fit_reg_tune_RMSE <- RMSE(rf_varimp_fit_reg_tune_pred, flare_train_reg_val$C_class)
rf_varimp_fit_reg_tune_RMSE

  #pcaNNet
flare_train_reg_varimp_fits[[4]][["results"]]
pcaNNet_reg_tune_grid <- expand.grid(size = c(1:3), decay = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1))
set.seed(1, sample.kind="Rounding")
pcaNNet_varimp_fit_reg_tune <- train(C_class ~ Historically_complex + Mod_Zur_Class_Code + Lrgst_spot_size_code + Area + Evolution + Spot_dist_code, method = "pcaNNet", data = flare_train_reg_trn, trControl = control, tuneGrid = pcaNNet_reg_tune_grid)
pcaNNet_varimp_fit_reg_tune
pcaNNet_varimp_fit_reg_tune$bestTune
pcaNNet_varimp_fit_reg_tune_pred <- predict(pcaNNet_varimp_fit_reg_tune, flare_train_reg_val)
pcaNNet_varimp_fit_reg_tune_RMSE <- RMSE(pcaNNet_varimp_fit_reg_tune_pred, flare_train_reg_val$C_class)
pcaNNet_varimp_fit_reg_tune_RMSE

  #neuralnet
flare_train_reg_varimp_fits[[5]][["results"]]
neuralnet_reg_tune_grid <- expand.grid(layer1 = c(1, 2, 3), layer2 = c(1, 2, 3), layer3 = c(1, 2, 3))
set.seed(1, sample.kind="Rounding")
neuralnet_varimp_fit_reg_tune <- train(C_class ~ Historically_complex + Mod_Zur_Class_Code + Lrgst_spot_size_code + Area + Evolution + Spot_dist_code, method = "neuralnet", data = flare_train_reg_trn, trControl = control, tuneGrid = neuralnet_reg_tune_grid, rep = 3, threshold = 0.1)
neuralnet_varimp_fit_reg_tune
neuralnet_varimp_fit_reg_tune$bestTune
neuralnet_varimp_fit_reg_tune_pred <- predict(neuralnet_varimp_fit_reg_tune, flare_train_reg_val)
neuralnet_varimp_fit_reg_tune_RMSE <- RMSE(neuralnet_varimp_fit_reg_tune_pred, flare_train_reg_val$C_class)
neuralnet_varimp_fit_reg_tune_RMSE

  #mlpML
flare_train_reg_varimp_fits[[6]][["results"]]
mlpML_reg_tune_grid <- expand.grid(layer1 = c(1, 2, 3), layer2 = c(1, 2, 3), layer3 = c(1, 2, 3))
set.seed(1, sample.kind="Rounding")
mlpML_varimp_fit_reg_tune <- train(C_class ~ Historically_complex + Mod_Zur_Class_Code + Lrgst_spot_size_code + Area + Evolution + Spot_dist_code, method = "mlpML", data = flare_train_reg_trn, trControl = control, tuneGrid = mlpML_reg_tune_grid)
mlpML_varimp_fit_reg_tune
mlpML_varimp_fit_reg_tune$bestTune
mlpML_varimp_fit_reg_tune_pred <- predict(mlpML_varimp_fit_reg_tune, flare_train_reg_val)
mlpML_varimp_fit_reg_tune_RMSE <- RMSE(mlpML_varimp_fit_reg_tune_pred, flare_train_reg_val$C_class)
mlpML_varimp_fit_reg_tune_RMSE

#earth
flare_train_reg_earth_fit_rmses
  #ranger
flare_train_reg_earth_fits[[1]][["results"]]
ranger_reg_tune_grid <- expand.grid(mtry = c(1:5), splitrule = c("variance", "extratrees"), min.node.size = c(2:8))
set.seed(1, sample.kind="Rounding")
ranger_earth_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Activity + Lrgst_spot_size_code + Spot_dist_code + Area, method = "ranger", data = flare_train_reg_trn, trControl = control, tuneGrid = ranger_reg_tune_grid)
ranger_earth_fit_reg_tune
ranger_earth_fit_reg_tune$bestTune
ranger_earth_fit_reg_tune_pred <- predict(ranger_earth_fit_reg_tune, flare_train_reg_val)
ranger_earth_fit_reg_tune_RMSE <- RMSE(ranger_earth_fit_reg_tune_pred, flare_train_reg_val$C_class)
ranger_earth_fit_reg_tune_RMSE
  
  #Rborist
flare_train_reg_earth_fits[[2]][["results"]]
Rborist_reg_tune_grid <- expand.grid(predFixed = c(2), minNode = c(2:10))
set.seed(1, sample.kind="Rounding")
Rborist_earth_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Activity + Lrgst_spot_size_code + Spot_dist_code + Area, method = "Rborist", data = flare_train_reg_trn, trControl = control, tuneGrid = Rborist_reg_tune_grid)
Rborist_earth_fit_reg_tune
Rborist_earth_fit_reg_tune$bestTune
Rborist_earth_fit_reg_tune_pred <- predict(Rborist_earth_fit_reg_tune, flare_train_reg_val)
Rborist_earth_fit_reg_tune_RMSE <- RMSE(Rborist_earth_fit_reg_tune_pred, flare_train_reg_val$C_class)
Rborist_earth_fit_reg_tune_RMSE
  
  #rf
flare_train_reg_earth_fits[[3]][["results"]]
rf_reg_tune_grid <- expand.grid(mtry = c(1:5))
set.seed(1, sample.kind="Rounding")
rf_earth_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Activity + Lrgst_spot_size_code + Spot_dist_code + Area, method = "rf", data = flare_train_reg_trn, trControl = control, tuneGrid = rf_reg_tune_grid)
rf_earth_fit_reg_tune
rf_earth_fit_reg_tune$bestTune
rf_earth_fit_reg_tune_pred <- predict(rf_earth_fit_reg_tune, flare_train_reg_val)
rf_earth_fit_reg_tune_RMSE <- RMSE(rf_earth_fit_reg_tune_pred, flare_train_reg_val$C_class)
rf_earth_fit_reg_tune_RMSE

  #pcaNNet
flare_train_reg_earth_fits[[4]][["results"]]
pcaNNet_reg_tune_grid <- expand.grid(size = c(1:3), decay = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1))
set.seed(1, sample.kind="Rounding")
pcaNNet_earth_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Activity + Lrgst_spot_size_code + Spot_dist_code + Area, method = "pcaNNet", data = flare_train_reg_trn, trControl = control, tuneGrid = pcaNNet_reg_tune_grid)
pcaNNet_earth_fit_reg_tune
pcaNNet_earth_fit_reg_tune$bestTune
pcaNNet_earth_fit_reg_tune_pred <- predict(pcaNNet_earth_fit_reg_tune, flare_train_reg_val)
pcaNNet_earth_fit_reg_tune_RMSE <- RMSE(pcaNNet_earth_fit_reg_tune_pred, flare_train_reg_val$C_class)
pcaNNet_earth_fit_reg_tune_RMSE

  #neuralnet
flare_train_reg_earth_fits[[5]][["results"]]
neuralnet_reg_tune_grid <- expand.grid(layer1 = c(1, 2, 3), layer2 = c(1, 2, 3), layer3 = c(1, 2, 3))
set.seed(1, sample.kind="Rounding")
neuralnet_earth_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Activity + Lrgst_spot_size_code + Spot_dist_code + Area, method = "neuralnet", data = flare_train_reg_trn, trControl = control, tuneGrid = neuralnet_reg_tune_grid, rep = 3, threshold = 0.1)
neuralnet_earth_fit_reg_tune
neuralnet_earth_fit_reg_tune$bestTune
neuralnet_earth_fit_reg_tune_pred <- predict(neuralnet_earth_fit_reg_tune, flare_train_reg_val)
neuralnet_earth_fit_reg_tune_RMSE <- RMSE(neuralnet_earth_fit_reg_tune_pred, flare_train_reg_val$C_class)
neuralnet_earth_fit_reg_tune_RMSE
  
  #mlpML
flare_train_reg_earth_fits[[6]][["results"]]
mlpML_reg_tune_grid <- expand.grid(layer1 = c(1:3), layer2 = c(1:5), layer3 = c(1:6))
set.seed(1, sample.kind="Rounding")
mlpML_earth_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Activity + Lrgst_spot_size_code + Spot_dist_code + Area, method = "mlpML", data = flare_train_reg_trn, trControl = control, tuneGrid = mlpML_reg_tune_grid)
mlpML_earth_fit_reg_tune
mlpML_earth_fit_reg_tune$bestTune
mlpML_earth_fit_reg_tune_pred <- predict(mlpML_earth_fit_reg_tune, flare_train_reg_val)
mlpML_earth_fit_reg_tune_RMSE <- RMSE(mlpML_earth_fit_reg_tune_pred, flare_train_reg_val$C_class)
mlpML_earth_fit_reg_tune_RMSE


#step
flare_train_reg_step_fit_rmses
  #ranger
flare_train_reg_step_fits[[1]][["results"]]
ranger_reg_tune_grid <- expand.grid(mtry = c(1:5), splitrule = c("variance", "extratrees"), min.node.size = c(2:10))
set.seed(1, sample.kind="Rounding")
ranger_step_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Activity + Area, method = "ranger", data = flare_train_reg_trn, trControl = control, tuneGrid = ranger_reg_tune_grid)
ranger_step_fit_reg_tune
ranger_step_fit_reg_tune$bestTune
ranger_step_fit_reg_tune_pred <- predict(ranger_step_fit_reg_tune, flare_train_reg_val)
ranger_step_fit_reg_tune_RMSE <- RMSE(ranger_step_fit_reg_tune_pred, flare_train_reg_val$C_class)
ranger_step_fit_reg_tune_RMSE

  #Rborist
flare_train_reg_step_fits[[2]][["results"]]
Rborist_reg_tune_grid <- expand.grid(predFixed = c(2), minNode = c(2:15))
set.seed(1, sample.kind="Rounding")
Rborist_step_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Activity + Area, method = "Rborist", data = flare_train_reg_trn, trControl = control, tuneGrid = Rborist_reg_tune_grid)
Rborist_step_fit_reg_tune
Rborist_step_fit_reg_tune$bestTune
Rborist_step_fit_reg_tune_pred <- predict(Rborist_step_fit_reg_tune, flare_train_reg_val)
Rborist_step_fit_reg_tune_RMSE <- RMSE(Rborist_step_fit_reg_tune_pred, flare_train_reg_val$C_class)
Rborist_step_fit_reg_tune_RMSE

  #rf
flare_train_reg_step_fits[[3]][["results"]]
rf_reg_tune_grid <- expand.grid(mtry = c(1:5))
set.seed(1, sample.kind="Rounding")
rf_step_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Activity + Area, method = "rf", data = flare_train_reg_trn, trControl = control, tuneGrid = rf_reg_tune_grid)
rf_step_fit_reg_tune
rf_step_fit_reg_tune$bestTune
rf_step_fit_reg_tune_pred <- predict(rf_step_fit_reg_tune, flare_train_reg_val)
rf_step_fit_reg_tune_RMSE <- RMSE(rf_step_fit_reg_tune_pred, flare_train_reg_val$C_class)
rf_step_fit_reg_tune_RMSE

  #pcaNNet
flare_train_reg_step_fits[[4]][["results"]]
pcaNNet_reg_tune_grid <- expand.grid(size = c(1:3), decay = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1))
set.seed(1, sample.kind="Rounding")
pcaNNet_step_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Activity + Area, method = "pcaNNet", data = flare_train_reg_trn, trControl = control, tuneGrid = pcaNNet_reg_tune_grid)
pcaNNet_step_fit_reg_tune
pcaNNet_step_fit_reg_tune$bestTune
pcaNNet_step_fit_reg_tune_pred <- predict(pcaNNet_step_fit_reg_tune, flare_train_reg_val)
pcaNNet_step_fit_reg_tune_RMSE <- RMSE(pcaNNet_step_fit_reg_tune_pred, flare_train_reg_val$C_class)
pcaNNet_step_fit_reg_tune_RMSE

  #neuralnet
flare_train_reg_step_fits[[5]][["results"]]
neuralnet_reg_tune_grid <- expand.grid(layer1 = c(1, 2, 3), layer2 = c(1, 2, 3), layer3 = c(1, 2, 3))
set.seed(1, sample.kind="Rounding")
neuralnet_step_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Activity + Area, method = "neuralnet", data = flare_train_reg_trn, trControl = control, tuneGrid = neuralnet_reg_tune_grid, rep = 3, threshold = 0.1)
neuralnet_step_fit_reg_tune
neuralnet_step_fit_reg_tune$bestTune
neuralnet_step_fit_reg_tune_pred <- predict(neuralnet_step_fit_reg_tune, flare_train_reg_val)
neuralnet_step_fit_reg_tune_RMSE <- RMSE(neuralnet_step_fit_reg_tune_pred, flare_train_reg_val$C_class)
neuralnet_step_fit_reg_tune_RMSE

  #mlpML
flare_train_reg_step_fits[[6]][["results"]]
mlpML_reg_tune_grid <- expand.grid(layer1 = c(1:3), layer2 = c(5:10), layer3 = c(5:10))
set.seed(1, sample.kind="Rounding")
mlpML_step_fit_reg_tune <- train(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Activity + Area, method = "mlpML", data = flare_train_reg_trn, trControl = control, tuneGrid = mlpML_reg_tune_grid)
mlpML_step_fit_reg_tune
mlpML_step_fit_reg_tune$bestTune
mlpML_step_fit_reg_tune_pred <- predict(mlpML_step_fit_reg_tune, flare_train_reg_val)
mlpML_step_fit_reg_tune_RMSE <- RMSE(mlpML_step_fit_reg_tune_pred, flare_train_reg_val$C_class)
mlpML_step_fit_reg_tune_RMSE

#Assemble & assess results & determine final test set RMSE(s)
tuned_feature_selected_model_performance <- data.table("Model" = c("ranger","ranger","ranger", "Rborist", "Rborist", "Rborist", "rf", "rf", "rf", "pcaNNet", "pcaNNet", "pcaNNet", "neurlnet", "neurlnet", "neurlnet", "mlpML", "mlpML", "mlpML"),
           "Feature Selection" = c("Variable Importance", "Earth Variable Importance", "Stepwise Regression", "Variable Importance", "Earth Variable Importance", "Stepwise Regression", "Variable Importance", "Earth Variable Importance", "Stepwise Regression", "Variable Importance", "Earth Variable Importance", "Stepwise Regression", "Variable Importance", "Earth Variable Importance", "Stepwise Regression", "Variable Importance", "Earth Variable Importance", "Stepwise Regression"),
           "Model RMSE" = c(min(ranger_varimp_fit_reg_tune$results$RMSE), min(ranger_earth_fit_reg_tune$results$RMSE), min(ranger_step_fit_reg_tune$results$RMSE), 
                          min(Rborist_varimp_fit_reg_tune$results$RMSE), min(Rborist_earth_fit_reg_tune$results$RMSE), min(Rborist_step_fit_reg_tune$results$RMSE), 
                          min(rf_varimp_fit_reg_tune$results$RMSE), min(rf_earth_fit_reg_tune$results$RMSE), min(rf_step_fit_reg_tune$results$RMSE), 
                          min(pcaNNet_varimp_fit_reg_tune$results$RMSE), min(pcaNNet_earth_fit_reg_tune$results$RMSE), min(pcaNNet_step_fit_reg_tune$results$RMSE), 
                          min(neuralnet_varimp_fit_reg_tune$results$RMSE), min(neuralnet_earth_fit_reg_tune$results$RMSE), min(neuralnet_step_fit_reg_tune$results$RMSE), 
                          min(mlpML_varimp_fit_reg_tune$results$RMSE), min(mlpML_earth_fit_reg_tune$results$RMSE), min(mlpML_step_fit_reg_tune$results$RMSE)),
           "Validation Predicted RMSE" = c(
                                         ranger_varimp_fit_reg_tune_RMSE, ranger_earth_fit_reg_tune_RMSE, ranger_step_fit_reg_tune_RMSE, 
                                         Rborist_varimp_fit_reg_tune_RMSE, Rborist_earth_fit_reg_tune_RMSE, Rborist_step_fit_reg_tune_RMSE, 
                                         rf_varimp_fit_reg_tune_RMSE, rf_earth_fit_reg_tune_RMSE, rf_step_fit_reg_tune_RMSE, 
                                         pcaNNet_varimp_fit_reg_tune_RMSE, pcaNNet_earth_fit_reg_tune_RMSE, pcaNNet_step_fit_reg_tune_RMSE, 
                                         neuralnet_varimp_fit_reg_tune_RMSE, neuralnet_earth_fit_reg_tune_RMSE, neuralnet_step_fit_reg_tune_RMSE, 
                                         mlpML_varimp_fit_reg_tune_RMSE, mlpML_earth_fit_reg_tune_RMSE, mlpML_step_fit_reg_tune_RMSE))

tuned_feature_selected_model_performance
tuned_feature_selected_model_performance %>% arrange(`Model RMSE`)
tuned_feature_selected_model_performance %>% arrange(`Validation Predicted RMSE`)

#Difference between trained RMSE and that calculated using validation set indicates a degree of overtraining.
#Given the poor performance even at trained model level it would still be too inaccurate to be a great deal of use
#were the overtraining eliminated and the validation set RMSE more reflective of the trained model performance.

#In terms of feature selection, for model performance stepwise regression generally gave a more favourable RMSE, 
#however when it came to RMSE on the validation set it was more split between Variable Importance calculated from an
#Earth model and Stepwise Regression. It should be noted, this result may be a little misleading as it could be
#reasonably expected for this to change were the potential overtraining dealt with.
#With this in mind we will retrain the models that performed best on the validation set on the basis that the
#RMSEs in the validation set predictions is a better reflection of actual performance. We will use the exact same 
#features and tuning parameters as was used before. This time, however, the full training set (including validation)
#will be used for training and the test set used for predictions.

#training control adjustment to try to deal with overtraining
control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

#Models

#Rborist - Earth Variable Importance
Rborist_reg_tune_grid <- expand.grid(predFixed = 2, minNode = 9)
set.seed(1, sample.kind="Rounding")
Rborist_earth_fit_reg_tune_fin <- train(C_class ~ Mod_Zur_Class_Code + Activity + Lrgst_spot_size_code + Spot_dist_code + Area, method = "Rborist", data = flare_train_reg, trControl = control, tuneGrid = Rborist_reg_tune_grid)
Rborist_earth_fit_reg_tune_fin
Rborist_earth_fit_reg_tune_fin_pred <- predict(Rborist_earth_fit_reg_tune_fin, flare_test_reg)
Rborist_earth_fit_reg_tune_fin_RMSE <- RMSE(Rborist_earth_fit_reg_tune_fin_pred, flare_test_reg$C_class)
Rborist_earth_fit_reg_tune_fin_RMSE

#Rborist - Stepwise Regression
Rborist_reg_tune_grid <- expand.grid(predFixed = 2, minNode = 14)
set.seed(1, sample.kind="Rounding")
Rborist_step_fit_reg_tune_fin <- train(C_class ~ Mod_Zur_Class_Code + Lrgst_spot_size_code + Activity + Area, method = "Rborist", data = flare_train_reg, trControl = control, tuneGrid = Rborist_reg_tune_grid)
Rborist_step_fit_reg_tune_fin
Rborist_step_fit_reg_tune_fin_pred <- predict(Rborist_step_fit_reg_tune_fin, flare_test_reg)
Rborist_step_fit_reg_tune_fin_RMSE <- RMSE(Rborist_step_fit_reg_tune_fin_pred, flare_test_reg$C_class)
Rborist_step_fit_reg_tune_fin_RMSE

#ranger -  Earth Variable Importance
ranger_reg_tune_grid <- expand.grid(mtry = 2, splitrule = "variance", min.node.size = 7)
set.seed(1, sample.kind="Rounding")
ranger_earth_fit_reg_tune_fin <- train(C_class ~ Mod_Zur_Class_Code + Activity + Lrgst_spot_size_code + Spot_dist_code + Area, method = "ranger", data = flare_train_reg, trControl = control, tuneGrid = ranger_reg_tune_grid)
ranger_earth_fit_reg_tune_fin
ranger_earth_fit_reg_tune_fin_pred <- predict(ranger_earth_fit_reg_tune_fin, flare_test_reg)
ranger_earth_fit_reg_tune_fin_RMSE <- RMSE(ranger_earth_fit_reg_tune_fin_pred, flare_test_reg$C_class)
ranger_earth_fit_reg_tune_fin_RMSE


Rborist_earth_fit_reg_tune_fin_RMSE
Rborist_step_fit_reg_tune_fin_RMSE
ranger_earth_fit_reg_tune_fin_RMSE
#All predictions made on the test set produce a more accurate result.
#Investigate

#Check C-class value distribution across train/val/test. This should be broadly the same, as per CreatePartition
#Caret documentation
flare_train_reg %>% group_by(C_class) %>% summarise(n = n()/length(flare_train_reg$C_class))
flare_train_reg_trn %>% group_by(C_class) %>% summarise(n = n()/length(flare_train_reg_trn$C_class))
flare_train_reg_val %>% group_by(C_class) %>% summarise(n = n()/length(flare_train_reg_val$C_class))
flare_test_reg %>% group_by(C_class) %>% summarise(n = n()/length(flare_test_reg$C_class))
#Mostly the same, differences observed aren't going to be large enough to produce observed effect.

#Check cross validation parameters
#initial training method repeatedcv, 10 folds, 5 repeats
#This approach could be more likely to decrease accuracy, given the imbalance in the data. If the likelihood of the
#fold validation sets containing all zero flare events the model could be optimised to over predict zero flare events.
#This could explain the consistent difference in training and validation RMSEs observed for all models.

#The other possible explanation is the increased size of the training set has precipitated a more accurate model 
#simply by having a greater range of data with which to work.


#ranger_earth_fit_reg_tune_fin_RMSE model produces most accurate result, though the overall fit is very poor - RMSE, 0.604957.
#Plot model predictions against actual values - establish relative performance
ggplot() +
  geom_point(aes(flare_test_reg$C_class, ranger_earth_fit_reg_tune_fin_pred), alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Ranger - Earth Variable Importance Features") +
  xlab("Actual Value") +
  ylab("Predicted Value") +
  ylim(0, 4)

#Model highly likely to under predict for 1+ flare events and over predict for zero flare events. As one might 
#expect, the high frequency of zero flare events in the training data has resulted in a model that is skewed in that
#direction. This is something that should definitely be addressed down the line.



#Further work down the line.
#Investigate the value in first creating a model to make a binary choice between zero and 1+ flare events followed
#by a model specifically to predict the number of flare events over 0.
#Revisit balancing the dataset for use with regression models and compare to imbalanced dataset.



save.image(file = "fin_flr.RData")



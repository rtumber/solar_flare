#Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org")


library(data.table)
library(DataExplorer)
library(gridExtra)
library(caTools)
library(kernlab)
library(caret)
library(tidyverse)
library(ROSE)

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

#Can these relationships be refined if we each variable against each other and flare count
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

#Returning to the earlier observations, we should construct a model using the following variables.
#Code for class (modified Zurich class)
#Code for largest spot size
#Code for spot distribution
#Activity
#Evolution
#Previous 24 hour flare activity code
#Did region become historically complex on this pass across the sun's disk
#Area

#Potentially Historically-complex variable, though the fairly even split indicate it is of low relevance 

#Need to return number of flares to categorical/discrete for predictions?
flare_data$`C-class_flares_(common)` <- as.factor(flare_data$`C-class_flares_(common)`)

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
#  control <- trainControl(method = "repeatedcv", number = 10, repeats = 5, sampling = splmtd)
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

#Now look at tuning the existing models before looking for alternatives.


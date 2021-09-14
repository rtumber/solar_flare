# solar_flare

## Solar flare prediction over 24 hour period from observation data

### Summary
Solar flare data set downloaded from from https://archive.ics.uci.edu/ml/datasets/Solar+Flare.
The aim of this learning exercise was to predict the number of solar flares of different classes over a 24 hour period based on observations from active regions on the Sun.

The distribution of each variable with in relation to the number of observed flares was first examined. The result of this was the decision to limit the scope of the exercise to C-Class flare counts  of 0-3 due the imbalanced nature of the dataset.

After an unsuccessful experiment in treating the exercise as one of classification on account of the discrete nature of flare events, a regression model was trained using caret, with features included determined (separately) by recursive feature elimination, examination of variable importance, and stepwise regression. Various models were used to determine which produced more accurate results. Random Forest and Neural Net models types typically produced lower RMSEs, so a number of models were trained and tuned before the most accurate were used to predict flare events a previously determined test set.

The lowest RMSE calculated was from a Ranger model using features selected using variable importance obtained from an Earth generated model, however, it still proved too inaccurate to be of any real use. When analysed it appeared this behaviour could stem from the imbalance in the dataset.


### Files
File FullScript.R is the script used to download and process the data, create the model and assess the performance.
File solar_flare_writeup.Rmd is R markdown for the report.
File solar_flare_writeup.pdf is final knitted report.
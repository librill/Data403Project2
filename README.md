## Home Credit Default Risk Classification

The goal of this project is to predict whether an applicant will be able to pay back their loan. The data is provided by [this Kaggle competition](https://www.kaggle.com/c/home-credit-default-risk/).

We use the following types of classification: Logistic, LDA, and SVC.

Our process is as follows:

-   Split the data into training, validation, and testing sets.
-   Fit and evaluate a bunch of models on training data.
-   Take the model of each type (Logistic, LDA, and SVC) that performed best on training and evaluate on validation data.
-   Take the model that performed best on validation data and report metrics as it performs on test set.

The code organization is as follows:

-   /proposal contains project proposal files

-   /scripts contains all of the scripts that are used in notebooks

    -   data_cleaning.R contains data cleaning

    -   functions.R contains a bunch of functions

    -   libraries.R loads all libraries that are used

    -   plots.R creates plots

    -   splits.R contains different functions that split data differently

-   /notebooks contains all the notebooks:

    -   lda.Rmd is fitting/evaluation of LDA on training data

    -   logistic.Rmd is fitting/evaluation of Logistic on training data

    -   svc.Rmd is fitting/evaluation of SVC on training data

    -   selection.Rmd is evaluation of models on validation and testing sets. also includes evaluation and discussion of model fairness.

    -   fairness.RMD contains manual error analysis on "eyeball" portion of validation set

-   /images contains images of plots

## March Machine Learning Mania 2015
Predict the 2015 NCAA Basketball Tournament, hosted on Kaggle http://www.kaggle.com/c/march-machine-learning-mania-2015. This placed 24th in this Machine Learning competition.

###Scripts
-NCAA2015Main.R contains all the important analyses, modeling and predictions. Please refer to this script for the imporant parts. -resultsPredictionMatrix.R contains a local evaluation helper function. It creates a matrix with two columns, the prediction by the model and the actual results. It also prints out the LogLoss score for those two columns.
-getSeedDivision.R extracts the seed and a division of a team from the data file.
-getScoreAndWins.R extracts the score and victories of past tournaments of a team from the data file.
-getForAndAgainstPoints.R extracts the points for and against of a given tournament of a team from list of results previously created.

UPDATE: The original code made use of h2o machine learning library, however as of may 2015 a new version of h2o became available, unfortunately the new h2o has not yet native cross validation and it's very difficult to do it on caret so the library was replaced by glmnet. The code will go back to using h2o if the results by h2o are superior and when native cross validation becomes available again. For the original code, check commit [d44ff9bdddd26b855cdba9692d8a92a8991a31e5](https://github.com/wacax/MarchMachineLearningMadness2015/commit/d44ff9bdddd26b855cdba9692d8a92a8991a31e5) and before.

![Imgur](http://i.imgur.com/jISWGOn.png)


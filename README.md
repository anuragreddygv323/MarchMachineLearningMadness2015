## March Machine Learning Mania 2015
Predict the 2015 NCAA Basketball Tournament, hosted on Kaggle http://www.kaggle.com/c/march-machine-learning-mania-2015. This placed 24th in this Machine Learning competition.

-NCAA2015Main.R contains all the important analyses, modeling and predictions. Please refer to this script for the imporant parts. 

UPDATE: The original code made use of h2o machine learning library, however as of may 2015 a new version of h2o became available, unfortunately the new h2o has not yet native cross validation and it's very difficult to do it on caret so the library was replaced by glmnet. The code will go back to using h2o if the results by h2o are superior and when native cross validation becomes available again.

![Imgur](http://i.imgur.com/jISWGOn.png)


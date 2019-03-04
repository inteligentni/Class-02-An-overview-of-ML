# 
# An overviwe of ML problems
# Vladan Devedzic, Aug 27, 2017
# 


# (Installing and) Loading the required R packages:
# install.packages('corrplot')
library(corrplot)
library(ggplot2)


#####################
# Linear regression #
#####################

# Reading the dataset

# A modified or newly created dataset might have been saved earlier using:
# write.csv(x = <dataframe>, file = "<filename>", row.names = F)  # do not include the row names (row numbers) column
# saveRDS(object = <dataframe or another R object>, file = "<filename>")  # save R object for the next session
# Restoring the dataset from the corresponding RData file:
# <dataframe or another R object> <- readRDS(file = "<filename>")         # restore R object in the next session
# The Beatles songs dataset has been saved earlier using:
# saveRDS(the.beatles.songs, "The Beatles songs dataset, v2.1.RData")
the.beatles.songs <- readRDS("The Beatles songs dataset, v2.1.RData")
summary(the.beatles.songs)

# Examining the data

# Scatterplot matrices (useful for examining the presence of linear relationship between several pairs of variables):
# pairs(~<x1> + <x2> + ..., data = <dataframe>)
pairs(~Top.50.Billboard + Duration + Other.releases, data = the.beatles.songs)

# Correlation plots:                            # correlations between numeric variables in the dataset
# <numeric dataframe> <-                                        # create all-numeric dataframe, 
#   data.frame(<num col 1 name> = <dataframe>$<num col 1>,      # leave out all non-numeric columns 
#              <num col 2 name> = <dataframe>$<num col 2>,      # from the original dataframe
#              ...)
# <correlation matrix> <- cor(<numeric dataframe>)              # all-numeric dataframe
# library(corrplot)
# corrplot.mixed(<correlation matrix>, tl.cex = <text font size>, number.cex = <number font size>)
the.beatles.songs.num <- data.frame(Top.50.Billboard = the.beatles.songs$Top.50.Billboard, 
                                    Duration = the.beatles.songs$Duration, 
                                    Other.releases = the.beatles.songs$Other.releases)
correlation.matrix <- cor(the.beatles.songs.num)
correlation.matrix
library(corrplot)
corrplot.mixed(correlation.matrix, tl.cex = 0.75, number.cex = 0.75)

# Scatterplots in ggplot2:
# ggplot(<dataset>, aes(x = <num.var.1>, y = <num.var.2>)) +
#   geom_point(shape = <n>,       # <n> = 1: hollow circle
#              fill = <color 1>,  # color of point fill (optional)
#              color = <color 2>, # color of point line (optional)
#              size = <s>) +      # size  of point line (optional)
#   geom_smooth(method = lm,      # add regression line (optional); if left out, nonlinear best-fit line is shown
#               se=FALSE)         # do NOT show 95% confidence region as a shaded area (optional)
g1 <- ggplot(the.beatles.songs, aes(x = Duration, y = Top.50.Billboard)) +
  geom_point(shape = 1)
g1

# g1 <- ggplot(the.beatles.songs, aes(x = Duration, y = Top.50.Billboard)) +
#   geom_point(shape = 21, size = 2, fill = "green", color = "red") +
#   geom_smooth(method = lm)
# g1

g1 <- ggplot(the.beatles.songs, aes(x = Duration, y = Top.50.Billboard)) +
  geom_point(shape = 1) + 
  geom_smooth(method = lm)  # linear regression
g1

g2 <- ggplot(the.beatles.songs, aes(x = Other.releases, y = Top.50.Billboard)) +
  geom_point(shape = 1) + 
  geom_smooth(method = lm)
g2

g3 <- ggplot(the.beatles.songs, aes(x = Other.releases, y = Top.50.Billboard)) +
  geom_point(shape = 1) + 
  geom_smooth()             # non-linear regression
g3

# Build/Fit simple linear regression model and examine it briefly:
# <model> <- lm(<y> ~ <x>,          # build/fit the model over the <dataset>; 
#               data = <dataset>)   # <x> and <y> are numeric variables from <dataset>
# <model>                   # show the model
# coef(<model>)             # show the coefficients of the linear model (intercept and slope)
# confint(<model>)          # show the confidence intervals for the estimated intercept and slope
# summary(<model>)          # show the model statistics
lm.fit <- lm(Top.50.Billboard ~ Other.releases, data = the.beatles.songs)
lm.fit
coef(lm.fit)
confint(lm.fit)
summary(lm.fit)

# Make predictions with this model:
# predict(<model>,                            # the model built above
#         <test dataframe>,                   # data frame built over a (small) vector of <x> to predict <y> for; 
#                                             # in the <test dataframe> the name of <x> as in the original dataframe
#         interval = "confidence" |           # include the confidence interval for the predictions (optional)
#                    "predict")               # include prediction intervals (optional)
the.beatles.songs.num <- data.frame(Other.releases = c(5, 15, 25))         
predict(lm.fit, newdata = the.beatles.songs.num, interval = "confidence")
predict(lm.fit, newdata = the.beatles.songs.num, interval = "predict")

# Check how well our model fits the data:
# par(mfrow = c(2,2))     # set up the plotting panel for 4 graphs
# plot(lm.fit)            # plot the 4 graphs
# par(mfrow = c(1,1))     # reset the plotting panel
# The 4 graphs:
#   Residuals vs Fitted: Is linear assumption justified? 
#     Can a non-linear pattern be observed in the residuals? 
#       If so, there is a non-linearity not explained by the model...
#     Are the residuals more-or-less evenly (randomly) distributed around the horizontal dotted line?
#       It's better if they are (the linearity assumption is then likely to hold).
#       Ideally (but very unlikely), the red line is overlapped with the horizontal dotted line.
#   Q-Q plot: Are the residuals normally distributed (reside on the diagonal line)?
#     It's good if they are.
#   Scale-Location: Is the variance of residuals similar (even) along the fitted line?
#     Are the residuals spread evenly along the range(s) of predictor(s)?
#       It's good if they are, and in that case the red line is more-or-less horizontal.
#   Residuals vs Leverage: Are there extreme values of a predictor (points of high leverage) 
#   that shouldn't be excluded from the analysis?
#     Are there data points outside the dashed red line (having a high Cook's distance score)?
#       If so, they should be given special attention. If they result from erroneous data, 
#       they can be excluded from the analysis. Otherwise, they shouldn't be excluded from the analysis, 
#       because R-squared and the slope will change a lot. In that case, linear regression is not applicable.
# The four plots show potential problematic cases with the row numbers of the data in the dataset. 
# If some cases are identified across all four plots (Residuals vs Leverage being especially critical), 
# or at least on all plots other than Q-Q plot, it is a good idea to take a closer look at them individually. 
# Is there anything special for the subject? Or could they be simply errors in data entry?
par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow = c(1,1))


###################################
# Classification - decision trees #
###################################

# Read the dataset:
# saveRDS(the.beatles.songs, "The Beatles songs dataset, v2.2.RData")     # saved earlier
the.beatles.songs <- readRDS("The Beatles songs dataset, v2.2.RData")
summary(the.beatles.songs)

# Examine the distribution of the two values of the output variable (which is a factor) more precisely:
# table(<dataset>$<output variable>)
# prop.table(table(<dataset>$<output variable>))
# round(prop.table(table(<dataset>$<output variable>)), digits = 2)
table(the.beatles.songs$Top.50)
prop.table(table(the.beatles.songs$Top.50))
round(prop.table(table(the.beatles.songs$Top.50)), digits = 2)

# Split the dataset into train and test sets:
# install.packages("caret")
# library(caret)
# set.seed(<n>)
# <train dataset indices> <-                            # stratified partitioning: 
#     createDataPartition(<dataset>$<output variable>,  # the same distribution of the output variable in both sets
#                         p = .80,                      # 80/20% of data in train/test sets
#                         list = FALSE)                 # don't make a list of results, make a matrix
# <train dataset> <- <dataset>[<train dataset indices>, ]
# <test dataset>  <- <dataset>[-<train dataset indices>, ]
library(caret)
set.seed(333)
train.data.indices <- createDataPartition(the.beatles.songs$Top.50, p = 0.80, list = FALSE)
train.data <- the.beatles.songs[train.data.indices, ]
test.data <- the.beatles.songs[-train.data.indices, ]

# Build the model / decision tree:
# install.packages("rpart")
# library(rpart)
# <decision tree> <- rpart(<output variable> ~                                     # build the tree
#                          <predictor variable 1> + <predictor variable 2> + ...,  # . to include all variables
#                          data = <train dataset>,
#                          method = "class")                                       # build classification tree
library(rpart)
top.50.tree <- rpart(Top.50 ~ Single.certification + Covered.by + Year, 
                     data = train.data,
                     method = "class")

# Depict the model:
# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
# library(rattle)
# library(rpart.plot)
# library(RColorBrewer)
# fancyRpartPlot(<decision tree>)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(top.50.tree)

# Make predictions:
# <predictions> <- predict(object = <decision tree>, 
#                          newdata = <test dataset>, 
#                          type = "class")
# <predictions>[<i1>:<ik>]                            # examine some of the predictions
# <predictions dataframe> <- 
#       data.frame(<observation ID> = <test dataset>$<observation ID column>, 
#                  <another relevant feature> = <test dataset>$<another relevant feature column>, 
#                  ..., 
#                  <predictions feature> = <predictions>)
# View(<predictions dataframe>)
top.50.predictions <- predict(top.50.tree, newdata = test.data, type = "class")
top.50.predictions[1:20]
top.50.predictions.dataframe <- data.frame(Song = test.data$Title, 
                                           Top.50.Billboard = test.data$Top.50.Billboard, 
                                           Top.50 = test.data$Top.50, 
                                           Prediction = top.50.predictions)
View(top.50.predictions.dataframe)


########################
# Clustering - K-Means #
########################

# Reading the dataset:
# saveRDS(the.beatles.songs, "The Beatles songs dataset, v2.3.RData")     # saved earlier
the.beatles.songs <- readRDS("The Beatles songs dataset, v2.3.RData")
summary(the.beatles.songs)

# Changing the row names, in order to focus on numeric variables only (for K-Means)
rownames(the.beatles.songs) <- the.beatles.songs$Title
the.beatles.songs$Title <- NULL                                     # no longer necessary

# See if there are some patterns in the data, pairwise, to possibly indicate clusters:
# pairs(~ <column 1 name> + <column 2 name> + ..., 
#       data = <dataframe>)
pairs(~ Duration + Other.releases + Covered.by + Top.50.Billboard,  # no any striking pattern, i.e. 
      the.beatles.songs)                                            # no visual indication of clusters

# Try K-Means with 2 variables 

# Plot the data first:
# <scatterplot> <- 
#   ggplot(<dataset>, aes(x = <num.var.1>, y = <num.var.2>)) +
#     geom_point(shape = <n>,         # <n> = 1: hollow circle, no fill; <n> = 21: circle that can be filled
#                fill = <color 1>,    # color of point fill (optional)
#                color = <color 2>,   # color of point line (optional)
#                size = <s>)          # size  of point line (optional)
# <scatterplot> <- <scatterplot> + xlab("<x label>")                    # label/caption on x-axis
# <scatterplot> <- <scatterplot> + ylab("<y label>")                    # label/caption on x-axis
# <scatterplot> <- <scatterplot> + ggtitle("<scatterplot title>")       # scatterplot title
scatterplot1 <- ggplot(the.beatles.songs, aes(x = Other.releases, y = Covered.by))
scatterplot1 <- scatterplot1 + geom_point(shape = 21, fill = "yellow", size = 2)
scatterplot1 <- scatterplot1 + xlab("Other.releases")
scatterplot1 <- scatterplot1 + ylab("Covered.by")
scatterplot1 <- scatterplot1 + ggtitle("Covered.by vs. Other.releases")
scatterplot1

# Subset the original data to include only the variables to be used in K-Means:
# <new dataframe> <- subset(<dataframe>[, c("<col1 name>", "<col2 name>")])
# <new dataframe> <- subset(<dataframe>[, <col1 index>:<col2 index>)])
the.beatles.songs.2 <- subset(the.beatles.songs[, c("Other.releases", "Covered.by")])
summary(the.beatles.songs.2)
head(the.beatles.songs.2)

# Data normalization: required by K-Means when the variables have different ranges. 
# range(<dataframe>$<variable>)
# install.packages("clusterSim")
# library(clusterSim)
# <dataframe with numeric columns> <-                       # works with vectors and matrices as well
#   data.Normalization(<dataframe with numeric columns>,
#                      type = "n4",                         # normalization: (x - min(x)) / (max(x) - min(x))
#                      normalization = "column")            # normalization by columns
range(the.beatles.songs.2$Other.releases)
range(the.beatles.songs.2$Covered.by)
library(clusterSim)
the.beatles.songs.2 <- data.Normalization(the.beatles.songs.2, type = "n4", normalization = "column")
tail(the.beatles.songs.2)

# Run K-Means for K = 3:
# set.seed(<seed>)
# <clusters> <- kmeans(x = <normalized dataframe>, 
#                      centers = 3,                         # K = 3
#                      iter.max = 20,                       # max number of iterations allowed
#                      nstart = 1000)                       # no. of initial configurations (report on the best one)
# <clusters>
set.seed(888)
clusters.K3 <- kmeans(x = the.beatles.songs.2, centers = 3, iter.max = 20, nstart = 1000)
clusters.K3

# Add the vector of clusters to the dataframe:
# <normalized dataframe>$<new column> <- factor(<clusters>$cluster)   # <clusters>: from the previous step
# head(<normalized dataframe>)
the.beatles.songs.2$Cluster <- factor(clusters.K3$cluster)
head(the.beatles.songs.2)

# Plot the clusters in a new scatterplot:
# <scatterplot> <- 
#   ggplot(<dataset with the cluster column>, 
#          aes(x = <num.var.1>, y = <num.var.2>, 
#              color = <cluster column>)) +       # color clusters differently
# <scatterplot> <- <scatterplot> + geom_point()   # fill colors can be added subsequently, see below
# <scatterplot> <- <scatterplot> + xlab("<x label>")                  # label/caption on x-axis
# <scatterplot> <- <scatterplot> + ylab("<y label>")                  # label/caption on x-axis
# <scatterplot> <- <scatterplot> + ggtitle("<scatterplot title>")     # scatterplot title
# <scatterplot> <- <scatterplot> + 
#   scale_fill_brewer(palette = "Set1",           # palettes: http://ggplot2.tidyverse.org/reference/scale_brewer.html
#                     name = "<cluster column>")  # legend title
# <scatterplot> <- <scatterplot> + theme_bw()     # white background
# <scatterplot> <- <scatterplot> +                # add cluster centers
#   geom_point(data =                             # "data = " MUST be here, otherwise it doesn't work!
#              as.data.frame(<clusters>$centers), # <clusters>: from the previous step
#              color = "<line color>",
#              fill = "<fill color>",
#              size = <size>,                     # frequently used <size>s: 3, 4
#              shape = <shape>)                   # diamond: 23; triangle: 24; circle: 21; ...
scatterplot2 <- ggplot(the.beatles.songs.2, 
                       aes(x = Other.releases, y = Covered.by, 
                           colour = Cluster))
scatterplot2 <- scatterplot2 + geom_point(size = 2)
scatterplot2 <- scatterplot2 + xlab("Other.releases")
scatterplot2 <- scatterplot2 + ylab("Covered.by")
scatterplot2 <- scatterplot2 + ggtitle("Clusters")
scatterplot2 <- scatterplot2 + 
  scale_fill_brewer(palette = "Set1", name = "Cluster")
scatterplot2 <- scatterplot2 + theme_bw()
scatterplot2
scatterplot2 + geom_point(data = as.data.frame(clusters.K3$centers),  # add cluster centers
             color = "black",
             fill = "black",
             size = 4, 
             shape = 24)

  
###################################
# Resources, readings, references #
###################################

# The corrplot package: https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html


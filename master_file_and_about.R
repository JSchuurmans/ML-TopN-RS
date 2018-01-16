####################################
# Final assignment Machine Learning
####################################
#
# Author: Jetze Schuurmans
#         Stud.nr: 359244
# 1. Make sure working directory is set correctly
# 2. Check if following files and directories exist:
#       Data/ml-1m/ratings.dat
#       Data/ml-1m/users.dat
#       Data/ml-20m/movies.csv
#       ML2017Indiv_data_final1.R
#       ML2017Indiv_cluster_final.R
#       ML2017Indiv_data_final2.R
#       ML2017Indiv_rf_final.R
#       ML2017Indiv_svm_final.R
# 3. If R does not have enought memory:
#       use gc() (does not work always, sometime R needs to be restarted to empty Cache)
#       check if R is running 64bit version
# 4. Seed is set at each R script such that the order in which R scripts are executed are not important
#       e.g. SVM can be runned before RF 

# Create first part of data for unsupervised learning
source('data1.R')

# Apply unsupervised Clustering
source('cluster_analysis.R')

# Create second part of data for supervised learning
source('data2.R')

# Apply Random Forest
source('random_forest.R')

# Apply SVM
source('svm_svr.R')

# DBpedia
# under construction 
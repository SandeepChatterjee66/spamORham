##Fetching the data
filepath<-"C:\\Users\\hp\\Documents\\R\\Datasets\\tennis.csv"
tennis_data <- read.csv(filepath,header=TRUE)
#if(!is.null(tennis_data)) print(tennis_data) else print("error reading the data")


#utility functions
P <- function(A, colA, B, colB){
  #Probability of A given B
  p<-length(which(colA==A & colB==B))/length(which(colB==B))
  if(p==0) cat('\nWarning! this is zero\n')
  return(p)
}

Pr <- function(E, col){
  #Probability of happening single event E
  return(length(which(col==E))/nrow(col))
}

#-----------------------------------
#   Naive Bayes Classifier
#-----------------------------------

#Part - 1 of 2
#Proportional probablity of one class
ProbClass_i <-function(conditions, data, class_i){
  #Probabilty of class_i given the conditions
  
  #Data of class to be predicted
  classCol = data[length(data)]
  
  Prb_Class_i = Pr(class_i, classCol)
  cat("(Simple) Probablity of ",class_i,
      " : ", Prb_Class_i, '\n')
  
  for(i in 1:length(conditions)){
    featureVal = conditions[i];
    featureCol = data[i];
    P_featVal_given_class_i = P(featureVal,featureCol,class_i,classCol)
    cat("Probablity of ",featureVal," given ",class_i," : ",
        P_featVal_given_class_i, '\n')
    Prb_Class_i = Prb_Class_i * P_featVal_given_class_i
  }
    
    cat("Proportional Probablity of ",class_i,
        " given conditions :",Prb_Class_i, '\n\n')
    return(Prb_Class_i)
  }



#Part - 2 of 2
#Actual Probablities of all the classes
predict_nvbayes<-function(conditions, data){
  #Naive Bayes Classifier
  #returns probablity of each class based on the conditions
  
  classes = c(unique(data[length(data)])[[1]])
  n = length(classes)
  
  Proportional_probls = numeric(n)
  Probablities        = numeric(n)
  
  for(i in 1:n){
    PrbClass_i = ProbClass_i(conditions, data, classes[i])
    Proportional_probls[i] = PrbClass_i
  }
  
  for( i in 1:n){
    Probablities[i] = Proportional_probls[i]/sum(Proportional_probls)
  }
  
  Predictions = data.frame(classes,Probablities)
  
  return(Predictions)
}
#-----------------------------------
#   The Function Ends here
#-----------------------------------

##------------------usage ----------

# driver
conditions <- c("rainy","mild","normal","weak")
#conditions <- c("sunny","cool","high","weak")
Predictions <- predict_nvbayes(conditions, tennis_data)
print(Predictions)
#View(Predictions)
dev.new()
pie(Predictions[,2], labels=Predictions[,1], main="Predicted probab. of each class")

conditions <- c("sunny","hot","high","strong")
Predictions <- predict_nvbayes(conditions, tennis_data)
print(Predictions)
#View(Predictions)
dev.new()
pie(Predictions[,2], labels=Predictions[,1], main="Predicted probab. of each class")

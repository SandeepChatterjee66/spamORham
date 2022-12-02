##Fetching the data
spampath<-"C:\\Users\\hp\\Documents\\R\\Datasets\\spam_mail.csv"
 hampath<-"C:\\Users\\hp\\Documents\\R\\Datasets\\ham_mail.csv"

#utility functions
#Function to process mail
processMail<-function(mailpath){
  string<-paste(readLines(newmailpath), collapse=" ")
  string<-gsub('[^A-Za-z ]','',tolower(string))
  string<-gsub('[ ]+',' ',string)
  v <-c(strsplit(string," ")[[1]])
  return (v)
}

#Function to calculate probablity
P <- function(A, colA, B, colB){
  p=0
  if(missing(B)) p<-(length(colA[colA==A])/nrow(colA))
    #Probability of happening A , P(A)
  else p<-length(which(colA==A & colB==B))/length(colB[colB==B])
    #Probability of A given B,  P(A|B)
  #if(p==0) cat('Warning! this is zero\n')
  return(p)
}

#-----------------------------------
#   Naive Bayes Classifier
#-----------------------------------

#Part - 1 of 2
#Proportional probability of one class
ProbClass_i <-function(conditions, data, class_i,classColAt1=FALSE){
  #Probabilty of class_i given the conditions
  
  #Data of class to be predicted
  classCol = data[length(data)]
  cols = 1:length(conditions)
  if(classColAt1){classCol = data[1]; cols=2:length(conditions)}
  
  alpha = 1
  n     = nrow(classCol)
  k     = length(data)
  
  Prb_Class_i = rep(P(class_i,classCol),2)
  cat("(Prior) Probablity of ",class_i,
      " : ", Prb_Class_i[1], '\n')
  Prb_Class_i=log(Prb_Class_i)

  for(i in cols){
    featureVal = conditions[i];
    featureCol = data[i];
    if(classColAt1) featureCol = data[i+1]
    
    P_featVal_giv_cl_i = P(featureVal,featureCol,class_i,classCol)
    Sm_ftVal_giv_cl_i  = (P_featVal_giv_cl_i*n + alpha)/(n + k*alpha)
      
    #cat("Probablity of ",featureVal," given ",class_i," : ",
    #     P_featVal_giv_cl_i, '\n')
    Prb_Class_i[1] = Prb_Class_i[1] + log(P_featVal_giv_cl_i)
    Prb_Class_i[2] = Prb_Class_i[2] + log(Sm_ftVal_giv_cl_i )
  }
  Prb_Class_i = exp(Prb_Class_i)
    
    cat("Proportional Prb. of ",class_i,
        " given condns :",Prb_Class_i[1], '\n')
    cat("Smooth Prop. Prb. of ",class_i,
        " given condns :",Prb_Class_i[2], '\n\n')
    return(Prb_Class_i)
  }



#Part - 2 of 2
#Actual Probablities of all the classes
predict_nvbayes<-function(conditions, data, classColAt1=FALSE){
  #Naive Bayes Classifier
  #returns probablity of each class based on the conditions
  
  classCol = data[length(data)]
  if(classColAt1) classCol = data[1]
  classes = c(unique(classCol)[[1]])
  
  n = length(classes)
  
  Proportional_probls = numeric(n)
  Sm_Proportio_probls = numeric(n)
  Probablities        = numeric(n)
  Laplace_smoothing   = FALSE
  
  for(i in 1:n){
    PrbClass_i=ProbClass_i(conditions, data, classes[i],classColAt1)
    if(PrbClass_i[1]==0) Laplace_smoothing = TRUE
    Proportional_probls[i] = PrbClass_i[1]
    Sm_Proportio_probls[i] = PrbClass_i[2]
  }
  if(Laplace_smoothing){Proportional_probls = Sm_Proportio_probls
                        cat("Used Laplace Smoothing!\n")}
  for( i in 1:n){
    Probablities[i] = Proportional_probls[i]/sum(Proportional_probls)
  }
  Predictions = data.frame(classes,Probablities)
  
  return(Predictions)
}
#-----------------------------------
#   Spam Filter
#-----------------------------------
spamfilter<-function(newmailpath,spampath,hampath){
  spam_data <- read.csv(spampath,header=FALSE)
   ham_data <- read.csv( hampath,header=FALSE)
   new_mail <- processMail(newmailpath)
  View(spam_data)
  View(ham_data)
  spam_data<-cbind(spam_data,rep("spam",nrow(spam_data)))
   ham_data<-cbind( ham_data,rep( "ham",nrow( ham_data)))
  names(spam_data)<-names(ham_data)
  mail_data<-rbind(spam_data,ham_data)
  colnames(mail_data)<-c("word","isSpam")
  
  Predictions<- predict_nvbayes(new_mail,mail_data)
  
  return(Predictions)
}


##------------------usage ----------
# driver
newmailpath<-"C:\\Users\\hp\\Documents\\R\\ML\\mail.txt"

Predictions <- spamfilter(newmailpath,spampath,hampath)
print(Predictions)
dev.new()
pie(Predictions[,2],
    labels=Predictions[,1], main="Predicted probab. of each class")

newmailpath<-"C:\\Users\\hp\\Documents\\R\\ML\\mail.txt"
spampath<-"C:\\Users\\hp\\Documents\\R\\Datasets\\spam_mail.csv"
hampath<-"C:\\Users\\hp\\Documents\\R\\Datasets\\ham_mail.csv"

processMail<-function(mailpath){
  string<-paste(readLines(mailpath), collapse=" ")
  string<-gsub('[^A-Za-z ]','',tolower(string))
  string<-gsub('[ ]+','\n',string)
  write.csv(string,mailpath)
  #v <-c(strsplit(string," ")[[1]])
  #return (v)
}

new_mail <- processMail(hampath)
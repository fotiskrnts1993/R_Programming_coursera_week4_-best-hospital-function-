best<-function(state,outcome){
        hospitals<-read.csv("outcome-of-care-measures.csv")
        
        #Keep the columns that I want to use
        hospitals<-hospitals[,c(2,7,13,19,25)]
        
        #giving them names
        colnames(hospitals)<-c('Hospital Name', 'State', 'Heart Attack', 'Heart Failure', 'Pneumonia')
        
        #Create a subset with the states of the data frame
        hospitalssub<-subset(hospitals, State==state)
        
        
        #transform the columns from char to numeric
        hospitalssub<-transform(hospitalssub, 'Heart Attack'=as.numeric(hospitalssub$'Heart Attack'),
                           'Heart Failure'=as.numeric(hospitalssub$'Heart Failure'),
                           'Pneumonia'=as.numeric(hospitalssub$Pneumonia))
        
        
        #Find the minimums of columns: 'Heart Attack','Heart Failure','Pneumonia'
        
        if(outcome=="Heart Attack"){
                xrow<-which(hospitalssub[,3]==min(hospitalssub[,3], na.rm = TRUE))
        }
        else if(outcome=="Heart Failure"){
                xrow<-which(hospitalssub[,4]==min(hospitalssub[,4], na.rm = TRUE))
        }
        else if(outcome=="Pneumonia"){
                xrow<-which(hospitalssub[,5]==min(hospitalssub[,5], na.rm = TRUE))
        }
        else{
                stop("Grageis malakies")
        }
        hospitalname<-hospitalssub[xrow,1]
        hospitalname
}
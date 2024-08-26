#create sample data set for testing
creditsneeded<-10
max100credits<-3 #for BS, 0 for BA
courses<-c("BIOL 001","BIOL 002","BIOL 003","BIOL 004","BIOL 005",
           "BIOL 006","BIOL 007","BIOL 008","CHEM 100","CHEM 101")
credits<-c(1,2,3,4,3,4,3,4,3,4)

data<-data.frame(courses,credits)
data

#calculate maxcourses
sorted<-sort(credits,decreasing=FALSE) #sort credits ascending
sums<-c(1:length(sorted)) #create vector to catch sums from loop
for (i in 1:length(sorted)) {
  temp1<-sum(sorted[1:i]) #sum recursively ascending
  sums[i] <- temp1 #store in sums
}
maxcourses<-sum(sums<=creditsneeded)+1

#calculate mincourses
sorted2<-sort(credits,decreasing=TRUE) #sort credits ascending
sums2<-c(1:length(sorted2)) #create vector to catch sums from loop
for (i in 1:length(sorted2)) {
  temp1<-sum(sorted2[1:i]) #sum recursively ascending
  sums2[i] <- temp1 #store in sums
}
mincourses<-sum(sums2<=creditsneeded)+1

#combinations
combo1<-combn(data$courses,mincourses, simplify=TRUE) #set up matrix to catch combos (first included)
tcombo1<-t(combo1) #transpose so column is courses and rows are combos
combinations<-tcombo1 #store combos in combinations
for (i in (mincourses+1):maxcourses) { 
  comboi<-combn(data$courses,i, simplify=TRUE) #the rest of the combos
  tcomboi<-t(comboi) #transpose
  combinations<-cbind(combinations,c(NA)) #add column of NA for new courses
  combinations<-rbind(combinations,tcomboi) #store combos in combinations
}

#lookup credits for each course
library(lookup)
creditspercombo<-lookup(combinations[1,],data[,1],data[,2]) #create vector to catch for loop (first one included)
for (i in 2:nrow(combinations)) {
  combocredit<-lookup(combinations[i,],data[,1],data[,2]) #lookup credit number for each course in a combo
  creditspercombo<-rbind(creditspercombo,as.numeric(combocredit)) #insert into vector
}

#sum credits for each course
creditsums<-as.numeric(rowSums(creditspercombo,na.rm=TRUE))

#collate all the data
combinationlabels<-c(1:nrow(combinations)) #create combo numbers
collateddata<-data.frame(combinationlabels,
                         combinations,
                         creditspercombo,
                         creditsums) #merge as dataframe
colnames(collateddata)<-c("Combination",
                          paste0("Course",1:maxcourses),
                          paste0("Course",1:maxcourses,"Credits"),
                          "CreditsSum") #label the columns with variable number of courses
rownames(collateddata)<-NULL #remove artifact row names

#remove combos with
#1. excess 100 level credits based on max100credits
#2. not enough total credits (after excluding any extra 100 level credits)

library(lookup)
combocredit100<-lookup(combinations[1,],data[,1],data[,2]) #credits for each course in combo 
temp4<-grepl(". 1.",combinations[1,],data[,1],data[,2]) #logical whether each course in combo is 100 level
for (j in 1:length(combocredit100)) {
  if(temp4[j]==FALSE){
    combocredit100[j]<-NA} #for any course in combo that is 100 level, change that course's credits to NA
}
creditspercombo100<-as.numeric(combocredit100) #create matrix to catch for loop (first one included)
for (i in 2:nrow(combinations)) {
  combocredit100<-lookup(combinations[i,],data[,1],data[,2]) #the rest of the combos 
  temp4<-grepl(". 1.",combinations[i,],data[,1],data[,2])
  for (j in 1:length(combocredit100)) {
    if(temp4[j]==FALSE){
      combocredit100[j]<-NA}
  }
  creditspercombo100<-rbind(creditspercombo100,as.numeric(combocredit100)) #insert into matrix
}
creditsums100<-rowSums(creditspercombo100,na.rm=TRUE) #sum of 100 level credits per combo
collateddata100<-data.frame("Credits100Course"=creditspercombo100,"CreditsSum100"=creditsums100) #collated data of only 100 level course - specific credits by course and sum
rownames(collateddata100)<-NULL #remove artifact row names
extra100credits<-collateddata100$CreditsSum100-max100credits #calculate number of 100 level credits that do not count towards elective total credits because they are over max allowed
adjustedtotalcredits<-collateddata$CreditsSum-pmax(extra100credits,c(rep_len(0,length(extra100credits)))) #remove extra 100 level credits from total elective credits, to be removed if less than total creditsneeded
credit100colrange<-1:maxcourses #set column range based on variable maxcourses
labeled100courses<-collateddata100$CreditsSum100-max100credits>collateddata100[,credit100colrange] #label (TRUE) 100 level courses with less credits than any extra 100 credits for removal
labeled100combos<-rowSums(labeled100courses,na.rm=TRUE) #label (0) rows (combos) to keep by counting any 100 courses labeled for removal above per row
collateddata100<-data.frame(collateddata,
                            "AdjustedTotalCredits"=adjustedtotalcredits,
                            "Labels100"=labeled100combos) #add to collateddata
collateddatafiltered100<-subset(collateddata100,
                                collateddata100$AdjustedTotalCredits>=creditsneeded &
                                  collateddata100$Labels100==0) #filter out 100 level

#keep only combos where all course's credits are more than any extra credits
creditcolrange<-(maxcourses+2):(2*maxcourses+1) #set column range based on variable maxcourses
filteredmax<-collateddatafiltered100
for (i in creditcolrange) {
  filteredmax<-subset(filteredmax,filteredmax$CreditsSum-creditsneeded<filteredmax[,i]
                      |is.na(filteredmax[,i])) #loop through filtering more each time without filtering out NAs, ignores negative remainders (does not filter if not enough credits)
}

#remove unnecessary columns (labels100 and creditssum)
combos<-filteredmax[,-c(maxcourses*2+2,maxcourses*2+4)]

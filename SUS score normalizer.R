#This program normalizes the SUS score provided in the file
#SUSsvar18och20.csv and also returns the mean, median, standard deviation,
#minimum and maximum values. The program should work equally well on any SUS
#file.

#It was written in 2022 by Nadia Tomperi, Alexander Winberg and Joakim Stewens.



#The first step is to open the required csv file and store it in the 
#basedata variable. Please note that the read.csv2 function is used as the
#file contains semicolon to separate the values. If the values were separated
#by a comma the read.csv function would be used instead.

basedata <- read.csv2(file.choose())


#The head function can be removed, we kept it as a way to quickly check
#if the file had been uploaded correctly.

head(basedata)


#Next we look for Not AVAILABLE values using the is.na function on the variable 
#basedata and replace them with 0. We know that there are no NA results in 
#the data, but it's good to implement it already if we want to use the program 
#on other csv files. The cat function prints the result, and only the result 
#instead of array[], [1] or similar results. Since there are no NA the printed
#number should be 0.

cat(basedata[is.na(basedata)] <- 0)


#The next step is to normalize the SUS score according to the following rules.
#Add up the total score for all odd-numbered questions, 
#then subtract 5 from the total to get (X).

#Add up the total score for all even-numbered questions, 
#then subtract the total score from 25 to get (Y).
#Add up the total score of the new values (X+Y) and multiply by 2.5.

#The first step is to collect the odd numbered question scores in the subset
#summedOdd. We used the rowSums function to do this as it allows us to add up
#all the required rows at once. The function chooses the columns in the right
#order by picking every other column (those who are TRUE).The same function 
#is then used on the even numbered questions, but here we start with FALSE since
#the first column is already used by Odd. The head function is again used as a
#way to check that the functions has worked properly.

#After that the calculations are performed according to the rules for SUS and
#the final result is stored in SUSScore.

#Please note that this will only work properly on SUS files with 10 questions.
#If there are more or less than 10 questions the program would need to be 
#modified.

basedata$summedOdd <- rowSums(basedata[ , c(TRUE,FALSE)])

basedata$summedEven <- rowSums(basedata[ , c(FALSE,TRUE)])

head(basedata)

basedata$OddSUS <- basedata$summedOdd - 5

basedata$evenSUS <- 25 - basedata$summedEven

basedata$SUSScore <- ((basedata$evenSUS + basedata$OddSUS)* 2.5)

#Two new vectors are created for the final part of the task. They will be used
#to calculate the required scores.We first use R's own functions to calculate
#the required values, for example mean(), and store it in a variable. Trialdata
#is used here but has no further use. Finaldata is used to store the values. 

trialdata <- vector()
finaldata <- vector()


var_medel <- mean(basedata$SUSScore)
finaldata.medel <-  append(trialdata, var_medel)


var_median <- median(basedata$SUSScore)
finaldata.median <- append(trialdata, var_median)


var_sd <- sd(basedata$SUSScore)
finaldata.sd <- append(trialdata, var_sd)


var_min <- min(basedata$SUSScore)
finaldata.mini <- append(trialdata, var_min)


var_max <- max(basedata$SUSScore)
finaldata.maxi <- append(trialdata, var_max)

#We have used three different options to provide the required results. Two of 
#the options are currently not in used, they are only included as a way to show
#different methods of getting the same result.

cat(var_medel, var_median, var_sd, var_min, var_max, sep="\n")

#cat(paste(finaldata.medel,finaldata.median, finaldata.sd,finaldata.mini,finaldata.maxi, sep="\n"))

#print(finaldata.median)
#print(finaldata.sd)
#print(finaldata.mini)
#print(finaldata.maxi)

save.image(file = "My_Object.RData")

#Hopefully this was useful to you. Feel free to use the code in your own 
#projects and have fun with R.

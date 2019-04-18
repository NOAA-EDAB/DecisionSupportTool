##Idiot-proof function to stack a wide dataframe into a long format, conserving the column names as a text string in a new column
# input variables: df - dataframe to be manipulated
#                   cols - the column numbers to be stacked
#                   fName - name for the column that will contain the old column headings
#                   vName - name for the column that will contain the data being stacked
# See example below code.

Wide2Long=function(df, cols, fName, vName, verbose=FALSE) {
IndexCol=1:ncol(df); IndexCol=IndexCol[-cols]; # define index columns of df
IndexNames=c(names(df)[IndexCol])
Index=data.frame(df[ ,IndexCol]); names(Index)[]=IndexNames; # extract and name index columns
IndexRows=nrow(Index); # calculate number of rows in Index

Output=data.frame(matrix(data=NA, nrow=length(cols)*IndexRows, ncol=length(IndexNames)+2))
  names(Output)=c(IndexNames, fName, vName)

col.fName=length(IndexNames)+1
col.vName=col.fName+1

for(i in 1:length(IndexNames) ) {
  Output[ ,i] = rep(Index[ ,i], length(cols))
  }

  for (i in 1:length(cols)) {
  if(verbose==TRUE) {print(paste(i, "/", length(cols)))}
    fI=rep(names(df)[(cols[i])],IndexRows)
    vI=df[ ,cols[i]];
    Output[((i-1)*IndexRows+1) : (i*IndexRows), col.fName]=fI;
    Output[((i-1)*IndexRows+1) : (i*IndexRows), col.vName]=vI;
    }
  
return(Output)
}

# example taken from reshape:
#  df <- data.frame(id=1:4, age=c(40,50,60,50), dose1=c(1,2,1,2), dose2=c(2,1,2,1), dose4=c(3,3,3,3)); df
#         id age dose1 dose2 dose4
#      1  1  40     1     2     3
#      2  2  50     2     1     3
#      3  3  60     1     2     3
#      4  4  50     2     1     3

#  Wide2Long(df, c(3:5), "Dose", "Dosage")
#         id age Dose   Dosage
#      1   1  40 dose1     1
#      2   2  50 dose1     2
#      3   3  60 dose1     1
#      4   4  50 dose1     2
#      5   1  40 dose2     2
#      6   2  50 dose2     1
#      7   3  60 dose2     2
#      8   4  50 dose2     1
#      9   1  40 dose4     3
#      10  2  50 dose4     3
#      11  3  60 dose4     3
#      12  4  50 dose4     3

##NOT-Idiot-proof function to unstack a long dataframe into a long format, conserving the test strings in the factor columns as the new column names
# input variables: df - dataframe to be manipulated
#                   fName - name for the column that contains the new column headings
#                   vName - name for the column that contains the data being unstacked
# See example below code.

# Function works only if all subsets have the same identification matrix.

Long2Wide=function(df, fName, vName, NullVal=NA, Prefix="") {
#  if(nargs()==3){Prefix=""}

  fCol=which(names(df)==fName);
  vCol=which(names(df)==vName);
  
  Names=names(df[-c(fCol, vCol)]);

  UfN=as.character(sort(unique(df[ ,fCol])))
  Key=data.frame(unique(df[ ,Names]));
    names(Key)=Names
  for(i in 1:ncol(Key)){
    if(i==1) {strKey=Key[ ,i]} else {strKey=paste(strKey, Key[ ,i], sep="_")}
    }
  Wide=data.frame(matrix(data=NullVal, ncol=ncol(Key)+length(UfN), nrow=nrow(Key)))
  names(Wide)=c(names(Key), paste(Prefix, UfN, sep=""))
  Wide[ ,1:ncol(Key)]=Key
  
  for (i in 1:length(UfN)){
    tempKey=data.frame(df[which(df[ ,fCol]==UfN[i]),Names]);
      names(tempKey)=Names
    for(j in 1:ncol(tempKey)){
      if(j==1) {strTempKey=tempKey[ ,j]} else {strTempKey=paste(strTempKey, tempKey[ ,j], sep="_")}
      }
    tempData=df[which(df[ ,fCol]==UfN[i]),vCol];

    Wide[ which(match(strKey, strTempKey)>0), i+ncol(Key)]=
      tempData[na.omit(match(strKey, strTempKey))]
    }


return(Wide)
}

# example taken from reshape:
#  dfLong <- data.frame(id=rep(1:4,3), age=c(40,50,60,50,40,50,60,50,40,50,60,50), Dose=rep(c("dose1", "dose2", "dose4"), each=4), Dosage=c(1,2,1,2,2,1,2,1,3,3,3,3)); dfLong
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

#  dfWide=Long2Wide(df, "Dose", "Dosage"); dfWide
#       id age dose1 dose2 dose4
#    1  1  40     1     2     3
#    2  2  50     2     1     3
#    3  3  60     1     2     3
#    4  4  50     2     1     3

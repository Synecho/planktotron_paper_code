SummarizeDataset = function(df, by, method = "sum"){
  print("Summarizing dataset...")
  S = names(df)
  for (i in 1:(length(S))){
    if (i == 1){
      print(paste(i, "of", length(S)))
      df.temp = data.frame(Class = by, Count = df[,i])
      names(df.temp) = c("Class", "Count")
      if(method == "sum"){df.s = summarise(group_by(df.temp, Class), Total = sum(Count))}
      if(method == "mean"){df.s = summarise(group_by(df.temp, Class), Mean = mean(Count))}
      names(df.s)[i+1] = S[i] 
    } 
    if (i > 1) {
      print(paste(i, "of", length(S)))
      df.temp = data.frame(Class = by, Count = df[,i])
      names(df.temp) = c("Class", "Count")
      if(method == "sum"){df.temp = summarise(group_by(df.temp, Class), Total = sum(Count))}
      if(method == "mean"){df.temp = summarise(group_by(df.temp, Class), Mean = mean(Count))}
      df.s = full_join(df.s, df.temp, by = "Class", .keep_all = T)
      names(df.s)[i+1] = S[i]
    }
    if (i == length(S)) {
      #df.s[is.na(df.s)] = 0
      print(paste("Done! :-)"))
      df.s = as.data.frame(df.s)
      df.s[,1] = as.character(df.s[,1])
      df.s[,1][is.na(df.s[,1])] = "unkown"
      rownames(df.s) = df.s[,1]
      df.s = df.s[,-1]
      return(df.s)
    }
  }
}

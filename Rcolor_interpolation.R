ColorVariationsSet = function(levels = NULL, ColSet = NULL, Colors = NULL, n = NULL){
  packages = as.data.frame(installed.packages())
  if (all(grepl("ggsci", packages$Package) == FALSE) == TRUE){
    install.packages("ggsci")
  }
  if (all(grepl("RColorBrewer", packages$Package) == FALSE) == TRUE){
    install.packages("RColorBrewer")
  }
  library(RColorBrewer)
  library(ggsci)
  
  table = table(levels)
  
  if(!is.null(ColSet)){
    if(ColSet == "Pastel1" | ColSet == "Set1" | ColSet == "Greens" | ColSet == "Reds" | ColSet == "Oranges" | ColSet == "Blues" | ColSet == "Purples" | ColSet == "PuBu" | ColSet == "Blues" | ColSet == "Greys"){
      length.ColSet = 9}
    if(ColSet == "Spectral" | ColSet == "YlOrRd" | ColSet == "PuOr" | ColSet == "RdGr" | ColSet == "RdYlGn"){
      length.ColSet = 11}
    if(ColSet == "Set2" | ColSet == "Pastel2" | ColSet == "Dark2" | ColSet == "Accent"){
      length.ColSet = 8}
    if(ColSet == "Set3" | ColSet == "Paired"){
      length.ColSet = 12}
    
    Cols = colorRampPalette(brewer.pal(n = length.ColSet, name = ColSet))(length(table))
  }else{
    if(is.null(Colors)){
      print("ERROR: Neither Color set nor custom colors provided.")
    }
    Cols = colorRampPalette(Colors)(length(table))
  }
  
  ColVec = NULL
  for (i in 1:length(table)){
    c = colorRampPalette(c("black", Cols[i], "white"))(table[i]+4)
    c = c[-c(1:2,(length(c)-1):length(c))]
    ColVec = c(ColVec, c) 
  }
  plot(1:length(levels), 1:length(levels), pch = 21, bg = ColVec, cex = 3, axes = F, ylab = "", xlab = "", main = "Color variations")
  return(data.frame(Levels = levels, Color = as.character(ColVec)))
}

ColorVariations = function(Color = NULL, n = 3, example = F, hue = "light"){
  if(hue == "both"){
    c = colorRampPalette(c("black", Color, "white"))(n+4)
    c = c[-c(1:2,(length(c)-1):length(c))]
  }
  if(hue == "light"){
    c = colorRampPalette(c(Color, "white"))(n*3)
    c = c[seq(from = 2, to = (n*3)-1, by = 3)]
  }
  if(hue == "dark"){
    c = colorRampPalette(c("black", Color))(n*3)
    c = c[seq(from = 2, to = (n*3)-1, by = 3)]
  }
  if(example == T){plot(1:n, 1:n, pch = 21, bg = c, cex = 3, axes = F, ylab = "", xlab = "", main = "Color variations")}
  return(as.character(c))
}



#tax.col$phylum <- change name to your dataframe name and column (Whether family level or class and so on) 
#This script should then take i.e phylum and assign it a certain color.
#every level below that will be assigned the same color but of decreasing alpha intensity. 
#Change pal_startrek("uniform")(7) to your color palette of choice and how many increments you need
#lev = length(levels(as.factor(ASV_sub_FL$Phylum)))
#cols = colorRampPalette(pal_startrek("uniform")(7))
#cols = cols(lev)
#mycolors = ColorVariationsSet(levels = ASV_sub_FL$Phylum, Colors = cols)

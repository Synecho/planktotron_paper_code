PlotLm = function(y = NULL, x = NULL, d = 1,
                  main = "", xlab = "Explanatory variable", ylab = "Response variable", type = "p", method = "lines", poly.col = "#66666688",
                  col = "black", bg = "black", l1.col = "red", l2.col = "black", pch = 21, cex = 1, las = 2, lty = 2, lwd = 2, zero.correction = F,
                  axes = TRUE, ylim = c(min(y)-(max(y)*0.1),max(y)+(max(y)*0.1)), xlim = NULL, mod.pos = "front", plot.result = NULL, add = FALSE, predict = 0.95){
  n = length(y)/length(unique(x))
  mod = lm(y ~ poly(x, d, raw = T))
  s.mod = summary(mod) 
  r = s.mod$adj.r.squared
  
  f = summary(mod)$fstatistic
  p = pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) = NULL
  
  if(p < 0.001){p = "< 0.001"}else{p = paste(" = ",round(p,3))}
  fit.mod = as.data.frame(predict(mod, interval = "conf", level = predict))
  fit.mod$f = x
  fit.mod = fit.mod[order(fit.mod$f),]
  if(add == FALSE){
    lm.plot = plot(y=y, x=x, type = "n", xlab = xlab, ylab = ylab, pch = pch, bg = bg, cex = cex, col = col, 
                   main = paste(main), axes = axes, ylim = ylim, xlim = xlim, las = las)
  }
  
  if(mod.pos == "front"){points(y=y, x=x, pch = pch, bg = bg, cex = cex, col = col)}
  if(is.numeric(predict)){
    if(method == "lines"){
      lines(fit.mod[,"lwr"] ~ fit.mod$f, col = l2.col, lty = 3, lwd = 2)
      lines(fit.mod[,"upr"] ~ fit.mod$f, col = l2.col, lty = 3, lwd = 2)
    }
    if(method == "poly"){
      polygon(x = c(fit.mod$f, rev(fit.mod$f)), c(fit.mod[,"upr"], rev(fit.mod[,"lwr"])), col = poly.col, border = NA)
    }
  }
  if(zero.correction == T){
    fit.mod[,"fit"][fit.mod[,"fit"] < 0] = 0
  }
  lines(fit.mod[,"fit"] ~ fit.mod$f, col = l1.col, lty = lty, lwd = 3)
  if(mod.pos == "back"){points(y=y, x=x, pch = pch, bg = bg, cex = cex, col = col)}
  if(mod.pos == "none"){}
  if(!is.null(plot.result)){
    text = paste(paste("adj.R2 = ", round(r,3),", ", sep = ""), paste("p.val ", p, sep = ""))
    legend(plot.result, bty = "n", legend = text)
    
  }
}
needed_packages = c("tidyverse", "mirt", "jtools")
for(i in 1:length(needed_packages)){
  haspackage = require(needed_packages[i], character.only = TRUE)
  if(haspackage == FALSE){
    install.packages(needed_packages[i])
  }
  library(needed_packages[i], character.only = TRUE)
}

inv_logit <- function(x){
  return(exp(x)/(1 + exp(x)))
}

pow <- function(x,y){
  return(x^y)
}

irf <- function(mirtmod, display.coefs = T, digits = 5){
  
  coefs.mirtmod <- coef(mirtmod, simplify = T)
  coefs.mirtmod <- as.data.frame(coefs.mirtmod)[,c(1,2)]
  
  coefs.mirtmod$difficulty <- -1*coefs.mirtmod$items.d/coefs.mirtmod$items.a1
  coefs.mirtmod = coefs.mirtmod[,c(1,3)]
  
  colnames(coefs.mirtmod) = c("Discrimination (a)", "Difficulty (b)")
  
  coefs.mirtmod <- as.data.frame(apply(coefs.mirtmod, 2, function(x)round(x, digits = digits)))
  
  coefs.mirtmod <<- coefs.mirtmod
  
  nObs = nrow(fscores(mirtmod))
  
  plist <- list()
  
  plot.theta <- seq(-6, 6, length.out = nObs)
  
  a.all = as.vector(coefs.mirtmod[,1])
  b.all = as.vector(coefs.mirtmod[,2])
  
  for(i in 1:nrow(coefs.mirtmod)){
    a = a.all[i]
    b = b.all[i]
    theta = plot.theta
    y = inv_logit(a*(theta-b))
    df <- data.frame(y = y, theta = theta)
    params <- data.frame(a = a, b = b)
    plist[[i]] <- ggplot()+
      geom_line(data = df, aes(x = theta, y = y), color = "blue")+
      geom_point(data = params, aes(y = .5, x = b, color = "Difficulty (b)"))+
      scale_color_manual(values = "orange")+
      xlim(-6,6)+
      scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = .25))+
      xlab("\u03b8 (Ability Measurement)")+
      ylab("Pr(Y = Correct | \u03b8)")+
      labs(title = paste0("Q ",i))+
      jtools::theme_apa()
  }
  
  plots <<- plist
  if(display.coefs){
    return(coefs.mirtmod)
  }
}

irf.display <- function(plotlist = plots, which.item = 1){
  stopifnot(is.numeric(which.item))
  stopifnot(is.list(plotlist))
  print(plotlist[[which.item]])
}

mirt.fscores <- function(mirtmod, digits = 5, visualize = T, SE.curve = F){
  f <- as.data.frame(fscores(mirtmod, full.scores.SE = T))
  f <- apply(f, 2, function(x)round(x, digits = digits))
  f <- as.data.frame(f)
  cn.f <- colnames(f)
  if(visualize){
    if(SE.curve){
      p <- ggplot(data = f)+
        geom_point(aes(x = F1, y =SE_F1), alpha = .25)+
        xlab("Estimated \u03b8")+
        ylab("SE of Measurement")+
        ylim(0, max(f$SE_F1)+1)+
        jtools::theme_apa()
      return(p)
    }
    else{
      f <- data.frame(F1 = f$F1, Info = 1/f$SE_F1)
      p <- ggplot(data = f)+
        geom_point(aes(x = F1, y = Info), alpha = .25)+
        xlab("Estimated \u03b8")+
        ylim(0, max(f$Info)+1)+
        ylab("Information")+
        jtools::theme_apa()
      return(p)
    }
  }
  colnames(f) <- c("\u03b8", "SE")
  rownames(f) <- paste0("Q", seq(1, nrow(f)))
  return(f)
}

loadresps <- function(path = NULL){
  if(is.null(path)){
    path <- paste0(getwd(), "/resps_examp_", sample(x = seq(1, 50), size = 1), ".csv")
  }
  respsdf <<- read.csv(path)
  lab1 <- gsub(paste0(getwd(), '/'), "", path)
  lab2 <- gsub(".csv", "", lab1)
  
  return(lab2)
}

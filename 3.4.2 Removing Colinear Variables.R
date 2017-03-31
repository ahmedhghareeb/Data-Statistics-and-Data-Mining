# See https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/
# http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis
require(MASS)
require(clusterGeneration)


#create  fifteen 'explanatory' variables with 200 
# observations each

set.seed(2)
num.vars<-15
num.obs<-200
cov.mat<-genPositiveDefMat(num.vars,covMethod="unifcorrmat")$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)
# 
# Now we create our response variable as a linear combination
# of the explanatory variables. First, we create a vector
# for the parameters describing the relationship of the 
# response variable with the explanatory variables. Then, 
# we use some matrix algebra and a randomly distributed 
# error term to create the response variable. This is the 
# standard form for a linear regression model.

parms<-runif(num.vars,-10,10)
y<-rand.vars %*% matrix(parms) + rnorm(num.obs,sd=20)

# We would expect a regression model to indicate each 
# of the fifteen explanatory variables are significantly 
# related to the response variable, since we know the true 
# relationship of y with each of the variables. However, 
# our explanatory variables are correlated. What happens 
# when we create the model?

lm.dat <-data.frame(y,rand.vars)
form.in<-paste('y ~',paste(names(lm.dat)[-1],collapse='+'))
mod1<-lm(form.in,data=lm.dat)
summary(mod1)

x <- rand.vars
# remove collinear variables and recreate lm
terms_to_keep <- vif_func(in_frame=x,thresh=5,trace=T)
form.in <-  paste('y ~',paste(terms_to_keep,collapse='+'))
mod2<-lm(form.in,data=lm.dat)
summary(mod2)


# vif_func ----------------------------------------------------------------


vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

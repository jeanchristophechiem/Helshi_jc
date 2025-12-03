#***************************************************************
#STAT-L5214
# 2023-2024
# jean-christophe chiem 
# https://myweb.uiowa.edu/pbreheny/7210/f15/notes/9-17.pdf
# https://iowabiostat.github.io/data-sets/anemia/anemia.html
#***************************************************************

#***************************************************************
#library call
#***************************************************************
rm(list=ls()) #clean environment
library(openxlsx)
library(survival)
library(haven)

#***************************************************************
#Read files
#***************************************************************
my_folder="/Users/jean-christophechiem/Documents/Helshi_jc/GVHD"
setwd(my_folder)
my_file="/Users/jean-christophechiem/Documents/Helshi_jc/GVHD/data/GVHD.Rda"
load(my_file) #chare le fichier qui contient une variable qui s'appelle a avec les données
#renommer la variable a
GVHD=a 
#***************************************************************
#Survival Analysis
#***************************************************************
Fit <- survfit(Surv(Time, Status)~ Trt, data= GVHD)
print(Fit) 

#dev.off()
#windows()
plot(Fit, xlab=c("Temps de survie"), 
     ylab=c("Probabilité de survie"), 
     lty=c(1, 2), 
     col=c("black", "red"))

legend("topright", legend=c("MTX","MTX+CSP"), 
       lty=c(1, 2), 
       col= c("black", "red"))  

surv_diff <-survdiff(Surv(Time, Status)~ Trt, data= GVHD)
print(surv_diff)


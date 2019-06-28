#1) download the gitlab repository from the website below, copy it to a folder of interest and unzip
#   https://gitlabce.statwb.eu.novartis.net/STEINANF/xgx_Rpackage/
#
#2) build the package using:
#   devtools::build("xgx_Rpackage-master")
#
#3) install the package using:
#   install.packages("xgx_Rpackage-master",repos=NULL,lib="./",type="source")
#
#4) load the library
#   library(xgx,lib.loc = "./")

#header ----
  #to run the first time you use it
  devtools::build("xgx_Rpackage-master")
  install.packages("xgx_Rpackage-master",repos=NULL,lib="./",type="source")
  
  #to run every time you use it
  library(xgx,lib.loc = "./")
  
#example code ----  
  library(ggplot2)
  ggplot(data=data.frame(x=rlnorm(1000,0,1),y=rlnorm(1000,0,3)),aes(x=x,y=y)) + 
    geom_point() + 
    xgx_scale_x_log10() + 
    xgx_scale_y_log10()

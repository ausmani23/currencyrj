#we take progressive tax rates from the structure of income taxation in the USA..
#https://www.nytimes.com/2019/10/11/opinion/sunday/wealth-income-tax-rate.html

#if they are flat, this is easy

getTaxRates<-function(q,type=c('flat','progressive')) {
  
  if(type=='flat') {
    
    0.05
    
  } else {
    #q is the quantile a person is in
    if(q<=10) {
      0.256 
    } else if(q>10 & q<=20) {
      0.242 
    } else if(q>20 & q<=30) {
      0.245 
    } else if(q>20 & q<=30) {
      0.235
    } else if(q>30 & q<=40) {
      0.242 
    } else if(q>40 & q<=50) {
      0.254
    } else if(q>50 & q<=60) {
      0.263
    } else if(q>60 & q<=70) {
      0.278
    } else if(q>70 & q<=80) {
      0.294
    } else if(q>80 & q<=90) {
      0.286
    } else if(q>90 & q<=95) {
      0.277
    } else if(q>95 & q<=99) {
      0.289
    } else if(q>99 & q<=99.9) {
      0.332
    } else if(q>99.9 & q<=99.99) {
      0.304
    } else if(q>99.99) {
      0.230
    }
  }

}
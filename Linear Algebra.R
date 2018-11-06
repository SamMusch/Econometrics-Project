library(pracma)

### Coef is the coefficient matrix
### d is the solutions alone
### E is the augmented matrix (Coef and d combined)
### I is the idenity matrix (7x7)

### new. is problem 14 where I have updated demand




################### This is the matrix from Problem #13 #####################################
Coef = matrix(c(.1588, .0064, .0025, .0304, .0014, .0083, .1594,
             .0057, .2645, .0436, .0099, .0083, .0201, .3413,
             .0264, .1506, .3557, .0139, .0142, .0070, .0236,
             .3299, .0565, .0495, .3636, .0204, .0483, .0649,
             .0089, .0081, .0333, .0295, .3412, .0237, .0020,
             .1190, .0901, .0996, .1260, .1722, .2368, .3369,
             .0063, .0126, .0196, .0098, .0064, .0132, .0012),nrow=7,ncol=7,byrow=TRUE)
d = matrix(c(74000,56000,10500,25000,17500,196000,5000),nrow=7,ncol=1,byrow = TRUE)
d

I = diag(nrow = 7,ncol = 7)
I
E <- I - Coef
E
C <- cbind(E,d)
C

firstref <- rref(C)
firstref
first <- firstref[,8]





########### Problem 14 with new demand

new.Coef = matrix(c(.1588, .0064, .0025, .0304, .0014, .0083, .1594,
                .0057, .2645, .0436, .0099, .0083, .0201, .3413,
                .0264, .1506, .3557, .0139, .0142, .0070, .0236,
                .3299, .0565, .0495, .3636, .0204, .0483, .0649,
                .0089, .0081, .0333, .0295, .3412, .0237, .0020,
                .1190, .0901, .0996, .1260, .1722, .2368, .3369,
                .0063, .0126, .0196, .0098, .0064, .0132, .0012),nrow=7,ncol=7,byrow=TRUE)
new.d = matrix(c(99640,75548,14444,33501,23527,263985,6526),nrow=7,ncol=1,byrow = TRUE)
new.d

new.I = diag(nrow = 7,ncol = 7)
new.I
new.E <- new.I - new.Coef
new.E
new.C <- cbind(new.E,new.d)
new.C
second <- new.C[,7]
rref(new.C)
secondref <- rref(new.C)
secondref
second <- secondref[,8]








### Part C
A <- (I + Coef 
+ (Coef*Coef) 
+ (Coef*Coef*Coef) 
+ (Coef*Coef*Coef*Coef) 
+ (Coef*Coef*Coef*Coef*Coef) 
+ (Coef*Coef*Coef*Coef*Coef*Coef) 
+ (Coef*Coef*Coef*Coef*Coef*Coef*Coef) 
+ (Coef*Coef*Coef*Coef*Coef*Coef*Coef*Coef) 
+ (Coef*Coef*Coef*Coef*Coef*Coef*Coef*Coef*Coef) 
+ (Coef*Coef*Coef*Coef*Coef*Coef*Coef*Coef*Coef*Coef))


third <- A %*% d
third


### Part C
Z <- (I + Coef 
      + (Coef*Coef) 
      + (Coef*Coef*Coef) 
      + (Coef*Coef*Coef*Coef) 
      + (Coef*Coef*Coef*Coef*Coef)) 
fourth <- Z %*% d
fourth


cbind(first,second,fourth,third)



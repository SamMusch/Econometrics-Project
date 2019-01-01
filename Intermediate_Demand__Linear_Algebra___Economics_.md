Intermediate Demand (Linear Algebra & Economics)
================
Sam Musch

Problem:
Production and consumption are intimately related. Imagine that we divide an economy into sectors. Walking into a grocery store and purchasing ears of corn creates demand on:
- the agricultural sector which much grow the corn
- the transportation sector to move the corn - the services provided by the grocery store
- the energy sector to transport it
- the energy sector to keep it cold.

To solve this problem, we will use the following information. The columns of our matrix are as follows:
- (1) Nonmetal household and personal products
- (2) Final metal products (eg cars)
- (3) Basic metal products and mining
- (4) Basic nonmetal products and agriculture
- (5) Energy
- (6) Services
- (7) Entertainment & misc products
- (8) Solution matrix - final demand

``` r
library(pracma)
```

    ## Warning: package 'pracma' was built under R version 3.5.2

``` r
Coef = matrix(c(.1588, .0064, .0025, .0304, .0014, .0083, .1594,
                .0057, .2645, .0436, .0099, .0083, .0201, .3413,
                .0264, .1506, .3557, .0139, .0142, .0070, .0236,
                .3299, .0565, .0495, .3636, .0204, .0483, .0649,
                .0089, .0081, .0333, .0295, .3412, .0237, .0020,
                .1190, .0901, .0996, .1260, .1722, .2368, .3369,
                .0063, .0126, .0196, .0098, .0064, .0132, .0012),nrow=7,ncol=7,byrow=TRUE)  
  
d = matrix(c(74000,56000,10500,25000,17500,196000,5000),nrow=7,ncol=1,byrow = TRUE)
```

Now that we have our data and package loaded, we can start.

Question \#1: From what other sector does "nonmetal household and personal products" demand the most?

Step 1: Create dentity matrix. We will use this matrix to help us solve the problem.

``` r
I = diag(nrow = 7,ncol = 7)
I
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
    ## [1,]    1    0    0    0    0    0    0
    ## [2,]    0    1    0    0    0    0    0
    ## [3,]    0    0    1    0    0    0    0
    ## [4,]    0    0    0    1    0    0    0
    ## [5,]    0    0    0    0    1    0    0
    ## [6,]    0    0    0    0    0    1    0
    ## [7,]    0    0    0    0    0    0    1

Step 2: Identity matrix - our coefficient matrix. This tells us how much each sector depends on the others. In this case, we want to know which sector "nonmetal household and personal products" (column 1) depends on the most.

The value of -.3299 (column 1, row 4) tells us that 33 units would be demanded of the "basic nonmetal products and agriculture" industry in order for the "nonmetal household and personal products" industry to produce 100 units.

Looking at column 1, row 4 below

``` r
E <- I - Coef
E
```

    ##         [,1]    [,2]    [,3]    [,4]    [,5]    [,6]    [,7]
    ## [1,]  0.8412 -0.0064 -0.0025 -0.0304 -0.0014 -0.0083 -0.1594
    ## [2,] -0.0057  0.7355 -0.0436 -0.0099 -0.0083 -0.0201 -0.3413
    ## [3,] -0.0264 -0.1506  0.6443 -0.0139 -0.0142 -0.0070 -0.0236
    ## [4,] -0.3299 -0.0565 -0.0495  0.6364 -0.0204 -0.0483 -0.0649
    ## [5,] -0.0089 -0.0081 -0.0333 -0.0295  0.6588 -0.0237 -0.0020
    ## [6,] -0.1190 -0.0901 -0.0996 -0.1260 -0.1722  0.7632 -0.3369
    ## [7,] -0.0063 -0.0126 -0.0196 -0.0098 -0.0064 -0.0132  0.9988

Now we need to take the coefficient values from Step 2 and set them equal to our solution matrix. Column 8 from the table below shows us how much each sector will need to produce in order to satisfy demand.

``` r
C <- cbind(E,d)
firstref <- rref(C)
firstref
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7]      [,8]
    ## [1,]    1    0    0    0    0    0    0  99575.65
    ## [2,]    0    1    0    0    0    0    0  97703.02
    ## [3,]    0    0    1    0    0    0    0  51230.52
    ## [4,]    0    0    0    1    0    0    0 131569.92
    ## [5,]    0    0    0    0    1    0    0  49488.49
    ## [6,]    0    0    0    0    0    1    0 329554.45
    ## [7,]    0    0    0    0    0    0    1  13835.34

The values in column 8 represent how much each sector will need to produce.

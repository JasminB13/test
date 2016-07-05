library(igraph)

# Assign value to an object
y <- 3

# remove/delete object
rm(y)

2==2 #Equality
2!=2 #Inequality

x <= y # less than or equal

### Special constants ###
# NA - for missing or undefined data -> check if missing with "is.na()"
# NULL - empy object -> check if empty with "is.null()"
# Inf and -Inf - positive and negativ infinity
# NaN - results that cannot be reasonable defined

### Vectors ###
# can be constructed by combining their elements with c()

v1 <- c(1, 5, 11, 33) # Numeric
v2 <- c("hello","world") # character vector
v3 <- c(TRUE, TRUE, FALSE) # logical vector

# Combining different types of elements in one vector will coerce the lements to the least
v4 <- c(v1, v2, v3, "boo") # all become strings!

# Other ways to create vectors
v <- 1:7
v <- rep(0,77) # repeat zero 77 times
v <- rep(1:3, times=2) # repeat 1,2,3 twice

v1 <- 1:5
v2 <- rep(1,5)

# mathematical operations
sum
mean
sd
cor

# logical operations
v1==v2 #returns logical vector
v1!=v2
(v1>2) | (v2>0)   # | is the boolean OR, returns a vector.
(v1>2) & (v2>0)   # & is the boolean AND, returns a vector.
(v1>2) || (v2>0)  # || is the boolean OR, returns a single value
(v1>2) && (v2>0)  # && is the boolean AND, ditto

# add more elemets to vector
v1[6:10] <- 6:10
length(v1) <- 15

### Factors ###
# are used to store categorial data
eye.col.v <- c("brown", "green", "blue", "blue")
eye.col.f <- factor(c("brown", "green", "blue", "green", "blue"))

eye.col.f

### Matrices & Arrays ###
# A matrix is a vector with dimensions

m <- rep(1, 20) # data
dim(m) <- c(5,4) # row, column
m

m <- matrix(data=1, nrow=5, ncol = 4)
m <- matrix(1, 5, 4)
dim(m)

m <- cbind(1:5, 5:1, 5:9) # binds as columns
m <- rbind(1:5, 5:1, 5:9) # binds as rows

m <- matrix (1:10,10,10)

# 3d with dimensions 3x3x2
a <- array(data=1:18,dim=c(3,3,2))
a

### Lists ###
# Lists are collctions of objects. A single list can contain all kinds of elements.

l1 <- list(boo=v1, foo=v2, moo=v3, zoo="Animals!")
l2 <- list(v1, v2, v3, "Animals")
l3 <- list()
l4 <- NULL

### Data Frames ###
# Data fram is a special kind of list used for storing dataset tables. 
# Think of rows as cases, columns as variables. 
# Each column is a vector or factor
dfr1 <- data.frame( ID=1:4,
                    FirstName=c("John", "Jim"),
                    Female=c(F, F),
                    Age=c(22,33))

dfr1$FirstName

# als Vector definieren
dfr1$FirstName <- as.vector(dfr1$FirstName)
#oder
dfr2 <- data.frame(FristName=c("John", "Jim"), stringsAsFactors = F)
dfr2$FristName

# Accessing elements in a data.frame
dfr1[1,]   # First row, all columns
dfr1[,1]   # First column, all rows
dfr1$Age   # Age column, all rows
dfr1[1:2,3:4] # Rows 1 and 2, columns 3 and 4 - the gender and age of John & Jim
dfr1[c(1,3),] # Rows 1 and 3, all columns

### Flow Control and Loops ###
# They determine if a block of code will be excuted, and how many times.
# Blocks of code in R are enclosed in curly brackets {}

# for expression

x <- 5

ASum <- 0
AProd <- 1
for (i in 1:x)
{
  ASum <- ASum + i
  AProd <- AProd * i
}

ASum
AProd


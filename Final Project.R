#Task 1.
pop <- read.csv("~/Documents/XJTLU/APH413-Advanced Methods in Biostatistics/Final Project Material-20211101/pop.csv")

y <- as.factor(ifelse(pop$infect == 0,0,1))

str(pop)
pop[colnames(pop)] <- lapply(pop[colnames(pop)],factor)
#Factorize and display all column contents using the apply function

pop$y <- y

pop$CARIACICA <- as.factor(ifelse(pop$mun == "CARIACICA",1,0))
pop$SERRA <- as.factor(ifelse(pop$mun == "SERRA",1,0))
pop$VILAVELHA <- as.factor(ifelse(pop$mun == "VILA VELHA",1,0))
str(pop)

model2 <- pop[,-c(1,6,7,8,13,14,15,16,17)]

for (i in 1:7) {print(table(y,model2[,i],useNA = 'always'))}
#Descriptive statistics for variables in batches using for loops

#model <- data.frame(pop$y,pop$female,pop$adult,pop$crowd,pop$smoking,pop$mun,pop$hiv)
model <- pop[,c(2,4,5,9,11,14,15,16,17)]

for (i in 2:7) {print(chisq.test(xtabs(~y + model[,i])))}
#The variable P-value is tested using a for loop batch to see if the variable is significant

#An alternative way to implementation

# install.packages('tableone')
# library(tableone)

# vars <- c("crowd","mun","female","adult","smoking","mal","hiv","diabetes")
# facvar <- c("crowd","female","adult","smoking","mal","hiv","diabetes")
# m <- CreateTableOne(vars = vars,data = pop,strata = "y",factorVars = facvar)
# CreateCatTable(vars = vars,data = pop,strata ="y")


# install.packages('table1')

library(table1)

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y1 <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y1)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y1 ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y1, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the
  # line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

table1(~crowd + mun + female + adult + smoking + mal + hiv + diabetes|y, data = pop
       , overall = F, extra.col = list(`P-value` = pvalue))




#Task 2.
model1y <- glm(y ~ female + adult + crowd,family = binomial,data = model)

summary(model1y)

model2y <- glm(y ~ female + adult + crowd + smoking + CARIACICA + SERRA + VILAVELHA,family = binomial,data = model)
summary(model2y)

model3y <- glm(y ~ female + adult + crowd + smoking + CARIACICA + SERRA + VILAVELHA + hiv,family = binomial,data = model)
summary(model3y)

anova(model1y,model2y,model3y,test = "LRT")

AIC(model1y,model2y,model3y)


# chisq.test(pop$mun,pop$female)
# chisq.test(pop$mun,pop$adult)

# chisq.test(pop$smoking,pop$crowd)
chisq.test(pop$smoking,pop$female)
chisq.test(pop$smoking,pop$adult)
chisq.test(pop$mun,pop$crowd)

chisq.test(pop$hiv,pop$adult)
#Task 3.
newdata <- subset(pop,pop$adult == 1 & pop$smear == 1 & pop$culture == 1)
#newdata <- sqldf("select*from pop where adult = 1 and culture=1 and smear = 1",row.names = TRUE)
#summary(newdf)

summary(newdata$hhid)

library(sqldf)
disthhid <- sqldf("select DISTINCT hhid from newdata")

unlist(disthhid)
str(disthhid)

hhc <- sqldf("select * from pop where hhid in disthhid")

str(hhc)

model4y <- glm(y ~ female + adult + crowd + smoking + CARIACICA + SERRA + VILAVELHA + hiv,family = binomial,data = hhc)

# model5y <- glm(y ~ female + adult + crowd + smoking + mun + hiv,family = binomial,data = hhc)
# summary(model5y)

summary(model3y)
summary(model4y)

#Calculating relative risk in pop datasets using SQL fetch functions

t1 <- (sqldf("select count(*) from pop where smoking == 1 and y == 1")/sqldf("select count(*) from pop where smoking == 1"))/
  (sqldf("select count(*) from pop where smoking == 0 and y == 1")/sqldf("select count(*) from pop where smoking == 0"))
t1

t2 <- (sqldf("select count(*) from pop where hiv == 1 and y == 1")/sqldf("select count(*) from pop where hiv == 1"))/
  (sqldf("select count(*) from pop where hiv == 0 and y  == 1")/sqldf("select count(*) from pop where hiv == 0"))
t2

t3 <- (sqldf("select count(*) from pop where crowd==1 and y == 1")/sqldf("select count(*) from pop where crowd == 1"))/
  (sqldf("select count(*) from pop where crowd == 0 and y == 1")/sqldf("select count(*) from pop where crowd == 0"))
t3

t4 <- (sqldf("select count(*) from pop where adult == 1 and y == 1")/sqldf("select count(*) from pop where adult == 1"))/
  (sqldf("select count(*) from pop where adult == 0 and y == 1")/sqldf("select count(*) from pop where adult == 0"))
t4

#Calculating relative risk in hhc datasets using SQL fetch functions

t5<- (sqldf("select count(*) from hhc where smoking == 1 and y == 1")/sqldf("select count(*) from hhc where smoking == 1"))/
  (sqldf("select count(*) from hhc where smoking == 0 and y == 1")/sqldf("select count(*) from hhc where smoking == 0"))
t5

t6 <- (sqldf("select count(*) from hhc where hiv == 1 and y == 1")/sqldf("select count(*) from hhc where hiv == 1"))/
  (sqldf("select count(*) from hhc where hiv == 0 and y  == 1")/sqldf("select count(*) from hhc where hiv == 0"))
t6

t7<- (sqldf("select count(*) from hhc where crowd==1 and y == 1")/sqldf("select count(*) from hhc where crowd == 1"))/
  (sqldf("select count(*) from hhc where crowd == 0 and y == 1")/sqldf("select count(*) from hhc where crowd == 0"))
t7

t8 <- (sqldf("select count(*) from hhc where adult == 1 and y == 1")/sqldf("select count(*) from hhc where adult == 1"))/
  (sqldf("select count(*) from hhc where adult == 0 and y == 1")/sqldf("select count(*) from hhc where adult == 0"))
t8

RR <- c(t1,t2,t3,t4,t5,t6,t7,t8)
str(RR)

# a <- sqldf("select count(*) from pop where smoking == 1 and y == 1")
# a1 <- sqldf("select count(*) from pop where smoking == 1")
# a2 <- sqldf("select count(*) from pop where smoking == 0 and y == 1")
# a3 <- sqldf("select count(*) from pop where smoking == 0")

# b <- sqldf("select count(*) from pop where hiv == 1 and y == 1")
# b1 <- sqldf("select count(*) from pop where hiv == 1")
# b2 <- sqldf("select count(*) from pop where hiv == 0 and y  == 1")
# b3 <- sqldf("select count(*) from pop where hiv == 0")

# c <- sqldf("select count(*) from pop where crowd==1 and y == 1")
# c1 <- sqldf("select count(*) from pop where crowd == 1")
# c2 <- sqldf("select count(*) from pop where crowd == 0 and y == 1")
# c3 <- sqldf("select count(*) from pop where crowd == 0")

# d <- sqldf("select count(*) from pop where adult == 1 and y == 1")
# d1 <- sqldf("select count(*) from pop where adult == 1")
# d2 <- sqldf("select count(*) from pop where adult == 0 and y == 1")
# d3 <- sqldf("select count(*) from pop where adult == 0")

# e <- sqldf("select count(*) from hhc where smoking == 1 and y == 1")
# e1 <- sqldf("select count(*) from hhc where smoking == 1")
# e2 <- sqldf("select count(*) from hhc where smoking == 0 and y == 1")
# e3 <- sqldf("select count(*) from hhc where smoking == 0")

# f <- sqldf("select count(*) from hhc where hiv == 1 and y == 1")
# f1 <- sqldf("select count(*) from hhc where hiv == 1")
# f2 <- sqldf("select count(*) from hhc where hiv == 0 and y  == 1")
# f3 <- sqldf("select count(*) from hhc where hiv == 0")

# g<- sqldf("select count(*) from hhc where crowd==1 and y == 1")
# g1 <- sqldf("select count(*) from hhc where crowd == 1")
# g2 <- sqldf("select count(*) from hhc where crowd == 0 and y == 1")
# g3 <- sqldf("select count(*) from hhc where crowd == 0")

# h <- sqldf("select count(*) from hhc where adult == 1 and y == 1")
# h1 <- sqldf("select count(*) from hhc where adult == 1")
# h2 <- sqldf("select count(*) from hhc where adult == 0 and y == 1")
# h3 <- sqldf("select count(*) from hhc where adult == 0")
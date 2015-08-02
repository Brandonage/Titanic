#### WE DO SOME VISUALISATION OF THE AGE DENSITY FOR THE DIFFERENT FACTORS
#### THIS WILL HELP US IN FIGURING OUT THE AGE PREDICTING MODEL

library(sm)
sm.density.compare(full[!is.na(full$Age),]$Age, full[!is.na(full$Age),]$Sex)
colfill <- c(2:(2+length(levels(full$Sex))))
legend(locator(1),levels(full$Sex),fill=colfill)

sm.density.compare(full[!is.na(full$Age),]$Age, full[!is.na(full$Age),]$Pclass)
colfill <- c(2:(2+length(levels(full$Pclass))))
legend(locator(1),levels(full$Pclass),fill=colfill)

### Another way of doing it with ggplot

m <- ggplot(full[!is.na(full$Age),], aes(x=Age))
m + geom_density(aes(colour=factor(Pclass)),size=1) + theme_bw()



sm.density.compare(full[!is.na(full$Age),]$Age, full[!is.na(full$Age),]$Sex.Name)
colfill <- c(2:(2+length(levels(full$Sex.Name))))
legend(locator(1),levels(full$Sex.Name),fill=colfill)

#### IT SEEMS THERE IS A DIFFERENCE BUT IS NOT SIGNIFICANT THEN IN OUR LINEAR MODEL

sm.density.compare(full[!is.na(full$Age),]$Age, full[!is.na(full$Age),]$Parch)
colfill <- c(2:(2+length(levels(factor(full$Parch)))))
legend(locator(1),levels(factor(full$Parch)),fill=colfill)

#### WE TRY TO MINE THE FACTORS THAT ARE MORE IMPORTANT IN SURVIVING OR NOT

### IF YOU HAVE FAMILY YOU ARE MORE LIKELY TO SURVIVE
counts <- table(train$Survived,train$family)
barplot(counts, main="Family vs Survived" , xlab="0 = no family, 1 = family", col=c("darkred","lightgreen"), legend = rownames(counts))

### DEPENDING ON THE NUMBER OF FAMILY MEMBERS
counts <- table(train$Survived,train$familia)
barplot(counts, main="# of family members vs Survived" , col=c("darkred","lightgreen"))


### IF YOU ARE FEMALE YOU ARE MORE LIKELY TO SURVIVE
counts <- table(train$Survived,train$Sex)
barplot(counts, main="Genre vs Survived" , xlab="Male or Female"
        , col=c("darkred","lightgreen"), legend = rownames(counts))

### IF YOU ARE HIGHER CLASS YOU ARE MORE LIKELY TO SURVIVE
counts <- table(train$Survived,train$Pclass)
barplot(counts, main="Class vs Survived" , xlab="1st 2nd 3rd"
        , col=c("darkred","lightgreen"), legend = rownames(counts))

### IF YOU HAVE A CABIN YOU ARE MORE LIKELY TO SURVIVE
counts <- table(train$Survived,train$Cabin)
barplot(counts, main="Cabin vs Survived" , xlab="1 = no cabin, 2 = cabin"
        , col=c("darkred","lightgreen"), legend = rownames(counts))


### THE TITLE IN THE NAME ALSO HAS AN EFFECT IN SURVIVAL

counts <- table(train$Survived,train$Sex.Name)
barplot(counts, main="Sex.Name vs Survived" ,col=c("darkred","lightgreen"),
        legend = rownames(counts))

### Another way of doing this with ggplot

ggplot(train,aes(x=Sex.Name,fill=Survived)) + geom_bar() + theme_minimal() +
  labs(title="Title of Passengers and survival rate",x="Title")

### DEPENDING ON YOUR AGE: THERE IS A LITTLE INCREASE IN SURVIVAL IF YOU 
### ARE A CHILD

sm.density.compare(train[!is.na(train$Age),]$Age, train[!is.na(train$Age),]$Survived)
colfill <- c(2:(2+length(levels(factor(train$Survived)))))
legend(locator(1),levels(factor(train$Survived)),fill=colfill)

### SEX and Passenger Class seem to be really important factors for survival
### We can see that in the mosaic plot

library(vcd)
mosaic(~ Sex + Pclass + Survived,data=train,shade=TRUE)



#### SEEMS LIKE HIGH FARES GIVE YOU A BETTER SURVIVAL RATE

fares <- matrix(c(82,509,40,260),ncol=2,byrow=TRUE)
colnames(fares) <- c("f>60", "f<60")
rownames(fares) <- c(0,1)
barplot(fares, main="Fares vs Survived" ,col=c("darkred","lightgreen"),
        legend = rownames(fares))


nrow(train[which(train$Fare>60 & train$Survived==1),])
nrow(train[which(train$Fare>60 & train$Survived==0),])
nrow(train[which(train$Fare<60 & train$Survived==0),])
nrow(train[which(train$Fare<60 & train$Survived==1),])





# Example preprocessing script.
# library(ProjectTemplate); load.project(list(munging=FALSE)) # use to debug munging file


# dput(names(rcases))
# dput(names(meta.bfi))
v <- list()
v$items <- c("a1", "a2", "a3", "a4", "a5", "c1", "c2", "c3", "c4", "c5", 
             "e1", "e2", "e3", "e4", "e5", "n1", "n2", "n3", "n4", "n5", "o1", 
             "o2", "o3", "o4", "o5")
v$scales <- c("agreeableness",  "conscientiousness", "extraversion", "neuroticism", "openness")


# recode gender and education
rcases$genderf <- factor(rcases$gender, c(1,2), c("male", "female")) # i.e., 1 = male, 2 = female
table(rcases$education)
rcases$educationf <- factor(rcases$education, c(1,2,3,4,5), c("HS", "finished HS", "some college",
                                                             "college graduate", "graduate degree")) # i.e., 1 = male, 2 = female

# check itmem ranges
Hmisc::describe(rcases[,v$items])
psych::describe(rcases[,v$items])
sapply(rcases[,v$items], function(X) range(X, na.rm = TRUE))
sapply(rcases[,v$items], table)

rcases$nmiss <- apply(rcases, 1, function(X) sum(is.na(X)))

sapply(rcases, function(X) sum(is.na(X)))

table(rcases$nmiss)
rcases$retain <- rcases$nmiss < 4

ccases <- rcases[ rcases$retain, ]



# score tests
sc <- scoreItems(meta.bfi[,v$scales], ccases[,meta.bfi$name])
ccases[,colnames(sc$scores)] <- sc$scores

print(sc, short = FALSE)

# create quantiles
percentile_rank <- function(x, digits = 1) {
    prank <- rank(x)/ length(x) * 100
    round(prank, digits)
}
v$percentiles <- paste0("perc_", v$scales)
    
ccases[,v$percentiles] <- sapply(ccases[,v$scales], 
                            function(X) percentile_rank(X))

write.csv(ccases, file = "output/copy-for-excel.csv", na = "")



                                


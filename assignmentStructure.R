############################################################
#     Statistics Assignment
#
#     Jack Aherne / 20093747 / Applied Computing 
#
#     R Script used to answer Questions
############################################################
#       Loading database.
############################################################
# Load the data from the clipboard
data = read.delim("clipboard", header=TRUE)
attach(data)
############################################################
#       Question 1
############################################################
#Plot the data
r = table(managed, type)
colnames(r) = c("House", "Apartment", "Other")
row.names(r) = c("Managed", "Unmanaged")
plot(r, main="Managed/Type")

#Test
pwr.anova.test()


############################################################
#       Question 2
############################################################
#Plot the data
boxplot(rent ~ type, names = c("House", "Apartment", "Other"),
        main = "Rent/Occupancy")

# Summary Stats
tapply(rent, type, mean, na.rm=T)
tapply(rent, type, sd, na.rm=T)
tapply(rent, type, length)

# Test
summary(aov(rent ~ type))

# Results
#               Df  Sum Sq  Mean Sq   F value   Pr(>F)    
# factor(type)   1  38334   38334   14.06    0.000285 ***
# Residuals     109 297093    2726                     

summary(aov(rent ~ factor(type)))

# Results
#               Df  Sum Sq  Mean Sq   F value  Pr(>F)   
# factor(type)   2  38917   19459   7.088    0.00128 **
# Residuals    108  296510    2745  

############################################################
#       Question 3
############################################################
#Plot the data
boxplot(rent - last, paired=TRUE, main="Current Rent - Previous Rent")

# Test
t.test(rent, last, paired=TRUE)

# Results
#         Paired t-test
#
# data:  rent and last
# t = -5.7629, df = 112, p-value = 7.393e-08
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#    -40.06463 -19.56368
# sample estimates:
# mean of the differences 
#             -29.81416


############################################################
#       Question 4
############################################################
#Plot the data
plot(rent ~ income, main="Rent/Income")

# Summary Stats
mean(rent[income], na.rm = TRUE)

# Results
# 495.6293

sd(rent[income], na.rm = TRUE)

# Results
#60.29327

# Test
cor.test(rent,income)

# Results
#     Pearson's product-moment correlation
#
# data:  rent and income
# t = -0.69177, df = 108, p-value = 0.4906
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.2505446  0.1223440
# sample estimates:
#        cor 
# -0.06641901


############################################################
#       Question 5
############################################################
#Plot the data
g = table(occupant, type)
colnames(g) = c("Family", "House Share", "Single Occupant", "Other")
row.names(g) = c("House", "Apartment", "Other")
plot(g, main ="Occupant/Type")

# Test
chisq.test(occupant, type)

# Results
#   Pearson's Chi-squared test
#
#  data:  occupant and type
#  X-squared = 16.137, df = 6, p-value = 0.01304

############################################################
#       Question 6
############################################################

############################################################
#       Question 7
############################################################

############################################################
#       Question 8
############################################################
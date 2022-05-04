############################################################
#
#     Statistics Assignment
#
#     Jack Aherne / 20093747 / Applied Computing 
#
#     R Script used to answer Questions
#
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
chisq.test(managed, type)

# Results
# Pearson's Chi-squared test
#
# data:  managed and type
# X-squared = 0.15502, df = 2, p-value = 0.9254

############################################################
#       Question 2
############################################################
#Plot the data
boxplot(rent ~ occupant, names = 
          c("Family", "House Share","Single", "other"),
        main = "Rent/Occupancy")

# Summary Stats
tapply(rent, occupant, mean, na.rm=T)

# Results
# 1        2        3        4 
# 497.0000 493.6000 502.4444 492.5600 

tapply(rent, occupant, sd, na.rm=T)

# Results
# 1        2        3        4 
# 62.17315 57.19694 47.09020 49.09012 

tapply(rent, occupant, length)

# Results
# 1  2  3  4 
# 28 32 28 26 

# Test
summary(aov(rent ~ occupant))

# Results
#               Df  Sum Sq  Mean Sq   F value   Pr(>F)    
# occupant      1     15    14.5      0.005     0.944
# Residuals   105 304721  2902.1                      

summary(aov(rent ~ factor(occupant)))

# Results
#                    Df  Sum Sq  Mean Sq   F value  Pr(>F)   
# factor(occupant)   3   1599    533.1     0.181    0.909
# Residuals         103 303136  2943.1 

#ANOVA TEST
anova = aov(rent~occupant)
summary(anova)
anova.factor = aov(rent ~ factor(occupant))
TukeyHSD(anova.factor)

# Results
# Tukey multiple comparisons of means
#   95% family-wise confidence level
#
# Fit: aov(formula = rent ~ factor(occupant))
#
# $`factor(occupant)`
# diff       lwr      upr     p adj
# 2-1 -3.400000 -41.76509 34.96509 0.9956068
# 3-1  5.444444 -33.87753 44.76642 0.9837341
# 4-1 -4.440000 -44.51103 35.63103 0.9915125
# 3-2  8.844444 -28.73761 46.42649 0.9272286
# 4-2 -1.040000 -39.40509 37.32509 0.9998719
# 4-3 -9.884444 -49.20642 29.43753 0.9130542


############################################################
#       Question 3
############################################################
#Plot the data
boxplot(rent - last, paired=TRUE, 
        main="Current Rent - Previous Rent")

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
colnames(g) = c("Family", "House Share", "Single", "Other")
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
#Plot the data
plot(population ~ rent, main="Population/Rent")


# Summary Stats
sd(population[rent], na.rm = TRUE)

mean(rent[population], na.rm = TRUE)

# Test
cor.test(population, rent)

# Results
#Pearson's product-moment correlation
#
# data:  population and rent
# t = 52.728, df = 107, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.9727468 0.9871802
# sample estimates:
#       cor 
# 0.9812956


############################################################
#       Question 7
############################################################
# Plot the data
boxplot(rent ~ managed, horizontal = TRUE,
        names = c("Managed","Unmanaged"), main="Rent/Managed",)

# Summary Stats
mean(rent[managed == 1], na.rm = TRUE)

# Results
# 492.2

sd(rent[managed == 1], na.rm = TRUE)

# Results
# 56.93154

mean(rent[managed == 2], na.rm = TRUE)

# Results
# 497.3214

sd(rent[managed == 2], na.rm = TRUE)

# Results
# 54.23186

# Test
t.test(rent ~ managed)

# Results
# Welch Two Sample t-test
#
# data:  rent by managed
# t = -0.48512, df = 108.52, p-value = 0.6286
# alternative hypothesis: true difference in means between group 1 
# and group 2 is not equal to 0
# 95 percent confidence interval:
#   -26.04612  15.80327
# sample estimates:
#   mean in group 1 mean in group 2 
# 492.2000        497.3214

############################################################
#       Question 8
############################################################
#Plot the data
boxplot(rent ~ type, names = c("House", "Apartment", "Other"),
        main = "Rent/Type")

# Summary Stats
tapply(rent, type, mean, na.rm=T)

# Results
# 1        2        3 
# 475.6667 492.7419 520.0571

tapply(rent, type, sd, na.rm=T)

# Results
# 1        2        3 
# 49.41614 51.18070 57.00358

tapply(rent, type, length)

# Results
#  1  2  3 
# 47 34 38

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

#ANOVA TEST
anova = aov(rent~type)
summary(anova)
anova.factor = aov(rent ~ factor(type))
TukeyHSD(anova.factor)

# Results
# Tukey multiple comparisons of means
#   95% family-wise confidence level
#
# Fit: aov(formula = rent ~ factor(type))
#
# $`factor(type)`
# diff        lwr      upr     p adj
# 2-1 17.07527 -11.988875 46.13941 0.3464810
# 3-1 44.39048  16.326929 72.45402 0.0008044
# 3-2 27.31521  -3.395863 58.02628 0.0917978
r = table(managed, type)
colnames(r) = c("House", "Apartment", "Other")
row.names(r) = c("Managed", "Unmanaged")
plot(r, main="Managed/Type")

#Test
pwr.anova.test
#ASK BIGMAN TOMORROW
# Plot the data
boxplot(rent ~ managed, horizontal = TRUE,
        names = c("Managed","Unmanaged"), main="Rent/Managed",)

# Summary Stats
mean(rent[managed == 1], na.rm = TRUE)
sd(rent[managed == 1], na.rm = TRUE)

mean(rent[managed == 2], na.rm = TRUE)
sd(rent[managed == 2], na.rm = TRUE)

# Test
t.test(rent ~ managed)

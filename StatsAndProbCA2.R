############################################################
# Statistics Assignment (Stats and Probability)
#
# Eric Butler / ID: 20094078 / Programme: Applied Computing
#
# R commands to address the two questions pertaining to
# electric motor appliences
############################################################

# Load the data from the clipboard
motors = read.table("clipboard", header=T)
head(motors)
attach (motors)


#rm(list = ls())

#motors$line_x
#motors$line_y
#motors$radio
#motors$temperature
#motors$shift
#motors$appliance
#motors$noise


# 1.	Are the defective rates for the two production lines operating in the same shift different? 


plot(line_x ~ shift)
cor3 = cor.test(line_x[shift == "3"], line_y[shift=="3"]) #night
cor2 = cor.test(line_x[shift == "2"], line_y[shift=="2"]) #evening
cor1 = cor.test(line_x[shift == "1"], line_y[shift=="1"]) #morning


meanCor = mean(c(cor3$estimate, evening$estimate, cor1$estimate))

print(paste("Night corelation: ", cor3$estimate))
print(paste("Evening corelation: ", cor2$estimate))
print(paste("Morning corelation: ", cor1$estimate))


print(paste(" The Mean correlation of the two production lines (X and Y) operating in the same shifts: ", meanCor))



# 2.	How, if at all, does defective rate vary between shifts?

plotX = boxplot(line_x ~ shift, names = c("Morning", "Evening", "Night"), horizontal = T, xlab = "Product line X", ylab = "Shift")
plotY = boxplot(line_y ~ shift, names = c("Morning", "Evening", "Night"), horizontal = T, xlab = "Product line Y", ylab = "Shift")



sd(shift[line_x], na.rm = TRUE)




tapply(line_x, shift, mean, na.rm = T)
tapply(line_y, shift, mean, na.rm = T)

anovaX = aov(line_x ~ shift)
anovaY = aov(line_y ~ shift)



anovaX.factor = aov(line_x ~ factor(shift))
anovaY.factor = aov(line_y ~ factor(shift))

summary(anovaX.factor)
summary(anovaY.factor)

TukeyHSD(anovaX.factor)
TukeyHSD(anovaY.factor)



#summary(anovaX)
#summary(anovaY)



# 3.	Is there evidence that defective rates are related to ambient noise?

plot(line_x ~ noise)

cor.test(line_x, noise)
mean(line_x[noise], na.rm= TRUE)
sd(line_x[noise], na.rm= TRUE)

plot(line_y ~ noise)

cor.test(line_y, noise)
mean(line_y[noise], na.rm= TRUE)
sd(line_y[noise], na.rm= TRUE)



# 4.	What effect does having a radio on have on the defective rate?

plot(as.factor(radio), as.factor(line_x),  xlab = "production line x")
chisq.test(as.factor(radio), as.factor(line_x))
# P value 0.4495 


plot(as.factor(radio), as.factor(line_y), xlab = "production line y")
chisq.test(as.factor(radio), as.factor(line_y))
# P value 0.3693



# 5.	Is the radio more likely to be on one some shifts compared to others?

plot(as.factor(radio), as.factor(shift), xlab = "production line", names = (c(" 1 = Morning", " 2 = Evening", "3 = Night")))
chisq.test(as.factor(radio), as.factor(shift))

# 6.	Does the ambient temperature have an impact on defective rate?

plot(line_x ~ temperature)

cor.test(line_x, temperature)
mean(line_x[temperature], na.rm= TRUE)
sd(line_x[temperature], na.rm= TRUE)



plot(line_y ~ temperature)

cor.test(line_y, temperature)
mean(line_y[temperature], na.rm= TRUE)
sd(line_y[temperature], na.rm= TRUE)



#p-value for line_x: 2.2e-16
#cor for line_x: 0.9477184

#p-value for line_y: 3.668e-05
#cor for line_y: 0.4604101



# 7.	Do motors made for one particular appliance tend to be more prone to defects that others?

plotX = boxplot(line_x ~ appliance, xlab = "Product line X", ylab = "appliance")
plotY = boxplot(line_y ~ appliance,  xlab = "Product line Y", ylab = "appliance")



sd(noise[line_x], na.rm = TRUE)




tapply(line_x, appliance, mean, na.rm = T)
tapply(line_y, appliance, mean, na.rm = T)

anovaX = aov(line_x ~ appliance)
anovaY = aov(line_y ~ appliance)



aovX.factor = aov(line_x ~ factor(appliance))
aovY.factor = aov(line_y ~ factor(appliance))

summary(aovX.factor)
summary(aovY.factor)

TukeyHSD(aovX.factor)
TukeyHSD(aovY.factor)


# Pr(>F) for line X: 1.56e-05
# Pr(>F) for line Y: 0.0958

# 8.	Are motors made for different appliance made with similar frequencies on all three shifts?
plot(as.factor(appliance), as.factor(shift), names = "Morning", "Evening", "Night")
chisq.test(as.factor(appliance), as.factor(shift))




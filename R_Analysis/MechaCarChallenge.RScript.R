mechacar_mpg <- read.csv('MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)
# rename columns
colnames(mechacar_mpg)
mechacar_mpg <- mechacar_mpg %>% rename(#vehicle_weight = `vehicle weight`, 
                spoiler_angle = `spoiler angle`, ground_clearance = `ground clearance`,
                vehicle_length = `vehicle length`)
# perform multiple linear regression
lm(mpg ~ vehicle_weight + spoiler_angle + ground_clearance,data=mechacar_mpg)
#Summarize MLR results
summary(lm(mpg ~ vehicle_weight + spoiler_angle + ground_clearance,data=mechacar_mpg))
#plot a linear model of ground clearance
lmodel <- lm(mpg ~ ground_clearance,mechacar_mpg)
# establish y axis values
yvalues <- lmodel$coefficients["ground_clearance"]*mechacar_mpg$ground_clearance +
  lmodel$coefficients['(Intercept)']
#import data to ggplot2 and plot linear model
plt <- ggplot(mechacar_mpg,aes(x=ground_clearance, y=mpg))
plt + geom_point() + geom_line(aes(y=yvalues), color= 'blue')


#Create Summary Statistics Table
suspension_coil <- read.csv('Suspension_Coil.csv', check.names = F, stringsAsFactors = F)
suspense_summary<- 
  suspension_coil %>% summarise(mean(PSI), median(PSI), var(PSI), sd(PSI))
#suspense_summary1<-suspension_coil %>% 
  #group_by(Manufacturing_Lot) %>% summarise(mean(PSI), median(PSI), var(PSI), sd(PSI))
# Run one sided t-test to compare population mean to suspension coil mean
t.test((suspension_coil$PSI),mu=1500)





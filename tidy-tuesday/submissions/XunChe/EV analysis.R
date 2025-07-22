library(dplyr)
library(ggplot2)
library(ggalluvial)
library(forcats)

EV.df<-read.csv("C:\\Read\\EV\\Electric_Vehicle_Registration_Data_20250513.csv")
#Four variables in the dataset seem to be relevant to vehicles' weight:

#Vehicle Weight:	              Vehicle weight (tons)	                              vehicleweight	              Number
#Vehicle Declared Gross Weight:	Vehicle declared gross weight (tons)	              vehicledeclaredgrossweight	Number
#Vehicle Recorded GVWR:	        Vehicle recorded gross weight vehicle rating (GVWR)	vehiclerecordedgvwr	        Number
#Vehicle Category:	            Vehicle category	                                  vehicle_category	          Text

#Without knowing exactly what these variables are, I was trying to find out what the data tell us about themselves 
#– how much are the variables related to each other? Which one may better represent the actual weight of an EV?
#Starting with Vehicle Weight and Vehicle Declared Gross Weight because they come with units (tons) so the meaning is clearer. 
#Plot Vehicle Weight against Vehicle Declared Gross Weight (and use color to represent Vehicle Category:
                                                             
ggplot(EV.df, aes(x = Vehicle.Weight, y = Vehicle.Declared.Gross.Weight,color=Vehicle.Category)) +
  geom_point() +
  labs(x = "Vehicle weight (tons)",
       y = "Vehicle declared gross weight (tons)") +
  scale_x_log10()+
  scale_y_log10()+
  theme_minimal()
#[Figure 1]
#Surprisingly there appears to be no correlation between Vehicle Weight and Vehicle Declared Gross Weight, 
#but Vehicle Category may be a useful predictor of Vehicle Weight.
#At the same time, the weights do not appear realistic because according to KBB, the heaviest EV is GMC Hummer, 
#which weighs 9700 pounds, i.e. ~5 tons. 
#So either the data do not reflect reality, or some units are wrong. 
#There are also numerous zero values likely due to lack of report.

##On the other hand, Vehicle.Declared.Gross.Weight is correlated with Vehicle Weight:
#ggplot(EV.df, aes(x = Vehicle.Weight, y = Vehicle.Recorded.GVWR,color=Vehicle.Category)) +
#  geom_point() +
#  labs(x = "Vehicle weight (tons)",
#       y = "Vehicle.Recorded.GVWR(tons)") +
#  scale_x_log10()+
#  scale_y_log10()+
#  theme_minimal()
##[Fig.2]

#Hopefully the Vehicle Category and Vehicle Weight can help us solve the mystery:

#First clean up the data by eliminating zero values (Vehicle Weight =0)
EV.df.VW <- EV.df %>%
  filter(Vehicle.Weight > 0)

#Plot Vehicle Weight by Vehicle Category (geom_jitter).
ggplot(EV.df.VW, aes(x = Vehicle.Category, y = Vehicle.Weight)) + 
  geom_jitter() +
  labs(x = "Vehicle category",
       y = "Vehicle weight (tons)") +
    scale_y_log10()+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#[Fig.3]
#The more detailed categories seem to be a better predictor of vehicle weight 
#– although there are overlaps between categories, there is a general trend of heavier vehicles in higher class.
#The broader categories especially "Light-Duty (Class 1-2)" has a very broad range of weight (possibly partly due to much larger size). 
#So the detailed categories and Heavy-Duty category ("selected categories") are carried on for further analysis. 

EV.df.VW.lim<-EV.df.VW %>%
  filter(!Vehicle.Category %in% c("Light-Duty (Class 1-2)","Light-Duty (Class 1-2A)","Medium-Duty (Class 3-6)",""))
#Also, shouldn't EVs of the same model tend to have similar weight (and also fall into the same category)? 
#To test this hypothesis, weights of the models with the top 25 counts are plotted:

top_20_models <- EV.df.VW.lim %>%
  count(Vehicle.Model, sort = TRUE) %>%
  head(20)

EV.df.top20<-EV.df.VW.lim %>%
  filter(Vehicle.Model %in% top_20_models$Vehicle.Model)
ggplot(EV.df.top20, aes(x = Vehicle.Model, y = Vehicle.Weight, color=Vehicle.Category)) + #color=Registration.Usage
  geom_jitter() +
  labs(x = "Top 20 Vehicle Model",
       y = "Vehicle weight (tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#[Fig.4]
#The visualization proves the hypothesis is generally true 
#– vehicles of the same model (variable "Vehicle Name") have similar weights. 
#But the weights of EVs within the same model still vary, and sometimes fall into different categories.


#Use median to represent the "standard weight" of a model and the majority of category to represent the "standard category".
EV.df.VW.std<-EV.df.VW.lim %>%
  group_by(Vehicle.Model) %>%
  summarise(
    std_VW = median(Vehicle.Weight, na.rm = TRUE),
    std_Cat = names(sort(table(Vehicle.Category), decreasing = TRUE))[1],
    .groups = 'drop'
  ) %>%
  arrange(desc(std_VW))
EV.df.VW.std <- EV.df.VW.std %>%
  mutate(Vehicle.Model = reorder(Vehicle.Model, std_VW, .desc = TRUE))

ggplot(EV.df.VW.std, aes(x = Vehicle.Model, y = std_VW, color=std_Cat)) + 
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1,size=5))+
  theme(legend.text=element_text(size=5))
#[Fig.5]

#After this simplification, the category divides becomes clearer
#- the "standard weight" of a model predicts its "standard category" ("selected categories" only). 
#And it's also clear now what the correct unit should be for most registered vehicles: 
#10000 lb (divide b/w Classes 2H&3) = 10000(unit?) (divide b/w Models Silverado Rst & Hummer Pickup) 
#The answer is "pound".
#Similarly, 
#9000 lb (div 2G/2H) = 9000 lbs (div Cybertruck/F150 Lightning Lariat)
#But other categories still have some overlaps.


#Continue to assume EVs of the same model have similar weights and converting units to lbs:
#  Model Example: Ford Mach-E

MachE<-EV.df.VW %>%
  filter(grepl("Mach", Vehicle.Model))
MachE$Vehicle.Category <- factor(MachE$Vehicle.Category, levels = names(sort(table(MachE$Vehicle.Category), decreasing = TRUE)))
ggplot(MachE, aes(axis1 = factor(Vehicle.Model), axis2 = factor(Vehicle.Name), axis3 = Vehicle.Category)) +
  geom_alluvium(aes(fill = factor(Vehicle.Model))) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Vehicle.Model","Vehicle.Name","Vehicle.Category")) +
  theme_minimal() +
  labs(title = "Relationships between Vehicle.Model,Vehicle.Name and Vehicle.Category")
#[Fig.6]
#The categorization is more likely based on "Vehicle Name" than on "Vehicle Model".



ggplot(MachE, aes(x = Vehicle.Name, y = Vehicle.Weight,color=Vehicle.Category)) + 
  geom_jitter() +
  geom_hline(yintercept=6000)+
  geom_hline(yintercept=5001)+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30))
#[Fig.7]
#Now most weights fall in the range of 5001-6000 lb but there are several outliers 
#– they do not seem to be unit errors because no common units would explain them.

#some additional observations of all EVs
EV.df.VW$Vehicle.Name <- factor(EV.df.VW$Vehicle.Name, levels = names(sort(table(EV.df.VW$Vehicle.Name), decreasing = TRUE)))
EV.df.VW.modified <- EV.df.VW %>%
  mutate(
    Vehicle.Name.Top20 = fct_lump_n(Vehicle.Name, n = 20, other_level = "Others")
  )

ggplot(EV.df.VW.modified, aes(axis1 = factor(Vehicle.Name.Top20), axis2 = factor(Vehicle.Category))) +
  geom_alluvium(aes(fill = factor(Vehicle.Name.Top20))) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Vehicle.Name.Top20", "Vehicle.Category")) +
  theme_minimal() +
  labs(title = "Relationships between Vehicle.Name and Vehicle.Category") +
  theme(legend.position = "none")
#[Fig.8]
#like MachE, most Vehicle Names predicts their Categories well.
#But there are exceptions,
#e.g. some Ford E-Transit are Light duty but some are Medium-duty

ETransit<-EV.df.VW %>%
  filter(Vehicle.Name=="Ford E-Transit")
ggplot(ETransit, aes(x = Vehicle.Weight, y = Vehicle.Recorded.GVWR,color=Vehicle.Category)) + 
  geom_point() +
  theme_minimal()
#[Fig.9]
#... but the different categories are not well predicted by the numerical weight variables

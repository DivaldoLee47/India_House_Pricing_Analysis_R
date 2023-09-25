# DIVALDO LEE
# TP062988

install.packages("ggplot2")
install.packages("corrplot")
install.packages("car")
install.packages("scales")
install.packages("tidyverse")
install.packages("RColorBrewer")
install.packages("cowplot")
install.packages("gridExtra")
install.packages("plotrix")

library(ggplot2)
library(corrplot)
library(car)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(cowplot)
library(gridExtra)
library(plotrix)

# import file
data_hr = read.csv("C:\\Users\\Asus\\OneDrive\\Desktop\\Apu CSDA\\Y2 Sem 1\\Programming For Data Analysis\\Assignment\\House_Rent_Dataset.csv", 
                       header=TRUE)
data_hr

View(data_hr)

# To view the data type and overall median, mode, mean, min, and max values of each columns.
summary(data_hr)
str(data_hr)

unique(data_hr$City)
unique(data_hr$Area.Type)

# Extra feature 1
# Removing outliers in order to perform a more accurate analysis since they may represent measurement errors

outlier_remove <- function(datafr, column, low, high){
  Q <- quantile(datafr[,column], probs = c(low, high), na.rm = FALSE)
  iqr<- IQR(datafr[,column])
  up <- Q[2] + 1.5*iqr
  down <- Q[1] - 1.5*iqr
  new_datafr <- subset(datafr, datafr[,column]> down & datafr[,column] < up)
  outlier <- subset(datafr, datafr[,column] <down)
  outlier <- rbind(outlier, subset(datafr, datafr[,column] > up))
  outlier[,column] = as.integer(mean(new_datafr[, column]))
  new_datafr <- rbind(new_datafr,outlier)
  return (new_datafr)
} 

data_hr <- outlier_remove(data_hr, "Rent", 0.15, 0.85)

# Change Posted.On Column into a date format
data_hr$Posted.On <- strptime(data_hr$Posted.On, "%m/%d/%Y")
View(data_hr)

head(data_hr$Posted.On)
# Arranging data frame for each month
summary(data_hr)
 
april <- data_hr[format(data_hr$Posted.On, "%m") == "04",]
may <- data_hr[format(data_hr$Posted.On, "%m") == "05",]
june <- data_hr[format(data_hr$Posted.On, "%m") == "06",]
july <- data_hr[format(data_hr$Posted.On, "%m") == "07",]

# Adding a column specifically for months
data_hr[, "Month"] <- format(data_hr[,"Posted.On"], "%m")

rep_str = c('04'='April','05'='May','06'='June','07'='July')
data_hr$Month <- str_replace_all(data_hr$Month, rep_str)

View(data_hr)

#Assigning columns
num_of_bhk <- data_hr$BHK

num_of_bathroom <- data_hr$Bathroom

house_size <- data_hr$Size

rent_price <- data_hr$Rent

furnish_type <- data_hr$Furnishing.Status
yes_furnish <- data_hr$Furnishing.Status == "Furnished"
no_furnish <- data_hr$Furnishing.Status == "Unfurnished"
semi_furnish <- data_hr$Furnishing.Status == "Semi-Furnished"

area_type <- data_hr$Area.Type
super_area <- data_hr$Area.Type == "Super Area"
carpet_area <- data_hr$Area.Type == "Carpet Area"
built_area <- data_hr$Area.Type == "Built Area"

tenant_types <- data_hr$Tenant.Preferred
bachelors <- data_hr$Tenant.Preferred == "Bachelors"
family <- data_hr$Tenant.Preferred == "Family"
bachelorsFamily <- data_hr$Tenant.Preferred == "Bachelors/Family"
city_types <- data_hr$City

bangalore_city <- data_hr$City == "Bangalore"
chennai_city <- data_hr$City == "Chennai"
delhi_city <- data_hr$City == "Delhi"
hyderabad_city <- data_hr$City == "Hyderabad"
kolkata_city <- data_hr$City == "Kolkata"
mumbai_city <- data_hr$City == "Mumbai"

# Question 1: 
# In what way do the numeric variables have a strong correlation between each other?

# Analysis 1-1:
# Calculate the correlation significant value between BHK, Rent, Size, and Bathroom.

crhouserent <- data_hr[,c(-1,-5,-6,-7,-8,-9,-10,-12,-13)]
plot(crhouserent, col = "darkgreen",  main = "House Rent Dataset")
cor(crhouserent)

# Conclusion: It is shown that all pairs of variables produce a positive correlation which implies each pair of variables to go in the same direction.

# Analysis 1-2:
# Determine the strength of each pairs of the variables.

round(cor(crhouserent))

# Conclusion: It appears that only rent prices tend to have a weaker relationship (independent) with the other variables since the correlation is closer to 0, compared to others which has a strong correlation that is closer to 1.

# Analysis 1-3:
# Create a correlogram for the correlation values of the variables.'

# Extra feature ( Correlation Diagram )
cr<-cor(crhouserent)

corrplot(cr, addCoef.col = TRUE, col=colorRampPalette(c("yellow","white","darkgreen"))(200), method='pie') 

# Analysis 1-4:
# Plot a boxplot for proving the strongest correlation between the numerical variables.

boxplot(Bathroom ~ BHK, data = data_hr, 
        xlab = "Number of BHK", ylab = "Number of Bathrooms", main = "Correlation between BHK & Bathroom", col="orange")

# Analysis 1-5:
# Provide another evidence to support Analysis 1-1(relationship between BHK & Bathroom) using scatterplot chart.

plot(num_of_bhk, num_of_bathroom, main = "Correlation between BHK & Bathroom",
     xlab = "Number of BHK", ylab = "Number of Bathroom",
     pch = 19, frame = FALSE)
abline(lm(num_of_bathroom ~ num_of_bhk, data = data_hr), col = "blue")

# Analysis 1-6:
# Based on the correlation diagram, find the evidence of Size and Bathroom of houses have the second strongest correlation.

ggplot(data_hr) + aes(x = num_of_bathroom, y = house_size) +
  geom_point(aes(color=house_size)) + 
  geom_smooth(method=lm, color = "red") +
  ggtitle("Scatterplot Correlation between Size and Bathroom") +
  labs(x = "Number of Bathrooms", y = "House Sizes") + theme_light()

# Analysis 1-7:
# How does house sizes affect the number of bathrooms in each house?
 
scatterplot(num_of_bathroom ~ house_size, data = data_hr, 
            xlab = "House Sizes", ylab = "Number of Bathrooms", 
            frame = FALSE, main="Distribution between House Sizes & Bathroom")

# Analysis 1-8:
# Find the evidence of BHK having the third strongest correlation with house sizes.

boxplot(Size ~ BHK, data = data_hr, 
        xlab = "Number of BHK", ylab = "House Sizes", 
        main = "Correlation between BHK & House Sizes",
        col = "yellow")

# Analysis 1-9:
# Does the size of houses correlate with the number of BHK?

plot(num_of_bhk, house_size, main = "Correlation between BHK & House Sizes",
     xlab = "Number of BHK", ylab = "House Sizes",
     pch = 19, frame = FALSE)
abline(lm(house_size ~ num_of_bhk, data = data_hr), col = "blue")
lines(lowess(num_of_bhk, house_size), col = "red")

# Question 2:
# Considering the various range of rent prices between houses, how does it vary between each tenant preferred?

# Analysis 2-1:
# Plot a histogram and density chart indicating the distribution of rent prices.

rent_hist <- ggplot(data_hr, aes(x=Rent, fill=..count..)) + geom_histogram(aes(y=..density..),color="black", size= 1) +
  scale_fill_gradient("Frequency", low="grey", high="purple") +
  ggtitle("Rent Histogram") + labs(y="Frequency", x="Rent Prices") + theme_light()

rent_histden <- rent_hist + geom_density(fill="green",color="navy", size=.9, alpha=.2)

rent_histden

# Analysis 2-2:
# Plot a bar graph of count for every type of tenants preferred.

ggplot(data_hr, aes(x = tenant_types, fill=tenant_types)) + geom_bar(color = "black", size=1, width=0.7) + #Get to know how many types of tenants 
 scale_fill_brewer(palette="Spectral", name = "Tenant Preferred") +
  labs(x = "Tenant Preferred", y = "Frequency") +
  ggtitle("Distribution of Tenants Preferred")

# Analysis 2-3:
# Create a Boxplot showing the relation between Tenants preferred and Rent price.

ggplot(data_hr, aes(x = tenant_types, y = rent_price)) + 
  geom_boxplot(outlier.shape = NA, fill = "yellow", color="darkgreen", size=1) + 
  labs(title = "Boxplot Between Rent Price and Tenants Preferred",
       x = "Tenants Preferred",
       y = "Rent Price") + 
  coord_cartesian(ylim =  c(50, 80000)) + theme_light()

# Analysis 2-4:
# Plot a histogram of rent prices for all types of tenants preferred.

ggplot(data_hr, aes(x=rent_price, colour=tenant_types)) + geom_histogram(aes(fill=..count..)) +
  scale_fill_gradient("Frequency", low="yellow", high="lightblue") +
  labs(x="Tenant Preferred", y="Frequency") +
  ggtitle("Distribution of Tenats Preferred") + theme_light()

# Analysis 2-5:
# Plot a histogram and density chart that shows distribution between rent prices and Bachelors/Family.

bf_hist <- ggplot(data_hr[bachelorsFamily, ], aes(x=Rent)) + 
  geom_histogram(colour="darkblue",aes(fill=..count..)) + 
  geom_vline(aes(xintercept=mean(Rent)),
             color="blue", linetype="dashed", size=1) +
  scale_fill_gradient("Frequency", low="lightblue", high="darkblue") +
  ggtitle("Histogram Distribution between Rent & Bachelors/Family") + 
  labs(y = "Frequency", x = "Rent of Bachelors/Family") + theme_light()

bf_den <- ggplot(data_hr[bachelorsFamily, ], aes(x=Rent)) + # Extra feature (Density Chart)
  geom_vline(aes(xintercept=mean(Rent)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Density Distribution between Rent & Bachelors/Family") +
  geom_density(color="darkblue", fill="lightblue", alpha=0.8, size=1) +
  labs(y = "Density", x = "Rent of Bachelors/Family") + theme_light()

plot_grid(bf_hist, bf_den, ncol = 1, nrow = 2) 

# Analysis 2-6:
# Plot a histogram and density chart that shows distribution between rent prices and Bachelors.

b_hist <- ggplot(data_hr[bachelors, ], aes(x=Rent)) + 
  geom_histogram(colour="orange",aes(fill=..count..)) +
  scale_fill_gradient("Frequency", low="yellow", high="red") + 
  geom_vline(aes(xintercept=mean(Rent)),color="red", linetype="dashed", size=1) +
  ggtitle("Histogram Distribution between Rent & Bachelors") + 
  labs(y = "Frequency", x = "Rent of Bachelors") + theme_light()

b_den <- ggplot(data_hr[bachelors, ], aes(x=Rent)) + # Extra feature (Density Chart)
  geom_density(colour="orange", fill="yellow", alpha=0.8, size=1) +
  geom_vline(aes(xintercept=mean(Rent)),
             color="red", linetype="dashed", size=1) +
  ggtitle("Density Distribution between Rent & Bachelors") +
  labs(y = "Density", x = "Rent of Bachelors") + theme_light()

plot_grid(b_hist, b_den, ncol = 1, nrow = 2) 

# Analysis 2-7:
# Plot a histogram and density chart that shows distribution between rent prices and Family.

f_hist <- ggplot(data_hr[family, ], aes(x=Rent)) + 
  geom_histogram(colour="black",aes(fill=..count..)) +
  scale_fill_gradient("Frequency", low="lightgreen", high="darkgreen") + 
  geom_vline(aes(xintercept=mean(Rent)),
             color="darkgreen", linetype="dashed", size=1) +
  ggtitle("Histogram Distribution between Rent & Family") + 
  labs(y = "Frequency", x = "Rent of Family") + theme_light()

f_den <- ggplot(data_hr[family, ], aes(x=Rent)) + # Extra feature (Density Chart)
  geom_density(colour="orange", fill="green", alpha=0.8, size=1) +
  geom_vline(aes(xintercept=mean(Rent)),
             color="darkgreen", linetype="dashed", size=1) +
  ggtitle("Density Distribution between Rent & Family") +
  labs(y = "Density", x = "Rent of Family") + theme_light()

plot_grid(f_hist, f_den, ncol = 1, nrow = 2)

# Question 3:
# How does each city affect the cost of renting?

# Analysis 3-1:
# Plot a pie and line chart showing types of cities.

freqPercentCity <- data_hr %>% drop_na(City) %>% # drop missing values by variable
  group_by(City) %>%  # specify categorical variable
  summarize(Frequency = n()) # return counts / frequencies

ggplot(freqPercentCity, aes(x="", y=Frequency, fill=City))+
  geom_bar(width = 1, stat = "identity") +
  scale_fill_brewer(palette="Dark2") +
  coord_polar(theta = "y", start = 0) + ggtitle("Type of Cities")

bangalore_freq <- nrow(data_hr[data_hr$City == "Bangalore",])
chennai_freq <- nrow(data_hr[data_hr$City == "Chennai",])
delhi_freq <- nrow(data_hr[data_hr$City == "Delhi",])
hyderabad_freq <- nrow(data_hr[data_hr$City == "Hyderabad",])
kolkota_freq <- nrow(data_hr[data_hr$City == "Kolkata",])
mumbai_freq <- nrow(data_hr[data_hr$City == "Mumbai",])
cities_freq <- c(bangalore_freq, chennai_freq, delhi_freq, hyderabad_freq, kolkota_freq, mumbai_freq)
city_names <- c("Bangalore", "Chennai", "Delhi", "Hyderabad", "Kolkota", "Mumbai")
city_df <- data.frame(city_names, cities_freq)

ggplot(city_df, aes(x=city_names, y=cities_freq, group=1)) +
  geom_line(linetype = "dashed", size=1, color="blue")+
  geom_point() + theme_classic() + labs(y="Frequency", x="City")

# Analysis 3-2:
# Plot a boxplot to evaluate average rent prices in every city.

ggplot(data_hr, aes(x = city_types, y = rent_price, fill=city_types)) + 
  geom_boxplot(alpha=1) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1") +
  labs(title = "Boxplot Between Rent Price and City Types", x = "City Types", y = "Rent Price") + theme_bw()

# Analysis 3-3:
# Plot a bar graph to view the highest rent among the cities.

ggplot(data_hr, aes( y=Rent, x=city_types, fill=city_types)) + 
  geom_bar(stat="identity", width=0.7) +
  scale_fill_brewer(palette="Set2", name = "City") +
  labs(title = "Rent Price Range between City Types", x = "City Types", y = "Rent Price") + theme_classic()

# Analysis 3-4:
# Plot both histogram and density chart for rent prices vary in every city.

  rentp_hist <- ggplot(data_hr, aes(x=rent_price, colour=city_types)) + geom_histogram(aes(fill=..count..)) +
    scale_fill_gradient("Frequency", low="lightblue", high="darkblue") +
    ggtitle("Histogram Distribution between Cities and Rent Prices") + 
    labs(y="Frequency", x="Rent Prices") + theme_light()
  
  rentp_den <- ggplot(data_hr, aes(x=rent_price, colour=city_types)) + geom_density(size=0.8) + # Extra feature (Density Chart)
    ggtitle("Density Distribution between Cities and Rent Prices") + 
    labs(y="Density", x="Rent Prices") + theme_light()
  
  plot_grid(rentp_hist, rentp_den, ncol = 1, nrow = 2) # Extra feature (Putting several charts into a single page)

# Analysis 3-5:
# Plot both histogram and density chart for rent prices vary in Bangalore specifically. 

bangaloreCity_hist <- ggplot(data_hr[bangalore_city, ], aes(x=Rent)) + 
  geom_histogram(colour="green",aes(fill=..count..)) +
  scale_fill_gradient("Frequency", low="beige", high="green") + 
  geom_vline(aes(xintercept=mean(Rent)),
             color="green", linetype="dashed", size=1) +
  ggtitle("Frequency Distribution between Rent & Bangalore") + 
  labs(y = "Frequency", x = "Rent Prices in Bangalore") + theme_light()

bangaloreCity_den <- ggplot(data_hr[bangalore_city, ], aes(x=Rent)) + # Extra feature (Density Chart)
  geom_density(colour="green", fill="beige", alpha=0.8, size=1) +
  geom_vline(aes(xintercept=mean(Rent)),
             color="green", linetype="dashed", size=1) +
  ggtitle("Density Distribution between Rent & Bangalore") +
  labs(y = "Density", x = "Rent Prices in Bangalore") + theme_light()

plot_grid(bangaloreCity_hist, bangaloreCity_den, ncol = 1, nrow = 2) # Extra feature (Put several ggplots into a single page)

# Analysis 3-6: 
# Plot both histogram and density chart for rent prices vary in Chennai specifically. 

chennaiCity_hist <- ggplot(data_hr[chennai_city, ], aes(x=Rent)) + 
  geom_histogram(colour="darksalmon",aes(fill=..count..)) +
  scale_fill_gradient("Frequency", low="cornsilk", high="orange") + 
  geom_vline(aes(xintercept=mean(Rent)),
             color="orange", linetype="dashed", size=1) +
  ggtitle("Frequency Distribution between Rent & Chennai") + 
  labs(y = "Frequency", x = "Rent Prices in Chennai") + theme_light()

chennaiCity_den <- ggplot(data_hr[bangalore_city, ], aes(x=Rent)) + # Extra feature (Density Chart)
  geom_density(colour="orange", fill="cornsilk", alpha=0.8, size=1) +
  geom_vline(aes(xintercept=mean(Rent)),
             color="orange", linetype="dashed", size=1) +
  ggtitle("Density Distribution between Rent & Chennai") +
  labs(y = "Density", x = "Rent Prices in Chennai") + theme_light()

plot_grid(chennaiCity_hist, chennaiCity_den, ncol = 1, nrow = 2) # Extra feature (Put several ggplots into a single page)

# Analysis 3-7:
# Plot both histogram and density chart for rent prices vary in Delhi specifically. 

delhiCity_hist <- ggplot(data_hr[delhi_city, ], aes(x=Rent)) + 
  geom_histogram(colour="blue",aes(fill=..count..)) +
  scale_fill_gradient("Frequency", low="cyan", high="blue") + 
  geom_vline(aes(xintercept=mean(Rent)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Frequency Distribution between Rent & Delhi") + 
  labs(y = "Frequency", x = "Rent Prices in Delhi") + theme_light()

delhiCity_den <- ggplot(data_hr[delhi_city, ], aes(x=Rent)) + # Extra feature (Density Chart)
  geom_density(colour="blue", fill="cyan", alpha=0.8, size=1) +
  geom_vline(aes(xintercept=mean(Rent)),
             color="blue", linetype="dashed", size=1) +
  ggtitle("Density Distribution between Rent & Delhi") +
  labs(y = "Density", x = "Rent Prices in Delhi") + theme_light()

plot_grid(delhiCity_hist, delhiCity_den, ncol = 1, nrow = 2) # Extra feature (Put several ggplots into a single page)

# Analysis 3-8:
# Plot both histogram and density chart for rent prices vary in Hyderabad specifically. 

hyderabadCity_hist <- ggplot(data_hr[hyderabad_city, ], aes(x=Rent)) + 
  geom_histogram(colour="blueviolet",aes(fill=..count..)) +
  scale_fill_gradient("Frequency", low="pink", high="blueviolet") + 
  geom_vline(aes(xintercept=mean(Rent)),
             color="blueviolet", linetype="dashed", size=1) +
  ggtitle("Frequency Distribution between Rent & Hyderabad") + 
  labs(y = "Frequency", x = "Rent Prices in Hyderabad") + theme_light()

hyderabadCity_den <- ggplot(data_hr[hyderabad_city, ], aes(x=Rent)) + # Extra feature (Density Chart)
  geom_density(colour="blueviolet", fill="pink", alpha=0.8, size=1) +
  geom_vline(aes(xintercept=mean(Rent)),
             color="blueviolet", linetype="dashed", size=1) +
  ggtitle("Density Distribution between Rent & Hyderabad") +
  labs(y = "Density", x = "Rent Prices in Hyderabad") + theme_light()

plot_grid(hyderabadCity_hist, hyderabadCity_den, ncol = 1, nrow = 2) # Extra feature (Put several ggplots into a single page)

# Analysis 3-9:
# Plot both histogram and density chart for rent prices vary in Kolkata specifically. 

 kolkotaCity_hist <- ggplot(data_hr[kolkata_city, ], aes(x=Rent)) + 
  geom_histogram(colour="darkolivegreen",aes(fill=..count..)) +
  scale_fill_gradient("Frequency", low="darkolivegreen1", high="darkolivegreen") + 
  geom_vline(aes(xintercept=mean(Rent)),
             color="darkolivegreen", linetype="dashed", size=1) +
  ggtitle("Frequency Distribution between Rent & Kolkota") + 
  labs(y = "Frequency", x = "Rent Prices in Kolkota") + theme_light()

kolkotaCity_den <- ggplot(data_hr[kolkata_city, ], aes(x=Rent)) + # Extra feature (Density Chart)
  geom_density(colour="darkolivegreen", fill="darkolivegreen1", alpha=0.8, size=1) +
  geom_vline(aes(xintercept=mean(Rent)),
             color="darkolivegreen", linetype="dashed", size=1) +
  ggtitle("Density Distribution between Rent & Kolkota") +
  labs(y = "Density", x = "Rent Prices in Kolkota") + theme_light()

plot_grid(kolkotaCity_hist, kolkotaCity_den, ncol = 1, nrow =2) # Extra feature (Put several ggplots into a single page)

# Analysis 3-10:
# Plot both histogram and density chart for rent prices vary in Mumbai specifically.

mumbaiCity_hist <- ggplot(data_hr[mumbai_city, ], aes(x=Rent)) + 
  geom_histogram(colour="brown2",aes(fill=..count..)) +
  scale_fill_gradient("Frequency", low="darkgoldenrod1", high="brown2") + 
  geom_vline(aes(xintercept=mean(Rent)),
             color="brown2", linetype="dashed", size=1) +
  ggtitle("Frequency Distribution between Rent & Mumbai") + 
  labs(y = "Frequency", x = "Rent Prices in Mumbai") + theme_light()

mumbaiCity_den <- ggplot(data_hr[mumbai_city, ], aes(x=Rent)) + # Extra feature (Density Chart)
  geom_density(colour="brown2", fill="darkgoldenrod1", alpha=0.8, size=1) +
  geom_vline(aes(xintercept=mean(Rent)),
             color="brown2", linetype="dashed", size=1) +
  ggtitle("Density Distribution between Rent & Mumbai") +
  labs(y = "Density", x = "Rent Prices in Mumbai") + theme_light()

plot_grid(mumbaiCity_hist, mumbaiCity_den, ncol = 1, nrow = 2) # Extra feature (Put several ggplots into a single page)
 
# Question 4
# What are the factors for Bachelors/Family tenants to choose a house?

# Analysis 4-1
# How many BHKs in a house do Bachelors/Family prefer the most?

bf_BHK <- data_hr$BHK[data_hr$Tenant.Preferred == "Bachelors/Family"]
bf_BHK
data_bf_BHK <- data.frame(bf_BHK)
data_bf_BHK

ggplot(data_bf_BHK, aes(x = bf_BHK))+geom_bar(aes(fill=..count..), color="black", width = 0.8, size=1) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  scale_fill_gradient(low="cyan",high="darkblue") +
  ggtitle("Number of BHK based on Bachelors/Family preferred") +
  labs(y = "Frequency", x = "Number of BHK") + theme_light()

# Analysis 4-2
# How many bathrooms in a house do Bachelors/Family prefer the most?

bf_bathroom <- data_hr$Bathroom[data_hr$Tenant.Preferred == "Bachelors/Family"]
bf_bathroom
data_bf_bath <- data.frame(bf_bathroom)
data_bf_bath

all_bath_bar <- ggplot(data_hr, aes(data_hr$Bathroom)) + geom_bar(aes(fill=Tenant.Preferred), color="black", size=1) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white", size=3.5) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Number of Bathrooms based on all Tenants preferred") +
  labs(y = "Frequency", x = "Number of Bathrooms") + theme_light()

all_bath_poly <- ggplot(data_hr, aes(Bathroom, colour=Tenant.Preferred)) + geom_freqpoly(binwidth=1, size=1) +
  ggtitle("Number of Bathrooms based on all Tenants preferred") +
  scale_fill_brewer(palette="Dark2") +
  labs(y = "Frequency", x = "Number of Bathrooms") + theme_light()

bf_bath_bar <- ggplot(data_bf_bath, aes(x = bf_bathroom)) + geom_bar(aes(fill=..count..), color="black", width = 0.8, size=1) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") +
  scale_fill_gradient(low="azure1",high="darkkhaki") +
  ggtitle("Number of Bathrooms based on Bachelors/Family preferred") +
  labs(y = "Frequency", x = "Number of Bathrooms") + theme_light()

grid.arrange(arrangeGrob(all_bath_bar, all_bath_poly, ncol=2), bf_bath_bar, nrow=2) # Extra feature (Put several ggplots 
                                                                                    # into a single page)

# Analysis 4-3
# In terms of size, how big of a house do Bachelors/Family prefer the most?

bf_size <- data_hr$Size[data_hr$Tenant.Preferred == "Bachelors/Family"]
bf_size
data_bf_size <- data.frame(bf_size)
data_bf_size

all_size_bar <- ggplot(data_bf_size) + geom_histogram(colour="black",aes(x=bf_size,fill=..count..)) +
  scale_fill_gradient("Frequency", low="cyan", high="darkviolet") + 
  ggtitle("House sizes based on Bachelors/Family preferred") + 
  labs(y = "Frequency", x = "House Size") + theme_light()

bf_size_poly <- ggplot(data_hr, aes(Size, colour=Tenant.Preferred)) +
  geom_freqpoly(binwidth = 500, size=1) +
  ggtitle("House sizes based on all Tenants preferred") +
  labs(y = "Frequency", x = "House Size") + theme_light()

grid.arrange(bf_size_poly, all_size_bar, nrow=2) # Extra feature (Put several ggplots into a single page)

# Analysis 4-4
# Do Bachelors/Family prefer to rent furnished houses?

bf_furnish <- data_hr$Furnishing.Status[data_hr$Tenant.Preferred == "Bachelors/Family"]
bf_furnish
data_bf_furnish <- data.frame(bf_furnish)
data_bf_furnish 

all_furnish_bar <- ggplot(data_hr, aes(data_hr$Furnishing.Status)) + 
  geom_bar(aes(fill=Tenant.Preferred), color="black", width = 0.7, size=1) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") +
  scale_fill_brewer(palette="Set2", name = "Tenants Preferred") +
  ggtitle("Distribution of Furnishing Status based on all Tenants Preferred") +
  labs(y = "Frequency", x = "Furnishing Status") + theme_light()

bf_furnish_bar <- ggplot(data_bf_furnish, aes(x = bf_furnish)) + 
  geom_bar(aes(fill=bf_furnish), color="black",width = 0.7, size=1) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") +
  scale_fill_brewer(palette="Set2", name = "Furnishing Status") +
  ggtitle("Distribution of Furnishing Status based on Bachelors/Family's choice") +
  labs(y = "Frequency", x = "Furnishing Status") + theme_light()

grid.arrange(all_furnish_bar, bf_furnish_bar, nrow=2) # Extra feature (Put several ggplots into a single page)

# Analysis 4-5
# What type of area do Bachelors/Family prefer the most?

bf_areatype <- data_hr$Area.Type[data_hr$Tenant.Preferred == "Bachelors/Family"]
bf_areatype
data_bf_areatype <- data.frame(bf_areatype)
data_bf_areatype

all_areatype_bar <- ggplot(data_hr, aes(data_hr$Area.Type)) + 
  geom_bar(aes(fill=Tenant.Preferred), color="black", width = 0.7, size=1) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  scale_fill_brewer(palette="Paired", name = "Tenants Preferred") +
  ggtitle("Distribution of Area Type based on all Tenants preferred") +
  labs(y = "Frequency", x = "Area Type") + theme_light()

bf_areatype_bar <- ggplot(data_bf_areatype, aes(x = bf_areatype)) + 
  geom_bar(aes(fill=bf_areatype), color="black",width = 0.7, size=1) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") +
  scale_fill_brewer(palette="Paired", name = "Furnishing Status") +
  ggtitle("Distribution of Area Type based on Bachelors/Family's choice") +
  labs(y = "Frequency", x = "Area Type") + theme_light()

grid.arrange(all_areatype_bar, bf_areatype_bar, nrow=2) # Extra feature (Put several ggplots into a single page)

# Analysis 4-6
# In which city do Bachelors/Family prefer their houses to be located the most?

bf_city <- data_hr$City[data_hr$Tenant.Preferred == "Bachelors/Family"]
bf_city
data_bf_city<- data.frame(bf_city)
data_bf_city

all_city_bar <- ggplot(data_hr, aes(data_hr$City)) + 
  geom_bar(aes(fill=Tenant.Preferred), color="black", width = 0.7, size=1) +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
  scale_fill_brewer(palette="RdYlGn", name = "Tenants Preferred") +
  ggtitle("Distribution of Cities based on all Tenants preferred") +
  labs(y = "Frequency", x = "City") + theme_light()

bf_city_bar <- ggplot(data_bf_city, aes(x = bf_city)) + 
  geom_bar(aes(fill=bf_city), color="black",width = 0.7, size=1) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") +
  scale_fill_brewer(palette="RdYlGn", name = "City") +
  ggtitle("Distribution of Cities based on Bachelors/Family's choice") +
  labs(y = "Frequency", x = "City") + theme_light()

grid.arrange(all_city_bar, bf_city_bar, nrow=2) # Extra feature (Put several ggplots into a single page)

# Question 5
# According to the furnishing status of a house, what are the factors for tenants to choose so?

# Analysis 5-1
# Plot a pie chart for the distribution of furnishing status.

furnished <- nrow(data_hr[furnish_type == "Furnished",])
furnished

semi_furnished <- nrow(data_hr[furnish_type == "Semi-Furnished",])
semi_furnished

unfurnished <- nrow(data_hr[furnish_type == "Unfurnished",])
unfurnished

furn_df <- c(furnished, semi_furnished, unfurnished)
percentage <- round(furn_df/sum(furn_df)*100)
names = c("Furnished", "Semi-Furnished", "Unfurnished")
label = paste(names,"-",percentage,"%", sep = "")
pie3D(furn_df,labels=label,explode=0.1,main="Furnishing Status")
  
# Analysis 5-2
# How does the furnishing status vary with rent prices?

rentfurn_groupden <- ggplot(data=data_hr, aes(x=Rent, group=furnish_type, fill=furnish_type)) +
  geom_density(adjust=1.5, alpha=.4, color="black", size=.75) +
  scale_fill_manual(values = c("green", "yellow", "red"), name = "Furnishing Status") + 
  ggtitle("Distribution between Furnishing Status and Rent Prices") + labs(y="Density") +
  theme_light()

rentfurn_facetden <- rentfurn_groupden + facet_wrap( ~ data_hr$Furnishing.Status) 

rentfurn_grouphist <- ggplot(data=data_hr, aes(x=Rent, colour=furnish_type, fill=furnish_type)) +
  geom_histogram(alpha=.5, position="identity", color="black",lwd = 0.75,
                 linetype = 1) +
  scale_fill_manual(values = c("green", "yellow", "red"), name = "Furnishing Status") + 
  ggtitle("Distribution between Furnishing Status and Rent Prices") + labs(y="Frequency") +
  theme_light()

rentfurn_facethist <- rentfurn_grouphist + facet_wrap( ~ data_hr$Furnishing.Status) 

grid.arrange(rentfurn_grouphist, rentfurn_facethist, rentfurn_groupden, rentfurn_facetden, ncol=2, nrow=2)

# Analysis 5-3
# How does the furnishing status vary with house size?

sizefurn_groupden <- ggplot(data=data_hr, aes(x=Size, group=furnish_type, fill=furnish_type)) +
  geom_density(adjust=1.5, alpha=.4, color="black", size=.7) +
  scale_fill_manual(values = c("red", "#56B4E9", "#E69F00"), name = "Furnishing Status") + 
  ggtitle("Distribution between Furnishing Status and House Sizes") + labs(y="Density") +
  theme_light()

sizefurn_facetden <- sizefurn_groupden + facet_wrap( ~ data_hr$Furnishing.Status) 
sizefurn_grouphist <- ggplot(data=data_hr, aes(x=Size, colour=furnish_type, fill=furnish_type)) +
  geom_histogram(alpha=.5, position="identity", color="black",lwd = 0.75,
                 linetype = 1,) +
  scale_fill_manual(values = c("red", "#56B4E9", "#E69F00"), name = "Furnishing Status") + 
  ggtitle("Distribution between Furnishing Status and House Sizes") + labs(y="Frequency") +
  theme_light()

sizefurn_facethist <- sizefurn_grouphist + facet_wrap( ~ data_hr$Furnishing.Status) 

grid.arrange(sizefurn_grouphist, sizefurn_facethist, sizefurn_groupden, sizefurn_facetden, ncol=2, nrow=2)

# Analysis 5-4
# How does the furnishing status vary with area type?

areafurn_groupbar <- ggplot(data=data_hr, aes(x=area_type)) +
  geom_bar(aes(fill=furnish_type), position = "dodge", color="black", lwd =0.75, linetype=1) +
  scale_fill_manual(values = c("purple", "orange", "blue"), name = "Furnishing Status") + 
  ggtitle("Distribution between Furnishing Status and Area Type") + labs(x="Area Type", y="Frequency") +
  theme_light()

areafurn_facetbar <- areafurn_groupbar + facet_wrap( ~ data_hr$Area.Type) 

grid.arrange(areafurn_groupbar, areafurn_facetbar, nrow=2)

# Analysis 5-5
# How does the furnishing status vary with the number of BHKs?

bhkfurn_groupden <- ggplot(data=data_hr, aes(x=BHK, group=furnish_type, fill=furnish_type)) +
  geom_density(adjust=1.5, alpha=.4, color="black", size=.7) +
  scale_fill_manual(values = c("purple", "orange", "blue"), name = "Furnishing Status") + 
  ggtitle("Distribution between Furnishing Status and Number of BHKs") + labs(y="Density") +
  theme_light()

bhkfurn_facetden <- bhkfurn_groupden + facet_wrap( ~ data_hr$Furnishing.Status) 

bhkfurn_grouphist <- ggplot(data=data_hr, aes(x=BHK, colour=furnish_type, fill=furnish_type)) +
  geom_bar(position="dodge", color="black",lwd = 0.75,
                 linetype = 1,) +
  scale_fill_manual(values = c("purple", "orange", "blue"), name = "Furnishing Status") + 
  ggtitle("Distribution between Furnishing Status and Number of BHKs") + labs(y="Frequency") +
  theme_light()

bhkfurn_facethist <- bhkfurn_grouphist + facet_wrap(~ data_hr$Furnishing.Status)

grid.arrange(bhkfurn_grouphist, bhkfurn_facethist, bhkfurn_groupden, bhkfurn_facetden, ncol=2, nrow=2)

# Analysis 5-6
# How does the furnishing status vary with number of Bathrooms?

bathfurn_grouphist <- ggplot(data=data_hr, aes(x=Bathroom, colour=furnish_type, fill=furnish_type)) +
  geom_bar(stat=, position="dodge", color="black",lwd = 0.75,
           linetype = 1,) +
  scale_fill_manual(values = c("navy", "yellow", "grey"), name = "Furnishing Status") + 
  ggtitle("Distribution between Furnishing Status and Number of Bathrooms") + labs(y="Frequency") +
  theme_light()

bathfurn_facethist <- bathfurn_grouphist + facet_wrap(~data_hr$Furnishing.Status) 

grid.arrange(bathfurn_grouphist, bathfurn_facethist, nrow=2)

# Question 6
# How does each month affect a tenant's preference on renting a type of house?

# Analysis 6-1
# Which month is the most preferable for tenants to rent?

tenantmonth_bar <- ggplot(data_hr, aes(x = Month, fill=tenant_types)) +
  geom_bar(position = "dodge", color="black", size=1) +
  scale_fill_brewer(palette="Set1", name = "Tenants Preferred") + theme_classic() +
  ggtitle("Frequencies of Rented Houses during Different Months") +
  labs(y="Frequency")

tenantmonth_facet <- tenantmonth_bar + facet_wrap(~data_hr$Tenant.Preferred)

grid.arrange(tenantmonth_bar,tenantmonth_facet, nrow=2)

# Analysis 6-2
# How do rent prices vary between each month?

ggplot(data_hr, aes(x=Month, y=Rent, fill=Month)) + 
  geom_violin(trim=FALSE, color="black", size=.75) + #Extra feature (Violin Diagram)
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + stat_summary(fun.y=median, geom="point", size=2, color="red") +
  ggtitle("Range of Rent Prices between each Month")

# Analysis 6-3
# How do the range of house sizes vary between each month? 

ggplot(data_hr, aes(x=Month, y=Size, fill=Month)) + 
  geom_violin(trim=FALSE, color="black", size=.75) + #Extra feature (Violin Diagram)
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + stat_summary(fun.y=median, geom="point", size=2, color="red") +
  scale_fill_brewer(palette="Spectral") +
  ggtitle("Range of House Sizes between each Month")

# Analysis 6-4
# How do types of areas of a house vary between each month?

areamonth_bar <- ggplot(data_hr, aes(x=Month)) +
  geom_bar(aes(fill=area_type), position = "dodge", color="black", lwd =0.75, linetype=1) +
  scale_fill_manual(values = c("blue", "orange", "purple"), name = "Area Type") + 
  ggtitle("Distribution between Month and Area Type") + labs(x="Month", y="Frequency") +
  theme_light()

areamonth_facet <- areamonth_bar + facet_wrap( ~ data_hr$Area.Type) 

grid.arrange(areamonth_bar, areamonth_facet, nrow=2)

# Analysis 6-5
# Which city do tenants prefer to stay in the most between each month?

citymonth_bar <- ggplot(data_hr, aes(x=Month)) +
  geom_bar(aes(fill=City), position = "dodge", color="black", lwd =0.75, linetype=1) +
  scale_fill_brewer(palette = "Set1", name = "City ") + 
  ggtitle("Distribution between Month and City") + labs(x="Month", y="Frequency") +
  theme_light()

citymonth_facet <- citymonth_bar + facet_wrap( ~ data_hr$City) 

grid.arrange(citymonth_bar, citymonth_facet, nrow=2)

# Analysis 6-6
# How do types of furnishing status of a house vary between each month?

furnmonth_bar <- ggplot(data_hr, aes(x=Month)) +
  geom_bar(aes(fill=furnish_type), position = "dodge", color="black", lwd =0.75, linetype=1) +
  scale_fill_brewer(palette = "Spectral",name = "Furnishing Status") + 
  ggtitle("Distribution between Month and Furnishing Status") + labs(x="Month", y="Frequency") +
  theme_light()

furnmonth_facet <- furnmonth_bar + facet_wrap( ~ data_hr$Furnishing.Status) 

grid.arrange(furnmonth_bar, furnmonth_facet, nrow=2)

# Analysis 6-7
# How do number of BHKs' present in a house vary between each month?

ggplot(data_hr, aes(x=Month, y=BHK, fill=Month)) +  
  geom_violin(trim=FALSE, color="black", size=.75) + #Extra feature (Violin Diagram)
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + stat_summary(fun.y=median, geom="point", size=2, color="red") +
  scale_fill_brewer(palette = "Accent") +
  ggtitle("Range of Number of BHKs between each Month")

# Analysis 6-8
# How do number of bathrooms present in a house vary between each month?

ggplot(data_hr, aes(x=Month, y=Bathroom, fill=Month)) + 
  geom_violin(trim=FALSE, color="black", size=.75) + #Extra feature (Violin Diagram)
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) + stat_summary(fun.y=median, geom="point", size=2, color="red") +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Range of Number of Bathrooms between each Month")

# Question 7
# Which type of tenants do owners prefer to allow rental the most?

# Analysis 7-1
# Plot a pie chart for owner preferred tenant types.

owner_bachelor <- nrow(data_hr[(data_hr$Point.of.Contact== "Contact Owner") & (data_hr$Tenant.Preferred == "Bachelors"),])
owner_bachelor
owner_bachfam <- nrow(data_hr[(data_hr$Point.of.Contact== "Contact Owner") & (data_hr$Tenant.Preferred == "Bachelors/Family"),])
owner_bachfam
owner_family <- nrow(data_hr[(data_hr$Point.of.Contact== "Contact Owner") & (data_hr$Tenant.Preferred == "Family"),])
owner_family

owner_tenant_df <- c(owner_bachelor,owner_bachfam,owner_family)
percentage <- round(owner_tenant_df/sum(owner_tenant_df)*100)
names = c("Bachelors", "Bachelors/Family", "Family")
label = paste(names,"-",percentage,"%", sep = "")
pie(owner_tenant_df,labels=label,main="Owner Tenants Preferred", clockwise = TRUE, radius=1)

# Analysis 7-2
# Plot a bar graph for distribution between tenant preferred types and owner.

owner_tenant <- data_hr$Tenant.Preferred[data_hr$Point.of.Contact == "Contact Owner"]
data_owner_tenant<- data.frame(owner_tenant)

ggplot(data=data_owner_tenant, aes(x=owner_tenant, fill=owner_tenant)) +
  geom_bar(color="black", size=1, width=.75) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  scale_fill_brewer(palette="Set1",  name = "Tenants Preferred") + 
  ggtitle("Frequency of Different Types of Tenants Preferred of Owner's Houses") + labs(y="Frequency", x="Tenants Preferred") +
  theme_light()

# Question 8
# What type of houses do owners have the most that is available for rent?

# Analysis 8-1
# How many BHK's present the most in owners' houses that is available for rent?

data_hr$BHK <- as.character(data_hr$BHK)
class(data_hr$BHK)

owner_BHK <- data_hr$BHK[data_hr$Point.of.Contact == "Contact Owner"]

data_owner_BHK<- data.frame(owner_BHK)
data_owner_BHK

BHKowner_groupbar <- ggplot(data=data_owner_BHK, aes(x=owner_BHK, fill=owner_BHK)) +
  geom_bar(color="black", size=1, width=.75) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") +
  scale_fill_brewer(palette="Spectral",  name = "Number of BHK") + 
  ggtitle("Frequency of different number of BHKs present in owner's houses") + labs(y="Frequency", x="Owner's House BHK") +
  theme_light()

# Analysis 8-2
# How much is the cost of renting do owners' mostly put for rent?

owner_rent <- data_hr$Rent[data_hr$Point.of.Contact == "Contact Owner"]
data_owner_rent <- data.frame(owner_rent)

ownerrent_hist <- ggplot(data=data_owner_rent, aes(x=owner_rent, fill=..count..)) +
  geom_histogram(color="black", size=.7) +
  scale_fill_gradient("Frequency", low="green", high="darkgreen") + 
  ggtitle("Frequency Distribution of Rent Prices of Owners' Houses") + labs(y="Frequency", x="Owners' House Rent") +
  theme_light()

ownerrent_den <- ggplot(data=data_owner_rent, aes(x=owner_rent)) + 
  geom_density(colour="darkgreen", fill="green", alpha=0.8, size=1) +
  ggtitle("Frequency Distribution of Rent Prices of Owners' Houses") +
  labs(y="Density", x="Owners' House Rent") + theme_light()

grid.arrange(ownerrent_hist, ownerrent_den, nrow=2)

# Analysis 8-3
# In terms of house sizes, how big do owners mostly have?

owner_size <- data_hr$Size[data_hr$Point.of.Contact == "Contact Owner"]
data_owner_size <- data.frame(owner_size)

ownersize_hist <- ggplot(data=data_owner_size, aes(x=owner_size, fill=..count..)) +
  geom_histogram(color="black", size=.7) +
  scale_fill_gradient("Frequency", low="lightblue", high="darkblue") + 
  ggtitle("Frequency Distribution of House Sizes of Owners' Houses") + labs(y="Frequency", x="Owners' House Sizes") +
  theme_light()

ownersize_den <- ggplot(data=data_owner_size, aes(x=owner_size)) + 
  geom_density(colour="darkblue", fill="lightblue", alpha=0.8, size=1) +
  ggtitle("Frequency Distribution of House Sizes of Owners' Houses") +
  labs(y="Density", x="Owners' House Sizes") + theme_light()

grid.arrange(ownersize_hist, ownersize_den, nrow=2)

# Analysis 8-4
# What area type of houses do owners mostly put for rent?

owner_area <- data_hr$Area.Type[data_hr$Point.of.Contact == "Contact Owner"]
data_owner_area <- data.frame(owner_area)

ggplot(data=data_owner_area, aes(x=owner_area, fill=owner_area)) +
  geom_bar(color="black", size=1, width=.75) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black") +
  scale_fill_brewer(palette="Spectral",  name = "Area Type") + 
  ggtitle("Frequency of different Area Types of owner's houses") + labs(y="Frequency", x="Area Type") +
  theme_light()

# Analysis 8-5
# In which city are owners' houses mostly located?

owner_city <- data_hr$City[data_hr$Point.of.Contact == "Contact Owner"]
data_owner_city<- data.frame(owner_city)

ggplot(data=data_owner_city, aes(x=owner_city, fill=owner_city)) +
  geom_bar(color="black", size=1, width=.75) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  scale_fill_brewer(palette="Dark2",  name = "City") + 
  ggtitle("Frequency of Owner's Houses in different Cities") + labs(y="Frequency", x="City") +
  theme_light()

# Analysis 8-6
# Are most of owners' houses put for rent have been fully furnished or vice versa?

owner_furn <- data_hr$Furnishing.Status[data_hr$Point.of.Contact == "Contact Owner"]
data_owner_furn<- data.frame(owner_furn)

ggplot(data=data_owner_furn, aes(x=owner_furn, fill=owner_furn)) +
  geom_bar(color="black", size=1, width=.75) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  scale_fill_brewer(palette="BrBG",  name = "Furnishing Status") + 
  ggtitle("Frequency of Different Furnishing Status of Owner's Houses") + labs(y="Frequency", x="Furnishing Status") +
  theme_light()

# Question 9
# Do numbers of bathrooms and BHK in a house affect rent prices?

# Analysis 9-1
# How does number of BHK affect rent prices?

data_hr$BHK <- as.character(data_hr$BHK)
class(data_hr$BHK)
  
ggplot(data_hr, aes(x=BHK, y=Rent, fill=BHK)) +
  geom_boxplot(outlier.shape = NA, color="black", size=.8) +
  ggtitle("Relationship between BHK and Rent") + theme_classic()

# Analysis 9-2
# How does number of Bathrooms affect rent prices?

data_hr$Bathroom <- as.character(data_hr$Bathroom)
class(data_hr$Bathroom)

ggplot(data_hr, aes(x=Bathroom, y=Rent, fill=Bathroom)) +
  geom_boxplot(outlier.shape = NA, color="black", size=.8) +
  ggtitle("Relationship between Bathroom and Rent") + theme_classic()

# Association Rule Mining Algorithm:

# Download necessary tools:

library(readxl)
library(arules)
library(arulesViz)

# Read the data from Excel:

dataPd = read_excel('C:/Users/kubra/Desktop/Supported Pd Catalysts Model Data.xlsx')


#Select columns for model (size from chemisorption is selected instead of dispersion)

dataPd <- dataPd[,c(1,5,6,8,11,13,14,15,17,20,23,33)]
dataPd <- dataPd[dataPd$size_chem != "NA",]


# Convert other columns into numeric:

columns <- c(2,3,5,8,9,10,11,12)
dataPd[, columns] <- lapply(columns, function(x) as.numeric(dataPd[[x]]))

glimpse(dataPd)


# Print data and convert into factors for Method, Precursor, 
#   Support, pH adjustment, Reduction Solvent/Gas: 

glimpse(dataPd)
names <- c(1,4,6,7)
dataPd[,names] <- lapply(dataPd[,names] , factor)



# Size Data is categorized into 4 groups as <2nm, 2-5 nm,  5-10 nm, and >10 nm:

dataPd[,12] <- as.factor(ifelse(dataPd[,12]<20,"1",ifelse(dataPd[,12]<50,"2",ifelse(dataPd[,12]<100,"3","4"))))


# Synthesis Method Categories:

dataPd[,1] <- as.factor(ifelse(dataPd[,1]=="Wet Impregnation","1",ifelse(dataPd[,1]=="Incipient wetness impregnation","2",
              ifelse(dataPd[,1]=="Deposition-precipitation","3",ifelse(dataPd[,1]=="Liquid-phase reduction","4",
              ifelse(dataPd[,1]=="Commercial","5",ifelse(dataPd[,1]=="Sol-gel","6","7")))))))

# Metal Loading: Less than 1 wt% is 1, Between 1-3 wt% is 2, Between 3-5 wt% is 3,
# Between 5-10 wt% is 4, Between 10-20 is 5, Higher than 20 wt% is 6:


dataPd[,2] <-as.factor(ifelse(dataPd[,2]<1,"1",ifelse(dataPd[,2]<3,"2",ifelse(dataPd[,2]<5,"3",
                        ifelse(dataPd[,2]<10,"4",ifelse(dataPd[,2]<20,"5","6"))))))
            
# Support SA: Less than 10 is 1, between 10-50 is 2, between 50-100 is 3, between 100-500 is 4,
#    between 500-1000 is 5, more than 1000 is 6:


dataPd[,3] <- as.factor(ifelse(dataPd[,3]<10,"1",ifelse(dataPd[,3]<50,"2",ifelse(dataPd[,3]<100,"3",
                        ifelse(dataPd[,3]<500,"4",ifelse(dataPd[,3]<1000,"5","6"))))))

# Support: Carbon 1, gamma-Al2O3 2, SiO2 4,
#     mixed-Al2O3 3, TiO2 5, Others 6:
                        
dataPd[,4] <- as.factor(ifelse(dataPd[,4]=="C","1",ifelse(dataPd[,4]=="gamma-Al2O3","2",
                        ifelse(dataPd[,4]=="SiO2","4",ifelse(dataPd[,4]=="mixed-Al2O3","3",
                        ifelse(dataPd[,4]=="TiO2","5","6"))))))

# Support Calcination Temperature: Less than 300K is 1, between 500K is 2, between 600K is 3, between 700K is 4,
#    between 800K is 5, more than 800K is 6:  


dataPd[,5] <- as.factor(ifelse(dataPd[,5]<300,"1",ifelse(dataPd[,5]<500,"2",ifelse(dataPd[,5]<600,"3",
                                    ifelse(dataPd[,5]<700,"4",ifelse(dataPd[,5]<800,"5","6"))))))
                        
                        
# Precursor: PdCl2 is 1, Pd(NO3)2 is 2, Pd(acac)2 is 3,
#     (NH3)4Pd(NO3)2 is 4, Pd(OAc)2 is 5, Others 6:

dataPd[,6] <- as.factor(ifelse(dataPd[,6]=="PdCl2","1",ifelse(dataPd[,6]=="Pd(NO3)2","2",
                        ifelse(dataPd[,6]=="Pd(acac)2","3",ifelse(dataPd[,6]=="Pd(NH3)4(NO3)2","4",
                        ifelse(dataPd[,6]=="Pd(OAc)2","5","6"))))))

# Solvent:

dataPd[,7] <- as.factor(ifelse(dataPd[,7]=="water","1",ifelse(dataPd[,7]=="aqueous hydrochloric acid","2",
                        ifelse(dataPd[,7]=="HCl","3",ifelse(dataPd[,7]=="acetone","4",
                        ifelse(dataPd[,7]=="toluene","5","6"))))))

# solvent pH:

dataPd[,8] <- as.factor(ifelse(dataPd[,8]<7,"1",ifelse(dataPd[,8]>7,"2","3")))

# support PZC:

dataPd[,9] <- as.factor(ifelse(dataPd[,9]<7,"1",ifelse(dataPd[,9]>7,"2","3")))
                        
                         
                         
# Calcination Temperature: Less than 300K is 1, between 500K is 2, between 600K is 3, between 700K is 4,
#    between 800K is 5, more than 800K is 6:  


dataPd[,10] <- as.factor(ifelse(dataPd[,10]<300,"1",ifelse(dataPd[,10]<500,"2",ifelse(dataPd[,10]<600,"3",
                         ifelse(dataPd[,10]<700,"4",ifelse(dataPd[,10]<800,"5","6"))))))


# Reduction Temperature: Less than 300K is 1, between 500K is 2, between 600K is 3, between 700K is 4,
#    between 800K is 5, more than 800K is 6:   


dataPd[,11] <- as.factor(ifelse(dataPd[,11]<300,"1",ifelse(dataPd[,11]<500,"2",ifelse(dataPd[,11]<600,"3",
                         ifelse(dataPd[,11]<700,"4",ifelse(dataPd[,11]<800,"5","6"))))))



dataPd_2nm <-dataPd[dataPd$size_chem==1,]

dataPd_5nm <-dataPd[dataPd$size_chem==2,]

dataPd_10nm <-dataPd[dataPd$size_chem==3,]

dataPd_others <-dataPd[dataPd$size_chem==4,]


dataPd_2nm  <- dataPd_2nm[,c(1,2,3,4,5,6,7,8,9,10,11)]
dataPd_5nm  <- dataPd_5nm[,c(1,2,3,4,5,6,7,8,9,10,11)]
dataPd_10nm  <- dataPd_10nm[,c(1,2,3,4,5,6,7,8,9,10,11)]
dataPd_others  <- dataPd_others[,c(1,2,3,4,5,6,7,8,9,10,11)]


# Relations for Size less than 2 nm:


rules_2nm <- apriori(dataPd_2nm,parameter = list(minlen=2,maxlen=2,supp=.001, conf=.001),control = list(verbose=F))
rules_df1 <- DATAFRAME(rules_2nm, setStart = '', setEnd = '', itemSep = ' + ')
rules_df1$count <- NULL
head(rules_df1)
inspectDT(rules_df1)
ARMresultfile1 <- write_xlsx(rules_df1)


# Relations for Size less than 5 nm:


rules_5nm <- apriori(dataPd_5nm,parameter = list(minlen=2,maxlen=2,supp=.001, conf=.001),control = list(verbose=F))
rules_df2 <- DATAFRAME(rules_5nm, setStart = '', setEnd = '', itemSep = ' + ')
rules_df2$count <- NULL
head(rules_df2)
inspectDT(rules_df2)
ARMresultfile2 <- write_xlsx(rules_df2)

# Relations for Size less than 10 nm:


rules_10nm <- apriori(dataPd_10nm,parameter = list(minlen=2,maxlen=2,supp=.001, conf=.001),control = list(verbose=F))
rules_df3 <- DATAFRAME(rules_10nm, setStart = '', setEnd = '', itemSep = ' + ')
rules_df3$count <- NULL
head(rules_df3)
inspectDT(rules_df3)
ARMresultfile3 <- write_xlsx(rules_df3)

# Relations for Size more than 10 nm:


rules_others <- apriori(dataPd_others,parameter = list(minlen=2,maxlen=2,supp=.001, conf=.001),control = list(verbose=F))
rules_df4 <- DATAFRAME(rules_others, setStart = '', setEnd = '', itemSep = ' + ')
rules_df4$count <- NULL
head(rules_df4)
inspectDT(rules_df4)
ARMresultfile4 <- write_xlsx(rules_df4)


# DIFFERENT GRAPH TYPES:

plot.new()

# Graph 1:


plot(rules_2nm, method="scatter",measure = c("support", "lift"), shading = "confidence", 
     engine="ggplot2")


plot(rules_5nm, method="scatter",measure = c("support", "lift"), shading = "confidence", 
     engine="ggplot2")

plot(rules_10nm, method="scatter",measure = c("support", "lift"), shading = "confidence", 
     engine="ggplot2")


plot(rules_others, method="scatter",measure = c("support", "lift"), shading = "confidence", 
     engine="ggplot2")


# Graph 2:

plot(rules_2nm, measure=c("support","lift"),engine = "ggplot2", main = NULL, limit = 100) +
  scale_color_gradient2(low = "red", mid = "orange", high = "blue",
                        midpoint = 1, limits = c(0,20)) +
  labs(x ="Support", y = "Lift", color = "Confidence") + 
  theme_classic()

# Graph 3:

subrules <- head(rules_2nm, n = 10, by = "lift")
plot(subrules, method = "graph")

# Graph 4:

plot(rules_2nm, method = "two-key plot")

# Graph 5:

selected_rules <- subset(rules_2nm, subset = lift > 1)
plot(selected_rules, method = "grouped")

# Graph 6: 

selected_rules <- subset(rules_2nm, subset = lift > 5)
plot(selected_rules, method = "graph",  engine = "htmlwidget")










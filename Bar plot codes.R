#Author:- Pratik Pathade

####################### Codes for Rizwana Ma'am  ######################################

library(xlsx)
library(ggplot2)

pathway_data=read.xlsx("Downloads/graph representation.xlsx",sheetName = "Sheet2")

pdf("Pathway Aalysis.pdf ",height=5,width=6)
ggplot(pathway_data, aes(x = (Log2FC), y = KO_level3, size = Log2FC,fill=p.val)) +
    geom_point(shape=21, alpha = 0.6) +  # Customize bubble color and transparency
    scale_size_continuous(range = c(1,6)) + 
    scale_fill_viridis_c() +
    # Adjust the range of bubble sizes
    labs(
      x = "Log2FC",size=10,
      y = "Pathways",size=8,
      size = "Log2FC",
      fill="p.val")+                     # Customize axis and legend labels
    theme_bw()+
    
    theme( axis.text.x = element_text(colour = "black", size = 8, face = "bold", angle = 00, vjust = 0.3, hjust = 1,),
           axis.text.y = element_text(colour = "black", size = 8, face = "bold", angle = 00, vjust = 0.3, hjust = 1) )+
    theme(legend.title = element_text(face = "bold",size=10))+guides(size = guide_legend(title = "Log2FC")) 
  # Add labels inside the bubbles (adjust vjust as needed)
  
dev.off()
  
  
  
  
  
  
  
  
  ########################### Pi charts
  
Class_data=read.xlsx("Downloads/graph representation.xlsx",sheetName = "Sheet3")
  

sizes <- sizes[which(sizes > 0)]

# Create labels
labels <- letters[1:length(sizes)]

# Create pie chart
pie(sizes, labels = labels, main = "Pie Chart without NAs and Negative Values")
  
pdf("Phylum.pdf",height=5,width=6)
pie( Class_data$NA., labels = Class_data$Phylum, main = "Phylum")
dev.off()
  
pdf("Class.pdf",height=5,width=6)
pie( Class_data$NA..1, labels = Class_data$Class, main = "Class")
dev.off()

pdf("Order.pdf",height=5,width=6)
pie( Class_data$NA..2, labels = Class_data$Order, main = "Order")
dev.off()

pdf("Family.pdf",height=5,width=6)
pie( Class_data$NA..3, labels = Class_data$Family, main = "Family")
dev.off()

pdf("Genus.pdf",height=5,width=6)
pie( Class_data$NA..4, labels = Class_data$Genus, main = "Genus")
dev.off()

pdf("Species.pdf",height=5,width=6)
pie( Class_data$NA..5, labels = Class_data$Species, main = "Species")
dev.off()
  



##################### Histogram ################################################

library(ggplot2)
library(tidyverse)
# Create a data frame with the Phylum-Class pairs and their corresponding values




####################### Working ################################################
library(tidyverse)

# Convert the data to long format
data_long <- data %>%
  gather(Type, Value, Phylum_Value, Class_Value) %>%
  mutate(Categories = ifelse(Type == "Phylum_Value", Phylum, Class),
         Type = ifelse(Type == "Phylum_Value", "Phylum", "Class"))

# Reorder the columns
data_long <- data_long[, c("Categories", "Type", "Value")]

# Print the long format data
print(data_long)










data <- data.frame(
  Phylum = c("Bacteroidota", "Firmicutes", "Proteobacteria", "Fusobacteriota", "Actinobacteriota"),
  Class = c("Bacteroidia", "Clostridia", "Alphaproteobacteria", "Fusobacteriia", "Actinobacteria"),
  Order = c("Bacteroidales", "Lactobacillales", "Enterobacterales", "Fusobacteriales", "Bifidobacteriales"),
  Family = c("Bacteroidaceae", "Streptococcaceae", "Enterobacteriaceae", "Fusobacteriaceae", "Bifidobacteriaceae"),
  Genus = c("Bacteroides", "Streptococcus", "Escherichia-Shigella", "Fusobacterium", "Bifidobacterium"),
  Species = c("uncultured_bacterium", "unclassified", "Escherichia_coli", "uncultured_bacterium", "unclassified"),
  Phylum_Value = c(238953.66, 201560.62, 163325.28, 47629.26, 17458.1),
  Class_Value = c(238953.54, 159667.74, 3362.7, 47629.26, 12896.12),
  Order_Value = c(238478.38, 66580.66, 124923.06, 47629.26, 10369.38),
  Family_Value <- c(134181.06, 66573.52, 121805.2, 40916.1, 10369.38),
  Genus_Value <- c(111516.3, 8986.86, 100129.46, 40916.1, 10366.7),
  Species_Value <- c(96963.06, 23995.92, 52140.06, 21692.56, 7601.22)
  
)


###############################################################################




#############################################################################


gg <- ggplot(data)
gg <- gg + geom_bar(aes(x = Year, y = Value, fill = Category), position = "stack", stat = "identity")
gg <- gg + scale_x_continuous(breaks=c(1:10))
gg <- gg + theme
print(gg)










################################# Working ######################################



data <- data.frame(
  Phylum = c("Bacteroidota", "Firmicutes", "Proteobacteria", "Fusobacteriota", "Actinobacteriota"),
  Class = c("Bacteroidia", "Clostridia", "Alphaproteobacteria", "Fusobacteriia", "Actinobacteria"),
  Order = c("Bacteroidales", "Lactobacillales", "Enterobacterales", "Fusobacteriales", "Bifidobacteriales"),
  Family = c("Bacteroidaceae", "Streptococcaceae", "Enterobacteriaceae", "Fusobacteriaceae", "Bifidobacteriaceae"),
  Genus = c("Bacteroides", "Streptococcus", "Escherichia-Shigella", "Fusobacterium", "Bifidobacterium"),
  Species = c("uncultured_bacterium", "unclassified", "Escherichia_coli", "uncultured_bacterium", "unclassified"),
  Phylum_Value = c(238953.66, 201560.62, 163325.28, 47629.26, 17458.1),
  Class_Value = c(238953.54, 159667.74, 3362.7, 47629.26, 12896.12),
  Order_Value = c(238478.38, 66580.66, 124923.06, 47629.26, 10369.38),
  Family_Value = c(134181.06, 66573.52, 121805.2, 40916.1, 10369.38),
  Genus_Value = c(111516.3, 8986.86, 100129.46, 40916.1, 10366.7),
  Species_Value = c(96963.06, 23995.92, 52140.06, 21692.56, 7601.22)
)

data_long <- data %>%
  gather(Type, Value, Phylum_Value, Class_Value, Order_Value, Family_Value, Genus_Value, Species_Value) %>%
  mutate(Categories = ifelse(Type == "Phylum_Value", Phylum,
                             ifelse(Type == "Class_Value", Class,
                                    ifelse(Type == "Order_Value", Order,
                                           ifelse(Type == "Family_Value", Family,
                                                  ifelse(Type == "Genus_Value", Genus,
                                                         ifelse(Type == "Species_Value", Species, NA)))))),
         Type = ifelse(Type == "Phylum_Value", "Phylum",
                       ifelse(Type == "Class_Value", "Class",
                              ifelse(Type == "Order_Value", "Order",
                                     ifelse(Type == "Family_Value", "Family",
                                            ifelse(Type == "Genus_Value", "Genus",
                                                   ifelse(Type == "Species_Value", "Species", NA)))))))

# Print the long format data
print(data_long)



pdf("graphs for proportion of species 1.pdf ",height=5,width=6)
ggplot(data_long, aes(x = Type, y = Value, fill = Categories)) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels=c("Phylum", "Class", "Order", "Family", "Genus", "Species")) +
  theme_minimal() +
labs(x = "", y = "Proportion of the species")
dev.off()














################################################################################


write.csv(data_long,"data_long.csv")


colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#800000", "#008000",
            "#000080", "#808000", "#800080", "#008080", "#C0C0C0", "#808080", "#9999FF", "#993366",
            "#FFFFCC", "#CCFFFF", "#660066", "#FF8080", "#0066CC", "#CCCCFF", "#000080", "#4682B4",
            "#FFFF00", "#00FFFF", "#800080", "#800000")



gg <- ggplot(data_long, aes(x = Type, y = Value, fill = Categories)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  labs(x = "", y = "Proportion of the species")

print(gg)








#################################################################################

data__ <- data.frame(
  Nutrient = c("Calories", "Protein", "Carbs", "Fat", "Fiber"),
  Normal_Range = c("1600 - 2400 (Kcal)/day", "55 - 60 g/day", "225 - 325 g/day", "44 - 77 g/day", "30 - 38 g/day"),
  Mean_SD = c(1257, 51.3, 158.4, 44.3, 19.8),
  SD = c(390, 23.1, 38.9, 16.8, 5.8)
)

# Create the bar graph
ggplot(data__, aes(x = Nutrient, y = Mean_SD, fill = Nutrient)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean_SD - SD, ymax = Mean_SD + SD), width = 0.4, position = position_dodge(0.9)) +
  labs(title = "Nutrient Content Comparison",
       y = "Mean ± SD",
       fill = "Nutrient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


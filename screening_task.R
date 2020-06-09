library(data.table)
library(ggplot2)
library(pheatmap)
library(magrittr)
library(dplyr)

basedir <- "C:/Users/Balaji Ganesh/OneDrive/Documents/Job Applications/Agendia/"
df <- fread(file.path(basedir, "Screening_Dataset.csv"))

one_two <- ggplot(data=df, mapping = aes(x=Var1, y=Var2)) + geom_point(aes(size = 2)) + labs(x = "Var1", y="Var2") + ggtitle("Plot between Variable 1 and 2") + theme(plot.title = element_text(hjust=0.5))
one_two + scale_size(guide='none')

correlation_1_2 <- cor(df$Var1, df$Var2)
correlation_2_3 <- cor(df$Var2, df$Var3, use="complete.obs")

cor_matrix <- cor(df, use="complete.obs")

pheatmap(cor_matrix, show_rownames = TRUE, show_colnames = TRUE, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE)

lm_16 <- lm(Var1 ~ Var6, df)
#plot(lm_16)

equation <- paste0("Var6 = ", lm_16$coefficients[2],"*","Var1 ","+"," ",lm_16$coefficients[1])
one_six <- ggplot(data  = df, aes(Var1, Var6)) + geom_point(aes(size = 2)) + geom_smooth(method='lm') + annotate("text", x = 0.6, y = 0, label = equation) + annotate("text", x = 0.6, y = -0.05, label = paste0("Pearson_Correlation = ", cor_matrix[1,6]))
one_six + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),plot.title = element_text(hjust = 0.5)) + scale_size(guide='none')+ggtitle("Plot between variable 1 and variable 6")

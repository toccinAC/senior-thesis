library(tidyverse)
library(tibble)
library(xtable)

Exp_result <- read.csv("./result.txt")
View(Exp_result)


ggplot(data = Exp_result, mapping = aes(x = Fiascoed, y = Pseudo, color = System_name)) + geom_point()


pseudoFound <- tibble(sys_name = Exp_result$System_name, fiascoed = Exp_result$Fiascoed, pseudo = Exp_result$Pseudo)
View(pseudoFound)
print(xtable(pseudoFound, type = "latex"), file = "pseudoTable.tex", table.placement = "H")

Exp_result$Change <- Exp_result$Function_cov - Exp_result$UC
attach(Exp_result)

tableRem <- Exp_result[0:9]
tableRem$Change <- Exp_result$Change

print(xtable(tableRem, type = "latex"), size = "tiny", file = "FullTable.tex", table.placement = "H")
Exp_result$var <- Exp_result$Pseudo/Exp_result$Fiascoed
cor(State_Cov, Pseudo)
ggscatter(Exp_result, x = "State_Cov", y = "var", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Statement Coverage", ylab = "Ratio of Pseudo/Fiascoed")
fit1 <- lm(State_Cov ~ Pseudo, data = Exp_result)
ggplot(data = Exp_result, mapping = aes(x = State_Cov, y = Pseudo)) + geom_point() + stat_smooth(method = "lm")

ggsave("stateCovPlot.pdf", plot = last_plot(), device = "pdf", path = "./images")

StringsUsed = sum(Exp_result$Num_Strings)
BooleansUsed = sum(Exp_result$Num_Boolean)
IntsUsed = sum(Exp_result$Num_Integer)
FloatsUsed = sum(Exp_result$Num_Float)

StringsPseudo = sum(Exp_result$Pseudo_Strings)
BooleansPseudo = sum(Exp_result$Pseudo_Booleans)
IntsPseudo = sum(Exp_result$Pseudo_Integer)
FloatsPseudo = sum(Exp_result$Pseudo_Float)

StringsRat = (StringsPseudo/StringsUsed)
BooleansRat = BooleansPseudo/BooleansUsed
IntsRat = IntsPseudo/IntsUsed
FloatsRat = FloatsPseudo/FloatsUsed

retTable <- tibble(Type = character(), Used = integer(), Found = integer(), ratio = numeric())
retTable <- retTable %>% add_row(Type = "Strings", Used = StringsUsed, Found = StringsPseudo, ratio = StringsRat)
retTable <- retTable %>% add_row(Type = "Booleans", Used = BooleansUsed, Found = BooleansPseudo, ratio = BooleansRat)
retTable <- retTable %>% add_row(Type = "Ints", Used = IntsUsed, Found = IntsPseudo, ratio = IntsRat)
retTable <- retTable %>% add_row(Type = "Floats", Used = FloatsUsed, Found = FloatsPseudo, ratio = FloatsRat)

retTable$ratio <- paste(round(retTable$ratio*100,digits=1),"%",sep="")
View(retTable)
print(StringsRat)
print(BooleansRat)
print(IntsRat)
print(FloatsRat)

print(xtable(retTable, type = "latex"), file = "returnTable.tex", table.placement = "H")

## Load In libraries

## Helper functions

firstVar <- function(Var1, Var2){
  ifelse(Var1 < Var2, Var1, Var2)
}

secondVar <- function(Var1, Var2){
  ifelse(Var1 < Var2, Var2, Var1)
}

## Read In Data

SpearSparp100 <- read_csv("Analysis/TaraOceansSpearmanSparCCAnalysis_p100.csv")
SpearSparp1000 <- read_csv("Analysis/TaraOceansSpearmanSparCCAnalysis_p1000.csv")
Glasso <- read_csv("Analysis/TaraOceansGLasso_out.csv")

## Flip variable order for Glassoo output so the alphabetically first variable always comes first

Glasso_Flip <- Glasso %>%
  mutate(Var1 = as.character(Var1), Var2 = as.character(Var2),
         VarA = map2_chr(Var1, Var2, firstVar), 
         VarB = map2_chr(Var1, Var2, secondVar)
  ) %>% 
  select(-Var1, -Var2) %>%
  select(VarA, VarB, everything()) %>%
  # Rename so that we remember that weight comes from Glasso
  rename(Weight.glasso = Weight)

SpearSparP1000Glasso <- left_join(SpearSparp1000, Glasso_Flip, by = c("VarA", "VarB"))
SpearSparP100Glasso <- left_join(SpearSparp100, Glasso_Flip, by = c("VarA", "VarB"))

write_csv(SpearSparP100Glasso, "Analysis/TaraOceansSpearSparP100GlassoAnalysis.csv")
write_csv(SpearSparP1000Glasso, "Analysis/TaraOceansSpearSparP1000GlassoAnalysis.csv")


## Write Data Out

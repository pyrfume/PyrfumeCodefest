#Agarwal code

df.KA <- NGS_Small %>%
  
  filter(ROSE_INT !="0") %>%
  
  select(ROSE_INT, PREG)

p <- ggplot(df.KA, aes(y=ROSE_INT, x=as.factor(PREG))) +
  
  geom_boxplot()

p


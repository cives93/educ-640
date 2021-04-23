needs(xaringan)
xaringanBuilder::build_pdf("slides/wk1-3.html")
hw1 <- read.sav(here("data/HW1_ereader.sav"), use.value.labels = T) %>% 
  janitor::clean_names()

#simple

contrast0 <- c(1, -.33, -.33, -.34) 
mat.temp <- rbind(constant = 1/2, contrast0)
mat.temp
solve(mat.temp)

mat <- solve(mat.temp)
mat
mat<- mat[ , -1]
mat

# 1 vs others
contrast0 <- c(1, -.33, -.33, -.34)
mat.temp <- rbind(constant = 1, contrast0)
mat <- MASS::ginv(mat.temp)
mat<- mat[ , -1]
?MASS::ginv()
mat

# 2nd vs others
contrast1 <- c(0, 1, -.5, -.5)
# 3rd vs last
contrast1 <- c(0, 0, -.33, -.34)


m_contrasts <- lm(reading_time ~ condition, data=hw1, contrasts = list(condition = mat))
summary(m_contrasts)


# Repeated
contrast1 <- c(1, -1, 0, 0)
contrast2 <- c(0, 1, -1, 0)
contrast3 <- c(0, 0, 1, -1)


mat.temp <- rbind(constant = 1/4, contrast1, contrast2, contrast3)
mat.temp

mat <- solve(mat.temp)
mat
mat<- mat[ , -1]
mat

m_contrasts <- lm(reading_time ~ condition, data=hw1, contrasts = list(condition = mat))
summary(m_contrasts)




# ```{r}
# needs(rstatix)
# m1 <- lm(vocab~ inst*meth, lb5)
# 
# lb5 %>%
#   group_by(meth) %>%
#   anova_test(vocab ~ inst, error = m1)
# 
# anova_test(vocab ~ inst|meth, lb5)
# m1 <- anova_test()
# m1 <- lm(vocab~ inst*meth, lb5)
# needs(lsmeans)
# lsm <- lsmeans(m1, ~inst*meth)
# lsm
# 
# c1 <- list(
#   v_i.ded = c(1, -.5, -.5, -1, .5, .5)
#   )

# contrast(lsm, c1, adjust = "holm")
# m3 <- emmeans(m1, ~  c("inst", "meth"))

# ````


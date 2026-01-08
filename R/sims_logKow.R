library(tidyverse); library(easystats)
theme_set(theme_bw(14))


# Simulations simple pour la relation entre coefficient de partage n-octanol et toxictité


n_compo = 1000 # nombre de composés
logKow = rnorm(n_compo, 2, 1)  # vecteur de Kow
b0 = -4 # Ordonnée à l'origine
b1 = .9 # Pente
sigma = .25 # variance résiduelle

log_inv_LC50 = rnorm(n_compo, b0 + b1 * logKow, sigma)

df = data.frame( # stockage
  logKow = logKow,
  log_inv_LC50 = log_inv_LC50
) 
write.csv(df, "data/data-sims/df.csv", row.names = FALSE) # Export du dataframe en fichier .csv
# Rappel pour import : df <- read.csv("data/data-sims/df.csv")
plot(log_inv_LC50 ~ logKow, df)

# regression linéaire
lm.tox.Kow = lm(log_inv_LC50 ~ logKow, df)
model_parameters(lm.tox.Kow)
saveRDS(lm.tox.Kow, "outputs/mods/lm.tox.Kow.RDS")
# lm.tox.Kow = readRDS("outputs/mods/lm.tox.Kow.RDS") charger le modele dans l'environnement de travail


# Graphique "clean"
fig_LC50_Kow_reg = df %>% 
  ggplot(aes(x = logKow, y = log_inv_LC50)) +
  geom_point(alpha = .3, size = 3) +
  geom_smooth(method = "lm") +
  xlab("log(Kow)") + ylab("log(1/LC50)")

ggsave("outputs/figs/fig_LC50_Kow_reg.png", fig_LC50_Kow_reg)

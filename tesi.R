library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(ggnewscale)
library(ggfortify)
library(reshape2)
library(gridExtra)
library(cowplot)
library(dendextend)
library(caret)
library(mclust)
library(officer)
library(flextable)
library(nnet)
library(MASS)
library(GGally)
library(boot)
library(FSA)


sheets=excel_sheets("C:/Users/Utente/Desktop/UNIVERSITA'/a.a. 2024-2025/TESI/TESI.xlsx")
malattie=list("CD(PLASMA SAMPLES)"="CD","CD(STOOL SAMPLES)"="CD",
  "CRC(PLASMA SAMPLES)"="CRC","CRC(STOOL SAMPLES)"="CRC",
  "Obese(PLASMA SAMPLES)"="Obesi","Obese(STOOL SAMPLES)"="Obesi",
  "HC(PLASMA SAMPLES)" = "HC","HC(STOOL SAMPLES)"="HC")
acidi_grassi=c("Acetico","Propionico","Butirrico","IsoButirrico","IsoValerico","2MetilButirrico","Valerico")
dati=data.frame()
for (sheet in sheets) {
  df=read_excel("C:/Users/Utente/Desktop/UNIVERSITA'/a.a. 2024-2025/TESI/TESI.xlsx", sheet = sheet)
  df$Malattia=malattie[[sheet]]
  df$Campione=ifelse(grepl("PLASMA", sheet), "Plasma", "Stool")
  dati=bind_rows(dati,df)
}
write.csv(dati, file="dati.csv", row.names=FALSE)
colnames(dati)=c("Età", "Sesso", "ID", "Acetico", "Propionico", "Butirrico", 
                    "IsoButirrico", "IsoValerico", "2MetilButirrico", "Valerico", 
                    "Malattia", "Campione")
dati[acidi_grassi] = dati[acidi_grassi] * 100

setwd("C:/Users/Utente/Desktop/UNIVERSITA'/a.a. 2024-2025/TESI/IMMAGINI")

dati_unici=dati[!duplicated(dati$ID),!(names(dati) %in% c(acidi_grassi, "Campione"))]
dati_stool=dati[dati$Campione == "Stool", -12]
dati_plasma=dati[dati$Campione == "Plasma", -12]

#dati_combinati
plasma = subset(dati, Campione == "Plasma")
feci   = subset(dati, Campione == "Stool")
colnames(plasma)[sapply(plasma, is.numeric)] = paste0(colnames(plasma)[sapply(plasma, is.numeric)], "_plasma")
colnames(feci)[sapply(feci, is.numeric)]     = paste0(colnames(feci)[sapply(feci, is.numeric)], "_feci")
dati_combinati = merge(plasma, feci, by = "ID", suffixes = c("_plasma", "_feci"))
stopifnot(all(dati_combinati$Età_plasma == dati_combinati$Età_feci))
stopifnot(all(dati_combinati$Sesso_plasma == dati_combinati$Sesso_feci))
stopifnot(all(dati_combinati$Malattia_plasma == dati_combinati$Malattia_feci))
dati_combinati$Età      = dati_combinati$Età_plasma
dati_combinati$Sesso    = dati_combinati$Sesso_plasma
dati_combinati$Malattia = dati_combinati$Malattia_plasma

dati_combinati = dati_combinati[ , !names(dati_combinati) %in% c(
  "Età_plasma", "Età_feci", 
  "Sesso_plasma", "Sesso_feci", 
  "Malattia_plasma", "Malattia_feci", 
  "Matrice_plasma", "Matrice_feci"
)]
dati_combinati = dati_combinati[ , !(names(dati_combinati) %in% c("Campione_plasma", "Campione_feci")) ]
dati_combinati$Malattia = factor(dati_combinati$Malattia)

dati_long = melt(dati,
                  id.vars = c("ID", "Campione", "Malattia"),
                  measure.vars = acidi_grassi,
                  variable.name = "Acido",
                  value.name = "Valore")
dati_stool_long = subset(dati_long, Campione == "Stool")[,-2]
dati_plasma_long = subset(dati_long, Campione == "Plasma")[,-2]


#TABELLE--------------------------------------------
#tabelle
table(dati_combinati$Malattia)
summary(dati_combinati)

dati_CD=dati_combinati[dati_combinati$Malattia == "CD", ]
dati_CRC=dati_combinati[dati_combinati$Malattia == "CRC", ]
dati_HC=dati_combinati[dati_combinati$Malattia == "HC", ]
dati_Obesi=dati_combinati[dati_combinati$Malattia == "Obesi", ]

mean(dati_CD$Età, na.rm = TRUE)
sum(dati_CD$Sesso == "F", na.rm = TRUE)
sum(dati_CD$Sesso == "M", na.rm = TRUE)

mean(dati_CRC$Età, na.rm = TRUE)
sum(dati_CRC$Sesso == "F", na.rm = TRUE)
sum(dati_CRC$Sesso == "M", na.rm = TRUE)

mean(dati_HC$Età, na.rm = TRUE)
sum(dati_HC$Sesso == "F", na.rm = TRUE)
sum(dati_HC$Sesso == "M", na.rm = TRUE)

mean(dati_Obesi$Età, na.rm = TRUE)
sum(dati_Obesi$Sesso == "F", na.rm = TRUE)
sum(dati_Obesi$Sesso == "M", na.rm = TRUE)

table(dati_combinati$Sesso)


#BOXPLOT--------------------------------------------
#boxplot stool
ragg::agg_png("boxplot_stool.png", width = 12, height = 9, units = "in", res = 300)
boxplot(dati_stool[acidi_grassi])
dev.off()

#boxplot plasma
ragg::agg_png("boxplot_plasma.png", width = 12, height = 9, units = "in", res = 300)
boxplot(dati_plasma[acidi_grassi])
dev.off()

#VIOLINPLOT-----------------------------------------
# violin plot PLASMA
ragg::agg_png("violinplot_plasma.png", width = 10, height = 5, units = "in", res = 300)
ggplot(dati_plasma_long %>% arrange(Malattia), 
       aes(x = Malattia, y = Valore, fill = Malattia)) +
  geom_violin(trim = FALSE, scale = "area") +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  facet_wrap(~Acido, nrow = 2, ncol = 4, scales = "free_y") +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "none")
dev.off()

# violin plot STOOL
ragg::agg_png("violinplot_stool.png", width = 10, height = 5, units = "in", res = 300)
ggplot(dati_stool_long, 
       aes(x = Malattia, y = Valore, fill = Malattia)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  facet_wrap(~Acido, nrow = 2, ncol = 4, scales = "free_y") +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"),
        legend.position = "none")

dev.off()

#BARPLOT--------------------------------------------
#Barplot Stool
ragg::agg_png("barplot_stool.png", width = 10, height = 5, units = "in", res = 300)
ggplot(dati_stool_long, 
       aes(x = ID, y = Valore, fill = Acido)) +
  geom_bar(stat = "identity")+
  facet_wrap(~Malattia, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = c(
    "Acetico" = "#E41A1C",
    "Propionico" = "#377EB8",
    "Butirrico" = "#4DAF4A",
    "IsoButirrico" = "#984EA3",
    "IsoValerico" = "#FF7F00",
    "2MetilButirrico" = "#FFFF33",
    "Valerico" = "#A65628"
  )) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),     # Nasconde il testo (etichette)
    axis.ticks.x = element_blank(),    # Rimuove i tick (segni verticali)
    axis.title.x = element_blank(),    # (opzionale) Rimuove il titolo asse X
    strip.text = element_text(face = "bold"))
dev.off()

#Barplot Plasma
ragg::agg_png("barplot_plasma.png", width = 10, height = 5, units = "in", res = 300)
ggplot(dati_plasma_long, 
       aes(x = ID, y = Valore, fill = Acido)) +
  geom_bar(stat = "identity")+
  facet_wrap(~Malattia, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = c(
    "Acetico" = "#E41A1C",
    "Propionico" = "#377EB8",
    "Butirrico" = "#4DAF4A",
    "IsoButirrico" = "#984EA3",
    "IsoValerico" = "#FF7F00",
    "2MetilButirrico" = "#FFFF33",
    "Valerico" = "#A65628"
  )) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),     # Nasconde il testo (etichette)
    axis.ticks.x = element_blank(),    # Rimuove i tick (segni verticali)
    axis.title.x = element_blank(),    # (opzionale) Rimuove il titolo asse X
    strip.text = element_text(face = "bold"))
dev.off()


#PCA------------------------------------------------
# PCA per Stool
pca_stool = prcomp(dati_stool[, acidi_grassi], center = TRUE, scale. = TRUE)
df_stool_pca = as.data.frame(pca_stool$x[, 1:2])
df_stool_pca$Malattia = dati_stool$Malattia

summary(pca_stool)$importance[3, ]

ragg::agg_png("screeplot_stool.png", width = 8, height = 3.33, units = "in", res = 300)
plot(pca_stool, type = "l", main = "Scree Plot - Varianza spiegata")
dev.off()

ragg::agg_png("pca_stool.png", width = 8, height = 3.33, units = "in", res = 300)
ggplot(df_stool_pca, aes(x = PC1, y = PC2, color = Malattia)) +
  geom_point(size = 2) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = c("CD" = "red", "CRC" = "green", 
                                "HC" = "blue", "Obesi" = "purple")) +
  theme_bw() +
  coord_fixed(ratio = 1)
dev.off()

# PCA per Plasma
pca_plasma = prcomp(dati_plasma[, acidi_grassi], center = TRUE, scale. = TRUE)
df_plasma_pca = as.data.frame(pca_plasma$x[, 1:2])
df_plasma_pca$Malattia = dati_plasma$Malattia

summary(pca_plasma)$importance[3, ]

ragg::agg_png("screeplot_plasma.png", width = 8, height = 3.33, units = "in", res = 300)
plot(pca_plasma, type = "l", main = "Scree Plot - Varianza spiegata")
dev.off()

ragg::agg_png("pca_plasma.png", width = 5.25, height = 6, units = "in", res = 300)
ggplot(df_plasma_pca, aes(x = PC1, y = PC2, color = Malattia)) +
  geom_point(size = 2) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = c("CD" = "red", "CRC" = "green", 
                                "HC" = "blue", "Obesi" = "purple")) +
  theme_bw() +
  coord_fixed(ratio = 1)
dev.off()



# PCA per combinati
pca = prcomp(dati_combinati[, 2:15], center = TRUE, scale. = TRUE)
df_pca = as.data.frame(pca$x[, 1:2])
df_pca$Malattia = dati_combinati$Malattia

ragg::agg_png("screeplot.png", width = 8, height = 3.33, units = "in", res = 300)
plot(pca, type = "l", main = "Scree Plot - Varianza spiegata")
dev.off()

ragg::agg_png("pca_combinati.png", width = 9, height = 7, units = "in", res = 300)
ggplot(df_pca, aes(x = PC1, y = PC2, color = Malattia)) +
  geom_point(size = 2) +
  labs(x = NULL, y = NULL) +
  scale_color_manual(values = c("CD" = "red", "CRC" = "green", 
                                "HC" = "blue", "Obesi" = "purple")) +
  theme_bw() +
  coord_fixed(ratio = 1)
dev.off()


#HEATMAP--------------------------------------------
malattie_uniche=unique(dati$Malattia)
crea_cor_heatmap_no_legend = function(data_subset, titolo) {
  dati_acidi = data_subset[, acidi_grassi]
  cor_matrix = cor(dati_acidi, use = "pairwise.complete.obs", method = "pearson")
  cor_long = melt(cor_matrix)
  
  ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, limits = c(-1, 1), name = "Correlazione") +
    labs(title = titolo, x = NULL, y = NULL) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8),
          plot.title = element_text(size = 11, face = "bold"))
}
estrai_legenda = function(plot) {
  ggdraw(get_legend(plot + theme(legend.position = "right",
                                 legend.title = element_text(size = 10),
                                 legend.text = element_text(size = 8))))
}
genera_pannello_correlazioni = function(data, tipo) {
  plots = list()
  nomi_plot = c()
    for (i in seq_len(min(5, length(malattie_uniche)))) {
    m = malattie_uniche[i]
    sotto = data[data$Malattia == m, ]
    if (nrow(sotto) >= 3) {
      plots[[length(plots) + 1]] = crea_cor_heatmap_no_legend(sotto, titolo = m)
      nomi_plot = c(nomi_plot, m)
    }
  }
  
  plots[[length(plots) + 1]] = crea_cor_heatmap_no_legend(data, titolo = "Generale")
  nomi_plot = c(nomi_plot, "Generale")
  
  legenda_plot = crea_cor_heatmap_no_legend(data, titolo = "Leggenda")
  legenda = estrai_legenda(legenda_plot + theme(legend.position = "right"))
  
  griglia = plot_grid(plotlist = plots, ncol = 3)
  finale = plot_grid(griglia, legenda, rel_widths = c(0.9, 0.1))
  
  print(finale)
}

ragg::agg_png("heatmap_stool.png", width = 8, height = 5, units = "in", res = 300)
genera_pannello_correlazioni(dati_stool, "Stool")
dev.off()

ragg::agg_png("heatmap_plasma.png", width = 8, height = 5, units = "in", res = 300)
genera_pannello_correlazioni(dati_plasma, "Plasma")
dev.off()

#COEFFICIENTI DI CORRELAZIONE-----------------------
#correlazioni tra acidi

tutti_risultati = list()
for (nome in malattie_uniche)  {
  df=get(paste0("dati_", nome))
  risultati = data.frame(
    acido = character(),
    pearson = numeric(),
    spearman = numeric(),
    stringsAsFactors = FALSE
  )
  for (acido in acidi_grassi) {
    col_plasma = paste0(acido, "_plasma")
    col_stool  = paste0(acido, "_feci")
    
    x = df[[col_plasma]]
    y = df[[col_stool]]
    pearson = cor(x,y, method = "pearson")
    spearman = cor(x, y, method = "spearman")
      
    risultati = rbind(risultati, data.frame(
      acido = acido,
      pearson = pearson,
      spearman = spearman
    ))
    }
  risultati$malattia = nome
  tutti_risultati[[nome]] = risultati
}
df_correlazioni = do.call(rbind, tutti_risultati)

#METODO WARD----------------------------------------
##WARD GENERALE

Z = scale(dati_combinati[ , sapply(dati_combinati, is.numeric) & names(dati_combinati) !=c("Età","ID","Sesso")])
rownames(Z) = dati_combinati$ID

hc = hclust(dist(Z), method = "ward.D2")
dend = as.dendrogram(hc)

ordered_ids = labels(dend)
id2malattia = setNames(as.character(dati_combinati$Malattia), dati_combinati$ID)
malattie_ordinate = id2malattia[ordered_ids]

palette_colori = c("CD"="forestgreen", "CRC"="darkorange", "HC"="royalblue", "Obesi"="firebrick")
labels_colors(dend) = palette_colori[malattie_ordinate]

ragg::agg_png("dendogramma_Ward.png", width = 12, height = 9, units = "in", res = 300)
plot(dend)
legend("topright", legend = names(palette_colori), fill = palette_colori, cex = 0.8)
rect.hclust(hc, k = 4, border = c("black", "gray40", "darkcyan", "purple"))
dev.off()

#matrici di confusione
malattie = as.factor(dati_combinati$Malattia)
gruppi = cutree(hc, k = 4)
mappa = c("1" = "CD", "2" = "CRC", "3" = "Obesi", "4" = "HC")
gruppi_mappati = factor(mappa[as.character(gruppi)], levels = levels(malattie))

conf_matrix = confusionMatrix(gruppi_mappati, malattie)
print(conf_matrix)


#Z_HC = scale(dati_HC[ , sapply(dati_HC, is.numeric) & names(dati_HC) !=c("Età","ID","Sesso")])
#rownames(Z_HC) = dati_HC$ID
#hc = hclust(dist(Z_HC), method = "ward.D2")
#dend = as.dendrogram(hc)
#colori_cluster = c("darkcyan", "purple")
#plot(dend)
#rect.hclust(hc, k = 2, border = colori_cluster)


##WARD PLASMA
Z_plasma = scale(dati_plasma[ , sapply(dati_plasma, is.numeric) & names(dati_plasma) !="Età"])
rownames(Z_plasma) = dati_plasma$ID

hc_plasma = hclust(dist(Z_plasma), method = "ward.D2")
dend_plasma = as.dendrogram(hc_plasma)

ordered_ids = labels(dend_plasma)
id2malattia = setNames(as.character(dati_plasma$Malattia), dati_plasma$ID)
malattie_ordinate = id2malattia[ordered_ids]

palette_colori = c("CD"="forestgreen", "CRC"="darkorange", "HC"="royalblue", "Obesi"="firebrick")
labels_colors(dend_plasma) = palette_colori[malattie_ordinate]

ragg::agg_png("dendogramma_Ward_plasma.png", width = 12, height = 9, units = "in", res = 300)
plot(dend_plasma)
legend("topright", legend = names(palette_colori), fill = palette_colori, cex = 0.8)
rect.hclust(hc_plasma, k = 4, border = c("black", "gray40", "darkcyan", "purple"))
dev.off()

#matrici di confusione
malattie_plasma = as.factor(dati_plasma$Malattia)
gruppi_plasma = cutree(hc_plasma, k = 4)
mappa_plasma = c("1" = "CD", "2" = "HC", "3" = "CRC", "4" = "Obesi")
gruppi_mappati_plasma = factor(mappa_plasma[as.character(gruppi_plasma)], levels = levels(malattie_plasma))

conf_matrix_plasma = confusionMatrix(gruppi_mappati_plasma, malattie_plasma)
print(conf_matrix_plasma)


##WARD STOOL
Z_stool = scale(dati_stool[ , sapply(dati_stool, is.numeric) & names(dati_stool) !="Età"])
rownames(Z_stool) = dati_stool$ID

hc_stool = hclust(dist(Z_stool), method = "ward.D2")
dend_stool = as.dendrogram(hc_stool)

ordered_ids = labels(dend_stool)
id2malattia = setNames(as.character(dati_stool$Malattia), dati_stool$ID)
malattie_ordinate = id2malattia[ordered_ids]

palette_colori = c("CD"="forestgreen", "CRC"="darkorange", "HC"="royalblue", "Obesi"="firebrick")
labels_colors(dend_stool) = palette_colori[malattie_ordinate]

ragg::agg_png("dendogramma_Ward_stool.png", width = 12, height = 9, units = "in", res = 300)
plot(dend_stool)
legend("topright", legend = names(palette_colori), fill = palette_colori, cex = 0.8)
rect.hclust(hc_stool, k = 4, border = c("black", "gray40", "darkcyan", "purple"))
dev.off()

#matrici di confusione
malattie_stool = as.factor(dati_stool$Malattia)
gruppi_stool = cutree(hc_stool, k = 4)
mappa_stool = c("1" = "CD", "2" = "Obesi", "3" = "HC", "4" = "CRC")
gruppi_mappati_stool = factor(mappa_stool[as.character(gruppi_stool)], levels = levels(malattie_stool))

conf_matrix_stool = confusionMatrix(gruppi_mappati_stool, malattie_stool)
print(conf_matrix_stool)

#TEST WRUSKAL-WALLIS--------------------------------
#test di kruskal

risultati_plasma = list()
dunn_plasma = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_plasma[, c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  
  risultati_plasma[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  
  if (p_value < 0.05) {
    dunn = dunnTest(valore ~ Malattia, data = subset, method = "bonferroni")
    dunn_df = as.data.frame(dunn$res)
    dunn_df$Acido = acido
    dunn_plasma[[acido]] = dunn_df
  }
  
  k=k+1
}
kruskal_risultati_plasma = do.call(rbind, risultati_plasma)
dunn_risultati_plasma = do.call(rbind, dunn_plasma)
print(kruskal_risultati_plasma)
print(dunn_risultati_plasma)

risultati_stool = list()
dunn_stool = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_stool[, c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  
  risultati_stool[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  
  if (p_value < 0.05) {
    dunn = dunnTest(valore ~ Malattia, data = subset, method = "bonferroni")
    dunn_df = as.data.frame(dunn$res)
    dunn_df$Acido = acido
    dunn_stool[[acido]] = dunn_df
  }
  k=k+1
}
kruskal_risultati_stool = do.call(rbind, risultati_stool)
dunn_risultati_stool = do.call(rbind, dunn_stool)

print(kruskal_risultati_stool)
print(dunn_risultati_stool)


#MODELLO REGRESSIONE LOGISTICA MULTINOMIALE---------

dati_combinati$Malattia = relevel(as.factor(dati_combinati$Malattia), ref = "HC")
dati_plasma$Malattia = relevel(as.factor(dati_plasma$Malattia), ref = "HC")
dati_stool$Malattia = relevel(as.factor(dati_stool$Malattia), ref = "HC")
dati_combinati$Sesso=factor(dati_combinati$Sesso)
dati_plasma$Sesso = factor(dati_plasma$Sesso)
dati_stool$Sesso = factor(dati_stool$Sesso)


modello_combinato = multinom(
  Malattia ~ Età + Sesso + Acetico_plasma + Propionico_plasma + Butirrico_plasma + 
    IsoButirrico_plasma +   IsoValerico_plasma + `2MetilButirrico_plasma` + Valerico_plasma + 
    Acetico_feci +   Propionico_feci + Butirrico_feci + IsoButirrico_feci + 
    IsoValerico_feci + `2MetilButirrico_feci` + Valerico_feci,
  data = dati_combinati[ , !(names(dati_combinati) %in% "ID")],
  trace = FALSE
)
modello_combinato

coefs_comb = summary(modello_combinato)$coefficients
std_err_comb = summary(modello_combinato)$standard.errors
z_scores_comb = coefs_comb / std_err_comb
p_values_comb = 2 * (1 - pnorm(abs(z_scores_comb)))


modello_plasma = multinom(
  Malattia ~ Età + Sesso + Acetico + Propionico + Butirrico + IsoButirrico +
    IsoValerico + `2MetilButirrico` + Valerico,
  data = dati_plasma[ , !(names(dati_plasma) %in% "ID")],
  trace = FALSE
)
modello_plasma

coefs_p = summary(modello_plasma)$coefficients
std_err_p = summary(modello_plasma)$standard.errors
z_scores_p = coefs_p / std_err_p
p_values_p = 2 * (1 - pnorm(abs(z_scores_p)))
odds_ratio = exp(coefs_p)

modello_stool = multinom(
  Malattia ~ Età + Sesso + Acetico + Propionico + Butirrico + IsoButirrico +
    IsoValerico + `2MetilButirrico` + Valerico,
  data = dati_stool[ , !(names(dati_stool) %in% "ID")],
  trace = FALSE
)
modello_stool

coefs_s = summary(modello_stool)$coefficients
std_err_s = summary(modello_stool)$standard.errors
z_scores_s = coefs_s / std_err_s
p_values_s = 2 * (1 - pnorm(abs(z_scores_s)))
odds_ratio_s = exp(coefs_s)


#CROSS-VALIDATION--------------------------------------
#CROSS-VALIDATION 
set.seed(1)

# Imposta il controllo per 10-fold CV
ctrl = trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = FALSE
)
dati_plasma = dati_plasma[order(dati_plasma$Malattia), ]

# PLASMA
modello_cv_plasma = train(
  Malattia ~ Età + Sesso + Acetico + Propionico + Butirrico + IsoButirrico +
    IsoValerico + `2MetilButirrico` + Valerico,
  data = dati_plasma,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)
conf_mat_plasma = confusionMatrix(modello_cv_plasma$pred$pred, modello_cv_plasma$pred$obs)
print(conf_mat_plasma)


#STOOL
modello_cv_stool = train(
  Malattia ~ Età + Sesso + Acetico + Propionico + Butirrico + IsoButirrico +
    IsoValerico + `2MetilButirrico` + Valerico,
  data = dati_stool,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)
conf_mat_stool = confusionMatrix(modello_cv_stool$pred$pred, modello_cv_stool$pred$obs)
print(conf_mat_stool)


#UNITI
modello_cv_comb = train(
  Malattia ~ Età + Sesso +
    Acetico_plasma + Propionico_plasma + Butirrico_plasma + IsoButirrico_plasma +
    IsoValerico_plasma + `2MetilButirrico_plasma` + Valerico_plasma +
    Acetico_feci + Propionico_feci + Butirrico_feci + IsoButirrico_feci +
    IsoValerico_feci + `2MetilButirrico_feci` + Valerico_feci,
  data = dati_combinati,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)
conf_mat_comb = confusionMatrix(modello_cv_comb$pred$pred, modello_cv_comb$pred$obs)
print(conf_mat_comb)


#ANALISI DISCRIMINANTE LINEARE DI FISHER---------

lda_mod_plasma = lda(Malattia ~ Acetico + Propionico + Butirrico + IsoButirrico +
                        IsoValerico + `2MetilButirrico` + Valerico,
                      data = dati_plasma[ , -(1:3)])

lda_mod_stool = lda(Malattia ~ Acetico + Propionico + Butirrico + IsoButirrico +
                       IsoValerico + `2MetilButirrico` + Valerico,
                     data = dati_stool[ , -(1:3)])

lda_mod_comb = lda(Malattia ~ Acetico_plasma + Propionico_plasma + Butirrico_plasma + IsoButirrico_plasma +
                       IsoValerico_plasma + `2MetilButirrico_plasma` + Valerico_plasma +
                       Acetico_feci + Propionico_feci + Butirrico_feci + IsoButirrico_feci +
                       IsoValerico_feci + `2MetilButirrico_feci` + Valerico_feci,
                     data = dati_combinati[ , -c(1,16,17)])


print(lda_mod_plasma)
print(lda_mod_stool)
print(lda_mod_comb)


# Plot
ragg::agg_png("lda_plasma.png", width = 12, height = 9, units = "in", res = 300)
lda_pred_plasma = predict(lda_mod_plasma)
ld_data = as.data.frame(lda_pred_plasma$x)
ld_data$Malattia = dati_plasma$Malattia

palette_colori = c("firebrick", "dodgerblue", "darkgreen", "darkorange")
names(palette_colori) = levels(ld_data$Malattia)
col = palette_colori[ld_data$Malattia]

pairs(ld_data[, 1:3],
      labels = colnames(ld_data)[1:3],
      upper.panel = function(x, y) {
        text(x, y, labels = ld_data$Malattia, col = col, cex = 0.7)
      },
      lower.panel = function(x, y) {
        text(x, y, labels = ld_data$Malattia, col = col, cex = 0.7)
      })
dev.off()

ragg::agg_png("lda_stool.png", width = 12, height = 9, units = "in", res = 300)
lda_pred_stool = predict(lda_mod_stool)
ld_data = as.data.frame(lda_pred_stool$x)
ld_data$Malattia = dati_plasma$Malattia
palette_colori = c("firebrick", "dodgerblue", "darkgreen", "darkorange")
names(palette_colori) = levels(ld_data$Malattia)
col = palette_colori[ld_data$Malattia]
pairs(ld_data[, 1:3],
      labels = colnames(ld_data)[1:3],
      upper.panel = function(x, y) {
        text(x, y, labels = ld_data$Malattia, col = col, cex = 0.7)
      },
      lower.panel = function(x, y) {
        text(x, y, labels = ld_data$Malattia, col = col, cex = 0.7)
      })
dev.off()

ragg::agg_png("lda_combinato.png", width = 12, height = 9, units = "in", res = 300)
lda_pred_comb = predict(lda_mod_comb)
ld_data = as.data.frame(lda_pred_comb$x)
ld_data$Malattia = dati_plasma$Malattia
palette_colori = c("firebrick", "dodgerblue", "darkgreen", "darkorange")
names(palette_colori) = levels(ld_data$Malattia)
col = palette_colori[ld_data$Malattia]
pairs(ld_data[, 1:3],
      labels = colnames(ld_data)[1:3],
      upper.panel = function(x, y) {
        text(x, y, labels = ld_data$Malattia, col = col, cex = 0.7)
      },
      lower.panel = function(x, y) {
        text(x, y, labels = ld_data$Malattia, col = col, cex = 0.7)
      })
dev.off()

# Matrice di confusione
livelli = levels(factor(malattie_plasma))
pred_class_plasma = factor(lda_pred_plasma$class, levels = livelli)
malattie_plasma   = factor(malattie_plasma, levels = livelli)
conf_matrix_plasma = confusionMatrix(pred_class_plasma, malattie_plasma)
print(conf_matrix_plasma)

livelli = levels(factor(malattie_stool))
pred_class_stool = factor(lda_pred_stool$class, levels = livelli)
malattie_stool   = factor(malattie_stool, levels = livelli)
conf_matrix_stool = confusionMatrix(pred_class_stool, malattie_stool)
print(conf_matrix_stool)

livelli = levels(factor(malattie))
pred_class_comb = factor(lda_pred_comb$class, levels = livelli)
malattie_comb   = factor(malattie, levels = livelli)
conf_matrix_comb = confusionMatrix(pred_class_comb, malattie)
print(conf_matrix_comb)


#cross-validation 10-fold
ctrl = trainControl(method = "cv", number = 10, savePredictions = "final")

modello_lda_cv_plasma = train(
  Malattia ~ Acetico + Propionico + Butirrico + IsoButirrico +
    IsoValerico + `2MetilButirrico` + Valerico,
  data = dati_plasma[ , !(names(dati_plasma) %in% "ID")],  # esclude ID se presente
  method = "lda",
  trControl = ctrl
)
conf_matrix_cv_plasma = confusionMatrix(modello_lda_cv_plasma$pred$pred, modello_lda_cv_plasma$pred$obs)
print(conf_matrix_cv_plasma)


modello_lda_cv_stool = train(
  Malattia ~ Acetico + Propionico + Butirrico + IsoButirrico +
    IsoValerico + `2MetilButirrico` + Valerico,
  data = dati_stool[ , !(names(dati_stool) %in% "ID")],  # esclude ID se presente
  method = "lda",
  trControl = ctrl
)
conf_matrix_cv_stool = confusionMatrix(modello_lda_cv_stool$pred$pred, modello_lda_cv_stool$pred$obs)
print(conf_matrix_cv_stool)


modello_lda_cv_comb = train(
  Malattia ~ Acetico_plasma + Propionico_plasma + Butirrico_plasma + IsoButirrico_plasma +
    IsoValerico_plasma + `2MetilButirrico_plasma` + Valerico_plasma +
    Acetico_feci + Propionico_feci + Butirrico_feci + IsoButirrico_feci +
    IsoValerico_feci + `2MetilButirrico_feci` + Valerico_feci,
  data = dati_combinati[ , !(names(dati_combinati) %in% "ID")],  # esclude ID se presente
  method = "lda",
  trControl = ctrl
)
conf_matrix_cv_comb = confusionMatrix(modello_lda_cv_comb$pred$pred, modello_lda_cv_comb$pred$obs)
print(conf_matrix_cv_comb)

#AGGIUNTE------------------------------

dati_5gruppi=dati_combinati

righe_HC2 = 1:75 
dati_5gruppi$Malattia_nuova = as.character(dati_combinati$Malattia)
dati_5gruppi$Malattia_nuova[dati_5gruppi$Malattia == "HC" & !(1:nrow(dati_5gruppi) %in% righe_HC2)] = "HC1"
dati_5gruppi$Malattia_nuova[dati_5gruppi$Malattia == "HC" & (1:nrow(dati_5gruppi) %in% righe_HC2)] = "HC2"
dati_5gruppi=dati_5gruppi[,-18]
colnames(dati_5gruppi)[18]="Malattia"

modello_5gruppi = multinom(
  Malattia ~ Età + Sesso + Acetico_plasma + Propionico_plasma + Butirrico_plasma + 
    IsoButirrico_plasma +   IsoValerico_plasma + `2MetilButirrico_plasma` + Valerico_plasma + 
    Acetico_feci +   Propionico_feci + Butirrico_feci + IsoButirrico_feci + 
    IsoValerico_feci + `2MetilButirrico_feci` + Valerico_feci,
  data = dati_5gruppi[ , !(names(dati_5gruppi) %in% "ID")],
  trace = FALSE
)
modello_5gruppi

coefs = summary(modello_5gruppi)$coefficients
std_err = summary(modello_5gruppi)$standard.errors
z_scores = coefs_comb / std_err_comb
p_values = 2 * (1 - pnorm(abs(z_scores)))



colonne_plasma = grep("_plasma$", colnames(dati_5gruppi), value = TRUE)
dati_5gruppi_plasma = dati_5gruppi[, c(colonne_plasma, colnames(dati_5gruppi[c(1,16:18)]))]

modello_5gruppi_plasma = multinom(
  Malattia ~ Età + Sesso + Acetico_plasma + Propionico_plasma + Butirrico_plasma + 
    IsoButirrico_plasma +   IsoValerico_plasma + `2MetilButirrico_plasma` + Valerico_plasma,
  data = dati_5gruppi_plasma,
  trace = FALSE
)
modello_5gruppi_plasma

coefs_5p = summary(modello_5gruppi_plasma)$coefficients
std_err_5p = summary(modello_5gruppi_plasma)$standard.errors
z_scores_5p = coefs_5p / std_err_5p
p_values_5p = 2 * (1 - pnorm(abs(z_scores_5p)))
odds_ratio = exp(coefs_5p)


colonne_stool = grep("_feci$", colnames(dati_5gruppi), value = TRUE)
dati_5gruppi_stool = dati_5gruppi[, c(colonne_stool, colnames(dati_5gruppi[c(1,16:18)]))]

modello_5gruppi_stool = multinom(
  Malattia ~ Età + Sesso + Acetico_feci + Propionico_feci + Butirrico_feci + 
    IsoButirrico_feci +   IsoValerico_feci + `2MetilButirrico_feci` + Valerico_feci,
  data = dati_5gruppi_stool,
  trace = FALSE
)
modello_5gruppi_stool

coefs_5s = summary(modello_5gruppi_stool)$coefficients
std_err_5s = summary(modello_5gruppi_stool)$standard.errors
z_scores_5s = coefs_5s / std_err_5s
p_values_5s = 2 * (1 - pnorm(abs(z_scores_5s)))
odds_ratio = exp(coefs_5s)


# PLASMA
modello_cv_plasma_5 = train(
  Malattia ~ Età + Sesso + Acetico_plasma + Propionico_plasma + Butirrico_plasma + 
    IsoButirrico_plasma +   IsoValerico_plasma + `2MetilButirrico_plasma` + Valerico_plasma,
  data = dati_5gruppi_plasma,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)
conf_mat_plasma_5 = confusionMatrix(modello_cv_plasma_5$pred$pred, modello_cv_plasma_5$pred$obs)
print(conf_mat_plasma_5)


#STOOL
modello_cv_stool_5 = train(
  Malattia ~ Età + Sesso + Acetico_feci + Propionico_feci + Butirrico_feci + 
    IsoButirrico_feci +   IsoValerico_feci + `2MetilButirrico_feci` + Valerico_feci,
  data = dati_5gruppi_stool,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)
conf_mat_stool_5 = confusionMatrix(modello_cv_stool_5$pred$pred, modello_cv_stool_5$pred$obs)
print(conf_mat_stool_5)


#COMB
modello_cv_comb_5 = train(
  Malattia ~ Età + Sesso + Acetico_plasma + Propionico_plasma + Butirrico_plasma + 
    IsoButirrico_plasma +   IsoValerico_plasma + `2MetilButirrico_plasma` + Valerico_plasma + 
    Acetico_feci +   Propionico_feci + Butirrico_feci + IsoButirrico_feci + 
    IsoValerico_feci + `2MetilButirrico_feci` + Valerico_feci,
  data = dati_5gruppi,
  method = "multinom",
  trControl = ctrl,
  trace = FALSE
)
conf_mat_comb_5 = confusionMatrix(modello_cv_comb_5$pred$pred, modello_cv_comb_5$pred$obs)
print(conf_mat_comb_5)

#KRUSKAL-WALLIS A COPPIE
risultati_p_HC_CD = list()
k = 1
for (acido in acidi_grassi) {
subset = dati_plasma[dati_plasma$Malattia %in% c("HC", "CD"), c("Malattia", acido)]
colnames(subset)[2] = "valore"
test = kruskal.test(valore ~ Malattia, data = subset)
p_value = test$p.value
statistic = test$statistic
risultati_p_HC_CD[[k]] = data.frame(
  Acido = acido,
  Statistica = statistic,
  P_value = p_value
)
k=k+1
}
kruskal_risultati_p_HC_CD = do.call(rbind, risultati_p_HC_CD)
print(kruskal_risultati_p_HC_CD)

risultati_s_HC_CD = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_stool[dati_stool$Malattia %in% c("HC", "CD"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_s_HC_CD[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_s_HC_CD = do.call(rbind, risultati_s_HC_CD)
print(kruskal_risultati_s_HC_CD)


risultati_p_HC_CRC = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_plasma[dati_plasma$Malattia %in% c("HC", "CRC"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_p_HC_CRC[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_p_HC_CRC = do.call(rbind, risultati_p_HC_CRC)
print(kruskal_risultati_p_HC_CRC)

risultati_s_HC_CRC = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_stool[dati_stool$Malattia %in% c("HC", "CRC"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_s_HC_CRC[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_s_HC_CRC = do.call(rbind, risultati_s_HC_CRC)
print(kruskal_risultati_s_HC_CRC)


risultati_p_HC_Obesi = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_plasma[dati_plasma$Malattia %in% c("HC", "Obesi"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_p_HC_Obesi[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_p_HC_Obesi = do.call(rbind, risultati_p_HC_Obesi)
print(kruskal_risultati_p_HC_Obesi)

risultati_s_HC_Obesi = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_stool[dati_stool$Malattia %in% c("HC", "Obesi"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_s_HC_Obesi[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_s_HC_Obesi = do.call(rbind, risultati_s_HC_Obesi)
print(kruskal_risultati_s_HC_Obesi)


risultati_p_CRC_Obesi = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_plasma[dati_plasma$Malattia %in% c("CRC", "Obesi"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_p_CRC_Obesi[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_p_CRC_Obesi = do.call(rbind, risultati_p_CRC_Obesi)
print(kruskal_risultati_p_CRC_Obesi)

risultati_s_CRC_Obesi = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_stool[dati_stool$Malattia %in% c("CRC", "Obesi"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_s_CRC_Obesi[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_s_CRC_Obesi = do.call(rbind, risultati_s_CRC_Obesi)
print(kruskal_risultati_s_CRC_Obesi)


risultati_p_CD_Obesi = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_plasma[dati_plasma$Malattia %in% c("CD", "Obesi"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_p_CD_Obesi[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_p_CD_Obesi = do.call(rbind, risultati_p_CD_Obesi)
print(kruskal_risultati_p_CD_Obesi)

risultati_s_CD_Obesi = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_stool[dati_stool$Malattia %in% c("CD", "Obesi"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_s_CD_Obesi[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_s_CD_Obesi = do.call(rbind, risultati_s_CD_Obesi)
print(kruskal_risultati_s_CD_Obesi)


risultati_p_CD_CRC = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_plasma[dati_plasma$Malattia %in% c("CD", "CRC"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_p_CD_CRC[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_p_CD_CRC = do.call(rbind, risultati_p_CD_CRC)
print(kruskal_risultati_p_CD_CRC)

risultati_s_CD_CRC = list()
k = 1
for (acido in acidi_grassi) {
  subset = dati_stool[dati_stool$Malattia %in% c("CD", "CRC"), c("Malattia", acido)]
  colnames(subset)[2] = "valore"
  test = kruskal.test(valore ~ Malattia, data = subset)
  p_value = test$p.value
  statistic = test$statistic
  risultati_s_CD_CRC[[k]] = data.frame(
    Acido = acido,
    Statistica = statistic,
    P_value = p_value
  )
  k=k+1
}
kruskal_risultati_s_CD_CRC = do.call(rbind, risultati_s_CD_CRC)
print(kruskal_risultati_s_CD_CRC)


#previsioni acidi
modelli_lm_plasma = list()
sommari_lm_plasma = list()
r2_risultati_plasma = data.frame()


variabili_feci = grep("_feci$", names(dati_combinati), value = TRUE)
variabili_feci_bt = paste0("`", variabili_feci, "`")  # per formula
for (acido_target in colonne_plasma) {
  acido_bt = paste0("`", acido_target, "`")
  formula_modello = as.formula(
    paste(acido_bt, "~", paste(variabili_feci_bt, collapse = " + "))
  )
  variabili_usate = c(acido_target, variabili_feci)
  dati_completi = dati_combinati[complete.cases(dati_combinati[, variabili_usate]), ]
  modello = lm(formula_modello, data = dati_completi)
  modelli_lm_plasma[[acido_target]] = modello
  sommari_lm_plasma[[acido_target]] = summary(modello)
  
  r2_risultati_plasma = rbind(
    r2_risultati_plasma,
    data.frame(
      Acido = acido_target,
      R2 = summary(modello)$r.squared
    )
  )
}

risultati_predetti_plasma = list()
for (acido in colonne_plasma) {
  modello = modelli_lm_plasma[[acido]]
  dati_completi = dati_combinati[complete.cases(dati_combinati[, c(acido, variabili_feci)]), ]
  predetti = predict(modello, newdata = dati_completi)

  risultati_predetti_plasma[[acido]] = data.frame(
    Osservato = dati_completi[[acido]],
    Predetto = predetti,
    row.names = dati_completi$ID
  )
}

risultati_predetti_plasma=as.data.frame(risultati_predetti_plasma)


#previsioni acidi
modelli_lm_stool = list()
sommari_lm_stool = list()
r2_risultati_stool = data.frame()

variabili_plasma = grep("_plasma$", names(dati_combinati), value = TRUE)
variabili_plasma_bt = paste0("`", variabili_plasma, "`")

for (acido_target in colonne_stool) {
  acido_bt = paste0("`", acido_target, "`")
  formula_modello = as.formula(
    paste(acido_bt, "~", paste(variabili_plasma_bt, collapse = " + "))
  )
  variabili_usate = c(acido_target, variabili_plasma)
  dati_completi = dati_combinati[complete.cases(dati_combinati[, variabili_usate]), ]
  modello = lm(formula_modello, data = dati_completi)
  modelli_lm_stool[[acido_target]] = modello
  sommari_lm_stool[[acido_target]] = summary(modello)
  
  r2_risultati_stool = rbind(
    r2_risultati_stool,
    data.frame(
      Acido = acido_target,
      R2 = summary(modello)$r.squared
    )
  )
}

risultati_predetti_stool = list()
for (acido in colonne_stool) {
  modello = modelli_lm_stool[[acido]]
  dati_completi = dati_combinati[complete.cases(dati_combinati[, c(acido, variabili_plasma)]), ]
  predetti = predict(modello, newdata = dati_completi)
  
  risultati_predetti_stool[[acido]] = data.frame(
    Osservato = dati_completi[[acido]],
    Predetto = predetti,
    row.names = dati_completi$ID
  )
  
  
}
risultati_predetti_stool=as.data.frame(risultati_predetti_stool)



Z = scale(dati_5gruppi_plasma[ , sapply(dati_5gruppi_plasma, is.numeric) & names(dati_5gruppi_plasma) !=c("Età","ID","Sesso")])
rownames(Z) = dati_5gruppi_plasma$ID
hc = hclust(dist(Z), method = "ward.D2")
dend = as.dendrogram(hc)
ordered_ids = labels(dend)
id2malattia = setNames(as.character(dati_5gruppi_plasma$Malattia), dati_5gruppi_plasma$ID)
malattie_ordinate = id2malattia[ordered_ids]
palette_colori = c("CD"="forestgreen", "CRC"="darkorange", "HC1"="royalblue", "Obesi"="firebrick", "HC2"="yellow")
labels_colors(dend) = palette_colori[malattie_ordinate]
ragg::agg_png("dendogramma_Ward_5gruppi_plasma.png", width = 12, height = 9, units = "in", res = 300)
plot(dend)
legend("topright", legend = names(palette_colori), fill = palette_colori, cex = 0.8)
rect.hclust(hc, k = 5, border = c("black", "gray40", "darkcyan", "purple", "darkblue"))
dev.off()
#matrici di confusione
malattie = as.factor(dati_5gruppi_plasma$Malattia)
gruppi = cutree(hc, k = 5)
table(gruppi, malattie)
mappa = c("1" = "HC2", "2" = "HC1", "3" = "CD", "4" = "Obesi", "5"="CRC")
gruppi_mappati = factor(mappa[as.character(gruppi)], levels = levels(malattie))
conf_matrix = confusionMatrix(gruppi_mappati, malattie)
print(conf_matrix)


Z = scale(dati_5gruppi_stool[ , sapply(dati_5gruppi_stool, is.numeric) & names(dati_5gruppi_stool) !=c("Età","ID","Sesso")])
rownames(Z) = dati_5gruppi_stool$ID
hc = hclust(dist(Z), method = "ward.D2")
dend = as.dendrogram(hc)
ordered_ids = labels(dend)
id2malattia = setNames(as.character(dati_5gruppi_stool$Malattia), dati_5gruppi_stool$ID)
malattie_ordinate = id2malattia[ordered_ids]
palette_colori = c("CD"="forestgreen", "CRC"="darkorange", "HC1"="royalblue", "Obesi"="firebrick", "HC2"="yellow")
labels_colors(dend) = palette_colori[malattie_ordinate]
ragg::agg_png("dendogramma_Ward_5gruppi_stool.png", width = 12, height = 9, units = "in", res = 300)
plot(dend)
legend("topright", legend = names(palette_colori), fill = palette_colori, cex = 0.8)
rect.hclust(hc, k = 5, border = c("black", "gray40", "darkcyan", "purple", "darkblue"))
dev.off()
#matrici di confusione
malattie = as.factor(dati_5gruppi_stool$Malattia)
gruppi = cutree(hc, k = 5)
table(gruppi, malattie)
mappa = c("1" = "CRC", "2" = "HC2", "3" = "CD", "4" = "Obesi", "5"="HC1")
gruppi_mappati = factor(mappa[as.character(gruppi)], levels = levels(malattie))
conf_matrix = confusionMatrix(gruppi_mappati, malattie)
print(conf_matrix)


Z = scale(dati_5gruppi[ , sapply(dati_5gruppi, is.numeric) & names(dati_5gruppi) !=c("Età","ID","Sesso")])
rownames(Z) = dati_5gruppi_stool$ID
hc = hclust(dist(Z), method = "ward.D2")
dend = as.dendrogram(hc)
ordered_ids = labels(dend)
id2malattia = setNames(as.character(dati_5gruppi$Malattia), dati_5gruppi$ID)
malattie_ordinate = id2malattia[ordered_ids]
palette_colori = c("CD"="forestgreen", "CRC"="darkorange", "HC1"="royalblue", "Obesi"="firebrick", "HC2"="yellow")
labels_colors(dend) = palette_colori[malattie_ordinate]
ragg::agg_png("dendogramma_Ward_5gruppi.png", width = 12, height = 9, units = "in", res = 300)
plot(dend)
legend("topright", legend = names(palette_colori), fill = palette_colori, cex = 0.8)
rect.hclust(hc, k = 5, border = c("black", "gray40", "darkcyan", "purple", "darkblue"))
dev.off()

#matrici di confusione
malattie = as.factor(dati_5gruppi$Malattia)
gruppi = cutree(hc, k = 5)
table(gruppi, malattie)
mappa = c("1" = "HC2", "2" = "CD", "3" = "HC1", "4" = "Obesi", "5"="CRC")
gruppi_mappati = factor(mappa[as.character(gruppi)], levels = levels(malattie))

conf_matrix = confusionMatrix(gruppi_mappati, malattie)
print(conf_matrix)
#test----------------------


matrici_cov = lapply(malattie_uniche, function(g) {
  subset = dati_plasma[dati_plasma$Malattia == g, -which(names(dati_plasma) %in% c("ID", "Gruppo"))]
  numeriche = subset[, sapply(subset, is.numeric)]
  cov(numeriche, use = "complete.obs")
})
names(matrici_cov) = malattie_uniche
heatmap(matrici_cov[["HC"]], main = "Covarianze - Gruppo HC")
heatmap(matrici_cov[["CRC"]], main = "Covarianze - Gruppo CRC")
heatmap(matrici_cov[["CD"]], main = "Covarianze - Gruppo CD")
heatmap(matrici_cov[["Obesi"]], main = "Covarianze - Gruppo Obesi")

matrici_cov = lapply(malattie_uniche, function(g) {
  subset = dati_stool[dati_stool$Malattia == g, -which(names(dati_stool) %in% c("ID", "Gruppo"))]
  numeriche = subset[, sapply(subset, is.numeric)]
  cov(numeriche, use = "complete.obs")
})
names(matrici_cov) = malattie_uniche
heatmap(matrici_cov[["HC"]], main = "Covarianze - Gruppo HC")
heatmap(matrici_cov[["CRC"]], main = "Covarianze - Gruppo CRC")
heatmap(matrici_cov[["CD"]], main = "Covarianze - Gruppo CD")
heatmap(matrici_cov[["Obesi"]], main = "Covarianze - Gruppo Obesi")


par(mfrow = c(2, 2))  # 4 grafici per volta
for (var in acidi_grassi) {
  for (g in malattie_uniche) {
    subset = dati_plasma[dati_plasma$Malattia == g, var]
    titolo = paste0(var, " - Gruppo ", g)
    hist(subset, breaks = 15, freq = FALSE, 
         main = titolo, xlab = "", cex.main = 0.8)
    curve(dnorm(x, mean = mean(subset), sd = sd(subset)), 
          add = TRUE, col = "red", lwd = 2)
    qqnorm(subset, main = paste("Q-Q plot:", var, "(", g, ")"), cex.main = 0.8)
    qqline(subset, col = "red", lwd = 2)
    if (dev.interactive()) readline("Premi INVIO per continuare...")
  }
}
for (var in acidi_grassi) {
  for (g in malattie_uniche) {
    subset = na.omit(dati_plasma[dati_plasma$Malattia == g, var])
    if (length(subset) >= 3 && length(subset) <= 5000) {
      test = shapiro.test(subset)
      cat("Shapiro-Wilk -", var, "in gruppo", g, ": p =", round(test$p.value, 4), "\n")
    } else {
      cat("Non testabile -", var, "in gruppo", g, "(n =", length(subset), ")\n")
    }
  }
}

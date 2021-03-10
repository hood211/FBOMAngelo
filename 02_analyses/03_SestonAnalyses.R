# LOAD LIBRARIES
	library(ggplot2)
	library(tidyverse)
	library(ggthemes)
	library(gridExtra)
	library(forcats)
	library(ggpubr)
	
# THEME
	# theme_set(theme_few(base_size = 20))

# LOAD DATA
	seston2 = read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/PHD/1-Angelo/Angelo Data '06 - '08/Angelo OM surveys/2008 FBOM survey/4_definativeDatasets/Seston/SestonSummary.csv")
	# write.csv(levels(seston$newSite), "Sites.csv") 
	
	sites = read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/PHD/1-Angelo/Angelo Data '06 - '08/Angelo OM surveys/2008 FBOM survey/4_definativeDatasets/Sites.csv", row.names = 1) # added WA areas
	
	seston3 = merge(seston2, sites, by.x = "newSite", by.y = "siteName", all.x = TRUE) 
	
	seston4 <- seston3 %>% 
	  mutate(C_N1 = ugC/ugN *14/12,
	         C_N2 = NIRS_ugC_L/NIR_ugN_L * 14/12,
	         C_N3 = C.N *14/12,
	         # C_N = rowMeans(c(C_N1, C_N2, C_N3), na.rm = TRUE),
	         Pdt = as.POSIXct(Date, format = "%m/%d/%y"),
	         M = as.numeric(as.character(strftime(Pdt, format = "%m")))) 
	
	seston4$C_N <- rowMeans(seston4[,19:21], na.rm = TRUE)
	
	ses <- seston4 %>% 
	  mutate(Y = as.factor(as.character(strftime(Pdt, format = "%Y"))),
	         logWA = log10(WA),
	         logChla = log10(ugChlaL + 0.001)) %>% 
	  filter(M >= 8) %>% 
	  select(newSite, Pdt, Y, d13C, d15N, C_N, NIRS_ugC_L, NIR_ugN_L, logChla, logWA)

	 ses2008 <- ses %>% 
	   filter(Y == "2008")
	 
	 ######
	 #for map
	 ses2008names <-  levels(ses2008$newSite)
	 # write.csv(ses2008names, "11_SestonSites.csv")
	 # Look at data 
	 #ug C and N/L
	 summary(lm(NIRS_ugC_L ~ log10(WA), seston4))
	 summary(lm(NIR_ugN_L ~ log10(WA), seston4))
	 

	# CN
summary(lm(C_N ~ logWA, ses2008))	 
sesP1lab <- "atop(R^2==0.63,P < 0.001)"
sesP1 <- ggplot(ses2008, aes(y = C_N, x = logWA)) +
    geom_point(shape = 21, fill = "gray", size = 8, alpha = 60/100) +
  xlab(expression(paste(log[10]," watershed area (",km^-2,")"))) +
  ylab("Suspended POM C:N") +
  stat_smooth(method = "lm", se = FALSE, color = "black") +
  ylim(0,20) +
  scale_x_continuous(limits = c(-1, 2.5), breaks = c(-1, -0.5, 0, 0.5,1, 1.5,2, 2.5)) +
  annotate("text", x = -1, y = 0, label = sesP1lab, parse = TRUE, size = 7, hjust = 0, vjust = 0) +
  annotate("text", x = -1, y = 19.75, label = "A)", size = 9, fontface = "bold") +
  theme(panel.background = element_rect(fill = "transparent", color = "black", size = 1.25),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 18),
        panel.grid = element_line(color = "transparent"))


sesChla <- ses %>% 
  mutate(Pdt = as.POSIXct(Pdt, format = "%Y-%m-%d"),
    M = as.numeric(strftime(Pdt, format = "%m"))) %>% 
  filter(M >= 8 & M <= 9) %>% 
  filter(logChla > -2.5)

summary(lm(logChla ~ logWA, sesChla))	
sesP2lab <- "atop(R^2==0.76,P < 0.001)"
sesP2 <- ggplot(sesChla, aes(y = logChla, x = logWA), color = Y) +
  geom_point(shape = 21, fill = "gray", size = 8, alpha = 60/100) +
  xlab(expression(paste(log[10]," watershed area (",km^-2,")"))) +
  ylab(expression(atop(paste(log[10]," suspended POM Chl a"), paste("(µg chl a ",L^-1,")")))) +
  stat_smooth(method = "lm", se = FALSE, color = "black") +
  annotate("text", x = 1.95, y = -2, label = sesP2lab, parse = TRUE, size = 7)+
  annotate("text", x = -1, y = 0.25, label = "C)", size = 9, fontface = "bold") +
  scale_x_continuous(limits = c(-1, 2.5), breaks = c(-1, -0.5, 0, 0.5,1, 1.5,2, 2.5)) +
  theme(panel.background = element_rect(fill = "transparent", color = "black", size = 1.25),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 18),
        panel.grid = element_line(color = "transparent"))



#little to no change in d13C with WA
summary(lm(d13C ~ logWA, ses2008))
sesP3 <- ggplot(ses2008, aes(y = d13C, x = logWA)) +
  geom_point(shape = 21, fill = "gray", size = 8, alpha = 60/100) +
  xlab(expression(paste(log[10]," watershed area (",km^-2,")"))) +
  ylab(expression(paste("Suspended POM ",delta^13,"C (‰)"))) +
  ylim(-29,-25) +
  # xlim(-0.75,2.25) +
  scale_x_continuous(limits = c(-1, 2.5), breaks = c(-1, -0.5, 0, 0.5,1, 1.5,2, 2.5)) +
  annotate("text", x = -1, y = -29, label = "P = 0.96", parse = TRUE, size = 7, hjust = 0, vjust = 0) +
  annotate("text", x = -1, y = -25.2, label = " D)", size = 9, fontface = "bold")+
  theme(panel.background = element_rect(fill = "transparent", color = "black", size = 1.25),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 18),
        panel.grid = element_line(color = "transparent"))


summary(lm(d15N ~ logWA, ses2008))
sesP4lab <- "atop(R^2==0.48,P==0.004)"
sesP4 <- ggplot(ses2008, aes(y = d15N, x = logWA)) +
  geom_point(shape = 21, fill = "lightgray", size = 8, alpha = 60/100) +
  xlab(expression(paste(log[10]," watershed area (",km^-2,")"))) +
  ylab(expression(paste("Suspended POM ",delta^15,"N (‰)"))) +
  scale_x_continuous(limits = c(-1, 2.5), breaks = c(-1, -0.5, 0, 0.5,1, 1.5,2, 2.5)) +
  scale_y_continuous(limits = c(-2, 6.5), breaks = c(-2,0, 2,4, 6))+
  stat_smooth(method = "lm", se = FALSE, color = "black") +
  annotate("text", x = -1, y = -2, label = sesP4lab, parse = TRUE, size = 7, hjust = 0, vjust = 0) +
  annotate("text", x = -1, y = 6.25, label = "B)", size = 9, fontface = "bold")+
  theme(panel.background = element_rect(fill = "transparent", color = "black", size = 1.25),
        axis.title = element_text(size = 26),
        axis.text = element_text(size = 18),
        panel.grid = element_line(color = "transparent"))


tiff(file.path(here::here("03_plots"), "FigS8.tiff"), height = 30, width = 36, units = 'cm', compression = "lzw", res = 400)
# grid.arrange(sesP1, sesP2, sesP3, sesP4, nrow = 2, ncol = 2)
ggarrange(sesP2, sesP1, sesP4, sesP3, nrow = 2, ncol = 2)
dev.off()


	
save.image("11_seston_Fig8_rdat")










# make a summary sheet to merge with FBOM data
sesFBOMmerge <- ses2008 %>% 
  group_by(newSite) %>% 
  summarise_at(.vars = vars(d13C, d15N, C_N, logChla, logWA),
               .funs = c(mean = "mean", sd = "sd"), na.rm = TRUE) 


# Get bulk data
fbom = read.csv("~/Dropbox/JMH_dropbox/stephanson2/Projects/PHD/1-Angelo/Angelo Data '06 - '08/Angelo OM surveys/2008 FBOM survey/4_definativeDatasets/SumData/FBOMsummary2_17Sept18.csv", row.names = 1)
fbom$pool = as.factor(fbom$pool)
fbom$stream = factor(fbom$stream, levels = c("MKLY", "Fox", "JOH", "Elder","SFAS", "SF"))
fbom$size = factor(fbom$size, levels = c("B", "L53", "G53", "G106", "G250"))
fbom$siteName = as.factor(fbom$siteName)
fbom$siteName = factor(fbom$siteName, levels = c("MKLY", "Fox", "JOH", "Elder","SFAS", "HQ", "MG", "GB", "BW"))
fbom$CNafdm = fbom$perCafdm / fbom$perNafdm * (14/12)

fbomS <- fbom %>% 
  mutate(siteName = fct_recode(siteName, LE = "Elder",
                               SF_AS = "SFAS",
                               SF_HQ = "HQ",
                               SF_Wlbridge = "GB",
                               SF_Wlbridge = "MG",
                               SF_Globbi = "BW")) %>% 
  group_by(siteName) %>% 
  summarise_at(.vars = vars(del13C, del15N, CNafdm),
               .funs = c(mean = "mean", sd = "sd"), na.rm = TRUE) %>% 
  mutate(siteName = fct_recode(siteName, LE = "Elder",
                               SF_AS = "SFAS",
                               SF_HQ = "HQ",
                               SF_Wlbridge = "GB",
                               SF_Wlbridge = "MG",
                               SF_Globbi = "BW")) 
  
  fbomSes <- fbomS %>% 
    left_join(sesFBOMmerge, by = c("siteName" = "newSite")) %>% 
    select(siteName, 
           F_13C = del13C_mean,
           F_15N = del15N_mean,
           F_CN = CNafdm_mean,
           S_13C = d13C_mean,
           S_15N = d15N_mean,
           S_CN = C_N_mean,
           logWA_mean) %>% 
    mutate(logWA_mean = ifelse(siteName == "SF_Wlbridge", 2.16, logWA_mean),
      WA = 10^logWA_mean,
      siteName = as.factor(siteName),
           siteName = fct_recode(siteName, SFAS = "SF_AS",
                             SFGB = "SF_HQ",
                             SFGB = "SF_Wlbridge",
                             SFGB = "SF_Globbi"),
      siteName = fct_relevel(siteName, "MKLY", "Fox", "JOH", "LE", "SFAS", "SFGB"))
#new plots
  FigS7a <- ggplot(fbomSes, aes(y = F_CN, x = S_CN, fill = siteName)) +
    geom_point(shape = 21, alpha = 75/100, size = 7) +
    geom_abline(intercept = 0, slope = 1) +
    xlim(0,35) +
    ylim(0,35) +
    ylab("FBOM C:N") +
    xlab("FSOM C:N") +
    annotate("text", x = 1, y = 35, label = "A)", size = 9)+
    scale_fill_manual(values = c("blue", "dodgerblue2", "deepskyblue", "lightpink", "hotpink", "red")) +
    theme(panel.background = element_rect(fill = "transparent", color = "black", size = 1.25),
          axis.title = element_text(size = 26),
          axis.text = element_text(size = 16),
          panel.grid = element_line(color = "transparent"),
          legend.justification = c(0,1),
          legend.position = c(0.8,0.98),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.key = element_rect(fill = "transparent"),
          legend.background = element_rect(color = "black"))
  
  FigS7b <- ggplot(fbomSes, aes(y = F_13C, x = S_13C, fill = siteName)) +
    geom_point(shape = 21, alpha = 75/100, size = 7) +
    geom_abline(intercept = 0, slope = 1)+
    ylab(expression(paste("FBOM ",delta^13,"C (‰)"))) +
    xlab(expression(paste("FSOM ",delta^13,"C (‰)"))) +
    xlim(-30,-20) +
    ylim(-30,-20)+
    annotate("text", x = -30, y = -20, label = "B)", size = 9)+
    scale_fill_manual(values = c("blue", "dodgerblue2", "deepskyblue", "lightpink", "hotpink", "red")) +
    theme(panel.background = element_rect(fill = "transparent", color = "black", size = 1.25),
          axis.title = element_text(size = 26),
          axis.text = element_text(size = 16),
          panel.grid = element_line(color = "transparent"),
          legend.justification = c(0,1),
          legend.position = "none",
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.key = element_rect(fill = "transparent"),
          legend.background = element_rect(color = "black"))
  
  FigS7c <- ggplot(fbomSes, aes(y = F_15N, x = S_15N, fill = siteName)) +
    geom_point(shape = 21, alpha = 75/100, size = 7) +
    geom_abline(intercept = 0, slope = 1)+
    ylab(expression(paste("FBOM ",delta^15,"N (‰)"))) +
    xlab(expression(paste("FSOM ",delta^15,"N (‰)"))) +
    xlim(-1,4) +
    ylim(-1,4)+
    annotate("text", x = -1, y = 3.9, label = "C)", size = 9)+
    scale_fill_manual(values = c("blue", "dodgerblue2", "deepskyblue", "lightpink", "hotpink", "red")) +
    theme(panel.background = element_rect(fill = "transparent", color = "black", size = 1.25),
          axis.title = element_text(size = 26),
          axis.text = element_text(size = 16),
          panel.grid = element_line(color = "transparent"),
          legend.justification = c(0,1),
          legend.position = "none",
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.key = element_rect(fill = "transparent"),
          legend.background = element_rect(color = "black"))
  
  tiff("00_NewPlots2/FigS5.tiff", height = 35, width = 15, units = 'cm', compression = "lzw", res = 400)
  grid.arrange(FigS7a, FigS7b, FigS7c, nrow = 3, ncol =1)
  dev.off()
  
# OLD PLOTS plots
  legendTitle <- expression(paste("WA (", km^2,")"))
 fs1 <- ggplot(fbomSes, aes(y = F_CN, x = S_CN, label = siteName, size = WA)) +
    # geom_text(size = 5) +
    geom_point(shape = 21, alpha = 75/100, fill = "grey80") +
    geom_abline(intercept = 0, slope = 1) +
    xlim(0,35) +
    ylim(0,35) +
    ylab("Benthic FPOM C:N") +
    xlab("Seston C:N") +
    annotate("text", x = 1, y = 33, label = "A)", size = 9)+
   scale_fill_manual(values = c("blue", "dodgerblue2", "deepskyblue", "lightpink", "hotpink", "red")) +
   scale_size_continuous(legendTitle, breaks = c(1,10, 100), range = c(4,10)) +
   theme(panel.background = element_rect(fill = "transparent", color = "black", size = 1.25),
         axis.title = element_text(size = 26),
         axis.text.y = element_text(size = 14),
         axis.text.x = element_text(size = 20),
         panel.grid.major = element_line(color = "grey90"),
         legend.position = c(0.8,0.25),
         # legend.justification = c(0,1),
         # legend.position = c(0,0),
         legend.text = element_text(size = 24),
         legend.title = element_text(size = 25, face = "bold"),
         legend.key = element_rect(fill = "white"),
         legend.background = element_rect(fill = "white", color = "black"))
  
  fs2 <- ggplot(fbomSes, aes(y = F_13C, x = S_13C, label = siteName, size = WA)) +
    # geom_text(size = 5) +
    geom_point(shape = 21, alpha = 75/100, fill = "grey80") +
    geom_abline(intercept = 0, slope = 1)+
    ylab(expression(paste("Benthic FPOM ",delta^13,"C (‰)"))) +
    xlab(expression(paste("Seston ",delta^13,"C (‰)"))) +
    xlim(-30,-20) +
    ylim(-30,-20)+
    annotate("text", x = -30, y = -30, label = "B)", size = 9)+
    scale_size_continuous(legendTitle, breaks = c(1,10, 100), range = c(4,10)) +
    theme(panel.background = element_rect(fill = "transparent", color = "black", size = 1.25),
          axis.title = element_text(size = 26),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 20),
          panel.grid.major = element_line(color = "grey90"),
          legend.position = "none")
  
 fs3 <- ggplot(fbomSes, aes(y = F_15N, x = S_15N, label = siteName, size = WA)) +
    # geom_text(size = 5) +
   geom_point(shape = 21, alpha = 75/100, fill = "grey80") +
    geom_abline(intercept = 0, slope = 1)+
    ylab(expression(paste("Benthic FPOM ",delta^15,"N (‰)"))) +
    xlab(expression(paste("Seston ",delta^15,"N (‰)"))) +
    xlim(-1,4) +
    ylim(-1,4)+
    annotate("text", x = -1, y = 3.9, label = "C)", size = 9)+
   scale_size_continuous(legendTitle, breaks = c(1,10, 100), range = c(4,10)) +
   theme(panel.background = element_rect(fill = "transparent", color = "black", size = 1.25),
         axis.title = element_text(size = 26),
         axis.text.y = element_text(size = 14),
         axis.text.x = element_text(size = 20),
         panel.grid.major = element_line(color = "grey90"),
         legend.position = "none")
 
 tiff("00_NewPlots/SFig6_sesonFPOMcomp.tiff", height = 35, width = 15, units = 'cm', compression = "lzw", res = 400)
 grid.arrange(fs1, fs2, fs3, nrow = 3, ncol =1)
 dev.off()

# This code estimates the contribution of seafood and specific seafood groups to household nutrient intake and adequacy
# Jessica Zamborain Mason 

#clear R
rm(list=ls())

#set working directory 
setwd("C:/Users/jez297/Dropbox/Harvard Postdoc/Kiribati")

#load required packages
library(reshape2)
library(ggplot2)
library(plyr)
library(ggpubr)
library(ggridges)
library(knitr)
library(rworldmap)
library (haven)  #to read stata files
library(tidyverse)
library(brms)
library(rstan)
library(dplyr)
#make rstan work with all cores
rstan_options(auto_write = T)
options(mc.cores = parallel::detectCores())

#Pearson's correlation for pairs plot
panel.cor = function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = abs(cor(x, y, method = "pearson",use = "complete.obs"))
  txt = format(c(r, 0.123456789), digits=digits)[1]
  txt = paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor = 2/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r,col="black")
}
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "grey", ...)
}

#data: HIES expenditure data
expenditure<-read_dta("30_ExpenditureAggregate.dta")

#get only food recall data
foodrecall<-expenditure[expenditure$section=="21_foodrecall",]
foodrecall<-droplevels(foodrecall)

#Pacific Nutrient DataBase 2020
#PNDB are for 100 grams of edible portion.
pnsb<-read_dta("PNSB_March2022.dta")

#merge data
foodrecall<-merge(foodrecall, pnsb,by="pndbcode",all.x=T)
colnames(foodrecall)
head(foodrecall)

#descriptions in restaurants cafes
unique(foodrecall$coicop_class)
foodrecall$Food_description_HIES[foodrecall$coicop_class=="Restaurants, cafes and the like"]
summary(as.factor(foodrecall$Food_description_HIES[foodrecall$coicop_class=="Food products n.e.c."]))
summary(as.factor(foodrecall$Food_description_HIES[foodrecall$coicop_class=="Fruit"]))

#check edible portions for aquatic food groupings
foodrecall %>%dplyr::filter(coicop_class==113) %>% dplyr::select (ep_, Food_description_HIES) %>%dplyr::group_by(Food_description_HIES) %>% dplyr::summarize(mean_ep=mean(ep_))

#Convert edible quantity consumption to grams of protein consumption: gen protein =  ep_grams / 100 * protein_g
foodrecall$gram_ep<- (foodrecall$ep_/100)*foodrecall$qty_gram_new

#kcal estimate
#as done in the slides but refuse is not from 0 to 1 so Proceeding like Mike mentioned in the email
#foodrecall$kcal_JZM<-foodrecall$qty_gram_new*(1-((foodrecall$refuse_)/100))*foodrecall$energy_kcal
summary(foodrecall$refuse_)
colnames(foodrecall)
unique(foodrecall$description)
#nutrients are per 100g of food (got units from PNCD user guide):https://www.fao.org/3/cb0267en/cb0267en.pdf
foodrecall<-foodrecall%>%mutate(kcal_JZM=gram_ep*(energy_kcal/100),
                                protein_tg=gram_ep*(protein_g/100),
                                total_fat_tg=gram_ep*(total_fat_g/100),
                                carbs_tg=gram_ep*(cho_available/100),
                                tdfibre_tg=gram_ep*(tdf_g/100),
                                alcohol_tg=gram_ep*(alcohol/100),
                                ash_tg=gram_ep*(ash_g/100),
                                sodium_tmg=gram_ep*(sodium_mg/100),
                                magnesium_tmg=gram_ep*(magnesium_mg/100),
                                potassium_tmg=gram_ep*(potassium_mg/100),
                                calcium_tmg=gram_ep*(calcium_mg/100),
                                iron_tmg=gram_ep*(iron_mg/100),
                                zinc_tmg=gram_ep*(zinc_mg/100),
                                retinol_tmug=gram_ep*(retinol/100),
                                betacaroten_tmug=gram_ep*(betacaroten/100),
                                VitA_RAE_tmug=gram_ep*(VitA_RAE/100),
                                VitA_RE_tmug=gram_ep*(VitA_RE/100),#TAKE IT OUT BECAUSE IT IS NO LONGER USED (IT OVERSTATES BETACAROTENS VS RETINOL:)
                                thiamin_tmg=gram_ep*(thiamin_mg/100),
                                riboflavin_tmg=gram_ep*(riboflavin_mg/100),
                                niacin_tmg=gram_ep*(niacin_mg/100),
                                Vit_B12_tmug=gram_ep*(Vit_B12/100),
                                Vit_C_tmg=gram_ep*(Vit_C/100),
                                VitE_tmug=gram_ep*(VitE/100),
                                cholesterol_tmg=gram_ep*(cholesterol_mg/100),
                                iron_non_haem_tmg=gram_ep*(iron_non_haem/100),
                                iron_haem_tmg=gram_ep*(iron_haem/100))

ggplot(foodrecall,aes(x=kcal_JZM,y=kcal))+geom_point()+geom_abline(slope=1,intercept = 0)

#refuse is the inverse to ep
ggplot(foodrecall,aes(x=ep_,y=refuse_))+geom_point()+geom_abline(slope=1,intercept = 0)

#get summaries by household: divide by 7 to get daily intake because questions where for 7 days
colnames(foodrecall)
hhfoodsummary<-ddply(foodrecall,.(interview__key),summarize,kcal_hh=sum(kcal_JZM,na.rm=T)/7,grams_hh=sum(gram_ep,na.rm=T)/7,hhsize=hhsize[1],island=as_factor(island[1]),village=as_factor(village[1]),
                    pr_g_hh=sum(protein_tg,na.rm=T)/7,tfats_g_hh=sum(total_fat_tg,na.rm=T)/7,carbs_g_hh=sum(carbs_tg,na.rm=T)/7,
                     fibre_g_hh=sum(tdfibre_tg,na.rm=T)/7,alcohol_g_hh=sum(alcohol_tg,na.rm=T)/7,ash_g_hh=sum(ash_tg,na.rm=T)/7,
                    sodium_mg_hh=sum(sodium_tmg,na.rm=T)/7,magnesium_mg_hh=sum(magnesium_tmg,na.rm=T)/7,potassium_mg_hh=sum(potassium_tmg,na.rm=T)/7,
                    calcium_mg_hh=sum(calcium_tmg,na.rm=T)/7,iron_mg_hh=sum(iron_tmg,na.rm=T)/7,zinc_mg_hh=sum(zinc_tmg,na.rm=T)/7,
                    retinol_mug_hh=sum(retinol_tmug,na.rm=T)/7,betacaroten_mug_hh=sum(betacaroten_tmug,na.rm=T)/7,VitA_RAE_mug_hh=sum(VitA_RAE_tmug,na.rm=T)/7,
                    VitA_RE_mug_hh=sum(VitA_RE_tmug,na.rm=T)/7,thiamin_mg_hh=sum(thiamin_tmg,na.rm=T)/7,riboflavin_mg_hh=sum(riboflavin_tmg,na.rm=T)/7,
                    niacin_mg_hh=sum(niacin_tmg,na.rm=T)/7,Vit_B12_mug_hh=sum(Vit_B12_tmug,na.rm=T)/7,Vit_C_mg_hh=sum(Vit_C_tmg,na.rm=T)/7,
                    VitE_mug_hh=sum(VitE_tmug,na.rm=T)/7,cholesterol_mg_hh=sum(cholesterol_tmg,na.rm=T)/7,iron_non_haem_mg_hh=sum(iron_non_haem_tmg,na.rm=T)/7,
                    iron_haem_mg_hh=sum(iron_haem_tmg,na.rm=T)/7)
                    
                     
#kcal obtained roughly per capita by dividing by household size (we expect high variation because 1 food recall but median values could be useful)
summary(hhfoodsummary$kcal_hh/hhfoodsummary$hhsize)
hist(log(hhfoodsummary$kcal_hh/hhfoodsummary$hhsize))
hhfoodsummary$pc_kcal<-hhfoodsummary$kcal_hh/hhfoodsummary$hhsize
pckcal_h<-brm(log(pc_kcal)~1+ (1|island/village),data=hhfoodsummary, family="gaussian")
pp_check(pckcal_h,nsamples=100)
ranef_community<-as.data.frame(ranef(pckcal_h, groups="island:village", probs = c(0.1,0.9)))
ranef_community$community<-row.names(ranef_community)
colnames(ranef_community)<-c("estimate","se","conf.low","conf.high","community")

a<-ggplot(hhfoodsummary,aes(x=pc_kcal,y=island))+geom_violin(draw_quantiles = c(0.5),fill="deeppink3",alpha=0.3)+theme_classic()+xlab("kcal per capita per day")+ylab("")+
  geom_vline(xintercept = median(hhfoodsummary$pc_kcal),lty=2)

b<-ggplot(ranef_community,aes(x=estimate,y=community,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")

ranef_island<-as.data.frame(ranef(pckcal_h, groups="island", probs = c(0.1,0.9)))
ranef_island$island<-row.names(ranef_island)
colnames(ranef_island)<-c("estimate","se","conf.low","conf.high","island")

c<-ggplot(ranef_island,aes(x=estimate,y=island,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")
windows()
ggarrange(a,b,c,nrow=1,ncol=3)
#island level kcals in arithmetic scale
country_kcal_intercept=exp(fixef(pckcal_h, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(pckcal_h, groups="island", probs = c(0.1,0.9)))[,-2])
ggplot(country_kcal_intercept,aes(x=island.Estimate.Intercept,y=rownames(country_kcal_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(pckcal_h, probs = c(0.1,0.9))[1]),lty=2)+theme_classic()+xlab("Estimated mean per capita kilocalorie intake")+ylab("")

###############################################################################
#seafood contribution to nutritional intake
#all foods divided by main groups
colnames(foodrecall)
unique(foodrecall$coicop_class)
allfood_contribution<-foodrecall %>% select (interview__key,coicop_class,gram_ep,kcal_JZM:iron_haem_tmg) %>%
  group_by(interview__key,coicop_class)%>%summarise_all(sum,na.rm=T)
allfood_contribution[,3:ncol(allfood_contribution)]<-allfood_contribution[,3:ncol(allfood_contribution)]/7
allfood_contribution<-merge(allfood_contribution,(hhfoodsummary %>%select(interview__key,kcal_hh:iron_haem_mg_hh)),by="interview__key",all.x=T)
allfood_contribution<-allfood_contribution %>%
  mutate(kcal=(kcal_JZM/kcal_hh)*100,
         grams=(gram_ep/grams_hh)*100,
         protein=(protein_tg/pr_g_hh)*100,
         total_fats=(total_fat_tg/tfats_g_hh)*100,
         carbs=(carbs_tg/carbs_g_hh)*100,
         fibre=(tdfibre_tg/fibre_g_hh)*100,
         sodium=(sodium_tmg/sodium_mg_hh)*100,
         magnesium=(magnesium_tmg/magnesium_mg_hh)*100,
         potassium=(potassium_tmg/potassium_mg_hh)*100,
         calcium=(calcium_tmg/calcium_mg_hh)*100,
         iron=(iron_tmg/iron_mg_hh)*100,
         zinc=(zinc_tmg/zinc_mg_hh)*100,
         retinol=(retinol_tmug/retinol_mug_hh)*100,
         betacaroten=(betacaroten_tmug/betacaroten_mug_hh)*100,
         vitA_RAE=(VitA_RAE_tmug/VitA_RAE_mug_hh)*100,
         vitA_RE=(VitA_RE_tmug/VitA_RE_mug_hh)*100,
         thiamin=(thiamin_tmg/thiamin_mg_hh)*100,
         riboflavin=(riboflavin_tmg/riboflavin_mg_hh)*100,
         niacin=(niacin_tmg/niacin_mg_hh)*100,
         vit_B12=(Vit_B12_tmug/Vit_B12_mug_hh)*100,
         vit_C=(Vit_C_tmg/Vit_C_mg_hh)*100,
         vitE=(VitE_tmug/VitE_mug_hh)*100,
         cholesterol=(cholesterol_tmg/cholesterol_mg_hh)*100,
         iron_non_haem=(iron_non_haem_tmg/iron_non_haem_mg_hh)*100,
         iron_haem=(iron_haem_tmg/iron_haem_mg_hh)*100)
#put to long format
allfood_contribution<-allfood_contribution %>% select(interview__key,coicop_class,kcal:iron_haem) %>% select(-vitA_RE) %>%gather(variable,value,-interview__key, -coicop_class)
windows()
ggplot(allfood_contribution)+geom_density_ridges_gradient(aes(x=value, y=variable,col=variable),fill="black",scale=2,rel_min_height = 0.01,alpha=0.5)+facet_wrap(~coicop_class,nrow=1,ncol=12)+
  xlab("% contribution to household intake")+ylab("")+theme(axis.text.y = element_text(size=9),panel.background = element_rect(fill="white",color="black"))+guides(col=F)
ggplot(allfood_contribution)+geom_density_ridges_gradient(aes(x=value, y=coicop_class,col=coicop_class),fill="black",scale=2,rel_min_height = 0.01,alpha=0.5)+facet_wrap(~variable,nrow=3,ncol = 8)+
  xlab("% contribution to household intake")+ylab("")+theme(axis.text.y = element_text(size=9),panel.background = element_rect(fill="white",color="black"))+guides(col=F)

#ranking of food groups
allfood_contribution_rank<-allfood_contribution[,-1] %>% group_by(coicop_class,variable) %>%summarise_all(mean,na.rm=T)%>% group_by(variable) %>%mutate(ranking=order(order(value,variable,decreasing=T)))%>% filter(variable!="cholesterol")
allfood_contribution_rank<-droplevels(allfood_contribution_rank) 
#raster plot
rank_sum<-as.data.frame(allfood_contribution_rank %>% group_by(coicop_class) %>%dplyr::summarise(ranking_sum=sum(ranking)) %>%arrange(ranking_sum))
rank_sum$coicop_class <- reorder(rank_sum$coicop_class, rank_sum$ranking_sum)
levels(rank_sum$coicop_class)
rank_variables<-as.data.frame(allfood_contribution_rank %>% filter(coicop_class %in% "Fish and sea food") %>%arrange(desc(value)))
rank_variables$variable <- reorder(as.factor(rank_variables$variable), rank_variables$value)

rank_variables <-rank_variables %>%mutate(variable=fct_relevel(variable,"grams",after=Inf))
rank_allfoods_fig<-ggplot(allfood_contribution_rank , aes(x = variable, y = coicop_class, fill = as.factor(ranking)))+
  geom_tile(colour = "white", size = 0.1, height = 1) +
  scale_fill_viridis_d("Main source
  ranking",option = "B",direction = -1) +
  labs(x = "", y = "", title = "") +guides(fill=F)+
  theme(#axis.text.x = element_text(angle = 90,hjust=1,vjust=0),
        axis.text.x =element_blank(),
        #legend.position="left",
        plot.margin = ggplot2::margin(t = 0.18, r = 0, b = 0.89, l = 0, "cm"),
        panel.grid = element_blank())+ylim(levels(rank_sum$coicop_class))+
  scale_x_discrete(limits=levels(rank_variables$variable),labels=c(kcal="Kilocalories",
                            protein="Protein",
                            total_fats="Total fats",
                            carbs="Carbohydrates",
                            fibre="Fibre",
                            sodium="Sodium",
                            magnesium="Magnesium",
                            potassium="Potasium",
                            calcium="Calcium",
                            iron="Total iron",
                            grams="Grams",
                            zinc="Zinc",
                            retinol="Retinol",
                            betacaroten="Betacaroten",
                            vitA_RAE="Vitamin A (RAE)",
                            thiamin="Vitamin B1 (thiamin)",
                            riboflavin="Vitamin B2 (riboflavin)",
                            niacin="Vitamin B3 (niacin)",
                            vit_B12="Vitamin B12 (colbalamine)",
                            vit_C="Vitamin C",
                            vitE="Vitamin E",
                            #cholesterol="Cholesterol",
                            iron_non_haem="Non-haem iron",
                            iron_haem="Haem iron"))+coord_flip()

allfoodcont_fig<-ggplot(allfood_contribution_rank, aes(x = variable, y = coicop_class, fill = value))+
  geom_tile(colour = "darkgrey", size = 0.01, height = 1) +
  scale_fill_viridis_c("% contribution",option = "B") +
  labs(x = "", y = "", title = "") +
  theme(#axis.text.x = element_text(angle = 90,hjust=1,vjust=0),
        axis.text.x = element_blank(),
        legend.position="top",
        #panel.background = element_rect(fill = 'black'),
        plot.margin = ggplot2::margin(t = 0.18, r = 0, b = 0.89, l = 0, "cm"),
        panel.grid = element_blank())+ylim(levels(rank_sum$coicop_class))+
  scale_x_discrete(limits=levels(rank_variables$variable),labels=c(kcal="Kilocalories",
                                                                   protein="Protein",
                                                                   total_fats="Total fats",
                                                                   carbs="Carbohydrates",
                                                                   fibre="Fibre",
                                                                   sodium="Sodium",
                                                                   magnesium="Magnesium",
                                                                   potassium="Potasium",
                                                                   calcium="Calcium",
                                                                   iron="Total iron",
                                                                   zinc="Zinc",
                                                                   grams="Grams",
                                                                   retinol="Retinol",
                                                                   betacaroten="Betacaroten",
                                                                   vitA_RAE="Vitamin A (RAE)",
                                                                   thiamin="Vitamin B1 (thiamin)",
                                                                   riboflavin="Vitamin B2 (riboflavin)",
                                                                   niacin="Vitamin B3 (niacin)",
                                                                   vit_B12="Vitamin B12 (colbalamine)",
                                                                   vit_C="Vitamin C",
                                                                   vitE="Vitamin E",
                                                                   #cholesterol="Cholesterol",
                                                                   iron_non_haem="Non-haem iron",
                                                                   iron_haem="Haem iron"))+coord_flip()




#seafood summary
unique(foodrecall$Food_description_HIES)
unique(as_factor(foodrecall$coicop_class))
foodrecall$coicop_class<-as_factor(foodrecall$coicop_class)
seafood2<-foodrecall[foodrecall$coicop_class=="Fish and sea food",]
unique(seafood2$Food_description_HIES)
unique(seafood2$description)
seafood2$Food_description_HIES[seafood2$description=="Lagoon and sandflat fish (fresh or frozen)"]
seafood2$Food_description_HIES[seafood2$description=="octopus"]
unique(seafood2$description[seafood2$Food_description_HIES=="Fish, dried, salted"])
unique(seafood2$description[seafood2$Food_description_HIES=="Snapper"])

#check descriptions
seafood2 %>% dplyr::select(Food_description_HIES, description) %>% dplyr::group_by(Food_description_HIES) %>% dplyr::summarize(desciptions=paste(unique(description), collapse = ', '))

seafood<-ddply(seafood2,.(interview__key),summarize,gseafood_pc=sum(gram_ep)/7/hhsize[1],
               kcal_seafood_hh=sum(kcal_JZM,na.rm=T)/7,grams_seafood_hh=sum(gram_ep)/7,
               pr_g_seafood_hh=sum(protein_tg,na.rm=T)/7,tfats_g_seafood_hh=sum(total_fat_tg,na.rm=T)/7,carbs_g_seafood_hh=sum(carbs_tg,na.rm=T)/7,
               fibre_g_seafood_hh=sum(tdfibre_tg,na.rm=T)/7,alcohol_g_seafood_hh=sum(alcohol_tg,na.rm=T)/7,ash_g_seafood_hh=sum(ash_tg,na.rm=T)/7,
               sodium_mg_seafood_hh=sum(sodium_tmg,na.rm=T)/7,magnesium_mg_seafood_hh=sum(magnesium_tmg,na.rm=T)/7,potassium_mg_seafood_hh=sum(potassium_tmg,na.rm=T)/7,
               calcium_mg_seafood_hh=sum(calcium_tmg,na.rm=T)/7,iron_mg_seafood_hh=sum(iron_tmg,na.rm=T)/7,zinc_mg_seafood_hh=sum(zinc_tmg,na.rm=T)/7,
               retinol_mug_seafood_hh=sum(retinol_tmug,na.rm=T)/7,betacaroten_mug_seafood_hh=sum(betacaroten_tmug,na.rm=T)/7,VitA_RAE_mug_seafood_hh=sum(VitA_RAE_tmug,na.rm=T)/7,
               VitA_RE_mug_seafood_hh=sum(VitA_RE_tmug,na.rm=T)/7,thiamin_mg_seafood_hh=sum(thiamin_tmg,na.rm=T)/7,riboflavin_mg_seafood_hh=sum(riboflavin_tmg,na.rm=T)/7,
               niacin_mg_seafood_hh=sum(niacin_tmg,na.rm=T)/7,Vit_B12_mug_seafood_hh=sum(Vit_B12_tmug,na.rm=T)/7,Vit_C_mg_seafood_hh=sum(Vit_C_tmg,na.rm=T)/7,
               VitE_mug_seafood_hh=sum(VitE_tmug,na.rm=T)/7,cholesterol_mg_seafood_hh=sum(cholesterol_tmg,na.rm=T)/7,iron_non_haem_mg_seafood_hh=sum(iron_non_haem_tmg,na.rm=T)/7,
               iron_haem_mg_seafood_hh=sum(iron_haem_tmg,na.rm=T)/7)

unique(foodrecall$Food_description_HIES[foodrecall$coicop_class=="Fish and sea food"])
hhfoodsummary<-merge(hhfoodsummary,seafood,by="interview__key",all.x=T)
hhfoodsummary[is.na(hhfoodsummary)]<-0

#340 weekly grams of seafood upper limit for 12 ounces per week to minimize exposure to methyl mercury (FDA/EPA, 2004)
#for pregnant women, women planning to become pregnant, nursing mothers, and young children
#https://www.ncbi.nlm.nih.gov/books/NBK305180/

windows()
ggplot(hhfoodsummary,aes(x=gseafood_pc))+geom_histogram(alpha=0.5)+geom_vline(xintercept = 340/7,lty=2)+xlab("Per capita daily grams of seafood consumed")+theme_classic()



#for each household: what proportion of each dietary nutrients comes from seafood
#seafood dependence
hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_seafood=(kcal_seafood_hh/kcal_hh)*100,
         perc_grams_hh_seafood=(grams_seafood_hh/grams_hh)*100,
         perc_pr_hh_seafood=(pr_g_seafood_hh/pr_g_hh)*100,
         perc_tfats_hh_seafood=(tfats_g_seafood_hh/tfats_g_hh)*100,
         perc_carbs_hh_seafood=(carbs_g_seafood_hh/carbs_g_hh)*100,
         perc_fibre_hh_seafood=(fibre_g_seafood_hh/fibre_g_hh)*100,
         perc_sodium_hh_seafood=(sodium_mg_seafood_hh/sodium_mg_hh)*100,
         perc_magnesium_hh_seafood=(magnesium_mg_seafood_hh/magnesium_mg_hh)*100,
         perc_potassium_hh_seafood=(potassium_mg_seafood_hh/potassium_mg_hh)*100,
         perc_calcium_hh_seafood=(calcium_mg_seafood_hh/calcium_mg_hh)*100,
         perc_iron_hh_seafood=(iron_mg_seafood_hh/iron_mg_hh)*100,
         perc_zinc_hh_seafood=(zinc_mg_seafood_hh/zinc_mg_hh)*100,
         perc_retinol_hh_seafood=(retinol_mug_seafood_hh/retinol_mug_hh)*100,
         perc_betacaroten_hh_seafood=(betacaroten_mug_seafood_hh/betacaroten_mug_hh)*100,
         perc_VitA_RAE_hh_seafood=(VitA_RAE_mug_seafood_hh/VitA_RAE_mug_hh)*100,
         perc_VitA_RE_hh_seafood=(VitA_RE_mug_seafood_hh/VitA_RE_mug_hh)*100,
         perc_thiamin_hh_seafood=(thiamin_mg_seafood_hh/thiamin_mg_hh)*100,
         perc_riboflavin_hh_seafood=(riboflavin_mg_seafood_hh/riboflavin_mg_hh)*100,
         perc_niacin_hh_seafood=(niacin_mg_seafood_hh/niacin_mg_hh)*100,
         perc_Vit_B12_hh_seafood=(Vit_B12_mug_seafood_hh/Vit_B12_mug_hh)*100,
         perc_Vit_C_hh_seafood=(Vit_C_mg_seafood_hh/Vit_C_mg_hh)*100,
         perc_VitE_hh_seafood=(VitE_mug_seafood_hh/VitE_mug_hh)*100,
         perc_cholesterol_hh_seafood=(cholesterol_mg_seafood_hh/cholesterol_mg_hh)*100,
         perc_iron_non_haem_hh_seafood=(iron_non_haem_mg_seafood_hh/iron_non_haem_mg_hh)*100,
         perc_iron_haem_hh_seafood=(iron_haem_mg_seafood_hh/iron_haem_mg_hh)*100)


#percents overall
perc_contribution<-hhfoodsummary %>%
  select(perc_kcal_hh_seafood:perc_iron_haem_hh_seafood) %>%select(-perc_VitA_RE_hh_seafood)
colnames(perc_contribution)<-c("Kcal","Grams","Protein","Total fats","Carbohydrates","Fibre","Sodium","Magnesium","Potassium","Calcium","Total iron","Zinc","Retinol","Betacaroten","Vitamin A (RAE)","Thiamin","Rivoflavin","Niacin","Vitamin B12","Vitamin C","Vitamin E","Cholesterol","Non-haem iron", "Haem iron")
perc_contribution2<-melt(perc_contribution)
#do density
fac2 <- with( perc_contribution2, reorder(variable, value, median, order = TRUE, na.rm=T))
perc_contribution2<- within(perc_contribution2, 
                                  variable<- factor(variable, 
                                                        levels=levels(fac2)))

unique(perc_contribution2$variable)

allseafoodcont_fig2<-ggplot(NULL)+  stat_density_ridges(data=perc_contribution2 %>%filter(variable!="Cholesterol"),aes(x=value, y=variable, fill=stat(x)),scale=3,rel_min_height = 0.01,alpha=0.5,
                                                       geom = "density_ridges_gradient", calc_ecdf = TRUE,
                                                       quantiles = 2, quantile_lines = TRUE,lwd=1.05)+
  guides(fill=F)+
  scale_fill_viridis_c(option = "B")+xlab("")+ylab("")+
  theme(panel.background = element_rect(fill="white",color="black"))+ggtitle ("Fish and seafood")+xlim(c(0,100))


ggarrange(allfoodcont_fig,allseafoodcont_fig2)



#contribution per seafood group
unique(seafood2$Food_description_HIES)

#create groups
seafood2$seafood_group<-ifelse(seafood2$Food_description_HIES=="Tuna canned, not further specified"|seafood2$Food_description_HIES=="Mackerel, canned, not further specified","Tinned fish",
                               ifelse(seafood2$Food_description_HIES=="Fish, pelagic/ocean, not further specified"|seafood2$Food_description_HIES=="Snapper"|(seafood2$Food_description_HIES=="Fish, not further specified" &!seafood2$description=="Lagoon and sandflat fish (fresh or frozen)"), "Pelagic & other fish",
                                      ifelse(seafood2$Food_description_HIES=="Fish, dried, salted","Dried & salted fish",
                                             ifelse(seafood2$Food_description_HIES=="Fish, reef, not further specified"|(seafood2$Food_description_HIES=="Fish, not further specified" &seafood2$description=="Lagoon and sandflat fish (fresh or frozen)"), "Reef fish",
                                                    ifelse(seafood2$Food_description_HIES=="Shark"|seafood2$Food_description_HIES=="Stingray", "Sharks & rays",
                                                           ifelse(seafood2$Food_description_HIES=="Seaweed","Seaweed","Invertebrates"))))))
                               


#non tinned seafood, reef fish, invertebrates and sharks and rays consumpton
unique(seafood2$Food_description_HIES)
unique(as_factor(foodrecall$coicop_class))
tinnedsf<-foodrecall[foodrecall$coicop_class=="Fish and sea food"& (foodrecall$Food_description_HIES=="Tuna canned, not further specified"|foodrecall$Food_description_HIES=="Mackerel, canned, not further specified"),]
tinnedsf<-ddply(tinnedsf,.(interview__key),summarize,gtinnedsf_pc=sum(gram_ep)/7/hhsize[1],
               kcal_tinnedsf_hh=sum(kcal_JZM,na.rm=T)/7, grams_tinnedsf_hh=sum(gram_ep,na.rm=T)/7,
               pr_g_tinnedsf_hh=sum(protein_tg,na.rm=T)/7,tfats_g_tinnedsf_hh=sum(total_fat_tg,na.rm=T)/7,carbs_g_tinnedsf_hh=sum(carbs_tg,na.rm=T)/7,
               fibre_g_tinnedsf_hh=sum(tdfibre_tg,na.rm=T)/7,alcohol_g_tinnedsf_hh=sum(alcohol_tg,na.rm=T)/7,ash_g_tinnedsf_hh=sum(ash_tg,na.rm=T)/7,
               sodium_mg_tinnedsf_hh=sum(sodium_tmg,na.rm=T)/7,magnesium_mg_tinnedsf_hh=sum(magnesium_tmg,na.rm=T)/7,potassium_mg_tinnedsf_hh=sum(potassium_tmg,na.rm=T)/7,
               calcium_mg_tinnedsf_hh=sum(calcium_tmg,na.rm=T)/7,iron_mg_tinnedsf_hh=sum(iron_tmg,na.rm=T)/7,zinc_mg_tinnedsf_hh=sum(zinc_tmg,na.rm=T)/7,
               retinol_mug_tinnedsf_hh=sum(retinol_tmug,na.rm=T)/7,betacaroten_mug_tinnedsf_hh=sum(betacaroten_tmug,na.rm=T)/7,VitA_RAE_mug_tinnedsf_hh=sum(VitA_RAE_tmug,na.rm=T)/7,
               VitA_RE_mug_tinnedsf_hh=sum(VitA_RE_tmug,na.rm=T)/7,thiamin_mg_tinnedsf_hh=sum(thiamin_tmg,na.rm=T)/7,riboflavin_mg_tinnedsf_hh=sum(riboflavin_tmg,na.rm=T)/7,
               niacin_mg_tinnedsf_hh=sum(niacin_tmg,na.rm=T)/7,Vit_B12_mug_tinnedsf_hh=sum(Vit_B12_tmug,na.rm=T)/7,Vit_C_mg_tinnedsf_hh=sum(Vit_C_tmg,na.rm=T)/7,
               VitE_mug_tinnedsf_hh=sum(VitE_tmug,na.rm=T)/7,cholesterol_mg_tinnedsf_hh=sum(cholesterol_tmg,na.rm=T)/7,iron_non_haem_mg_tinnedsf_hh=sum(iron_non_haem_tmg,na.rm=T)/7,
               iron_haem_mg_tinnedsf_hh=sum(iron_haem_tmg,na.rm=T)/7)


hhfoodsummary<-merge(hhfoodsummary,tinnedsf,by="interview__key",all.x=T)
hhfoodsummary[is.na(hhfoodsummary)]<-0

#for each household: what proportion of each dietary nutrients comes from tinnedsf
#tinnedsf dependence
hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_tinnedsf=(kcal_tinnedsf_hh/kcal_seafood_hh)*100,
         perc_grams_hh_tinnedsf=(grams_tinnedsf_hh/grams_seafood_hh)*100,
         perc_pr_hh_tinnedsf=(pr_g_tinnedsf_hh/pr_g_seafood_hh)*100,
         perc_tfats_hh_tinnedsf=(tfats_g_tinnedsf_hh/tfats_g_seafood_hh)*100,
         perc_carbs_hh_tinnedsf=(carbs_g_tinnedsf_hh/carbs_g_seafood_hh)*100,
         perc_fibre_hh_tinnedsf=(fibre_g_tinnedsf_hh/fibre_g_seafood_hh)*100,
         perc_sodium_hh_tinnedsf=(sodium_mg_tinnedsf_hh/sodium_mg_seafood_hh)*100,
         perc_magnesium_hh_tinnedsf=(magnesium_mg_tinnedsf_hh/magnesium_mg_seafood_hh)*100,
         perc_potassium_hh_tinnedsf=(potassium_mg_tinnedsf_hh/potassium_mg_seafood_hh)*100,
         perc_calcium_hh_tinnedsf=(calcium_mg_tinnedsf_hh/calcium_mg_seafood_hh)*100,
         perc_iron_hh_tinnedsf=(iron_mg_tinnedsf_hh/iron_mg_seafood_hh)*100,
         perc_zinc_hh_tinnedsf=(zinc_mg_tinnedsf_hh/zinc_mg_seafood_hh)*100,
         perc_retinol_hh_tinnedsf=(retinol_mug_tinnedsf_hh/retinol_mug_seafood_hh)*100,
         perc_betacaroten_hh_tinnedsf=(betacaroten_mug_tinnedsf_hh/betacaroten_mug_seafood_hh)*100,
         perc_VitA_RAE_hh_tinnedsf=(VitA_RAE_mug_tinnedsf_hh/VitA_RAE_mug_seafood_hh)*100,
         perc_VitA_RE_hh_tinnedsf=(VitA_RE_mug_tinnedsf_hh/VitA_RE_mug_seafood_hh)*100,
         perc_thiamin_hh_tinnedsf=(thiamin_mg_tinnedsf_hh/thiamin_mg_seafood_hh)*100,
         perc_riboflavin_hh_tinnedsf=(riboflavin_mg_tinnedsf_hh/riboflavin_mg_seafood_hh)*100,
         perc_niacin_hh_tinnedsf=(niacin_mg_tinnedsf_hh/niacin_mg_seafood_hh)*100,
         perc_Vit_B12_hh_tinnedsf=(Vit_B12_mug_tinnedsf_hh/Vit_B12_mug_seafood_hh)*100,
         perc_Vit_C_hh_tinnedsf=(Vit_C_mg_tinnedsf_hh/Vit_C_mg_seafood_hh)*100,
         perc_VitE_hh_tinnedsf=(VitE_mug_tinnedsf_hh/VitE_mug_seafood_hh)*100,
         perc_cholesterol_hh_tinnedsf=(cholesterol_mg_tinnedsf_hh/cholesterol_mg_seafood_hh)*100,
         perc_iron_non_haem_hh_tinnedsf=(iron_non_haem_mg_tinnedsf_hh/iron_non_haem_mg_seafood_hh)*100,
         perc_iron_haem_hh_tinnedsf=(iron_haem_mg_tinnedsf_hh/iron_haem_mg_seafood_hh)*100)

hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_tinnedsf=(kcal_tinnedsf_hh/kcal_hh)*100,
         perc_grams_hh_tinnedsf=(grams_tinnedsf_hh/grams_hh)*100,
         perc_pr_hh_tinnedsf=(pr_g_tinnedsf_hh/pr_g_hh)*100,
         perc_tfats_hh_tinnedsf=(tfats_g_tinnedsf_hh/tfats_g_hh)*100,
         perc_carbs_hh_tinnedsf=(carbs_g_tinnedsf_hh/carbs_g_hh)*100,
         perc_fibre_hh_tinnedsf=(fibre_g_tinnedsf_hh/fibre_g_hh)*100,
         perc_sodium_hh_tinnedsf=(sodium_mg_tinnedsf_hh/sodium_mg_hh)*100,
         perc_magnesium_hh_tinnedsf=(magnesium_mg_tinnedsf_hh/magnesium_mg_hh)*100,
         perc_potassium_hh_tinnedsf=(potassium_mg_tinnedsf_hh/potassium_mg_hh)*100,
         perc_calcium_hh_tinnedsf=(calcium_mg_tinnedsf_hh/calcium_mg_hh)*100,
         perc_iron_hh_tinnedsf=(iron_mg_tinnedsf_hh/iron_mg_hh)*100,
         perc_zinc_hh_tinnedsf=(zinc_mg_tinnedsf_hh/zinc_mg_hh)*100,
         perc_retinol_hh_tinnedsf=(retinol_mug_tinnedsf_hh/retinol_mug_hh)*100,
         perc_betacaroten_hh_tinnedsf=(betacaroten_mug_tinnedsf_hh/betacaroten_mug_hh)*100,
         perc_VitA_RAE_hh_tinnedsf=(VitA_RAE_mug_tinnedsf_hh/VitA_RAE_mug_hh)*100,
         perc_VitA_RE_hh_tinnedsf=(VitA_RE_mug_tinnedsf_hh/VitA_RE_mug_hh)*100,
         perc_thiamin_hh_tinnedsf=(thiamin_mg_tinnedsf_hh/thiamin_mg_hh)*100,
         perc_riboflavin_hh_tinnedsf=(riboflavin_mg_tinnedsf_hh/riboflavin_mg_hh)*100,
         perc_niacin_hh_tinnedsf=(niacin_mg_tinnedsf_hh/niacin_mg_hh)*100,
         perc_Vit_B12_hh_tinnedsf=(Vit_B12_mug_tinnedsf_hh/Vit_B12_mug_hh)*100,
         perc_Vit_C_hh_tinnedsf=(Vit_C_mg_tinnedsf_hh/Vit_C_mg_hh)*100,
         perc_VitE_hh_tinnedsf=(VitE_mug_tinnedsf_hh/VitE_mug_hh)*100,
         perc_cholesterol_hh_tinnedsf=(cholesterol_mg_tinnedsf_hh/cholesterol_mg_hh)*100,
         perc_iron_non_haem_hh_tinnedsf=(iron_non_haem_mg_tinnedsf_hh/iron_non_haem_mg_hh)*100,
         perc_iron_haem_hh_tinnedsf=(iron_haem_mg_tinnedsf_hh/iron_haem_mg_hh)*100)
#percents overall
perc_contribution_tinnedsf<-hhfoodsummary %>%
  select(perc_kcal_hh_tinnedsf:perc_iron_haem_hh_tinnedsf)%>%select(-perc_VitA_RE_hh_tinnedsf)
colnames(perc_contribution_tinnedsf)<-c("Kcal","Grams","Protein","Total fats","Carbohydrates","Fibre","Sodium","Magnesium","Potassium","Calcium","Total iron","Zinc","Retinol","Betacaroten","Vitamin A (RAE)","Thiamin","Rivoflavin","Niacin","Vitamin B12","Vitamin C","Vitamin E","Cholesterol","Non-haem iron", "Haem iron")
summary(perc_contribution_tinnedsf)
perc_contribution_tinnedsf2<-melt(perc_contribution_tinnedsf)
unique(perc_contribution_tinnedsf2$variable)
perc_contribution_tinnedsf2<- within(perc_contribution_tinnedsf2, 
                                    variable<- factor(variable, 
                                                      levels=levels(fac2)))

#other fish (not reef)--snapper included here
fishnoreef<-foodrecall[foodrecall$coicop_class=="Fish and sea food"& (foodrecall$Food_description_HIES=="Fish, pelagic/ocean, not further specified"|foodrecall$Food_description_HIES=="Snapper"|(foodrecall$Food_description_HIES=="Fish, not further specified" &!foodrecall$description=="Lagoon and sandflat fish (fresh or frozen)")),]
summary(as.factor(fishnoreef$description))
summary(as.factor(fishnoreef$Food_description_HIES[fishnoreef$description=="Sea worm"]))

#seaworms missclassified
fishnoreef<-fishnoreef %>%filter(description!="Sea worm")

fishnoreef<-ddply(fishnoreef,.(interview__key),summarize,gfishnoreef_pc=sum(gram_ep)/7/hhsize[1],
               kcal_fishnoreef_hh=sum(kcal_JZM,na.rm=T)/7,grams_fishnoreef_hh=sum(gram_ep,na.rm=T)/7,
               pr_g_fishnoreef_hh=sum(protein_tg,na.rm=T)/7,tfats_g_fishnoreef_hh=sum(total_fat_tg,na.rm=T)/7,carbs_g_fishnoreef_hh=sum(carbs_tg,na.rm=T)/7,
               fibre_g_fishnoreef_hh=sum(tdfibre_tg,na.rm=T)/7,alcohol_g_fishnoreef_hh=sum(alcohol_tg,na.rm=T)/7,ash_g_fishnoreef_hh=sum(ash_tg,na.rm=T)/7,
               sodium_mg_fishnoreef_hh=sum(sodium_tmg,na.rm=T)/7,magnesium_mg_fishnoreef_hh=sum(magnesium_tmg,na.rm=T)/7,potassium_mg_fishnoreef_hh=sum(potassium_tmg,na.rm=T)/7,
               calcium_mg_fishnoreef_hh=sum(calcium_tmg,na.rm=T)/7,iron_mg_fishnoreef_hh=sum(iron_tmg,na.rm=T)/7,zinc_mg_fishnoreef_hh=sum(zinc_tmg,na.rm=T)/7,
               retinol_mug_fishnoreef_hh=sum(retinol_tmug,na.rm=T)/7,betacaroten_mug_fishnoreef_hh=sum(betacaroten_tmug,na.rm=T)/7,VitA_RAE_mug_fishnoreef_hh=sum(VitA_RAE_tmug,na.rm=T)/7,
               VitA_RE_mug_fishnoreef_hh=sum(VitA_RE_tmug,na.rm=T)/7,thiamin_mg_fishnoreef_hh=sum(thiamin_tmg,na.rm=T)/7,riboflavin_mg_fishnoreef_hh=sum(riboflavin_tmg,na.rm=T)/7,
               niacin_mg_fishnoreef_hh=sum(niacin_tmg,na.rm=T)/7,Vit_B12_mug_fishnoreef_hh=sum(Vit_B12_tmug,na.rm=T)/7,Vit_C_mg_fishnoreef_hh=sum(Vit_C_tmg,na.rm=T)/7,
               VitE_mug_fishnoreef_hh=sum(VitE_tmug,na.rm=T)/7,cholesterol_mg_fishnoreef_hh=sum(cholesterol_tmg,na.rm=T)/7,iron_non_haem_mg_fishnoreef_hh=sum(iron_non_haem_tmg,na.rm=T)/7,
               iron_haem_mg_fishnoreef_hh=sum(iron_haem_tmg,na.rm=T)/7)



hhfoodsummary<-merge(hhfoodsummary,fishnoreef,by="interview__key",all.x=T)
hhfoodsummary[is.na(hhfoodsummary)]<-0

#for each household: what proportion of each dietary nutrients comes from fishnoreef
#fishnoreef dependence
hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_fishnoreef=(kcal_fishnoreef_hh/kcal_seafood_hh)*100,
         perc_grams_hh_fishnoreef=(grams_fishnoreef_hh/grams_seafood_hh)*100,
         perc_pr_hh_fishnoreef=(pr_g_fishnoreef_hh/pr_g_seafood_hh)*100,
         perc_tfats_hh_fishnoreef=(tfats_g_fishnoreef_hh/tfats_g_seafood_hh)*100,
         perc_carbs_hh_fishnoreef=(carbs_g_fishnoreef_hh/carbs_g_seafood_hh)*100,
         perc_fibre_hh_fishnoreef=(fibre_g_fishnoreef_hh/fibre_g_seafood_hh)*100,
         perc_sodium_hh_fishnoreef=(sodium_mg_fishnoreef_hh/sodium_mg_seafood_hh)*100,
         perc_magnesium_hh_fishnoreef=(magnesium_mg_fishnoreef_hh/magnesium_mg_seafood_hh)*100,
         perc_potassium_hh_fishnoreef=(potassium_mg_fishnoreef_hh/potassium_mg_seafood_hh)*100,
         perc_calcium_hh_fishnoreef=(calcium_mg_fishnoreef_hh/calcium_mg_seafood_hh)*100,
         perc_iron_hh_fishnoreef=(iron_mg_fishnoreef_hh/iron_mg_seafood_hh)*100,
         perc_zinc_hh_fishnoreef=(zinc_mg_fishnoreef_hh/zinc_mg_seafood_hh)*100,
         perc_retinol_hh_fishnoreef=(retinol_mug_fishnoreef_hh/retinol_mug_seafood_hh)*100,
         perc_betacaroten_hh_fishnoreef=(betacaroten_mug_fishnoreef_hh/betacaroten_mug_seafood_hh)*100,
         perc_VitA_RAE_hh_fishnoreef=(VitA_RAE_mug_fishnoreef_hh/VitA_RAE_mug_seafood_hh)*100,
         perc_VitA_RE_hh_fishnoreef=(VitA_RE_mug_fishnoreef_hh/VitA_RE_mug_seafood_hh)*100,
         perc_thiamin_hh_fishnoreef=(thiamin_mg_fishnoreef_hh/thiamin_mg_seafood_hh)*100,
         perc_riboflavin_hh_fishnoreef=(riboflavin_mg_fishnoreef_hh/riboflavin_mg_seafood_hh)*100,
         perc_niacin_hh_fishnoreef=(niacin_mg_fishnoreef_hh/niacin_mg_seafood_hh)*100,
         perc_Vit_B12_hh_fishnoreef=(Vit_B12_mug_fishnoreef_hh/Vit_B12_mug_seafood_hh)*100,
         perc_Vit_C_hh_fishnoreef=(Vit_C_mg_fishnoreef_hh/Vit_C_mg_seafood_hh)*100,
         perc_VitE_hh_fishnoreef=(VitE_mug_fishnoreef_hh/VitE_mug_seafood_hh)*100,
         perc_cholesterol_hh_fishnoreef=(cholesterol_mg_fishnoreef_hh/cholesterol_mg_seafood_hh)*100,
         perc_iron_non_haem_hh_fishnoreef=(iron_non_haem_mg_fishnoreef_hh/iron_non_haem_mg_seafood_hh)*100,
         perc_iron_haem_hh_fishnoreef=(iron_haem_mg_fishnoreef_hh/iron_haem_mg_seafood_hh)*100)

hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_fishnoreef=(kcal_fishnoreef_hh/kcal_hh)*100,
         perc_grams_hh_fishnoreef=(grams_fishnoreef_hh/grams_hh)*100,
         perc_pr_hh_fishnoreef=(pr_g_fishnoreef_hh/pr_g_hh)*100,
         perc_tfats_hh_fishnoreef=(tfats_g_fishnoreef_hh/tfats_g_hh)*100,
         perc_carbs_hh_fishnoreef=(carbs_g_fishnoreef_hh/carbs_g_hh)*100,
         perc_fibre_hh_fishnoreef=(fibre_g_fishnoreef_hh/fibre_g_hh)*100,
         perc_sodium_hh_fishnoreef=(sodium_mg_fishnoreef_hh/sodium_mg_hh)*100,
         perc_magnesium_hh_fishnoreef=(magnesium_mg_fishnoreef_hh/magnesium_mg_hh)*100,
         perc_potassium_hh_fishnoreef=(potassium_mg_fishnoreef_hh/potassium_mg_hh)*100,
         perc_calcium_hh_fishnoreef=(calcium_mg_fishnoreef_hh/calcium_mg_hh)*100,
         perc_iron_hh_fishnoreef=(iron_mg_fishnoreef_hh/iron_mg_hh)*100,
         perc_zinc_hh_fishnoreef=(zinc_mg_fishnoreef_hh/zinc_mg_hh)*100,
         perc_retinol_hh_fishnoreef=(retinol_mug_fishnoreef_hh/retinol_mug_hh)*100,
         perc_betacaroten_hh_fishnoreef=(betacaroten_mug_fishnoreef_hh/betacaroten_mug_hh)*100,
         perc_VitA_RAE_hh_fishnoreef=(VitA_RAE_mug_fishnoreef_hh/VitA_RAE_mug_hh)*100,
         perc_VitA_RE_hh_fishnoreef=(VitA_RE_mug_fishnoreef_hh/VitA_RE_mug_hh)*100,
         perc_thiamin_hh_fishnoreef=(thiamin_mg_fishnoreef_hh/thiamin_mg_hh)*100,
         perc_riboflavin_hh_fishnoreef=(riboflavin_mg_fishnoreef_hh/riboflavin_mg_hh)*100,
         perc_niacin_hh_fishnoreef=(niacin_mg_fishnoreef_hh/niacin_mg_hh)*100,
         perc_Vit_B12_hh_fishnoreef=(Vit_B12_mug_fishnoreef_hh/Vit_B12_mug_hh)*100,
         perc_Vit_C_hh_fishnoreef=(Vit_C_mg_fishnoreef_hh/Vit_C_mg_hh)*100,
         perc_VitE_hh_fishnoreef=(VitE_mug_fishnoreef_hh/VitE_mug_hh)*100,
         perc_cholesterol_hh_fishnoreef=(cholesterol_mg_fishnoreef_hh/cholesterol_mg_hh)*100,
         perc_iron_non_haem_hh_fishnoreef=(iron_non_haem_mg_fishnoreef_hh/iron_non_haem_mg_hh)*100,
         perc_iron_haem_hh_fishnoreef=(iron_haem_mg_fishnoreef_hh/iron_haem_mg_hh)*100)
#percents overall
perc_contribution_fishnoreef<-hhfoodsummary %>%
  select(perc_kcal_hh_fishnoreef:perc_iron_haem_hh_fishnoreef) %>%select(-perc_VitA_RE_hh_fishnoreef)
colnames(perc_contribution_fishnoreef)<-c("Kcal","Grams","Protein","Total fats","Carbohydrates","Fibre","Sodium","Magnesium","Potassium","Calcium","Total iron","Zinc","Retinol","Betacaroten","Vitamin A (RAE)", "Thiamin","Rivoflavin","Niacin","Vitamin B12","Vitamin C","Vitamin E","Cholesterol","Non-haem iron", "Haem iron")
perc_contribution_fishnoreef2<-melt(perc_contribution_fishnoreef)
perc_contribution_fishnoreef2<- within(perc_contribution_fishnoreef2, 
                                    variable<- factor(variable, 
                                                      levels=levels(fac2)))

#dried fish
driedfish<-foodrecall[foodrecall$coicop_class=="Fish and sea food"& foodrecall$Food_description_HIES=="Fish, dried, salted",]
driedfish<-ddply(driedfish,.(interview__key),summarize,gdriedfish_pc=sum(gram_ep)/7/hhsize[1],
                kcal_driedfish_hh=sum(kcal_JZM,na.rm=T)/7,grams_driedfish_hh=sum(gram_ep,na.rm=T)/7,
                pr_g_driedfish_hh=sum(protein_tg,na.rm=T)/7,tfats_g_driedfish_hh=sum(total_fat_tg,na.rm=T)/7,carbs_g_driedfish_hh=sum(carbs_tg,na.rm=T)/7,
                fibre_g_driedfish_hh=sum(tdfibre_tg,na.rm=T)/7,alcohol_g_driedfish_hh=sum(alcohol_tg,na.rm=T)/7,ash_g_driedfish_hh=sum(ash_tg,na.rm=T)/7,
                sodium_mg_driedfish_hh=sum(sodium_tmg,na.rm=T)/7,magnesium_mg_driedfish_hh=sum(magnesium_tmg,na.rm=T)/7,potassium_mg_driedfish_hh=sum(potassium_tmg,na.rm=T)/7,
                calcium_mg_driedfish_hh=sum(calcium_tmg,na.rm=T)/7,iron_mg_driedfish_hh=sum(iron_tmg,na.rm=T)/7,zinc_mg_driedfish_hh=sum(zinc_tmg,na.rm=T)/7,
                retinol_mug_driedfish_hh=sum(retinol_tmug,na.rm=T)/7,betacaroten_mug_driedfish_hh=sum(betacaroten_tmug,na.rm=T)/7,VitA_RAE_mug_driedfish_hh=sum(VitA_RAE_tmug,na.rm=T)/7,
                VitA_RE_mug_driedfish_hh=sum(VitA_RE_tmug,na.rm=T)/7,thiamin_mg_driedfish_hh=sum(thiamin_tmg,na.rm=T)/7,riboflavin_mg_driedfish_hh=sum(riboflavin_tmg,na.rm=T)/7,
                niacin_mg_driedfish_hh=sum(niacin_tmg,na.rm=T)/7,Vit_B12_mug_driedfish_hh=sum(Vit_B12_tmug,na.rm=T)/7,Vit_C_mg_driedfish_hh=sum(Vit_C_tmg,na.rm=T)/7,
                VitE_mug_driedfish_hh=sum(VitE_tmug,na.rm=T)/7,cholesterol_mg_driedfish_hh=sum(cholesterol_tmg,na.rm=T)/7,iron_non_haem_mg_driedfish_hh=sum(iron_non_haem_tmg,na.rm=T)/7,
                iron_haem_mg_driedfish_hh=sum(iron_haem_tmg,na.rm=T)/7)


hhfoodsummary<-merge(hhfoodsummary,driedfish,by="interview__key",all.x=T)
hhfoodsummary[is.na(hhfoodsummary)]<-0

#for each household: what proportion of each dietary nutrients comes from driedfish
#driedfish dependence
hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_driedfish=(kcal_driedfish_hh/kcal_seafood_hh)*100,
         perc_grams_hh_driedfish=(grams_driedfish_hh/grams_seafood_hh)*100,
         perc_pr_hh_driedfish=(pr_g_driedfish_hh/pr_g_seafood_hh)*100,
         perc_tfats_hh_driedfish=(tfats_g_driedfish_hh/tfats_g_seafood_hh)*100,
         perc_carbs_hh_driedfish=(carbs_g_driedfish_hh/carbs_g_seafood_hh)*100,
         perc_fibre_hh_driedfish=(fibre_g_driedfish_hh/fibre_g_seafood_hh)*100,
         perc_sodium_hh_driedfish=(sodium_mg_driedfish_hh/sodium_mg_seafood_hh)*100,
         perc_magnesium_hh_driedfish=(magnesium_mg_driedfish_hh/magnesium_mg_seafood_hh)*100,
         perc_potassium_hh_driedfish=(potassium_mg_driedfish_hh/potassium_mg_seafood_hh)*100,
         perc_calcium_hh_driedfish=(calcium_mg_driedfish_hh/calcium_mg_seafood_hh)*100,
         perc_iron_hh_driedfish=(iron_mg_driedfish_hh/iron_mg_seafood_hh)*100,
         perc_zinc_hh_driedfish=(zinc_mg_driedfish_hh/zinc_mg_seafood_hh)*100,
         perc_retinol_hh_driedfish=(retinol_mug_driedfish_hh/retinol_mug_seafood_hh)*100,
         perc_betacaroten_hh_driedfish=(betacaroten_mug_driedfish_hh/betacaroten_mug_seafood_hh)*100,
         perc_VitA_RAE_hh_driedfish=(VitA_RAE_mug_driedfish_hh/VitA_RAE_mug_seafood_hh)*100,
         perc_VitA_RE_hh_driedfish=(VitA_RE_mug_driedfish_hh/VitA_RE_mug_seafood_hh)*100,
         perc_thiamin_hh_driedfish=(thiamin_mg_driedfish_hh/thiamin_mg_seafood_hh)*100,
         perc_riboflavin_hh_driedfish=(riboflavin_mg_driedfish_hh/riboflavin_mg_seafood_hh)*100,
         perc_niacin_hh_driedfish=(niacin_mg_driedfish_hh/niacin_mg_seafood_hh)*100,
         perc_Vit_B12_hh_driedfish=(Vit_B12_mug_driedfish_hh/Vit_B12_mug_seafood_hh)*100,
         perc_Vit_C_hh_driedfish=(Vit_C_mg_driedfish_hh/Vit_C_mg_seafood_hh)*100,
         perc_VitE_hh_driedfish=(VitE_mug_driedfish_hh/VitE_mug_seafood_hh)*100,
         perc_cholesterol_hh_driedfish=(cholesterol_mg_driedfish_hh/cholesterol_mg_seafood_hh)*100,
         perc_iron_non_haem_hh_driedfish=(iron_non_haem_mg_driedfish_hh/iron_non_haem_mg_seafood_hh)*100,
         perc_iron_haem_hh_driedfish=(iron_haem_mg_driedfish_hh/iron_haem_mg_seafood_hh)*100)

hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_driedfish=(kcal_driedfish_hh/kcal_hh)*100,
         perc_grams_hh_driedfish=(grams_driedfish_hh/grams_hh)*100,
         perc_pr_hh_driedfish=(pr_g_driedfish_hh/pr_g_hh)*100,
         perc_tfats_hh_driedfish=(tfats_g_driedfish_hh/tfats_g_hh)*100,
         perc_carbs_hh_driedfish=(carbs_g_driedfish_hh/carbs_g_hh)*100,
         perc_fibre_hh_driedfish=(fibre_g_driedfish_hh/fibre_g_hh)*100,
         perc_sodium_hh_driedfish=(sodium_mg_driedfish_hh/sodium_mg_hh)*100,
         perc_magnesium_hh_driedfish=(magnesium_mg_driedfish_hh/magnesium_mg_hh)*100,
         perc_potassium_hh_driedfish=(potassium_mg_driedfish_hh/potassium_mg_hh)*100,
         perc_calcium_hh_driedfish=(calcium_mg_driedfish_hh/calcium_mg_hh)*100,
         perc_iron_hh_driedfish=(iron_mg_driedfish_hh/iron_mg_hh)*100,
         perc_zinc_hh_driedfish=(zinc_mg_driedfish_hh/zinc_mg_hh)*100,
         perc_retinol_hh_driedfish=(retinol_mug_driedfish_hh/retinol_mug_hh)*100,
         perc_betacaroten_hh_driedfish=(betacaroten_mug_driedfish_hh/betacaroten_mug_hh)*100,
         perc_VitA_RAE_hh_driedfish=(VitA_RAE_mug_driedfish_hh/VitA_RAE_mug_hh)*100,
         perc_VitA_RE_hh_driedfish=(VitA_RE_mug_driedfish_hh/VitA_RE_mug_hh)*100,
         perc_thiamin_hh_driedfish=(thiamin_mg_driedfish_hh/thiamin_mg_hh)*100,
         perc_riboflavin_hh_driedfish=(riboflavin_mg_driedfish_hh/riboflavin_mg_hh)*100,
         perc_niacin_hh_driedfish=(niacin_mg_driedfish_hh/niacin_mg_hh)*100,
         perc_Vit_B12_hh_driedfish=(Vit_B12_mug_driedfish_hh/Vit_B12_mug_hh)*100,
         perc_Vit_C_hh_driedfish=(Vit_C_mg_driedfish_hh/Vit_C_mg_hh)*100,
         perc_VitE_hh_driedfish=(VitE_mug_driedfish_hh/VitE_mug_hh)*100,
         perc_cholesterol_hh_driedfish=(cholesterol_mg_driedfish_hh/cholesterol_mg_hh)*100,
         perc_iron_non_haem_hh_driedfish=(iron_non_haem_mg_driedfish_hh/iron_non_haem_mg_hh)*100,
         perc_iron_haem_hh_driedfish=(iron_haem_mg_driedfish_hh/iron_haem_mg_hh)*100)

#percents overall
perc_contribution_driedfish<-hhfoodsummary %>%
  select(perc_kcal_hh_driedfish:perc_iron_haem_hh_driedfish) %>%select(-perc_VitA_RE_hh_driedfish)
colnames(perc_contribution_driedfish)<-c("Kcal","Grams","Protein","Total fats","Carbohydrates","Fibre","Sodium","Magnesium","Potassium","Calcium","Total iron","Zinc","Retinol","Betacaroten","Vitamin A (RAE)","Thiamin","Rivoflavin","Niacin","Vitamin B12","Vitamin C","Vitamin E","Cholesterol","Non-haem iron", "Haem iron")
perc_contribution_driedfish2<-melt(perc_contribution_driedfish)
perc_contribution_driedfish2<- within(perc_contribution_driedfish2, 
                                     variable<- factor(variable, 
                                                       levels=levels(fac2)))

#reef fish
reeffish<-foodrecall[foodrecall$coicop_class=="Fish and sea food"& (foodrecall$Food_description_HIES=="Fish, reef, not further specified"|(foodrecall$Food_description_HIES=="Fish, not further specified" &foodrecall$description=="Lagoon and sandflat fish (fresh or frozen)")),]
summary(as.factor(reeffish$description))

reeffish<-ddply(reeffish,.(interview__key),summarize,greeffish_pc=sum(gram_ep)/7/hhsize[1],
               kcal_reeffish_hh=sum(kcal_JZM,na.rm=T)/7,grams_reeffish_hh=sum(gram_ep,na.rm=T)/7,
               pr_g_reeffish_hh=sum(protein_tg,na.rm=T)/7,tfats_g_reeffish_hh=sum(total_fat_tg,na.rm=T)/7,carbs_g_reeffish_hh=sum(carbs_tg,na.rm=T)/7,
               fibre_g_reeffish_hh=sum(tdfibre_tg,na.rm=T)/7,alcohol_g_reeffish_hh=sum(alcohol_tg,na.rm=T)/7,ash_g_reeffish_hh=sum(ash_tg,na.rm=T)/7,
               sodium_mg_reeffish_hh=sum(sodium_tmg,na.rm=T)/7,magnesium_mg_reeffish_hh=sum(magnesium_tmg,na.rm=T)/7,potassium_mg_reeffish_hh=sum(potassium_tmg,na.rm=T)/7,
               calcium_mg_reeffish_hh=sum(calcium_tmg,na.rm=T)/7,iron_mg_reeffish_hh=sum(iron_tmg,na.rm=T)/7,zinc_mg_reeffish_hh=sum(zinc_tmg,na.rm=T)/7,
               retinol_mug_reeffish_hh=sum(retinol_tmug,na.rm=T)/7,betacaroten_mug_reeffish_hh=sum(betacaroten_tmug,na.rm=T)/7,VitA_RAE_mug_reeffish_hh=sum(VitA_RAE_tmug,na.rm=T)/7,
               VitA_RE_mug_reeffish_hh=sum(VitA_RE_tmug,na.rm=T)/7,thiamin_mg_reeffish_hh=sum(thiamin_tmg,na.rm=T)/7,riboflavin_mg_reeffish_hh=sum(riboflavin_tmg,na.rm=T)/7,
               niacin_mg_reeffish_hh=sum(niacin_tmg,na.rm=T)/7,Vit_B12_mug_reeffish_hh=sum(Vit_B12_tmug,na.rm=T)/7,Vit_C_mg_reeffish_hh=sum(Vit_C_tmg,na.rm=T)/7,
               VitE_mug_reeffish_hh=sum(VitE_tmug,na.rm=T)/7,cholesterol_mg_reeffish_hh=sum(cholesterol_tmg,na.rm=T)/7,iron_non_haem_mg_reeffish_hh=sum(iron_non_haem_tmg,na.rm=T)/7,
               iron_haem_mg_reeffish_hh=sum(iron_haem_tmg,na.rm=T)/7)


hhfoodsummary<-merge(hhfoodsummary,reeffish,by="interview__key",all.x=T)
hhfoodsummary[is.na(hhfoodsummary)]<-0

#for each household: what proportion of each dietary nutrients comes from reeffish
#reeffish dependence
hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_reeffish=(kcal_reeffish_hh/kcal_seafood_hh)*100,
         perc_grams_hh_reeffish=(grams_reeffish_hh/grams_seafood_hh)*100,
         perc_pr_hh_reeffish=(pr_g_reeffish_hh/pr_g_seafood_hh)*100,
         perc_tfats_hh_reeffish=(tfats_g_reeffish_hh/tfats_g_seafood_hh)*100,
         perc_carbs_hh_reeffish=(carbs_g_reeffish_hh/carbs_g_seafood_hh)*100,
         perc_fibre_hh_reeffish=(fibre_g_reeffish_hh/fibre_g_seafood_hh)*100,
         perc_sodium_hh_reeffish=(sodium_mg_reeffish_hh/sodium_mg_seafood_hh)*100,
         perc_magnesium_hh_reeffish=(magnesium_mg_reeffish_hh/magnesium_mg_seafood_hh)*100,
         perc_potassium_hh_reeffish=(potassium_mg_reeffish_hh/potassium_mg_seafood_hh)*100,
         perc_calcium_hh_reeffish=(calcium_mg_reeffish_hh/calcium_mg_seafood_hh)*100,
         perc_iron_hh_reeffish=(iron_mg_reeffish_hh/iron_mg_seafood_hh)*100,
         perc_zinc_hh_reeffish=(zinc_mg_reeffish_hh/zinc_mg_seafood_hh)*100,
         perc_retinol_hh_reeffish=(retinol_mug_reeffish_hh/retinol_mug_seafood_hh)*100,
         perc_betacaroten_hh_reeffish=(betacaroten_mug_reeffish_hh/betacaroten_mug_seafood_hh)*100,
         perc_VitA_RAE_hh_reeffish=(VitA_RAE_mug_reeffish_hh/VitA_RAE_mug_seafood_hh)*100,
         perc_VitA_RE_hh_reeffish=(VitA_RE_mug_reeffish_hh/VitA_RE_mug_seafood_hh)*100,
         perc_thiamin_hh_reeffish=(thiamin_mg_reeffish_hh/thiamin_mg_seafood_hh)*100,
         perc_riboflavin_hh_reeffish=(riboflavin_mg_reeffish_hh/riboflavin_mg_seafood_hh)*100,
         perc_niacin_hh_reeffish=(niacin_mg_reeffish_hh/niacin_mg_seafood_hh)*100,
         perc_Vit_B12_hh_reeffish=(Vit_B12_mug_reeffish_hh/Vit_B12_mug_seafood_hh)*100,
         perc_Vit_C_hh_reeffish=(Vit_C_mg_reeffish_hh/Vit_C_mg_seafood_hh)*100,
         perc_VitE_hh_reeffish=(VitE_mug_reeffish_hh/VitE_mug_seafood_hh)*100,
         perc_cholesterol_hh_reeffish=(cholesterol_mg_reeffish_hh/cholesterol_mg_seafood_hh)*100,
         perc_iron_non_haem_hh_reeffish=(iron_non_haem_mg_reeffish_hh/iron_non_haem_mg_seafood_hh)*100,
         perc_iron_haem_hh_reeffish=(iron_haem_mg_reeffish_hh/iron_haem_mg_seafood_hh)*100)

hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_reeffish=(kcal_reeffish_hh/kcal_hh)*100,
         perc_grams_hh_reeffish=(grams_reeffish_hh/grams_hh)*100,
         perc_pr_hh_reeffish=(pr_g_reeffish_hh/pr_g_hh)*100,
         perc_tfats_hh_reeffish=(tfats_g_reeffish_hh/tfats_g_hh)*100,
         perc_carbs_hh_reeffish=(carbs_g_reeffish_hh/carbs_g_hh)*100,
         perc_fibre_hh_reeffish=(fibre_g_reeffish_hh/fibre_g_hh)*100,
         perc_sodium_hh_reeffish=(sodium_mg_reeffish_hh/sodium_mg_hh)*100,
         perc_magnesium_hh_reeffish=(magnesium_mg_reeffish_hh/magnesium_mg_hh)*100,
         perc_potassium_hh_reeffish=(potassium_mg_reeffish_hh/potassium_mg_hh)*100,
         perc_calcium_hh_reeffish=(calcium_mg_reeffish_hh/calcium_mg_hh)*100,
         perc_iron_hh_reeffish=(iron_mg_reeffish_hh/iron_mg_hh)*100,
         perc_zinc_hh_reeffish=(zinc_mg_reeffish_hh/zinc_mg_hh)*100,
         perc_retinol_hh_reeffish=(retinol_mug_reeffish_hh/retinol_mug_hh)*100,
         perc_betacaroten_hh_reeffish=(betacaroten_mug_reeffish_hh/betacaroten_mug_hh)*100,
         perc_VitA_RAE_hh_reeffish=(VitA_RAE_mug_reeffish_hh/VitA_RAE_mug_hh)*100,
         perc_VitA_RE_hh_reeffish=(VitA_RE_mug_reeffish_hh/VitA_RE_mug_hh)*100,
         perc_thiamin_hh_reeffish=(thiamin_mg_reeffish_hh/thiamin_mg_hh)*100,
         perc_riboflavin_hh_reeffish=(riboflavin_mg_reeffish_hh/riboflavin_mg_hh)*100,
         perc_niacin_hh_reeffish=(niacin_mg_reeffish_hh/niacin_mg_hh)*100,
         perc_Vit_B12_hh_reeffish=(Vit_B12_mug_reeffish_hh/Vit_B12_mug_hh)*100,
         perc_Vit_C_hh_reeffish=(Vit_C_mg_reeffish_hh/Vit_C_mg_hh)*100,
         perc_VitE_hh_reeffish=(VitE_mug_reeffish_hh/VitE_mug_hh)*100,
         perc_cholesterol_hh_reeffish=(cholesterol_mg_reeffish_hh/cholesterol_mg_hh)*100,
         perc_iron_non_haem_hh_reeffish=(iron_non_haem_mg_reeffish_hh/iron_non_haem_mg_hh)*100,
         perc_iron_haem_hh_reeffish=(iron_haem_mg_reeffish_hh/iron_haem_mg_hh)*100)
ggplot(hhfoodsummary,aes(x=perc_kcal_hh_reeffish,y=island))+geom_density_ridges(quantile_lines = TRUE, quantiles = 2,scale=0.99,rel_min_height = 0.01)+xlim(c(0,100))+
  geom_vline(xintercept = median(hhfoodsummary$perc_kcal_hh_reeffish,na.rm=T),lty=2)+ xlab("% of seafood hhld Kcal from reef fish")+ylab("")+
  ggtitle(paste("Median=",round(median(hhfoodsummary$perc_kcal_hh_reeffish,na.rm=T)),"%"))+  theme(panel.background = element_rect(fill="white",colour="black"))
ggplot(hhfoodsummary,aes(x=perc_kcal_hh_fishnoreef,y=island))+geom_density_ridges(quantile_lines = TRUE, quantiles = 2,scale=0.99,rel_min_height = 0.01)+xlim(c(0,100))+
  geom_vline(xintercept = median(hhfoodsummary$perc_kcal_hh_fishnoreef,na.rm=T),lty=2)+ xlab("% of seafood hhld Kcal from non reef fish")+ylab("")+
  ggtitle(paste("Median=",round(median(hhfoodsummary$perc_kcal_hh_fishnoreef,na.rm=T)),"%"))+  theme(panel.background = element_rect(fill="white",colour="black"))


#percents overall
perc_contribution_reeffish<-hhfoodsummary %>%
  select(perc_kcal_hh_reeffish:perc_iron_haem_hh_reeffish) %>%select(-perc_VitA_RE_hh_reeffish)
colnames(perc_contribution_reeffish)<-c("Kcal","Grams","Protein","Total fats","Carbohydrates","Fibre","Sodium","Magnesium","Potassium","Calcium","Total iron","Zinc","Retinol","Betacaroten","Vitamin A (RAE)","Thiamin","Rivoflavin","Niacin","Vitamin B12","Vitamin C","Vitamin E","Cholesterol","Non-haem iron", "Haem iron")
perc_contribution_reeffish2<-melt(perc_contribution_reeffish)
perc_contribution_reeffish2<- within(perc_contribution_reeffish2, 
                                    variable<- factor(variable, 
                                                      levels=levels(fac2)))


#invertebrates
inverts<-foodrecall[foodrecall$coicop_class=="Fish and sea food"& (foodrecall$Food_description_HIES=="Crab, coconut"|
                                                                     foodrecall$Food_description_HIES=="Crab, not further specified"|foodrecall$Food_description_HIES=="Crayfish / lobster, not further specified"|                        
                                                                   foodrecall$Food_description_HIES=="Mussels"|foodrecall$Food_description_HIES=="Octopus"|                                                          
                                                                   foodrecall$Food_description_HIES=="Scallop"|foodrecall$Food_description_HIES=="Squid, not further specified"|                                   
                                                                   foodrecall$Food_description_HIES=="Trochus"|foodrecall$Food_description_HIES=="Sea-hare, not further specified"|
                                                                   foodrecall$Food_description_HIES=="Fish, not further specified"& foodrecall$description=="Sea worm"),]
summary(as.factor(inverts$description))

inverts<-ddply(inverts,.(interview__key),summarize,ginverts_pc=sum(gram_ep)/7/hhsize[1],
               kcal_inverts_hh=sum(kcal_JZM,na.rm=T)/7,
               grams_inverts_hh=sum(gram_ep,na.rm=T)/7,
               pr_g_inverts_hh=sum(protein_tg,na.rm=T)/7,tfats_g_inverts_hh=sum(total_fat_tg,na.rm=T)/7,carbs_g_inverts_hh=sum(carbs_tg,na.rm=T)/7,
               fibre_g_inverts_hh=sum(tdfibre_tg,na.rm=T)/7,alcohol_g_inverts_hh=sum(alcohol_tg,na.rm=T)/7,ash_g_inverts_hh=sum(ash_tg,na.rm=T)/7,
               sodium_mg_inverts_hh=sum(sodium_tmg,na.rm=T)/7,magnesium_mg_inverts_hh=sum(magnesium_tmg,na.rm=T)/7,potassium_mg_inverts_hh=sum(potassium_tmg,na.rm=T)/7,
               calcium_mg_inverts_hh=sum(calcium_tmg,na.rm=T)/7,iron_mg_inverts_hh=sum(iron_tmg,na.rm=T)/7,zinc_mg_inverts_hh=sum(zinc_tmg,na.rm=T)/7,
               retinol_mug_inverts_hh=sum(retinol_tmug,na.rm=T)/7,betacaroten_mug_inverts_hh=sum(betacaroten_tmug,na.rm=T)/7,VitA_RAE_mug_inverts_hh=sum(VitA_RAE_tmug,na.rm=T)/7,
               VitA_RE_mug_inverts_hh=sum(VitA_RE_tmug,na.rm=T)/7,thiamin_mg_inverts_hh=sum(thiamin_tmg,na.rm=T)/7,riboflavin_mg_inverts_hh=sum(riboflavin_tmg,na.rm=T)/7,
               niacin_mg_inverts_hh=sum(niacin_tmg,na.rm=T)/7,Vit_B12_mug_inverts_hh=sum(Vit_B12_tmug,na.rm=T)/7,Vit_C_mg_inverts_hh=sum(Vit_C_tmg,na.rm=T)/7,
               VitE_mug_inverts_hh=sum(VitE_tmug,na.rm=T)/7,cholesterol_mg_inverts_hh=sum(cholesterol_tmg,na.rm=T)/7,iron_non_haem_mg_inverts_hh=sum(iron_non_haem_tmg,na.rm=T)/7,
               iron_haem_mg_inverts_hh=sum(iron_haem_tmg,na.rm=T)/7)


hhfoodsummary<-merge(hhfoodsummary,inverts,by="interview__key",all.x=T)
hhfoodsummary[is.na(hhfoodsummary)]<-0

#for each household: what proportion of each dietary nutrients comes from inverts
#inverts dependence
hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_inverts=(kcal_inverts_hh/kcal_seafood_hh)*100,
         perc_grams_hh_inverts=(grams_inverts_hh/grams_seafood_hh)*100,
         perc_pr_hh_inverts=(pr_g_inverts_hh/pr_g_seafood_hh)*100,
         perc_tfats_hh_inverts=(tfats_g_inverts_hh/tfats_g_seafood_hh)*100,
         perc_carbs_hh_inverts=(carbs_g_inverts_hh/carbs_g_seafood_hh)*100,
         perc_fibre_hh_inverts=(fibre_g_inverts_hh/fibre_g_seafood_hh)*100,
         perc_sodium_hh_inverts=(sodium_mg_inverts_hh/sodium_mg_seafood_hh)*100,
         perc_magnesium_hh_inverts=(magnesium_mg_inverts_hh/magnesium_mg_seafood_hh)*100,
         perc_potassium_hh_inverts=(potassium_mg_inverts_hh/potassium_mg_seafood_hh)*100,
         perc_calcium_hh_inverts=(calcium_mg_inverts_hh/calcium_mg_seafood_hh)*100,
         perc_iron_hh_inverts=(iron_mg_inverts_hh/iron_mg_seafood_hh)*100,
         perc_zinc_hh_inverts=(zinc_mg_inverts_hh/zinc_mg_seafood_hh)*100,
         perc_retinol_hh_inverts=(retinol_mug_inverts_hh/retinol_mug_seafood_hh)*100,
         perc_betacaroten_hh_inverts=(betacaroten_mug_inverts_hh/betacaroten_mug_seafood_hh)*100,
         perc_VitA_RAE_hh_inverts=(VitA_RAE_mug_inverts_hh/VitA_RAE_mug_seafood_hh)*100,
         perc_VitA_RE_hh_inverts=(VitA_RE_mug_inverts_hh/VitA_RE_mug_seafood_hh)*100,
         perc_thiamin_hh_inverts=(thiamin_mg_inverts_hh/thiamin_mg_seafood_hh)*100,
         perc_riboflavin_hh_inverts=(riboflavin_mg_inverts_hh/riboflavin_mg_seafood_hh)*100,
         perc_niacin_hh_inverts=(niacin_mg_inverts_hh/niacin_mg_seafood_hh)*100,
         perc_Vit_B12_hh_inverts=(Vit_B12_mug_inverts_hh/Vit_B12_mug_seafood_hh)*100,
         perc_Vit_C_hh_inverts=(Vit_C_mg_inverts_hh/Vit_C_mg_seafood_hh)*100,
         perc_VitE_hh_inverts=(VitE_mug_inverts_hh/VitE_mug_seafood_hh)*100,
         perc_cholesterol_hh_inverts=(cholesterol_mg_inverts_hh/cholesterol_mg_seafood_hh)*100,
         perc_iron_non_haem_hh_inverts=(iron_non_haem_mg_inverts_hh/iron_non_haem_mg_seafood_hh)*100,
         perc_iron_haem_hh_inverts=(iron_haem_mg_inverts_hh/iron_haem_mg_seafood_hh)*100)

hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_inverts=(kcal_inverts_hh/kcal_hh)*100,
         perc_grams_hh_inverts=(grams_inverts_hh/grams_hh)*100,
         perc_pr_hh_inverts=(pr_g_inverts_hh/pr_g_hh)*100,
         perc_tfats_hh_inverts=(tfats_g_inverts_hh/tfats_g_hh)*100,
         perc_carbs_hh_inverts=(carbs_g_inverts_hh/carbs_g_hh)*100,
         perc_fibre_hh_inverts=(fibre_g_inverts_hh/fibre_g_hh)*100,
         perc_sodium_hh_inverts=(sodium_mg_inverts_hh/sodium_mg_hh)*100,
         perc_magnesium_hh_inverts=(magnesium_mg_inverts_hh/magnesium_mg_hh)*100,
         perc_potassium_hh_inverts=(potassium_mg_inverts_hh/potassium_mg_hh)*100,
         perc_calcium_hh_inverts=(calcium_mg_inverts_hh/calcium_mg_hh)*100,
         perc_iron_hh_inverts=(iron_mg_inverts_hh/iron_mg_hh)*100,
         perc_zinc_hh_inverts=(zinc_mg_inverts_hh/zinc_mg_hh)*100,
         perc_retinol_hh_inverts=(retinol_mug_inverts_hh/retinol_mug_hh)*100,
         perc_betacaroten_hh_inverts=(betacaroten_mug_inverts_hh/betacaroten_mug_hh)*100,
         perc_VitA_RAE_hh_inverts=(VitA_RAE_mug_inverts_hh/VitA_RAE_mug_hh)*100,
         perc_VitA_RE_hh_inverts=(VitA_RE_mug_inverts_hh/VitA_RE_mug_hh)*100,
         perc_thiamin_hh_inverts=(thiamin_mg_inverts_hh/thiamin_mg_hh)*100,
         perc_riboflavin_hh_inverts=(riboflavin_mg_inverts_hh/riboflavin_mg_hh)*100,
         perc_niacin_hh_inverts=(niacin_mg_inverts_hh/niacin_mg_hh)*100,
         perc_Vit_B12_hh_inverts=(Vit_B12_mug_inverts_hh/Vit_B12_mug_hh)*100,
         perc_Vit_C_hh_inverts=(Vit_C_mg_inverts_hh/Vit_C_mg_hh)*100,
         perc_VitE_hh_inverts=(VitE_mug_inverts_hh/VitE_mug_hh)*100,
         perc_cholesterol_hh_inverts=(cholesterol_mg_inverts_hh/cholesterol_mg_hh)*100,
         perc_iron_non_haem_hh_inverts=(iron_non_haem_mg_inverts_hh/iron_non_haem_mg_hh)*100,
         perc_iron_haem_hh_inverts=(iron_haem_mg_inverts_hh/iron_haem_mg_hh)*100)

ggplot(hhfoodsummary,aes(x=perc_kcal_hh_inverts,y=island))+geom_density_ridges(quantile_lines = TRUE, quantiles = 2,scale=0.99,rel_min_height = 0.01)+xlim(c(0,100))+
  geom_vline(xintercept = median(hhfoodsummary$perc_kcal_hh_inverts,na.rm=T),lty=2)+ xlab("% of seafood hhld Kcal from non reef fish")+ylab("")+
  ggtitle(paste("Median=",round(median(hhfoodsummary$perc_kcal_hh_inverts,na.rm=T)),"%"))+  theme(panel.background = element_rect(fill="white",colour="black"))

#percents overall
perc_contribution_inverts<-hhfoodsummary %>%
  select(perc_kcal_hh_inverts:perc_iron_haem_hh_inverts) %>%select(-perc_VitA_RE_hh_inverts)
colnames(perc_contribution_inverts)<-c("Kcal","Grams","Protein","Total fats","Carbohydrates","Fibre","Sodium","Magnesium","Potassium","Calcium","Total iron","Zinc","Retinol","Betacaroten","Vitamin A (RAE)", "Thiamin","Rivoflavin","Niacin","Vitamin B12","Vitamin C","Vitamin E","Cholesterol","Non-haem iron", "Haem iron")
perc_contribution_inverts2<-melt(perc_contribution_inverts)
perc_contribution_inverts2<- within(perc_contribution_inverts2, 
                            variable<- factor(variable, 
                                              levels=levels(fac2)))


#sharks and rays
sharksrays<-foodrecall[foodrecall$coicop_class=="Fish and sea food"& (foodrecall$Food_description_HIES=="Shark"|foodrecall$Food_description_HIES=="Stingray"),]
sharksrays<-ddply(sharksrays,.(interview__key),summarize,gsharksrays_pc=sum(gram_ep)/7/hhsize[1],
               kcal_sharksrays_hh=sum(kcal_JZM,na.rm=T)/7,
               grams_sharksrays_hh=sum(gram_ep,na.rm=T)/7,
               pr_g_sharksrays_hh=sum(protein_tg,na.rm=T)/7,tfats_g_sharksrays_hh=sum(total_fat_tg,na.rm=T)/7,carbs_g_sharksrays_hh=sum(carbs_tg,na.rm=T)/7,
               fibre_g_sharksrays_hh=sum(tdfibre_tg,na.rm=T)/7,alcohol_g_sharksrays_hh=sum(alcohol_tg,na.rm=T)/7,ash_g_sharksrays_hh=sum(ash_tg,na.rm=T)/7,
               sodium_mg_sharksrays_hh=sum(sodium_tmg,na.rm=T)/7,magnesium_mg_sharksrays_hh=sum(magnesium_tmg,na.rm=T)/7,potassium_mg_sharksrays_hh=sum(potassium_tmg,na.rm=T)/7,
               calcium_mg_sharksrays_hh=sum(calcium_tmg,na.rm=T)/7,iron_mg_sharksrays_hh=sum(iron_tmg,na.rm=T)/7,zinc_mg_sharksrays_hh=sum(zinc_tmg,na.rm=T)/7,
               retinol_mug_sharksrays_hh=sum(retinol_tmug,na.rm=T)/7,betacaroten_mug_sharksrays_hh=sum(betacaroten_tmug,na.rm=T)/7,VitA_RAE_mug_sharksrays_hh=sum(VitA_RAE_tmug,na.rm=T)/7,
               VitA_RE_mug_sharksrays_hh=sum(VitA_RE_tmug,na.rm=T)/7,thiamin_mg_sharksrays_hh=sum(thiamin_tmg,na.rm=T)/7,riboflavin_mg_sharksrays_hh=sum(riboflavin_tmg,na.rm=T)/7,
               niacin_mg_sharksrays_hh=sum(niacin_tmg,na.rm=T)/7,Vit_B12_mug_sharksrays_hh=sum(Vit_B12_tmug,na.rm=T)/7,Vit_C_mg_sharksrays_hh=sum(Vit_C_tmg,na.rm=T)/7,
               VitE_mug_sharksrays_hh=sum(VitE_tmug,na.rm=T)/7,cholesterol_mg_sharksrays_hh=sum(cholesterol_tmg,na.rm=T)/7,iron_non_haem_mg_sharksrays_hh=sum(iron_non_haem_tmg,na.rm=T)/7,
               iron_haem_mg_sharksrays_hh=sum(iron_haem_tmg,na.rm=T)/7)


hhfoodsummary<-merge(hhfoodsummary,sharksrays,by="interview__key",all.x=T)
hhfoodsummary[is.na(hhfoodsummary)]<-0

#for each household: what proportion of each dietary nutrients comes from sharksrays
#sharksrays dependence
hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_sharksrays=(kcal_sharksrays_hh/kcal_seafood_hh)*100,
         perc_grams_hh_sharksrays=(grams_sharksrays_hh/grams_seafood_hh)*100,
         perc_pr_hh_sharksrays=(pr_g_sharksrays_hh/pr_g_seafood_hh)*100,
         perc_tfats_hh_sharksrays=(tfats_g_sharksrays_hh/tfats_g_seafood_hh)*100,
         perc_carbs_hh_sharksrays=(carbs_g_sharksrays_hh/carbs_g_seafood_hh)*100,
         perc_fibre_hh_sharksrays=(fibre_g_sharksrays_hh/fibre_g_seafood_hh)*100,
         perc_sodium_hh_sharksrays=(sodium_mg_sharksrays_hh/sodium_mg_seafood_hh)*100,
         perc_magnesium_hh_sharksrays=(magnesium_mg_sharksrays_hh/magnesium_mg_seafood_hh)*100,
         perc_potassium_hh_sharksrays=(potassium_mg_sharksrays_hh/potassium_mg_seafood_hh)*100,
         perc_calcium_hh_sharksrays=(calcium_mg_sharksrays_hh/calcium_mg_seafood_hh)*100,
         perc_iron_hh_sharksrays=(iron_mg_sharksrays_hh/iron_mg_seafood_hh)*100,
         perc_zinc_hh_sharksrays=(zinc_mg_sharksrays_hh/zinc_mg_seafood_hh)*100,
         perc_retinol_hh_sharksrays=(retinol_mug_sharksrays_hh/retinol_mug_seafood_hh)*100,
         perc_betacaroten_hh_sharksrays=(betacaroten_mug_sharksrays_hh/betacaroten_mug_seafood_hh)*100,
         perc_VitA_RAE_hh_sharksrays=(VitA_RAE_mug_sharksrays_hh/VitA_RAE_mug_seafood_hh)*100,
         perc_VitA_RE_hh_sharksrays=(VitA_RE_mug_sharksrays_hh/VitA_RE_mug_seafood_hh)*100,
         perc_thiamin_hh_sharksrays=(thiamin_mg_sharksrays_hh/thiamin_mg_seafood_hh)*100,
         perc_riboflavin_hh_sharksrays=(riboflavin_mg_sharksrays_hh/riboflavin_mg_seafood_hh)*100,
         perc_niacin_hh_sharksrays=(niacin_mg_sharksrays_hh/niacin_mg_seafood_hh)*100,
         perc_Vit_B12_hh_sharksrays=(Vit_B12_mug_sharksrays_hh/Vit_B12_mug_seafood_hh)*100,
         perc_Vit_C_hh_sharksrays=(Vit_C_mg_sharksrays_hh/Vit_C_mg_seafood_hh)*100,
         perc_VitE_hh_sharksrays=(VitE_mug_sharksrays_hh/VitE_mug_seafood_hh)*100,
         perc_cholesterol_hh_sharksrays=(cholesterol_mg_sharksrays_hh/cholesterol_mg_seafood_hh)*100,
         perc_iron_non_haem_hh_sharksrays=(iron_non_haem_mg_sharksrays_hh/iron_non_haem_mg_seafood_hh)*100,
         perc_iron_haem_hh_sharksrays=(iron_haem_mg_sharksrays_hh/iron_haem_mg_seafood_hh)*100)

hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_sharksrays=(kcal_sharksrays_hh/kcal_hh)*100,
         perc_grams_hh_sharksrays=(grams_sharksrays_hh/grams_hh)*100,
         perc_pr_hh_sharksrays=(pr_g_sharksrays_hh/pr_g_hh)*100,
         perc_tfats_hh_sharksrays=(tfats_g_sharksrays_hh/tfats_g_hh)*100,
         perc_carbs_hh_sharksrays=(carbs_g_sharksrays_hh/carbs_g_hh)*100,
         perc_fibre_hh_sharksrays=(fibre_g_sharksrays_hh/fibre_g_hh)*100,
         perc_sodium_hh_sharksrays=(sodium_mg_sharksrays_hh/sodium_mg_hh)*100,
         perc_magnesium_hh_sharksrays=(magnesium_mg_sharksrays_hh/magnesium_mg_hh)*100,
         perc_potassium_hh_sharksrays=(potassium_mg_sharksrays_hh/potassium_mg_hh)*100,
         perc_calcium_hh_sharksrays=(calcium_mg_sharksrays_hh/calcium_mg_hh)*100,
         perc_iron_hh_sharksrays=(iron_mg_sharksrays_hh/iron_mg_hh)*100,
         perc_zinc_hh_sharksrays=(zinc_mg_sharksrays_hh/zinc_mg_hh)*100,
         perc_retinol_hh_sharksrays=(retinol_mug_sharksrays_hh/retinol_mug_hh)*100,
         perc_betacaroten_hh_sharksrays=(betacaroten_mug_sharksrays_hh/betacaroten_mug_hh)*100,
         perc_VitA_RAE_hh_sharksrays=(VitA_RAE_mug_sharksrays_hh/VitA_RAE_mug_hh)*100,
         perc_VitA_RE_hh_sharksrays=(VitA_RE_mug_sharksrays_hh/VitA_RE_mug_hh)*100,
         perc_thiamin_hh_sharksrays=(thiamin_mg_sharksrays_hh/thiamin_mg_hh)*100,
         perc_riboflavin_hh_sharksrays=(riboflavin_mg_sharksrays_hh/riboflavin_mg_hh)*100,
         perc_niacin_hh_sharksrays=(niacin_mg_sharksrays_hh/niacin_mg_hh)*100,
         perc_Vit_B12_hh_sharksrays=(Vit_B12_mug_sharksrays_hh/Vit_B12_mug_hh)*100,
         perc_Vit_C_hh_sharksrays=(Vit_C_mg_sharksrays_hh/Vit_C_mg_hh)*100,
         perc_VitE_hh_sharksrays=(VitE_mug_sharksrays_hh/VitE_mug_hh)*100,
         perc_cholesterol_hh_sharksrays=(cholesterol_mg_sharksrays_hh/cholesterol_mg_hh)*100,
         perc_iron_non_haem_hh_sharksrays=(iron_non_haem_mg_sharksrays_hh/iron_non_haem_mg_hh)*100,
         perc_iron_haem_hh_sharksrays=(iron_haem_mg_sharksrays_hh/iron_haem_mg_hh)*100)
#percents overall
perc_contribution_sharksrays<-hhfoodsummary %>%
  select(perc_kcal_hh_sharksrays:perc_iron_haem_hh_sharksrays)%>%select(-perc_VitA_RE_hh_sharksrays)
colnames(perc_contribution_sharksrays)<-c("Kcal","Grams","Protein","Total fats","Carbohydrates","Fibre","Sodium","Magnesium","Potassium","Calcium","Total iron","Zinc","Retinol","Betacaroten","Vitamin A (RAE)","Thiamin","Rivoflavin","Niacin","Vitamin B12","Vitamin C","Vitamin E","Cholesterol","Non-haem iron", "Haem iron")
perc_contribution_sharksrays2<-melt(perc_contribution_sharksrays)
perc_contribution_sharksrays2<- within(perc_contribution_sharksrays2, 
                                    variable<- factor(variable, 
                                                      levels=levels(fac2)))

#SEAWEED
seaweed<-foodrecall[foodrecall$coicop_class=="Fish and sea food"& (foodrecall$Food_description_HIES=="Seaweed"),]
summary(as.factor(seaweed$description))

seaweed<-ddply(seaweed,.(interview__key),summarize,gseaweed_pc=sum(gram_ep)/7/hhsize[1],
               kcal_seaweed_hh=sum(kcal_JZM,na.rm=T)/7,
               grams_seaweed_hh=sum(gram_ep,na.rm=T)/7,
               pr_g_seaweed_hh=sum(protein_tg,na.rm=T)/7,tfats_g_seaweed_hh=sum(total_fat_tg,na.rm=T)/7,carbs_g_seaweed_hh=sum(carbs_tg,na.rm=T)/7,
               fibre_g_seaweed_hh=sum(tdfibre_tg,na.rm=T)/7,alcohol_g_seaweed_hh=sum(alcohol_tg,na.rm=T)/7,ash_g_seaweed_hh=sum(ash_tg,na.rm=T)/7,
               sodium_mg_seaweed_hh=sum(sodium_tmg,na.rm=T)/7,magnesium_mg_seaweed_hh=sum(magnesium_tmg,na.rm=T)/7,potassium_mg_seaweed_hh=sum(potassium_tmg,na.rm=T)/7,
               calcium_mg_seaweed_hh=sum(calcium_tmg,na.rm=T)/7,iron_mg_seaweed_hh=sum(iron_tmg,na.rm=T)/7,zinc_mg_seaweed_hh=sum(zinc_tmg,na.rm=T)/7,
               retinol_mug_seaweed_hh=sum(retinol_tmug,na.rm=T)/7,betacaroten_mug_seaweed_hh=sum(betacaroten_tmug,na.rm=T)/7,VitA_RAE_mug_seaweed_hh=sum(VitA_RAE_tmug,na.rm=T)/7,
               VitA_RE_mug_seaweed_hh=sum(VitA_RE_tmug,na.rm=T)/7,thiamin_mg_seaweed_hh=sum(thiamin_tmg,na.rm=T)/7,riboflavin_mg_seaweed_hh=sum(riboflavin_tmg,na.rm=T)/7,
               niacin_mg_seaweed_hh=sum(niacin_tmg,na.rm=T)/7,Vit_B12_mug_seaweed_hh=sum(Vit_B12_tmug,na.rm=T)/7,Vit_C_mg_seaweed_hh=sum(Vit_C_tmg,na.rm=T)/7,
               VitE_mug_seaweed_hh=sum(VitE_tmug,na.rm=T)/7,cholesterol_mg_seaweed_hh=sum(cholesterol_tmg,na.rm=T)/7,iron_non_haem_mg_seaweed_hh=sum(iron_non_haem_tmg,na.rm=T)/7,
               iron_haem_mg_seaweed_hh=sum(iron_haem_tmg,na.rm=T)/7)


hhfoodsummary<-merge(hhfoodsummary,seaweed,by="interview__key",all.x=T)
hhfoodsummary[is.na(hhfoodsummary)]<-0

#for each household: what proportion of each dietary nutrients comes from seaweed
#seaweed dependence
hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_seaweed=(kcal_seaweed_hh/kcal_seafood_hh)*100,
         perc_grams_hh_seaweed=(grams_seaweed_hh/grams_seafood_hh)*100,
         perc_pr_hh_seaweed=(pr_g_seaweed_hh/pr_g_seafood_hh)*100,
         perc_tfats_hh_seaweed=(tfats_g_seaweed_hh/tfats_g_seafood_hh)*100,
         perc_carbs_hh_seaweed=(carbs_g_seaweed_hh/carbs_g_seafood_hh)*100,
         perc_fibre_hh_seaweed=(fibre_g_seaweed_hh/fibre_g_seafood_hh)*100,
         perc_sodium_hh_seaweed=(sodium_mg_seaweed_hh/sodium_mg_seafood_hh)*100,
         perc_magnesium_hh_seaweed=(magnesium_mg_seaweed_hh/magnesium_mg_seafood_hh)*100,
         perc_potassium_hh_seaweed=(potassium_mg_seaweed_hh/potassium_mg_seafood_hh)*100,
         perc_calcium_hh_seaweed=(calcium_mg_seaweed_hh/calcium_mg_seafood_hh)*100,
         perc_iron_hh_seaweed=(iron_mg_seaweed_hh/iron_mg_seafood_hh)*100,
         perc_zinc_hh_seaweed=(zinc_mg_seaweed_hh/zinc_mg_seafood_hh)*100,
         perc_retinol_hh_seaweed=(retinol_mug_seaweed_hh/retinol_mug_seafood_hh)*100,
         perc_betacaroten_hh_seaweed=(betacaroten_mug_seaweed_hh/betacaroten_mug_seafood_hh)*100,
         perc_VitA_RAE_hh_seaweed=(VitA_RAE_mug_seaweed_hh/VitA_RAE_mug_seafood_hh)*100,
         perc_VitA_RE_hh_seaweed=(VitA_RE_mug_seaweed_hh/VitA_RE_mug_seafood_hh)*100,
         perc_thiamin_hh_seaweed=(thiamin_mg_seaweed_hh/thiamin_mg_seafood_hh)*100,
         perc_riboflavin_hh_seaweed=(riboflavin_mg_seaweed_hh/riboflavin_mg_seafood_hh)*100,
         perc_niacin_hh_seaweed=(niacin_mg_seaweed_hh/niacin_mg_seafood_hh)*100,
         perc_Vit_B12_hh_seaweed=(Vit_B12_mug_seaweed_hh/Vit_B12_mug_seafood_hh)*100,
         perc_Vit_C_hh_seaweed=(Vit_C_mg_seaweed_hh/Vit_C_mg_seafood_hh)*100,
         perc_VitE_hh_seaweed=(VitE_mug_seaweed_hh/VitE_mug_seafood_hh)*100,
         perc_cholesterol_hh_seaweed=(cholesterol_mg_seaweed_hh/cholesterol_mg_seafood_hh)*100,
         perc_iron_non_haem_hh_seaweed=(iron_non_haem_mg_seaweed_hh/iron_non_haem_mg_seafood_hh)*100,
         perc_iron_haem_hh_seaweed=(iron_haem_mg_seaweed_hh/iron_haem_mg_seafood_hh)*100)

#try as % of total foods instead of seafood
hhfoodsummary<- hhfoodsummary %>%
  mutate(perc_kcal_hh_seaweed=(kcal_seaweed_hh/kcal_hh)*100,
         perc_grams_hh_seaweed=(grams_seaweed_hh/grams_hh)*100,
         perc_pr_hh_seaweed=(pr_g_seaweed_hh/pr_g_hh)*100,
         perc_tfats_hh_seaweed=(tfats_g_seaweed_hh/tfats_g_hh)*100,
         perc_carbs_hh_seaweed=(carbs_g_seaweed_hh/carbs_g_hh)*100,
         perc_fibre_hh_seaweed=(fibre_g_seaweed_hh/fibre_g_hh)*100,
         perc_sodium_hh_seaweed=(sodium_mg_seaweed_hh/sodium_mg_hh)*100,
         perc_magnesium_hh_seaweed=(magnesium_mg_seaweed_hh/magnesium_mg_hh)*100,
         perc_potassium_hh_seaweed=(potassium_mg_seaweed_hh/potassium_mg_hh)*100,
         perc_calcium_hh_seaweed=(calcium_mg_seaweed_hh/calcium_mg_hh)*100,
         perc_iron_hh_seaweed=(iron_mg_seaweed_hh/iron_mg_hh)*100,
         perc_zinc_hh_seaweed=(zinc_mg_seaweed_hh/zinc_mg_hh)*100,
         perc_retinol_hh_seaweed=(retinol_mug_seaweed_hh/retinol_mug_hh)*100,
         perc_betacaroten_hh_seaweed=(betacaroten_mug_seaweed_hh/betacaroten_mug_hh)*100,
         perc_VitA_RAE_hh_seaweed=(VitA_RAE_mug_seaweed_hh/VitA_RAE_mug_hh)*100,
         perc_VitA_RE_hh_seaweed=(VitA_RE_mug_seaweed_hh/VitA_RE_mug_hh)*100,
         perc_thiamin_hh_seaweed=(thiamin_mg_seaweed_hh/thiamin_mg_hh)*100,
         perc_riboflavin_hh_seaweed=(riboflavin_mg_seaweed_hh/riboflavin_mg_hh)*100,
         perc_niacin_hh_seaweed=(niacin_mg_seaweed_hh/niacin_mg_hh)*100,
         perc_Vit_B12_hh_seaweed=(Vit_B12_mug_seaweed_hh/Vit_B12_mug_hh)*100,
         perc_Vit_C_hh_seaweed=(Vit_C_mg_seaweed_hh/Vit_C_mg_hh)*100,
         perc_VitE_hh_seaweed=(VitE_mug_seaweed_hh/VitE_mug_hh)*100,
         perc_cholesterol_hh_seaweed=(cholesterol_mg_seaweed_hh/cholesterol_mg_hh)*100,
         perc_iron_non_haem_hh_seaweed=(iron_non_haem_mg_seaweed_hh/iron_non_haem_mg_hh)*100,
         perc_iron_haem_hh_seaweed=(iron_haem_mg_seaweed_hh/iron_haem_mg_hh)*100)



#percents overall
perc_contribution_seaweed<-hhfoodsummary %>%
  select(perc_kcal_hh_seaweed:perc_iron_haem_hh_seaweed)%>%select(-perc_VitA_RE_hh_seaweed)
colnames(perc_contribution_seaweed)<-c("Kcal","Grams","Protein","Total fats","Carbohydrates","Fibre","Sodium","Magnesium","Potassium","Calcium","Total iron","Zinc","Retinol","Betacaroten","Vitamin A (RAE)", "Thiamin","Rivoflavin","Niacin","Vitamin B12","Vitamin C","Vitamin E","Cholesterol","Non-haem iron", "Haem iron")
perc_contribution_seaweed2<-melt(perc_contribution_seaweed)
perc_contribution_seaweed2<- within(perc_contribution_seaweed2, 
                                    variable<- factor(variable, 
                                                      levels=levels(fac2)))


#calcultae median contributions for each seafood group
seafoodgroup_contribution<-rbind(perc_contribution_reeffish2%>%group_by(variable)%>%summarise_all(mean,na.rm=T) %>%mutate(seafood_group=rep("Reef fish",ncol(perc_contribution_reeffish) )),
      perc_contribution_fishnoreef2%>%group_by(variable)%>%summarise_all(mean,na.rm=T) %>%mutate(seafood_group=rep("Pelagic & other fish",ncol(perc_contribution_fishnoreef) )),
      perc_contribution_driedfish2%>%group_by(variable)%>%summarise_all(mean,na.rm=T) %>%mutate(seafood_group=rep("Dried & salted fish",ncol(perc_contribution_driedfish) )),
      perc_contribution_inverts2%>%group_by(variable)%>%summarise_all(mean,na.rm=T) %>%mutate(seafood_group=rep("Invertebrates",ncol(perc_contribution_inverts) )),
      perc_contribution_tinnedsf2%>%group_by(variable)%>%summarise_all(mean,na.rm=T) %>%mutate(seafood_group=rep("Tinned fish",ncol(perc_contribution_tinnedsf) )),
      perc_contribution_sharksrays2%>%group_by(variable)%>%summarise_all(mean,na.rm=T) %>%mutate(seafood_group=rep("Sharks & rays",ncol(perc_contribution_sharksrays) )),
      perc_contribution_seaweed2%>%group_by(variable)%>%summarise_all(mean,na.rm=T) %>%mutate(seafood_group=rep("Seaweed",ncol(perc_contribution_seaweed) )))
seafoodgroup_contribution[is.na(seafoodgroup_contribution)]<-0
#ranking of food groups
seafoodgroup_contribution_rank<-seafoodgroup_contribution%>% group_by(variable) %>%mutate(ranking=order(order(value,variable,decreasing=T)))%>% filter(variable!="cholesterol")
seafoodgroup_contribution_rank<-droplevels(seafoodgroup_contribution_rank)
#raster plot
seafoodgrouprank_sum<-as.data.frame(seafoodgroup_contribution_rank %>% group_by(seafood_group) %>%dplyr::summarise(ranking_sum=sum(ranking)) %>%arrange(ranking_sum))
seafoodgrouprank_sum$seafood_group <- reorder(seafoodgrouprank_sum$seafood_group, seafoodgrouprank_sum$ranking_sum)
seafoodgrouprank_variables<-as.data.frame(seafoodgroup_contribution_rank %>% filter(seafood_group %in% "Reef fish") %>%arrange(desc(value)))
seafoodgrouprank_variables$variable <- reorder(as.factor(seafoodgrouprank_variables$variable), seafoodgrouprank_variables$value)
seafoodgrouprank_fig<-ggplot(seafoodgroup_contribution_rank, aes(x = variable, y = seafood_group, fill = as.factor(ranking)))+
  geom_tile(colour = "white", size = 0.1, height = 1) + 
  scale_fill_viridis_d("Main source
  ranking",option = "B",direction = -1) +
  labs(x = "", y = "", title = "") +
  theme(#axis.text.x = element_text(angle = 90,hjust=1,vjust=0),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    #legend.position="left",
        plot.margin = ggplot2::margin(t = 0.18, r = 0, b = 0.89, l = 0, "cm"),
        panel.grid = element_blank())+ylim(levels(seafoodgrouprank_sum$seafood_group))+
  scale_x_discrete(limits=c("Non-haem iron","Carbohydrates","Fibre","Vitamin C","Betacaroten","Kcal","Sodium","Thiamin","Zinc","Total fats","Calcium","Total iron","Potassium","Magnesium","Rivoflavin","Vitamin E","Protein","Vitamin A (RAE)","Niacin","Retinol","Cholesterol","Haem iron","Vitamin B12","Grams"))+coord_flip()

seafoodgroupcont_fig<-ggplot(seafoodgroup_contribution_rank, aes(x = variable, y = seafood_group, fill = value))+
  geom_tile(colour = "white", size = 0.1, height = 1) +
  scale_fill_viridis_c("% contribution",option = "G") +
  labs(x = "", y = "", title = "") +
  theme(#axis.text.x = element_text(angle = 90,hjust=1,vjust=0),
    axis.text.x = element_blank(),
    legend.position="top",
        #panel.background = element_rect(fill = 'black'),
        plot.margin = ggplot2::margin(t = 0.18, r = 0, b = 0.89, l = 0, "cm"),
        panel.grid = element_blank(), axis.text.y=element_blank())+ylim(levels(seafoodgrouprank_sum$seafood_group))+
  scale_x_discrete(limits=c("Non-haem iron","Carbohydrates","Fibre","Vitamin C","Betacaroten","Kcal","Sodium","Thiamin","Zinc","Total fats","Calcium","Total iron","Potassium","Magnesium","Rivoflavin","Vitamin E","Protein","Vitamin A (RAE)","Niacin","Retinol","Cholesterol","Haem iron","Vitamin B12","Grams"))+coord_flip()


ggarrange(allfoodcont_fig,seafoodgroupcont_fig,widths=c(1.5,1))
windows()
ggarrange(rank_allfoods_fig,seafoodgrouprank_fig,widths=c(1.3,1))


#observed variability figure
windows()
a<-ggplot(NULL)+ geom_boxplot(data=perc_contribution_reeffish2%>%filter(variable!="Cholesterol"),aes(x=value, y=variable))+
  geom_jitter(data=perc_contribution_reeffish2%>%filter(variable!="Cholesterol"),aes(x=value, y=variable, fill=value),pch=21,alpha=0.04)+ guides(fill=F)+
  scale_fill_viridis_c("",option="B")+xlab("")+ylab("")+theme(plot.title = element_text(size = 9),axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+ggtitle ("Reef fish")+xlim(c(0,100))
b<-ggplot(NULL)+  geom_boxplot(data=perc_contribution_fishnoreef2%>%filter(variable!="Cholesterol"),aes(x=value, y=variable))+
  geom_jitter(data=perc_contribution_fishnoreef2%>%filter(variable!="Cholesterol"), aes(x=value, y=variable, fill=value),pch=21,alpha=0.04)+guides(fill=F)+
  scale_fill_viridis_c("",option="B")+xlab("")+ylab("")+theme(plot.title = element_text(size = 9),axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+ggtitle ("Pelagic/other fish")+xlim(c(0,100))
c<-ggplot(NULL)+  geom_boxplot(data=perc_contribution_tinnedsf2%>%filter(variable!="Cholesterol"),aes(x=value, y=variable))+
  geom_jitter(data=perc_contribution_tinnedsf2%>%filter(variable!="Cholesterol"), aes(x=value, y=variable, fill=value),pch=21,alpha=0.04)+guides(fill=F)+
  scale_fill_viridis_c("",option="B")+xlab("")+ylab("")+theme(plot.title = element_text(size = 9),axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+ggtitle ("Tinned fish")+xlim(c(0,100))
d<-ggplot(NULL)+  geom_boxplot(data=perc_contribution_inverts2%>%filter(variable!="Cholesterol"),aes(x=value, y=variable))+
  geom_jitter(data=perc_contribution_inverts2%>%filter(variable!="Cholesterol"), aes(x=value, y=variable, fill=value),pch=21,alpha=0.04)+guides(fill=F)+
  scale_fill_viridis_c("",option="B")+xlab("")+ylab("")+theme(plot.title = element_text(size = 9),axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+ggtitle ("Invertebrates")+xlim(c(0,100))
e<-ggplot(NULL)+  geom_boxplot(data=perc_contribution_sharksrays2%>%filter(variable!="Cholesterol"),aes(x=value, y=variable))+
  geom_jitter(data=perc_contribution_sharksrays2%>%filter(variable!="Cholesterol"), aes(x=value, y=variable, fill=value),pch=21,alpha=0.04)+guides(fill=F)+
  scale_fill_viridis_c("",option="B")+xlab("")+ylab("")+theme(plot.title = element_text(size = 9),axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+ggtitle ("Sharks/rays")+xlim(c(0,100))
f<-ggplot(NULL)+  geom_boxplot(data=perc_contribution_seaweed2%>%filter(variable!="Cholesterol"),aes(x=value, y=variable))+
  geom_jitter(data=perc_contribution_seaweed2%>%filter(variable!="Cholesterol"), aes(x=value, y=variable, fill=value),pch=21,alpha=0.04)+guides(fill=F)+
  scale_fill_viridis_c("",option="B")+xlab("")+ylab("")+theme(plot.title = element_text(size = 9),axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+ggtitle ("Seaweed")+xlim(c(0,100))
g<-ggplot(NULL)+  geom_boxplot(data=perc_contribution_driedfish2%>%filter(variable!="Cholesterol"),aes(x=value, y=variable))+
  geom_jitter(data=perc_contribution_driedfish2%>%filter(variable!="Cholesterol"), aes(x=value, y=variable, fill=value),pch=21,alpha=0.04)+guides(fill=F)+
  scale_fill_viridis_c("",option="B")+xlab("")+ylab("")+theme(plot.title = element_text(size = 9),axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+ggtitle ("Dried/salted fish")+xlim(c(0,100))


seafoodgroups_fig<-annotate_figure(ggarrange(a,b,c,d,g,e,f,nrow=1,ncol=7))
windows()
annotate_figure(ggarrange(allseafoodcont_fig2,seafoodgroups_fig, widths=c(0.5,1.5),labels=c("a","b"),nrow=1,ncol=2), bottom=" % contribution to household intake")

###############################################################################
#contribution of seafood to nutritional adequacy________________________________

#hhid and person specific data from hies data
HIES_HHid<-read.csv("2021-08-30_hies_tidy_household-level.csv")
HIES_personid<-read.csv("2021-08-30_hies_tidy_individual-level.csv")
HIES_personid<-merge(HIES_personid,HIES_HHid[,c("interview__key","hhld_id")],by="interview__key",all.x=T)
HIES_personid$hhld_id_hm_basic__id<-paste(HIES_personid$hhld_id,HIES_personid$hm_basic__id,sep="::" )

#upload rda and ai)
rda<-read.csv("rda_ai.csv")
#VB12_ugha ;Protein_gha;Selenium_ugha;Niacin_mgha;VB6_mgha;Sodium_mgha;VD_ugha;Potassium_mgha;Magnesium_mgha;Calcium_mgha
#Copper_mgha;Zinc_mgha;Iron_mgha;Riboflavin_mgha;VA_ugha;VE_mgha;O3_gha;O6_gha;DHA_EPA_gha;ALA_gha;Iodine_ugha
#ALL HAVE SAME UNITS BUT COPPER (AFCDmg, earMUG)

#pass everything to nummeric
rda2<-rda[-1,] %>% mutate(Calcium=(as.numeric(gsub(",","",Calcium))),
                          Proteinb=(as.numeric(gsub(",","",Proteinb))),
                          Carbohydrate=(as.numeric(gsub(",","",Carbohydrate))),
                          Energy=(as.numeric(gsub(",","",Energy))),
                          Fat=(as.numeric(gsub(",","",Fat))),
                           Vitamin.A=(as.numeric(gsub(",","",Vitamin.A))),
                          Iron=(as.numeric(gsub(",","",Iron))),
                          Magnesium=(as.numeric(gsub(",","",Magnesium))),
                          Zinc=(as.numeric(gsub(",","",Zinc))),
                          Vitamin.B12=(as.numeric(gsub(",","",Vitamin.B12))),
                          Vitamin.C=(as.numeric(gsub(",","",Vitamin.C))),
                           Riboflavin=(as.numeric(gsub(",","",Riboflavin))),
                           Thiamin=(as.numeric(gsub(",","",Thiamin))),
                          Niacin=(as.numeric(gsub(",","",Niacin))),
                          Vitamin.E=(as.numeric(gsub(",","",Vitamin.E))),
                          Potassium=(as.numeric(gsub(",","",Potassium))),
                          Sodium=(as.numeric(gsub(",","",Sodium))),
                          Total.Fiber=(as.numeric(gsub(",","",Total.Fiber))),
                          Iodine=(as.numeric(gsub("[*]","",Iodine))),
                          Total.Watera=(as.numeric(gsub("[*]","",Total.Watera))))
#upper intake levels
ui<-read.csv("ui.csv")
ui<- ui%>% mutate_all(na_if,"")
ui2<-ui[-1,] %>% mutate(Calcium=(as.numeric(gsub(",","",Calcium))),
                          Proteinb=(as.numeric(gsub(",","",Proteinb))),
                          Carbohydrate=(as.numeric(gsub(",","",Carbohydrate))),
                          Energy=(as.numeric(gsub(",","",Energy))),
                          Fat=(as.numeric(gsub(",","",Fat))),
                          Vitamin.A=(as.numeric(gsub(",","",Vitamin.A))),
                          Iron=(as.numeric(gsub(",","",Iron))),
                          Magnesium=(as.numeric(gsub(",","",Magnesium))),
                          Zinc=(as.numeric(gsub(",","",Zinc))),
                          Vitamin.B12=(as.numeric(gsub(",","",Vitamin.B12))),
                          Vitamin.C=(as.numeric(gsub(",","",Vitamin.C))),
                          Riboflavin=(as.numeric(gsub(",","",Riboflavin))),
                          Thiamin=(as.numeric(gsub(",","",Thiamin))),
                          Niacin=(as.numeric(gsub(",","",Niacin))),
                          Vitamin.E=(as.numeric(gsub(",","",Vitamin.E))),
                          Potassium=(as.numeric(gsub(",","",Potassium))),
                          Sodium=(as.numeric(gsub(",","",Sodium))),
                          Total.Fiber=(as.numeric(gsub(",","",Total.Fiber))),
                          Iodine=(as.numeric(gsub("[*]","",Iodine))),
                          Total.Watera=(as.numeric(gsub("[*]","",Total.Watera))))

#note: vitamin A accroding to the PNCD is in mug, and rda is mg (however, it gives really strange results --units in PNCD are in mg not mug

#calculate kcal and nutritional requirements needed by household using person specific characteristics
#add a category identifier to rda
rda_cat <-rda2 
colnames(rda_cat)<-paste(colnames(rda_cat),"indrda",sep="_")
rda_cat<-rda_cat%>% mutate(rda_cat=as.character(seq(1,nrow(rda_cat))))

#add fat recommendations as 25 % of kcal recomendations (https://link.springer.com/article/10.1186/s12937-017-0271-4)
rda_cat$Fat_indrda_perccal<-25*rda_cat$Energy_indrda/100

#add category identifier to ui
ui_cat <-ui2 
colnames(ui_cat)<-paste(colnames(ui_cat),"indui",sep="_")
ui_cat<-ui_cat%>% mutate(ui_cat=as.character(seq(1,nrow(ui_cat))))
ui_cat <- ui_cat %>% mutate_all(na_if,"")

#some nutrients do not have upper intake levels (however, we know there are limits)
  #Dietary Guidelines for Americans recommends adults limit sodium intake to less than 2,300 mg per day
  #https://health.gov/our-work/nutrition-physical-activity/dietary-guidelines/previous-dietary-guidelines/2015/advisory-report/appendix-e-3/appendix-e-31a4

ui_cat$Sodium_indui<-c(1500,1500, 1500, 1800, 2200, 2300,2300,2300,2300,2300,2200,2300,2300,2300,2300,2300,2300,2300,2300,2300,2300,2300)

#units are difefrent (macronutrients are in percentage of energy)
#relation between rda and ui for those that have the same units
summary(mutate_all(ui_cat[,3:31], function(x) as.numeric(as.character(x)))/mutate_all(rda_cat[,3:31], function(x) as.numeric(as.character(x))))
cbind(rda[-1,1:2],mutate_all(ui_cat[,3:31], function(x) as.numeric(as.character(x)))/mutate_all(rda_cat[,3:31], function(x) as.numeric(as.character(x))))
#magnesium upper tolerable limit is below rda!!!!

#categorize individuals
HIES_personid<-HIES_personid %>%
  mutate(rda_cat=case_when(age_m<=6~"1",
                           age_m>=7 ~"2",
                           age>=1 & age<=3~"3",
                           age>=4 & age<=8~"4",
                           age>=9 & age<=13 & sex=="Male"~"5",
                           age>=14 & age<=18 & sex=="Male"~"6",
                           age>=19 & age<=30 & sex=="Male"~"7",
                           age>=31 & age<=50 & sex=="Male"~"8",
                           age>=51 & age<=70 & sex=="Male"~"9",
                           age>70 & sex=="Male"~"10",
                           age>=9 & age<=13 & sex=="Female"~"11",
                           age>=14 & age<=18 & sex=="Female"& (pregnant=="No"|is.na(pregnant))~"12",
                           age>=19 & age<=30 & sex=="Female"& (pregnant=="No"|is.na(pregnant))~"13",
                           age>=31 & age<=50 & sex=="Female"& (pregnant=="No"|is.na(pregnant))~"14",
                           age>=51 & age<=70 & sex=="Female"~"15",
                           age>70 & sex=="Female"~"16",
                           age>=14 & age<=18 & sex=="Female" & pregnant=="Yes" ~"17",
                           age>=19 & age<=30 & sex=="Female" & pregnant=="Yes" ~"18",
                           age>=31 & age<=50 & sex=="Female" & pregnant=="Yes" ~"19"))



#merge requirements
HIES_person_req<-merge(HIES_personid,rda_cat[,-(1:2)],by="rda_cat",all.x=T)
HIES_person_ui<-merge(HIES_personid, ui_cat[,-(1:2)],by.x="rda_cat",by.y="ui_cat",all.x=T)
#get household level requirements (and numbers())
hh_req<-HIES_person_req %>% dplyr::select(interview__key,Calcium_indrda:Fat_indrda_perccal) %>% dplyr::group_by(interview__key) %>% 
  mutate_if(is.character,as.numeric) %>%summarise_all(sum,na.rm=T) %>%left_join(HIES_person_req %>% group_by(interview__key) %>% dplyr::summarise(hh_number = n()))
#(upper intakes) get sums for thsoe based on total quantity and (weighted by household numbers) averages for percentages                                                                               
hh_ui<-HIES_person_ui %>% dplyr::select(interview__key,Calcium_indui:Total.Watera_indui) %>% dplyr::group_by(interview__key) %>% 
  mutate_if(is.character,as.numeric) %>%summarise_all(sum,na.rm=T) %>% left_join(HIES_person_ui %>% dplyr::select(interview__key,Carbohydrate_indui:Energy_indui) %>% dplyr::group_by(interview__key) %>% 
                                                                                   mutate_if(is.character,as.numeric) %>%summarise_all(mean,na.rm=T))%>%left_join(HIES_person_req %>% group_by(interview__key) %>% dplyr::summarise(hh_number = n()))
summary(hh_ui)
#IF UPPER INTAKE IS 0 IS BECAUSE THERE INST ONE
hh_ui[hh_ui==0]<-NA

#merge with food recall data
hhfoodsummary_indrda<-merge(hhfoodsummary,hh_req,by="interview__key",all.X=T)
hhfoodsummary_indui<-merge(hhfoodsummary,hh_ui,by="interview__key",all.X=T)


#is there a relationship between kcal adequacy and nutrient adequacy 
a<-ggplot(hhfoodsummary_indrda,aes(y=iron_mg_hh/Iron_indrda,x=kcal_hh/Energy_indrda))+geom_point()+geom_abline(slope=1,intercept = 0)+geom_vline(xintercept=1,lty=2)+geom_hline(yintercept=1,lty=2)+geom_smooth()+xlab("Household kcal adequacy")+ylab("Household iron adequacy")+theme_classic()+guides(col=F)
b<-ggplot(hhfoodsummary_indrda,aes(y=Vit_B12_mug_hh/Vitamin.B12_indrda,x=kcal_hh/Energy_indrda))+geom_point()+geom_abline(slope=1,intercept = 0)+geom_vline(xintercept=1,lty=2)+geom_hline(yintercept=1,lty=2)+geom_smooth()+xlab("Household kcal adequacy")+ylab("Household vitamin B12 adequacy")+theme_classic()+guides(col=F)
c<-ggplot(hhfoodsummary_indrda,aes(y=calcium_mg_hh/Calcium_indrda,x=kcal_hh/Energy_indrda))+geom_point()+geom_abline(slope=1,intercept = 0)+geom_vline(xintercept=1,lty=2)+geom_hline(yintercept=1,lty=2)+geom_smooth()+xlab("Household kcal adequacy")+ylab("Household calcium adequacy")+theme_classic()+guides(col=F)
d<-ggplot(hhfoodsummary_indrda,aes(y=pr_g_hh/Proteinb_indrda,x=kcal_hh/Energy_indrda))+geom_point()+geom_abline(slope=1,intercept = 0)+geom_vline(xintercept=1,lty=2)+geom_hline(yintercept=1,lty=2)+geom_smooth()+xlab("Household kcal adequacy")+ylab("Household protein adequacy")+theme_classic()+guides(col=F)
ggarrange(a,b,c,d)

#estimate percentage of energy from difefrent macronutirnts at the hh level
#based on https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7589789/#:~:text=Total%20consumption%20of%20carbohydrates%2C%20protein,g%20fat%20%3D%209%20kcal).
# 4kcal per gram (kcal/g) (17 kJ/g) for protein, 4 kcal/g for carbohydrates and 9 kcal/g  for fat

hhfoodsummary_indrda<- hhfoodsummary_indrda %>%
  mutate(hh_perc_kcal_pr=(pr_g_hh*4/kcal_hh)*100,
         hh_perc_kcal_carbs=(carbs_g_hh*4/kcal_hh)*100,
         hh_perc_kcal_fat=(tfats_g_hh*9/kcal_hh)*100,
         hh_perc_kcal_pr_seafood=(pr_g_seafood_hh*4/kcal_hh)*100,
         hh_perc_kcal_carbs_seafood=(carbs_g_seafood_hh*4/kcal_hh)*100,
         hh_perc_kcal_fat_seafood=(tfats_g_seafood_hh*9/kcal_hh)*100) %>%
  #and now calculateleative to fat rda
  mutate(hh_fat_rel_rda=(tfats_g_hh*9)/Fat_indrda_perccal,
         hh_fat_rel_rda_seafood=(tfats_g_seafood_hh*9)/Fat_indrda_perccal,
         hh_fat_rel_rda_tinnedsf=(tfats_g_tinnedsf_hh*9)/Fat_indrda_perccal,
         hh_fat_rel_rda_fishnoreef=(tfats_g_fishnoreef_hh*9)/Fat_indrda_perccal,
         hh_fat_rel_rda_driedfish=(tfats_g_driedfish_hh*9)/Fat_indrda_perccal,
         hh_fat_rel_rda_reeffish=(tfats_g_reeffish_hh*9)/Fat_indrda_perccal,
         hh_fat_rel_rda_inverts=(tfats_g_inverts_hh*9)/Fat_indrda_perccal,
         hh_fat_rel_rda_sharksrays=(tfats_g_sharksrays_hh*9)/Fat_indrda_perccal,
         hh_fat_rel_rda_seaweed=(tfats_g_seaweed_hh*9)/Fat_indrda_perccal)


ggplot(hhfoodsummary_indrda)+geom_histogram(aes(x=hh_fat_rel_rda))

hhfoodsummary_indui<- hhfoodsummary_indui %>%
  mutate(hh_perc_kcal_pr=(pr_g_hh*4/kcal_hh)*100,
         hh_perc_kcal_carbs=(carbs_g_hh*4/kcal_hh)*100,
         hh_perc_kcal_fat=(tfats_g_hh*9/kcal_hh)*100,
         hh_perc_kcal_pr_seafood=(pr_g_seafood_hh*4/kcal_hh)*100,
         hh_perc_kcal_carbs_seafood=(carbs_g_seafood_hh*4/kcal_hh)*100,
         hh_perc_kcal_fat_seafood=(tfats_g_seafood_hh*9/kcal_hh)*100) %>%
  #and now calculateleative to upper intake limits
  mutate(hh_pr_rel_ui=hh_perc_kcal_pr/Proteinb_indui,
         hh_carbs_rel_ui=hh_perc_kcal_carbs/Carbohydrate_indui,
         hh_fat_rel_ui=hh_perc_kcal_fat/Fat_indui) %>%
  mutate(hh_sodium_rel_ui=sodium_mg_hh/Sodium_indui,
         hh_magnesium_rel_ui=magnesium_mg_hh/Magnesium_indui,
         hh_calcium_rel_ui=calcium_mg_hh/Calcium_indui,
         hh_iron_rel_ui=iron_mg_hh/Iron_indui,
         hh_zinc_rel_ui=zinc_mg_hh/Zinc_indui,
         hh_VA_rel_ui=VitA_RAE_mug_hh/Vitamin.A_indui,
        hh_niacin_rel_ui=niacin_mg_hh/Niacin_indui,
        hh_VC_rel_ui=Vit_C_mg_hh/Vitamin.C_indui,
         hh_VE_rel_ui=VitE_mug_hh/Vitamin.E_indui,
        #seafood
         hh_pr_rel_ui_seafood=hh_perc_kcal_pr_seafood/Proteinb_indui,
         hh_fat_rel_ui_seafood=hh_perc_kcal_fat_seafood/Fat_indui,
         hh_carbs_rel_ui_seafood=hh_perc_kcal_carbs_seafood/Carbohydrate_indui,
         hh_sodium_rel_ui_seafood=sodium_mg_seafood_hh/Sodium_indui,
         hh_magnesium_rel_ui_seafood=magnesium_mg_seafood_hh/Magnesium_indui,
         hh_calcium_rel_ui_seafood=calcium_mg_seafood_hh/Calcium_indui,
         hh_iron_rel_ui_seafood=iron_mg_seafood_hh/Iron_indui,
         hh_zinc_rel_ui_seafood=zinc_mg_seafood_hh/Zinc_indui,
         hh_VA_rel_ui_seafood=VitA_RAE_mug_seafood_hh/Vitamin.A_indui,
         hh_niacin_rel_ui_seafood=niacin_mg_seafood_hh/Niacin_indui,
         hh_VC_rel_ui_seafood=Vit_C_mg_seafood_hh/Vitamin.C_indui,
         hh_VE_rel_ui_seafood=VitE_mug_seafood_hh/Vitamin.E_indui)



#estimate a nutrient adequacy index (i.e., how many nutrients besides kcal households are meeting in terms of adequacy )
colnames(hhfoodsummary_indrda)
hhfoodsummary_indrda<-hhfoodsummary_indrda %>%
  mutate(hh_pr_adeq=pr_g_hh/Proteinb_indrda,
         #hh_fat_adeq=tfats_g_hh/Fat_indrda,
         hh_fat_adeq=hh_fat_rel_rda,
         hh_carbs_adeq=carbs_g_hh/Carbohydrate_indrda,
         hh_fibre_adeq=fibre_g_hh/Total.Fiber_indrda,
         hh_sodium_adeq=sodium_mg_hh/Sodium_indrda,
         hh_magnesium_adeq=magnesium_mg_hh/Magnesium_indrda,
         hh_potassium_adeq=potassium_mg_hh/Potassium_indrda,
         hh_calcium_adeq=calcium_mg_hh/Calcium_indrda,
         hh_iron_adeq=iron_mg_hh/Iron_indrda,
         hh_zinc_adeq=zinc_mg_hh/Zinc_indrda,
         hh_VA_adeq=VitA_RAE_mug_hh/Vitamin.A_indrda,
         hh_thiamin_adeq=thiamin_mg_hh/Thiamin_indrda,
         hh_riboflavin_adeq=riboflavin_mg_hh/Riboflavin_indrda,
         hh_niacin_adeq=niacin_mg_hh/Niacin_indrda,
         hh_VB12_adeq=Vit_B12_mug_hh/Vitamin.B12_indrda,
         hh_VC_adeq=Vit_C_mg_hh/Vitamin.C_indrda,
         hh_VE_adeq=VitE_mug_hh/Vitamin.E_indrda,
         hh_kcal_adequacy=kcal_hh/Energy_indrda,
     #from seafood    
         hh_pr_adeq_seafood=pr_g_seafood_hh/Proteinb_indrda,
         hh_fat_adeq_seafood=hh_fat_rel_rda_seafood,
         hh_carbs_adeq_seafood=carbs_g_seafood_hh/Carbohydrate_indrda,
         hh_fibre_adeq_seafood=fibre_g_seafood_hh/Total.Fiber_indrda,
         hh_sodium_adeq_seafood=sodium_mg_seafood_hh/Sodium_indrda,
         hh_magnesium_adeq_seafood=magnesium_mg_seafood_hh/Magnesium_indrda,
         hh_potassium_adeq_seafood=potassium_mg_seafood_hh/Potassium_indrda,
         hh_calcium_adeq_seafood=calcium_mg_seafood_hh/Calcium_indrda,
         hh_iron_adeq_seafood=iron_mg_seafood_hh/Iron_indrda,
         hh_zinc_adeq_seafood=zinc_mg_seafood_hh/Zinc_indrda,
         hh_VA_adeq_seafood=VitA_RAE_mug_seafood_hh/Vitamin.A_indrda,
         hh_thiamin_adeq_seafood=thiamin_mg_seafood_hh/Thiamin_indrda,
         hh_riboflavin_adeq_seafood=riboflavin_mg_seafood_hh/Riboflavin_indrda,
         hh_niacin_adeq_seafood=niacin_mg_seafood_hh/Niacin_indrda,
         hh_VB12_adeq_seafood=Vit_B12_mug_seafood_hh/Vitamin.B12_indrda,
         hh_VC_adeq_seafood=Vit_C_mg_seafood_hh/Vitamin.C_indrda,
         hh_VE_adeq_seafood=VitE_mug_seafood_hh/Vitamin.E_indrda,
         hh_kcal_adequacy_seafood=kcal_seafood_hh/Energy_indrda,
     #from tinnedsf    
     hh_pr_adeq_tinnedsf=pr_g_tinnedsf_hh/Proteinb_indrda,
     hh_fat_adeq_tinnedsf=hh_fat_rel_rda_tinnedsf,
     hh_carbs_adeq_tinnedsf=carbs_g_tinnedsf_hh/Carbohydrate_indrda,
     hh_fibre_adeq_tinnedsf=fibre_g_tinnedsf_hh/Total.Fiber_indrda,
     hh_sodium_adeq_tinnedsf=sodium_mg_tinnedsf_hh/Sodium_indrda,
     hh_magnesium_adeq_tinnedsf=magnesium_mg_tinnedsf_hh/Magnesium_indrda,
     hh_potassium_adeq_tinnedsf=potassium_mg_tinnedsf_hh/Potassium_indrda,
     hh_calcium_adeq_tinnedsf=calcium_mg_tinnedsf_hh/Calcium_indrda,
     hh_iron_adeq_tinnedsf=iron_mg_tinnedsf_hh/Iron_indrda,
     hh_zinc_adeq_tinnedsf=zinc_mg_tinnedsf_hh/Zinc_indrda,
     hh_VA_adeq_tinnedsf=VitA_RAE_mug_tinnedsf_hh/Vitamin.A_indrda,
     hh_thiamin_adeq_tinnedsf=thiamin_mg_tinnedsf_hh/Thiamin_indrda,
     hh_riboflavin_adeq_tinnedsf=riboflavin_mg_tinnedsf_hh/Riboflavin_indrda,
     hh_niacin_adeq_tinnedsf=niacin_mg_tinnedsf_hh/Niacin_indrda,
     hh_VB12_adeq_tinnedsf=Vit_B12_mug_tinnedsf_hh/Vitamin.B12_indrda,
     hh_VC_adeq_tinnedsf=Vit_C_mg_tinnedsf_hh/Vitamin.C_indrda,
     hh_VE_adeq_tinnedsf=VitE_mug_tinnedsf_hh/Vitamin.E_indrda,
     hh_kcal_adequacy_tinnedsf=kcal_tinnedsf_hh/Energy_indrda,
     #from reeffish    
     hh_pr_adeq_reeffish=pr_g_reeffish_hh/Proteinb_indrda,
     hh_fat_adeq_reeffish=hh_fat_rel_rda_reeffish,
     hh_carbs_adeq_reeffish=carbs_g_reeffish_hh/Carbohydrate_indrda,
     hh_fibre_adeq_reeffish=fibre_g_reeffish_hh/Total.Fiber_indrda,
     hh_sodium_adeq_reeffish=sodium_mg_reeffish_hh/Sodium_indrda,
     hh_magnesium_adeq_reeffish=magnesium_mg_reeffish_hh/Magnesium_indrda,
     hh_potassium_adeq_reeffish=potassium_mg_reeffish_hh/Potassium_indrda,
     hh_calcium_adeq_reeffish=calcium_mg_reeffish_hh/Calcium_indrda,
     hh_iron_adeq_reeffish=iron_mg_reeffish_hh/Iron_indrda,
     hh_zinc_adeq_reeffish=zinc_mg_reeffish_hh/Zinc_indrda,
     hh_VA_adeq_reeffish=VitA_RAE_mug_reeffish_hh/Vitamin.A_indrda,
     hh_thiamin_adeq_reeffish=thiamin_mg_reeffish_hh/Thiamin_indrda,
     hh_riboflavin_adeq_reeffish=riboflavin_mg_reeffish_hh/Riboflavin_indrda,
     hh_niacin_adeq_reeffish=niacin_mg_reeffish_hh/Niacin_indrda,
     hh_VB12_adeq_reeffish=Vit_B12_mug_reeffish_hh/Vitamin.B12_indrda,
     hh_VC_adeq_reeffish=Vit_C_mg_reeffish_hh/Vitamin.C_indrda,
     hh_VE_adeq_reeffish=VitE_mug_reeffish_hh/Vitamin.E_indrda,
     hh_kcal_adequacy_reeffish=kcal_reeffish_hh/Energy_indrda,
     #from fishnoreef    
     hh_pr_adeq_fishnoreef=pr_g_fishnoreef_hh/Proteinb_indrda,
     hh_fat_adeq_fishnoreef=hh_fat_rel_rda_fishnoreef,
     hh_carbs_adeq_fishnoreef=carbs_g_fishnoreef_hh/Carbohydrate_indrda,
     hh_fibre_adeq_fishnoreef=fibre_g_fishnoreef_hh/Total.Fiber_indrda,
     hh_sodium_adeq_fishnoreef=sodium_mg_fishnoreef_hh/Sodium_indrda,
     hh_magnesium_adeq_fishnoreef=magnesium_mg_fishnoreef_hh/Magnesium_indrda,
     hh_potassium_adeq_fishnoreef=potassium_mg_fishnoreef_hh/Potassium_indrda,
     hh_calcium_adeq_fishnoreef=calcium_mg_fishnoreef_hh/Calcium_indrda,
     hh_iron_adeq_fishnoreef=iron_mg_fishnoreef_hh/Iron_indrda,
     hh_zinc_adeq_fishnoreef=zinc_mg_fishnoreef_hh/Zinc_indrda,
     hh_VA_adeq_fishnoreef=VitA_RAE_mug_fishnoreef_hh/Vitamin.A_indrda,
     hh_thiamin_adeq_fishnoreef=thiamin_mg_fishnoreef_hh/Thiamin_indrda,
     hh_riboflavin_adeq_fishnoreef=riboflavin_mg_fishnoreef_hh/Riboflavin_indrda,
     hh_niacin_adeq_fishnoreef=niacin_mg_fishnoreef_hh/Niacin_indrda,
     hh_VB12_adeq_fishnoreef=Vit_B12_mug_fishnoreef_hh/Vitamin.B12_indrda,
     hh_VC_adeq_fishnoreef=Vit_C_mg_fishnoreef_hh/Vitamin.C_indrda,
     hh_VE_adeq_fishnoreef=VitE_mug_fishnoreef_hh/Vitamin.E_indrda,
     hh_kcal_adequacy_fishnoreef=kcal_fishnoreef_hh/Energy_indrda,
     #from driedfish    
     hh_pr_adeq_driedfish=pr_g_driedfish_hh/Proteinb_indrda,
     hh_fat_adeq_driedfish=hh_fat_rel_rda_driedfish,
     hh_carbs_adeq_driedfish=carbs_g_driedfish_hh/Carbohydrate_indrda,
     hh_fibre_adeq_driedfish=fibre_g_driedfish_hh/Total.Fiber_indrda,
     hh_sodium_adeq_driedfish=sodium_mg_driedfish_hh/Sodium_indrda,
     hh_magnesium_adeq_driedfish=magnesium_mg_driedfish_hh/Magnesium_indrda,
     hh_potassium_adeq_driedfish=potassium_mg_driedfish_hh/Potassium_indrda,
     hh_calcium_adeq_driedfish=calcium_mg_driedfish_hh/Calcium_indrda,
     hh_iron_adeq_driedfish=iron_mg_driedfish_hh/Iron_indrda,
     hh_zinc_adeq_driedfish=zinc_mg_driedfish_hh/Zinc_indrda,
     hh_VA_adeq_driedfish=VitA_RAE_mug_driedfish_hh/Vitamin.A_indrda,
     hh_thiamin_adeq_driedfish=thiamin_mg_driedfish_hh/Thiamin_indrda,
     hh_riboflavin_adeq_driedfish=riboflavin_mg_driedfish_hh/Riboflavin_indrda,
     hh_niacin_adeq_driedfish=niacin_mg_driedfish_hh/Niacin_indrda,
     hh_VB12_adeq_driedfish=Vit_B12_mug_driedfish_hh/Vitamin.B12_indrda,
     hh_VC_adeq_driedfish=Vit_C_mg_driedfish_hh/Vitamin.C_indrda,
     hh_VE_adeq_driedfish=VitE_mug_driedfish_hh/Vitamin.E_indrda,
     hh_kcal_adequacy_driedfish=kcal_driedfish_hh/Energy_indrda,
     #from inverts    
     hh_pr_adeq_inverts=pr_g_inverts_hh/Proteinb_indrda,
     hh_fat_adeq_inverts=hh_fat_rel_rda_inverts,
     hh_carbs_adeq_inverts=carbs_g_inverts_hh/Carbohydrate_indrda,
     hh_fibre_adeq_inverts=fibre_g_inverts_hh/Total.Fiber_indrda,
     hh_sodium_adeq_inverts=sodium_mg_inverts_hh/Sodium_indrda,
     hh_magnesium_adeq_inverts=magnesium_mg_inverts_hh/Magnesium_indrda,
     hh_potassium_adeq_inverts=potassium_mg_inverts_hh/Potassium_indrda,
     hh_calcium_adeq_inverts=calcium_mg_inverts_hh/Calcium_indrda,
     hh_iron_adeq_inverts=iron_mg_inverts_hh/Iron_indrda,
     hh_zinc_adeq_inverts=zinc_mg_inverts_hh/Zinc_indrda,
     hh_VA_adeq_inverts=VitA_RAE_mug_inverts_hh/Vitamin.A_indrda,
     hh_thiamin_adeq_inverts=thiamin_mg_inverts_hh/Thiamin_indrda,
     hh_riboflavin_adeq_inverts=riboflavin_mg_inverts_hh/Riboflavin_indrda,
     hh_niacin_adeq_inverts=niacin_mg_inverts_hh/Niacin_indrda,
     hh_VB12_adeq_inverts=Vit_B12_mug_inverts_hh/Vitamin.B12_indrda,
     hh_VC_adeq_inverts=Vit_C_mg_inverts_hh/Vitamin.C_indrda,
     hh_VE_adeq_inverts=VitE_mug_inverts_hh/Vitamin.E_indrda,
     hh_kcal_adequacy_inverts=kcal_inverts_hh/Energy_indrda,
     #from sharksrays    
     hh_pr_adeq_sharksrays=pr_g_sharksrays_hh/Proteinb_indrda,
     hh_fat_adeq_sharksrays=hh_fat_rel_rda_sharksrays,
     hh_carbs_adeq_sharksrays=carbs_g_sharksrays_hh/Carbohydrate_indrda,
     hh_fibre_adeq_sharksrays=fibre_g_sharksrays_hh/Total.Fiber_indrda,
     hh_sodium_adeq_sharksrays=sodium_mg_sharksrays_hh/Sodium_indrda,
     hh_magnesium_adeq_sharksrays=magnesium_mg_sharksrays_hh/Magnesium_indrda,
     hh_potassium_adeq_sharksrays=potassium_mg_sharksrays_hh/Potassium_indrda,
     hh_calcium_adeq_sharksrays=calcium_mg_sharksrays_hh/Calcium_indrda,
     hh_iron_adeq_sharksrays=iron_mg_sharksrays_hh/Iron_indrda,
     hh_zinc_adeq_sharksrays=zinc_mg_sharksrays_hh/Zinc_indrda,
     hh_VA_adeq_sharksrays=VitA_RAE_mug_sharksrays_hh/Vitamin.A_indrda,
     hh_thiamin_adeq_sharksrays=thiamin_mg_sharksrays_hh/Thiamin_indrda,
     hh_riboflavin_adeq_sharksrays=riboflavin_mg_sharksrays_hh/Riboflavin_indrda,
     hh_niacin_adeq_sharksrays=niacin_mg_sharksrays_hh/Niacin_indrda,
     hh_VB12_adeq_sharksrays=Vit_B12_mug_sharksrays_hh/Vitamin.B12_indrda,
     hh_VC_adeq_sharksrays=Vit_C_mg_sharksrays_hh/Vitamin.C_indrda,
     hh_VE_adeq_sharksrays=VitE_mug_sharksrays_hh/Vitamin.E_indrda,
     hh_kcal_adequacy_sharksrays=kcal_sharksrays_hh/Energy_indrda,
     #from seaweed    
     hh_pr_adeq_seaweed=pr_g_seaweed_hh/Proteinb_indrda,
     hh_fat_adeq_seaweed=hh_fat_rel_rda_seaweed,
     hh_carbs_adeq_seaweed=carbs_g_seaweed_hh/Carbohydrate_indrda,
     hh_fibre_adeq_seaweed=fibre_g_seaweed_hh/Total.Fiber_indrda,
     hh_sodium_adeq_seaweed=sodium_mg_seaweed_hh/Sodium_indrda,
     hh_magnesium_adeq_seaweed=magnesium_mg_seaweed_hh/Magnesium_indrda,
     hh_potassium_adeq_seaweed=potassium_mg_seaweed_hh/Potassium_indrda,
     hh_calcium_adeq_seaweed=calcium_mg_seaweed_hh/Calcium_indrda,
     hh_iron_adeq_seaweed=iron_mg_seaweed_hh/Iron_indrda,
     hh_zinc_adeq_seaweed=zinc_mg_seaweed_hh/Zinc_indrda,
     hh_VA_adeq_seaweed=VitA_RAE_mug_seaweed_hh/Vitamin.A_indrda,
     hh_thiamin_adeq_seaweed=thiamin_mg_seaweed_hh/Thiamin_indrda,
     hh_riboflavin_adeq_seaweed=riboflavin_mg_seaweed_hh/Riboflavin_indrda,
     hh_niacin_adeq_seaweed=niacin_mg_seaweed_hh/Niacin_indrda,
     hh_VB12_adeq_seaweed=Vit_B12_mug_seaweed_hh/Vitamin.B12_indrda,
     hh_VC_adeq_seaweed=Vit_C_mg_seaweed_hh/Vitamin.C_indrda,
     hh_VE_adeq_seaweed=VitE_mug_seaweed_hh/Vitamin.E_indrda,
     hh_kcal_adequacy_seaweed=kcal_seaweed_hh/Energy_indrda) %>%
  
  mutate(seafood_pr_contr=hh_pr_adeq_seafood/hh_pr_adeq,
         seafood_fat_contr=hh_fat_adeq_seafood/hh_fat_adeq,
         seafood_carbs_contr=hh_carbs_adeq_seafood/hh_carbs_adeq,
         seafood_fibre_contr=hh_fibre_adeq_seafood/hh_fibre_adeq,
         seafood_sodium_contr=hh_sodium_adeq_seafood/hh_sodium_adeq,
         seafood_magnesium_contr=hh_magnesium_adeq_seafood/hh_magnesium_adeq,
         seafood_potassium_contr=hh_potassium_adeq_seafood/hh_potassium_adeq,
         seafood_calcium_contr=hh_calcium_adeq_seafood/hh_calcium_adeq,
         seafood_iron_contr=hh_iron_adeq_seafood/hh_iron_adeq,
         seafood_zinc_contr=hh_zinc_adeq_seafood/hh_zinc_adeq,
         seafood_VA_contr=hh_VA_adeq_seafood/hh_VA_adeq,
         seafood_thiamin_contr=hh_thiamin_adeq_seafood/hh_thiamin_adeq,
         seafood_riboflavin_contr=hh_riboflavin_adeq_seafood/hh_riboflavin_adeq,
         seafood_niacin_contr=hh_niacin_adeq_seafood/hh_niacin_adeq,
         seafood_VB12_contr=hh_VB12_adeq_seafood/hh_VB12_adeq,
         seafood_VC_contr=hh_VC_adeq_seafood/hh_VC_adeq,
         seafood_VE_contr=hh_VE_adeq_seafood/hh_VE_adeq,
         seafood_kcal_contr=hh_kcal_adequacy_seafood/hh_kcal_adequacy)


#total adequacies
adequacies<-hhfoodsummary_indrda %>% 
  select(hh_pr_adeq:hh_kcal_adequacy)
adequacies[]<-ifelse(adequacies>1, 1, 0)
ncol(adequacies)
hhfoodsummary_indrda$nut_adeq_met_out18<-rowSums(adequacies)
summary(hhfoodsummary_indrda$nut_adeq_met_out18)
length(hhfoodsummary_indrda$nut_adeq_met_out18[hhfoodsummary_indrda$nut_adeq_met_out18==18])/length(hhfoodsummary_indrda$nut_adeq_met_out18)
length(hhfoodsummary_indrda$nut_adeq_met_out18[hhfoodsummary_indrda$nut_adeq_met_out18==17])/length(hhfoodsummary_indrda$nut_adeq_met_out18)

hhfoodsummary_indrda$island[hhfoodsummary_indrda$nut_adeq_met_out18==18]
length(unique(hhfoodsummary_indrda$island[hhfoodsummary_indrda$nut_adeq_met_out18==18]))
length(unique(hhfoodsummary_indrda$village[hhfoodsummary_indrda$nut_adeq_met_out18==18]))

#relationship
ggplot(hhfoodsummary_indrda,aes(y=nut_adeq_met_out18,x=hh_kcal_adequacy) )+geom_point(aes())+geom_smooth(aes())+
  geom_vline(xintercept = 1,lty=2)+geom_hline(yintercept = mean(hhfoodsummary_indrda$nut_adeq_met_out16),lty=2)+ylim(0,16)+theme_classic()+
  ylab("Nut adequacies met by household (out of 18)")+xlab("Household kcal adequacy")

ggplot()+geom_point(aes(y=hhfoodsummary_indui$hh_sodium_rel_ui,x=hhfoodsummary_indrda$hh_kcal_adequacy))+
  geom_vline(xintercept = 1,lty=2)+geom_hline(yintercept = 1,lty=2)+theme_classic()+
  ylab("hh sodium intake relative to upper recommendations")+xlab("Household kcal adequacy")

ggplot(melt(hhfoodsummary_indui %>%select(interview__key,hh_pr_rel_ui:hh_VE_rel_ui)))+geom_density_ridges(aes(x=value,y=variable),scale=1,rel_min_height = 0.01,quantile_lines = TRUE, quantile_fun=function(value,...)mean(value))+geom_vline(lty=2,xintercept=1) +theme_classic()+xlab(" household intake relative to upper intake recommendations")+ylab("")

#write.csv(hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq:nut_adeq_met_out16),"HH_ADEQUACY.CSV",row.names=F)
#write.csv(hhfoodsummary_indui %>%select(interview__key,hh_pr_rel_ui:hh_VE_rel_ui),"HH_INTAKE_RELUI.CSV",row.names=F)
#write.csv(hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq:nut_adeq_met_out16) %>% left_join(hhfoodsummary_indui %>%select(interview__key,hh_pr_rel_ui:hh_VE_rel_ui)),"HH_nut_rel_adeq_ui.CSV",row.names=F)



#################################################################################
#re-do adequacy analyses of seafood contribution paper using the demiographic requirements
nut_adequacy<-hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq:hh_kcal_adequacy)%>%gather(variable,value,-interview__key)
ggplot(hhfoodsummary_indrda, aes(y=hh_pr_adeq,x=hh_kcal_adequacy))+geom_point(fill="grey",pch=21,alpha=0.5)+theme_classic()+ylab("Household protein adequacy")+xlab("Household kilocalorie adequacy")+geom_vline(xintercept = 1)+geom_hline(yintercept = 1)
#percent households above 1
nut_adequacy %>% group_by(variable) %>% dplyr::summarise(perc_hh_adeq=(sum(value>1)/length(value))*100)%>%arrange(desc(perc_hh_adeq))
#mean
rank_ia<-as.data.frame(nut_adequacy %>% group_by(variable) %>% dplyr::summarise(mean_inadequacy=mean(value)) %>%arrange(desc(mean_inadequacy)))
rank_ia$variable <- reorder(as.factor(rank_ia$variable), rank_ia$mean_inadequacy)
#to add mean to ridges
#quantile_fun=function(value,...)mean(value)
a<-ggplot(nut_adequacy)+geom_density_ridges_gradient(aes(x=value*100, y=variable,fill=stat(x)),scale=1,rel_min_height = 0.01,quantile_lines = TRUE, quantiles=2)+xlim(c(0,500))+
  scale_fill_gradient2("",high="navyblue",mid="white",low="darkred",midpoint = 100)+
  xlab("% of RDA from all foods")+ylab("")+theme(axis.text.y = element_text(size=9),panel.background = element_rect(fill="white",color="black"))+guides(fill=F)+geom_vline(aes(xintercept=100))+
  scale_y_discrete(limits=levels(rank_ia$variable),labels=c(hh_kcal_adequacy="Kilocalories",
                                                            hh_pr_adeq="Protein",
                                                            hh_fat_adeq="Total fats",
                                                            hh_carbs_adeq="Carbohydrates",
                                                            hh_fibre_adeq="Fibre",
                                                            hh_sodium_adeq="Sodium",
                                                            hh_magnesium_adeq="Magnesium",
                                                            hh_potassium_adeq="Potasium",
                                                            hh_calcium_adeq="Calcium",
                                                            hh_iron_adeq="Total iron",
                                                            hh_zinc_adeq="Zinc",
                                                            hh_VA_adeq="Vitamin A (RAE)",
                                                            hh_thiamin_adeq="Vitamin B1 (thiamin)",
                                                            hh_riboflavin_adeq="Vitamin B2 (riboflavin)",
                                                            hh_niacin_adeq="Vitamin B3 (niacin)",
                                                            hh_VB12_adeq="Vitamin B12 (colbalamine)",
                                                            hh_VC_adeq="Vitamin C",
                                                            hh_VE_adeq="Vitamin E"))


nut_adequacy_seafood<-hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq_seafood:hh_kcal_adequacy_seafood)%>%gather(variable,value,-interview__key)
nut_adequacy_seafood<-nut_adequacy_seafood %>% mutate(variable=gsub("_seafood","",variable))
nut_adequacy$seafood_value<-nut_adequacy_seafood$value
nut_adequacy$withoutseafood<-(nut_adequacy$value)-(nut_adequacy$seafood_value)
rank_ia_seafood<-as.data.frame(nut_adequacy_seafood %>% group_by(variable) %>% dplyr::summarise(mean_inadequacy=median(value)) %>%arrange(desc(mean_inadequacy)))
rank_ia_seafood$variable <- reorder(as.factor(rank_ia_seafood$variable), rank_ia_seafood$mean_inadequacy)


b<-ggplot(nut_adequacy)+geom_density_ridges_gradient(aes(x=seafood_value*100, y=variable,fill=stat(x)),scale=1,rel_min_height = 0.01,quantile_lines = TRUE, quantiles = 2)+xlim(c(0,400))+
  scale_fill_gradient2("",high="navyblue",mid="white",low="darkred",midpoint = 100)+  scale_y_discrete(limits=levels(rank_ia$variable))+
  xlab("% of RDA from seafood")+ylab("")+theme(axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+guides(fill=F)+geom_vline(aes(xintercept=100))
nut_adequacy$perc_withoutseafood<-(nut_adequacy$value*100)-(nut_adequacy$seafood_value*100)
c<-ggplot(nut_adequacy)+geom_density_ridges_gradient(aes(x=perc_withoutseafood, y=variable,fill=stat(x)),scale=1,rel_min_height = 0.01,quantile_lines = TRUE, quantiles = 2)+xlim(c(0,400))+
  scale_fill_gradient2("",high="navyblue",mid="white",low="darkred",midpoint = 100)+  scale_y_discrete(limits=levels(rank_ia$variable))+
  xlab("% of RDA without seafood")+ylab("")+theme(axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+guides(fill=F)+geom_vline(aes(xintercept=100))
ggarrange(a,b,c,nrow=1,ncol=3,widths=c(1.5,1,1),labels=c("a","b","c"))


#add different seafood groups
nut_adequacy_reeffish<-hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq_reeffish:hh_kcal_adequacy_reeffish)%>%gather(variable,value,-interview__key)
nut_adequacy_reeffish<-nut_adequacy_reeffish %>% mutate(variable=gsub("_reeffish","",variable))
nut_adequacy$reeffish_value<-nut_adequacy_reeffish$value

nut_adequacy_fishnoreef<-hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq_fishnoreef:hh_kcal_adequacy_fishnoreef)%>%gather(variable,value,-interview__key)
nut_adequacy_fishnoreef<-nut_adequacy_fishnoreef %>% mutate(variable=gsub("_fishnoreef","",variable))
nut_adequacy$fishnoreef_value<-nut_adequacy_fishnoreef$value

nut_adequacy_inverts<-hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq_inverts:hh_kcal_adequacy_inverts)%>%gather(variable,value,-interview__key)
nut_adequacy_inverts<-nut_adequacy_inverts %>% mutate(variable=gsub("_inverts","",variable))
nut_adequacy$inverts_value<-nut_adequacy_inverts$value

nut_adequacy_tinnedsf<-hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq_tinnedsf:hh_kcal_adequacy_tinnedsf)%>%gather(variable,value,-interview__key)
nut_adequacy_tinnedsf<-nut_adequacy_tinnedsf %>% mutate(variable=gsub("_tinnedsf","",variable))
nut_adequacy$tinnedsf_value<-nut_adequacy_tinnedsf$value

nut_adequacy_driedfish<-hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq_driedfish:hh_kcal_adequacy_driedfish)%>%gather(variable,value,-interview__key)
nut_adequacy_driedfish<-nut_adequacy_driedfish %>% mutate(variable=gsub("_driedfish","",variable))
nut_adequacy$driedfish_value<-nut_adequacy_driedfish$value

nut_adequacy_sharksrays<-hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq_sharksrays:hh_kcal_adequacy_sharksrays)%>%gather(variable,value,-interview__key)
nut_adequacy_sharksrays<-nut_adequacy_sharksrays %>% mutate(variable=gsub("_sharksrays","",variable))
nut_adequacy$sharksrays_value<-nut_adequacy_sharksrays$value

nut_adequacy_seaweed<-hhfoodsummary_indrda %>%select(interview__key,hh_pr_adeq_seaweed:hh_kcal_adequacy_seaweed)%>%gather(variable,value,-interview__key)
nut_adequacy_seaweed<-nut_adequacy_seaweed %>% mutate(variable=gsub("_seaweed","",variable))
nut_adequacy$seaweed_value<-nut_adequacy_seaweed$value

#bar plots showing the means
rank_ia<-as.data.frame(nut_adequacy %>% group_by(variable) %>% dplyr::summarise(mean_inadequacy=mean(value), 
                                                                                mean_seafood=mean(seafood_value),
                                                                                mean_noseafood=mean(withoutseafood), 
                                                                                mean_reeffish=mean(reeffish_value),
                                                                                mean_fishnoreef=mean(fishnoreef_value),
                                                                                mean_inverts=mean(inverts_value),
                                                                                mean_tinnedsf=mean(tinnedsf_value),
                                                                                mean_driedfish=mean(driedfish_value),
                                                                                mean_sharksrays=mean(sharksrays_value),
                                                                                mean_seaweed=mean(seaweed_value)) %>%arrange(desc(mean_inadequacy)))
rank_ia$variable <- reorder(as.factor(rank_ia$variable), rank_ia$mean_inadequacy)

rank_ia%>%rename(nutrient=variable)%>%select(nutrient, mean_seafood:mean_noseafood)%>%gather(variable,value,-nutrient)
rda_fig<-ggplot()+geom_bar(data=rank_ia,aes(y=mean_inadequacy*100,x=variable),fill="turquoise",stat = "identity",col="black")+geom_bar(data=rank_ia,aes(y=mean_noseafood*100,x=variable), stat = "identity")+geom_hline(yintercept = 100,lty=2)+ coord_flip()+
  scale_x_discrete(limits=levels(rank_ia$variable),labels=c(hh_kcal_adequacy="Kilocalories",
                                                            hh_pr_adeq="Protein",
                                                            hh_fat_adeq="Total fats",
                                                            hh_carbs_adeq="Carbohydrates",
                                                            hh_fibre_adeq="Fibre",
                                                            hh_sodium_adeq="Sodium",
                                                            hh_magnesium_adeq="Magnesium",
                                                            hh_potassium_adeq="Potasium",
                                                            hh_calcium_adeq="Calcium",
                                                            hh_iron_adeq="Total iron",
                                                            hh_zinc_adeq="Zinc",
                                                            hh_VA_adeq="Vitamin A (RAE)",
                                                            hh_thiamin_adeq="Vitamin B1 (thiamin)",
                                                            hh_riboflavin_adeq="Vitamin B2 (riboflavin)",
                                                            hh_niacin_adeq="Vitamin B3 (niacin)",
                                                            hh_VB12_adeq="Vitamin B12 (colbalamine)",
                                                            hh_VC_adeq="Vitamin C",
                                                            hh_VE_adeq="Vitamin E"))+theme_classic()+ylab("Mean household adequacy (% of RDA met)")+xlab("")

rda_fig<-ggplot()+geom_bar(data=rank_ia%>%rename(nutrient=variable)%>%select(nutrient, mean_seafood:mean_noseafood)%>%gather(variable,value,-nutrient),aes(y=value*100,x=nutrient, fill=factor(variable,levels=rev(c("mean_seafood","mean_noseafood")))),stat = "identity",col="black")+
  scale_fill_manual("",values=c("mean_noseafood"="darkgrey",
                                "mean_seafood"="cyan3"))+ coord_flip()+geom_hline(yintercept = 100,lty=2)+
  scale_x_discrete(limits=levels(rank_ia$variable),labels=c(hh_kcal_adequacy="Kilocalories",
                                                            hh_pr_adeq="Protein",
                                                            hh_fat_adeq="Total fats",
                                                            hh_carbs_adeq="Carbohydrates",
                                                            hh_fibre_adeq="Fibre",
                                                            hh_sodium_adeq="Sodium",
                                                            hh_magnesium_adeq="Magnesium",
                                                            hh_potassium_adeq="Potasium",
                                                            hh_calcium_adeq="Calcium",
                                                            hh_iron_adeq="Total iron",
                                                            hh_zinc_adeq="Zinc",
                                                            hh_VA_adeq="Vitamin A (RAE)",
                                                            hh_thiamin_adeq="Vitamin B1 (thiamin)",
                                                            hh_riboflavin_adeq="Vitamin B2 (riboflavin)",
                                                            hh_niacin_adeq="Vitamin B3 (niacin)",
                                                            hh_VB12_adeq="Vitamin B12 (colbalamine)",
                                                            hh_VC_adeq="Vitamin C",
                                                            hh_VE_adeq="Vitamin E"))+theme(panel.background = element_rect(fill="white",color="black"))+ylab("Mean household adequacy (% of RDA met)")+xlab("")+guides(fill=F)

seafoodgroup_rankai<-rank_ia%>%rename(nutrient=variable)%>%select(nutrient, mean_reeffish:mean_seaweed)%>%gather(variable,value,-nutrient)
rda_fig2<-ggplot()+geom_bar(data=seafoodgroup_rankai,aes(y=value*100,x=nutrient, fill=factor(variable,levels=rev(c("mean_reeffish","mean_fishnoreef","mean_inverts","mean_tinnedsf","mean_driedfish","mean_sharksrays","mean_seaweed")))), stat = "identity",col="black")+
  scale_fill_brewer("",palette="BrBG")+ coord_flip()+geom_hline(yintercept = 100,lty=2)+
  scale_x_discrete(limits=levels(rank_ia$variable),labels=c(hh_kcal_adequacy="Kilocalories",
                                                            hh_pr_adeq="Protein",
                                                            hh_fat_adeq="Total fats",
                                                            hh_carbs_adeq="Carbohydrates",
                                                            hh_fibre_adeq="Fibre",
                                                            hh_sodium_adeq="Sodium",
                                                            hh_magnesium_adeq="Magnesium",
                                                            hh_potassium_adeq="Potasium",
                                                            hh_calcium_adeq="Calcium",
                                                            hh_iron_adeq="Total iron",
                                                            hh_zinc_adeq="Zinc",
                                                            hh_VA_adeq="Vitamin A (RAE)",
                                                            hh_thiamin_adeq="Vitamin B1 (thiamin)",
                                                            hh_riboflavin_adeq="Vitamin B2 (riboflavin)",
                                                            hh_niacin_adeq="Vitamin B3 (niacin)",
                                                            hh_VB12_adeq="Vitamin B12 (colbalamine)",
                                                            hh_VC_adeq="Vitamin C",
                                                            hh_VE_adeq="Vitamin E"))+theme(axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+ylab("Mean household adequacy from seafood (% of RDA met)")+xlab("")#+guides(fill=F)

rda_fig3<-ggplot()+geom_bar(data=seafoodgroup_rankai,aes(y=value*100,x=nutrient, fill=factor(variable,levels=rev(c("mean_reeffish","mean_fishnoreef","mean_inverts","mean_tinnedsf","mean_driedfish","mean_sharksrays","mean_seaweed")))),position="fill", stat = "identity",col="black")+
  scale_fill_brewer("",palette="BrBG")+ coord_flip()+#geom_hline(yintercept = 100,lty=2)+
  scale_x_discrete(limits=levels(rank_ia$variable),labels=c(hh_kcal_adequacy="Kilocalories",
                                                            hh_pr_adeq="Protein",
                                                            hh_fat_adeq="Total fats",
                                                            hh_carbs_adeq="Carbohydrates",
                                                            hh_fibre_adeq="Fibre",
                                                            hh_sodium_adeq="Sodium",
                                                            hh_magnesium_adeq="Magnesium",
                                                            hh_potassium_adeq="Potasium",
                                                            hh_calcium_adeq="Calcium",
                                                            hh_iron_adeq="Total iron",
                                                            hh_zinc_adeq="Zinc",
                                                            hh_VA_adeq="Vitamin A (RAE)",
                                                            hh_thiamin_adeq="Vitamin B1 (thiamin)",
                                                            hh_riboflavin_adeq="Vitamin B2 (riboflavin)",
                                                            hh_niacin_adeq="Vitamin B3 (niacin)",
                                                            hh_VB12_adeq="Vitamin B12 (colbalamine)",
                                                            hh_VC_adeq="Vitamin C",
                                                            hh_VE_adeq="Vitamin E"))+theme(axis.text.y = element_blank(),panel.background = element_rect(fill="white",color="black"))+ylab("Seafood proportion")+xlab("")+guides(fill=F)

windows()
ggarrange(rda_fig,rda_fig2,rda_fig3,widths=c(2,1.5,0.6),nrow=1,ncol=3)
ggarrange(rda_fig2,rda_fig3,widths=c(1.6,0.6),nrow=1,ncol=2)

###############################################################################

#hierarchical models to partition variance

#overall adequacy______________________________________________________________
hist(hhfoodsummary_indrda$hh_sodium_adeq)
sodium_model<-brm(hh_sodium_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(sodium_model)
hist(hhfoodsummary_indrda$hh_carbs_adeq)
carbs_model<-brm(hh_carbs_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(carbs_model)
hist(hhfoodsummary_indrda$hh_kcal_adequacy)
kcal_model<-brm(hh_kcal_adequacy~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(kcal_model)
hist(hhfoodsummary_indrda$hh_pr_adeq)
pr_model<-brm(hh_pr_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(pr_model)
hist(log(hhfoodsummary_indrda$hh_fat_adeq))
tfats_model<-brm(hh_fat_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(tfats_model)
hist(hhfoodsummary_indrda$hh_fibre_adeq)
fibre_model<-brm(hh_fibre_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(fibre_model)
hist(hhfoodsummary_indrda$hh_magnesium_adeq)
magnesium_model<-brm(hh_magnesium_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(magnesium_model)
hist(hhfoodsummary_indrda$hh_potassium_adeq)
potassium_model<-brm(hh_potassium_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(potassium_model)
hist(hhfoodsummary_indrda$hh_calcium_adeq)
calcium_model<-brm(hh_calcium_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(calcium_model)
hist(hhfoodsummary_indrda$hh_iron_adeq)
iron_model<-brm(hh_iron_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(iron_model)
hist(hhfoodsummary_indrda$hh_zinc_adeq)
zinc_model<-brm(hh_zinc_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(zinc_model)
hist(hhfoodsummary_indrda$hh_VA_adeq)
VitA_RAE_model<-brm(hh_VA_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitA_RAE_model)
hist(hhfoodsummary_indrda$hh_thiamin_adeq)
thiamin_model<-brm(hh_thiamin_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(thiamin_model)
hist(hhfoodsummary_indrda$hh_riboflavin_adeq)
riboflavin_model<-brm(hh_riboflavin_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(riboflavin_model)
hist(hhfoodsummary_indrda$hh_niacin_adeq)
niacin_model<-brm(hh_niacin_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="lognormal")
pp_check(niacin_model)
hist(hhfoodsummary_indrda$hh_VB12_adeq)
Vit_B12_model<-brm(hh_VB12_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_B12_model)
hist(hhfoodsummary_indrda$hh_VC_adeq)
Vit_C_model<-brm(hh_VC_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_C_model)
hist(hhfoodsummary_indrda$hh_VE_adeq)
VitE_model<-brm(hh_VE_adeq~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitE_model)

#do figures
#global intercept
intercepts_global<-as.data.frame(cbind(rbind(exp(fixef(sodium_model)[,-2]), exp(fixef(carbs_model)[,-2]),exp(fixef(Vit_B12_model)[,-2]),exp(fixef(pr_model)[,-2]),exp(fixef(tfats_model)[,-2]),
                                             exp(fixef(kcal_model)[,-2]),exp(fixef(niacin_model)[,-2]),exp(fixef(potassium_model)[,-2]),exp(fixef(magnesium_model)[,-2]),exp(fixef(zinc_model)[,-2]),
                                             exp(fixef(thiamin_model)[,-2]),exp(fixef(riboflavin_model)[,-2]),exp(fixef(iron_model)[,-2]),exp(fixef(Vit_C_model)[,-2]),exp(fixef(fibre_model)[,-2]),
                                             exp(fixef(VitE_model)[,-2]),exp(fixef(calcium_model)[,-2]),exp(fixef(VitA_RAE_model)[,-2])),
                                       c("Sodium","Carbohydrates","Vitamin B12 (colbalamine)","Protein","Total fats","Kilocalories",
                                         "Vitamin B3 (niacin)","Potasium","Magnesium","Zinc","Vitamin B1 (thiamin)","Vitamin B2 (riboflavin)","Total iron","Vitamin C",
                                         "Fibre","Vitamin E","Calcium","Vitamin A (RAE)")))
colnames(intercepts_global)<-c("Estimate","Q2.5","Q97.5","variable")
ggplot(intercepts_global,aes(x=as.numeric(Estimate),y=as.factor(variable),xmin=as.numeric(Q2.5),xmax=as.numeric(Q97.5)))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+theme_classic()+xlab("Nutrient adequacy (prop of RDA met)")+ylab("")

#random effects
ranef_community<-as.data.frame(ranef(sodium_model, groups="island:village", probs = c(0.1,0.9)))
ranef_community$community<-row.names(ranef_community)
colnames(ranef_community)<-c("estimate","se","conf.low","conf.high","community")
country_sodium_intercept=exp(fixef(sodium_model, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model, groups="island", probs = c(0.1,0.9)))[,-2])
ggplot(country_sodium_intercept,aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(sodium_model, probs = c(0.1,0.9))[1]),lty=2)+theme_classic()+xlab("Estimated proportion of sodium RDA intake")+ylab("")

a<-ggplot(ranef_community,aes(x=estimate,y=community,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")

#island level random effects
ranef_island<-as.data.frame(ranef(sodium_model, groups="island", probs = c(0.1,0.9)))
ranef_island$island<-row.names(ranef_island)
colnames(ranef_island)<-c("estimate","se","conf.low","conf.high","island")

b<-ggplot(ranef_island,aes(x=estimate,y=island,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")
windows()
ggarrange(a,b,nrow=1,ncol=2)
#random effects fo all islands
a<-ggplot(as.data.frame(ranef(sodium_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(sodium_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_text(size=7), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Sodium")
b<-ggplot(as.data.frame(ranef(carbs_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(carbs_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Carbohydrates")
c<-ggplot(as.data.frame(ranef(Vit_B12_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(Vit_B12_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin B12")
d<-ggplot(as.data.frame(ranef(pr_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(pr_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Protein")
e<-ggplot(as.data.frame(ranef(tfats_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(tfats_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Total fats")
f<-ggplot(as.data.frame(ranef(kcal_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(kcal_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Kilocalories")
g<-ggplot(as.data.frame(ranef(niacin_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(niacin_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Niacin")
h<-ggplot(as.data.frame(ranef(potassium_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(potassium_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Potassium")
i<-ggplot(as.data.frame(ranef(magnesium_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(magnesium_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Magnessium")
j<-ggplot(as.data.frame(ranef(zinc_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(zinc_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_text(size=7), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Zinc")
k<-ggplot(as.data.frame(ranef(thiamin_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(thiamin_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Thiamin")
l<-ggplot(as.data.frame(ranef(riboflavin_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(riboflavin_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Riboflavin")
m<-ggplot(as.data.frame(ranef(iron_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(iron_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Iron")
n<-ggplot(as.data.frame(ranef(Vit_C_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(Vit_C_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin C")
o<-ggplot(as.data.frame(ranef(fibre_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(fibre_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Fiber")
p<-ggplot(as.data.frame(ranef(VitE_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(VitE_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin E")
q<-ggplot(as.data.frame(ranef(calcium_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(calcium_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Calcium")
r<-ggplot(as.data.frame(ranef(VitA_RAE_model, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(VitA_RAE_model, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin A")
windows()
annotate_figure(ggarrange(a,b,c,d,e,f,g,h,i,nrow=1,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Offset from island-level intercepts")
windows()
annotate_figure(ggarrange(j,k,l,m,n,o,p,q,r,nrow=1,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Offset from island-level intercepts")
windows()
annotate_figure(ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,nrow=2,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Proportion of RDA met from all foods")


#letters[seq( from = 1, to = 10 )]
#variance partitioning#########################################################
ggplot(melt((as.data.frame(sodium_model)[,2:4])^2))+geom_density(aes(x=value,fill=variable),alpha=0.5)+ggtitle("sodium RDA")+xlab("variance")+scale_fill_discrete("level",labels=c("sigma"="among household","sd_island__Intercept"="among island","sd_island:village__Intercept"="among village"))+theme_classic()+scale_fill_viridis_d()

variance_decomp<-as.data.frame(cbind(rbind(matrixStats::colMedians(as.matrix((as.data.frame(sodium_model)[,2:4])^2)), matrixStats::colMedians(as.matrix((as.data.frame(carbs_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(Vit_B12_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(pr_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(tfats_model)[,2:4])^2)),
                                           matrixStats::colMedians(as.matrix((as.data.frame(kcal_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(niacin_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(potassium_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(magnesium_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(zinc_model)[,2:4])^2)),
                                           matrixStats::colMedians(as.matrix((as.data.frame(thiamin_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(riboflavin_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(iron_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(Vit_C_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(fibre_model)[,2:4])^2)),
                                           matrixStats::colMedians(as.matrix((as.data.frame(VitE_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(calcium_model)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(VitA_RAE_model)[,2:4])^2))),
                                     c("Sodium","Carbohydrates","Vitamin B12 (cobalamine)","Protein","Total fats","Kilocalories",
                                       "Vitamin B3 (niacin)","Potasium","Magnesium","Zinc","Vitamin B1 (thiamin)","Vitamin B2 (riboflavin)","Total iron","Vitamin C",
                                       "Fibre","Vitamin E","Calcium","Vitamin A (RAE)")))
colnames(variance_decomp)<-c("among islands","among villages","among households","variable")
variance_decomp %>%gather(level,variance,-variable)
variance_decomp_fig<-ggplot(variance_decomp %>%gather(level,variance,-variable),aes(x=variable,y=as.numeric(variance),fill=level))+geom_bar(position="stack",stat="identity")+theme(axis.text.x = element_text(angle=90,vjust=0),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("Median variance")+scale_fill_viridis_d()+ggtitle("Nutritional adequacy from all foods")+ylim(c(0,2.25))


##now do for seafood contributions to adequacy#########################################################
hist(hhfoodsummary_indrda$hh_sodium_adeq_seafood)
sodium_model_seafood<-brm(hh_sodium_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(sodium_model_seafood)
hist(hhfoodsummary_indrda$hh_carbs_adeq_seafood)
carbs_model_seafood<-brm(hh_carbs_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(carbs_model_seafood)
hist(hhfoodsummary_indrda$hh_kcal_adequacy_seafood)
kcal_model_seafood<-brm(hh_kcal_adequacy_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(kcal_model_seafood)
hist(hhfoodsummary_indrda$hh_pr_adeq_seafood)
pr_model_seafood<-brm(hh_pr_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(pr_model_seafood)
hist(log(hhfoodsummary_indrda$hh_fat_adeq_seafood))
tfats_model_seafood<-brm(hh_fat_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(tfats_model_seafood)
hist(hhfoodsummary_indrda$hh_fibre_adeq_seafood)
fibre_model_seafood<-brm(hh_fibre_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(fibre_model_seafood)
hist(hhfoodsummary_indrda$hh_magnesium_adeq_seafood)
magnesium_model_seafood<-brm(hh_magnesium_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(magnesium_model_seafood)
hist(hhfoodsummary_indrda$hh_potassium_adeq_seafood)
potassium_model_seafood<-brm(hh_potassium_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(potassium_model_seafood)
hist(hhfoodsummary_indrda$hh_calcium_adeq_seafood)
calcium_model_seafood<-brm(hh_calcium_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(calcium_model_seafood)
hist(hhfoodsummary_indrda$hh_iron_adeq_seafood)
iron_model_seafood<-brm(hh_iron_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(iron_model_seafood)
hist(hhfoodsummary_indrda$hh_zinc_adeq_seafood)
zinc_model_seafood<-brm(hh_zinc_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(zinc_model_seafood)
hist(hhfoodsummary_indrda$hh_VA_adeq_seafood)
VitA_RAE_model_seafood<-brm(hh_VA_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitA_RAE_model_seafood)
hist(hhfoodsummary_indrda$hh_thiamin_adeq_seafood)
thiamin_model_seafood<-brm(hh_thiamin_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(thiamin_model_seafood)
hist(hhfoodsummary_indrda$hh_riboflavin_adeq_seafood)
riboflavin_model_seafood<-brm(hh_riboflavin_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(riboflavin_model_seafood)
hist(hhfoodsummary_indrda$hh_niacin_adeq_seafood)
niacin_model_seafood<-brm(hh_niacin_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(niacin_model_seafood)
hist(hhfoodsummary_indrda$hh_VB12_adeq_seafood)
Vit_B12_model_seafood<-brm(hh_VB12_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_B12_model_seafood)
hist(hhfoodsummary_indrda$hh_VC_adeq_seafood)
Vit_C_model_seafood<-brm(hh_VC_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_C_model_seafood)
hist(hhfoodsummary_indrda$hh_VE_adeq_seafood)
VitE_model_seafood<-brm(hh_VE_adeq_seafood~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitE_model_seafood)

#do figures
#global intercept
intercepts_global_seafood<-as.data.frame(cbind(rbind(exp(fixef(sodium_model_seafood)[,-2]), exp(fixef(carbs_model_seafood)[,-2]),exp(fixef(Vit_B12_model_seafood)[,-2]),exp(fixef(pr_model_seafood)[,-2]),exp(fixef(tfats_model_seafood)[,-2]),
                                             exp(fixef(kcal_model_seafood)[,-2]),exp(fixef(niacin_model_seafood)[,-2]),exp(fixef(potassium_model_seafood)[,-2]),exp(fixef(magnesium_model_seafood)[,-2]),exp(fixef(zinc_model_seafood)[,-2]),
                                             exp(fixef(thiamin_model_seafood)[,-2]),exp(fixef(riboflavin_model_seafood)[,-2]),exp(fixef(iron_model_seafood)[,-2]),exp(fixef(Vit_C_model_seafood)[,-2]),exp(fixef(fibre_model_seafood)[,-2]),
                                             exp(fixef(VitE_model_seafood)[,-2]),exp(fixef(calcium_model_seafood)[,-2]),exp(fixef(VitA_RAE_model_seafood)[,-2])),
                                       c("Sodium","Carbohydrates","Vitamin B12 (colbalamine)","Protein","Total fats","Kilocalories",
                                         "Vitamin B3 (niacin)","Potasium","Magnesium","Zinc","Vitamin B1 (thiamin)","Vitamin B2 (riboflavin)","Total iron","Vitamin C",
                                         "Fibre","Vitamin E","Calcium","Vitamin A (RAE)")))
colnames(intercepts_global_seafood)<-c("Estimate","Q2.5","Q97.5","variable")
ggplot(intercepts_global_seafood,aes(x=as.numeric(Estimate),y=as.factor(variable),xmin=as.numeric(Q2.5),xmax=as.numeric(Q97.5)))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+theme_classic()+xlab("Nutrient adequacy (prop of RDA met)")+ylab("")

#random effects
ranef_community<-as.data.frame(ranef(sodium_model_seafood, groups="island:village", probs = c(0.1,0.9)))
ranef_community$community<-row.names(ranef_community)
colnames(ranef_community)<-c("estimate","se","conf.low","conf.high","community")
country_sodium_intercept=exp(fixef(sodium_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2])
ggplot(country_sodium_intercept,aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(sodium_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme_classic()+xlab("Estimated proportion of sodium RDA intake")+ylab("")

a<-ggplot(ranef_community,aes(x=estimate,y=community,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")

#island level random effects
ranef_island<-as.data.frame(ranef(sodium_model_seafood, groups="island", probs = c(0.1,0.9)))
ranef_island$island<-row.names(ranef_island)
colnames(ranef_island)<-c("estimate","se","conf.low","conf.high","island")

b<-ggplot(ranef_island,aes(x=estimate,y=island,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")
windows()
ggarrange(a,b,nrow=1,ncol=2)
#random effects fo all islands
a<-ggplot(exp(fixef(sodium_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(sodium_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Sodium")
b<-ggplot(exp(fixef(carbs_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(carbs_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(carbs_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Carbohydrates")
c<-ggplot(exp(fixef(Vit_B12_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(Vit_B12_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(Vit_B12_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin B12")
d<-ggplot(exp(fixef(pr_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(pr_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(pr_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Protein")
e<-ggplot(exp(fixef(tfats_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(tfats_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(tfats_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Total fats")
f<-ggplot(exp(fixef(kcal_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(kcal_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(kcal_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Kilocalories")
g<-ggplot(exp(fixef(niacin_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(niacin_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(niacin_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Niacin")
h<-ggplot(exp(fixef(potassium_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(potassium_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(potassium_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Potassium")
i<-ggplot(exp(fixef(magnesium_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(magnesium_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(magnesium_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Magnesium")
j<-ggplot(exp(fixef(zinc_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(zinc_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(zinc_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Zinc")
k<-ggplot(exp(fixef(thiamin_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(thiamin_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(thiamin_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Thiamin")
l<-ggplot(exp(fixef(riboflavin_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(riboflavin_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(riboflavin_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Riboflavin")
m<-ggplot(exp(fixef(iron_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(iron_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(iron_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Iron")
n<-ggplot(exp(fixef(Vit_C_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(Vit_C_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(Vit_C_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin C")
o<-ggplot(exp(fixef(fibre_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(fibre_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(fibre_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Fiber")
p<-ggplot(exp(fixef(VitE_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitE_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(VitE_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin E")
q<-ggplot(exp(fixef(calcium_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(calcium_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(calcium_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Calcium")
r<-ggplot(exp(fixef(VitA_RAE_model_seafood, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitA_RAE_model_seafood, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(VitA_RAE_model_seafood, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin A")
windows()
annotate_figure(ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,nrow=2,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Proportion of RDA met from seafood")

#random effects fo all islands
a<-ggplot(as.data.frame(ranef(sodium_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(sodium_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_text(size=7), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Sodium")
b<-ggplot(as.data.frame(ranef(carbs_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(carbs_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Carbohydrates")
c<-ggplot(as.data.frame(ranef(Vit_B12_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(Vit_B12_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin B12")
d<-ggplot(as.data.frame(ranef(pr_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(pr_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Protein")
e<-ggplot(as.data.frame(ranef(tfats_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(tfats_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Total fats")
f<-ggplot(as.data.frame(ranef(kcal_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(kcal_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Kilocalories")
g<-ggplot(as.data.frame(ranef(niacin_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(niacin_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Niacin")
h<-ggplot(as.data.frame(ranef(potassium_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(potassium_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Potassium")
i<-ggplot(as.data.frame(ranef(magnesium_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(magnesium_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Magnessium")
j<-ggplot(as.data.frame(ranef(zinc_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(zinc_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_text(size=7), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Zinc")
k<-ggplot(as.data.frame(ranef(thiamin_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(thiamin_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Thiamin")
l<-ggplot(as.data.frame(ranef(riboflavin_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(riboflavin_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Riboflavin")
m<-ggplot(as.data.frame(ranef(iron_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(iron_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Iron")
n<-ggplot(as.data.frame(ranef(Vit_C_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(Vit_C_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin C")
o<-ggplot(as.data.frame(ranef(fibre_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(fibre_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Fiber")
p<-ggplot(as.data.frame(ranef(VitE_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(VitE_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin E")
q<-ggplot(as.data.frame(ranef(calcium_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(calcium_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Calcium")
r<-ggplot(as.data.frame(ranef(VitA_RAE_model_seafood, groups="island:village", probs = c(0.1,0.9)))[,-2],aes(x=island.village.Estimate.Intercept,y=rownames(as.data.frame(ranef(VitA_RAE_model_seafood, groups="island:village", probs = c(0.1,0.9)))),xmin=island.village.Q10.Intercept,xmax=island.village.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0, lty=2)+theme(axis.text.y=element_blank(), panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin A")
windows()
annotate_figure(ggarrange(a,b,c,d,e,f,g,h,i,nrow=1,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Offset from island-level intercepts")
windows()
annotate_figure(ggarrange(j,k,l,m,n,o,p,q,r,nrow=1,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Offset from island-level intercepts")
windows()
annotate_figure(ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,nrow=2,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Proportion of RDA met from all foods")


#variance partitioning#########################################################

variance_decomp_seafood<-as.data.frame(cbind(rbind(matrixStats::colMedians(as.matrix((as.data.frame(sodium_model_seafood)[,2:4])^2)), matrixStats::colMedians(as.matrix((as.data.frame(carbs_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(Vit_B12_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(pr_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(tfats_model_seafood)[,2:4])^2)),
                                           matrixStats::colMedians(as.matrix((as.data.frame(kcal_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(niacin_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(potassium_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(magnesium_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(zinc_model_seafood)[,2:4])^2)),
                                           matrixStats::colMedians(as.matrix((as.data.frame(thiamin_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(riboflavin_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(iron_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(Vit_C_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(fibre_model_seafood)[,2:4])^2)),
                                           matrixStats::colMedians(as.matrix((as.data.frame(VitE_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(calcium_model_seafood)[,2:4])^2)),matrixStats::colMedians(as.matrix((as.data.frame(VitA_RAE_model_seafood)[,2:4])^2))),
                                     c("Sodium","Carbohydrates","Vitamin B12 (cobalamine)","Protein","Total fats","Kilocalories",
                                       "Vitamin B3 (niacin)","Potasium","Magnesium","Zinc","Vitamin B1 (thiamin)","Vitamin B2 (riboflavin)","Total iron","Vitamin C",
                                       "Fibre","Vitamin E","Calcium","Vitamin A (RAE)")))
colnames(variance_decomp_seafood)<-c("among islands","among villages","among households","variable")
variance_decomp_seafood_fig<-ggplot(variance_decomp_seafood %>%gather(level,variance,-variable),aes(x=variable,y=as.numeric(variance),fill=level))+geom_bar(position="stack",stat="identity")+theme(axis.text.x = element_text(angle=90,vjust=0),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+scale_fill_viridis_d("")+ggtitle("Nutritional adequacy from seafood")+ylim(c(0,2.25))

ggarrange(variance_decomp_fig+guides(fill=F), variance_decomp_seafood_fig, labels=c("a","b"), widths=c(1,1.2))

##############################################################################

##now do for speciefic groups


##now do for reeffish contributions to adequacy#########################################################
hist(hhfoodsummary_indrda$hh_sodium_adeq_reeffish)
sodium_model_reeffish<-brm(hh_sodium_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(sodium_model_reeffish)
hist(hhfoodsummary_indrda$hh_carbs_adeq_reeffish)
carbs_model_reeffish<-brm(hh_carbs_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(carbs_model_reeffish)
hist(hhfoodsummary_indrda$hh_kcal_adequacy_reeffish)
kcal_model_reeffish<-brm(hh_kcal_adequacy_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(kcal_model_reeffish)
hist(hhfoodsummary_indrda$hh_pr_adeq_reeffish)
pr_model_reeffish<-brm(hh_pr_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(pr_model_reeffish)
hist(log(hhfoodsummary_indrda$hh_fat_adeq_reeffish))
tfats_model_reeffish<-brm(hh_fat_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(tfats_model_reeffish)
summary(hhfoodsummary_indrda$hh_fibre_adeq_reeffish)
fibre_model_reeffish<-brm(hh_fibre_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(fibre_model_reeffish)
hist(hhfoodsummary_indrda$hh_magnesium_adeq_reeffish)
magnesium_model_reeffish<-brm(hh_magnesium_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(magnesium_model_reeffish)
hist(hhfoodsummary_indrda$hh_potassium_adeq_reeffish)
potassium_model_reeffish<-brm(hh_potassium_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(potassium_model_reeffish)
hist(hhfoodsummary_indrda$hh_calcium_adeq_reeffish)
calcium_model_reeffish<-brm(hh_calcium_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(calcium_model_reeffish)
hist(hhfoodsummary_indrda$hh_iron_adeq_reeffish)
iron_model_reeffish<-brm(hh_iron_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(iron_model_reeffish)
hist(hhfoodsummary_indrda$hh_zinc_adeq_reeffish)
zinc_model_reeffish<-brm(hh_zinc_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(zinc_model_reeffish)
hist(hhfoodsummary_indrda$hh_VA_adeq_reeffish)
VitA_RAE_model_reeffish<-brm(hh_VA_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitA_RAE_model_reeffish)
hist(hhfoodsummary_indrda$hh_thiamin_adeq_reeffish)
thiamin_model_reeffish<-brm(hh_thiamin_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(thiamin_model_reeffish)
hist(hhfoodsummary_indrda$hh_riboflavin_adeq_reeffish)
riboflavin_model_reeffish<-brm(hh_riboflavin_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(riboflavin_model_reeffish)
hist(hhfoodsummary_indrda$hh_niacin_adeq_reeffish)
niacin_model_reeffish<-brm(hh_niacin_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(niacin_model_reeffish)
hist(hhfoodsummary_indrda$hh_VB12_adeq_reeffish)
Vit_B12_model_reeffish<-brm(hh_VB12_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_B12_model_reeffish)
hist(hhfoodsummary_indrda$hh_VC_adeq_reeffish)
Vit_C_model_reeffish<-brm(hh_VC_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_C_model_reeffish)
hist(hhfoodsummary_indrda$hh_VE_adeq_reeffish)
VitE_model_reeffish<-brm(hh_VE_adeq_reeffish~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitE_model_reeffish)

#do figures
#global intercept
intercepts_global_reeffish<-as.data.frame(cbind(rbind(exp(fixef(sodium_model_reeffish)[,-2]), exp(fixef(carbs_model_reeffish)[,-2]),exp(fixef(Vit_B12_model_reeffish)[,-2]),exp(fixef(pr_model_reeffish)[,-2]),exp(fixef(tfats_model_reeffish)[,-2]),
                                                     exp(fixef(kcal_model_reeffish)[,-2]),exp(fixef(niacin_model_reeffish)[,-2]),exp(fixef(potassium_model_reeffish)[,-2]),exp(fixef(magnesium_model_reeffish)[,-2]),exp(fixef(zinc_model_reeffish)[,-2]),
                                                     exp(fixef(thiamin_model_reeffish)[,-2]),exp(fixef(riboflavin_model_reeffish)[,-2]),exp(fixef(iron_model_reeffish)[,-2]),exp(fixef(Vit_C_model_reeffish)[,-2]),
                                                     exp(fixef(VitE_model_reeffish)[,-2]),exp(fixef(calcium_model_reeffish)[,-2]),exp(fixef(VitA_RAE_model_reeffish)[,-2])),
                                               c("Sodium","Carbohydrates","Vitamin B12 (colbalamine)","Protein","Total fats","Kilocalories",
                                                 "Vitamin B3 (niacin)","Potasium","Magnesium","Zinc","Vitamin B1 (thiamin)","Vitamin B2 (riboflavin)","Total iron","Vitamin C",
                                                 "Vitamin E","Calcium","Vitamin A (RAE)")))
colnames(intercepts_global_reeffish)<-c("Estimate","Q2.5","Q97.5","variable")
ggplot(intercepts_global_reeffish,aes(x=as.numeric(Estimate),y=as.factor(variable),xmin=as.numeric(Q2.5),xmax=as.numeric(Q97.5)))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+theme_classic()+xlab("Nutrient adequacy (prop of RDA met)")+ylab("")

#random effects
ranef_community<-as.data.frame(ranef(sodium_model_reeffish, groups="island:village", probs = c(0.1,0.9)))
ranef_community$community<-row.names(ranef_community)
colnames(ranef_community)<-c("estimate","se","conf.low","conf.high","community")
country_sodium_intercept=exp(fixef(sodium_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2])
ggplot(country_sodium_intercept,aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(sodium_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme_classic()+xlab("Estimated proportion of sodium RDA intake")+ylab("")

a<-ggplot(ranef_community,aes(x=estimate,y=community,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")

#island level random effects
ranef_island<-as.data.frame(ranef(sodium_model_reeffish, groups="island", probs = c(0.1,0.9)))
ranef_island$island<-row.names(ranef_island)
colnames(ranef_island)<-c("estimate","se","conf.low","conf.high","island")

b<-ggplot(ranef_island,aes(x=estimate,y=island,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")
windows()
ggarrange(a,b,nrow=1,ncol=2)
#random effects fo all islands
a<-ggplot(exp(fixef(sodium_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(sodium_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Sodium")
b<-ggplot(exp(fixef(carbs_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(carbs_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(carbs_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Carbohydrates")
c<-ggplot(exp(fixef(Vit_B12_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(Vit_B12_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(Vit_B12_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin B12")
d<-ggplot(exp(fixef(pr_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(pr_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(pr_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Protein")
e<-ggplot(exp(fixef(tfats_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(tfats_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(tfats_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Total fats")
f<-ggplot(exp(fixef(kcal_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(kcal_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(kcal_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Kilocalories")
g<-ggplot(exp(fixef(niacin_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(niacin_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(niacin_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Niacin")
h<-ggplot(exp(fixef(potassium_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(potassium_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(potassium_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Potassium")
i<-ggplot(exp(fixef(magnesium_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(magnesium_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(magnesium_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Magnesium")
j<-ggplot(exp(fixef(zinc_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(zinc_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(zinc_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Zinc")
k<-ggplot(exp(fixef(thiamin_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(thiamin_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(thiamin_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Thiamin")
l<-ggplot(exp(fixef(riboflavin_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(riboflavin_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(riboflavin_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Riboflavin")
m<-ggplot(exp(fixef(iron_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(iron_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(iron_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Iron")
n<-ggplot(exp(fixef(Vit_C_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(Vit_C_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(Vit_C_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin C")
p<-ggplot(exp(fixef(VitE_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitE_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(VitE_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin E")
q<-ggplot(exp(fixef(calcium_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(calcium_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(calcium_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Calcium")
r<-ggplot(exp(fixef(VitA_RAE_model_reeffish, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitA_RAE_model_reeffish, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(VitA_RAE_model_reeffish, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin A")
windows()
annotate_figure(ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,p,q,r,nrow=2,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Proportion of RDA met from reeffish")


##############################################################################

##now do for fishnoreef contributions to adequacy#########################################################
hist(hhfoodsummary_indrda$hh_sodium_adeq_fishnoreef)
sodium_model_fishnoreef<-brm(hh_sodium_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(sodium_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_carbs_adeq_fishnoreef)
carbs_model_fishnoreef<-brm(hh_carbs_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(carbs_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_kcal_adequacy_fishnoreef)
kcal_model_fishnoreef<-brm(hh_kcal_adequacy_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(kcal_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_pr_adeq_fishnoreef)
pr_model_fishnoreef<-brm(hh_pr_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(pr_model_fishnoreef)
hist(log(hhfoodsummary_indrda$hh_fat_adeq_fishnoreef))
tfats_model_fishnoreef<-brm(hh_fat_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(tfats_model_fishnoreef)
summary(hhfoodsummary_indrda$hh_fibre_adeq_fishnoreef)
#fibre_model_fishnoreef<-brm(hh_fibre_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
#pp_check(fibre_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_magnesium_adeq_fishnoreef)
magnesium_model_fishnoreef<-brm(hh_magnesium_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(magnesium_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_potassium_adeq_fishnoreef)
potassium_model_fishnoreef<-brm(hh_potassium_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(potassium_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_calcium_adeq_fishnoreef)
calcium_model_fishnoreef<-brm(hh_calcium_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(calcium_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_iron_adeq_fishnoreef)
iron_model_fishnoreef<-brm(hh_iron_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(iron_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_zinc_adeq_fishnoreef)
zinc_model_fishnoreef<-brm(hh_zinc_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(zinc_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_VA_adeq_fishnoreef)
VitA_RAE_model_fishnoreef<-brm(hh_VA_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitA_RAE_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_thiamin_adeq_fishnoreef)
thiamin_model_fishnoreef<-brm(hh_thiamin_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(thiamin_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_riboflavin_adeq_fishnoreef)
riboflavin_model_fishnoreef<-brm(hh_riboflavin_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(riboflavin_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_niacin_adeq_fishnoreef)
niacin_model_fishnoreef<-brm(hh_niacin_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(niacin_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_VB12_adeq_fishnoreef)
Vit_B12_model_fishnoreef<-brm(hh_VB12_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_B12_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_VC_adeq_fishnoreef)
#Vit_C_model_fishnoreef<-brm(hh_VC_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
#pp_check(Vit_C_model_fishnoreef)
hist(hhfoodsummary_indrda$hh_VE_adeq_fishnoreef)
VitE_model_fishnoreef<-brm(hh_VE_adeq_fishnoreef~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitE_model_fishnoreef)

#do figures
#global intercept
intercepts_global_fishnoreef<-as.data.frame(cbind(rbind(exp(fixef(sodium_model_fishnoreef)[,-2]), exp(fixef(carbs_model_fishnoreef)[,-2]),exp(fixef(Vit_B12_model_fishnoreef)[,-2]),exp(fixef(pr_model_fishnoreef)[,-2]),exp(fixef(tfats_model_fishnoreef)[,-2]),
                                                      exp(fixef(kcal_model_fishnoreef)[,-2]),exp(fixef(niacin_model_fishnoreef)[,-2]),exp(fixef(potassium_model_fishnoreef)[,-2]),exp(fixef(magnesium_model_fishnoreef)[,-2]),exp(fixef(zinc_model_fishnoreef)[,-2]),
                                                      exp(fixef(thiamin_model_fishnoreef)[,-2]),exp(fixef(riboflavin_model_fishnoreef)[,-2]),exp(fixef(iron_model_fishnoreef)[,-2]),
                                                      exp(fixef(VitE_model_fishnoreef)[,-2]),exp(fixef(calcium_model_fishnoreef)[,-2]),exp(fixef(VitA_RAE_model_fishnoreef)[,-2])),
                                                c("Sodium","Carbohydrates","Vitamin B12 (colbalamine)","Protein","Total fats","Kilocalories",
                                                  "Vitamin B3 (niacin)","Potasium","Magnesium","Zinc","Vitamin B1 (thiamin)","Vitamin B2 (riboflavin)","Total iron",
                                                  "Vitamin E","Calcium","Vitamin A (RAE)")))
colnames(intercepts_global_fishnoreef)<-c("Estimate","Q2.5","Q97.5","variable")
ggplot(intercepts_global_fishnoreef,aes(x=as.numeric(Estimate),y=as.factor(variable),xmin=as.numeric(Q2.5),xmax=as.numeric(Q97.5)))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+theme_classic()+xlab("Nutrient adequacy (prop of RDA met)")+ylab("")


#random effects fo all islands
a<-ggplot(exp(fixef(sodium_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(sodium_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Sodium")
b<-ggplot(exp(fixef(carbs_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(carbs_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(carbs_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Carbohydrates")
c<-ggplot(exp(fixef(Vit_B12_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(Vit_B12_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(Vit_B12_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin B12")
d<-ggplot(exp(fixef(pr_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(pr_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(pr_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Protein")
e<-ggplot(exp(fixef(tfats_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(tfats_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(tfats_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Total fats")
f<-ggplot(exp(fixef(kcal_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(kcal_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(kcal_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Kilocalories")
g<-ggplot(exp(fixef(niacin_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(niacin_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(niacin_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Niacin")
h<-ggplot(exp(fixef(potassium_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(potassium_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(potassium_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Potassium")
i<-ggplot(exp(fixef(magnesium_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(magnesium_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(magnesium_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Magnesium")
j<-ggplot(exp(fixef(zinc_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(zinc_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(zinc_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Zinc")
k<-ggplot(exp(fixef(thiamin_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(thiamin_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(thiamin_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Thiamin")
l<-ggplot(exp(fixef(riboflavin_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(riboflavin_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(riboflavin_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Riboflavin")
m<-ggplot(exp(fixef(iron_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(iron_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(iron_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Iron")
p<-ggplot(exp(fixef(VitE_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitE_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(VitE_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin E")
q<-ggplot(exp(fixef(calcium_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(calcium_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(calcium_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Calcium")
r<-ggplot(exp(fixef(VitA_RAE_model_fishnoreef, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitA_RAE_model_fishnoreef, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(VitA_RAE_model_fishnoreef, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin A")
windows()
annotate_figure(ggarrange(a,b,c,d,e,f,g,h,j,i,k,l,m,p,q,r,nrow=2,ncol=8,widths=c(2,1,1,1,1,1,1,1,1)), bottom="Proportion of RDA met from pelagic and other fish")


##now do for inverts contributions to adequacy#########################################################
hist(hhfoodsummary_indrda$hh_sodium_adeq_inverts)
sodium_model_inverts<-brm(hh_sodium_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(sodium_model_inverts)
hist(hhfoodsummary_indrda$hh_carbs_adeq_inverts)
carbs_model_inverts<-brm(hh_carbs_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(carbs_model_inverts)
hist(hhfoodsummary_indrda$hh_kcal_adequacy_inverts)
kcal_model_inverts<-brm(hh_kcal_adequacy_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(kcal_model_inverts)
hist(hhfoodsummary_indrda$hh_pr_adeq_inverts)
pr_model_inverts<-brm(hh_pr_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(pr_model_inverts)
hist(log(hhfoodsummary_indrda$hh_fat_adeq_inverts))
tfats_model_inverts<-brm(hh_fat_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(tfats_model_inverts)
hist(hhfoodsummary_indrda$hh_magnesium_adeq_inverts)
magnesium_model_inverts<-brm(hh_magnesium_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(magnesium_model_inverts)
hist(hhfoodsummary_indrda$hh_potassium_adeq_inverts)
potassium_model_inverts<-brm(hh_potassium_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(potassium_model_inverts)
hist(hhfoodsummary_indrda$hh_calcium_adeq_inverts)
calcium_model_inverts<-brm(hh_calcium_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(calcium_model_inverts)
hist(hhfoodsummary_indrda$hh_iron_adeq_inverts)
iron_model_inverts<-brm(hh_iron_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(iron_model_inverts)
hist(hhfoodsummary_indrda$hh_zinc_adeq_inverts)
zinc_model_inverts<-brm(hh_zinc_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(zinc_model_inverts)
hist(hhfoodsummary_indrda$hh_VA_adeq_inverts)
VitA_RAE_model_inverts<-brm(hh_VA_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitA_RAE_model_inverts)
hist(hhfoodsummary_indrda$hh_thiamin_adeq_inverts)
thiamin_model_inverts<-brm(hh_thiamin_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(thiamin_model_inverts)
hist(hhfoodsummary_indrda$hh_riboflavin_adeq_inverts)
riboflavin_model_inverts<-brm(hh_riboflavin_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(riboflavin_model_inverts)
hist(hhfoodsummary_indrda$hh_niacin_adeq_inverts)
niacin_model_inverts<-brm(hh_niacin_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(niacin_model_inverts)
hist(hhfoodsummary_indrda$hh_VB12_adeq_inverts)
Vit_B12_model_inverts<-brm(hh_VB12_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_B12_model_inverts)
hist(hhfoodsummary_indrda$hh_VC_adeq_inverts)
Vit_C_model_inverts<-brm(hh_VC_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_C_model_inverts)
hist(hhfoodsummary_indrda$hh_VE_adeq_inverts)
VitE_model_inverts<-brm(hh_VE_adeq_inverts~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitE_model_inverts)

#do figures
#global intercept
intercepts_global_inverts<-as.data.frame(cbind(rbind(exp(fixef(sodium_model_inverts)[,-2]), exp(fixef(carbs_model_inverts)[,-2]),exp(fixef(Vit_B12_model_inverts)[,-2]),exp(fixef(pr_model_inverts)[,-2]),exp(fixef(tfats_model_inverts)[,-2]),
                                                      exp(fixef(kcal_model_inverts)[,-2]),exp(fixef(niacin_model_inverts)[,-2]),exp(fixef(potassium_model_inverts)[,-2]),exp(fixef(magnesium_model_inverts)[,-2]),exp(fixef(zinc_model_inverts)[,-2]),
                                                      exp(fixef(thiamin_model_inverts)[,-2]),exp(fixef(riboflavin_model_inverts)[,-2]),exp(fixef(iron_model_inverts)[,-2]),exp(fixef(Vit_C_model_inverts)[,-2]),
                                                      exp(fixef(VitE_model_inverts)[,-2]),exp(fixef(calcium_model_inverts)[,-2]),exp(fixef(VitA_RAE_model_inverts)[,-2])),
                                                c("Sodium","Carbohydrates","Vitamin B12 (colbalamine)","Protein","Total fats","Kilocalories",
                                                  "Vitamin B3 (niacin)","Potasium","Magnesium","Zinc","Vitamin B1 (thiamin)","Vitamin B2 (riboflavin)","Total iron","Vitamin C",
                                                  "Vitamin E","Calcium","Vitamin A (RAE)")))
colnames(intercepts_global_inverts)<-c("Estimate","Q2.5","Q97.5","variable")
ggplot(intercepts_global_inverts,aes(x=as.numeric(Estimate),y=as.factor(variable),xmin=as.numeric(Q2.5),xmax=as.numeric(Q97.5)))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+theme_classic()+xlab("Nutrient adequacy (prop of RDA met)")+ylab("")

#random effects
ranef_community<-as.data.frame(ranef(sodium_model_inverts, groups="island:village", probs = c(0.1,0.9)))
ranef_community$community<-row.names(ranef_community)
colnames(ranef_community)<-c("estimate","se","conf.low","conf.high","community")
country_sodium_intercept=exp(fixef(sodium_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2])
ggplot(country_sodium_intercept,aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(sodium_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme_classic()+xlab("Estimated proportion of sodium RDA intake")+ylab("")

a<-ggplot(ranef_community,aes(x=estimate,y=community,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")

#island level random effects
ranef_island<-as.data.frame(ranef(sodium_model_inverts, groups="island", probs = c(0.1,0.9)))
ranef_island$island<-row.names(ranef_island)
colnames(ranef_island)<-c("estimate","se","conf.low","conf.high","island")

b<-ggplot(ranef_island,aes(x=estimate,y=island,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")
windows()
ggarrange(a,b,nrow=1,ncol=2)
#random effects fo all islands
a<-ggplot(exp(fixef(sodium_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(sodium_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Sodium")
b<-ggplot(exp(fixef(carbs_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(carbs_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(carbs_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Carbohydrates")
c<-ggplot(exp(fixef(Vit_B12_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(Vit_B12_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(Vit_B12_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin B12")
d<-ggplot(exp(fixef(pr_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(pr_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(pr_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Protein")
e<-ggplot(exp(fixef(tfats_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(tfats_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(tfats_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Total fats")
f<-ggplot(exp(fixef(kcal_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(kcal_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(kcal_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Kilocalories")
g<-ggplot(exp(fixef(niacin_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(niacin_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(niacin_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Niacin")
h<-ggplot(exp(fixef(potassium_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(potassium_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(potassium_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Potassium")
i<-ggplot(exp(fixef(magnesium_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(magnesium_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(magnesium_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Magnesium")
j<-ggplot(exp(fixef(zinc_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(zinc_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(zinc_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Zinc")
k<-ggplot(exp(fixef(thiamin_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(thiamin_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(thiamin_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Thiamin")
l<-ggplot(exp(fixef(riboflavin_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(riboflavin_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(riboflavin_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Riboflavin")
m<-ggplot(exp(fixef(iron_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(iron_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(iron_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Iron")
n<-ggplot(exp(fixef(Vit_C_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(Vit_C_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(Vit_C_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin C")
p<-ggplot(exp(fixef(VitE_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitE_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(VitE_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin E")
q<-ggplot(exp(fixef(calcium_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(calcium_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(calcium_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Calcium")
r<-ggplot(exp(fixef(VitA_RAE_model_inverts, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitA_RAE_model_inverts, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(VitA_RAE_model_inverts, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin A")
windows()
annotate_figure(ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,p,q,r,nrow=2,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Proportion of RDA met from inverts")


##now do for tinnedsf contributions to adequacy#########################################################
hist(hhfoodsummary_indrda$hh_sodium_adeq_tinnedsf)
sodium_model_tinnedsf<-brm(hh_sodium_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(sodium_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_carbs_adeq_tinnedsf)
carbs_model_tinnedsf<-brm(hh_carbs_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(carbs_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_kcal_adequacy_tinnedsf)
kcal_model_tinnedsf<-brm(hh_kcal_adequacy_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(kcal_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_pr_adeq_tinnedsf)
pr_model_tinnedsf<-brm(hh_pr_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(pr_model_tinnedsf)
hist(log(hhfoodsummary_indrda$hh_fat_adeq_tinnedsf))
tfats_model_tinnedsf<-brm(hh_fat_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(tfats_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_magnesium_adeq_tinnedsf)
magnesium_model_tinnedsf<-brm(hh_magnesium_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(magnesium_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_potassium_adeq_tinnedsf)
potassium_model_tinnedsf<-brm(hh_potassium_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(potassium_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_calcium_adeq_tinnedsf)
calcium_model_tinnedsf<-brm(hh_calcium_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(calcium_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_iron_adeq_tinnedsf)
iron_model_tinnedsf<-brm(hh_iron_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(iron_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_zinc_adeq_tinnedsf)
zinc_model_tinnedsf<-brm(hh_zinc_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(zinc_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_VA_adeq_tinnedsf)
VitA_RAE_model_tinnedsf<-brm(hh_VA_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitA_RAE_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_thiamin_adeq_tinnedsf)
thiamin_model_tinnedsf<-brm(hh_thiamin_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(thiamin_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_riboflavin_adeq_tinnedsf)
riboflavin_model_tinnedsf<-brm(hh_riboflavin_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(riboflavin_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_niacin_adeq_tinnedsf)
niacin_model_tinnedsf<-brm(hh_niacin_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(niacin_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_VB12_adeq_tinnedsf)
Vit_B12_model_tinnedsf<-brm(hh_VB12_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_B12_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_VC_adeq_tinnedsf)
#Vit_C_model_tinnedsf<-brm(hh_VC_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
#pp_check(Vit_C_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_VE_adeq_tinnedsf)
VitE_model_tinnedsf<-brm(hh_VE_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitE_model_tinnedsf)
hist(hhfoodsummary_indrda$hh_fibre_adeq_tinnedsf)
fibre_model_tinnedsf<-brm(hh_fibre_adeq_tinnedsf~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(fibre_model_tinnedsf)

#do figures
#global intercept
intercepts_global_tinnedsf<-as.data.frame(cbind(rbind(exp(fixef(sodium_model_tinnedsf)[,-2]), exp(fixef(carbs_model_tinnedsf)[,-2]),exp(fixef(Vit_B12_model_tinnedsf)[,-2]),exp(fixef(pr_model_tinnedsf)[,-2]),exp(fixef(tfats_model_tinnedsf)[,-2]),
                                                     exp(fixef(kcal_model_tinnedsf)[,-2]),exp(fixef(niacin_model_tinnedsf)[,-2]),exp(fixef(potassium_model_tinnedsf)[,-2]),exp(fixef(magnesium_model_tinnedsf)[,-2]),exp(fixef(zinc_model_tinnedsf)[,-2]),
                                                     exp(fixef(thiamin_model_tinnedsf)[,-2]),exp(fixef(riboflavin_model_tinnedsf)[,-2]),exp(fixef(iron_model_tinnedsf)[,-2]),exp(fixef(fibre_model_tinnedsf)[,-2]),
                                                     exp(fixef(VitE_model_tinnedsf)[,-2]),exp(fixef(calcium_model_tinnedsf)[,-2]),exp(fixef(VitA_RAE_model_tinnedsf)[,-2])),
                                               c("Sodium","Carbohydrates","Vitamin B12 (colbalamine)","Protein","Total fats","Kilocalories",
                                                 "Vitamin B3 (niacin)","Potasium","Magnesium","Zinc","Vitamin B1 (thiamin)","Vitamin B2 (riboflavin)","Total iron","Fiber",
                                                 "Vitamin E","Calcium","Vitamin A (RAE)")))
colnames(intercepts_global_tinnedsf)<-c("Estimate","Q2.5","Q97.5","variable")
ggplot(intercepts_global_tinnedsf,aes(x=as.numeric(Estimate),y=as.factor(variable),xmin=as.numeric(Q2.5),xmax=as.numeric(Q97.5)))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+theme_classic()+xlab("Nutrient adequacy (prop of RDA met)")+ylab("")

#random effects
ranef_community<-as.data.frame(ranef(sodium_model_tinnedsf, groups="island:village", probs = c(0.1,0.9)))
ranef_community$community<-row.names(ranef_community)
colnames(ranef_community)<-c("estimate","se","conf.low","conf.high","community")
country_sodium_intercept=exp(fixef(sodium_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2])
ggplot(country_sodium_intercept,aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(sodium_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme_classic()+xlab("Estimated proportion of sodium RDA intake")+ylab("")

a<-ggplot(ranef_community,aes(x=estimate,y=community,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")

#island level random effects
ranef_island<-as.data.frame(ranef(sodium_model_tinnedsf, groups="island", probs = c(0.1,0.9)))
ranef_island$island<-row.names(ranef_island)
colnames(ranef_island)<-c("estimate","se","conf.low","conf.high","island")

b<-ggplot(ranef_island,aes(x=estimate,y=island,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")
windows()
ggarrange(a,b,nrow=1,ncol=2)
#random effects fo all islands
a<-ggplot(exp(fixef(sodium_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(sodium_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Sodium")
b<-ggplot(exp(fixef(carbs_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(carbs_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(carbs_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Carbohydrates")
c<-ggplot(exp(fixef(Vit_B12_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(Vit_B12_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(Vit_B12_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin B12")
d<-ggplot(exp(fixef(pr_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(pr_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(pr_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Protein")
e<-ggplot(exp(fixef(tfats_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(tfats_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(tfats_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Total fats")
f<-ggplot(exp(fixef(kcal_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(kcal_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(kcal_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Kilocalories")
g<-ggplot(exp(fixef(niacin_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(niacin_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(niacin_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Niacin")
h<-ggplot(exp(fixef(potassium_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(potassium_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(potassium_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Potassium")
i<-ggplot(exp(fixef(magnesium_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(magnesium_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(magnesium_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Magnesium")
j<-ggplot(exp(fixef(zinc_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(zinc_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(zinc_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Zinc")
k<-ggplot(exp(fixef(thiamin_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(thiamin_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(thiamin_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Thiamin")
l<-ggplot(exp(fixef(riboflavin_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(riboflavin_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(riboflavin_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Riboflavin")
m<-ggplot(exp(fixef(iron_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(iron_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(iron_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Iron")
n<-ggplot(exp(fixef(fibre_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(fibre_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(fibre_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Fiber")
p<-ggplot(exp(fixef(VitE_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitE_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(VitE_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin E")
q<-ggplot(exp(fixef(calcium_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(calcium_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(calcium_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Calcium")
r<-ggplot(exp(fixef(VitA_RAE_model_tinnedsf, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitA_RAE_model_tinnedsf, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(VitA_RAE_model_tinnedsf, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin A")
windows()
annotate_figure(ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,p,q,r,nrow=2,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Proportion of RDA met from tinnedsf")


##now do for seaweed contributions to adequacy#########################################################
(hhfoodsummary_indrda$hh_sodium_adeq_seaweed)
sodium_model_seaweed<-brm(hh_sodium_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(sodium_model_seaweed)
hist(hhfoodsummary_indrda$hh_carbs_adeq_seaweed)
#carbs_model_seaweed<-brm(hh_carbs_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
#pp_check(carbs_model_seaweed)
hist(hhfoodsummary_indrda$hh_kcal_adequacy_seaweed)
kcal_model_seaweed<-brm(hh_kcal_adequacy_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(kcal_model_seaweed)
hist(hhfoodsummary_indrda$hh_pr_adeq_seaweed)
pr_model_seaweed<-brm(hh_pr_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(pr_model_seaweed)
hist(log(hhfoodsummary_indrda$hh_fat_adeq_seaweed))
tfats_model_seaweed<-brm(hh_fat_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(tfats_model_seaweed)
summary(hhfoodsummary_indrda$hh_fibre_adeq_seaweed)
fibre_model_seaweed<-brm(hh_fibre_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(fibre_model_seaweed)
hist(hhfoodsummary_indrda$hh_magnesium_adeq_seaweed)
magnesium_model_seaweed<-brm(hh_magnesium_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(magnesium_model_seaweed)
hist(hhfoodsummary_indrda$hh_potassium_adeq_seaweed)
potassium_model_seaweed<-brm(hh_potassium_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(potassium_model_seaweed)
hist(hhfoodsummary_indrda$hh_calcium_adeq_seaweed)
calcium_model_seaweed<-brm(hh_calcium_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(calcium_model_seaweed)
hist(hhfoodsummary_indrda$hh_iron_adeq_seaweed)
iron_model_seaweed<-brm(hh_iron_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(iron_model_seaweed)
hist(hhfoodsummary_indrda$hh_zinc_adeq_seaweed)
zinc_model_seaweed<-brm(hh_zinc_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(zinc_model_seaweed)
hist(hhfoodsummary_indrda$hh_VA_adeq_seaweed)
VitA_RAE_model_seaweed<-brm(hh_VA_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitA_RAE_model_seaweed)
hist(hhfoodsummary_indrda$hh_thiamin_adeq_seaweed)
thiamin_model_seaweed<-brm(hh_thiamin_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(thiamin_model_seaweed)
hist(hhfoodsummary_indrda$hh_riboflavin_adeq_seaweed)
riboflavin_model_seaweed<-brm(hh_riboflavin_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(riboflavin_model_seaweed)
hist(hhfoodsummary_indrda$hh_niacin_adeq_seaweed)
niacin_model_seaweed<-brm(hh_niacin_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(niacin_model_seaweed)
hist(hhfoodsummary_indrda$hh_VB12_adeq_seaweed)
Vit_B12_model_seaweed<-brm(hh_VB12_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_B12_model_seaweed)
hist(hhfoodsummary_indrda$hh_VC_adeq_seaweed)
Vit_C_model_seaweed<-brm(hh_VC_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(Vit_C_model_seaweed)
hist(hhfoodsummary_indrda$hh_VE_adeq_seaweed)
VitE_model_seaweed<-brm(hh_VE_adeq_seaweed~1+ (1|island/village),data=hhfoodsummary_indrda, family="hurdle_lognormal")
pp_check(VitE_model_seaweed)

#do figures
#global intercept
intercepts_global_seaweed<-as.data.frame(cbind(rbind(exp(fixef(sodium_model_seaweed)[,-2]), exp(fixef(carbs_model_seaweed)[,-2]),exp(fixef(Vit_B12_model_seaweed)[,-2]),exp(fixef(pr_model_seaweed)[,-2]),exp(fixef(tfats_model_seaweed)[,-2]),
                                                      exp(fixef(kcal_model_seaweed)[,-2]),exp(fixef(niacin_model_seaweed)[,-2]),exp(fixef(potassium_model_seaweed)[,-2]),exp(fixef(magnesium_model_seaweed)[,-2]),exp(fixef(zinc_model_seaweed)[,-2]),
                                                      exp(fixef(thiamin_model_seaweed)[,-2]),exp(fixef(riboflavin_model_seaweed)[,-2]),exp(fixef(iron_model_seaweed)[,-2]),exp(fixef(Vit_C_model_seaweed)[,-2]),
                                                      exp(fixef(VitE_model_seaweed)[,-2]),exp(fixef(calcium_model_seaweed)[,-2]),exp(fixef(VitA_RAE_model_seaweed)[,-2])),
                                                c("Sodium","Carbohydrates","Vitamin B12 (colbalamine)","Protein","Total fats","Kilocalories",
                                                  "Vitamin B3 (niacin)","Potasium","Magnesium","Zinc","Vitamin B1 (thiamin)","Vitamin B2 (riboflavin)","Total iron","Vitamin C",
                                                  "Vitamin E","Calcium","Vitamin A (RAE)")))
colnames(intercepts_global_seaweed)<-c("Estimate","Q2.5","Q97.5","variable")
ggplot(intercepts_global_seaweed,aes(x=as.numeric(Estimate),y=as.factor(variable),xmin=as.numeric(Q2.5),xmax=as.numeric(Q97.5)))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+theme_classic()+xlab("Nutrient adequacy (prop of RDA met)")+ylab("")

#random effects
ranef_community<-as.data.frame(ranef(sodium_model_seaweed, groups="island:village", probs = c(0.1,0.9)))
ranef_community$community<-row.names(ranef_community)
colnames(ranef_community)<-c("estimate","se","conf.low","conf.high","community")
country_sodium_intercept=exp(fixef(sodium_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2])
ggplot(country_sodium_intercept,aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 1)+geom_vline(xintercept = exp(fixef(sodium_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme_classic()+xlab("Estimated proportion of sodium RDA intake")+ylab("")

a<-ggplot(ranef_community,aes(x=estimate,y=community,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")

#island level random effects
ranef_island<-as.data.frame(ranef(sodium_model_seaweed, groups="island", probs = c(0.1,0.9)))
ranef_island$island<-row.names(ranef_island)
colnames(ranef_island)<-c("estimate","se","conf.low","conf.high","island")

b<-ggplot(ranef_island,aes(x=estimate,y=island,xmin=conf.low,xmax=conf.high))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = 0,lty=2)+theme_classic()+xlab("Offset from intercept")
windows()
ggarrange(a,b,nrow=1,ncol=2)
#random effects fo all islands
a<-ggplot(exp(fixef(sodium_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(sodium_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(sodium_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Sodium")
b<-ggplot(exp(fixef(carbs_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(carbs_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(carbs_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Carbohydrates")
c<-ggplot(exp(fixef(Vit_B12_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(Vit_B12_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(Vit_B12_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin B12")
d<-ggplot(exp(fixef(pr_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(pr_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(pr_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Protein")
e<-ggplot(exp(fixef(tfats_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(tfats_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(tfats_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Total fats")
f<-ggplot(exp(fixef(kcal_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(kcal_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(kcal_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Kilocalories")
g<-ggplot(exp(fixef(niacin_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(niacin_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(niacin_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Niacin")
h<-ggplot(exp(fixef(potassium_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(potassium_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(potassium_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Potassium")
i<-ggplot(exp(fixef(magnesium_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(magnesium_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(magnesium_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Magnesium")
j<-ggplot(exp(fixef(zinc_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(zinc_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(zinc_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Zinc")
k<-ggplot(exp(fixef(thiamin_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(thiamin_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(thiamin_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Thiamin")
l<-ggplot(exp(fixef(riboflavin_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(riboflavin_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(riboflavin_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Riboflavin")
m<-ggplot(exp(fixef(iron_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(iron_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(iron_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Iron")
n<-ggplot(exp(fixef(Vit_C_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(Vit_C_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(Vit_C_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin C")
p<-ggplot(exp(fixef(VitE_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitE_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(VitE_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin E")
q<-ggplot(exp(fixef(calcium_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(calcium_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(calcium_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Calcium")
r<-ggplot(exp(fixef(VitA_RAE_model_seaweed, probs = c(0.1,0.9))[,-2]+as.data.frame(ranef(VitA_RAE_model_seaweed, groups="island", probs = c(0.1,0.9)))[,-2]),aes(x=island.Estimate.Intercept,y=rownames(country_sodium_intercept),xmin=island.Q10.Intercept,xmax=island.Q90.Intercept))+geom_errorbar()+geom_point(fill="darkred",pch=21,size=3)+geom_vline(xintercept = exp(fixef(VitA_RAE_model_seaweed, probs = c(0.1,0.9))[1]),lty=2)+theme(axis.text.y = element_blank(),panel.background = element_rect(color="black",fill="white"))+xlab("")+ylab("")+ggtitle("Vitamin A")
windows()
annotate_figure(ggarrange(a,b,c,d,e,f,g,h,i,j,k,l,m,n,p,q,r,nrow=2,ncol=9,widths=c(2,1,1,1,1,1,1,1,1,1)), bottom="Proportion of RDA met from seaweed")

#model fits
#overall adequacy______________________________________________________________
annotate_figure(ggarrange(pp_check(sodium_model)+guides(col=F)+ggtitle("Sodium"),
          pp_check(carbs_model)+guides(col=F)+ggtitle("Carbohydrates"),
          pp_check(kcal_model)+guides(col=F)+ggtitle("Kilocalories"),
          pp_check(pr_model)+guides(col=F)+ggtitle("Protein"),
          pp_check(tfats_model)+guides(col=F)+ggtitle("Total fats"),
          pp_check(fibre_model)+guides(col=F)+ggtitle("Fiber"),
          pp_check(magnesium_model)+guides(col=F)+ggtitle("Magnesium"),
          pp_check(potassium_model)+guides(col=F)+ggtitle("Potassium"),
          pp_check(calcium_model)+ggtitle("Calcium"),
          pp_check(iron_model)+guides(col=F)+ggtitle("Iron"),
          pp_check(zinc_model)+guides(col=F)+ggtitle("Zinc"),
          pp_check(VitA_RAE_model)+guides(col=F)+ggtitle("Vitamin A"),
          pp_check(thiamin_model)+guides(col=F)+ggtitle("Thiamin"),
          pp_check(riboflavin_model)+guides(col=F)+ggtitle("Riboflavin"),
          pp_check(niacin_model)+guides(col=F)+ggtitle("Niacin"),
          pp_check(Vit_B12_model)+guides(col=F)+ggtitle("Vitamin B12"),
          pp_check(Vit_C_model)+guides(col=F)+ggtitle("Vitamin C"),
          pp_check(VitE_model)+ggtitle("Vitamin E"), nrow=2,ncol=9, widths=c(rep(1,8),1.5)), bottom=c("Household adequacy (Proportion of RDA met from all foods)"))


annotate_figure(ggarrange(pp_check(sodium_model_seafood)+guides(col=F)+ggtitle("Sodium"),
                          pp_check(carbs_model_seafood)+guides(col=F)+ggtitle("Carbohydrates"),
                          pp_check(kcal_model_seafood)+guides(col=F)+ggtitle("Kilocalories"),
                          pp_check(pr_model_seafood)+guides(col=F)+ggtitle("Protein"),
                          pp_check(tfats_model_seafood)+guides(col=F)+ggtitle("Total fats"),
                          pp_check(fibre_model_seafood)+guides(col=F)+ggtitle("Fiber"),
                          pp_check(magnesium_model_seafood)+guides(col=F)+ggtitle("Magnesium"),
                          pp_check(potassium_model_seafood)+guides(col=F)+ggtitle("Potassium"),
                          pp_check(calcium_model_seafood)+ggtitle("Calcium"),
                          pp_check(iron_model_seafood)+guides(col=F)+ggtitle("Iron"),
                          pp_check(zinc_model_seafood)+guides(col=F)+ggtitle("Zinc"),
                          pp_check(VitA_RAE_model_seafood)+guides(col=F)+ggtitle("Vitamin A"),
                          pp_check(thiamin_model_seafood)+guides(col=F)+ggtitle("Thiamin"),
                          pp_check(riboflavin_model_seafood)+guides(col=F)+ggtitle("Riboflavin"),
                          pp_check(niacin_model_seafood)+guides(col=F)+ggtitle("Niacin"),
                          pp_check(Vit_B12_model_seafood)+guides(col=F)+ggtitle("Vitamin B12"),
                          pp_check(Vit_C_model_seafood)+guides(col=F)+ggtitle("Vitamin C"),
                          pp_check(VitE_model_seafood)+ggtitle("Vitamin E"), nrow=2,ncol=9, widths=c(rep(1,8),1.5)), bottom=c("Household adequacy (Proportion of RDA met from seafood)"))


#write.csv(hhfoodsummary,"HHfoodrecallestimates.csv",row.names=F)
#save.image(file='kiribati_seafood.RData')
#load(file='kiribati_seafood.RData')

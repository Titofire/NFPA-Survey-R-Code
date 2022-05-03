#### SBFD 2021 NFPA survey ####
#### Load packages 
library(tidyverse)


NFPASurvey2021.df <- read.csv("Pivotpropuse2021.csv")
View(NFPASurvey2021.df)
str(NFPASurvey2021.df)

#################  PART III   Breakdown of Structure Fires and Other Fires and Incidents   ########################################

NFPA_Filtered.df <- NFPASurvey2021.df %>%
  select(inci_no,alm_date,number,street,city,inci_type,fatal_civ,inj_civ,loss_total,prop_loss,cont_loss,prop_use) %>%
  #drop_na(ASSESSED.VALUE) %>%
  filter(inci_type %in% c( 110:129))
  View(NFPA_Filtered.df)
  
  #### Privite Dwellings  ####
  NFPA_Filtered1.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% 419, mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered1.df)
  
  #### Apartments  ####
  NFPA_Filtered2.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% 429,mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered2.df)
  
  #### Hotels and Motels  ####
  NFPA_Filtered3.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% 449,mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered3.df)
  
  #### All Other Residential   ####
  NFPA_Filtered4.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% c(400,439,459:499),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered4.df)
  
  #### Public Assembly   ####
  NFPA_Filtered6.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% c(100:199),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered6.df)
  
  #### Schools and Colleges   ####
  NFPA_Filtered7.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% c(200:299),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered7.df)
  
  #### Healthcare   ####
  NFPA_Filtered8.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% c(300:399),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered8.df)
  
  #### Stores and Offices   ####
  NFPA_Filtered9.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% c(500:599),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered9.df)
  
  #### Industry Utiity Defense Laboratories Manufacturing   ####
  NFPA_Filtered10.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% c(600:799),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered10.df)
  
  #### Storage in Structures  ####
  NFPA_Filtered11.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% c(800:899),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered11.df)
  
  #### Other Structures   ####
  NFPA_Filtered12.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(110:129), prop_use %in% c(900:999),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered12.df)
  
  #### Fires in Highway Vehicles  ####
  NFPA_Filtered14a.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(131,132,136:137),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered14a.df)
  
  #### Fires in Other Vehicles####
  NFPA_Filtered14b.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(130,133:135,138),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered14b.df)
  
  #### Fires outside of structures with value involved, not vehicles   ####
  NFPA_Filtered15.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(140,141,161:162,164,170:173),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered15.df)
  
  #### Fires in Brush,Grass, Wildland   ####
  NFPA_Filtered16.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(142,143),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered16.df)
  
  #### Fires In Rubbish   ####
  NFPA_Filtered17.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(150:155),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered17.df)
  
  
#### All other Fires Incident Type 100,160,163####
NFPA_Filtered18.df <- NFPASurvey2021.df %>%
  select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,loss_total,prop_loss,cont_loss,prop_use) %>%
  #drop_na(ASSESSED.VALUE) %>%
  filter(inci_type %in% c(100,160,163),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered18.df)

#### Rescue, EMS responses (in the city) Incident Type 300:381 ####
NFPA_Filtered20.df <- NFPASurvey2021.df %>%
  select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
  #drop_na(ASSESSED.VALUE) %>%
  filter(inci_type %in% c(300:381),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered20.df)
  
#### False alarm responses (in the city) inci_type 700-751####
  NFPA_Filtered21.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(700:751),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered21.df)
  
  #### All mutlaid responses GIVEN ####
  NFPA_Filtered22.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(mutl_aid %in% c(3,4,5))
  View(NFPA_Filtered22.df)
  
  #### Hazmat Materials Responses ####
  NFPA_Filtered23a.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(410:431),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered23a.df)
  
  #### Hazmat Materials Responses other inci_type 440:480,400 ####
  NFPA_Filtered23b.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(440:480,400),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered23b.df)

  #### All other responses  ####
  NFPA_Filtered24.df <- NFPASurvey2021.df %>%
    select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
    #drop_na(ASSESSED.VALUE) %>%
    filter(inci_type %in% c(200:251,500:699,800:911),mutl_aid %in% c("N",1,2,16))
  View(NFPA_Filtered24.df)
  
  ####  Confined, Non- Confined Fires in Part III  #########################################################################
  
      #### Section 3, confined fires in lines 1-5 (residential Prop_use 419,429,449,400,459:499), inci_type 113:118,  ####
      NFPA_Filtered25.df <- NFPASurvey2021.df %>%
        select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
        #drop_na(ASSESSED.VALUE) %>%
        filter(inci_type %in% c(113:118),mutl_aid %in% c("N",1,2,16),prop_use %in% c(419,429,449,400,459:499))
      View(NFPA_Filtered25.df)
      
      #### Section 3, non-confined fires in lines 1-5 (residential Prop_use 419,429,449,400,459:499), inci_type 110:112,120:123 ,  ####
      NFPA_Filtered26.df <- NFPASurvey2021.df %>%
        select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
        #drop_na(ASSESSED.VALUE) %>%
        filter(inci_type %in% c(110:112,120:123),mutl_aid %in% c("N",1,2,16),prop_use %in% c(419,429,449,400,459:499))
      View(NFPA_Filtered26.df)
      
      #### Section 3, non-confined fires in lines 1-13 (All Prop_use 100:199,200:299,300:399,419,429,449,400,459:499,500:599,600:799,800:899,900:999), inci_type 110:112,120:123 ,  ####
      NFPA_Filtered27.df <- NFPASurvey2021.df %>%
        select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
        #drop_na(ASSESSED.VALUE) %>%
        filter(inci_type %in% c(113:118),mutl_aid %in% c("N",1,2,16),prop_use %in% c(100:199,200:299,300:399,419,429,449,400,459:499,500:599,600:799,800:899,900:999))
      View(NFPA_Filtered27.df)
      
      #### Section 3, non-confined fires in lines 1-13 (All Prop_use 100:199,200:299,300:399,419,429,449,400,459:499,500:599,600:799,800:899,900:999), inci_type 110:112,120:123 ,  ####
      NFPA_Filtered28.df <- NFPASurvey2021.df %>%
        select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
        #drop_na(ASSESSED.VALUE) %>%
        filter(inci_type %in% c(110:112,120:123),mutl_aid %in% c("N",1,2,16),prop_use %in% c(100:199,200:299,300:399,419,429,449,400,459:499,500:599,600:799,800:899,900:999))
      View(NFPA_Filtered28.df)
      
  #################  PART IV  Breakdown of False Alarm Responses   ########################################
  
      #### Malicious, Mischievous False Call (Inci_type 710:715) ####
      NFPA_Filtered29.df <- NFPASurvey2021.df %>%
        select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
        #drop_na(ASSESSED.VALUE) %>%
        filter(inci_type %in% c(710:715),mutl_aid %in% c("N",1,2,16))
      View(NFPA_Filtered29.df)
      
      #### System Malfunction (Inci_type 730:739) ####
      NFPA_Filtered30.df <- NFPASurvey2021.df %>%
        select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
        #drop_na(ASSESSED.VALUE) %>%
        filter(inci_type %in% c(730:739),mutl_aid %in% c("N",1,2,16))
      View(NFPA_Filtered30.df)
      
      #### Unintentional (tripping on interior devide accidentally, etc.(Inci_type 740:749)) ####
      NFPA_Filtered31.df <- NFPASurvey2021.df %>%
        select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
        #drop_na(ASSESSED.VALUE) %>%
        filter(inci_type %in% c(740:749),mutl_aid %in% c("N",1,2,16))
      View(NFPA_Filtered31.df)
      
      #### Other False Alarms (bomb scares, etc)(Inci_type 700,721,751)) ####
      NFPA_Filtered32.df <- NFPASurvey2021.df %>%
        select(inci_no,number,street,city,inci_type,fatal_civ,inj_civ,mutl_aid,loss_total,prop_loss,cont_loss,prop_use) %>%
        #drop_na(ASSESSED.VALUE) %>%
        filter(inci_type %in% c(700,721,751),mutl_aid %in% c("N",1,2,16))
      View(NFPA_Filtered32.df)
      
   ####  PART V Intentionally Set Fires in Structures and Vehicles   #####
      
      
      
# #############################################################################
# This is the script to analyse the data from SP Megacity dataframe
# The aim is to classify substance users into 5 categories according to (cite report here).
# Author : Henrique Pinto Gomide, henriquepgomide@gmail.com
# License: MIT License. You are free to re-use the code, but cite me.
# Have fun :) #################################################################


# Dica - O banco deve ser aberto no formato *.csv. O banco também pode ser aberto na extensão sas7bat **sem compressão**. Link para pacote que faz a mágica: https://github.com/BioStatMatt/sas7bdat

# Open data
megaCity  <- read.csv("data.csv")

# ----------------
# Data Mugling----
# ----------------

## Create new variables ----

## Criar pelos seguintes cortes: 18-34, 35-49, 50-64, 65+

## Alcohol criteria ----

# 1 .  Recode Variables into binary
megaCity$su12m   <- ifelse(megaCity$su12  == 1, 1, 0)
megaCity$su12abm  <- ifelse(megaCity$su12a == 1 & megaCity$su12b == 1, 1, 0)
megaCity$su12cm  <- ifelse(megaCity$su12c == 1, 1, 0)
megaCity$su12dm  <- ifelse(megaCity$su12d == 1, 1, 0)
megaCity$su19m   <- ifelse(megaCity$su19  == 1, 1, 0) 
megaCity$su19am  <- ifelse(megaCity$su19a == 1, 1, 0) 
megaCity$su19bcm  <- ifelse(megaCity$su19b == 1 & megaCity$su19c == 1, 1, 0)
megaCity$su19defm  <- ifelse(megaCity$su19d == 1 & megaCity$su19e == 1 & megaCity$su19f == 1, 1, 0)
megaCity$su19gm  <- ifelse(megaCity$su19g == 1, 1, 0) 
megaCity$su19hm  <- ifelse(megaCity$su19h == 1, 1, 0)
megaCity$su19im  <- ifelse(megaCity$su19i == 1, 1, 0)
megaCity$su19jm  <- ifelse(megaCity$su19j == 1, 1, 0)


# 2. Create var dsm5 which is the sum of the presence/ausence of AUD symptoms
megaCity$dsm5a  <- rowSums(megaCity, su12m, su12abm, su12cm, su12dm, su19m, su19am, su19bcm, su19defm, su19gm, su19hm, su19im, su19jm)


## Substance criteria ----

## Substance disorders criteria ----
### Recode - * Check criteria

megaCity$su65m   <- ifelse(megaCity$su65 ==  1, 1, 0)
megaCity$su65abdm  <- ifelse(megaCity$su65a == 1 & megaCity$su12b == 1 & megaCity$su12d == 1 , 1, 0)
megaCity$su65cm  <- ifelse(megaCity$su65c == 1, 1, 0)
megaCity$su72em  <- ifelse(megaCity$su72e == 1, 1, 0)
megaCity$su72dfm   <- ifelse(megaCity$su72d ==  1 & megaCity$su72f ==  1 , 1, 0)
megaCity$su72gm  <- ifelse(megaCity$su72g == 1, 1, 0)
megaCity$su72m  <- ifelse(megaCity$su72 == 1, 1, 0)
megaCity$su72hm  <- ifelse(megaCity$su72h == 1, 1, 0)
megaCity$su72im  <- ifelse(megaCity$su72i == 1, 1, 0)
megaCity$su72am  <- ifelse(megaCity$su72a == 1, 1, 0)
megaCity$su72bcm  <- ifelse(megaCity$su72b == 1 & megaCity$su72c == 1, 1, 0)

## Soma das variáveis de dependência de substâncias
dsm5s  <- rowSums(megaCity, su65m, su65abdm, su65cm, su72em, su72dfm, su72gm, su72m, su72hm, su72im, su72am, su72bcm)


## Mental disorders criteria ----
# OBS - Para recodificação funcionar, dependemos da classificação das variaveis feitas no SAS. Os nomes delas são megaCity$d_mddh12; megaCity$d_dysh12; megaCity$d_pds12; megaCity$d_sp12; megaCity$d_so12; megaCity$d_agp12; megaCity$d_gadh12; megaCity$d_iedh12; megaCity$d_oddh12; megaCity$d_sad12; megaCity$d_add12; megaCity$d_bip12; d_ocd12

# Major depressive disorder
megaCity$depression <- ifelse(megaCity$d_mddh12 & (megaCity$d66a >= 4 | megaCity$d66b >= 4  | megaCity$d66c >= 4  | megaCity$d66d >= 4), 1,0)

# Dysthymia
megaCity$dysthymia <- ifelse(megaCity$d_dysh12 & (megaCity$d66a >= 4   | megaCity$d66b >= 4   | megaCity$d66c >= 4   | megaCity$d66d >= 4), 1,0)

# Bipolar Disorder (Broad)
megaCity$bipolar <- ifelse(megaCity$d_bip12  & (megaCity$m27a >= 4   | megaCity$m27b >= 4   | megaCity$m27c >= 4   | megaCity$m27d >= 4), 1,0)

# Panic disorder
megaCity$panic <- ifelse(megaCity$d_bip12  & (megaCity$m27a >= 4   | megaCity$m27b >= 4   | megaCity$m27c >= 4   | megaCity$m27d >= 4), 1,0)

# Specific phobia
megaCity$sphobia <- ifelse(megaCity$d_sp12 & (megaCity$sp23a >= 4  | megaCity$sp23b >= 4  | megaCity$sp23c >= 4  | megaCity$sp23d >= 4), 1,0)

# Social phobia
megaCity$sophobia <- ifelse(megaCity$d_so12   & (megaCity$so21a >= 4  | megaCity$so21b >= 4  | megaCity$so21c >= 4  | megaCity$so21d >= 4), 1,0)

# Agoraphobia without panic
megaCity$agoraphobia <- ifelse(megaCity$d_agp12  & (megaCity$ag20a >= 4  | megaCity$ag20b >= 4  | megaCity$ag20c >= 4  | megaCity$ag20d >= 4), 1,0)

# Generalized anxiety disorder
megaCity$ganxiety <- ifelse(megaCity$d_gadh12 & (megaCity$g38a >= 4   | megaCity$g38b >= 4   | megaCity$g38c >= 4   | megaCity$g38d >= 4), 1,0)

# Intermittent explosive disorder
megaCity$ied <- ifelse(megaCity$d_iedh12 & (megaCity$ied26a >= 4 | megaCity$ied26b >= 4 | megaCity$ied26c >= 4 | megaCity$ied26d >= 4), 1,0)

# Oppositional-defiant disorder
megaCity$oddh <- ifelse(megaCity$d_oddh12 & (megaCity$od38a >= 4  | megaCity$od38b >= 4  | megaCity$od38c >= 4  | megaCity$od38d >= 4), 1,0)

# Conduct disorder
megaCity$cd12 <- ifelse(megaCity$cd12 == 1, 1,0)

# Separation anxiety disorder
megaCity$sad <- ifelse(megaCity$d_sad12  & (megaCity$sa25a >= 4  | megaCity$sa25b >= 4  | megaCity$sa25c >= 4  | megaCity$sa25d >= 4), 1,0)

# ADHD
megaCity$add <- ifelse(megaCity$d_add12  & (megaCity$ad48a >= 4  | megaCity$ad48b >= 4  | megaCity$ad48c >= 4  | megaCity$ad48d >= 4), 1,0)

# Obsessive-compulsive disorder
megaCity$ocd <- ifelse(megaCity$d_ocd12  & (megaCity$o49a >= 4  | megaCity$o49b >= 4  | megaCity$o49c >= 4  | megaCity$o49d >= 4), 1,0)

# Post-traumatic stress disorder 
# OBS - Verificar porque não há escala de interferência.
megaCity$ptsd <- ifelse(megaCity$d_pts12  == 1 , 1,0)


# Soma dos transtornos mentais
megaCity$sumMentalDis  <- megaCity$depression + megaCity$dysthymia + megaCity$bipolar + megaCity$panic + megaCity$sphobia + megaCity$sophobia +  megaCity$agoraphobia + megaCity$ganxiety + megaCity$ied + megaCity$oddh + megaCity$cd12 + megaCity$sad + megaCity$add + megaCity$ocd + megaCity$ptsd


# ----------------
# Pyramid one ----
# ----------------

## Tier A - Low risk or abstainers
megaCity$P1tierA  <- ifelse( 
  ((megaCity$su3 == 6) | (megaCity$su3 == 5 & megaCity$su9.1 == 0 & megaCity$dsm5a == 0))  &  
  ((megaCity$su55c == 5 & megaCity$su56c == 5 & megaCity$su57c == 5 ) | (megaCity$su55d == 5 & megaCity$su56d == 5 & megaCity$su57d == 5  & megaCity$dsm5s == 0)) , 
  "tierA", "Not tierA")

## Tier B1 - Risky ou heavy users
megaCity$P1tierB1  <- ifelse( 
  (megacity$su3 < 5 & megacity$su9.1 > 0 & megacity$dsm5a == 0) | 
  ((megacity$su55d < 5 | megacity$su56d < 5 | megacity$su57d < 5) & megacity$dsm5s == 0), 
  "tierB1", "Not tierB1")

## Tier B.2 - Risky ou heavy users with mild disorders
megaCity$P1tierB2  <- ifelse( 
  (megacity$su3 < 5 & megacity$su9.1 > 0 & megacity$dsm5a == 1) | 
  (megacity$su55d < 5 | megacity$su56d < 5 | megacity$su57d < 5 & megacity$dsm5s == 1)
  , "tierB2", "Not tierB2")

## Tier C - Users with alcohol or substance use disorders
megaCity$P1tierC  <- ifelse(
  (megaCity$dsm5a >= 2 & megaCity$su3 <=5 & megaCity$su5 ==5 & megaCity$sr122 == 5 & megaCity$su103 = 5 & (megaCity$su39a < 4 & megaCity$su39b < 4 & megaCity$su39c < 4 & megaCity$su39d < 4)) | 
  (megaCity$dsm5s >= 2 & megaCity$su69 < 4 & megaCity$sr122 == 5 & megaCity$su103 == 5 & (megaCity$su86f.1 < 4 & megaCity$su86f.2 < 4 & megaCity$su86f.3 < 4 & megaCity$su86f.4 < 4)), 
  "tierC", "Not tierC")

## Tier D - Users with severe disorders
megaCity$P1tierD  <- ifelse(
  (megaCity$dsm5a >= 2 &  megaCity$su3 <=5 & megaCity$su5 ==5 & (megaCity$sr122 == 1 | megaCity$su103 == 1 | megaCity$su39a >= 4 | megaCity$su39b >= 4 | megaCity$su39c >= 4 | megaCity$su39d >= 4)) |
  (megaCity$dsm5s >= 2 & megaCity$su69 < 4 & (megaCity$sr122 == 1 | megaCity$su103 == 1 | megaCity$su86f.1 >= 4 | megaCity$su86f.2 >= 4 | megaCity$su86f.3 >= 4 | megaCity$su86f.4 >= 4)), 
  "tierD", 'Not tierD')

## Tier E - Comorbity
# OBS - Dependemos da classificação das variaveis feitas no SAS. Os nomes delas são megaCity$d_mddh12; megaCity$d_dysh12; megaCity$d_pds12; megaCity$d_sp12; megaCity$d_so12; megaCity$d_agp12; megaCity$d_gadh12; megaCity$d_iedh12; megaCity$d_oddh12; megaCity$d_sad12; megaCity$d_add12; megaCity$d_bip12; d_ocd12
# Dúvida - Qual a diferença entre d_bip12 e i_bpd12 (imputed)?

megaCity$P1tierE  <- ifelse(   
     (megaCity$dsm5a >= 2 & megaCity$su3 <=5 & megaCity$su5 ==5 & (megaCity$sr122 == 1 | megaCity$su103 == 1 | megaCity$su39a >= 4 | megaCity$su39b >= 4 | megaCity$su39c >= 4 | megaCity$su39d >= 4) & (megaCity$cc10A >= 4 | megaCity$cc10B >= 4 | megaCity$cc10C >= 4 | megaCity$cc10D >= 4) & megaCity$cc12 > 0) |       
     (megaCity$dsm5s >= 2 & megaCity$su69 < 4 & (megaCity$sr122 == 1 | megaCity$su103 == 1 | megaCity$su86f.1 >= 4 | megaCity$su86f.2 >= 4 | megaCity$su86f.3 >= 4 | megaCity$su86f.4 >= 4) & (megaCity$cc10A >= 4 | megaCity$cc10B >= 4 | megaCity$cc10C >= 4 | megaCity$cc10D) & megaCity$cc12 > 0 & megaCity$sumMentalDis >= 2
   , "tierE", "Not tierE")

# --------------
# Pyramid two
# --------------

## Help Seeking
criteriaC  <- ifelse(
  (megaCity$su103 == 1 | (megaCity$sr2 == 1 & megaCity$sr5a < 4) | megaCity$sr8 == 1) | 
  (megaCity$su95 == 1 & (megaCity$sr20 < 4 | megaCity$sr28 < 4 | megaCity$sr41 < 4 | megaCity$sr49 < 4 | megaCity$sr58 < 4 | megaCity$sr67 < 4 | megaCity$sr75 < 4 | megaCity$sr88 < 4 | megaCity$sr101 < 4 | megaCity$sr128 == 1))
  , TRUE, FALSE)

## Tier B1 - Risky ou heavy users
megaCity$P2tierB1  <- ifelse( 
  (megacity$su3 < 5 & megacity$su9.1 > 0 & megacity$dsm5a == 0) | 
    ((megacity$su55d < 5 | megacity$su56d < 5 | megacity$su57d < 5) & megacity$dsm5s == 0) &criteriaC, 
  "tierB1", "Not tierB1")

## Tier B.2 - Risky ou heavy users with mild disorders
megaCity$P2tierB2  <- ifelse( 
  (megacity$su3 < 5 & megacity$su9.1 > 0 & megacity$dsm5a == 1) | 
    (megacity$su55d < 5 | megacity$su56d < 5 | megacity$su57d < 5 & megacity$dsm5s == 1) & criteriaC
  , "tierB2", "Not tierB2")


## Tier C - Users with alcohol or substance use disorders
megaCity$P2tierC  <- ifelse(
  (megaCity$dsm5a >= 2 & megaCity$su3 <=5 & megaCity$su5 ==5 & megaCity$sr122 == 5 & megaCity$su103 = 5 & (megaCity$su39a < 4 & megaCity$su39b < 4 & megaCity$su39c < 4 & megaCity$su39d < 4)) | 
    (megaCity$dsm5s >= 2 & megaCity$su69 < 4 & megaCity$sr122 == 5 & megaCity$su103 == 5 & (megaCity$su86f.1 < 4 & megaCity$su86f.2 < 4 & megaCity$su86f.3 < 4 & megaCity$su86f.4 < 4)) & criteriaC, 
  "tierC", "Not tierC")


## Tier D - Users with severe disorders
megaCity$P2tierD  <- ifelse(
  (megaCity$dsm5a >= 2 & megaCity$su3 <=5 & megaCity$su5 ==5 & (megaCity$sr122 == 1 | megaCity$su103 == 1 | megaCity$su39a >= 4 | megaCity$su39b >= 4 | megaCity$su39c >= 4 | megaCity$su39d >= 4)) |
    (megaCity$dsm5s >= 2 & megaCity$su69 < 4 & (megaCity$sr122 == 1 | megaCity$su103 == 1 | megaCity$su86f.1 >= 4 | megaCity$su86f.2 >= 4 | megaCity$su86f.3 >= 4 | megaCity$su86f.4 >= 4)) & criteriaC, 
  "tierD", 'Not tierD')

## Tier E - Comorbity
# OBS - Dependemos da classificação das variaveis feitas no SAS. Os nomes delas são megaCity$d_mddh12; megaCity$d_dysh12; megaCity$d_pds12; megaCity$d_sp12; megaCity$d_so12; megaCity$d_agp12; megaCity$d_gadh12; megaCity$d_iedh12; megaCity$d_oddh12; megaCity$d_sad12; megaCity$d_add12; megaCity$d_bip12; d_ocd12
# Dúvida - Qual a diferença entre d_bip12 e i_bpd12 (imputed)?

megaCity$P2tierE  <- ifelse(   
  (megaCity$dsm5a >= 2 & megaCity$su3 <=5 & megaCity$su5 ==5 & (megaCity$sr122 == 1 | megaCity$su103 == 1 | megaCity$su39a >= 4 | megaCity$su39b >= 4 | megaCity$su39c >= 4 | megaCity$su39d >= 4) & (megaCity$cc10A >= 4 | megaCity$cc10B >= 4 | megaCity$cc10C >= 4 | megaCity$cc10D >= 4) & megaCity$cc12 > 0) |       
    (megaCity$dsm5s >= 2 & megaCity$su69 < 4 & (megaCity$sr122 == 1 | megaCity$su103 == 1 | megaCity$su86f.1 >= 4 | megaCity$su86f.2 >= 4 | megaCity$su86f.3 >= 4 | megaCity$su86f.4 >= 4) & (megaCity$cc10A >= 4 | megaCity$cc10B >= 4 | megaCity$cc10C >= 4 | megaCity$cc10D) & megaCity$cc12 > 0 & megaCity$sumMentalDis >= 2 $ criteriaC
     , "tierE", "Not tierE")


###################
# ANALYSES -------
###################

# A Fazer:
# 1 - Ver se as categorias estão excludentes. Para isso inspecione o banco de dados. Tabelas de frequência podem ser uteis também.

## Inspecionar o banco de dados
megaCity[1:50, c("P1tierA", "P1tierB1", "P1tierB2", "P1tierC", "P1tierD", "P1tierE")]
megaCity[1:50, c("P2tierB1", "P2tierB2", "P2tierC", "P2tierD", "P2tierE")]


## Tableas de frequência
table(megaCity$P1tierA)
table(megaCity$P1tierB1)
table(megaCity$P1tierB2)
table(megaCity$P1tierC)
table(megaCity$P1tierD)
table(megaCity$P1tierE)

table(megaCity$P2tierB1)
table(megaCity$P2tierB2)
table(megaCity$P2tierC)
table(megaCity$P2tierD)
table(megaCity$P2tierE)


## Pacote 'survey'. Ele é bem documentado. Maiores informações aqui: http://www.r-bloggers.com/video-survey-package-in-r/; http://users.stat.umn.edu/~gmeeden/classes/5201/handouts/survey_handout.pdf. Eu tenho um livro sobre o pacote e existe um bolsista do Fernando, que pode dar orientações gerais. Contato: guilherme1marcelino@gmail.com

## Load survey package
library("survey")

## Create survey Design
svyMegaCity <- svydesign(
  id = ~secu,  
  strata = ~str,
  weights = ~finalp1wt
  data = megaCity,    
)

## Fazer tabelas de frequência das variáveis: sc1 (idade); sc1.1 (sexo).

## Examples - Frequency tables
round(prop.table(svytable(~fumoleve, sample.pnadPes)),3)*100

## Examples - Frequency tables by factor variable
round(prop.table(svytable(~fumoleve+sexo, sample.pnadPes)),3)*100

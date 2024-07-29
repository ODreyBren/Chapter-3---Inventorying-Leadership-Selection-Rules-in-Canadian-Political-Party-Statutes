# Purpose
R code for the Analysis of the MAXQDA Coding of Chapter 3 of the doctoral dissertation "Beyond the Ballot : Reviewing Canadian Political Parties’ Leadership Selection Rules"

# Notes on the data
The data (various political party statutes) were collected using the Webarchive, Bibliothèque de l'Assemblée national du Québec, and shared by some political parties. The presented data is the quantitative version. There are no text excerpts. We understand that these documents were one publically accessible, and we curretly ensuring that when we do fully share the party document database for Canadian Federal and Provincial parties, that it respects research ethics ans political parties. For more information please contact the author.

# Code
```R
#setpath
setwd("/Users/audreybrennan/Library/CloudStorage/Dropbox/PhD-Laval-ULB/_rCodesData/chapter1Descriptive")
```

## load packages
```
library(tidyverse) #data manipulations and ggplot
library(readxl) #importing Excel files
library(RColorBrewer)
```

## load data and select desired document type for the chapter
```
dataWrangling <- read_excel("data_dec_2023.xlsx")

# keep only the leadership race documents

chDescData <- dataWrangling %>%
                          filter(PartyDocType == "bylaw" | PartyDocType == "constitution" | #PartyDocType == "ldrQuestionnaire" |
                                   PartyDocType == "ldrRace" | PartyDocType == "ldrRaceMoney")
```

## Clean some variables

```
##### ##### ##### 
###Main variables
##### ##### #####
# rename party variables

qc <- chDescData %>%
  select(PartyDocType, `Document name`, partyNameFam, jurisdiction) %>%
    filter(jurisdiction == "QC")

qc$partyNameFam[qc$partyNameFam == "PC"] <- "PQ"

mb<- chDescData %>%
  select(PartyDocType, `Document name`, partyNameFam, jurisdiction) %>%
  filter(jurisdiction == "MB")

mb$partyNameFam[mb$partyNameFam == "PC"] <- "CONS"

test <- full_join(chDescData, mb)
test <- full_join(test, qc)

# double check to make sure the values went to right parties
table(test$jurisdiction, test$partyNameFam)

#send test data back to chDesc 
chDescData <- test 

#drop Yukon because territories are not covered in this chapter
chDescData <- test %>%
  filter(jurisdiction != "YK")

# drop PC values
chDescData <- chDescData %>%
  filter(partyNameFam != "PC")

#rename Newfoundland and labrador

chDescData$jurisdiction[chDescData$jurisdiction == "NFLDL"] <- "NL"

#rename party families

chDescData$partyNameFam[chDescData$partyNameFam == "CONS"] <- "Conservative"
chDescData$partyNameFam[chDescData$partyNameFam == "LIB"] <- "Liberal"
chDescData$partyNameFam[chDescData$partyNameFam == "GREEN"] <- "Green"

#reorder family names so that they are the same in every ggplot (major (libs/cons) to opposition parties)


# Party family with year (For cross tabs)
##### ##### ##### 
chDescData$partyNameJurisdictionYear <- paste(chDescData$jurisdiction, chDescData$partyNameFam, chDescData$docYear, sep=" ")



#make year numeric

chDescData$docYear <- as.numeric(chDescData$docYear)
```

## Select only post-2000 documents for description of data
File Name: `post2010Justification.pdf`
```
#Only keep after 2000 (for next graph)

chDescData <- chDescData %>%
  filter(docYear >= 2000) 





        # graph to justify 2010 cutoff point
        chDescData %>%
          filter(!is.na(jurisdiction)) %>%
          mutate(across(jurisdiction, factor, levels=c("Canada","BC","AB", "SK", "MB", "ON", "QC", "NB", "NS", "NL"))) %>% # so the jurisdictions are in the right order
          mutate(across(partyNameFam, factor, levels=c("QS", "PQ", "BQ", "Green", "NDP", "Liberal", "Conservative"))) %>% # so the party families are in the right order
          ggplot(aes(x = docYear, fill = PartyDocType)) + 
          geom_bar(stat ="bin",bins = 30) +
          geom_vline(xintercept=2010, color="black",
                     linetype="dashed", size = 1) +
          scale_fill_grey() +
          theme_bw()  +
          scale_x_continuous(breaks=seq(1980, 2023, 10)) + # for the x axis ticks to all appear
          labs(x = "Year", 
               y = "Document quantity", 
               fill = "Type of party document") +
          facet_wrap(~ jurisdiction, ncol = 5) +
          theme( # Facet_wrap labels
            strip.text.x = element_text(
              size = 16, color = "black", face = "bold"
            ),
            strip.text.y = element_text(
              size = 12, color = "black", face = "bold"
              ),
            strip.background=element_rect(fill=NA, color=NA),
            # Legend labels
            legend.title = element_text(color = "black", size = 14, face = "bold"),
            legend.text = element_text(color = "black", size = 14, face = "bold"), 
            legend.position="bottom",
            axis.text.x = element_text(color="black", 
                                       size=12),
            axis.text.y = element_text(face="bold", color="black", 
                                       size=14)
          )

```


## Take Only post-2010 for the chapter

Descriptive Figure of data. File Name: `partiesInData.pdf`

```
chDescData <- chDescData %>%
  filter(docYear >= 2010) 



##### ##### ##### ##### ##### ##### 
###Parties in the dataset     ##### 
##### ##### ##### ##### ##### ##### 

      chDescData %>%
      filter(!is.na(jurisdiction)) %>%
        mutate(across(jurisdiction, factor, levels=c("Canada","BC","AB", "SK", "MB", "ON", "QC", "NB", "NS", "NL"))) %>% # so the jurisdictions are in the right order
        mutate(across(partyNameFam, factor, levels=c("QS", "PQ", "BQ", "Green", "NDP", "Liberal", "Conservative"))) %>% # so the party families are in the right order
        ggplot(aes(x = docYear, y = partyNameFam, shape = PartyDocType))+
        geom_point(size = 5) +
        scale_shape_manual(values = c(0, 8 ,1)) +
        #scale_color_manual(values=c("#56B4E9", "blue", "darkgreen", "red", "darkorange", "darkblue", "chocolate1")) +
        theme_bw()  +
        scale_x_continuous(breaks=seq(2010, 2023, 5)) + # for the x axis ticks to all appear
        labs(x = "Year", 
             y= "Party Family Name", 
             color = "Party Family", 
             shape = "Party Document Type:") +
      facet_wrap(~ jurisdiction, ncol = 5) +
           theme( # Facet_wrap labels
          strip.text.x = element_text(
            size = 16, color = "black", face = "bold"
          ),
          strip.text.y = element_text(
            size = 12, color = "black", face = "bold"
          ),
          strip.background=element_rect(fill=NA, color=NA),
          # Legend labels
          legend.title = element_text(color = "black", size = 14, face = "bold"),
          legend.text = element_text(color = "black", size = 14, face = "bold"), 
          legend.position="bottom",
          axis.text.x = element_text(color="black", 
                                     size=12),
          axis.text.y = element_text(face="bold", color="black", 
                                     size=14)
        )



#### Reorder data so that it is organized by partyNameJurisdictionYear, this makes sure that the categorical variables are assigned to the
### right party 

chDescData <- chDescData %>%
  arrange(jurisdiction, partyNameFam, docYear)
```

# Proportionality

```
chDescData_Proportionality <- chDescData %>%
  select(c(`Document name`: partyNameFam, 
           Vote...207 : Members, 
           partyNameJurisdictionYear
  )) %>%
  filter(docYear >=2010)



#Different ways votes are weighted
#this is part of the proportionality framework
chDescData_Proportionality$`Weight - Supporters`[chDescData_Proportionality$`Weight - Supporters` > 0] <- 1
chDescData_Proportionality$`Weight-riding`[chDescData_Proportionality$`Weight-riding` > 0 ] <- 1
chDescData_Proportionality$`Weight- age group`[chDescData_Proportionality$`Weight- age group` > 0 ] <- 1


#riding weight
table(chDescData_Proportionality$partyNameJurisdictionYear, chDescData_Proportionality$`Weight-riding`)


#age weight
table(chDescData_Proportionality$partyNameJurisdictionYear, chDescData_Proportionality$`Weight- age group`)

#supporter weight
table(chDescData_Proportionality$partyNameJurisdictionYear, chDescData_Proportionality$`Weight - Supporters`)


#add variable together to create a change variable for later. This is to calculate years for which there is a change
# The way it works is that if there are plus and minuses between years, then I'll be able to calculate that and
## include it in the tables for the 'discussion' section of the chapter.
#create new vars to have categorical vars instead of binary before adding them together
chDescData_Proportionality$weightSupp[chDescData_Proportionality$`Weight - Supporters` >= 1] <- "supporter"
#chDescData_Proportionality$weightSupp[chDescData_Proportionality$`Weight - Supporters` == 0] <- NA #converting the 0s to NAs, because in the binary that's what it is
chDescData_Proportionality$weightSupp <- as.factor(chDescData_Proportionality$weightSupp) #the paste function has a hard time otherwise

chDescData_Proportionality$weightRid[chDescData_Proportionality$`Weight-riding` >= 1 ] <- "riding"
#chDescData_Proportionality$weightRid[chDescData_Proportionality$`Weight-riding` == 0] <- NA
chDescData_Proportionality$weightRid <- as.factor(chDescData_Proportionality$weightRid)

chDescData_Proportionality$weightAge[chDescData_Proportionality$`Weight- age group` >= 0 ] <- "age"
#chDescData_Proportionality$weightAge[chDescData_Proportionality$`Weight- age group` == 0] <- NA
chDescData_Proportionality$weightAge <- as.factor(chDescData_Proportionality$weightAge)

chDescData_Proportionality$allWeightCat <- paste(chDescData_Proportionality$weightRid, chDescData_Proportionality$weightSupp, chDescData_Proportionality$weightAge, sep = "_")






#selectorate

# DELEGATES
chDescData_Proportionality$delegates <- NA
chDescData_Proportionality$delegates [chDescData_Proportionality$`All members can be delegates` >= 1 |
                                                chDescData_Proportionality$`who chooses: when certain conditions filled` >= 1 |
                                                chDescData_Proportionality$`who chooses: national party rules` >= 1 |
                                                chDescData_Proportionality$`who chooses: riding` >= 1 |
                                                chDescData_Proportionality$Delegate...233 >= 1 ] <- 1

chDescData_Proportionality$delegates[is.na(chDescData_Proportionality$delegates)] <- 0


table(chDescData_Proportionality$partyNameJurisdictionYear, chDescData_Proportionality$delegates)

#create a categorical for delegates, so that it can be added to a categorical variableof all selectorate afterwards
chDescData_Proportionality$selectorateDelegates[chDescData_Proportionality$delegates >= 1] <- "delegate"
chDescData_Proportionality$selectorateDelegates <- as.factor(chDescData_Proportionality$selectorateDelegates)


#SUPPORTERS
chDescData_Proportionality$Supporters[chDescData_Proportionality$Supporters >= 1 ] <- 1
chDescData_Proportionality$Supporters[chDescData_Proportionality$Supporters == 0 ] <- 0


#selectorate supporters
table(chDescData_Proportionality$partyNameJurisdictionYear, chDescData_Proportionality$Supporters)

#create a categorical for supporters, so that it can be added to a categorical variable of all selectorate afterwards
chDescData_Proportionality$selectorateSupporters[chDescData_Proportionality$Supporters >= 1] <- "supporters"
chDescData_Proportionality$selectorateSupporters <- as.factor(chDescData_Proportionality$selectorateSupporters)

#MEMBERS
chDescData_Proportionality$Members[chDescData_Proportionality$Members >= 1 ] <- 1
chDescData_Proportionality$Members[chDescData_Proportionality$Members ==0 ] <- 0

#selectorate members
table(chDescData_Proportionality$partyNameJurisdictionYear, chDescData_Proportionality$Members)

#create a categorical for members, so that it can be added to a categorical variable of all selectorate afterwards
chDescData_Proportionality$selectorateMembers[chDescData_Proportionality$Members >= 1] <- "members"
chDescData_Proportionality$selectorateMembers <- as.factor(chDescData_Proportionality$selectorateMembers)


#ALL ELECTORS
chDescData_Proportionality$`All electors`[chDescData_Proportionality$`All electors` >= 1 ] <- 1
chDescData_Proportionality$`All electors`[chDescData_Proportionality$`All electors` ==0 ] <- 0

#selectorate all
table(chDescData_Proportionality$partyNameJurisdictionYear, chDescData_Proportionality$`All electors`)

#create a categorical for open primary, so that it can be added to a categorical variable of all selectorate afterwards
chDescData_Proportionality$selectorateallElectorate[chDescData_Proportionality$`All electors` >= 1] <- "allElectorate"
chDescData_Proportionality$selectorateallElectorate <- as.factor(chDescData_Proportionality$selectorateallElectorate)


# new categorical variable for selectorate
chDescData_Proportionality$selectorateCat <- paste(chDescData_Proportionality$selectorateDelegates,
                                              chDescData_Proportionality$selectorateMembers,
                                              chDescData_Proportionality$selectorateSupporters, sep = "_")



#Acclamation recode
chDescData_Proportionality$`Provisions for acclamation`[chDescData_Proportionality$`Provisions for acclamation` > 0 ] <- 1
chDescData_Proportionality$`Provisions for acclamation`[chDescData_Proportionality$`Provisions for acclamation` <=0 ] <- 0

table(chDescData_Proportionality$partyNameJurisdictionYear, chDescData_Proportionality$`Provisions for acclamation`)
```

# Ballot Structure 
File Name Signature Plot: `signatureQTY.pdf`
File Name Registration Fee Plot: `registrationFeeCh1_below2500.pdf`
File Name Compliance Deposit Plot: `complianceDepositCh1.pdf`

```
#### Who Can be candidate-major
#### Candidate Vetting-minor
#### Who chooses candidate-technical
#### How are vetting rules determines
##### ##### ##### ##### ##### ##### 
chDescDataBallotStructure <- chDescData %>%
                                select(c(`Document name`: partyNameFam, partyNameJurisdictionYear, 
                                        `Candidacy requirements` : Member, # who + vetting
                                        Yes, No, #interim leader can be candidate
                                        `Candidacy evaluation` : Committee,  # who chooses
                                        Fees : `Must be kept full` # financial requirements
                                        )) 
                                


####     ####
##  who    #
####     ####

## Interim leader can be candidate

chDescDataBallotStructure$interimCanBCandidateYes <- NA
chDescDataBallotStructure$interimCanBCandidateYes[chDescDataBallotStructure$Yes  >= 1 ] <- 1
chDescDataBallotStructure$interimCanBCandidateYes[chDescDataBallotStructure$Yes  == 0 ] <- 0

chDescDataBallotStructure$interimCanBCandidateNo <- NA
chDescDataBallotStructure$interimCanBCandidateNo[chDescDataBallotStructure$No  >= 1 ] <- 1
chDescDataBallotStructure$interimCanBCandidateNo[chDescDataBallotStructure$No  == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$interimCanBCandidateYes)
table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$interimCanBCandidateNo)


#Who candidate
chDescDataBallotStructure$whoCanBCandidateMember <- NA
chDescDataBallotStructure$whoCanBCandidateMember[chDescDataBallotStructure$Member >= 1 ] <- 1
chDescDataBallotStructure$whoCanBCandidateMember[chDescDataBallotStructure$Member == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$whoCanBCandidateMember)



chDescDataBallotStructure$whoCanBCandidateGender <- NA
chDescDataBallotStructure$whoCanBCandidateGender[chDescDataBallotStructure$`Gender parity` >= 1 ] <- 1
chDescDataBallotStructure$whoCanBCandidateGender[chDescDataBallotStructure$`Gender parity` == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$whoCanBCandidateGender)


####                 ####
##  Signature weight    #
####                 ####

#electronic
chDescDataBallotStructure$electronicSignatures <- chDescDataBallotStructure$Electronic...173
chDescDataBallotStructure$electronicSignatures[chDescDataBallotStructure$electronicSignatures >= 1] <- 1
chDescDataBallotStructure$electronicSignatures[chDescDataBallotStructure$electronicSignatures == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$electronicSignatures)

#create a categorical for electronic weight, so that it can be added to a categorical variable signature weight
chDescDataBallotStructure$signWeightElect[chDescDataBallotStructure$electronicSignatures >= 1] <- "electronic"
chDescDataBallotStructure$signWeightElect <- as.factor(chDescDataBallotStructure$signWeightElect)


#region 
chDescDataBallotStructure$regionSignatures <- chDescDataBallotStructure$Region
chDescDataBallotStructure$regionSignatures[chDescDataBallotStructure$Region >= 1 ] <- 1
chDescDataBallotStructure$regionSignatures[chDescDataBallotStructure$Region == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$regionSignatures)

#create a categorical for region weight, so that it can be added to a categorical variable signature weight
chDescDataBallotStructure$signWeightRegion[chDescDataBallotStructure$regionSignatures >= 1] <- "region"
chDescDataBallotStructure$signWeightRegion <- as.factor(chDescDataBallotStructure$signWeightRegion)


#other 
chDescDataBallotStructure$otherSignatures <- chDescDataBallotStructure$Other...175
chDescDataBallotStructure$otherSignatures[chDescDataBallotStructure$Other...175 >= 1 ] <- 1
chDescDataBallotStructure$otherSignatures[chDescDataBallotStructure$Other...175 == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$otherSignatures)

#create a categorical for other weight, so that it can be added to a categorical variable signature weight
chDescDataBallotStructure$signWeightOther[chDescDataBallotStructure$otherSignatures >= 1] <- "other"
chDescDataBallotStructure$signWeightOther <- as.factor(chDescDataBallotStructure$signWeightOther)

#province 
chDescDataBallotStructure$provSignatures <- chDescDataBallotStructure$Provinces
chDescDataBallotStructure$provSignatures[chDescDataBallotStructure$Provinces >= 1 ] <- 1
chDescDataBallotStructure$provSignatures[chDescDataBallotStructure$Provinces == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$provSignatures)


#create a categorical for provincial weight, so that it can be added to a categorical variable signature weight
chDescDataBallotStructure$signWeightProv[chDescDataBallotStructure$provSignatures >= 1] <- "province"
chDescDataBallotStructure$signWeightProv <- as.factor(chDescDataBallotStructure$signWeightProv)


#riding 
chDescDataBallotStructure$ridingSignatures <- chDescDataBallotStructure$Ridings
chDescDataBallotStructure$ridingSignatures[chDescDataBallotStructure$Ridings >= 1 ] <- 1
chDescDataBallotStructure$ridingSignatures[chDescDataBallotStructure$Ridings == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$ridingSignatures)


#create a categorical for riding weight, so that it can be added to a categorical variable signature weight
chDescDataBallotStructure$signWeightRiding[chDescDataBallotStructure$ridingSignatures >= 1] <- "riding"
chDescDataBallotStructure$signWeightRiding <- as.factor(chDescDataBallotStructure$signWeightRiding)


# new categorical variable for signature weight
chDescDataBallotStructure$signatureWeightCat <- paste(chDescDataBallotStructure$signWeightElect,
                                                   chDescDataBallotStructure$signWeightOther,
                                                   chDescDataBallotStructure$signWeightProv,
                                                   chDescDataBallotStructure$signWeightRegion, 
                                                   chDescDataBallotStructure$signWeightRiding, sep = "_")



#Signatures numbers. These were exported separately from the main data in MAXQDA since it does not automatically align the numbers with the vars
signaturesData <- read_xlsx("signatureNumbers.xlsx")


signaturesData <- signaturesData %>%
  select(c(`Document name`,  
           Segment))

signaturesData$signatureQty <- signaturesData$Segment

signaturesData <- signaturesData %>%
  select(-c(Segment))


# clean up a bit before parsing

signaturesData$signatureQty[signaturesData$signatureQty == "2 000"] <- "2,000"
signaturesData$signatureQty[signaturesData$signatureQty == "2000"] <- "2,000"
signaturesData$signatureQty[signaturesData$signatureQty == "1 000"] <- "1,000"
signaturesData$signatureQty[signaturesData$signatureQty == "1 500"] <- "1,500"
signaturesData$signatureQty[signaturesData$signatureQty == "fifty"] <- "50"


signaturesData$signatureQty <- parse_number(signaturesData$signatureQty)

library(rio)
#export to have a clean .csv
export(signaturesData, "signatureNumbers.csv")


#merge signature numbers with the candidate requirements

chDescDataBallotStructure <- merge(chDescDataBallotStructure, signaturesData)

####                            ####
##  Signature Number evolution     #
####                            ####


        chDescDataBallotStructure %>%
          mutate(across(jurisdiction, factor, levels=c("Canada","BC","AB", "SK", "MB", "ON", "QC", "NB", "NS", "NL"))) %>% # so the jurisdictions are in the right order
        ggplot(aes(x = docYear, y = signatureQty, colour = factor(partyNameFam), shape = PartyDocType))+
          geom_point(size = 5) +
          scale_shape_manual(values = c(0, 8 ,1)) +
          scale_color_manual(values=c("#56B4E9", "blue", "darkgreen", "red", "darkorange", "darkblue", "chocolate1")) +
          theme_bw() +
          scale_x_continuous(breaks=seq(2010, 2023, 5)) + # for the x axis ticks to all appear
          labs(x = "Year", 
               y= "Signature Quantity", 
               color = "Party Family") +
          facet_wrap(~ jurisdiction, ncol = 5) +
          theme(# Facet_wrap labels
            strip.text.x = element_text(
              size = 16, color = "black", face = "bold"
            ),
            strip.text.y = element_text(
              size = 12, color = "black", face = "bold"
            ),
            strip.background=element_rect(fill=NA, color=NA),
            # Legend labels
            legend.title = element_text(color = "black", size = 14, face = "bold"),
            legend.text = element_text(color = "black", size = 14, face = "bold"), 
            legend.position="bottom",
            axis.text.x = element_text(color="black", 
                                       size=12),
            axis.text.y = element_text(face="bold", color="black", 
                                       size=14)
          )



## financial requirements

#Turn off scientific notation as global setting
#convert the amounts so that they do not appear in scientific notation in plots
options(scipen=999)


chDescDataBallotStructure$typeFeesOther <- NA
chDescDataBallotStructure$typeFeesOther[chDescDataBallotStructure$Other...192 >= 1] <- 1
chDescDataBallotStructure$typeFeesOther[chDescDataBallotStructure$Other...192 == 0] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$typeFeesOther)

#create a categorical for other fees, so that it can be added to a categorical variable types of fees
chDescDataBallotStructure$otherFees <- NA
chDescDataBallotStructure$otherFees[chDescDataBallotStructure$typeFeesOther == 1] <- "other"
chDescDataBallotStructure$otherFees <- as.factor(chDescDataBallotStructure$otherFees)


chDescDataBallotStructure$typeFeesAdmin <- NA
chDescDataBallotStructure$typeFeesAdmin[chDescDataBallotStructure$Administrative >= 1] <- 1
chDescDataBallotStructure$typeFeesAdmin[chDescDataBallotStructure$Administrative == 0] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$typeFeesAdmin)

#create a categorical for other fees, so that it can be added to a categorical variable types of fees
chDescDataBallotStructure$adminFees[chDescDataBallotStructure$typeFeesAdmin >= 1] <- "administrative"
chDescDataBallotStructure$adminFees <- as.factor(chDescDataBallotStructure$adminFees)


chDescDataBallotStructure$typeFeesApplication <- NA
chDescDataBallotStructure$typeFeesApplication[chDescDataBallotStructure$`Application/registration` >= 1] <- 1
chDescDataBallotStructure$typeFeesApplication[chDescDataBallotStructure$`Application/registration` == 0] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$typeFeesApplication)

#create a categorical for other fees, so that it can be added to a categorical variable types of fees
chDescDataBallotStructure$registrationFees <- NA
chDescDataBallotStructure$registrationFees[chDescDataBallotStructure$typeDeesApplication >= 1] <- "registration"
chDescDataBallotStructure$registrationFees <- as.factor(chDescDataBallotStructure$registrationFees)


chDescDataBallotStructure$typeFeesCompliance <- NA
chDescDataBallotStructure$typeFeesCompliance[chDescDataBallotStructure$`Compliance deposit` >= 1] <- 1
chDescDataBallotStructure$typeFeesCompliance[chDescDataBallotStructure$`Compliance deposit` == 0] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$typeFeesCompliance)


#create a categorical for conpliance deposit, so that it can be added to a categorical variable types of fees
chDescDataBallotStructure$compliance[chDescDataBallotStructure$typeFeesCompliance >= 1] <- "compliance"
chDescDataBallotStructure$compliance <- as.factor(chDescDataBallotStructure$compliance)


# new categorical variable for types of fees
chDescDataBallotStructure$typesFeesCat <- paste(chDescDataBallotStructure$registrationFees,
                                                      chDescDataBallotStructure$otherFees,
                                                      chDescDataBallotStructure$adminFees,
                                                      chDescDataBallotStructure$compliance, sep = "_")






#load numbers (same issue as with Signatures above)
#registrationfee

registrationFeeData <- read_xlsx("registrationFeeNumbers.xlsx")


registrationFeeData <- registrationFeeData %>%
  select(c(`Document name`,  
           Segment))

registrationFeeData$registrationFeeNumbers <- registrationFeeData$Segment

registrationFeeData <- registrationFeeData %>%
  select(-c(Segment))


#cleaning up a few weird values
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "(20 000"  ] <- "20,000$" 
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "$20,000.00" ] <- "20,000$" 
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "$5000" ] <- "5,000$" 
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "11.1.1 Initial non-refundable Application Fee: $1,000 due at the time the application is  submitted for review"] <- "1,000$"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "14.8 Up to $7,500 of the administrative fees retained by the GPC can be used to cover part of  the contestant’s entry fees, the total of which is $30,000."]<- "30,000$"  
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "20 000"]<- "20,000$"  
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "50,000.00 $" ] <- "50,000$" 
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "60 000 $"  ] <- "60,000$"

registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "a certified cheque, solicitor's trust cheque or bank draft made payable to the NSLP in   the total sum $25,000.00."] <- "25,000$"
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "Afin d’obtenir un bulletin de candidature, une personne candidate doit remettre un paiement  non remboursable de 10 000 $, à titre de contribution aux dépenses du Parti pour l’organisation  et la tenue de l’élection, encaissable immédiatement, même s’il y a retrait subséquent de la  candidature" ] <- "10,000$" 
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "Application Fee Payment \nDirected contributions will always be retained at 50% unless the contestant instructs the Party  to retain the contributions at 100%. No other percentage will be administered. \nThe Application Fee must be paid using one or more of the following options:  ● Funds transferred from the Contestant’s campaign bank account to the  Party by cheque or e-transfer; \n● Newly directed contributions retained by the Party at 100%; \nCheques are payable to the “Green Party of Canada Fund” with the Applicant's name and  reason for the payment (ie Jean Grey campaign, for application fee) in the Memo field and paid  with funds from the Applicant’s Campaign Bank account. Cheques are to be mailed to: \nGreen Party of Canada  PO Box 997, Station B  Ottawa, ON  K1P 5R1 \nE-transfers will be accepted at finance@greenparty.ca. The e-transfer must include the name of  the Applicant and the purpose of the transfer (ie, Jean Grey campaign, application fee) and  funds must come from the Applicants Campaign Bank account. The application fee is a  campaign expense. E-transfers will not be accepted from individual contributors."  ] <- NA 
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "e. provide a non-refundable registration fee in the form of a certified cheque in the amount   of $10,000.00 payable to the Progressive Conservative Party of Newfoundland and   Labrador;  \nf. plus, a $10,000.00 deposit, in the form of a certified cheque, the amount will be refunded,   less any imposed penalties or deductions as per Rules 125, 126 and 186(c)."] <- "10,000$"
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "0 confirmed, the registered candidate   will be eligible for the services provided by the Party.   f) To be registered, a candidate must fundraise a total of $5000 in new contributions  directly to the Party. The period of time for raising new contributions begins on the   date that Provincial Council adopts these leadership rules. If a candidate raises an   amount in excess of $5000 at the time of registration, any amount in excess of $5000   is to be counted towards the amount required to be certified. The Provincial Secretary   shall confirm receipt and eligibility of all contributions to the Party." 
] <- "5,000$"
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "$75 000"  ] <- "75,000$"
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "1 000"  ] <- "1,000$"
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "50,000.00"   ] <- "50,000$"
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "20 000"  ] <- "20,000$" 
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "un paiement non remboursable de 15 000 $, à titre de contribution aux dépenses du Parti  pourl’organisation et la tenue de l’élection"  ] <- "15,000$" 
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "pay the applicable fee and deposit, as determined by the Party  Executive; and  \n"] <- NA
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "A deposit in the form of a certified cheque in the ammmt of :five thousand dollars  ($5,000) made payable to the Party shall accompany the nomination papers. This  deposit shall be non-refundable and utilized for the administration costs of the  Co11vention."] <- "5,000$"                                                                                                                                                                                                                                                                                                                                                            
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "CANDIDATE REGISTRATION FEES - Candidate registration fees amounting to a total of   $100,000 shall be paid by each Leadership Candidate in accordance with the following   schedule:  \nNOTE: These Rules of Procedure supplement and are subordinate to the Ontario Liberal Party   Constitution. Please refer to the Ontario Liberal Party website for the most current version of these   documents.  \nOntario Liberal Party Rules of Procedure No. 8  Rules of Procedure for the 2023 Leadership Contest (Approved April 16, 2023) – p. 2 \na) An initial registration fee in the amount of $5,000 shall be paid by each  \nLeadership Candidate to the Ontario Liberal Party, shall accompany their   Nomination Papers, and may be paid using the Leadership Candidate’s personal   monies or funds raised independently of these Rules. For clarity, all   supplemental fees described below must be paid from the campaign account   maintained by the Leadership Candidate’s Chief Financial Officer.  b) The first supplementary registration fee, in the amount of $20,000, shall be paid   by each Leadership Candidate to the Ontario Liberal Party, not later than 5:00   p.m. on July 31, 2023 or by 5:00 p.m. on the date that is two weeks after the date   of the Leadership Candidate’s registration with Elections Ontario, whichever shall   last occur.  c) The second supplementary registration fee, in the amount of $25,000, shall be  \npaid by each Leadership Candidate to the Ontario Liberal Party, not later than   5:00 p.m. on August 31, 2023 or by 5:00 p.m. on the date that is two weeks after   the date of the Leadership Candidate’s registration with Elections Ontario,   whichever shall last occur.  d) The third supplementary registration fee, in the amount of $25,000, shall be paid   by each Leadership Candidate to the Ontario Liberal Party, not later than 5:00   p.m. on September 29, 2023.  e) The fourth supplementary registration fee, in the amount of $25,000, shall be  \npaid by each Leadership Candidate to the Ontario Liberal Party, not later than   5:00 p.m. on October 31, 2023.   \n"] <- "100,000$"
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "The first instalment of the registration fee, twenty-five thousand ($25,000)   dollars, as per Section 3.3."  ] <- NA
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "To be registered, a candidate must fundraise a total of $5000 in new contributions   directly to the Party. The period of time for raising new contributions begins on the   date that Provincial Council adopts these leadership rules. If a candidate raises an   amount in excess of $5000 at the time of registration, any amount in excess of $5000   is to be counted towards the amount required to be certified. The Provincial Secretary   shall confirm receipt and eligibility of all contributions to the Party."] <- "5,000$"
#remove coding error
registrationFeeData$registrationFeeNumbers[registrationFeeData$registrationFeeNumbers == "To ensure compliance with these Rules and good conduct of the Candidates in   the election process, a compliance deposit of $5,000 will be levied. The compliance   deposit shall be paid at the time of filing the LCNP."] <- NA


#get only the numbers 
library(readr)
registrationFeeData$registrationFeeNumbers<- parse_number(registrationFeeData$registrationFeeNumbers)

#export the new vector
library(rio)
export(registrationFeeData, "registrationFeeNumbers.csv")



### compliance deposit

complianceNumbers <- read_excel("complianceDepNumbers.xlsx")


complianceNumbers <- complianceNumbers %>%
  select(c(`Document name`,  
           Segment))

complianceNumbers$amountComplianceDepNumbers <- complianceNumbers$Segment

complianceNumbers <- complianceNumbers %>%
  select(-c(Segment))

##Cleanup a few weird numbers

complianceNumbers$amountComplianceDepNumbers[complianceNumbers$amountComplianceDepNumbers == "plus a $10,000.00 deposit, in the form of a certified cheque, the amount will be   refunded, less any imposed penalties or deductions as per Rules 122, 123 and   186(c).  \n"] <-"$10,000"
complianceNumbers$amountComplianceDepNumbers[complianceNumbers$amountComplianceDepNumbers == "10 000" ] <- "$10,000"

complianceNumbers$amountComplianceDepNumbers <- parse_number(complianceNumbers$amountComplianceDepNumbers)

#export

export(complianceNumbers, "complianceDepAmount.csv")



### Spending limit numbers

spendingLimitData <- read_excel("spendingLimitNumbers.xlsx")

spendingLimitData <- spendingLimitData %>%
  select(c(`Document name`,  
           Segment))

spendingLimitData$spendingLimitNumbers <- spendingLimitData$Segment

spendingLimitData <- spendingLimitData %>%
  select(-c(Segment))


#cleaning up weird values
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "$3,400,000.00" ] <- "$3,400,000"
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "$350,000.00"] <- "$350,000"
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "$950 000" ] <- "$950,000" 
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "100 000"  ] <- "100,000"  
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "125 000 $" ] <- "125,000$"
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "150 000"  ] <- "150,000" 
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "200 000" ] <- "200,000" 
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "200 000 $" ] <- "200,000 $" 
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "200 000$" ] <- "200,000$" 
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "400 000" ] <- "400,000" 
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "500 000 $" ] <- "500,000 $"
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "600 000 $" ] <- "600,000 $"
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "600,000.00 $" ] <- "600,000 $" 
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "Le plafond des dépenses électorales des candidats a été fixé à 150 000 dollars" ] <- "150,000 $" 
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "The spending limit for all Candidates shall not exceed [$600,000 (November),   $800,000 (January) or $900,000 (February)] in election expenses." ] <- "900,000 $" 
spendingLimitData$spendingLimitNumbers[spendingLimitData$spendingLimitNumbers == "2 500"] <- "2,500$"

#get the numbers and omit the text
spendingLimitData$spendingLimitNumbers<- parse_number(spendingLimitData$spendingLimitNumbers)

#export as csv

export(spendingLimitData, "spendingLimitAmounts.csv")


#merge money datasets
chDescDataBallotStructure <- full_join(chDescDataBallotStructure, registrationFeeData)
chDescDataBallotStructure <- full_join(chDescDataBallotStructure, spendingLimitData)
chDescDataBallotStructure <- full_join(chDescDataBallotStructure, complianceNumbers)




#compliance deposit has to be kept full
chDescDataBallotStructure$complicanceDepKeptFull <- chDescDataBallotStructure$`Must be kept full`



table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$amountComplianceDep)

####     ####
##  Vetting #
####     ####


# recoding who evals the candidate candidacies

chDescDataBallotStructure$whoSelsCandidatesPresident <- NA
chDescDataBallotStructure$whoSelsCandidatesPresident[chDescDataBallotStructure$`Race president / CEO` >0 ] <- 1
chDescDataBallotStructure$whoSelsCandidatesPresident[chDescDataBallotStructure$`Race president / CEO` == 0 ] <- 0


table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$whoSelsCandidatesPresident)


#create a categorical for ceo, so that it can be added to a categorical variable vetting
chDescDataBallotStructure$vettingCeo[chDescDataBallotStructure$whoSelsCandidatesPresident >= 1] <- "ceo-president"
chDescDataBallotStructure$vettingCeo <- as.factor(chDescDataBallotStructure$vettingCeo)




chDescDataBallotStructure$whoSelsCandidatesCentral <- NA
chDescDataBallotStructure$whoSelsCandidatesCentral[chDescDataBallotStructure$`Central office or official` >0 ] <- 1
chDescDataBallotStructure$whoSelsCandidatesCentral[chDescDataBallotStructure$`Central office or official` == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$whoSelsCandidatesCentral)

#create a categorical for party central office, so that it can be added to a categorical variable vetting
chDescDataBallotStructure$vettingCentral[chDescDataBallotStructure$whoSelsCandidatesCentral >= 1] <- "central"
chDescDataBallotStructure$vettingCentral <- as.factor(chDescDataBallotStructure$vettingCentral)

chDescDataBallotStructure$whoSelsCandidatesCommittee <- NA
chDescDataBallotStructure$whoSelsCandidatesCommittee[chDescDataBallotStructure$Committee >0 ] <- 1
chDescDataBallotStructure$whoSelsCandidatesCommittee[chDescDataBallotStructure$Committee == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$whoSelsCandidatesCommittee)

#create a categorical for committee, so that it can be added to a categorical variable vetting
chDescDataBallotStructure$vettingCommittee[chDescDataBallotStructure$whoSelsCandidatesCommittee >= 1] <- "committee"
chDescDataBallotStructure$vettingCommittee <- as.factor(chDescDataBallotStructure$vettingCommittee)


# new categorical variable for types of fees
chDescDataBallotStructure$vettingCat <- paste(chDescDataBallotStructure$vettingCeo,
                                                chDescDataBallotStructure$vettingCentral,
                                                chDescDataBallotStructure$vettingCommittee, sep = "_")

#Interviews mandatory or not
chDescDataBallotStructure$mandatoryInterviewMandatory <- NA
chDescDataBallotStructure$mandatoryInterviewMandatory[chDescDataBallotStructure$Mandatory >0 ] <- 1
chDescDataBallotStructure$mandatoryInterviewMandatory[chDescDataBallotStructure$Mandatory == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$mandatoryInterviewMandatory )


chDescDataBallotStructure$mandatoryInterviewNotMandatory <- NA
chDescDataBallotStructure$mandatoryInterviewNotMandatory[chDescDataBallotStructure$`upon request` >0 ] <- 1
chDescDataBallotStructure$mandatoryInterviewNotMandatory[chDescDataBallotStructure$`upon request` == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$mandatoryInterviewNotMandatory )



#clear approval or rejection rules for candidate applications

chDescDataBallotStructure$aprouveClear <- NA
chDescDataBallotStructure$aprouveClear[chDescDataBallotStructure$Approuval > 0 ] <- 1
chDescDataBallotStructure$aprouveClear[chDescDataBallotStructure$Approuval == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$aprouveClear )


chDescDataBallotStructure$rejectClear <- NA
chDescDataBallotStructure$rejectClear[chDescDataBallotStructure$Rejection > 0 ] <- 1
chDescDataBallotStructure$rejectClear[chDescDataBallotStructure$Rejection == 0 ] <- 0

table(chDescDataBallotStructure$partyNameJurisdictionYear, chDescDataBallotStructure$rejectClear )


####                            ####
##  Entrance Fee evolution     #
####                            ####


chDescDataBallotStructure %>%
  filter(!is.na(jurisdiction)) %>%
  mutate(across(jurisdiction, factor, levels = c("Canada", "BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "NL")),
         PartyDocType = factor(PartyDocType, levels = c("bylaw", "constitution", "ldrRace"))) %>% # Ensure PartyDocType is a factor with specific levels
  ggplot(aes(x = docYear, y = registrationFeeNumbers, colour = factor(partyNameFam), shape = PartyDocType, na.rm = TRUE)) +
  geom_point(size = 5) +
  scale_shape_manual(values = c("bylaw" = 16, "constitution" = 17, "ldrRace" = 15), 
                     name = "Document Type") + # Map each categorical value to a specific shape and set legend title
  scale_color_manual(values = c("#56B4E9", "blue", "darkgreen", "red", "darkorange", "darkblue", "chocolate1"), 
                     name = "Party Family") + # Set the title for color legend
  theme_linedraw() +
  scale_x_continuous(breaks = seq(2010, 2023, 5)) + # for the x axis ticks to all appear
  labs(x = "Year", 
       y = "Registration Fees (Canadian Dollars)") + # Axis titles
  facet_wrap(~ jurisdiction, ncol = 5) +
  theme( # Facet_wrap labels
    strip.text.x = element_text(size = 16, color = "black", face = "bold"),
    strip.text.y = element_text(size = 12, color = "black", face = "bold"),
    strip.background = element_rect(fill = NA, color = NA),
    # Legend labels
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 14, face = "bold"), 
    legend.position = "bottom",
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(face = "bold", color = "black", size = 14)
  )



####                            ####
##  Compliance deposit evolution   #
####                            ####


      chDescDataBallotStructure %>%
  filter(!is.na(jurisdiction)) %>%
  mutate(across(jurisdiction, factor, levels = c("Canada", "BC", "AB", "SK", "MB", "ON", "QC", "NB", "NS", "NL")),
         PartyDocType = factor(PartyDocType, levels = c("bylaw", "constitution", "ldrRace"))) %>% # Ensure PartyDocType is a factor with specific levels
  ggplot(aes(x = docYear, y = amountComplianceDepNumbers, colour = factor(partyNameFam), shape = PartyDocType, na.rm = TRUE)) +
  geom_point(size = 5) +
  scale_shape_manual(values = c("bylaw" = 16, "constitution" = 17, "ldrRace" = 15), 
                     name = "Document Type") + # Map each categorical value to a specific shape and set legend title
  scale_color_manual(values = c("#56B4E9", "blue", "darkgreen", "red", "darkorange", "darkblue", "chocolate1"), 
                     name = "Party Family") + # Set the title for color legend
  theme_bw() +
  scale_x_continuous(breaks = seq(2010, 2025, 5)) + # for the x axis ticks to all appear
  labs(x = "Year", 
       y = "Compliance Deposit (Canadian Dollars)") + # Axis titles
  facet_wrap(~ jurisdiction, ncol = 5) +
  theme( # Facet_wrap labels
    strip.text.x = element_text(size = 16, color = "black", face = "bold"),
    strip.text.y = element_text(size = 12, color = "black", face = "bold"),
    strip.background = element_rect(fill = NA, color = NA),
    # Legend labels
    legend.title = element_text(color = "black", size = 14, face = "bold"),
    legend.text = element_text(color = "black", size = 14, face = "bold"), 
    legend.position = "bottom",
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(face = "bold", color = "black", size = 14)
  )


```



# Electoral Procedure

```
#### When are leaders chosen
chDescDataElectProc <- chDescData %>%
  select(c(`Document name`: partyNameFam, partyNameJurisdictionYear, 
           `When declare leadership vote` : Retirement, 
  )) 




# when leader selection

#incapacity

chDescDataElectProc$launchLdrSel_incapacity <- NA
chDescDataElectProc$launchLdrSel_incapacity[chDescDataElectProc$`Incapacity (judicial or other)` >= 1 ] <- 1
chDescDataElectProc$launchLdrSel_incapacity[chDescDataElectProc$`Incapacity (judicial or other)` == 0 ] <- 0

table(chDescDataElectProc$partyNameJurisdictionYear, chDescDataElectProc$launchLdrSel_incapacity)

#create a categorical for incapacity, so that it can be added to a categorical variable launch of leadership race

chDescDataElectProc$launchIncaCat[chDescDataElectProc$launchLdrSel_incapacity >= 1] <- "incapacity"
chDescDataElectProc$launchIncaCat <- as.factor(chDescDataElectProc$launchIncaCat)

#Administrative issues
chDescDataElectProc$launchLdrSel_admin <- NA
chDescDataElectProc$launchLdrSel_admin[chDescDataElectProc$`Admin issue` >= 1 ] <- 1
chDescDataElectProc$launchLdrSel_admin[chDescDataElectProc$`Admin issue` == 0 ] <- 0

table(chDescDataElectProc$partyNameJurisdictionYear, chDescDataElectProc$launchLdrSel_admin)

#create a categorical for admin issue, so that it can be added to a categorical variable launch of leadership race

chDescDataElectProc$launchadminCat[chDescDataElectProc$launchLdrSel_admin >= 1] <- "admin issue"
chDescDataElectProc$launchadminCat <- as.factor(chDescDataElectProc$launchadminCat)


#such as an invalid leadership vote
chDescDataElectProc$launchLdrSel_adminInvalidLdrVote <- NA
chDescDataElectProc$launchLdrSel_adminInvalidLdrVote[chDescDataElectProc$`Invalid leadership vote` >= 1 ] <- 1
chDescDataElectProc$launchLdrSel_adminInvalidLdrVote[chDescDataElectProc$`Invalid leadership vote` == 0 ] <- 0

table(chDescDataElectProc$partyNameJurisdictionYear, chDescDataElectProc$launchLdrSel_adminInvalidLdrVote)


#create a categorical for invalid leadership vote, so that it can be added to a categorical variable launch of leadership race

chDescDataElectProc$launchinvalidVCat[chDescDataElectProc$launchLdrSel_adminInvalidLdrVote >= 1] <- "invalid ldr vote"
chDescDataElectProc$launchinvalidVCat <- as.factor(chDescDataElectProc$launchinvalidVCat)

#death
chDescDataElectProc$launchLdrSel_death <- NA
chDescDataElectProc$launchLdrSel_death[chDescDataElectProc$Death >= 1 ] <- 1
chDescDataElectProc$launchLdrSel_death[chDescDataElectProc$Death == 0 ] <- 0

table(chDescDataElectProc$partyNameJurisdictionYear, chDescDataElectProc$launchLdrSel_death)


#create a categorical for death, so that it can be added to a categorical variable launch of leadership race

chDescDataElectProc$launchDeathCat[chDescDataElectProc$launchLdrSel_adminInvalidLdrVote >= 1] <- "death"
chDescDataElectProc$launchDeathCat <- as.factor(chDescDataElectProc$launchDeathCat)


#Leadership review Other
chDescDataElectProc$launchLdrSel_ldrReviewOther <- NA
chDescDataElectProc$launchLdrSel_ldrReviewOther[chDescDataElectProc$Other...99 >= 1  ] <- 1
chDescDataElectProc$launchLdrSel_ldrReviewOther[chDescDataElectProc$Other...99  == 0 ] <- 0


table(chDescDataElectProc$partyNameJurisdictionYear, chDescDataElectProc$launchLdrSel_ldrReviewOther)

#create a categorical for review other, so that it can be added to a categorical variable launch of leadership race
chDescDataElectProc$launchReviewOtherCat <- NA
chDescDataElectProc$launchReviewOtherCat[chDescDataElectProc$launchLdrSel_ldrReviewOther == 1] <- "review other"
chDescDataElectProc$launchReviewOtherCat <- as.factor(chDescDataElectProc$launchReviewOtherCat)


#leadership review Every convention
chDescDataElectProc$launchLdrSel_ldrReviewEveryCon <- NA
chDescDataElectProc$launchLdrSel_ldrReviewEveryCon[chDescDataElectProc$`Every convention` >= 1  ] <- 1
chDescDataElectProc$launchLdrSel_ldrReviewEveryCon[chDescDataElectProc$`Every convention`  == 0 ] <- 0


table(chDescDataElectProc$partyNameJurisdictionYear, chDescDataElectProc$launchLdrSel_ldrReviewEveryCon)

#create a categorical for review every convention, so that it can be added to a categorical variable launch of leadership race
chDescDataElectProc$launchReviewEveryCCat <- NA
chDescDataElectProc$launchReviewEveryCCat[chDescDataElectProc$launchLdrSel_ldrReviewOther == 1] <- "review every c"
chDescDataElectProc$launchReviewEveryCCat <- as.factor(chDescDataElectProc$launchReviewEveryCCat)


#leadership review convention following election if party not in government
chDescDataElectProc$launchLdrSel_ldrReviewConv_if_not_in_gov <- NA
chDescDataElectProc$launchLdrSel_ldrReviewConv_if_not_in_gov[chDescDataElectProc$`After election if party does not form gov` >= 1  ] <- 1
chDescDataElectProc$launchLdrSel_ldrReviewConv_if_not_in_gov[chDescDataElectProc$`After election if party does not form gov`  == 0 ] <- 0


table(chDescDataElectProc$partyNameJurisdictionYear, chDescDataElectProc$`After election if party does not form gov` )

#create a categorical for review if party does not for gov after elect,
##so that it can be added to a categorical variable launch of leadership race
chDescDataElectProc$launchReviewloseElectCat <- NA
chDescDataElectProc$launchReviewloseElectCat[chDescDataElectProc$launchLdrSel_ldrReviewConv_if_not_in_gov == 1] <- "review lose election"
chDescDataElectProc$launchReviewloseElectCat <- as.factor(chDescDataElectProc$launchReviewloseElectCat)


#resignation
chDescDataElectProc$launchLdrSel_resigntation <- NA
chDescDataElectProc$launchLdrSel_resigntation[chDescDataElectProc$Resignation >= 1  ] <- 1
chDescDataElectProc$launchLdrSel_resigntation[chDescDataElectProc$Resignation > 0 ] <- 0


table(chDescDataElectProc$partyNameJurisdictionYear, chDescDataElectProc$launchLdrSel_resigntation)

#create a categorical for resignation, so that it can be added to a categorical variable launch of leadership race
chDescDataElectProc$launchResignationCat <- NA
chDescDataElectProc$launchResignationCat[chDescDataElectProc$launchLdrSel_resigntation == 1] <- "resignation"
chDescDataElectProc$launchResignationCat <- as.factor(chDescDataElectProc$launchResignationCat)



#retirement
chDescDataElectProc$launchLdrSel_retirement <- NA
chDescDataElectProc$launchLdrSel_retirement[chDescDataElectProc$Retirement >= 1  ] <- 1
chDescDataElectProc$launchLdrSel_retirement[chDescDataElectProc$Retirement  == 0 ] <- 0


table(chDescDataElectProc$partyNameJurisdictionYear, chDescDataElectProc$launchLdrSel_retirement)


#create a categorical for  retirement, so that it can be added to a categorical variable launch of leadership race
chDescDataElectProc$launchRetirementCat <- NA
chDescDataElectProc$launchRetirementCat[chDescDataElectProc$launchLdrSel_retirement == 1] <- "retirement"
chDescDataElectProc$launchRetirementCat <- as.factor(chDescDataElectProc$launchRetirementCat)

#other
chDescDataElectProc$launchLdrSel_other <- NA
chDescDataElectProc$launchLdrSel_other[chDescDataElectProc$OTHER >= 1  ] <- 1
chDescDataElectProc$launchLdrSel_other[chDescDataElectProc$OTHER  == 0 ] <- 0


table(chDescDataElectProc$partyNameJurisdictionYear, chDescDataElectProc$launchLdrSel_other)


#create a categorical for any other reasons, so that it can be added to a categorical variable launch of leadership race
chDescDataElectProc$launchOtherCat <- NA
chDescDataElectProc$launchOtherCat[chDescDataElectProc$launchLdrSel_other == 1] <- "retirement"
chDescDataElectProc$launchOtherCat <- as.factor(chDescDataElectProc$launchOtherCat)


# new categorical variable for types of fees
chDescDataElectProc$launchRaceCat <- paste(chDescDataElectProc$launchIncaCat,
                                        chDescDataElectProc$launchadminCat,
                                        chDescDataElectProc$launchinvalidVCat, 
                                        chDescDataElectProc$launchDeathCat,
                                        chDescDataElectProc$launchReviewOtherCat,
                                        chDescDataElectProc$launchReviewEveryCCat, 
                                        chDescDataElectProc$launchReviewloseElectCat, 
                                        chDescDataElectProc$launchResignationCat,
                                        chDescDataElectProc$launchRetirementCat,
                                        chDescDataElectProc$launchOtherCat, sep = "_")


```



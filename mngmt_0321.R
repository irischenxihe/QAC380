################## 3/20 Christina Edits ########################

covid <- read.csv("Spring 2022/QAC380/Deident.RoutineCOVID19Testin_DATA_Jen class.csv")
covid_copy<-covid

###code gender identity - use gender identity or sex assigned at birth?
unique(covid$Current.gender.identity.)

covid_copy$genderId_coded[covid_copy$Current.gender.identity.==""] <-NA
covid_copy$genderId_coded[covid_copy$Current.gender.identity.=="Man"] <- 1
covid_copy$genderId_coded[covid_copy$Current.gender.identity.=="Woman"] <- 2
covid_copy$genderId_coded[covid_copy$Current.gender.identity.=="Gender nonconforming"|
                            covid_copy$Current.gender.identity.=="Genderqueer"|
                            covid_copy$Current.gender.identity.=="Non-binary"|
                            covid_copy$Current.gender.identity.=="Questioning/Unsure"|
                            covid_copy$Record.ID==68] <- 3
covid_copy$genderId_coded[ (covid_copy$Current.gender.identity.=="Not listed" & covid_copy$Record.ID!=68)|
                            covid_copy$Current.gender.identity.=="Choose not to disclose"] <- 4

#unique(covid_copy$genderId_coded)
#covid_copy$genderId_coded<-as.factor(covid_copy$genderId_coded)
  ##depends on regression model
#drop observation if genderID is NA?
 
###code race -- use race or ethnicity?
covid_copy$race_StringCoded[covid_copy$Race...choice.White.=="Checked"] <- "White"
covid_copy$race_StringCoded[covid_copy$Race...choice.African.American.=="Checked"] <- "African American"
covid_copy$race_StringCoded[covid_copy$Race...choice.Asian.=="Checked"] <- "Asian"
covid_copy$race_StringCoded[covid_copy$Race...choice.American.Indian.Pacific.Islander.=="Checked"] <- "American Indian Pacific Islander"

#each type only have one person. Include them in one type?
covid_copy$race_StringCoded[covid_copy$Record.ID==42] <- "Hispanic"
covid_copy$race_StringCoded[covid_copy$Record.ID==127] <- "Spanish"
covid_copy$race_StringCoded[covid_copy$Record.ID==183] <- "Brazilian"

covid_copy$race_StringCoded[covid_copy$Race...choice.Other.Not.listed.=="Checked" &
                              covid_copy$Record.ID!=c(42,127,183)] <- "Not disclosed"
#covid_copy[16:18,c("Record.ID","Race...choice.Other.Not.listed.","race_StringCoded")]

###code insurance
covid_copy$insurance[covid_copy$Primary.Insurance.Payer...choice.Medicaid.=="Checked"] <- "Medicaid"
covid_copy$insurance[covid_copy$Primary.Insurance.Payer...choice.Medicare.=="Checked"] <- "Medicare"
covid_copy$insurance[covid_copy$Primary.Insurance.Payer...choice.Private.=="Checked"] <- "Private"
covid_copy$insurance[covid_copy$Primary.Insurance.Payer...choice.None.=="Checked"] <- "None"

###code whether vaccinated
covid_copy$IfVaccinated[covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.No.=="Checked"] <- "No"
covid_copy$IfVaccinated[covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...one.dose.=="Checked"] <- "1 dose"
covid_copy$IfVaccinated[covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...2.doses.=="Checked"|
                        covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Unsure.=="Checked"] <- "2 doses"
covid_copy$IfVaccinated[covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...Booster.=="Checked"] <- "Booster"

#unique(covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Unsure.)
#covid_copy[covid_copy$Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Unsure.=="Checked",
#           c("Record.ID")]


###code reason of refusing rapid test
covid_copy$ReasonRefuse[covid_copy$Reason.why.patient.refused.COVID.19.testing...choice.Patient.was.recently.tested.=="Checked"|
                          covid_copy$Record.ID %in% c(222,228,230,280,306,346)] <- "recently tested"
covid_copy$ReasonRefuse[covid_copy$Reason.why.patient.refused.COVID.19.testing...choice.Patient.tested.positive.for.COVID.19.in.the.past.90.days.=="Checked"|
                          covid_copy$Record.ID %in% c(220,342)] <- "tested positive in 90 days"
covid_copy$ReasonRefuse[covid_copy$Reason.why.patient.refused.COVID.19.testing...choice.Patient.has.no.signs.or.symptoms.=="Checked"] <- "no symptoms"
covid_copy$ReasonRefuse[covid_copy$Reason.why.patient.refused.COVID.19.testing...choice.Patient.has.not.been.exposed.=="Checked"|
                          covid_copy$Record.ID %in% c(279,284)] <- "no exposure"
covid_copy$ReasonRefuse[covid_copy$Reason.why.patient.refused.COVID.19.testing...choice.Patient.didnt.want.to.isolate.for.the.holidays.=="Checked"] <- "avoid isolation"
covid_copy$ReasonRefuse[covid_copy$Reason.why.patient.refused.COVID.19.testing...choice.Patient.did.not.think.they.needed.a.test.=="Checked"|
                          covid_copy$Record.ID %in% c(3,14,62,67,71,156,163,211,221,223,227,272,273,278,312,315,321)] <- "passive attitude"
covid_copy$ReasonRefuse[covid_copy$Please.specify.reason.why.patient.refused.testing.=="vaccinated"|
                          covid_copy$Record.ID %in% c(282,320)] <- "(will) get vaccinated"

#unique(covid_copy$ReasonRefuse)


###delete redundant variables
covid_copy=subset(covid_copy,select = -c(Race...choice.White.,Race...choice.African.American.,Race...choice.Asian.,
                                         Race...choice.American.Indian.Pacific.Islander.,Race...choice.Other.Not.listed.,
                                         Primary.Insurance.Payer...choice.Medicaid.,Primary.Insurance.Payer...choice.Medicare.,
                                         Primary.Insurance.Payer...choice.Private.,Primary.Insurance.Payer...choice.None.,
                                         Race.other.not.listed..please.specify.,
                                         Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.No.,
                                         Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...one.dose.,
                                         Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...2.doses.,
                                         Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Unsure.,
                                         Has.this.patient.been.vaccinated.against.COVID.19...choose.all.that.apply...choice.Yes...Booster.,
                                         Reason.why.patient.refused.COVID.19.testing...choice.Patient.was.recently.tested.,
                                         Reason.why.patient.refused.COVID.19.testing...choice.Patient.tested.positive.for.COVID.19.in.the.past.90.days.,
                                         Reason.why.patient.refused.COVID.19.testing...choice.Patient.has.no.signs.or.symptoms.,
                                         Reason.why.patient.refused.COVID.19.testing...choice.Patient.has.not.been.exposed.,
                                         Reason.why.patient.refused.COVID.19.testing...choice.Patient.didnt.want.to.isolate.for.the.holidays.,
                                         Reason.why.patient.refused.COVID.19.testing...choice.Patient.did.not.think.they.needed.a.test.,
                                         Reason.why.patient.refused.COVID.19.testing...choice.Other.reason.,
                                         Please.specify.reason.why.patient.refused.testing.
                                         ### question: not delete reason? - AL
                                         ))

write.csv(covid_copy,"/Users/crisssxu/MyDesk/QAC380/covid_edited.csv", row.names = TRUE)



################## 3/21 Alison Edits ########################

# rename some variables names to be concise

library(dplyr)
library(tidyverse)

covid_copy <- covid_copy %>%
    rename(appmt_date = Appointment.date..D.ID,
           appmt_time = Appointment.time.,
           appmt_type = Appointment.Type.,
           appmt_other_reason = Other.reason.for.visit..please.specify.,
           age = Age,
           sex_birth = Sex.assigned.at.birth,
           gender_current = Current.gender.identity.,
           gender_specify = Please.specify.current.gender.identity.,
           ethnicity = Ethnicity,
           zipcode = Zip.code.of.residence.,
           consent_rapid = Did.the.patient.consent.to.COVID.19.rapid.testing.,
           symptoms = Did.the.patient.have.any.symptoms.,
           symptoms_cough = Specify.symptoms....choice.Cough.,
           symptoms_feverchills = Specify.symptoms....choice.Fever.chills.,
           symptoms_musclepain = Specify.symptoms....choice.Muscle.pain.,
           symptoms_sorethroat = Specify.symptoms....choice.Sore.throat.,
           symptoms_headache = Specify.symptoms....choice.Headache.,
           symptoms_vomit = Specify.symptoms....choice.Nausea.vomiting.,
           symptoms_diarrhea = Specify.symptoms....choice.Diarrhea.,
           symptoms_runnynose = Specify.symptoms....choice.Runny.nose.,
           symptoms_fatigue = Specify.symptoms....choice.Fatigue.,
           symptoms_congestion = Specify.symptoms....choice.Congestion.,
           symptoms_losstastesmell = Specify.symptoms....choice.Loss.of.taste.smell.,
           symptoms_other = Specify.symptoms....choice.Other.,
           symptoms_other_specify = Please.specify.other.symptoms.,
           positive_past = Has.the.patient.ever.tested.positive.for.COVID.19.,
           positive_apxdate = Approximate.date.s..patient.tested.positive.,
           closecontact_10 = In.the.last.10.days..has.this.patient.been.in.close.contact.with.someone.who.tested.positive.for.COVID.19.,
           closecontact_date = Date.of.last.contact.,
           travel_NewEngland_14 = Has.the.patient.traveled.outside.of.New.England.in.the.last.14.days.,
           travel_location = Location.of.travel.,
           travel_datereturn = Date.returned.from.travel.,
           test_last = When.was.the.patient.s.last.COVID.19.test.,
           vax_brand_1st = What.brand.did.the.patient.receive.for.their.1st.dose.,
           vax_brand_2nd = What.brand.did.the.patient.receive.for.their.2nd.dose.,
           vax_brand_booster = What.brand.did.the.patient.receive.for.their.booster.,
           vax_date_1st = Date.of.first.vaccination.,
           vax_date_2nd = Date.of.second.vaccination.,
           vax_date_booster = Date.of.booster.,
           novax_why_naturalimmune = If.not.vaccinated..why...choice.I.had.COVID.19.and.have.natural.immunity.,
           novax_why_notreq = If.not.vaccinated..why...choice.I.am.not.required.to.,
           novax_why_sideeffects = If.not.vaccinated..why...choice.I.am.worried.about.long.term.side.effects.,
           novax_why_infertility = If.not.vaccinated..why...choice.I.am.worried.about.infertility.,
           novax_why_other = If.not.vaccinated..why...choice.Other.reason.,
           novax_why_other_specify = If.other.reason.for.not.being.vaccinated..please.specify.,
           test_rapid = What.type.of.COVID.19.test.did.the.patient.get...choice.Rapid.,
           test_PCR = What.type.of.COVID.19.test.did.the.patient.get...choice.PCR.,
           test_none = What.type.of.COVID.19.test.did.the.patient.get...choice.None.,
           test_rapid_results = Rapid.Test.Results,
           test_PCR_results = PCR.Test.Results
           #complete?
           #other vars? 
           
    )


# Recode binary vars (mostly checked/unchecked boxes) into only 0 1 

# 

recode(covid_copy$ethnicity,"Not Hispanic/Latinx" = 0, "Hispanic / Latinx" = 1, .missing = NULL)

# Basic EDA 

summary(covid_copy)


remotes::install_github("rkabacoff/eda", upgrades = "never", quiet = TRUE)
library("eda")

contents(covid_copy) # gives the pct of missing obs for each var
df_plot(covid_copy)

barcharts(covid_copy)

# factor analysis - want to look at cluster variables 

library(factorAnalysis)

screePlot(x, method = "pa")





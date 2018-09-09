#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)




# import data
data <- read_csv("data/non-compliance-in-personal-insolvencies.csv")

# coerce data types
data$`Calendar Year of Insolvency` <- parse_date(data$`Calendar Year of Insolvency`)
data$`SA3 of Debtor`<- factor(data$`SA3 of Debtor`)
data$`SA3 Code of Debtor` <- factor(data$`SA3 Code of Debtor`)
data$`GCCSA of Debtor` <- factor(data$`GCCSA of Debtor`)
data$`GCCSA Code of Debtor` <- factor(data$`GCCSA Code of Debtor`)
data$`Sex of Debtor` <- parse_factor(data$`Sex of Debtor`, levels = c("Male","Female"), na = c("Unknown","Not Stated"))
data$`Family Situation` <- parse_factor(data$`Family Situation`, levels = NULL)
data$`Debtor Occupation Code (ANZSCO)`<- factor(data$`Debtor Occupation Code (ANZSCO)`)
data$`Debtor Occupation Name (ANZSCO)` <- factor(data$`Debtor Occupation Name (ANZSCO)`)

#Cause of insolvency feature engineering
data$`Main Cause of Insolvency` <- factor(data$`Main Cause of Insolvency`)
getGeneralNonBusinessReason <- function(reason) {
  excessConsumptionReasons <- c("Gambling or speculation", "Gambling, speculation & extravagance in living","Excessive use of credit facilities including losses on repossessions, high interest payments and pressure selling")
  personaReasons <- c("Ill health or absence of health insurance", "Domestic discord or relationship breakdowns","Domestic discord or relationship breakdown","Personal reasons, including ill health of self or dependents, domestic discord & other personal reasons")
  employmentReasons <- c("Unemployment or loss of income")
  
  if_else(reason %in% excessConsumptionReasons, "Excess Consumption", 
          if_else(reason %in% personaReasons, "Personal",
                  if_else(reason %in% employmentReasons, "Employment", "All Other"))) 
}

data <- data %>% mutate(NonBusinessInsolvencyReason = getGeneralNonBusinessReason(data$`Main Cause of Insolvency`))
data$NonBusiness
InsolvencyReason <- parse_factor(data$NonBusinessInsolvencyReason, levels = NULL)


isBus <- function (yesNo) {
  if_else(yesNo == "Yes",TRUE,FALSE)
}

data <- data %>% mutate(IsBusiness = isBus(data$`Business Related Insolvency`)) 

allowedIncomes <- c("$0-$49999","$50000-$99999","$100000-$149999","$150000-$199999","More Than $200000")
data$`Debtor Income` <- parse_factor(data$`Debtor Income`,levels = allowedIncomes,ordered = TRUE,na = "NA")

data$`Primary Income Source` <- factor(data$`Primary Income Source`)

debtsValues <- data %>% group_by(`Unsecured Debts`) %>% count %>% pull(`Unsecured Debts`)

data$`Unsecured Debts` <- parse_factor(data$`Unsecured Debts`,ordered = TRUE, levels = debtsValues)

assetsValues <- data %>% group_by(`Value of Assets`) %>% count() %>% pull(`Value of Assets`)
data$`Value of Assets` <- parse_factor(data$`Value of Assets`, ordered = TRUE, levels = assetsValues)

data %>% group_by(`Non-Compliance Type`) %>% count

data$`Non-Compliance Type` <- factor(data$`Non-Compliance Type`)

complianceFactor = as.factor(c("Compliant","Non-compliant"))

isIndividualNonCompliance <- function(nonComplianceType) {
  individualNonComplianceTypes <- c("Objection to Discharge","Offence Referral")
  nonComplianceType %in% individualNonComplianceTypes
}

data <- data %>% mutate(IndividualNonComplianceInvestigated = isIndividualNonCompliance(`Non-Compliance Type`))

#engineer feature - family situation unknown or not stated
fsIsAbsent <- function(familySituation) {
  familySituation %in% c("Unknown","Not Stated")
}

data <- data %>% mutate(FamilySituationOmitted = fsIsAbsent(data$`Family Situation`))

compliant <- data %>% filter(IndividualNonComplianceInvestigated == FALSE) 
nonCompliant <- data %>% filter(IndividualNonComplianceInvestigated == TRUE)

compliantSex <-  compliant %>% group_by(`Sex of Debtor`) 
nonCompliantSex <- nonCompliant %>% group_by(`Sex of Debtor`)

sexPlot <- ggplot(data, aes(data$IndividualNonComplianceInvestigated)) +
  geom_bar(aes(fill = `Sex of Debtor`), position = "fill") + 
  labs(title = "Males Over-repesented in Non-Compliances Cases",x = "Case indicates individual non-compliance", y = "Proportion of cases") 

familiesPlot <-  ggplot(data, aes(data$IndividualNonComplianceInvestigated)) +
  geom_bar(aes(fill = `Family Situation`), position = "fill") + 
  labs(title = "Family Situations", subtitle = "Fewer single with dependents and\n much more often left unstated in non-complance cases",x = "Case indicates individual non-compliance", y = "Proportion of cases") 

businessPlot <- ggplot(data, aes(data$IndividualNonComplianceInvestigated)) +
  geom_bar(aes(fill = data$IsBusiness), position = "fill") + 
  labs(title = "Business-Related Insolvencies Over-represented",
       x = "Case indicates individual non-compliance", 
       y = "Proportion of cases",
       fill = "Is Business Related") 

justNonBusinessRelatedInsolvencies <- data %>% filter(IsBusiness == FALSE)

synthReasons <- ggplot(justNonBusinessRelatedInsolvencies, aes(justNonBusinessRelatedInsolvencies$IndividualNonComplianceInvestigated)) +
  geom_bar(aes(fill = justNonBusinessRelatedInsolvencies$NonBusinessInsolvencyReason), position = "fill") + 
  labs(title = "Reasons for Non-Business Related Insolvencies",
       x = "Case indicates individual non-compliance", 
       y = "Proportion of cases",
       fill = "Reason") 

#Engineer feature - other reason specified as main reason for insolvency

causesCount <- data %>% group_by(`Main Cause of Insolvency`) %>%  count() %>% arrange(n)
causesOrdered <- causesCount %>% pull(1)

causes <- data %>% group_by(`Main Cause of Insolvency`)

causesOfExcess <- c(causesOrdered[1])

grubbyReasons <- ggplot(data = data, aes(x = (reorder(data$`Main Cause of Insolvency`, data$`Main Cause of Insolvency`, function(x)-length(x))))) +
  geom_bar() +
  labs(title = "Main Reason for Insolvency", subtitle = "Grubby Data Quality Disaster", x = "Reason") +
  coord_flip()

# just for non-business, just for non-compliance

nonBusinessRelatedNonCompliant <- data %>% 
  filter(IsBusiness == FALSE) %>% 
  filter(IndividualNonComplianceInvestigated == TRUE)


nonBusReasons <- ggplot(data = nonBusinessRelatedNonCompliant, aes(x = (reorder(nonBusinessRelatedNonCompliant$`Main Cause of Insolvency`, nonBusinessRelatedNonCompliant$`Main Cause of Insolvency`, function(x)-length(x))))) +
  geom_bar() +
  labs(title = "Main Reason for Insolvency", subtitle = "Non Business Related, Non-Compliance Cases", x = "Reason") +
  coord_flip() 


#all non compliant reasons
allNonCompliant <- data %>% 
 # filter(IsBusiness == FALSE) %>% 
  filter(IndividualNonComplianceInvestigated == TRUE)


ggplot(data = allNonCompliant, aes(x = (reorder(allNonCompliant$`Main Cause of Insolvency`, allNonCompliant$`Main Cause of Insolvency`, function(x)-length(x))))) +
  geom_bar() +
  labs(title = "Main Reason for Insolvency", subtitle = "All Non-Compliance Cases", x = "Reason") +
  coord_flip() 


data %>% group_by(IndividualNonComplianceInvestigated) %>% count

data %>% group_by(`Type of Party`) %>% count
# Ignore type of party - not important who does compliance action

data %>% group_by(`Debtor Income`) %>% count

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Predicting Non-Compliance with Personal Bankruptcy Conditions"),
  titlePanel("GovHack 2018 entry from Team 42"),
  
   
   tabsetPanel(
     tabPanel("Advisory Notice",
              flowLayout(img(src="dp.png"))),
     tabPanel("Data Story", 
              
                h1("'I may not have gone where I intended to go, but I think I have ended up where I needed to be.' 
― Douglas Adams, The Long Dark Tea-Time of the Soul"),
                img(src="da.jpg"),
                
              
              h1("Isolated dependent variables"),
              h2("Focused on investigation of non-compliance"),
              p("Focused on whether there was some investigation of non-compliance."),
              p("AFSA mentor River Paul confirmed this was the most interesting."),
              p("That simplifies life because I can ignore fields about the outcome of the investigation or action, which happens later."),
              
              img(src="goal.png"),
              h2("Focused on non-compliance by individual, not trustee"),
              p("I decided to focus on non-compliance by the debtor.  That means we don't care about types of non-compliance by the trustee."),
              img(src = "types.png"),
              p("Life gets simpler still."),
              
              h1("Scrubbadubdub, Get All the Data in the Tub"),
              p("There followed some hours of going through each of the potential features, one by one, scrubbing and cleaning."),
              img(src = "boring.png"),
              
              h1("Exploratory Data Analysis"),
              h2("Males are Miscreants"),
              plotOutput("sexPlot"),
              h2("Family Uncertainty Signal"),
              plotOutput("familiesPlot"),
              img(src = "families.png"),
              h2("Business Related Insolvencies"),
              plotOutput("businessPlot"),
              h2("Reason for Involvency"),
              plotOutput("grubbyReasons"),
              p("I attempted to group them."),
              img(src="sin.png"),
              p("Feature emerged."),
              plotOutput("synthReasons"),
              p("I confirmed this by looking back at the raw reasons..."),
              plotOutput("nonBusReasons")
   )
   )
   # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #    sidebarPanel(
   #       # sliderInput("bins",
   #       #             "Number of bins:",
   #       #             min = 1,
   #       #             max = 50,
   #       #             value = 30)
   #    ),
   #    
   #    # Show a plot of the generated distribution
   #    mainPanel(
   #       plotOutput("sexPlot")
   #    )
   # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$sexPlot <- renderPlot(sexPlot)
  output$familiesPlot <-renderPlot(familiesPlot)
  output$businessPlot <- renderPlot(businessPlot)
  output$grubbyReasons <- renderPlot(grubbyReasons)
  output$synthReasons <- renderPlot(synthReasons)
  output$nonBusReasons <- renderPlot(nonBusReasons)
  
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
}

# Run the application 
shinyApp(ui = ui, server = server)

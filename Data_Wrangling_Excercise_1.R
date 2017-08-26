install.packages("xlsx")

# import statements

library("dplyr")
library("xlsx")


products <- read.xlsx("D://Data_Wrangling//refine.xlsx",sheetIndex = 1)



# split product in product_code and product_number
split_product <- products %>% separate(Product.code...number.,c("product_code","product_number"),sep = "-")



#View(split_product)

# Mapping code to product

mapping_product <- split_product %>%  mutate(product_category = ifelse(product_code=="p","SmartPhone","")) %>% 
  mutate(product_category = ifelse(product_code=="v","TV",product_category)) %>%  mutate(product_category = ifelse(product_code=="x","Laptop",product_category)) %>% 
  mutate(product_category = ifelse(product_code=="q","Tablet",product_category))

# Full Address

full_address <- mapping_product %>% unite("full_address",address,city,country,sep = ",")

test <- function(cou){
  print(paste("First Value",cou))
  if(grepl("^phil|^fil|^phl",cou,ignore.case = TRUE))
  {
    print(paste("Second Value",cou))
    return("philips")
  }
  else if(grepl("^ak",cou,ignore.case = TRUE))
    {
    print(paste("Second Value",cou))
      return("akzo")
    }
  else if(grepl("^van",cou,ignore.case = TRUE))
  {
    return("van houten")
  }
  else if(grepl("^uni",cou,ignore.case = TRUE))
  {
    return("unilever")
  }
}

# to fix company name
full_address$company <- sapply(full_address$company,test)

# to add company_philips , company_akzo , company_van_houten , company_unilever and other variable

new_variable_set <- full_address %>% mutate(company_philips = ifelse(company == "philips",1,0)) %>% 
  mutate(company_akzo = ifelse(company == "akzo",1,0)) %>% mutate(company_van_houten = ifelse(company == "van houten",1,0)) %>% 
  mutate(company_unilever = ifelse(company == "unilever",1,0)) %>% 
  mutate(product_smartphone = ifelse(product_code == "p",1,0)) %>% 
  mutate(product_tv = ifelse(product_code == "x",1,0)) %>% 
  mutate(product_laptop  = ifelse(product_code == "v",1,0)) %>% 
  mutate(product_tablet = ifelse(product_code == "q",1,0))


#write.csv(file = "refine_clean",x = new_variable_set)

write.csv(file = "D:\\refine_clean.csv",x = new_variable_set)


# company name changes

#company_name_change <- full_address %>%  mutate(company = ifelse(grepl("^phil|^fil|^phl",company,ignore.case = TRUE),"philips",company)) %>%  
#  mutate(company = ifelse(grepl("^ar",company,ignore.case = TRUE),"arzo",company)) %>%  
 # mutate(company = ifelse(grepl("^van",company,ignore.case = TRUE),"van houten",company)) %>%  
#  mutate(company = ifelse(grepl("^uni",company,ignore.case = T),"unilever",company))



  
  

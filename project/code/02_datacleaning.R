# ==============================================================================
# 02_datacleaning.R
#
# This script cleans the raw HILDA panel data without creating any new derived variables.
# It:
#   - Recodes HILDA's special missing codes (-1 to -10) as NA
#   - Recodes the out-of-range codes on the percent-chance items (997 / 999)
#   - Applies value labels and factorises categorical variables
#   - Renames variables to [descriptive_name]_[original_hilda_varname]
#
# This script requires 01_setup.R to be sourced first using the code below:
# source("01_setup.R")
# ==============================================================================

# --- Recode HILDA's special missing codes to NA -------------------------------

hilda_missing_codes <- c(
  -10, # Non-responding person
  -9, # Non-responding household
  -8, # No SCQ
  -7, # Not able to be determined
  -6, # Implausible value
  -5, # Multiple response SCQ
  -4, # Refused / Not stated
  -3, # Don't know
  -2, # Not applicable
  -1 # Not asked
)

# In HILDA, values like 997 and 999 are non-response or skip-pattern codes
# 997 = would not seek another job (jbmpgj only)
# 999 = not currently working
# Hence we recode these to NA
pct_chance_vars <- c("jbmpgj", "jbmplej", "jbmploj")


# --- Rename HILDA variables and factorise categorical variables -------------

hilda_labels <- list(
  hhstate = c(
    "1" = "New South Wales",
    "2" = "Victoria",
    "3" = "Queensland",
    "4" = "South Australia",
    "5" = "Western Australia",
    "6" = "Tasmania",
    "7" = "Northern Territory",
    "8" = "Australian Capital Territory"
  ),
  hhs3gcc = c(
    "11" = "Greater Sydney",
    "19" = "Rest of NSW",
    "21" = "Greater Melbourne",
    "29" = "Rest of VIC",
    "31" = "Greater Brisbane",
    "39" = "Rest of QLD",
    "41" = "Greater Adelaide",
    "49" = "Rest of SA",
    "51" = "Greater Perth",
    "59" = "Rest of WA",
    "61" = "Tasmania",
    "71" = "Northern Territory",
    "81" = "ACT"
  ),
  hhs3sos = c(
    "0" = "Major Urban",
    "1" = "Other Urban",
    "2" = "Bounded Locality",
    "3" = "Rural Balance",
    "7" = "Migratory / Offshore / Shipping",
    "9" = "No usual address"
  ),
  hhs3ra = c(
    "0" = "Major cities of Australia",
    "1" = "Inner regional Australia",
    "2" = "Outer regional Australia",
    "3" = "Remote Australia",
    "4" = "Very remote Australia",
    "5" = "Migratory / offshore / shipping",
    "9" = "No usual address"
  ),
  hstenr = c(
    "1" = "Own/currently paying off mortgage",
    "2" = "Rent (or pay board)",
    "3" = "Involved in a rent-buy scheme",
    "4" = "Live here rent free/Life Tenure"
  ),
  hsmgpd = c(
    "1" = "Loan paid off",
    "2" = "Loan not paid off"
  ),
  hsmguse = c(
    "1" = "Took out institutional loan",
    "2" = "No institutional loan"
  ),
  hsrntby = c(
    "1" = "Involved in a rent-buy arrangement",
    "2" = "Not involved in a rent-buy arrangement",
    "3" = "No"
  ),
  fiprbeg = c(
    "1" = "Yes (could not pay)",
    "2" = "No (could pay)"
  ),
  fiprbmr = c(
    "1" = "Yes (could not pay)",
    "2" = "No (could pay)"
  ),
  fiprbfh = c(
    "1" = "Yes (asked for help)",
    "2" = "No (did not ask for help)"
  ),
  fiprbps = c(
    "1" = "Yes",
    "2" = "No"
  ),
  fiprbuh = c(
    "1" = "Yes",
    "2" = "No"
  ),
  fiprbwm = c(
    "1" = "Yes",
    "2" = "No"
  ),
  fiprbwo = c(
    "1" = "Yes",
    "2" = "No"
  ),
  fiprosp = c(
    "1" = "Prosperous",
    "2" = "Very comfortable",
    "3" = "Reasonably comfortable",
    "4" = "Just getting along",
    "5" = "Poor",
    "6" = "Very poor"
  ),
  fiemerf = c(
    "1" = "Could easily raise emergency funds",
    "2" = "Could raise emergency funds, but it would involve some sacrifices",
    "3" = "Would have to do something drastic to raise emergency funds",
    "4" = "Couldn't raise emergency funds"
  ),
  hhs3add = c(
    "1" = "Decile 1 (most disadvantaged)",
    "2" = "Decile 2",
    "3" = "Decile 3",
    "4" = "Decile 4",
    "5" = "Decile 5",
    "6" = "Decile 6",
    "7" = "Decile 7",
    "8" = "Decile 8",
    "9" = "Decile 9",
    "10" = "Decile 10 (least disadvantaged)"
  ),
  hgsex = c(
    "1" = "Male",
    "2" = "Female"
  ),
  mrcurr = c(
    "1" = "Legally married",
    "2" = "De facto",
    "3" = "Separated",
    "4" = "Divorced",
    "5" = "Widowed",
    "6" = "Never married and not de facto"
  ),
  anbcob = c(
    "1" = "Australia",
    "2" = "Main English-speaking country",
    "3" = "Other country"
  ),
  aneab = c(
    "1" = "Speaks English very well",
    "2" = "Speaks English well",
    "3" = "Does not speak English well",
    "4" = "Does not speak English at all"
  ),
  hgeab = c(
    "1" = "Speaks English very well",
    "2" = "Speaks English well",
    "3" = "Does not speak English well",
    "4" = "Does not speak English at all"
  ),
  anengf = c(
    "1" = "English is first language",
    "2" = "Non-English first language"
  ),
  anmigc = c(
    "1" = "Skilled migration visa",
    "2" = "Business migration visa",
    "3" = "Family migration visa",
    "4" = "Refugee / humanitarian visa",
    "5" = "New Zealand citizen",
    "8" = "Other or unknown visa category"
  ),
  anrefn = c(
    "1" = "Yes",
    "2" = "No"
  ),
  anlote = c(
    "1" = "Yes",
    "2" = "No"
  ),
  edsscmp = c(
    "1" = "Year 12 or equivalent",
    "2" = "Year 11 or equivalent",
    "3" = "Year 10 or equivalent",
    "4" = "Year 9 or equivalent",
    "8" = "Other"
  ),
  edlhqn = c(
    "110" = "Doctorate",
    "120" = "Master's degree",
    "211" = "Graduate diploma",
    "221" = "Graduate certificate",
    "311" = "Honours bachelor degree",
    "312" = "Bachelor degree (without honours)",
    "411" = "Advanced diploma (3 years full-time or equivalent)",
    "413" = "Associate degree",
    "421" = "Diploma (2 years full-time or equivalent)",
    "500" = "Certificate (level not known)",
    "511" = "Certificate level IV",
    "514" = "Certificate level III",
    "521" = "Certificate level II",
    "524" = "Certificate level I",
    "995" = "Other qualification"
  ),
  edpsqau = c(
    "1" = "Yes",
    "2" = "No"
  ),
  edhigh1 = c(
    "1" = "Postgrad (master or doctorate)",
    "2" = "Grad diploma, grad certificate",
    "3" = "Bachelor or honours",
    "4" = "Adv diploma, diploma",
    "5" = "Certificate III or IV",
    "8" = "Year 12",
    "9" = "Year 11 and below",
    "10" = "Undetermined"
  ),
  es = c(
    "1" = "Employee",
    "2" = "Employer",
    "3" = "Own account worker",
    "4" = "Contributing family member"
  ),
  esbrd = c(
    "1" = "Employed",
    "2" = "Unemployed",
    "3" = "Not in the labour force"
  ),
  esempdt = c(
    "1" = "Employee",
    "2" = "Employee of own business (with employees)",
    "3" = "Employee of own business (without employees)",
    "4" = "Employer or self-employed (with employees)",
    "5" = "Employer or self-employed (without employees)",
    "6" = "Unpaid family worker"
  ),
  esdtl = c(
    "1" = "Employed full-time",
    "2" = "Employed part-time",
    "3" = "Unemployed, looking for full-time work",
    "4" = "Unemployed, looking for part-time work",
    "5" = "Not in the labour force, marginally attached",
    "6" = "Not in the labour force, not marginally attached",
    "7" = "Employed, usual hours unknown"
  ),
  jbmo61 = c(
    "1" = "Managers",
    "2" = "Professionals",
    "3" = "Technicians and Trades Workers",
    "4" = "Community and Personal Service Workers",
    "5" = "Clerical and Administrative Workers",
    "6" = "Sales Workers",
    "7" = "Machinery Operators and Drivers",
    "8" = "Labourers"
  ),
  jbmo62 = c(
    "10" = "Managers",
    "11" = "Chief Executives, General Managers and Legislators",
    "12" = "Farmers and Farm Managers",
    "13" = "Specialist Managers",
    "14" = "Hospitality, Retail and Service Managers",
    "20" = "Professionals",
    "21" = "Arts and Media Professionals",
    "22" = "Business, Human Resource and Marketing Professionals",
    "23" = "Design, Engineering, Science and Transport Professionals",
    "24" = "Education Professionals",
    "25" = "Health Professionals",
    "26" = "ICT Professionals",
    "27" = "Legal, Social and Welfare Professionals",
    "30" = "Technicians and Trades Workers",
    "31" = "Engineering, ICT and Science Technicians",
    "32" = "Automotive and Engineering Trades Workers",
    "33" = "Construction Trades Workers",
    "34" = "Electrotechnology and Telecommunications Trades Workers",
    "35" = "Food Trades Workers",
    "36" = "Skilled Animal and Horticultural Workers",
    "39" = "Other Technicians and Trades Workers",
    "40" = "Community and Personal Service Workers",
    "41" = "Health and Welfare Support Workers",
    "42" = "Carers and Aides",
    "43" = "Hospitality Workers",
    "44" = "Protective Service Workers",
    "45" = "Sports and Personal Service Workers",
    "50" = "Clerical and Administrative Workers",
    "51" = "Office Managers and Program Administrators",
    "52" = "Personal Assistants and Secretaries",
    "53" = "General Clerical Workers",
    "54" = "Inquiry Clerks and Receptionists",
    "55" = "Numerical Clerks",
    "56" = "Clerical and Office Support Workers",
    "59" = "Other Clerical and Administrative Workers",
    "60" = "Sales Workers",
    "61" = "Sales Representatives and Agents",
    "62" = "Sales Assistants and Salespersons",
    "63" = "Sales Support Workers",
    "70" = "Machinery Operators and Drivers",
    "71" = "Machine and Stationary Plant Operators",
    "72" = "Mobile Plant Operators",
    "73" = "Road and Rail Drivers",
    "74" = "Storepersons",
    "80" = "Labourers",
    "81" = "Cleaners and Laundry Workers",
    "82" = "Construction and Mining Labourers",
    "83" = "Factory Process Workers",
    "84" = "Farm, Forestry and Garden Workers",
    "85" = "Food Preparation Assistants",
    "89" = "Other Labourers"
  ),
  jbmi61 = c(
    "1" = "Agriculture, Forestry and Fishing",
    "2" = "Mining",
    "3" = "Manufacturing",
    "4" = "Electricity, Gas, Water and Waste Services",
    "5" = "Construction",
    "6" = "Wholesale Trade",
    "7" = "Retail Trade",
    "8" = "Accommodation and Food Services",
    "9" = "Transport, Postal and Warehousing",
    "10" = "Information Media and Telecommunications",
    "11" = "Financial and Insurance Services",
    "12" = "Rental, Hiring and Real Estate Services",
    "13" = "Professional, Scientific and Technical Services",
    "14" = "Administrative and Support Services",
    "15" = "Public Administration and Safety",
    "16" = "Education and Training",
    "17" = "Health Care and Social Assistance",
    "18" = "Arts and Recreation Services",
    "19" = "Other Services"
  ),
  jbmi62 = c(
    "01" = "Agriculture",
    "02" = "Aquaculture",
    "03" = "Forestry and Logging",
    "04" = "Fishing, Hunting and Trapping",
    "05" = "Agriculture, Forestry and Fishing Support Services",
    "06" = "Coal Mining",
    "07" = "Oil and Gas Extraction",
    "08" = "Metal Ore Mining",
    "09" = "Non-Metallic Mineral Mining and Quarrying",
    "10" = "Exploration and Other Mining Support Services",
    "11" = "Food Product Manufacturing",
    "12" = "Beverage and Tobacco Product Manufacturing",
    "13" = "Textile, Leather, Clothing and Footwear Manufacturing",
    "14" = "Wood Product Manufacturing",
    "15" = "Pulp, Paper and Converted Paper Product Manufacturing",
    "16" = "Printing (including Reproduction of Recorded Media)",
    "17" = "Petroleum and Coal Product Manufacturing",
    "18" = "Basic Chemical and Chemical Product Manufacturing",
    "19" = "Polymer Product and Rubber Product Manufacturing",
    "20" = "Non-Metallic Mineral Product Manufacturing",
    "21" = "Primary Metal and Metal Product Manufacturing",
    "22" = "Fabricated Metal Product Manufacturing",
    "23" = "Transport Equipment Manufacturing",
    "24" = "Machinery and Equipment Manufacturing",
    "25" = "Furniture and Other Manufacturing",
    "26" = "Electricity Supply",
    "27" = "Gas Supply",
    "28" = "Water Supply, Sewerage and Drainage Services",
    "29" = "Waste Collection, Treatment and Disposal Services",
    "30" = "Building Construction",
    "31" = "Heavy and Civil Engineering Construction",
    "32" = "Construction Services",
    "33" = "Basic Material Wholesaling",
    "34" = "Machinery and Equipment Wholesaling",
    "35" = "Motor Vehicle and Motor Vehicle Parts Wholesaling",
    "36" = "Grocery, Liquor and Tobacco Product Wholesaling",
    "37" = "Other Goods Wholesaling",
    "38" = "Commission-Based Wholesaling",
    "39" = "Motor Vehicle and Motor Vehicle Parts Retailing",
    "40" = "Fuel Retailing",
    "41" = "Food Retailing",
    "42" = "Other Store-Based Retailing",
    "43" = "Non-Store Retailing and Retail Commission-Based Buying/Selling",
    "44" = "Accommodation",
    "45" = "Food and Beverage Services",
    "46" = "Road Transport",
    "47" = "Rail Transport",
    "48" = "Water Transport",
    "49" = "Air and Space Transport",
    "50" = "Other Transport",
    "51" = "Postal and Courier Pick-up and Delivery Services",
    "52" = "Transport Support Services",
    "53" = "Warehousing and Storage Services",
    "54" = "Publishing (except Internet and Music Publishing)",
    "55" = "Motion Picture and Sound Recording Activities",
    "56" = "Broadcasting (except Internet)",
    "57" = "Internet Publishing and Broadcasting",
    "58" = "Telecommunications Services",
    "59" = "Internet Service Providers, Web Search Portals and Data Processing Services",
    "60" = "Library and Other Information Services",
    "62" = "Finance",
    "63" = "Insurance and Superannuation Funds",
    "64" = "Auxiliary Finance and Insurance Services",
    "66" = "Rental and Hiring Services (except Real Estate)",
    "67" = "Property Operators and Real Estate Services",
    "69" = "Professional, Scientific and Technical Services (except Computer System Design)",
    "70" = "Computer System Design and Related Services",
    "72" = "Administrative Services",
    "73" = "Building Cleaning, Pest Control and Other Support Services",
    "75" = "Public Administration",
    "76" = "Defence",
    "77" = "Public Order, Safety and Regulatory Services",
    "80" = "Preschool and School Education",
    "81" = "Tertiary Education",
    "82" = "Adult, Community and Other Education",
    "84" = "Hospitals",
    "85" = "Medical and Other Health Care Services",
    "86" = "Residential Care Services",
    "87" = "Social Assistance Services",
    "89" = "Heritage Activities",
    "90" = "Creative and Performing Arts Activities",
    "91" = "Sports and Recreation Activities",
    "92" = "Gambling Activities",
    "94" = "Repair and Maintenance",
    "95" = "Personal and Other Services",
    "96" = "Private Households Employing Staff and Undifferentiated Activities of Households for Own Use"
  ),
  jbmwpsz = c(
    "1" = "Fewer than 5 employees",
    "2" = "5 to 9 employees",
    "3" = "10 to 19 employees",
    "4" = "20 to 49 employees",
    "5" = "50 to 99 employees",
    "6" = "100 to 199 employees",
    "7" = "200 to 499 employees",
    "8" = "500 or more employees",
    "9" = "Don't know, but fewer than 20",
    "10" = "Don't know, but 20 or more"
  ),
  jbmwps = c(
    "1" = "Self only",
    "2" = "2 to 4 employees",
    "3" = "5 to 9 employees",
    "4" = "10 to 19 employees",
    "5" = "20 to 49 employees",
    "6" = "50 to 99 employees",
    "7" = "100 to 199 employees",
    "8" = "200 to 499 employees",
    "9" = "500 or more employees",
    "10" = "Don't know, but fewer than 20",
    "11" = "Don't know, but 20 or more"
  ),
  jbmemsz = c(
    "1" = "Fewer than 20 employees",
    "2" = "20 to 99 employees",
    "3" = "100 to 499 employees",
    "4" = "500 to 999 employees",
    "5" = "1,000 to 4,999 employees",
    "6" = "5,000 to 19,999 employees",
    "7" = "20,000 or more employees",
    "8" = "Don't know, but fewer than 100",
    "9" = "Don't know, but 100 or more"
  ),
  jbmems2 = c(
    "1" = "1 employee",
    "2" = "2 to 4 employees",
    "3" = "5 to 9 employees",
    "4" = "10 to 19 employees",
    "5" = "20 to 99 employees",
    "6" = "100 to 499 employees",
    "7" = "500 to 999 employees",
    "8" = "1,000 to 4,999 employees",
    "9" = "5,000 to 19,999 employees",
    "10" = "20,000 or more employees",
    "11" = "Don't know, but fewer than 20",
    "12" = "Don't know, but fewer than 100",
    "13" = "Don't know, but 100 or more"
  ),
  jbmmwp = c(
    "1" = "Employer operates from more than one location in Australia",
    "2" = "Employer operates from one location in Australia"
  ),
  jbcasab = c(
    "1" = "Casual employee",
    "2" = "Permanent"
  ),
  jbmcnt = c(
    "1" = "Fixed-term contract",
    "2" = "Casual",
    "3" = "Permanent or ongoing",
    "8" = "Other"
  ),
  jbmsch = c(
    "1" = "Regular daytime schedule",
    "2" = "Regular evening shift",
    "3" = "Regular night shift",
    "4" = "Rotating shift",
    "5" = "Split shift",
    "6" = "On call",
    "7" = "Irregular schedule",
    "8" = "Other"
  ),
  wscmef = c(
    "0" = "Not imputed",
    "1" = "Imputed"
  ),
  wscef = c(
    "0" = "Not imputed",
    "1" = "Imputed"
  ),
  biff = c(
    "0" = "Not imputed",
    "1" = "Imputed"
  ),
  jbhrqf = c(
    "0" = "Main job hours < all jobs hours",
    "1" = "Main job hours = all jobs hours",
    "2" = "Main job hours > all jobs hours"
  ),
  bifhave = c(
    "1" = "Yes",
    "2" = "No",
    "3" = "No (both business questions)"
  ),
  jbmmplr = c(
    "1" = "Private sector for profit organisation",
    "2" = "Private sector not for profit organisation",
    "3" = "Government business enterprise or commercial statutory authority",
    "4" = "Other commercial",
    "5" = "Other governmental organisation",
    "6" = "Other non-commercial"
  ),
  jbmmpl = c(
    "1" = "Private sector for profit organisation",
    "2" = "Private sector not for profit organisation",
    "3" = "Government business enterprise or commercial statutory authority",
    "4" = "Other commercial",
    "5" = "Other governmental organisation",
    "6" = "Other non-commercial"
  ),
  jbmmply = c(
    "1" = "Private sector for profit organisation",
    "2" = "Government business enterprise or commercial statutory authority",
    "3" = "Other commercial",
    "4" = "Private sector not for profit organisation",
    "5" = "Other governmental organisation",
    "6" = "Other non-commercial"
  ),
  jbmday = c(
    "1" = "Monday to Friday",
    "2" = "Nine day fortnight",
    "3" = "Days vary from week to week",
    "4" = "Days vary from month to month",
    "8" = "Other"
  ),
  jbmsl = c(
    "1" = "Employer provides paid sick leave",
    "2" = "Employer does not provide paid sick leave"
  ),
  jbmsvsr = c(
    "1" = "Has supervisory responsibilities",
    "2" = "No supervisory responsibilities"
  ),
  jbmtuea = c(
    "2" = "No",
    "1" = "Yes"
  ),
  jbn = c(
    "1" = "More than one job",
    "2" = "Only one job"
  ),
  jbnewjs = c(
    "1" = "Has searched for a new job in last 4 weeks",
    "2" = "Has not searched in last 4 weeks"
  ),
  jbhrcpr = c(
    "1" = "Prefer fewer hours",
    "2" = "About the same",
    "3" = "Prefer more hours"
  ),
  lefni = c(
    "1" = "No",
    "2" = "Yes"
  ),
  lefnw = c(
    "1" = "No",
    "2" = "Yes"
  ),
  lefrd = c(
    "1" = "No",
    "2" = "Yes"
  )
)

# New naming convention: [new short name]_[original variable name in HILDA]
hilda_rename <- c(
  hhstate = "state_hhstate",
  hhs3ra = "remoteness_hhs3ra",
  hhs3gcc = "statistical_region_hhs3gcc",
  hstenr = "housing_tenure_hstenr",
  hsvalui = "home_value_hsvalui",
  hsmgowe = "home_loan_outstanding_hsmgowe",
  hsdebt = "total_home_debt_hsdebt",
  hwhmhl = "apportioned_home_loan_hwhmhl",
  hsmgpd = "completely_paid_off_home_loans_hsmgpd",
  hsrntby = "involved_in_rent_buy_arrangement_hsrntby",
  hsmguse = "loan_or_mortgage_hsmguse",
  hsmg = "mortgage_usual_repayment_month_hsmg",
  hsmgi = "imputed_mortgage_usual_repayment_month_hsmgi",
  hsrnt = "rent_usual_repayment_month_hsrnt",
  hsrnti = "imputed_rent_usual_repayment_month_hsrnti",
  hsyr = "year_purchased_current_home_hsyr",
  fiprbeg = "cannot_pay_bills_on_time_fiprbeg",
  fiprbmr = "cannot_pay_mortgage_rent_on_time_fiprbmr",
  fiprbfh = "asked_financial_help_friends_family_fiprbfh",
  fiprbwo = "asked_financial_help_welfare_community_fiprbwo",
  fiprbps = "pawned_or_sold_something_fiprbps",
  fiprbuh = "unable_heat_home_fiprbuh",
  fiprbwm = "went_without_meals_fiprbwm",
  fiprosp = "prosperity_given_needs_and_financial_responsibilities_fiprosp",
  fiemerf = "difficulty_raise_emergency_funds_fiemerf", # (waves 1-8) $2,000 (waves 9-19) $3,000 (waves 20+) $4,000
  hhs3add = "disadvantage_decile_hhs3add",
  hgsex = "sex_hgsex",
  mrcurr = "marital_status_mrcurr",
  anbcob = "birthplace_group_anbcob",
  aneab = "english_proficiency_aneab",
  hgeab = "english_proficiency_hgeab",
  anengf = "english_first_lang_anengf",
  anmigc = "visa_category_anmigc",
  anrefn = "refugee_migrant_anrefn",
  anlote = "speaks_lote_home_anlote",
  sktarea = "english_reading_skill_sktarea",
  edsscmp = "highest_school_edsscmp",
  edlhqn = "highest_qual_edlhqn",
  edcoq = "edu_country_edcoq",
  edpsqau = "postschool_qual_aus_edpsqau",
  edhigh1 = "highest_education_achieved_edhigh1",
  es = "worker_type_es",
  esbrd = "lf_status_broad_esbrd",
  esempdt = "emp_status_detail_esempdt",
  esdtl = "lf_status_detail_esdtl",
  jbmi61 = "industry_19categories_jbmi61",
  jbmi62 = "industry_sparsecategories_jbmi62",
  jbmo61 = "occupation_1dig_jbmo61",
  jbmo62 = "occupation_2dig_jbmo62",
  jbmwpsz = "workplace_size_jbmwpsz",
  jbmwps = "workplace_size_jbmwps",
  jbmemsz = "employer_size_jbmemsz",
  jbmems2 = "employer_size_jbmems2",
  jbmmwp = "multi_site_jbmmwp",
  jbcasab = "casual_perm_jbcasab",
  jbmcnt = "contract_type_jbmcnt",
  jbmsch = "work_schedule_jbmsch",
  bifhave = "owns_business_bifhave",
  jbhrqf = "hours_main_vs_all_job_jbhrqf",
  wscmef = "impute_flag_wage_weekly_main_job_wscmef",
  wscef = "impute_flag_wage_weekly_all_job_wscef",
  biff = "impute_flag_fy_biz_income_biff",
  tifeftf = "impute_flag_fy_income_tifeftf",
  hgage = "age_hgage",
  ancob = "birthplace_country_ancob",
  anyoa = "year_arrival_aust_anyoa",
  jbemlyr = "year_worked_current_employer_jbemlyr",
  jbocct = "tenure_current_occ_jbocct",
  jbempt = "tenure_current_employer_jbempt",
  wscmei = "wage_weekly_main_job_wscmei",
  wscei = "wage_weekly_all_job_wscei",
  jbmhruc = "hours_weekly_main_job_jbmhruc",
  jbhruc = "hours_weekly_all_job_jbhruc",
  tifefp = "fy_income_pos_tifefp",
  tifefn = "fy_income_neg_tifefn",
  bifip = "fy_biz_income_pos_bifip",
  bifin = "fy_biz_income_neg_bifin",
  bifp = "fy_biz_income_noimpute_pos_bifp",
  bifn = "fy_biz_income_noimpute_neg_bifn",
  wsfei = "fy_gross_wages_salaries_wsfei",
  hhwth = "householdpop_crosssectional_weight_hhwth",
  hhwtrp = "respondingperson_crosssectional_weight_hhwtrp",
  hhwte = "enumerated_person_population_weight",
  lnwtrp = "responding_person_longitudinal_weight_lnwtrp",
  ehtujyr = "years_unemployed_looking_for_work_ehtujyr",
  ehtsemt = "months_since_fulltime_education_ehtsemt",
  jbmmplr = "employer_sector_jbmmplr",
  jbmmpl = "employer_sector_jbmmpl",
  jbmmply = "employer_sector_jbmmply",
  jbmday = "work_schedule_jbmday",
  jbmo6s = "occ_socioecon_status_ausei06_jbmo6s",
  jbmsl = "paid_sick_leave_jbmsl",
  jbmsvsr = "supervisory_responsibilities_jbmsvsr",
  jbmtuea = "union_employeeassociation_membership_jbmtuea",
  jbn = "multiple_jobs_jbn",
  jbmpgj = "pct_find_equal_job_jbmpgj",
  jbmplej = "pct_leave_job_12m_jbmplej",
  jbmploj = "pct_lose_job_12m_jbmploj",
  jbnewjs = "searched_new_job_4wk_jbnewjs",
  jbhrcpr = "hours_preference_jbhrcpr",
  jbmsall = "satisfaction_job_overall_jbmsall",
  jbmsflx = "satisfaction_flexibility_jbmsflx",
  jbmshrs = "satisfaction_hours_jbmshrs",
  jbmspay = "satisfaction_pay_jbmspay",
  jbmssec = "satisfaction_job_security_jbmssec",
  jbmswrk = "satisfaction_work_itself_jbmswrk",
  lefni = "finances_improved__life_event_lefni",
  lefnw = "finances_worsened_life_event_lefnw",
  lefrd = "fired_redundant_life_event_lefrd",
  hifdip = "hh_disposable_income_pos_hifdip",
  hifdin = "hh_disposable_income_neg_hifdin"
)

hilda_ordered_vars <- c("hhs3add") # labelled variables to be made ordered factors

# Variables factorised straight from their integer codes
# Note: factorise_asis() runs BEFORE rename_hilda()
hilda_factor_asis <- c(
  "sktarea",
  "edcoq",
  "jbmsall",
  "jbmsflx",
  "jbmshrs",
  "jbmspay",
  "jbmssec",
  "jbmswrk"
)
hilda_factor_asis_ord <- c(
  "sktarea", # 0 very poor - 10 very good
  "jbmsall",
  "jbmsflx",
  "jbmshrs",
  "jbmspay",
  "jbmssec",
  "jbmswrk" # 0 totally dissatisfied - 10 totally satisfied
)


# --- Helper functions -------------------------------------------------------

# Helper function: rename selected variables
apply_hilda_labels <- function(
  data,
  labels = hilda_labels,
  ordered = hilda_ordered_vars,
  suffix = NULL,
  report = TRUE,
  lower = TRUE
) {
  vars <- intersect(names(labels), names(data))
  if (length(vars) == 0L) {
    warning("No labelled variables found.")
    return(data)
  }
  unmapped_log <- list()
  for (v in vars) {
    map <- labels[[v]]
    codes <- suppressWarnings(as.numeric(names(map)))
    labs <- unname(map)
    if (lower) {
      labs <- tolower(labs)
    }
    raw <- data[[v]]
    um <- setdiff(unique(raw[!is.na(raw)]), codes)
    if (length(um) > 0L) {
      unmapped_log[[v]] <- sort(um)
    }
    f <- factor(raw, levels = codes, labels = labs, ordered = v %in% ordered)
    data[[if (is.null(suffix)) v else paste0(v, suffix)]] <- f
  }
  if (report) {
    cat("Labelled", length(vars), "variable(s).\n")
    if (length(unmapped_log) > 0L) {
      cat("Codes not in dictionary (set to NA):\n")
      for (v in names(unmapped_log)) {
        cat(
          "  ",
          v,
          ": ",
          paste(unmapped_log[[v]], collapse = ", "),
          "\n",
          sep = ""
        )
      }
    }
  }
  data
}

# Helper function: factorise categorical variables "as is" and drop negatives to NA according to HILDA's special missing codes
factorise_asis <- function(
  data,
  vars = hilda_factor_asis,
  ordered = hilda_factor_asis_ord,
  suffix = NULL,
  keep_negatives = FALSE
) {
  vars <- intersect(vars, names(data))
  for (v in vars) {
    x <- data[[v]]
    if (!keep_negatives) {
      x[!is.na(x) & x < 0] <- NA
    }
    lev <- sort(unique(x[!is.na(x)]))
    f <- factor(x, levels = lev, ordered = v %in% ordered)
    data[[if (is.null(suffix)) v else paste0(v, suffix)]] <- f
  }
  if (length(vars) > 0L) {
    cat("Factorised as-is:", paste(vars, collapse = ", "), "\n")
  }
  data
}

# Helper function: rename all variables to new format [new short name]_[original variable name in HILDA]
rename_hilda <- function(data, rename = hilda_rename) {
  present <- intersect(names(rename), names(data))
  names(data)[match(present, names(data))] <- rename[present]
  data
}

# --- Build the cleaned panel --------------------------------------------------

panel_clean <- panel_raw |>
  mutate(across(
    any_of(vars_pull),
    ~ replace(as.numeric(.x), as.numeric(.x) %in% hilda_missing_codes, NA_real_)
  )) |>
  # Positive-valued special codes on the percentage/chance items to NA
  mutate(across(any_of(pct_chance_vars), ~ replace(.x, .x > 100, NA_real_))) |>
  apply_hilda_labels() |>
  factorise_asis() |>
  rename_hilda()

# ==============================================================================
# End of 02_datacleaning.R
# ==============================================================================

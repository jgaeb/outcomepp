library(groundhog)
groundhog.library("
  xgboost
  tidymodels
  fs
  tidyverse
", "2024-07-04")

################################################################################
# Setup the environment.

source("setup.R")

DATASET <- "sqf"

################################################################################
# Load and clean the SQF data

YN = c(
  "Y" = "yes",
  "N" = "no"
)

INOUT = c(
  "I" = "inside",
  "O" = "outside"
)

TRHSLOC = c(
  "H" = "housing",
  "N" = "neither",
  "P" = "housing",  # NOTE: This is a typo in the original data
  "T" = "transit"
)

SEX = c(
  "F" = "FEMALE",
  "M" = "MALE",
  "Z" = "UNKNOWN"
)

HAIRCOLR = c(
  "BA"	= "bald",
  "BK"	= "black",
  "BL"	= "blond",
  "BR"	= "brown",
  "DY"	= "dyed",
  "FR"	= "frosted",
  "GY"	= "gray",
  "RA"  = "red",  # NOTE: This is a typo in the original data
  "RD"	= "red",
  "SN"	= "sandy",
  "SP"	= "salt and pepper",
  "WH"	= "white",
  "XX"	= "unknown",
  "ZZ"	= "other"
)

EYECOLOR = c(
  "BK" = "black",
  "BL" = "blue",
  "BR" = "brown",
  "DF" = "two different",
  "GR" = "green",
  "GY" = "gray",
  "HA" = "hazel",
  "MA" = "maroon",
  "MC" = "two different", # NOTE: This is a typo in the data
  "PK" = "pink",
  "P"  = "pink", # NOTE: This is a typo in the data
  "VI" = "violet",
  "XX" = "unknown",
  "ZZ" = "other",
  "Z"  = "other" # NOTE: This is a typo in the data
)

BUILD = c(
  "H"  = "heavy",
  "M"  = "medium",
  "T"  = "thin",
  "U"  = "muscular",
  "Z"  = "unknown"
)

DETAILCM = c(
  "1"   = "abandonment of a child",
  "2"   = "abortion",
  "3"   = "absconding",
  "4"   = "adultery",
  "5"   = "aggravated assault",
  "6"   = "aggravated harassment",
  "7"   = "aggravated sexual abuse",
  "8"   = "arson",
  "9"   = "assault",
  "10"  = "auto stripping",
  "11"  = "bigamy",
  "12"  = "bribe receiving",
  "13"  = "bribery",
  "14"  = "burglary",
  "15"  = "coercion",
  "16"  = "computer tampering",
  "17"  = "computer trespass",
  "18"  = "course of sexual conduct",
  "19"  = "cpsp",
  "20"  = "cpw",
  "21"  = "creating a hazard",
  "22"  = "criminal contempt",
  "23"  = "criminal mischief",
  "24"  = "criminal possesion of controlled substance",
  "25"  = "criminal possession of computer material",
  "26"  = "criminal possession of forged instrument",
  "27"  = "criminal possession of marihuana",
  "28"  = "criminal sale of controlled substance",
  "29"  = "criminal sale of marihuana",
  "30"  = "criminal tampering",
  "31"  = "criminal trespass",
  "32"  = "custodial interference",
  "33"  = "eaves dropping",
  "34"  = "endanger the welfare of a child",
  "35"  = "escape",
  "36"  = "falsify business records",
  "37"  = "forgery",
  "38"  = "forgery of a vin",
  "39"  = "fortune telling",
  "40"  = "fraud",
  "41"  = "fraudulent accosting",
  "42"  = "fraudulent make electronic access device",
  "43"  = "fraudulent obtaining a signature",
  "44"  = "gambling",
  "45"  = "grand larceny",
  "46"  = "grand larceny auto",
  "47"  = "harassment",
  "48"  = "hazing",
  "49"  = "hindering prosecution",
  "50"  = "incest",
  "51"  = "insurance fraud",
  "52"  = "issue a false certificate",
  "53"  = "issue a false financial statement",
  "54"  = "issuing abortion articles",
  "55"  = "jostling",
  "56"  = "kidnapping",
  "57"  = "killing or injuring a poilce animal",
  "58"  = "loitering",
  "59"  = "making graffiti",
  "60"  = "menacing",
  "61"  = "misapplication of property",
  "62"  = "murder",
  "63"  = "obscenity",
  "64"  = "obstructing firefighting operations",
  "65"  = "obstructing governmental administration",
  "66"  = "offering a false instrument",
  "67"  = "official misconduct",
  "68"  = "petit larceny",
  "69"  = "possession of burglar tools",
  "70"  = "possession of eaves dropping devices",
  "71"  = "possession of graffiti instruments",
  "72"  = "prohibited use of weapon",
  "73"  = "promoting suicide",
  "74"  = "prostitution",
  "75"  = "public display of offensive sexual material",
  "76"  = "public lewdness",
  "77"  = "rape",
  "78"  = "reckless endangerment",
  "79"  = "reckless endangerment property",
  "80"  = "refusing to aid a peace or police officer",
  "81"  = "rent gouging",
  "82"  = "resisting arrest",
  "83"  = "reward official misconduct",
  "84"  = "riot",
  "85"  = "robbery",
  "86"  = "self abortion",
  "87"  = "sexual abuse",
  "88"  = "sexual misconduct",
  "89"  = "sexual performance by a child",
  "90"  = "sodomy",
  "91"  = "substitution of children",
  "92"  = "tampering with a public record",
  "93"  = "tampering with consumer product",
  "94"  = "tampering with private communications",
  "95"  = "terrorism",
  "96"  = "theft of services",
  "97"  = "trademark counterfeiting",
  "98"  = "unlawfully dealing with fireworks",
  "99"  = "unauthorized recording",
  "100" = "unauthorized use of a vehicle",
  "101" = "unauthorized use of computer",
  "102" = "unlawful assembly",
  "103" = "unlawful duplication of computer material",
  "104" = "unlawful possession of radio devices",
  "105" = "unlawful use of credit card, debit card",
  "106" = "unlawful use of secret scientific material",
  "107" = "unlawful wearing a body vest",
  "108" = "unlawfull imprisonment",
  "109" = "unlawfully dealing with a child",
  "110" = "unlawfully use slugs",
  "111" = "vehicular assault",
  "112" = "other",
  "113" = "forcible touching"
)

COLS <- cols(
  year 	    = col_integer(),                                # YEAR OF STOP (CCYY)
  pct 	    = col_factor(levels = as.character(seq(123))),  # PRECINCT OF STOP (FROM 1 TO 123)
  ser_num 	= col_skip(),                                   # UF250 SERIAL NUMBER
  datestop 	= col_character(),                              # DATE OF STOP (MM-DD-YYYY)
  timestop 	= col_character(),                              # TIME OF STOP (HH:MM)
  recstat 	= col_skip(),                                   # RECORD STATUS
  inout 	  = col_factor(levels = names(INOUT)),            # WAS STOP INSIDE OR OUTSIDE ?
  trhsloc 	= col_factor(levels = names(TRHSLOC)),          # WAS LOCATION HOUSING OR TRANSIT AUTHORITY ?
  perobs 	  = col_skip(),                                   # PERIOD OF OBSERVATION (MMM)
  crimsusp 	= col_skip(),                                   # CRIME SUSPECTED
  perstop 	= col_skip(),                                   # PERIOD OF STOP (MMM)
  typeofid 	= col_skip(),                                   # STOPPED PERSON'S IDENTIFICATION TYPE
  explnstp 	= col_skip(),                                   # DID OFFICER EXPLAIN REASON FOR STOP ?
  othpers 	= col_skip(),                                   # WERE OTHER PERSONS STOPPED, QUESTIONED OR FRISKED ?
  arstmade 	= col_skip(),                                   # WAS AN ARREST MADE ?
  arstoffn 	= col_skip(),                                   # OFFENSE SUSPECT ARRESTED FOR
  sumissue 	= col_skip(),                                   # WAS A SUMMONS ISSUED ?
  sumoffen 	= col_skip(),                                   # OFFENSE SUSPECT WAS SUMMONSED FOR
  compyear 	= col_skip(),                                   # COMPLAINT YEAR (IF COMPLAINT REPORT PREPARED)
  comppct 	= col_skip(),                                   # COMPLAINT PRECINCT (IF COMPLAINT REPORT PREPARED)
  offunif 	= col_skip(),                                   # WAS OFFICER IN UNIFORM ?
  officrid 	= col_skip(),                                   # ID CARD PROVIDED BY OFFICER (IF NOT IN UNIFORM)
  frisked 	= col_character(),                              # WAS SUSPECT FRISKED ?
  searched 	= col_skip(),                                   # WAS SUSPECT SEARCHED ?
  contrabn 	= col_skip(),                                   # WAS CONTRABAND FOUND ON SUSPECT ?
  adtlrept 	= col_skip(),                                   # WERE ADDITIONAL REPORTS PREPARED ?
  pistol 	  = col_factor(levels = names(YN)),               # WAS A PISTOL FOUND ON SUSPECT ?
  riflshot 	= col_factor(levels = names(YN)),               # WAS A RIFLE FOUND ON SUSPECT ?
  asltweap 	= col_factor(levels = names(YN)),               # WAS AN ASSAULT WEAPON FOUND ON SUSPECT ?
  knifcuti 	= col_factor(levels = names(YN)),               # WAS A KNIFE OR CUTTING INSTRUMENT FOUND ON SUSPECT ?
  machgun 	= col_factor(levels = names(YN)),               # WAS A MACHINE GUN FOUND ON SUSPECT ?
  othrweap 	= col_factor(levels = names(YN)),               # WAS ANOTHER TYPE OF WEAPON FOUND ON SUSPECT ?
  pf_hands 	= col_skip(),                                   # PHYSICAL FORCE USED BY OFFICER - HANDS
  pf_wall 	= col_skip(),                                   # PHYSICAL FORCE USED BY OFFICER - SUSPECT AGAINST WALL
  pf_grnd 	= col_skip(),                                   # PHYSICAL FORCE USED BY OFFICER - SUSPECT ON GROUND
  pf_drwep 	= col_skip(),                                   # PHYSICAL FORCE USED BY OFFICER - WEAPON DRAWN
  pf_ptwep 	= col_skip(),                                   # PHYSICAL FORCE USED BY OFFICER - WEAPON POINTED
  pf_baton 	= col_skip(),                                   # PHYSICAL FORCE USED BY OFFICER - BATON
  pf_hcuff 	= col_skip(),                                   # PHYSICAL FORCE USED BY OFFICER - HANDCUFFS
  pf_pepsp 	= col_skip(),                                   # PHYSICAL FORCE USED BY OFFICER - PEPPER SPRAY
  pf_other 	= col_skip(),                                   # PHYSICAL FORCE USED BY OFFICER - OTHER
  radio 	  = col_skip(),                                   # RADIO RUN
  ac_rept 	= col_factor(levels = names(YN)),               # ADDITIONAL CIRCUMSTANCES - REPORT BY VICTIM/WITNESS/OFFICER
  ac_inves 	= col_factor(levels = names(YN)),               # ADDITIONAL CIRCUMSTANCES - ONGOING INVESTIGATION
  rf_vcrim 	= col_factor(levels = names(YN)),               # REASON FOR FRISK - VIOLENT CRIME SUSPECTED
  rf_othsw 	= col_factor(levels = names(YN)),               # REASON FOR FRISK - OTHER SUSPICION OF WEAPONS
  ac_proxm 	= col_factor(levels = names(YN)),               # ADDITIONAL CIRCUMSTANCES - PROXIMITY TO SCENE OF OFFENSE
  rf_attir 	= col_factor(levels = names(YN)),               # REASON FOR FRISK - INAPPROPRIATE ATTIRE FOR SEASON
  cs_objcs 	= col_factor(levels = names(YN)),               # REASON FOR STOP - CARRYING SUSPICIOUS OBJECT
  cs_descr 	= col_factor(levels = names(YN)),               # REASON FOR STOP - FITS A RELEVANT DESCRIPTION
  cs_casng 	= col_factor(levels = names(YN)),               # REASON FOR STOP - CASING A VICTIM OR LOCATION
  cs_lkout 	= col_factor(levels = names(YN)),               # REASON FOR STOP - SUSPECT ACTING AS A LOOKOUT
  rf_vcact 	= col_factor(levels = names(YN)),               # REASON FOR FRISK-  ACTIONS OF ENGAGING IN A VIOLENT CRIME
  cs_cloth 	= col_factor(levels = names(YN)),               # REASON FOR STOP - WEARING CLOTHES COMMONLY USED IN A CRIME
  cs_drgtr 	= col_factor(levels = names(YN)),               # REASON FOR STOP - ACTIONS INDICATIVE OF A DRUG TRANSACTION
  ac_evasv 	= col_factor(levels = names(YN)),               # ADDITIONAL CIRCUMSTANCES - EVASIVE RESPONSE TO QUESTIONING
  ac_assoc 	= col_factor(levels = names(YN)),               # ADDITIONAL CIRCUMSTANCES - ASSOCIATING WITH KNOWN CRIMINALS
  cs_furtv 	= col_factor(levels = names(YN)),               # REASON FOR STOP - FURTIVE MOVEMENTS
  rf_rfcmp 	= col_factor(levels = names(YN)),               # REASON FOR FRISK - REFUSE TO COMPLY W OFFICER'S DIRECTIONS
  ac_cgdir 	= col_factor(levels = names(YN)),               # ADDITIONAL CIRCUMSTANCES - CHANGE DIRECTION AT SIGHT OF OFFICER
  rf_verbl 	= col_factor(levels = names(YN)),               # REASON FOR FRISK - VERBAL THREATS BY SUSPECT
  cs_vcrim 	= col_factor(levels = names(YN)),               # REASON FOR STOP - ACTIONS OF ENGAGING IN A VIOLENT CRIME
  cs_bulge 	= col_factor(levels = names(YN)),               # REASON FOR STOP - SUSPICIOUS BULGE
  cs_other 	= col_factor(levels = names(YN)),               # REASON FOR STOP - OTHER
  ac_incid 	= col_factor(levels = names(YN)),               # ADDITIONAL CIRCUMSTANCES - AREA HAS HIGH CRIME INCIDENCE
  ac_time 	= col_factor(levels = names(YN)),               # ADDITIONAL CIRCUMSTANCES - TIME OF DAY FITS CRIME INCIDENCE
  rf_knowl 	= col_factor(levels = names(YN)),               # REASON FOR FRISK - KNOWLEDGE OF SUSPECT'S PRIOR CRIM BEHAV
  ac_stsnd 	= col_factor(levels = names(YN)),               # ADDITIONAL CIRCUMSTANCES - SIGHTS OR SOUNDS OF CRIMINAL ACTIVITY
  ac_other 	= col_factor(levels = names(YN)),               # ADDITIONAL CIRCUMSTANCES - OTHER
  sb_hdobj 	= col_skip(),                                   # BASIS OF SEARCH - HARD OBJECT
  sb_outln 	= col_skip(),                                   # BASIS OF SEARCH - OUTLINE OF WEAPON
  sb_admis 	= col_skip(),                                   # BASIS OF SEARCH - ADMISSION BY SUSPECT
  sb_other 	= col_skip(),                                   # BASIS OF SEARCH - OTHER
  repcmd 	  = col_skip(),                                   # REPORTING OFFICER'S COMMAND (1 TO 999)
  revcmd 	  = col_skip(),                                   # REVIEWING OFFICER'S COMMAND (1 TO 999)
  rf_furt 	= col_factor(levels = names(YN)),               # REASON FOR FRISK - FURTIVE MOVEMENTS
  rf_bulg 	= col_factor(levels = names(YN)),               # REASON FOR FRISK - SUSPICIOUS BULGE
  offverb 	= col_skip(),                                   # VERBAL STATEMENT PROVIDED BY OFFICER (IF NOT IN UNIFORM)
  offshld 	= col_skip(),                                   # SHIELD PROVIDED BY OFFICER (IF NOT IN UNIFORM)
  sex 	    = col_factor(levels = names(SEX)),              # SUSPECT'S SEX
  race 	    = col_character(),                              # SUSPECT'S RACE
  dob 	    = col_skip(),                                   # SUSPECT'S DATE OF BIRTH (CCYY-MM-DD)
  age 	    = col_integer(),                                # SUSPECT'S AGE
  ht_feet 	= col_integer(),                                # SUSPECT'S HEIGHT (FEET)
  ht_inch 	= col_integer(),                                # SUSPECT'S HEIGHT (INCHES)
  weight 	  = col_integer(),                                # SUSPECT'S WEIGHT
  haircolr 	= col_factor(levels = names(HAIRCOLR)),         # SUSPECT'S HAIRCOLOR
  eyecolor 	= col_factor(levels = names(EYECOLOR)),         # SUSPECT'S EYE COLOR
  build 	  = col_factor(levels = names(BUILD)),            # SUSPECT'S BUILD
  othfeatr 	= col_skip(),                                   # SUSPECT'S OTHER FEATURES (SCARS, TATOOS ETC.)
  addrtyp 	= col_skip(),                                   # LOCATION OF STOP ADDRESS TYPE
  rescode 	= col_skip(),                                   # LOCATION OF STOP RESIDENT CODE
  premtype 	= col_skip(),                                   # LOCATION OF STOP PREMISE TYPE
  premname 	= col_skip(),                                   # LOCATION OF STOP PREMISE NAME
  addrnum 	= col_skip(),                                   # LOCATION OF STOP ADDRESS NUMBER
  stname 	  = col_skip(),                                   # LOCATION OF STOP STREET NAME
  stinter 	= col_skip(),                                   # LOCATION OF STOP INTERSECTION
  crossst 	= col_skip(),                                   # LOCATION OF STOP CROSS STREET
  aptnum 	  = col_skip(),                                   # LOCATION OF STOP APT NUMBER
  city 	    = col_skip(),                                   # LOCATION OF STOP CITY
  state 	  = col_skip(),                                   # LOCATION OF STOP STATE
  zip 	    = col_skip(),                                   # LOCATION OF STOP ZIP CODE
  addrpct 	= col_skip(),                                   # LOCATION OF STOP ADDRESS PRECINCT
  sector 	  = col_skip(),                                   # LOCATION OF STOP SECTOR
  beat 	    = col_skip(),                                   # LOCATION OF STOP BEAT
  post 	    = col_skip(),                                   # LOCATION OF STOP POST
  xcoord 	  = col_skip(),                                   # LOCATION OF STOP X COORD
  ycoord 	  = col_skip(),                                   # LOCATION OF STOP Y COORD
  dettypcm 	= col_skip(),                                   # DETAILS TYPES CODE
  linecm 	  = col_skip(),                                   # COUNT >1 ADDITIONAL DETAILS
  detailcm 	= col_factor(levels = names(DETAILCM))          # CRIME CODE DESCRIPTION
)

################################################################################

FEATS <- c(
  "race",
  "detailcm",
  "month",
  "hour",
  "precinct",
  "inout",
  "trhsloc",
  "sex",
  "age",
  "ht_feet",
  "ht_inch",
  "weight",
  "haircolr",
  "eyecolor",
  "build",
  "cs_descr",
  "cs_casng",
  "cs_lkout",
  "cs_cloth",
  "cs_drgtr",
  "cs_furtv",
  "cs_bulge",
  "cs_other",
  "ac_rept",
  "ac_inves",
  "ac_proxm",
  "ac_evasv",
  "ac_assoc",
  "ac_cgdir",
  "ac_incid",
  "ac_time",
  "ac_stsnd",
  "ac_other"
)

################################################################################
# Convenience function for loading SQF data
read_sqf <- function(filepath) {
  read_csv(filepath, col_types = COLS, progress = FALSE) %>%
    filter(race %in% c("B", "P", "Q", "W")) %>%
    mutate(
      i = row_number(),
      # Parse datestop
      # NOTE: Some dates need to be left-padded with a zero
      datestop = str_pad(datestop, 8, pad = "0"),
      # NOTE: Insert a hyphen between the month, day, and year
      datestop = str_c(
        str_sub(datestop, 1, 2),
        "-",
        str_sub(datestop, 3, 4),
        "-",
        str_sub(datestop, 5, 8)
      ),
      datestop = mdy(datestop, quiet = TRUE),
      # Parse timestop
      # NOTE: Some times need to be left-padded with a zero
      timestop = str_pad(timestop, 4, pad = "0"),
      # NOTE: Insert a colon between the hour and minute
      timestop = str_c(
        str_sub(timestop, 1, 2),
        ":",
        str_sub(timestop, 3, 4)
      ),
      timestop = hm(timestop, quiet = TRUE),
      # Add time and time of year features
      hour     = hour(timestop),
      month    = month(datestop),
      # Add search feature
      frisked  = frisked == "Y",
      # Add explicit NA to trhsloc
      trhsloc  = fct_na_value_to_level(trhsloc, level = "N"),
      race     = case_when(
        race == "B" ~ "b",
        race == "P" ~ "h",
        race == "Q" ~ "h",
        race == "W" ~ "w"
      ),
      race     = factor(race, c("b", "h", "w")),
      # Correct labels of factor levels
      inout    = fct_relabel(inout, ~ INOUT[.]),
      trhsloc  = fct_relabel(trhsloc, ~ TRHSLOC[.]),
      sex      = fct_relabel(sex, ~ SEX[.]),
      haircolr = fct_relabel(haircolr, ~ HAIRCOLR[.]),
      eyecolor = fct_relabel(eyecolor, ~ EYECOLOR[.]),
      build    = fct_relabel(build, ~ BUILD[.]),
      detailcm = fct_relabel(detailcm, ~ DETAILCM[.]),
      mutate(across(
        c(starts_with("cs_"), starts_with("rf_"), starts_with("ac_")),
        ~ fct_relabel(., ~ YN[.])
      )),
      # Lump together rare factor levels
      haircolr = fct_lump_min(haircolr, 100, other_level = "other"),
      eyecolor = fct_lump_min(eyecolor, 100, other_level = "other"),
      build    = fct_lump_min(build, 100, other_level = "other"),
      detailcm = fct_lump_min(detailcm, 100, other_level = "other"),
      # Add outcome
      outcome  = (pistol == "Y" | riflshot == "Y" | asltweap == "Y"
                | knifcuti == "Y" | machgun == "Y" | othrweap == "Y"),
      # Add a factor for whether a weapon was found for the model
      outcomef = factor(outcome, c("TRUE", "FALSE"))
    ) %>%
    select(
      # Basic info
      year,
      i,
      precinct = pct,
      date = datestop,
      time = timestop,
      # Actions and outcomes
      frisked,
      outcome,
      outcomef,
      # Features
      # (Suspected crime)
      detailcm,
      # (Time and time of year)
      hour,
      month, 
      # (Location)
      inout,
      trhsloc,
      # (Suspect description)
      sex,
      race,
      age,
      ht_feet,
      ht_inch,
      weight,
      haircolr,
      eyecolor,
      build,
      # (Reasons for stop)
      starts_with("cs_"),
      # (Reasons for frisk)
      starts_with("rf_"),
      # (Additional circumstances)
      starts_with("ac_")
    )
}

# Load the training and test data
df <- dir_ls(path("data", "sqf"), glob = "*.csv") %>%
  map(read_sqf) %>%
  list_rbind() %>%
  # NOTE: 12 rows are missing a time
  drop_na()
train <- filter(df, year %in% c("2008", "2009", "2010"), frisked) %>%
  sample_frac(0.8)
eval  <- filter(df, year %in% c("2008", "2009", "2010"), frisked) %>%
  anti_join(train, by = "i")
test  <- filter(df, year %in% c("2011", "2012"))

rm(df)

################################################################################
# Model risk.

# Fit a GBM to predict risk.
# NOTE: We stop early if the model fails to improve for twenty iterations, as
#       calculated on an (internal) 10% validation set.
m_risk <- boost_tree(
    trees = 10000,
    tree_depth = 2,
    learn_rate = 0.1,
    stop_iter = 20
  ) %>%
  set_engine("xgboost", nthread = 8, validation = 0.1) %>%
  set_mode("classification") %>%
  fit(reformulate(FEATS, "outcomef"), data = train)

# Add predictions to the test data.
df <- predict(m_risk, new_data = test, type = "prob") %>%
  bind_cols(test) %>%
  select(outcome, risk = .pred_TRUE, race)

rm(train, test)

################################################################################
# Generate the data needed for the monotonicity plots.

# Convenience function for calculating the proportion of people belonging to
# each group at a given risk level.
density_ratio <- function(df, n_buckets) {
  df %>%
    group_by(bucket = ntile(risk, {{ n_buckets }})) %>%
    summarize(
      p_b = mean(race == "b"),
      p_h = mean(race == "h"),
      p_w = mean(race == "w"),
      r_b = p_w / (p_w + p_b),
      r_h = p_w / (p_w + p_h)
    )
}

source("monotonicity.R")

################################################################################
# Run the simulation.

source("simulation.R")

################################################################################
# Perform model checks.

df <- predict(m_risk, new_data = eval, type = "prob") %>%
  bind_cols(eval) %>%
  select(outcome = outcomef, risk = .pred_TRUE, race)

source("model_checks.R")

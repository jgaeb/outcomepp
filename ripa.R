library(groundhog)
groundhog.library("
  glue
  fs
  readxl
  tidyverse
", "2024-07-04")

################################################################################

COL_TYPES <- c(
  "skip",     # A  # DOI_RECORD_ID
  "skip",     # B  # PERSON_NUMBER
  "skip",     # C  # AGENCY_ORI
  "text",     # D  # AGENCY_NAME
  "skip",     # E  # TIME_OF_STOP
  "skip",     # F  # DATE_OF_STOP
  "skip",     # G  # STOP_DURATION
  "skip",     # H  # CLOSEST_CITY
  "skip",     # I  # SCHOOL_CODE
  "skip",     # J  # SCHOOL_NAME
  "skip",     # K  # STOP_STUDENT
  "skip",     # L  # K12_SCHOOL_GROUNDS
  ## RAE: Race and Ethnicity
  "text",     # M  # RAE_FULL
  "skip",     # N  # RAE_ASIAN
  "skip",     # O  # RAE_BLACK_AFRICAN_AMERICAN
  "skip",     # P  # RAE_HISPANIC_LATINO
  "skip",     # Q  # RAE_MIDDLE_EASTERN_SOUTH_ASIAN
  "skip",     # R  # RAE_NATIVE_AMERICAN
  "skip",     # S  # RAE_PACIFIC_ISLANDER
  "skip",     # T  # RAE_WHITE
  "skip",     # U  # RAE_MULTI_RACIAL
  ## G: Gender    
  "text",     # V  # G_FULL
  "skip",     # W  # G_MALE
  "skip",     # X  # G_FEMALE
  "skip",     # Y  # G_TRANSGENDER_MAN
  "skip",     # Z  # G_TRANSGENDER_WOMAN
  "skip",     # AA # G_GENDER_NONCONFORMING
  "skip",     # AB # G_MULTIGENDER
  "skip",     # AC # LGBT
  "skip",     # AD # AGE
  "skip",     # AE # AGE_GROUP
  "skip",     # AF # LIMITED_ENGLISH_FLUENCY
  ## PD: Perceived Disability
  "skip",     # AG # PD_FULL
  "skip",     # AH # PD_DEAFNESS_HEARING
  "skip",     # AI # PD_SPEECH_IMPAIR
  "skip",     # AJ # PD_BLIND
  "skip",     # AK # PD_MENTAL_HEALTH
  "skip",     # AL # PD_DEVEL_DISAB
  "skip",     # AM # PD_HYPERACTIVITY_DISABILITY
  "skip",     # AN # PD_OTHER
  "skip",     # AO # PD_NONE_DISABILITY
  "skip",     # AP # PD_MULTI
  "text",     # AQ # REASON_FOR_STOP
  ## RFS: Reason for Stop
  "skip",     # AR # RFS_TRAFFIC_VIOLATION_TYPE
  "skip",     # AS # RFS_TRAFFIC_VIOLATION_CODE
  "skip",     # AT # RFS_RS_CODE
  "skip",     # AU # RFS_RS_OFF_WITNESS
  "skip",     # AV # RFS_RS_MATCH_SUSPECT
  "skip",     # AW # RFS_RS_WITNESS_ID
  "skip",     # AX # RFS_RS_CARRY_SUS_OBJECT
  "skip",     # AY # RFS_RS_ACTIONS_INDICATIVE
  "skip",     # AZ # RFS_RS_SUSPECT_LOOK
  "skip",     # BA # RFS_RS_DRUG_TRANS
  "skip",     # BB # RFS_RS_VIOLENT_CRIME
  "skip",     # BC # RFS_RS_REASON_SUSP
  "skip",     # BD # RFS_EC_DISCIPLINE_CODE
  "skip",     # BE # RFS_EC_DISCIPLINE
  "skip",     # BF # CALL_FOR_SERVICE
  ## ADS: Actions Taken During Stop
  "skip",     # BG # ADS_REMOVED_VEHICLE_ORDER
  "skip",     # BH # ADS_REMOVED_VEHICLE_PHYCONTACT
  "skip",     # BI # ADS_SOBRIETY_TEST
  "skip",     # BJ # ADS_CURB_DETENT
  "skip",     # BK # ADS_HANDCUFFED
  "skip",     # BL # ADS_PATCAR_DETENT
  "skip",     # BM # ADS_CANINE_SEARCH
  "skip",     # BN # ADS_FIREARM_POINT
  "skip",     # BO # ADS_FIREARM_DISCHARGE
  "skip",     # BP # ADS_ELECT_DEVICE
  "skip",     # BQ # ADS_IMPACT_DISCHARGE
  "skip",     # BR # ADS_CANINE_BITE
  "skip",     # BS # ADS_BATON
  "skip",     # BT # ADS_CHEM_SPRAY
  "skip",     # BU # ADS_OTHER_CONTACT
  "skip",     # BV # ADS_PHOTO
  "skip",     # BW # ADS_ASKED_SEARCH_PER
  "skip",     # BX # ADS_SEARCHED_PER
  "skip",     # BY # ADS_ASKED_SEARCHED_PROP
  "skip",     # BZ # ADS_SEARCHED_PROP
  "skip",     # CA # ADS_PROP_SEIZE
  "skip",     # CB # ADS_VEHICLE_IMPOUND
  "skip",     # CC # ADS_WRITTEN_STATEMENT
  "skip",     # CD # ADS_NO_ACTIONS
  "skip",     # CE # ADS_SEARCH_PER_CONSENT
  "skip",     # CF # ADS_SEARCH_PROP_CONSENT
  ## BFS: Basis for Search
  "logical",  # CG # BFS_CONSENT_GIVEN
  "logical",  # CH # BFS_OFFICER_SAFETY
  "logical",  # CI # BFS_SEARCH_WARRANT
  "logical",  # CJ # BFS_PAROLE
  "logical",  # CK # BFS_SUSPECT_WEAPON
  "logical",  # CL # BFS_VISIBLE_CONTRABAND
  "logical",  # CM # BFS_ODOR_CONTRABAND
  "logical",  # CN # BFS_CANINE_DETECT
  "logical",  # CO # BFS_EVIDENCE
  "logical",  # CP # BFS_INCIDENT
  "logical",  # CQ # BFS_EXIGENT_CIRCUM
  "logical",  # CR # BFS_VEHICLE_INVENT
  "logical",  # CS # BFS_SCHOOL_POLICY
  ## CED: Contraband or Evidence Discovered
  "logical",  # CT # CED_NONE_CONTRABAND
  "skip",     # CU # CED_FIREARM
  "skip",     # CV # CED_AMMUNITION
  "skip",     # CW # CED_WEAPON
  "skip",     # CX # CED_DRUGS
  "skip",     # CY # CED_ALCOHOL
  "skip",     # CZ # CED_MONEY
  "skip",     # DA # CED_DRUG_PARAPHERNALIA
  "skip",     # DB # CED_STOLEN_PROPERTY
  "skip",     # DC # CED_ELECT_DEVICE
  "skip",     # DD # CED_OTHER_CONTRABAND
  ## BPS: Basis for Property Seizure
  "skip",     # DE # BPS_SAFEKEEPING
  "skip",     # DF # BPS_CONTRABAND
  "skip",     # DG # BPS_EVIDENCE
  "skip",     # DH # BPS_IMPOUND_VEHICLE
  "skip",     # DI # BPS_ABANDON_PROP
  "skip",     # DJ # BPS_VIOLENT_SCHOOL
  ## TPS: Type of Property Seized
  "skip",     # DK # TPS_FIREARM
  "skip",     # DL # TPS_AMMUNITION
  "skip",     # DM # TPS_WEAPON
  "skip",     # DN # TPS_DRUGS
  "skip",     # DO # TPS_ALCOHOL
  "skip",     # DP # TPS_MONEY
  "skip",     # DQ # TPS_DRUG_PARAPHERNALIA
  "skip",     # DR # TPS_STOLEN_PROPERTY
  "skip",     # DS # TPS_CELLPHONE
  "skip",     # DT # TPS_VEHICLE
  "skip",     # DU # TPS_CONTRABAND
  ## ROS: Result of Stop
  "skip",     # DV # ROS_NO_ACTION
  "skip",     # DW # ROS_WARNING
  "skip",     # DX # ROS_CITATION
  "skip",     # DY # ROS_IN_FIELD_CITE_RELEASE
  "skip",     # DZ # ROS_CUSTODIAL_WARRANT
  "skip",     # EA # ROS_CUSTODIAL_WITHOU_WARRANT
  "skip",     # EB # ROS_FIELD_INTERVIEW_CARD
  "skip",     # EC # ROS_NON_CRIMINAL_TRANSPORT
  "skip",     # ED # ROS_CONTACT_LEGAL_GUARDIAN
  "skip",     # EE # ROS_PSYCH_HOLD
  "skip",     # EF # ROS_US_HOMELAND
  "skip",     # EG # ROS_REFFERAL_SCHOOL_ADMIN
  "skip",     # EH # ROS_REFFERAL_SCHOOL_COUNSELOR
  "skip",     # EI # ROS_WARNING_CDS
  "skip",     # EJ # ROS_CITATION_CDS
  "skip",     # EK # ROS_IN_FIELD_CITE_RELEASE_CDS
  "skip"      # EL # ROS_CUSTODIAL_WOUT_WARRANT_CDS
)

################################################################################

# Convenience function to read in all the RIPA data
read_ripa <- function(filepath) {
  read_xlsx(filepath, col_types = COL_TYPES, progress = FALSE) %>%
    # Remove non-discretionary stops
    filter(REASON_FOR_STOP %in% c(1, 2)) %>%
    # Add relevant columns
    transmute(
      agency = AGENCY_NAME,
      race = fct_recode(
        factor(RAE_FULL, levels = 1:8),
        "b"        = "2",
        "h"        = "3",
        "w"        = "7",
        "Other"    = "1",
        "Other"    = "4",
        "Other"    = "5",
        "Other"    = "6",
        "Other"    = "8"
      ),
      # Only count a search as a search if it was conducted discretionary
      searched = (
          BFS_CONSENT_GIVEN
        | BFS_OFFICER_SAFETY
        | BFS_SUSPECT_WEAPON
        | BFS_VISIBLE_CONTRABAND
        | BFS_ODOR_CONTRABAND
        | BFS_CANINE_DETECT
        | BFS_EVIDENCE
        | BFS_EXIGENT_CIRCUM
        | BFS_SCHOOL_POLICY
      ),
      contraband = !CED_NONE_CONTRABAND
    ) %>%
    # Retain only Black, Hispanic, and White individuals. (Only 2 instances
    # of truly missing race.)
    drop_na(race) %>%
    # Blanks indicate that someone was not searched or that no contraband was
    # found.
    replace_na(list(searched = FALSE, contraband = FALSE, race = "Other")) %>%
    # Only count contraband from discretionary searches
    mutate(contraband = searched & contraband)
}

# Read in the data
dir_ls(path = path("data", "ripa"), regexp = "RIPA.*\\.xlsx$") %>%
  map(read_ripa) %>%
  list_rbind() %>%
  group_by(agency, race) %>%
  # Calculate the decision rate and outcome rate
  summarize(
    decision_rate = mean(searched),
    outcome_rate  = mean(contraband[searched]),
    var_sr        = var(searched) / n(),
    var_hr        = var(contraband[searched]) / sum(searched),
    n             = n(),
    .groups       = "drop"
  ) %>%
  pivot_wider(
    id_cols = agency,
    names_from = race,
    values_from = c(decision_rate, outcome_rate, var_sr, var_hr, n),
    values_fill = list(n = 0)
  ) %>%
  # Look at the difference in decision rates and outcome rates
  transmute(
    agency                = agency,
    Delta_decision_rate_b = decision_rate_b - decision_rate_w,
    Delta_decision_rate_h = decision_rate_h - decision_rate_w,
    Delta_outcome_rate_b  = outcome_rate_b - outcome_rate_w,
    Delta_outcome_rate_h  = outcome_rate_h - outcome_rate_w,
    var_sr_b              = var_sr_b + var_sr_w,
    var_sr_h              = var_sr_h + var_sr_w,
    var_hr_b              = var_hr_b + var_hr_w,
    var_hr_h              = var_hr_h + var_hr_w,
    n_b                   = n_b,
    n_h                   = n_h,
    n_w                   = n_w,
    n_agency              = n_w + n_b + n_h
  ) %>%
  filter(n_b >= 1000, n_h >= 1000, n_w >= 1000) %>%
  select(-n_w) %>%
  pivot_longer(
    cols = -c(agency, n_agency),
    names_pattern = "(.*)_(b|h)",
    names_to = c("statistic", "race")
  ) %>%
  pivot_wider(
    id_cols = c(agency, n_agency, race),
    names_from = statistic,
    values_from = value
  ) %>%
  rename(n_race = n) %>%
  select(
    agency,
    n_agency,
    race,
    n_race,
    Delta_decision_rate,
    Delta_outcome_rate,
    var_sr,
    var_hr
  ) %>%
  arrange(n_agency, agency, race) %>%
  mutate(race = factor(race, levels = c("b", "h", "w"))) %>%
  write_rds(path("data", "clean", "ripa.rds"))

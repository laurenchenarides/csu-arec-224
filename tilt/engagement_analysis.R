# ============================================================
# AREC 224 TILT Study - Data Cleaning Script
# ============================================================
# Inputs: 01_all_deidentified_for_cleaning.xlsx
# ram_select_data_sheet.xlsx
# Outputs: matched_survey, matched_eidp
# ============================================================
cat("\f")
rm(list=ls())

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psych)
library(stringr)

path <- "tilt/01_all_deidentified_for_cleaning.xlsx"

# ------------------------------------------------------------
# STEP 1: Load all sheets
# ------------------------------------------------------------

pre_quant  <- read_excel(path, sheet = "Pre-Survey_Quant")
pre_qual   <- read_excel(path, sheet = "Pre-Survey_Qual")
pre_eidp   <- read_excel(path, sheet = "Pre-EIDP_data")
post_quant <- read_excel(path, sheet = "Post-Survey_Quant")
post_qual  <- read_excel(path, sheet = "Post-Survey_Qual")
post_eidp  <- read_excel(path, sheet = "Post-EIDP_data")

# ------------------------------------------------------------
# STEP 2: Rename post-survey columns to match pre-survey 
#         naming conventions (derived from crosswalk)
# ------------------------------------------------------------

post_quant <- post_quant %>%
  rename(
    # Emotional engagement - podcasting
    Q28_1_Pod_Excited      = Q6_1,
    Q28_2_Pod_Nervous      = Q6_2,
    Q28_3_Pod_Bored        = Q6_3,
    Q28_4_Pod_Calm         = Q6_4,
    Q28_5_Pod_Enthused     = Q6_5,
    Q28_6_Pod_Anxious      = Q6_6,
    Q28_7_Pod_Dull         = Q6_7,
    Q28_8_Pod_Relaxed      = Q6_8,
    Q28_9_Pod_Elated       = Q6_9,
    Q28_10_Pod_Overwhelmed = Q6_10,
    Q28_11_Pod_Disinterest = Q6_11,
    Q28_12_Pod_Content     = Q6_12,
    
    # Emotional engagement - case study
    Q31_1_CS_Excited       = Q7_1,
    Q31_2_CS_Nervous       = Q7_2,
    Q31_3_CS_Bored         = Q7_3,
    Q31_4_CS_Calm          = Q7_4,
    Q31_5_CS_Enthused      = Q7_5,
    Q31_6_CS_Anxious       = Q7_6,
    Q31_7_CS_Dull          = Q7_7,
    Q31_8_CS_Relaxed       = Q7_8,
    Q31_9_CS_Elated        = Q7_9,
    Q31_10_CS_Overwhelmed  = Q7_10,
    Q31_11_CS_Disinterest  = Q7_11,
    Q31_12_CS_Content      = Q7_12,
    
    # Emotional engagement - business case presentation
    Q32_1_Biz_Excited      = Q8_1,
    Q32_2_Biz_Nervous      = Q8_2,
    Q32_3_Biz_Bored        = Q8_3,
    Q32_4_Biz_Calm         = Q8_4,
    Q32_5_Biz_Enthused     = Q8_5,
    Q32_6_Biz_Anxious      = Q8_6,
    Q32_7_Biz_Dull         = Q8_7,
    Q32_8_Biz_Relaxed      = Q8_8,
    Q32_9_Biz_Elated       = Q8_9,
    Q32_10_Biz_Overwhelmed = Q8_10,
    Q32_11_Biz_Disinterest = Q8_11,
    Q32_12_Biz_Content     = Q8_12,
    
    # Emotional engagement - mentorship
    Q37_1_Ment_Excited      = Q9_1,
    Q37_2_Ment_Nervous      = Q9_2,
    Q37_3_Ment_Bored        = Q9_3,
    Q37_4_Ment_Calm         = Q9_4,
    Q37_5_Ment_Enthused     = Q9_5,
    Q37_6_Ment_Anxious      = Q9_6,
    Q37_7_Ment_Dull         = Q9_7,
    Q37_8_Ment_Relaxed      = Q9_8,
    Q37_9_Ment_Elated       = Q9_9,
    Q37_10_Ment_Overwhelmed = Q9_10,
    Q37_11_Ment_Disinterest = Q9_11,
    Q37_12_Ment_Content     = Q9_12,
    
    # Behavioral engagement
    Q38_1_Contrib_Podcast    = Q10_1,
    Q38_2_Contrib_CaseStudy  = Q10_2,
    Q38_3_Contrib_BizCase    = Q10_3,
    Q38_4_Contrib_Mentor     = Q10_4,
    Q38_5_Contrib_Coursework = Q10_5,
    Q38_6_Contrib_Learn      = Q10_6,
    
    # Cognitive engagement
    Q39_1_Think_PodcastCrit    = Q11_1,
    Q39_2_Think_CaseStudySides = Q11_2,
    Q39_3_Think_BizRealWorld   = Q11_3,
    Q39_4_Think_MentorFeedback = Q11_4,
    Q39_5_Think_ApplyDecisions = Q11_5,
    Q39_6_Think_AnalyzeAgBiz   = Q11_6,
    
    # Beliefs and values
    Q40_1_Belong_Dept          = Q12_1,
    Q40_2_Relations_Class      = Q12_2,
    Q40_3_TimeMgmt             = Q12_3,
    Q40_4_ClassParticipate     = Q12_4,
    Q40_5_SelfEfficacy         = Q12_5,
    Q40_6_HandleChallenge      = Q12_6,
    Q40_7_GrowthMindset        = Q12_7,
    Q40_8_CapableUnderstanding = Q12_8,
    Q40_9_EntreDailyLife       = Q12_9,
    Q40_10_EntreInterest       = Q12_10,
    Q40_11_CourseHelpsFuture   = Q12_11,
    Q40_12_GoalCompletion      = Q12_12,
    Q40_13_OvercameSetbacks    = Q12_13,
    
    # Knowledge assessment
    Q44_Def_Market             = Q14,
    Q45_SmallOrg_3rdAdv        = Q15,
    Q46_CoFounder_Issues       = Q16,
    Q47_LearningProcess_Name   = Q17,
    Q48_BizPlan_PotentialBlank = Q18,
    Q49_SWOT_Def               = Q19,
    Q50_Venture_Reqs           = Q20,
    Q51_MktResearch_Purpose    = Q21,
    Q52_VC_Measures            = Q22,
    Q53_NoResearch_Reasons     = Q23,
    Q54_ReviewPlans_Benefit    = Q24,
    
    # Course satisfaction (post-only)
    Q_Satisfaction             = Q25,
    
    # Engagement rankings (post-only)
    Q26_1_Rank_Engage_Podcast  = Q26_1,
    Q26_2_Rank_Engage_CS       = Q26_2,
    Q26_3_Rank_Engage_Biz      = Q26_3,
    Q26_4_Rank_Engage_Mentor   = Q26_4,
    
    # Learning rankings (post-only)
    Q29_1_Rank_Learn_Podcast   = Q29_1,
    Q29_2_Rank_Learn_CS        = Q29_2,
    Q29_3_Rank_Learn_Biz       = Q29_3,
    Q29_4_Rank_Learn_Mentor    = Q29_4,
    
    # Post-only performance comparison
    Q42_Performance_Relative   = Q37
  )

# ------------------------------------------------------------
# STEP 3: Rename post-EIDP columns, build characteristic
#         lookup, and add labels to both pre- and post-EIDP
# ------------------------------------------------------------

# Confirmed characteristic lookup table (pre and post use same codes)
eidp_chars <- tibble::tibble(
  data_value = c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
  characteristic = c(
    "Calculated risk taking",
    "Communication",
    "Creativity and innovativeness",
    "Determination and perseverance",
    "Drive to achieve",
    "Entrepreneurial coachability",
    "Opportunity orientation",
    "Passion",
    "Persistent problem solving",
    "Team building",
    "Tolerance for ambiguity",
    "Vision",
    "Seeking feedback",
    "Personal agency",
    "Entrepreneurial hustle",
    "High energy level"
  )
)

post_eidp <- post_eidp %>%
  select(-Q6, -Q8, -Q10) %>%
  left_join(eidp_chars %>% rename(Q6_num  = data_value, char1_label = characteristic),
            by = "Q6_num") %>%
  left_join(eidp_chars %>% rename(Q8_num  = data_value, char2_label = characteristic),
            by = "Q8_num") %>%
  left_join(eidp_chars %>% rename(Q10_num = data_value, char3_label = characteristic),
            by = "Q10_num") 

post_eidp <- post_eidp %>%
  rename(
    Q6_characteristic1_selection_num  = Q6_num,
    Q6_characteristic1_selection  = char1_label,
    Q7_1_characteristic1_confidence   = Q7_1,
    Q7_2_characteristic1_application  = Q7_2,
    Q7_3_characteristic1_strength     = Q7_3,
    Q7_4_characteristic1_alignment    = Q7_4,
    Q7_5_characteristic1_motivation   = Q7_5,
    Q8_characteristic2_selection_num  = Q8_num,
    Q8_characteristic2_selection  = char2_label,
    Q9_1_characteristic2_confidence   = Q9_1,
    Q9_2_characteristic2_application  = Q9_2,
    Q9_3_characteristic2_strength     = Q9_3,
    Q9_4_characteristic2_alignment    = Q9_4,
    Q9_5_characteristic2_motivation   = Q9_5,
    Q10_characteristic3_selection_num  = Q10_num,
    Q10_characteristic3_selection = char3_label,
    Q11_1_characteristic3_confidence  = Q11_1,
    Q11_2_characteristic3_application = Q11_2,
    Q11_3_characteristic3_strength    = Q11_3,
    Q11_4_characteristic3_alignment   = Q11_4,
    Q11_5_characteristic3_motivation  = Q11_5,
    # Post-only qualitative items - descriptive names
    Q_EIDP_Growth_Narrative       = Q12,
    Q_EIDP_Activity_Supported     = Q13,
    Q_EIDP_Peer_Feedback          = Q14,
    Q_EIDP_Mentor_Feedback        = Q15,
    Q_EIDP_Future_Application     = Q16
  ) 

post_eidp <- post_eidp %>%
  select(`Anonymous Code`, 
         Q6_characteristic1_selection_num, Q6_characteristic1_selection, starts_with("Q7"), 
         Q8_characteristic2_selection_num, Q8_characteristic2_selection, starts_with("Q9"), 
         Q10_characteristic3_selection_num, Q10_characteristic3_selection, starts_with("Q11"), 
         starts_with("Q_EIDP"))



# ------------------------------------------------------------
# STEP 4: Recode 0 as NA across all Likert items in both 
#         surveys (0 = no response in the codebook)
# ------------------------------------------------------------

# Helper: recode 0 to NA for all numeric columns except IDs
recode_zeros <- function(df) {
  id_cols <- c("ID", "Anonymous Code")
  df %>%
    mutate(across(
      where(is.numeric) & !any_of(id_cols),
      ~ na_if(., 0)
    ))
}

pre_quant  <- recode_zeros(pre_quant)
post_quant <- recode_zeros(post_quant)
pre_eidp   <- recode_zeros(pre_eidp)
post_eidp  <- recode_zeros(post_eidp)

# ------------------------------------------------------------
# STEP 5: Add time-point indicator
# ------------------------------------------------------------

pre_quant  <- pre_quant  %>% mutate(time = "pre")
post_quant <- post_quant %>% mutate(time = "post")
pre_eidp   <- pre_eidp   %>% mutate(time = "pre")
post_eidp  <- post_eidp  %>% mutate(time = "post")

# ------------------------------------------------------------
# STEP 6: Build matched analytic samples
# ------------------------------------------------------------

# Matched survey sample (n = 38 expected)
matched_survey <- inner_join(
  pre_quant,
  post_quant,
  by = "Anonymous Code",
  suffix = c("_pre", "_post")
)
cat("Matched survey n:", nrow(matched_survey), "\n")

# Pre-only and post-only for attrition documentation
pre_only_codes  <- setdiff(pre_quant$`Anonymous Code`,
                           post_quant$`Anonymous Code`)
post_only_codes <- setdiff(post_quant$`Anonymous Code`,
                           pre_quant$`Anonymous Code`)
cat("Pre-only codes:", pre_only_codes, "\n")
cat("Post-only codes:", post_only_codes, "\n")

# Matched EIDP sample (n = 31 expected)
matched_eidp <- inner_join(
  pre_eidp,
  post_eidp,
  by = "Anonymous Code",
  suffix = c("_pre", "_post")
)

cat("Matched EIDP n:", nrow(matched_eidp), "\n")

# Pre-only and post-only EIDP for attrition documentation
pre_only_eidp  <- setdiff(pre_eidp$`Anonymous Code`,
                          post_eidp$`Anonymous Code`)
post_only_eidp <- setdiff(post_eidp$`Anonymous Code`,
                          pre_eidp$`Anonymous Code`)
cat("Pre-only EIDP codes:", pre_only_eidp, "\n")
cat("Post-only EIDP codes:", post_only_eidp, "\n")

# Check that each student selected the same three characteristics
# at pre and post before computing any change scores
matched_eidp <- matched_eidp %>%
  mutate(
    char1_match     = Q6_characteristic1_selection_num_pre == Q6_characteristic1_selection_num_post,
    char2_match     = Q8_characteristic2_selection_num_pre == Q8_characteristic2_selection_num_post,
    char3_match     = Q10_characteristic3_selection_num_pre == Q10_characteristic3_selection_num_post,
    all_chars_match = char1_match & char2_match & char3_match
  )

cat("Students with all three characteristics matching pre to post:",
    sum(matched_eidp$all_chars_match, na.rm = TRUE), "\n")
cat("Students with at least one mismatch:",
    sum(!matched_eidp$all_chars_match, na.rm = TRUE), "\n")

# Review any mismatches
matched_eidp %>%
  filter(!all_chars_match) %>%
  select(`Anonymous Code`,
         Q6_characteristic1_selection_num_pre, Q6_characteristic1_selection_num_post,
         Q8_characteristic2_selection_num_pre, Q8_characteristic2_selection_num_post,
         Q10_characteristic3_selection_num_pre, Q10_characteristic3_selection_num_post)

# ------------------------------------------------------------
# STEP 7: Missingness check on matched survey
# ------------------------------------------------------------

# Count NAs per variable in the matched sample
miss_check <- matched_survey %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to = "variable",
               values_to = "n_missing") %>%
  filter(n_missing > 0) %>%
  arrange(desc(n_missing))

print(miss_check, n = 50)

# ------------------------------------------------------------
# STEP 8: Reverse code negatively valenced emotion items
#         (scale is 1-5 after recoding 0 to NA in Step 4,
#          so reverse = 6 - x; higher scores now consistently
#          indicate more positive emotional engagement)
# ------------------------------------------------------------

neg_emo_stems <- c("Nervous", "Bored", "Anxious", "Dull",
                   "Overwhelmed", "Disinterest")

neg_emo_pre <- names(pre_quant)[
  grepl(paste(neg_emo_stems, collapse = "|"), names(pre_quant))
]

neg_emo_post <- names(post_quant)[
  grepl(paste(neg_emo_stems, collapse = "|"), names(post_quant))
]

pre_quant <- pre_quant %>%
  mutate(across(all_of(neg_emo_pre), ~ 6 - .))
post_quant <- post_quant %>%
  mutate(across(all_of(neg_emo_post), ~ 6 - .))

# ------------------------------------------------------------
# STEP 9: Load and merge demographic data from RAM Select
# ------------------------------------------------------------

demo <- read_excel("tilt/ram_select_data_sheet.xlsx",
                   sheet = "data_sheet") %>%
  select(-ID, -note) %>%
  mutate(`Undergrad Gpa` = na_if(`Undergrad Gpa`, 0)) %>%
  rename(
    anon_code      = `Anonymous Code`,
    major          = `Primary Major`,
    class_standing = `Student Class`,
    gpa            = `Undergrad Gpa`,
    eth_am_indian  = `Ethnicity - American Indian`,
    eth_asian      = `Ethnicity - Asian`,
    eth_black      = `Ethnicity - Black`,
    eth_hawaiian   = `Ethnicity - Hawaiian`,
    eth_hispanic   = `Ethnicity - Hispanic/Latino`,
    eth_white      = `Ethnicity - White`,
    eth_multiple   = `Ethnicity - Multiple`,
    gender         = `Gender`,
    age            = `Age`,
    residency      = `Residency`,
    transfer_dare  = `Transfer to DARE FA_2025`,
    prior_field    = `Prior field of study if new to DARE_FA 2025`,
    final_score    = `Final Score`,
    final_grade    = `Final Grade`
  ) %>%
  mutate(
    major = recode(major,
                   `1` = "Agricultural Business",
                   `2` = "Animal Science",
                   `3` = "Livestock Business Management",
                   `4` = "International Exchange",
                   `5` = "Soil and Crop Sciences",
                   `6` = "Equine Science"
    ),
    class_standing = recode(class_standing,
                            `1` = "Freshman",
                            `2` = "Sophomore",
                            `3` = "Junior",
                            `4` = "Senior"
    ),
    gender = recode(gender,
                    `0` = "Male",
                    `1` = "Female"
    ),
    residency = recode(residency,
                       `0` = "Non-resident",
                       `1` = "Colorado resident"
    ),
    transfer_dare = recode(transfer_dare,
                           `0` = "No",
                           `1` = "Yes"
    ),
    prior_field = recode(prior_field,
                         `0` = "N/A",
                         `1` = "Business interest",
                         `2` = "Experiential studies",
                         `3` = "Business administration"
    )
  )

# Merge onto matched survey sample
matched_survey <- matched_survey %>%
  left_join(demo, by = c("Anonymous Code" = "anon_code"))

# Merge onto matched EIDP sample
matched_eidp <- matched_eidp %>%
  left_join(demo, by = c("Anonymous Code" = "anon_code"))

# Check merge - all 38 survey and 31 EIDP students should have demo data
cat("matched_survey rows with missing major:", 
    sum(is.na(matched_survey$major)), "\n")
cat("matched_eidp rows with missing major:",  
    sum(is.na(matched_eidp$major)), "\n")

# Quick descriptive check of the sample
cat("\nClass standing distribution (matched survey):\n")
print(table(matched_survey$class_standing))

cat("\nGender distribution (matched survey):\n")
print(table(matched_survey$gender))

cat("\nMajor distribution (matched survey):\n")
print(table(matched_survey$major))

cat("\nFinal grade distribution:\n")
print(table(matched_survey$final_grade, useNA = "ifany"))

# ============================================================
# STEP 10: Construct engagement subscales and check
#          internal consistency (Cronbach's alpha)
# ============================================================
# Scale structure follows Fredricks et al. (2004):
#   - Behavioral engagement: participation, effort, persistence
#   - Emotional engagement: interest, enjoyment, connection
#     (emotion items follow Russell 1980 circumplex;
#      negatively valenced items already reverse coded in Step 8)
#   - Cognitive engagement: critical thinking, application,
#     investment in learning
# Beliefs and values subscales follow Dweck et al. (2014)
#   Academic Tenacity framework.
# All subscales computed as row means (NA removed).
# Cronbach's alpha computed using psych::alpha().
# ============================================================

# ------------------------------------------------------------
# 10a. Behavioral engagement subscales
#      One item per activity serves as an individual indicator;
#      two general items form a subscale where reliability permits
# ------------------------------------------------------------

beh_pod  <- "Q38_1_Contrib_Podcast"
beh_cs   <- "Q38_2_Contrib_CaseStudy"
beh_biz  <- "Q38_3_Contrib_BizCase"
beh_ment <- "Q38_4_Contrib_Mentor"
beh_gen  <- c("Q38_5_Contrib_Coursework", "Q38_6_Contrib_Learn")

# Alpha for general behavioral subscale
# Pre alpha expected ~0.938; post alpha ~0.583
# Post alpha is below threshold; treat as two individual
# indicators at post rather than a composite
cat("Behavioral - General (pre) alpha:",
    round(psych::alpha(
      pre_quant %>% select(all_of(beh_gen)))$total$raw_alpha, 3), "\n")
cat("Behavioral - General (post) alpha:",
    round(psych::alpha(
      post_quant %>% select(all_of(beh_gen)))$total$raw_alpha, 3), "\n")

# Compute behavioral scores
# Activity items retained as individual indicators (single item,
# no alpha applicable); general subscale computed as mean at pre
# only due to low post reliability
pre_quant <- pre_quant %>%
  mutate(
    beh_podcast_pre   = Q38_1_Contrib_Podcast,
    beh_casestudy_pre = Q38_2_Contrib_CaseStudy,
    beh_bizcase_pre   = Q38_3_Contrib_BizCase,
    beh_mentor_pre    = Q38_4_Contrib_Mentor,
    beh_general_pre   = rowMeans(across(all_of(beh_gen)),
                                 na.rm = TRUE)
  )

post_quant <- post_quant %>%
  mutate(
    beh_podcast_post   = Q38_1_Contrib_Podcast,
    beh_casestudy_post = Q38_2_Contrib_CaseStudy,
    beh_bizcase_post   = Q38_3_Contrib_BizCase,
    beh_mentor_post    = Q38_4_Contrib_Mentor,
    # Retained as separate indicators at post; composite not
    # computed due to alpha = 0.583
    beh_coursework_post = Q38_5_Contrib_Coursework,
    beh_learn_post      = Q38_6_Contrib_Learn
  )

# ------------------------------------------------------------
# 10b. Emotional engagement subscales
#      12 items per activity follow Russell (1980) circumplex;
#      negatively valenced items reverse coded in Step 8.
#
#      Reliability findings:
#      Post alphas: 0.756-0.825 (all acceptable, no warnings)
#      Pre alphas with correction: 0.685-0.780
#        - Podcast (0.703) and case study (0.780): acceptable,
#          compute composite with caution
#        - Business case (0.643) and mentorship (0.635):
#          below threshold; anticipated emotions did not form
#          a reliable single dimension, consistent with
#          multidimensional anticipatory affect (Russell, 1980)
#          Pre composites for these two activities NOT computed;
#          individual items retained for descriptive reporting
# ------------------------------------------------------------

emo_pod  <- paste0("Q28_", 1:12, "_Pod_",
                   c("Excited","Nervous","Bored","Calm","Enthused",
                     "Anxious","Dull","Relaxed","Elated",
                     "Overwhelmed","Disinterest","Content"))
emo_cs   <- paste0("Q31_", 1:12, "_CS_",
                   c("Excited","Nervous","Bored","Calm","Enthused",
                     "Anxious","Dull","Relaxed","Elated",
                     "Overwhelmed","Disinterest","Content"))
emo_biz  <- paste0("Q32_", 1:12, "_Biz_",
                   c("Excited","Nervous","Bored","Calm","Enthused",
                     "Anxious","Dull","Relaxed","Elated",
                     "Overwhelmed","Disinterest","Content"))
emo_ment <- paste0("Q37_", 1:12, "_Ment_",
                   c("Excited","Nervous","Bored","Calm","Enthused",
                     "Anxious","Dull","Relaxed","Elated",
                     "Overwhelmed","Disinterest","Content"))

# Post emotional composites - all four activities reliable
post_quant <- post_quant %>%
  mutate(
    emo_podcast_post   = rowMeans(across(all_of(emo_pod)),
                                  na.rm = TRUE),
    emo_casestudy_post = rowMeans(across(all_of(emo_cs)),
                                  na.rm = TRUE),
    emo_bizcase_post   = rowMeans(across(all_of(emo_biz)),
                                  na.rm = TRUE),
    emo_mentor_post    = rowMeans(across(all_of(emo_ment)),
                                  na.rm = TRUE)
  )

# Pre emotional composites - podcast and case study only
# Business case and mentorship pre composites not computed;
# see reliability notes above
pre_quant <- pre_quant %>%
  mutate(
    emo_podcast_pre   = rowMeans(across(all_of(emo_pod)),
                                 na.rm = TRUE),
    emo_casestudy_pre = rowMeans(across(all_of(emo_cs)),
                                 na.rm = TRUE)
  )

# ------------------------------------------------------------
# 10c. Cognitive engagement subscales
#      One item per activity serves as an individual indicator;
#      two general items form a subscale
# ------------------------------------------------------------

cog_pod  <- "Q39_1_Think_PodcastCrit"
cog_cs   <- "Q39_2_Think_CaseStudySides"
cog_biz  <- "Q39_3_Think_BizRealWorld"
cog_ment <- "Q39_4_Think_MentorFeedback"
cog_gen  <- c("Q39_5_Think_ApplyDecisions",
              "Q39_6_Think_AnalyzeAgBiz")

# Alpha for general cognitive subscale
cat("Cognitive - General (pre) alpha:",
    round(psych::alpha(
      pre_quant %>% select(all_of(cog_gen)))$total$raw_alpha, 3), "\n")
cat("Cognitive - General (post) alpha:",
    round(psych::alpha(
      post_quant %>% select(all_of(cog_gen)))$total$raw_alpha, 3), "\n")

# Compute cognitive scores
pre_quant <- pre_quant %>%
  mutate(
    cog_podcast_pre   = Q39_1_Think_PodcastCrit,
    cog_casestudy_pre = Q39_2_Think_CaseStudySides,
    cog_bizcase_pre   = Q39_3_Think_BizRealWorld,
    cog_mentor_pre    = Q39_4_Think_MentorFeedback,
    cog_general_pre   = rowMeans(across(all_of(cog_gen)),
                                 na.rm = TRUE)
  )

post_quant <- post_quant %>%
  mutate(
    cog_podcast_post   = Q39_1_Think_PodcastCrit,
    cog_casestudy_post = Q39_2_Think_CaseStudySides,
    cog_bizcase_post   = Q39_3_Think_BizRealWorld,
    cog_mentor_post    = Q39_4_Think_MentorFeedback,
    cog_general_post   = rowMeans(across(all_of(cog_gen)),
                                  na.rm = TRUE)
  )

# ------------------------------------------------------------
# 10d. Beliefs and values subscales
#      Six subscales following Dweck et al. (2014) Academic
#      Tenacity framework, mapped in beliefs_values_only.xlsx:
#        belonging (2 items), self-regulation (2 items),
#        efficacy (2 items), growth mindset (2 items),
#        relevance (3 items), persistence (2 items)
#      Note: two-item subscales produce unstable alpha
#      estimates at n = 38; interpret with caution
# ------------------------------------------------------------

bv_belonging      <- c("Q40_1_Belong_Dept", "Q40_2_Relations_Class")
bv_selfregulation <- c("Q40_3_TimeMgmt", "Q40_4_ClassParticipate")
bv_efficacy       <- c("Q40_5_SelfEfficacy", "Q40_6_HandleChallenge")
bv_growth         <- c("Q40_7_GrowthMindset", "Q40_8_CapableUnderstanding")
bv_relevance      <- c("Q40_9_EntreDailyLife", "Q40_10_EntreInterest",
                       "Q40_11_CourseHelpsFuture")
bv_persist        <- c("Q40_12_GoalCompletion", "Q40_13_OvercameSetbacks")

bv_subscales <- list(
  belonging       = bv_belonging,
  self_regulation = bv_selfregulation,
  efficacy        = bv_efficacy,
  growth_mindset  = bv_growth,
  relevance       = bv_relevance,
  persistence     = bv_persist
)

# Alpha for each beliefs and values subscale at pre and post
for (tag in c("pre", "post")) {
  df_use <- if (tag == "pre") pre_quant else post_quant
  cat("\nBeliefs and values alphas (", tag, "):\n")
  for (name in names(bv_subscales)) {
    a <- psych::alpha(df_use %>% select(all_of(bv_subscales[[name]])),
                      na.rm = TRUE)
    cat(" ", name, "alpha:",
        round(a$total$raw_alpha, 3), "\n")
  }
}

# Compute beliefs and values subscale means at pre and post
for (tag in c("pre", "post")) {
  df_use <- if (tag == "pre") pre_quant else post_quant
  df_use <- df_use %>%
    mutate(
      bv_belonging_score      = rowMeans(across(all_of(bv_belonging)),
                                         na.rm = TRUE),
      bv_selfregulation_score = rowMeans(across(all_of(bv_selfregulation)),
                                         na.rm = TRUE),
      bv_efficacy_score       = rowMeans(across(all_of(bv_efficacy)),
                                         na.rm = TRUE),
      bv_growth_score         = rowMeans(across(all_of(bv_growth)),
                                         na.rm = TRUE),
      bv_relevance_score      = rowMeans(across(all_of(bv_relevance)),
                                         na.rm = TRUE),
      bv_persist_score        = rowMeans(across(all_of(bv_persist)),
                                         na.rm = TRUE)
    )
  if (tag == "pre")  pre_quant  <- df_use
  if (tag == "post") post_quant <- df_use
}

# ------------------------------------------------------------
# 10e. Rebuild matched_survey with all subscale columns,
#      then merge demographic data
# ------------------------------------------------------------

matched_survey <- inner_join(
  pre_quant,
  post_quant,
  by = "Anonymous Code",
  suffix = c("_pre", "_post")
) %>%
  left_join(demo, by = c("Anonymous Code" = "anon_code"))

cat("\nmatched_survey rebuilt with subscales. Rows:",
    nrow(matched_survey), "\n")

# Check merge - all 38 students should have demo data
cat("Rows missing major (demo merge check):",
    sum(is.na(matched_survey$major)), "\n")

# Confirm subscale columns are present
subscale_cols <- names(matched_survey)[
  grepl("^beh_|^emo_|^cog_|^bv_", names(matched_survey))
]
cat("Subscale columns created:", length(subscale_cols), "\n")
print(subscale_cols)

cat("\f")

# ============================================================
# AREC 224 TILT Study — Analysis Script
# Steps 11-23: Descriptives through supplementary outcomes
#
# Script follows the Analysis Roadmap document exactly.
# Each step header references the roadmap component label
# and research question for cross-document navigation.
# ============================================================

# Helper: rank-biserial correlation from wilcox.test output
# Used as effect size for all Wilcoxon signed-rank tests
# r = 1 - (2*W) / (n*(n+1)/2); ranges -1 to +1
# Benchmarks: small = 0.1, medium = 0.3, large = 0.5
rank_biserial <- function(W, n) {
  1 - (2 * W) / (n * (n + 1) / 2)
}

# Helper: run a paired Wilcoxon test and return a tidy summary
# Null hypothesis: no systematic difference between pre and post
# Two-sided test throughout (see analysis plan rationale)
paired_wilcox <- function(data, pre_var, post_var, label) {
  x <- data[[pre_var]]
  y <- data[[post_var]]
  valid <- complete.cases(x, y)
  x <- x[valid]; y <- y[valid]
  n <- sum(valid)
  d <- y - x  # post minus pre
  test <- wilcox.test(d, mu = 0, exact = FALSE)
  r <- rank_biserial(test$statistic, n)
  tibble(
    activity_dimension = label,
    n_pairs            = n,
    mean_pre           = round(mean(x), 2),
    mean_post          = round(mean(y), 2),
    mean_change        = round(mean(d), 2),
    W                  = round(test$statistic, 1),
    p_value            = round(test$p.value, 3),
    r_effect           = round(r, 3),
    sig                = ifelse(test$p.value < .05, "*", "")
  )
}

# Helper: run a paired t-test and return a tidy summary
# Used for emotional engagement composites (multi-item means,
# more continuous, t-test assumptions more plausible)
# Effect size: Cohen's d = mean difference / SD of differences
# Benchmarks: small = 0.2, medium = 0.5, large = 0.8
paired_ttest <- function(data, pre_var, post_var, label) {
  x <- data[[pre_var]]
  y <- data[[post_var]]
  valid <- complete.cases(x, y)
  x <- x[valid]; y <- y[valid]
  n <- sum(valid)
  d <- y - x
  test <- t.test(d, mu = 0)
  cohens_d <- mean(d) / sd(d)
  tibble(
    activity_dimension = label,
    n_pairs            = n,
    mean_pre           = round(mean(x), 2),
    mean_post          = round(mean(y), 2),
    mean_change        = round(mean(d), 2),
    t_stat             = round(test$statistic, 2),
    df                 = round(test$parameter, 1),
    p_value            = round(test$p.value, 3),
    d_effect           = round(cohens_d, 3),
    sig                = ifelse(test$p.value < .05, "*", "")
  )
}

# ------------------------------------------------------------
# STEP 11: Sample description
# Roadmap: Supplementary — "Sample description"
# Produces frequency tables for key demographic variables
# ------------------------------------------------------------

cat("\n=== STEP 11: Sample Description ===\n")

cat("\nClass standing (n =", nrow(matched_survey), "):\n")
print(table(matched_survey$class_standing))
print(round(prop.table(table(matched_survey$class_standing)) * 100, 1))

cat("\nGender:\n")
print(table(matched_survey$gender))
print(round(prop.table(table(matched_survey$gender)) * 100, 1))

cat("\nPrimary major:\n")
print(table(matched_survey$major))
print(round(prop.table(table(matched_survey$major)) * 100, 1))

cat("\nResidency:\n")
print(table(matched_survey$residency))

cat("\nTransfer to DARE:\n")
print(table(matched_survey$transfer_dare))

cat("\nGPA summary (excludes 2 students with no GPA on file):\n")
print(summary(matched_survey$gpa))

# Ethnicity: binary indicators, report as % selecting each
eth_vars <- c("eth_am_indian","eth_asian","eth_black",
              "eth_hawaiian","eth_hispanic","eth_white","eth_multiple")
cat("\nEthnicity (% identifying with each group):\n")
eth_summary <- matched_survey %>%
  summarise(across(all_of(eth_vars), ~ round(mean(. == 1, na.rm=TRUE)*100, 1)))
print(eth_summary)

# ------------------------------------------------------------
# STEP 12: Familiarity at entry
# Roadmap: Supplementary — "Familiarity at entry"
# Frequency distributions for prior experience with each
# activity; provides context for interpreting engagement change
# Recode numeric values to descriptive labels per codebook
# Q7, Q13, Q19: 1=Never, 2=Rarely, 3=Sometimes, 4=Often, 5=Very often
# Q23: 1=Yes, 2=No, 3=Unsure
# ------------------------------------------------------------

cat("\n=== STEP 12: Familiarity at Entry ===\n")

freq_labels  <- c(`1`="Never", `2`="Rarely", `3`="Sometimes",
                  `4`="Often", `5`="Very often")
mentor_labels <- c(`1`="Yes", `2`="No", `3`="Unsure")

cat("\nPodcast creation frequency (Q7_PodCreateFreq):\n")
print(table(factor(matched_survey$Q7_PodCreateFreq,
                   levels = 1:5, labels = freq_labels),
            useNA = "ifany"))

cat("\nCase-study use frequency (Q13_CSUseFreq):\n")
print(table(factor(matched_survey$Q13_CSUseFreq,
                   levels = 1:5, labels = freq_labels),
            useNA = "ifany"))

cat("\nBusiness case competition frequency (Q19_BizCompFreq):\n")
print(table(factor(matched_survey$Q19_BizCompFreq,
                   levels = 1:5, labels = freq_labels),
            useNA = "ifany"))

cat("\nPrior industry mentor match (Q23_MentorMatched):\n")
print(table(factor(matched_survey$Q23_MentorMatched,
                   levels = 1:3, labels = mentor_labels),
            useNA = "ifany"))

# ------------------------------------------------------------
# STEP 13: Knowledge assessment score
# Roadmap: Secondary RQ 3 — "Self-outcome congruence: knowledge"
# Items retained: Q44, Q46, Q49, Q51, Q53 (fewer than 10
# missing at post; adequate response rate for comparison)
# Score = total correct out of 5; paired Wilcoxon pre vs post
# Null hypothesis: no systematic change in total correct
# ------------------------------------------------------------

cat("\n=== STEP 13: Knowledge Assessment ===\n")

# Items with adequate response rates at both time points
ka_items <- c("Q44_Def_Market", "Q46_CoFounder_Issues",
              "Q49_SWOT_Def",   "Q51_MktResearch_Purpose",
              "Q53_NoResearch_Reasons")

# Compute total correct (sum of binary items; NA treated as 0
# for scoring only — students who skipped get 0 credit)
matched_survey <- matched_survey %>%
  mutate(
    ka_score_pre  = rowSums(across(paste0(ka_items, "_pre")),
                            na.rm = TRUE),
    ka_score_post = rowSums(across(paste0(ka_items, "_post")),
                            na.rm = TRUE)
  )

cat("\nKnowledge score distribution (pre, max = 5):\n")
print(table(matched_survey$ka_score_pre))

cat("\nKnowledge score distribution (post, max = 5):\n")
print(table(matched_survey$ka_score_post))

# Paired Wilcoxon: did total correct improve from pre to post?
ka_result <- paired_wilcox(matched_survey,
                           "ka_score_pre", "ka_score_post",
                           "Knowledge assessment (5 items)")
print(ka_result)

# Item-level response rates for reporting (documents exclusions)
cat("\nItem-level % correct at pre and post:\n")
for (item in ka_items) {
  pct_pre  <- round(mean(matched_survey[[paste0(item,"_pre")]],  na.rm=TRUE)*100, 1)
  pct_post <- round(mean(matched_survey[[paste0(item,"_post")]], na.rm=TRUE)*100, 1)
  cat(" ", item, "— pre:", pct_pre, "%  post:", pct_post, "%\n")
}

# ------------------------------------------------------------
# STEP 14: Behavioral engagement — pre-post paired comparisons
# Roadmap: Primary RQ — "Behavioral engagement by activity"
# Single item per activity (ordinal); paired Wilcoxon
# Null hypothesis: no systematic pre-post difference in
# self-reported contribution/effort for each activity
# ------------------------------------------------------------

cat("\n=== STEP 14: Behavioral Engagement (Pre-Post) ===\n")

beh_results <- bind_rows(
  paired_wilcox(matched_survey, "beh_podcast_pre",
                "beh_podcast_post",   "Behavioral — Podcasting"),
  paired_wilcox(matched_survey, "beh_casestudy_pre",
                "beh_casestudy_post", "Behavioral — Case study"),
  paired_wilcox(matched_survey, "beh_bizcase_pre",
                "beh_bizcase_post",   "Behavioral — Business case"),
  paired_wilcox(matched_survey, "beh_mentor_pre",
                "beh_mentor_post",    "Behavioral — Mentorship")
)

print(beh_results)

# General behavioral: pre composite only (post alpha too low)
# Report pre descriptives as context for anticipated effort
cat("\nGeneral behavioral — pre composite (anticipated effort):\n")
cat(" Mean:", round(mean(matched_survey$beh_general_pre, na.rm=TRUE), 2),
    " SD:", round(sd(matched_survey$beh_general_pre, na.rm=TRUE), 2), "\n")

# Post general items reported separately (not as composite)
cat("\nGeneral behavioral post — coursework item:\n")
print(table(matched_survey$beh_coursework_post))
cat("General behavioral post — learning item:\n")
print(table(matched_survey$beh_learn_post))

# ------------------------------------------------------------
# STEP 15: Emotional engagement — pre-post paired comparisons
# Roadmap: Primary RQ — "Emotional engagement by activity"
# Multi-item composite (12 items); paired t-test where pre
# and post composites both available; post-only descriptive
# for business case and mentorship (pre composite unreliable;
# see Step 10b reliability notes)
# Null hypothesis (where tested): no systematic pre-post
# difference in mean emotional engagement composite
# ------------------------------------------------------------

cat("\n=== STEP 15: Emotional Engagement (Pre-Post) ===\n")

# Paired t-test where pre composite is reliable
emo_results <- bind_rows(
  paired_ttest(matched_survey, "emo_podcast_pre",
               "emo_podcast_post",   "Emotional — Podcasting"),
  paired_ttest(matched_survey, "emo_casestudy_pre",
               "emo_casestudy_post", "Emotional — Case study")
)
print(emo_results)

# Post-only descriptives for business case and mentorship
cat("\nEmotional — Business case (post only; pre composite unreliable):\n")
cat(" Mean:", round(mean(matched_survey$emo_bizcase_post, na.rm=TRUE), 2),
    " SD:", round(sd(matched_survey$emo_bizcase_post, na.rm=TRUE), 2), "\n")

cat("Emotional — Mentorship (post only; pre composite unreliable):\n")
cat(" Mean:", round(mean(matched_survey$emo_mentor_post, na.rm=TRUE), 2),
    " SD:", round(sd(matched_survey$emo_mentor_post, na.rm=TRUE), 2), "\n")

# ------------------------------------------------------------
# STEP 16: Cognitive engagement — pre-post paired comparisons
# Roadmap: Primary RQ — "Cognitive engagement by activity"
# Single item per activity (ordinal); paired Wilcoxon
# General subscale (2 items, alpha > 0.70 at both time points):
# paired t-test
# Null hypothesis: no systematic pre-post difference in
# self-reported critical thinking or application per activity
# ------------------------------------------------------------

cat("\n=== STEP 16: Cognitive Engagement (Pre-Post) ===\n")

cog_results <- bind_rows(
  paired_wilcox(matched_survey, "cog_podcast_pre",
                "cog_podcast_post",   "Cognitive — Podcasting"),
  paired_wilcox(matched_survey, "cog_casestudy_pre",
                "cog_casestudy_post", "Cognitive — Case study"),
  paired_wilcox(matched_survey, "cog_bizcase_pre",
                "cog_bizcase_post",   "Cognitive — Business case"),
  paired_wilcox(matched_survey, "cog_mentor_pre",
                "cog_mentor_post",    "Cognitive — Mentorship")
)
print(cog_results)

# General cognitive: 2-item composite, t-test appropriate
cog_gen_result <- paired_ttest(matched_survey,
                               "cog_general_pre", "cog_general_post",
                               "Cognitive — General (2-item composite)")
print(cog_gen_result)

# ------------------------------------------------------------
# STEP 17: Student-ranked engagement and learning
# Roadmap: Primary RQ — "Student-ranked engagement" and
#          "Student-ranked learning"
# Post-only rankings (1 = most engaging/learned, 4 = least)
# Summarize: frequency ranked 1st; median rank per activity
# ------------------------------------------------------------

cat("\n=== STEP 17: Student Rankings (Post-Only) ===\n")

# Define column names exactly as they appear in matched_survey
eng_rank_vars <- c(
  Podcasting      = "Q26_1_Rank_Engage_Podcast",
  `Case study`    = "Q26_2_Rank_Engage_CS",
  `Business case` = "Q26_3_Rank_Engage_Biz",
  Mentorship      = "Q26_4_Rank_Engage_Mentor"
)

lrn_rank_vars <- c(
  Podcasting      = "Q29_1_Rank_Learn_Podcast",
  `Case study`    = "Q29_2_Rank_Learn_CS",
  `Business case` = "Q29_3_Rank_Learn_Biz",
  Mentorship      = "Q29_4_Rank_Learn_Mentor"
)

# Coerce all ranking columns to numeric
rank_cols <- c(unname(eng_rank_vars), unname(lrn_rank_vars))
matched_survey <- matched_survey %>%
  mutate(across(all_of(rank_cols), as.numeric))

# Engagement rankings summary
cat("\nEngagement rankings (1 = most engaging, 4 = least):\n")
eng_rank_summary <- tibble(
  Activity = names(eng_rank_vars),
  n_ranked_1st = sapply(unname(eng_rank_vars), function(v)
    sum(matched_survey[[v]] == 1, na.rm = TRUE)),
  pct_ranked_1st = sapply(unname(eng_rank_vars), function(v)
    round(mean(matched_survey[[v]] == 1, na.rm = TRUE) * 100, 1)),
  median_rank = sapply(unname(eng_rank_vars), function(v)
    median(matched_survey[[v]], na.rm = TRUE)),
  mean_rank = sapply(unname(eng_rank_vars), function(v)
    round(mean(matched_survey[[v]], na.rm = TRUE), 2))
)
print(eng_rank_summary)

# Learning rankings summary
cat("\nLearning rankings (1 = most learned, 4 = least):\n")
lrn_rank_summary <- tibble(
  Activity = names(lrn_rank_vars),
  n_ranked_1st = sapply(unname(lrn_rank_vars), function(v)
    sum(matched_survey[[v]] == 1, na.rm = TRUE)),
  pct_ranked_1st = sapply(unname(lrn_rank_vars), function(v)
    round(mean(matched_survey[[v]] == 1, na.rm = TRUE) * 100, 1)),
  median_rank = sapply(unname(lrn_rank_vars), function(v)
    median(matched_survey[[v]], na.rm = TRUE)),
  mean_rank = sapply(unname(lrn_rank_vars), function(v)
    round(mean(matched_survey[[v]], na.rm = TRUE), 2))
)
print(lrn_rank_summary)

# ------------------------------------------------------------
# STEP 18: Engagement profile by activity — change scores
#          and effect size comparison across dimensions
# Roadmap: Primary RQ / Secondary RQ 1 —
#          "Engagement profile by activity" and
#          "Differential activation by engagement type"
# Change score = post minus pre for each subscale
# Cohen's d computed for each change score to allow
# comparison of magnitude across dimensions within an activity
# Note: d computed on change scores directly (mean/SD of diff)
# ------------------------------------------------------------

cat("\n=== STEP 18: Engagement Profiles and Effect Sizes ===\n")

# Compute change scores for all available pre-post pairs
matched_survey <- matched_survey %>%
  mutate(
    # Behavioral change (post - pre)
    chg_beh_podcast   = beh_podcast_post   - beh_podcast_pre,
    chg_beh_casestudy = beh_casestudy_post - beh_casestudy_pre,
    chg_beh_bizcase   = beh_bizcase_post   - beh_bizcase_pre,
    chg_beh_mentor    = beh_mentor_post    - beh_mentor_pre,
    
    # Emotional change (post - pre; podcast and case study only)
    chg_emo_podcast   = emo_podcast_post   - emo_podcast_pre,
    chg_emo_casestudy = emo_casestudy_post - emo_casestudy_pre,
    
    # Cognitive change (post - pre)
    chg_cog_podcast   = cog_podcast_post   - cog_podcast_pre,
    chg_cog_casestudy = cog_casestudy_post - cog_casestudy_pre,
    chg_cog_bizcase   = cog_bizcase_post   - cog_bizcase_pre,
    chg_cog_mentor    = cog_mentor_post    - cog_mentor_pre
  )

# Compute Cohen's d for each change score
# d = mean(change) / sd(change)
# Positive d = improvement; negative d = decline
cohens_d_chg <- function(data, chg_var, label) {
  x <- data[[chg_var]]
  x <- x[!is.na(x)]
  tibble(
    component   = label,
    n           = length(x),
    mean_change = round(mean(x), 2),
    sd_change   = round(sd(x), 2),
    cohens_d    = round(mean(x) / sd(x), 3)
  )
}

profile_results <- bind_rows(
  # Podcasting
  cohens_d_chg(matched_survey, "chg_beh_podcast",   "Podcast — Behavioral"),
  cohens_d_chg(matched_survey, "chg_emo_podcast",   "Podcast — Emotional"),
  cohens_d_chg(matched_survey, "chg_cog_podcast",   "Podcast — Cognitive"),
  # Case study
  cohens_d_chg(matched_survey, "chg_beh_casestudy", "Case study — Behavioral"),
  cohens_d_chg(matched_survey, "chg_emo_casestudy", "Case study — Emotional"),
  cohens_d_chg(matched_survey, "chg_cog_casestudy", "Case study — Cognitive"),
  # Business case (emotional pre unavailable)
  cohens_d_chg(matched_survey, "chg_beh_bizcase",   "Business case — Behavioral"),
  cohens_d_chg(matched_survey, "chg_cog_bizcase",   "Business case — Cognitive"),
  # Mentorship (emotional pre unavailable)
  cohens_d_chg(matched_survey, "chg_beh_mentor",    "Mentorship — Behavioral"),
  cohens_d_chg(matched_survey, "chg_cog_mentor",    "Mentorship — Cognitive")
)

print(profile_results, n = 20)

# ------------------------------------------------------------
# STEP 19: Beliefs and values predicting anticipated
#          behavioral engagement
# Roadmap: Secondary RQ 2 —
#          "Beliefs/values predicting anticipated behavioral
#           engagement" and "Belonging, self-regulation,
#           efficacy"
# Bivariate Pearson correlations: three reliable pre
# beliefs/values composites (growth mindset, relevance,
# persistence) with each pre behavioral indicator
# ------------------------------------------------------------

cat("\n=== STEP 19: Beliefs/Values and Anticipated Engagement ===\n")

# Reliable pre beliefs/values composites
bv_predictors <- c("bv_growth_score_pre",
                   "bv_relevance_score_pre",
                   "bv_persist_score_pre")

# Pre behavioral indicators (anticipated engagement)
beh_outcomes <- c("beh_podcast_pre", "beh_casestudy_pre",
                  "beh_bizcase_pre", "beh_mentor_pre",
                  "beh_general_pre")

cat("\nCorrelation matrix: beliefs/values vs. anticipated behavioral engagement\n")
cor_data <- matched_survey %>%
  select(all_of(c(bv_predictors, beh_outcomes))) %>%
  drop_na()

cor_matrix <- cor(cor_data, method = "pearson")
# Show only the cross-correlations (predictors x outcomes)
print(round(cor_matrix[bv_predictors, beh_outcomes], 3))

# Individual item descriptives for belonging, self-regulation,
# and efficacy (composites unreliable at post; report pre only)
cat("\nDescriptives for remaining beliefs/values items (pre):\n")
bv_items_pre <- c("Q40_1_Belong_Dept_pre", "Q40_2_Relations_Class_pre",
                  "Q40_3_TimeMgmt_pre",    "Q40_4_ClassParticipate_pre",
                  "Q40_5_SelfEfficacy_pre","Q40_6_HandleChallenge_pre")

bv_desc <- matched_survey %>%
  summarise(across(all_of(bv_items_pre),
                   list(mean = ~ round(mean(., na.rm=TRUE), 2),
                        sd   = ~ round(sd(., na.rm=TRUE), 2))))
print(t(bv_desc))

# ------------------------------------------------------------
# STEP 20: Self-outcome congruence — relative performance
# Roadmap: Secondary RQ 3 —
#          "Self-outcome congruence: relative performance"
# Pre: where did students expect to rank in the class?
# Post: where did they think they actually ranked?
# Summarize shift: up, same, or down
# Note: scale direction differs between pre and post surveys;
# both recoded so higher = more optimistic relative ranking
# ------------------------------------------------------------

cat("\n=== STEP 20: Self-Outcome Congruence — Relative Performance ===\n")

# Pre scale: 1=Bottom 50%, 2=Top 50%, 3=Top 25%, 4=Top 10%, 5=Not sure
# Post scale: 1=Top 10%, 2=Top 25%, 3=Top 50%, 4=Bottom 50%, 5=Not sure
# Recode both to a common ordinal: 1=Bottom 50% through 4=Top 10%
# (exclude "Not sure" responses from shift analysis)

matched_survey <- matched_survey %>%
  mutate(
    # Pre: already ordered low to high (1-4); 5 = not sure -> NA
    perf_pre_recoded = case_when(
      Q42_Performance_Relative_pre == 1 ~ 1,  # Bottom 50%
      Q42_Performance_Relative_pre == 2 ~ 2,  # Top 50%
      Q42_Performance_Relative_pre == 3 ~ 3,  # Top 25%
      Q42_Performance_Relative_pre == 4 ~ 4,  # Top 10%
      TRUE ~ NA_real_
    ),
    # Post: reverse order (1=Top 10% is highest); recode to match
    perf_post_recoded = case_when(
      Q42_Performance_Relative_post == 4 ~ 1, # Bottom 50%
      Q42_Performance_Relative_post == 3 ~ 2, # Top 50%
      Q42_Performance_Relative_post == 2 ~ 3, # Top 25%
      Q42_Performance_Relative_post == 1 ~ 4, # Top 10%
      TRUE ~ NA_real_
    ),
    # Direction of shift
    perf_shift = case_when(
      is.na(perf_pre_recoded) | is.na(perf_post_recoded) ~ NA_character_,
      perf_post_recoded > perf_pre_recoded ~ "More optimistic",
      perf_post_recoded < perf_pre_recoded ~ "Less optimistic",
      TRUE ~ "No change"
    )
  )

# Performance level labels (common to pre and post after recoding)
perf_labels <- c(`1` = "Bottom 50%", `2` = "Top 50%",
                 `3` = "Top 25%",    `4` = "Top 10%")

cat("\nPre-semester performance expectations:\n")
print(table(factor(matched_survey$perf_pre_recoded,
                   levels = 1:4, labels = perf_labels),
            useNA = "ifany"))

cat("\nPost-semester performance assessments:\n")
print(table(factor(matched_survey$perf_post_recoded,
                   levels = 1:4, labels = perf_labels),
            useNA = "ifany"))

cat("\nDirection of shift from pre to post:\n")
print(table(matched_survey$perf_shift, useNA = "ifany"))

cat("\nCross-tabulation (pre rows x post columns):\n")
print(table(
  pre  = factor(matched_survey$perf_pre_recoded,
                levels = 1:4, labels = perf_labels),
  post = factor(matched_survey$perf_post_recoded,
                levels = 1:4, labels = perf_labels),
  useNA = "ifany"
))

# ------------------------------------------------------------
# STEP 20b: Realized vs. expected performance congruence
# Roadmap: Secondary RQ 3 — "Self-outcome congruence"
#
# Uses continuous final_scoreto compute each student's realized
# percentile rank within the full enrolled class (n=48).
# Percentile rank is then mapped to the same 4-tier ordinal used
# in perf_post_recoded for direct comparison.
#
# Tier definitions:
# Top 10% = 4
# Top 25% = 3
# Top 50% = 2
# Bottom 50% = 1
#
# IMPORTANT ATTRITION NOTE: Students who received F, D, and
# C+ grades are present in the full demo file but did not
# complete the post-survey and are therefore absent from
# matched_survey. The congruence analysis below covers only
# students who completed both surveys (n = 38), all of whom
# received grades of B- or higher. This selective attrition
# should be noted as a study limitation: lower-performing
# students are not represented in survey-based outcomes.
# ------------------------------------------------------------

cat("\n=== STEP 20b: Realized vs. Expected Performance Congruence ===\n")

# Compute percentile rank for every student in the full class 
# using their continuous final score.
# percentile_rank = (number of students scoring below) / (n - 1) * 100
# This give values from 0 to 100.

demo <- demo %>%
  mutate(
    percentile_rank = percent_rank(final_score) * 100
    )

cat("Full class final score summary (n=", nrow(demo), "):\n")
print(summary(demo$final_score))

cat("Percentile rank summary:\n")
print(summary(demo$percentile_rank))

# Map percentile rank to the same 4-tier ordinal 
demo <- demo %>%
  mutate(
    grade_tier = case_when(
      is.na(percentile_rank)    ~ NA_real_,
      percentile_rank >= 90     ~ 4,  # Top 10%
      percentile_rank >= 75     ~ 3,  # Top 25%
      percentile_rank >= 50     ~ 2,  # Top 50%
      percentile_rank <  50     ~ 1   # Bottom 50%
    )
  )

cat("\nGrade tier distribution (full class, n=", nrow(demo), "):\n")
print(table(factor(demo$grade_tier, levels = 1:4, labels = perf_labels), useNA = "ifany"))

# Re-merge demo onto matched_survey so grade_tier
# and percentile_rank are available for congruence analysis
# (removes the old grade_tier column if present, then re-joins)
matched_survey <- matched_survey %>%
  select(-any_of(c("grade_tier", "percentile_rank",
                   "final_score", "congruence",
                   "congruence_dir", "congruence_group"))) %>%
  left_join(demo %>% select(anon_code, final_score,
                            percentile_rank, grade_tier),
            by = c("Anonymous Code" = "anon_code"))

# Compute congruence
matched_survey <- matched_survey %>%
  mutate(
    congruence = perf_post_recoded - grade_tier,
    congruence_dir = case_when(
      is.na(congruence) ~ NA_character_,
      congruence > 0    ~ "Overestimate",
      congruence < 0    ~ "Underestimate",
      congruence == 0   ~ "Accurate"
    )
  )

cat("\nGrade tier distribution (matched sample, n = 38):\n")
print(table(factor(matched_survey$grade_tier,
                   levels = 1:4, labels = perf_labels),
            useNA = "ifany"))

cat("\nPercentile rank in matched sample:\n")
print(summary(matched_survey$percentile_rank))

cat("\nPost self-assessment vs. realized percentile tier — congruence direction:\n")
print(table(matched_survey$congruence_dir, useNA = "ifany"))

cat("\nCongruence score (0 = accurate, + = overestimate, - = underestimate):\n")
print(table(matched_survey$congruence, useNA = "ifany"))

cat("\nCross-tabulation: post self-assessment (rows) x realized percentile tier (cols):\n")
print(table(
  `Post self-assessment` = factor(matched_survey$perf_post_recoded,
                                  levels = 1:4, labels = perf_labels),
  `Realized percentile tier` = factor(matched_survey$grade_tier,
                                    levels = 1:4, labels = perf_labels),
  useNA = "ifany"
))

cat("\nMean congruence score (0 = perfectly accurate):",
    round(mean(matched_survey$congruence, na.rm = TRUE), 2), "\n")
cat("SD:", round(sd(matched_survey$congruence, na.rm = TRUE), 2), "\n")

# Rebuild congruence_group for Step 20c
matched_survey <- matched_survey %>%
  mutate(
    congruence_group = case_when(
      is.na(congruence) ~ NA_character_,
      congruence >= 0   ~ "Accurate or over",
      congruence <  0   ~ "Underestimate"
    )
  )

cat("\nCongruence group sizes:\n")
print(table(matched_survey$congruence_group, useNA = "ifany"))

# ------------------------------------------------------------
# STEP 20c: Engagement patterns by self-awareness group
# Roadmap: Secondary RQ 3 — "Self-outcome congruence"
# Splits matched sample into three congruence groups and
# compares post-intervention engagement profiles across them
#
# Three groups based on congruence score (post self-assessment
# minus actual percentile tier):
#   Underestimate: congruence < 0  (n = 4)
#   Accurate:      congruence = 0  (n = 9)
#   Overestimate:  congruence > 0  (n = 21)
#
# IMPORTANT: Subgroups are small, especially underestimate
# (n = 4). No inferential conclusions can be drawn from
# group comparisons. Results are reported descriptively only
# as exploratory context for the congruence findings.
# ------------------------------------------------------------

cat("\n=== STEP 20c: Engagement by Self-Awareness Group ===\n")

# Redefine congruence_group as three categories
matched_survey <- matched_survey %>%
  mutate(
    congruence_group3 = case_when(
      is.na(congruence)  ~ NA_character_,
      congruence < 0     ~ "Underestimate",
      congruence == 0    ~ "Accurate",
      congruence > 0     ~ "Overestimate"
    )
  )

cat("Congruence group sizes:\n")
print(table(matched_survey$congruence_group3, useNA = "ifany"))

# Post engagement variables to compare
eng_vars <- c(
  "beh_podcast_post",   "beh_casestudy_post",
  "beh_bizcase_post",   "beh_mentor_post",
  "emo_podcast_post",   "emo_casestudy_post",
  "emo_bizcase_post",   "emo_mentor_post",
  "cog_podcast_post",   "cog_casestudy_post",
  "cog_bizcase_post",   "cog_mentor_post"
)

# Descriptive means by group — no significance tests given
# subgroup sizes; reported as exploratory patterns only
cat("\nPost-engagement means by congruence group (exploratory, descriptive only):\n")
group_means3 <- matched_survey %>%
  filter(!is.na(congruence_group3)) %>%
  group_by(congruence_group3) %>%
  summarise(
    n = n(),
    across(all_of(eng_vars), ~ round(mean(., na.rm = TRUE), 2)),
    .groups = "drop"
  )
print(t(group_means3))

# Beliefs/values composites by group
cat("\nBeliefs/values composites by congruence group (pre scores, exploratory):\n")
bv_vars <- c("bv_growth_score_pre", "bv_relevance_score_pre",
             "bv_persist_score_pre")

bv_means3 <- matched_survey %>%
  filter(!is.na(congruence_group3)) %>%
  group_by(congruence_group3) %>%
  summarise(
    n = n(),
    across(all_of(bv_vars), ~ round(mean(., na.rm = TRUE), 2)),
    .groups = "drop"
  )
print(t(bv_means3))

# GPA and percentile rank by group as context
cat("\nGPA and percentile rank by congruence group:\n")
matched_survey %>%
  filter(!is.na(congruence_group3)) %>%
  group_by(congruence_group3) %>%
  summarise(
    n               = n(),
    mean_gpa        = round(mean(gpa, na.rm = TRUE), 2),
    sd_gpa          = round(sd(gpa, na.rm = TRUE), 2),
    mean_percentile = round(mean(percentile_rank, na.rm = TRUE), 1),
    sd_percentile   = round(sd(percentile_rank, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  print()

cat("\nNote: Underestimate group n = 4; Accurate n = 9; Overestimate n = 21.\n")
cat("All group comparisons are exploratory and descriptive only.\n")
cat("No inferential statistics reported given subgroup sizes.\n")

# ------------------------------------------------------------
# STEP 21: EIDP pre-post skill ratings
# Roadmap: Secondary RQ 3 —
#          "Self-outcome congruence: EIDP skills"
# For each of three selected entrepreneurial characteristics,
# compare pre and post self-ratings on five dimensions:
#   confidence, application, strength, alignment, motivation
# Paired Wilcoxon per dimension (ordinal 1-5 scale)
# Null hypothesis: no systematic change in self-rated skill
# Only students with matching characteristics pre and post
# are included (see Step 6 match check)
# ------------------------------------------------------------

cat("\n=== STEP 21: EIDP Pre-Post Skill Ratings ===\n")

eidp_matched <- matched_eidp %>%
  filter(all_chars_match == TRUE)
cat("EIDP students with matching characteristics:", nrow(eidp_matched), "\n")

# Each characteristic maps to a Q number and dimension numbers 1-5
char_qnums  <- c(7, 9, 11)
char_labels <- c("characteristic1", "characteristic2", "characteristic3")
eidp_dims   <- c("confidence", "application", "strength",
                 "alignment", "motivation")

for (i in 1:3) {
  cat("\nCharacteristic", i, ":\n")
  for (j in seq_along(eidp_dims)) {
    pre_var  <- paste0("Q", char_qnums[i], "_", j, "_",
                       char_labels[i], "_", eidp_dims[j], "_pre")
    post_var <- paste0("Q", char_qnums[i], "_", j, "_",
                       char_labels[i], "_", eidp_dims[j], "_post")
    if (pre_var %in% names(eidp_matched) &&
        post_var %in% names(eidp_matched)) {
      result <- paired_wilcox(eidp_matched, pre_var, post_var,
                              paste0("Char ", i, " — ", eidp_dims[j]))
      cat(" ", result$activity_dimension,
          "| mean pre:", result$mean_pre,
          "| mean post:", result$mean_post,
          "| W:", result$W,
          "| p:", result$p_value,
          "| r:", result$r_effect,
          result$sig, "\n")
    }
  }
}

# ------------------------------------------------------------
# STEP 22: General engagement trends
# Roadmap: Supplementary — "General engagement trends"
# General behavioral composite (pre only; post unreliable)
# General cognitive composite (pre and post; t-test)
# Beliefs/values reliable composites (pre and post)
# ------------------------------------------------------------

cat("\n=== STEP 22: General Engagement Trends ===\n")

# General cognitive: reliable at both time points
cog_gen_result <- paired_ttest(matched_survey,
                               "cog_general_pre",
                               "cog_general_post",
                               "Cognitive — General (2-item composite)")
print(cog_gen_result)

# Reliable beliefs/values composites: pre-post t-tests
cat("\nBeliefs/values pre-post comparisons (reliable subscales only):\n")

bv_reliable <- list(
  growth_mindset = c("bv_growth_score_pre",  "bv_growth_score_post"),
  relevance      = c("bv_relevance_score_pre","bv_relevance_score_post"),
  persistence    = c("bv_persist_score_pre",  "bv_persist_score_post")
)

bv_results <- bind_rows(lapply(names(bv_reliable), function(name) {
  vars <- bv_reliable[[name]]
  paired_ttest(matched_survey, vars[1], vars[2], name)
}))
print(bv_results)

# ------------------------------------------------------------
# STEP 23: Course satisfaction
# Roadmap: Supplementary — "Course satisfaction"
# Post-only single item; frequency distribution and mean
# ------------------------------------------------------------

cat("\n=== STEP 23: Course Satisfaction (Post-Only) ===\n")

cat("Satisfaction distribution (1=Strongly dissatisfied,
    5=Strongly satisfied):\n")
print(table(matched_survey$Q_Satisfaction, useNA = "ifany"))
cat("Mean satisfaction:",
    round(mean(matched_survey$Q_Satisfaction, na.rm = TRUE), 2), "\n")
cat("SD:",
    round(sd(matched_survey$Q_Satisfaction, na.rm = TRUE), 2), "\n")

# ------------------------------------------------------------
# Poster Figures
# ------------------------------------------------------------

# ------------------------------------------------------------
# RQ1. Finding 1 — Mentorship ranked #1
# Mentorship dominated 47.4% ranked it most engaging; 
# 57.9% ranked it the activity from which they learned most. 
# Computed directly from matched_survey ranking columns
# ------------------------------------------------------------

# Activity labels matched to ranking variable names
eng_rank_vars <- c(
  "Mentorship"    = "Q26_4_Rank_Engage_Mentor",
  "Podcasting"    = "Q26_1_Rank_Engage_Podcast",
  "Business case" = "Q26_3_Rank_Engage_Biz",
  "Case study"    = "Q26_2_Rank_Engage_CS"
)

lrn_rank_vars <- c(
  "Mentorship"    = "Q29_4_Rank_Learn_Mentor",
  "Podcasting"    = "Q29_1_Rank_Learn_Podcast",
  "Business case" = "Q29_3_Rank_Learn_Biz",
  "Case study"    = "Q29_2_Rank_Learn_CS"
)

# Compute % of students ranking each activity #1
eng_pct <- sapply(eng_rank_vars, function(v)
  round(mean(matched_survey[[v]] == 1, na.rm = TRUE) * 100, 1))

lrn_pct <- sapply(lrn_rank_vars, function(v)
  round(mean(matched_survey[[v]] == 1, na.rm = TRUE) * 100, 1))

rank_data <- bind_rows(
  tibble(activity = names(eng_pct), pct = eng_pct,
         type = "Most Engaging"),
  tibble(activity = names(lrn_pct), pct = lrn_pct,
         type = "Most Learned From")
) %>%
  mutate(
    activity = factor(activity,
                      levels = c("Case study", "Business case",
                                 "Podcasting", "Mentorship")),
    type = factor(type,
                  levels = c("Most Engaging", "Most Learned From"))
  )

# Verify values
print(rank_data %>% arrange(type, desc(pct)))

fig1_caption <- str_wrap(
  "Figure 2. Percentage of students (n = 38) ranking each course activity as most engaging (left) and as the activity from which they learned the most (right). Rankings collected via post-survey; 1 = most engaging/most learned, 4 = least.",
  width = 120
)

ggplot(rank_data, aes(x = pct, y = activity, fill = activity)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.15, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c(
    "Mentorship"    = "#2E5C3E",
    "Podcasting"    = "#1F3864",
    "Business case" = "#2A4A8A",
    "Case study"    = "#888888"
  )) +
  scale_x_continuous(limits = c(0, 75),
                     labels = function(x) paste0(x, "%")) +
  facet_wrap(~ type) +
  labs(title = "Student Rankings of Course Activities",
       subtitle = "Percentage of students ranking each activity #1",
       caption = fig1_caption,
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
        plot.caption = element_text(hjust = 0, size = 9,
                                    color = "gray40", face = "italic"),
        plot.caption.position = "plot",
        strip.text = element_text(face = "bold", size = 12),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(color = "gray50"),
        panel.grid.major.y = element_blank())

ggsave("tilt/finding1_rankings.png",
       width = 9, height = 4.5, dpi = 300)

# ------------------------------------------------------------
# RQ2. Finding 2
# Engagement profiles — change scores by activity and dimension
# Computed directly from matched_survey
# ------------------------------------------------------------

# Cohen's d helper (already defined in analysis script)
# d = mean(change) / sd(change)
compute_d <- function(data, chg_var) {
  x <- data[[chg_var]]
  x <- x[!is.na(x)]
  round(mean(x) / sd(x), 3)
}

# Wilcoxon p-value helper for significance flagging
compute_p <- function(data, pre_var, post_var) {
  x <- data[[pre_var]]
  y <- data[[post_var]]
  valid <- complete.cases(x, y)
  d <- y[valid] - x[valid]
  wilcox.test(d, mu = 0, exact = FALSE)$p.value
}

profile_data <- bind_rows(
  # Podcasting
  tibble(activity = "Podcasting",    dimension = "Behavioral",
         d = compute_d(matched_survey, "chg_beh_podcast"),
         p = compute_p(matched_survey, "beh_podcast_pre",   "beh_podcast_post")),
  tibble(activity = "Podcasting",    dimension = "Emotional",
         d = compute_d(matched_survey, "chg_emo_podcast"),
         p = compute_p(matched_survey, "emo_podcast_pre",   "emo_podcast_post")),
  tibble(activity = "Podcasting",    dimension = "Cognitive",
         d = compute_d(matched_survey, "chg_cog_podcast"),
         p = compute_p(matched_survey, "cog_podcast_pre",   "cog_podcast_post")),
  # Case study
  tibble(activity = "Case study",    dimension = "Behavioral",
         d = compute_d(matched_survey, "chg_beh_casestudy"),
         p = compute_p(matched_survey, "beh_casestudy_pre", "beh_casestudy_post")),
  tibble(activity = "Case study",    dimension = "Emotional",
         d = compute_d(matched_survey, "chg_emo_casestudy"),
         p = compute_p(matched_survey, "emo_casestudy_pre", "emo_casestudy_post")),
  tibble(activity = "Case study",    dimension = "Cognitive",
         d = compute_d(matched_survey, "chg_cog_casestudy"),
         p = compute_p(matched_survey, "cog_casestudy_pre", "cog_casestudy_post")),
  # Business case — emotional pre unavailable
  tibble(activity = "Business case", dimension = "Behavioral",
         d = compute_d(matched_survey, "chg_beh_bizcase"),
         p = compute_p(matched_survey, "beh_bizcase_pre",   "beh_bizcase_post")),
  tibble(activity = "Business case", dimension = "Cognitive",
         d = compute_d(matched_survey, "chg_cog_bizcase"),
         p = compute_p(matched_survey, "cog_bizcase_pre",   "cog_bizcase_post")),
  # Mentorship — emotional pre unavailable
  tibble(activity = "Mentorship",    dimension = "Behavioral",
         d = compute_d(matched_survey, "chg_beh_mentor"),
         p = compute_p(matched_survey, "beh_mentor_pre",    "beh_mentor_post")),
  tibble(activity = "Mentorship",    dimension = "Cognitive",
         d = compute_d(matched_survey, "chg_cog_mentor"),
         p = compute_p(matched_survey, "cog_mentor_pre",    "cog_mentor_post"))
) %>%
  mutate(
    sig = ifelse(p < .05, "*", ""),
    activity  = factor(activity,
                       levels = c("Podcasting", "Case study",
                                  "Business case", "Mentorship")),
    dimension = factor(dimension,
                       levels = c("Behavioral", "Emotional", "Cognitive")),
    color = case_when(
      d >  0.1 ~ "Positive",
      d < -0.1 ~ "Negative",
      TRUE     ~ "Negligible"
    )
  )

# Verify values
print(profile_data %>% select(activity, dimension, d, p, sig))

profile_caption <- str_wrap(
  "Figure 3. Cohen's d effect sizes for pre-to-post engagement change by activity and dimension. Positive values indicate improvement; negative values indicate decline relative to pre-semester expectations. Emotional engagement not available for business case or mentorship at pre-semester due to reliability constraints; omitted from those panels. * p < .05, Wilcoxon signed-rank test, two-sided.",
  width = 120
)

ggplot(profile_data, aes(x = d, y = dimension, fill = color)) +
  geom_col(width = 0.55) +
  geom_vline(xintercept = 0, color = "gray30", linewidth = 0.8) +
  geom_text(aes(label = paste0(ifelse(d > 0, "+", ""), round(d, 2), sig),
                hjust = ifelse(d >= 0, -0.15, 1.15)),
            fontface = "bold", size = 4) +
  scale_fill_manual(values = c(
    "Positive"   = "#1F3864",
    "Negative"   = "#C8960C",
    "Negligible" = "#BBBBBB"
  )) +
  scale_x_continuous(limits = c(-0.65, 0.55),
                     breaks = c(-0.4, -0.2, 0, 0.2, 0.4),
                     labels = c("-0.4", "-0.2", "0", "+0.2", "+0.4")) +
  facet_wrap(~ activity, ncol = 4) +
  labs(title = "Engagement Change by Activity and Dimension",
       subtitle = "Cohen's d (post minus pre)  ·  * p < .05",
       caption = profile_caption,
       x = "Effect size (Cohen's d)", y = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
        plot.caption = element_text(hjust = 0, size = 9,
                                    color = "gray40", face = "italic"),
        plot.caption.position = "plot",
        strip.text = element_text(face = "bold", size = 12),
        axis.text = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

ggsave("tilt/finding2_engagement_profiles.png",
       width = 10, height = 4.5, dpi = 300)

# ------------------------------------------------------------
# RQX. Finding 3
# Knowledge Assessment — Pre/Post Score Distribution
# Computed directly from matched_survey
# ------------------------------------------------------------

# Compute score distributions from matched_survey
ka_data <- bind_rows(
  matched_survey %>%
    count(score = ka_score_pre) %>%
    mutate(time = "Pre"),
  matched_survey %>%
    count(score = ka_score_post) %>%
    mutate(time = "Post")
) %>%
  # Ensure all score levels appear for both time points
  # (e.g. if no one scored 0 or 1 at post, still show as 0)
  complete(score, time, fill = list(n = 0)) %>%
  filter(score >= 2) %>%  # drop scores below 2 if none present
  mutate(
    time  = factor(time, levels = c("Pre", "Post")),
    score = factor(score,
                   levels = sort(unique(score)),
                   labels = paste0(sort(unique(score)), " of 5"))
  )

# Verify distributions match Step 13 output
cat("Pre distribution:\n")
print(matched_survey %>% count(ka_score_pre))
cat("\nPost distribution:\n")
print(matched_survey %>% count(ka_score_post))

# Compute mean scores for annotation
mean_pre  <- round(mean(matched_survey$ka_score_pre,  na.rm = TRUE), 2)
mean_post <- round(mean(matched_survey$ka_score_post, na.rm = TRUE), 2)

ka_caption <- str_wrap(
  "Figure 4. Distribution of knowledge assessment scores at pre- and post-semester (n = 38). Score = number correct out of 5 items retained for adequate response rates. Mean improved from 3.82 to 4.34 (W = 234, p = .003, r = 0.37, medium effect). The combination of active learning strategies precludes attribution to any single activity; the result is consistent with a course environment that supported content comprehension alongside engagement.",
  width = 120
)

ggplot(ka_data, aes(x = score, y = n, fill = time)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = ifelse(n > 0, n, "")),
            position = position_dodge(width = 0.7),
            vjust = -0.4, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Pre" = "#D5E3F0", "Post" = "#1F3864"),
                    name = NULL) +
  scale_y_continuous(limits = c(0, 22), breaks = seq(0, 20, 5)) +
  annotate("text", x = 3.5, y = 21,
           label = paste0("Mean: ", mean_pre, " \u2192 ", mean_post,
                          "  (p = .003, r = 0.37)"),
           fontface = "bold.italic", size = 4.5, color = "#1F3864") +
  labs(title = "Knowledge Assessment: Pre- and Post-Semester Scores",
       subtitle = "Students demonstrated significant gains in content knowledge over the semester",
       caption = ka_caption,
       x = "Score (items correct out of 5)",
       y = "Number of students") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
        plot.caption = element_text(hjust = 0, size = 9,
                                    color = "gray40", face = "italic"),
        plot.caption.position = "plot",
        legend.position = "top",
        axis.text = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

ggsave("tilt/finding3_knowledge.png", width = 7, height = 5, dpi = 300)

# ------------------------------------------------------------
# RQ3. Finding 4 — Correlation heatmap
# Beliefs predict engagement intentions r = 0.50 to 0.65 
# across all nine correlations. 
# ------------------------------------------------------------

# Define variable groups with updated labels
bv_vars <- c(
  "Growth mindset"              = "bv_growth_score_pre",
  "Grit"                        = "bv_persist_score_pre",
  "Entrepreneurial identity"    = "bv_relevance_score_pre"
)

beh_vars <- c(
  "Podcasting"    = "beh_podcast_pre",
  "Case study"    = "beh_casestudy_pre",
  "Business case" = "beh_bizcase_pre",
  "Mentorship"    = "beh_mentor_pre"
)

# Compute Pearson correlations between each belief/values
# composite and each anticipated behavioral engagement item
cor_long <- expand.grid(
  belief   = names(bv_vars),
  activity = names(beh_vars),
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    r = cor(
      matched_survey[[bv_vars[belief]]],
      matched_survey[[beh_vars[activity]]],
      use = "complete.obs",
      method = "pearson"
    )
  ) %>%
  ungroup() %>%
  mutate(
    belief   = factor(belief,
                      levels = c("Growth mindset", "Grit",
                                 "Entrepreneurial identity")),
    activity = factor(activity,
                      levels = c("Podcasting", "Case study",
                                 "Business case", "Mentorship"))
  )

# Verify correlations match previously reported values
print(cor_long %>% arrange(belief, activity) %>%
        mutate(r = round(r, 3)))

fig2_caption <- str_wrap(
  "Figure 5. Pearson correlations between pre-semester beliefs and values composites and anticipated behavioral engagement across the four course activities (n = 38). Growth mindset = belief that intelligence and ability can be developed through effort (Dweck et al., 2014). Grit = tendency to follow through on goals and overcome setbacks. Entrepreneurial identity = perceived applicability of entrepreneurship to students' own lives, interests, and futures. All 12 correlations are positive and range from r = 0.50 to r = 0.65.",
  width = 120
)

ggplot(cor_long, aes(x = activity, y = belief, fill = r)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(r, 2)), size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#D5E3F0", high = "#1F3864",
                      limits = c(0.45, 0.70),
                      name = "Pearson r") +
  scale_y_discrete(labels = scales::label_wrap(13)) +
  labs(title = "Beliefs and Values vs. Anticipated Behavioral Engagement",
       caption = fig2_caption,
       x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", lineheight = 0.9),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 9,
                                    color = "gray40", face = "italic"),
        plot.caption.position = "plot",
        legend.position = "right")

ggsave("tilt/finding4_correlation_heatmap.png",
       width = 7, height = 4, dpi = 300)

# ------------------------------------------------------------
# Beliefs vs. post-semester realized behavioral engagement
# Side-by-side with pre-semester anticipated engagement
# for direct comparison
# ------------------------------------------------------------

# Post behavioral engagement variables
beh_vars_post <- c(
  "Podcasting"    = "beh_podcast_post",
  "Case study"    = "beh_casestudy_post",
  "Business case" = "beh_bizcase_post",
  "Mentorship"    = "beh_mentor_post"
)

# Compute correlations: beliefs (pre) vs. realized engagement (post)
cor_long_post <- expand.grid(
  belief   = names(bv_vars),
  activity = names(beh_vars_post),
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    r = cor(
      matched_survey[[bv_vars[belief]]],
      matched_survey[[beh_vars_post[activity]]],
      use = "complete.obs",
      method = "pearson"
    )
  ) %>%
  ungroup() %>%
  mutate(
    belief   = factor(belief,
                      levels = c("Growth mindset", "Grit",
                                 "Entrepreneurial identity")),
    activity = factor(activity,
                      levels = c("Podcasting", "Case study",
                                 "Business case", "Mentorship")),
    timing = "Realized (Post-Semester)"
  )

# Add timing label to existing pre correlations
cor_long_pre <- cor_long %>%
  mutate(timing = "Anticipated (Pre-Semester)")

# Combine
cor_both <- bind_rows(cor_long_pre, cor_long_post) %>%
  mutate(timing = factor(timing,
                         levels = c("Anticipated (Pre-Semester)",
                                    "Realized (Post-Semester)")))

# Verify post correlations
cat("Post-semester correlations:\n")
print(cor_long_post %>% arrange(belief, activity) %>%
        mutate(r = round(r, 3)))

fig_cor_both_caption <- str_wrap(
  "Figure 5. Pearson correlations between pre-semester beliefs and values composites and behavioral engagement — anticipated at pre-semester (left) and realized at post-semester (right) — across the four course activities (n = 38). Pre-semester correlations range from r = 0.50 to r = 0.65, all p ≤ .001. Post-semester correlations range from r = 0.06 to r = 0.32, none significant (all p > .05). Growth mindset = belief that intelligence and ability can be developed through effort (Dweck et al., 2014). Grit = tendency to follow through on goals and overcome setbacks. Entrepreneurial identity = perceived applicability of entrepreneurship to students' own lives, interests, and futures.",
  width = 150
)

# Shared color scale across both panels
r_limits <- range(cor_both$r, na.rm = TRUE)
r_limits <- c(floor(min(r_limits) * 20) / 20,
              ceiling(max(r_limits) * 20) / 20)

ggplot(cor_both, aes(x = activity, y = belief, fill = r)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(r, 2)), size = 4.5, fontface = "bold") +
  scale_fill_gradient(low = "#D5E3F0", high = "#1F3864",
                      limits = r_limits,
                      name = "Pearson r") +
  scale_y_discrete(labels = scales::label_wrap(13)) +
  facet_wrap(~ timing) +
  labs(title = "Beliefs and Values vs. Behavioral Engagement: Anticipated and Realized",
       caption = fig_cor_both_caption,
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(axis.text = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold", lineheight = 0.9),
        axis.text.x = element_text(angle = 15, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.caption = element_text(hjust = 0, size = 9,
                                    color = "gray40", face = "italic"),
        plot.caption.position = "plot",
        strip.text = element_text(face = "bold", size = 12),
        legend.position = "right")

ggsave("tilt/finding_beliefs_pre_post_engagement.png",
       width = 11, height = 4.5, dpi = 300)

# Test significance of post-semester correlations
cor_long_post %>%
  rowwise() %>%
  mutate(
    p_value = cor.test(
      matched_survey[[bv_vars[belief]]],
      matched_survey[[beh_vars_post[activity]]],
      use = "complete.obs",
      method = "pearson"
    )$p.value,
    p_value = round(p_value, 3),
    sig = ifelse(p_value < .05, "*", "")
  ) %>%
  select(belief, activity, r, p_value, sig) %>%
  print(n = 12)

# Test significance of pre-semester correlations
expand.grid(
  belief   = names(bv_vars),
  activity = names(beh_vars),
  stringsAsFactors = FALSE
) %>%
  rowwise() %>%
  mutate(
    r = cor(
      matched_survey[[bv_vars[belief]]],
      matched_survey[[beh_vars[activity]]],
      use = "complete.obs",
      method = "pearson"
    ),
    p_value = cor.test(
      matched_survey[[bv_vars[belief]]],
      matched_survey[[beh_vars[activity]]],
      use = "complete.obs",
      method = "pearson"
    )$p.value,
    r       = round(r, 3),
    p_value = round(p_value, 3),
    sig     = ifelse(p_value < .05, "*", "")
  ) %>%
  ungroup() %>%
  select(belief, activity, r, p_value, sig) %>%
  arrange(belief, activity) %>%
  print(n = 12)

# ------------------------------------------------------------
# RQ4. Finding 5 — Dunning-Kruger chart
# Dunning-Kruger pattern in self-assessment
# Overestimators: mean percentile rank 44.9. 
# Accurate students: 82.6. Underestimators: 89.1. 
# Computed directly from matched_survey
# ------------------------------------------------------------

# Compute group sizes, mean percentile, and mean GPA
# directly from matched_survey
dkg_data <- matched_survey %>%
  filter(!is.na(congruence_group3)) %>%
  group_by(congruence_group3) %>%
  summarise(
    n               = n(),
    mean_percentile = round(mean(percentile_rank, na.rm = TRUE), 1),
    mean_gpa        = round(mean(gpa, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    # Build group label with n embedded, matching plot style
    group = factor(
      paste0(congruence_group3, "\n(n=", n, ")"),
      levels = paste0(
        c("Underestimate", "Accurate", "Overestimate"),
        "\n(n=",
        n[match(c("Underestimate", "Accurate", "Overestimate"),
                congruence_group3)],
        ")"
      )
    )
  )

# Verify values
print(dkg_data)

fig_dkg_caption <- str_wrap(
  "Figure 6. Mean class percentile rank by congruence group among matched survey respondents (n = 34 with complete data; 4 excluded due to missing post-survey performance item). Percentile ranks computed across all enrolled students (n = 48) using continuous final scores. Congruence defined as post-semester self-assessment minus actual percentile tier; negative = underestimate, zero = accurate, positive = overestimate. Most overestimators misplaced themselves by one tier; however, the lowest-performing overestimators showed the largest gaps between self-assessment and actual standing, consistent with the Dunning-Kruger effect (Kruger & Dunning, 1999).",
  width = 120
)

ggplot(dkg_data, aes(x = group, y = mean_percentile, fill = congruence_group3)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = paste0(mean_percentile, "th\npercentile")),
            vjust = -0.4, fontface = "bold", size = 5) +
  scale_fill_manual(values = c(
    "Underestimate" = "#2E5C3E",
    "Accurate"      = "#1F3864",
    "Overestimate"  = "#C8960C"
  )) +
  scale_y_continuous(limits = c(0, 105),
                     labels = function(x) paste0(x, "th")) +
  labs(title = "Mean Class Percentile Rank by Self-Assessment Group",
       subtitle = "Students who overestimated their performance fell in the lower half of the class on average",
       caption = fig_dkg_caption,
       x = "Congruence Group", y = "Mean Percentile Rank") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40", size = 11),
        plot.caption = element_text(hjust = 0, size = 9,
                                    color = "gray40", face = "italic"),
        plot.caption.position = "plot",
        axis.text = element_text(face = "bold"))

ggsave("tilt/finding5_dunning_kruger.png",
       width = 8, height = 5.5, dpi = 300)

# ------------------------------------------------------------
# RQ4. Finding 6 — EIDP pre-post gains
# EIDP skill development
# Only confidence and strength shown (significant dimensions)
# Confidence and strength improved significantly for 
# characteristics 1 and 2. 
# Computed directly from eidp_matched
# ------------------------------------------------------------

# Variable mapping: dimension name -> pre and post column names
eidp_vars <- list(
  list(dimension = "Confidence", characteristic = "Characteristic 1",
       pre  = "Q7_1_characteristic1_confidence_pre",
       post = "Q7_1_characteristic1_confidence_post"),
  list(dimension = "Strength",   characteristic = "Characteristic 1",
       pre  = "Q7_3_characteristic1_strength_pre",
       post = "Q7_3_characteristic1_strength_post"),
  list(dimension = "Confidence", characteristic = "Characteristic 2",
       pre  = "Q9_1_characteristic2_confidence_pre",
       post = "Q9_1_characteristic2_confidence_post"),
  list(dimension = "Strength",   characteristic = "Characteristic 2",
       pre  = "Q9_3_characteristic2_strength_pre",
       post = "Q9_3_characteristic2_strength_post")
)

# Compute pre and post means from eidp_matched
eidp_data <- bind_rows(lapply(eidp_vars, function(v) {
  tibble(
    dimension      = v$dimension,
    characteristic = v$characteristic,
    pre            = round(mean(eidp_matched[[v$pre]],  na.rm = TRUE), 2),
    post           = round(mean(eidp_matched[[v$post]], na.rm = TRUE), 2)
  )
})) %>%
  pivot_longer(cols = c(pre, post),
               names_to = "time", values_to = "mean") %>%
  mutate(
    time = factor(time, levels = c("pre", "post"),
                  labels = c("Pre", "Post")),
    dimension      = factor(dimension,
                            levels = c("Confidence", "Strength")),
    characteristic = factor(characteristic,
                            levels = c("Characteristic 1",
                                       "Characteristic 2"))
  )

# Verify values
print(eidp_data %>% arrange(characteristic, dimension, time))

fig_eidp_caption <- str_wrap(
  "Figure 7. Pre- and post-semester self-ratings on confidence and strength for students' first and second selected entrepreneurial characteristics (n = 21 with matched selections). Alignment and motivation are not shown; both were near ceiling at pre-semester and did not change significantly. Scale: 1 = Strongly disagree, 5 = Strongly agree. * p < .05, Wilcoxon signed-rank test, two-sided.",
  width = 120
)

ggplot(eidp_data, aes(x = time, y = mean,
                      group = interaction(dimension, characteristic),
                      color = dimension)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  facet_wrap(~ characteristic) +
  scale_color_manual(values = c("Confidence" = "#1F3864",
                                "Strength"   = "#2E5C3E")) +
  scale_y_continuous(limits = c(3, 5), breaks = 3:5) +
  labs(title = "E-IDP: Significant Pre-Post Gains in Confidence and Strength",
       subtitle = "* p < .05, Wilcoxon signed-rank",
       caption = fig_eidp_caption,
       x = NULL, y = "Mean Self-Rating (1–5)",
       color = "Dimension") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
        plot.caption = element_text(hjust = 0, size = 9,
                                    color = "gray40", face = "italic"),
        plot.caption.position = "plot",
        strip.text = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("tilt/finding6_eidp_gains.png", width = 7, height = 5, dpi = 300)


# ------------------------------------------------------------
# Sample retention rates
# Pre/post survey and E-IDP matched samples
# ------------------------------------------------------------

# Survey retention
n_pre_survey   <- nrow(pre_quant)
n_post_survey  <- nrow(post_quant)
n_matched_survey <- nrow(matched_survey)

cat("=== Survey Retention ===\n")
cat("Pre-survey respondents:         ", n_pre_survey, "\n")
cat("Post-survey respondents:        ", n_post_survey, "\n")
cat("Matched (both pre and post):    ", n_matched_survey, "\n")
cat("Retention rate (pre -> matched):",
    round(n_matched_survey / n_pre_survey * 100, 1), "%\n")
cat("Pre-only (did not complete post):",
    n_pre_survey - n_matched_survey, "\n")
cat("Post-only (no pre on file):     ",
    n_post_survey - n_matched_survey, "\n")

cat("\n=== E-IDP Retention ===\n")
n_pre_eidp   <- nrow(pre_eidp)
n_post_eidp  <- nrow(post_eidp)
n_matched_eidp <- nrow(matched_eidp)
n_matched_chars <- nrow(eidp_matched)  # matching characteristics

cat("Pre-EIDP respondents:                    ", n_pre_eidp, "\n")
cat("Post-EIDP respondents:                   ", n_post_eidp, "\n")
cat("Matched (both pre and post):             ", n_matched_eidp, "\n")
cat("Retention rate (pre -> matched):         ",
    round(n_matched_eidp / n_pre_eidp * 100, 1), "%\n")
cat("Matched with same characteristics pre/post:", n_matched_chars, "\n")
cat("Retention rate (pre -> same chars):      ",
    round(n_matched_chars / n_pre_eidp * 100, 1), "%\n")
cat("Students who changed characteristics:    ",
    n_matched_eidp - n_matched_chars, "\n")

cat("\n=== Full Enrollment Context ===\n")
n_enrolled <- nrow(demo)
cat("Total enrolled (demo file):              ", n_enrolled, "\n")
cat("Survey match rate (of enrolled):         ",
    round(n_matched_survey / n_enrolled * 100, 1), "%\n")
cat("E-IDP match rate (of enrolled):          ",
    round(n_matched_eidp / n_enrolled * 100, 1), "%\n")
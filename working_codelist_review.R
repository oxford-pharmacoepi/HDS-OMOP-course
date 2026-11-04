
# reviewCohortConcepts
# reviewOrphanConcepts

library(ellmer)
library(dplyr)
library(stringr)

chat_medgemma <- chat_ollama(
  model = "MedGemma:4b"
)

PhenotypeR::draftClinicalDescription(chat_medgemma,
                                     c("hypertension",
                                       "primary hypertension",
                                       "secondary hypertension"),
                                     here::here())
# clinical_desc_json <- readr::read_file(here::here("hypertension.json"))


reviewCohortConcepts <- function(chat, cohortConcepts, name, clinicalDescription){


prompt <- interpolate(
  "You are a clinical epidemiologist reviewing candidate OMOP concepts to build a codelist for: {{target_phenotype}}.

Use the following clinical description as your source of truth for the clinical idea the cohort is meant to represent:
<clinical_description>
{{clinical_description}}
</clinical_description>

Classification Rules:
- 'include': Concepts that are commonly used to specifically represent {{target_phenotype}}, its synonyms, or included subtypes.
- 'exclude': Concepts that match exclusion criteria in the clinical description, something biologically distinct/ separate from {{target_phenotype}}, or just completely unrelated clinical entities.
- 'uncertain': Ambiguous concepts that could plausibly represent {{target_phenotype}} in clinical coding but are broader/ higher-level description and so require clinical review and/ or further specification of the phenotyping intent (e.g. incident vs prevalent cases, sensitivity vs specificity prioritisation, etc).

Formatting Rule: Write EXACTLY ONE short sentence (under 15 words) starting with 'Included because', 'Excluded because', or 'Uncertain because'.

Examples of format:
- explanation: 'Included because it is a standard clinical term specifically for {{target_phenotype}}.' -> score: 'include'
- explanation: 'Included because it represents a clinical subtype of {{target_phenotype}}.' -> score: 'include'
- explanation: 'Uncertain because the term is non-diagnostic or a history code requiring manual review.' -> score: 'uncertain'
- explanation: 'Uncertain because the term applies to a specific subpopulation that depends on cohort study design.' -> score: 'uncertain'
- explanation: 'Uncertain because the term is broader than solely {{target_phenotype}}. It could represent {{target_phenotype}}, but could also represent other diagnoses/procedures/medications so includion depends on sensitivity/ specificity trade off.' -> score: 'uncertain'
- explanation: 'Excluded because it represents an unrelated clinical entity.' -> score: 'exclude'
- explanation: 'Excluded because it represents a distinct or explicitly excluded subtype.' -> score: 'exclude'",
  target_phenotype = name,
  clinical_description = readr::read_file(clinicalDescription)
)

chat <- chat$clone()$set_turns(list())$set_system_prompt(prompt)


# 2. Schema reinforces starting with "Included/Excluded/Uncertain because..."
concept_review_schema <- type_object(
  "Evaluation of an OMOP concept for a study cohort.",
  explanation = type_string(
    "One short sentence (max 15 words) starting with 'Included because...', 'Excluded because...', or 'Uncertain because...'."
  ),
  score = type_enum(
    values = c("include", "uncertain", "exclude"),
    description = "Classification matching the explanation: 'include', 'uncertain', or 'exclude'."
  )
)

prompts <- as.list(
  paste0("Concept: '", cohortConcepts$concept_name, "'")
)

results <- parallel_chat_structured(
  chat = chat,
  prompts = prompts,
  type = concept_review_schema
)

# 3. Bind, recode 'uncertain', and enforce 1-sentence truncation just in case
concepts_reviewed <- bind_cols(cohortConcepts, results) |>
  mutate(
    score = recode(score, "uncertain" = "check/ uncertain"),
    explanation = str_extract(explanation, "^[^.!?]+[.!?]") |>
      coalesce(explanation) |>
      str_trim()
  ) |>
  select(concept_id, concept_name, score, explanation)

concepts_reviewed
}



included_concepts <- data.frame(
  concept_id = c(1, 2, 3, 4),
  concept_name = c("primary hypertension", "secondary hypertension", "high blood pressure", "asthma")
)
reviewCohortConcepts(chat_medgemma, included_concepts, "hypertension", here::here("hypertension.json"))
reviewCohortConcepts(chat_medgemma, included_concepts, "primary hypertension", here::here("primary hypertension.json"))
reviewCohortConcepts(chat_medgemma, included_concepts, "secondary hypertension", here::here("secondary hypertension.json"))

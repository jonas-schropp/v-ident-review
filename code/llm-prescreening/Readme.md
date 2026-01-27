# LLM Pre-Screening

This Folder contains the pre-screening code used to speed up initial screening of titles and abstracts. It refers to step 1 of the following excerpt from the protocol:  


All records retrieved from the literature search were imported into R for preprocessing andscreening. Initial deduplication was performed by comparing DOIs and matching titles using string distance measures. A four-stage screening process was employed:  

1. **LLM-based prescreening: The ChatGPT Turbo API is used to prescreen titles and abstracts against the predefined eligibility criteria. The prompt and LLM parameters used for thisprescreening procedure are reported in Appendix B.**
2. Manual title and abstract screening: One reviewer who is blinded to the prior LLMprescreening results screens all titles and abstracts for relevance. A small calibration subset is reviewed beforehand to ensure consistent application of eligibility criteria.
3. Conflict resolution screening: Records where LLM and manual screening disagreed areindependently screened by a second reviewer who is blinded to the prior screening results.
4. Full-text screening: All potentially relevant articles are retrieved in full and assessedindependently by both reviewers. Disagreements at any stage are resolved through discussionor by consulting a third reviewer.The full selection process is conducted in accordance with PRISMA guidelines and will bevisualized using a PRISMA flow diagram. 

## Purpose

To support the initial screening of records while preserving reviewer blinding and independenteligibility assessment, an automated prescreening step using a large language model (LLM) wasapplied to titles and abstracts. The LLM produced an initial eligibility label (“include”, “exclude”,or “unsure”) and a brief justification for each record.   

These outputs were used for workflow triageand were not considered final inclusion decisions.  

## Inputs

For each record, the following information was provided to the LLM:  

 - Title
 - Abstract
 - Predefined inclusion criteria (as a single text block)
 - Predefined exclusion criteria (as a single text block)

## Prompt (verbatim)

The following prompt was used for each record (with {title} and {abstract} replaced by the corresponding record fields, and {inclusion_criteria} / {exclusion_criteria} replaced by the protocol criteria text):  

You are helping with a systematic review. Classify the following paper basedon the inclusion and exclusion criteria.  

Inclusion: {inclusion_criteria}  
Exclusion: {exclusion_criteria}  
Paper:Title: {title}  
Abstract: {abstract}  
Answer format (use JSON):{"decision": <include / exclude / unsure>,"reason": <short reason>}  
Only use the terms 'include', 'exclude', or 'unsure' as the decision.  

## Model and execution environment

LLM inference was performed using GPT-4 Turbo (June 2025 version) accessed via the OpenAIPython SDK through the OpenRouter-compatible API endpoint (https://openrouter.ai/api/v1). Screening was conducted in June 2025.  

## Inference Parameters

 - Temperature: 0.2
 - Maximum output tokens: 150
 - Message format: single user message containing the full prompt 

## Output schema and parsing 

The LLM was instructed to return a JSON object with two fields:  

 - decision: one of include, exclude, or unsure
 - reason: a short free-text explanation

To ensure robust parsing, Markdown code-fence wrappers (e.g., json ... ) were removed before attempting JSON decoding. If JSON parsing failed, the record was assigned:  
 
 - decision = "unsure"
 - reason = "Failed to parse JSON: <truncated raw output>"

If the parsed decision value was not one of the allowed labels, it was automatically coerced to “unsure”.  

## Reliability safeguards

Each record was screened with up to three attempts to obtain a valid JSON response. If a valid response was obtained, the retry loop terminated. Rate-limit errors triggered incremental waitingperiods before retrying. Other exceptions resulted in an “unsure” classification with thecorresponding error message recorded as the reason.Stored outputsFor each screened record, the following fields were stored:  

 - llm_decision: include / exclude / unsure
 - llm_reason: truncated to 500 characters

Token usage (prompt and completion tokens) was accumulated across all records to document thescale of the LLM screening step.Role in the screening workflowLLM prescreening served only as an initial workflow aid. Human screening followed the predefined multi-stage process described in the main protocol, including blinded manual title/abstract screening and independent conflict-resolution screening. Disagreements between LLM output and human decisions were resolved exclusively through the human Reviewer workflow. The LLM was not used for full-text screening, data extraction, risk-of-bias assessment, or final eligibility decisions.  
 

## For more Information:
Vital-Sign-Based Passive Authentication in Wearable Devices: Protocol for a Systematic Review and Meta-Analysis of Accuracy. Available from: [Researchgate](https://www.researchgate.net/publication/399670795_Vital-Sign-Based_Passive_Authentication_in_Wearable_Devices_Protocol_for_a_Systematic_Review_and_Meta-Analysis_of_Accuracy).
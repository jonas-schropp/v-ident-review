# Keyword Generation

To develop a comprehensive and reproducible search strategy, we followed an iterative processgrounded in best practices for systematic evidence synthesis and incorporating both expert inputand semi-automated keyword refinement tools. Our initial query was constructed to target studiesthat use physiological signals or vital signs for user authentication. The initial Boolean expressioncombined terms for biometric signals with those for authentication purposes, and was structured as follows:

("biometrics" OR "vital sign*" OR "physiological signal*" OR "biosignal*" OR"biometric signal*" OR "biophysical signal) AND ("user recognition" OR "identity verification" OR "user authentication")

To refine this query, we employed the following multi-step approach:

1. Seed Set Generation: We first identify a small, representative seed set of highly relevantpublications from previous knowledge and preliminary database queries. To increase thebreadth of information, we include review papers into the seed set.
2. Text Mining via Co-occurrence Networks: We apply the litsearchr package  in R to analyze keyword co-occurrence across titles and abstracts in the seed set. This enables us toextract and visualize frequently co-occurring terms that may indicate overlooked synonyms,related modalities, or alternate phrasing in the literature.
3. Term Frequency–Inverse Document Frequency (TF-IDF): We calculated TF-IDF scores forcandidate terms to prioritize inclusion of those most distinctive to the domain of interest, thusenhancing the query’s precision without unduly compromising recall. 
4. LLM-assisted Query Expansion: To further broaden and validate our terminology coverage,we employed a large language model (LLM) to suggest additional semantically similar orrelated terms not identified via co-occurrence analysis. These suggestions were manuallyreviewed for relevance and incorporated when appropriate. 

## LLM Query Expansion Prompt

I'm conducting a systematic review on authentication based on vital signs.  

My research objectives are:  

Primary Objectives  

 - Which types of vital signs, individually or in combination, have been used for user authentication?
 - What computational or machine learning methods have been employed to perform user authentication based on vital signs?
Secondary Objectives
 - In what types of devices or systems are vital sign–based user authentication methods integrated (e.g., wearables, clinical monitors, security systems)?
 - What are the results of the presented approach (number of used data sets for evaluation, confusion matrix)? 
 - What are the remaining challenges and problems?
 - What use cases or application domains have been explored for user authentication using vital signs (e.g., authentication, continuous monitoring, security)?

I've already devised the following search string:  

Vital signs and biosignals = ("biometric system*", "biometric signal*", "vital sign*", "physiological signal*", "biosignal*", "biophysical signal*",  "heart rate", "respiration rate", "respiratory rate",  "skin temperature", "blood pressure", "ECG", "PPG",  "GSR", "EDA", "EEG") 
Authentication concepts = ("authentication",  "user recognition", "identity verification",  "user access") OR ("biometric access", "biometric recognition", "biometric verification")

My inclusion and exclusion criteria are:   

Inclusion criteria:  

 - Studies involving human subjects or datasets that include vital signs or physiological signals for passive user authentication.
 - Concept: Studies that propose, implement, evaluate, or discuss methods for user authentication based on vital signs or physiological signals (e.g., heart rate, respiration rate, blood pressure, ECG, PPG, GSR, EEG).
 - Context: Studies applied in any context (e.g., healthcare, consumer electronics, security systems, HCI, telemedicine, or fitness).
 - Data Type: Studies using real-world data, experimental data, or simulated data representing vital signs.
 - Study Type: Peer-reviewed articles, conference proceedings, or preprints presenting original research.
 - Publication Date: Published between January 01, 2015 and June 01, 2025.
 - Language: Articles published in English or German.
Exclusion criteria:
 - Studies focusing exclusively on biometric authentication not involving vital signs (e.g., facial recognition, iris scans, fingerprinting).
 - Studies that only use demographic or behavioral signals (e.g., keystroke dynamics, gait) without physiological components.
 - Studies that require active intervention by the user (e.g., gestures, signatures) or a second person for authentication (e.g., providing stimuli).
 - Reviews, editorials, opinion papers, or patents (though relevant reviews will be screened for additional sources).
 - Studies without sufficient methodological detail to understand the signal types and authentication methods used.
 - Animal studies or purely theoretical/mathematical modeling without application to biometric authentication.
  
Here are a few seed articles (title + abstract):  

[Paper 1]  
Title: A novel biometric authentication approach using ECG and EMG signals.  

Abstract: Security Biometrics is a secure alternative to traditional methods of identity verification of individuals, such as authentication systems based on user name and password. Recently, it has been found that the electrocardiogram (ECG) signal formed by five successive waves (P, Q, R, S and T) is unique to each individual. In fact, better than any other biometrics’ measures, it delivers proof of subject's being alive as extra information which other biometrics cannot deliver. The main purpose of this work is to present a low-cost method for online acquisition and processing of ECG signals for person authentication and to study the possibility of providing additional information and retrieve personal data from electrocardiogram signal to yield a reliable decision. We explore the effectiveness of a novel biometric system resulting from the fusion of information and knowledge provided by ECG and EMG (Electromyogram) physiological recordings. We showed that biometrics based on these signals ECG/EMG offers a novel way to robustly authenticate subjects. Five ECG databases (MIT-BIH, ST-T, NSR, PTB and ECG-ID) and several ECG signals collected in-house from volunteers were exploited. A palm-based ECG biometric system was developed where the signals are collected from the palm of the subject through a minimally intrusive one-lead ECG setup. A total of 3750 ECG beats were used in this work. Feature extraction was performed on ECG signals using Fourier descriptors (spectral coefficients). Optimum-Path Forest classifier was used to calculate the degree of similarity between individuals. The obtained results from the proposed approach look promising for individuals’ authentication.  

[Paper 2]  

Title: Deep learning-based photoplethysmography biometric authentication for continuous user verification.   

Abstract: Biometric authentication methods have gained prominence as secure and convenient alternatives to traditional passwords and PINs. In this paper, we propose a novel approach for biometric authentication using photoplethysmography (PPG) signals and deep learning techniques. PPG is a non-invasive method that measures variations in blood volume within microvascular tissue beds, and it is typically used for monitoring heart rate and oxygen saturation. Our research leverages the unique characteristics of PPG signals to develop a robust and continuous user verification system. The primary goal of our study is to explore the feasibility and effectiveness of PPG-based biometric authentication, enabling a seamless and secure means of confirming the identity of individuals. We use a diverse dataset of PPG signals from various individuals, ensuring that it encompasses differences in skin tone, age, and other variables that can influence PPG signal characteristics. The collected data undergoes careful preprocessing, including noise removal, baseline correction, and heartbeat segmentation. For the core of our authentication system, we design and train a multiscale feature fusion deep learning (MFFD) model. This model, utilizing a Convolutional Neural Network (CNN) architecture, takes as input the relevant features extracted from PPG signals and learns to differentiate between individuals based on their unique PPG patterns. In this study, the input is constructed by gradually incorporating various features, beginning with a single PPG signal. In this study, the CNN model was trained independently, followed by the implementation of score fusion techniques. Our evaluation demonstrates the effectiveness of the PPG-based biometric authentication system, achieving high accuracy while addressing key security concerns. We consider false acceptance rate (FAR) and false rejection rate (FRR) to assess the system's performance. The model achieves the Accuracy of 99.5 % on BIDMC, 98.6 % on MIMIC, 99.2 % on CapnoBase dataset.  

Can you suggest additional keywords, including synonyms, abbreviations, and related concepts, that I might include in my search strategy to improve coverage without increasing too much noise?  

Please favor terms that are likely to be specific to authentication based on vital signs, rather than general biometric or clinical monitoring terms. If a term is ambiguous (e.g., “monitoring”), note its risk of low precision.


## For more Information:
Vital-Sign-Based Passive Authentication in Wearable Devices: Protocol for a Systematic Review and Meta-Analysis of Accuracy. Available from: [Researchgate](https://www.researchgate.net/publication/399670795_Vital-Sign-Based_Passive_Authentication_in_Wearable_Devices_Protocol_for_a_Systematic_Review_and_Meta-Analysis_of_Accuracy).
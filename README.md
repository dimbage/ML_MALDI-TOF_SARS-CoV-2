# ML_MALDI-TOF_SARS-CoV-2

## The raw data (spectra), R-code, and outcomes acquired for the study titled  "Application of MALDI-MS and Machine Learning to Detection of SARS-CoV-2 and non-SARS-CoV-2 Respiratory Infections"

### Background: 
Matrix-assisted laser desorption/ionization mass spectrometry (MALDI-MS) is an attractive technology to aid the diagnosis of respiratory infections such as SARS-CoV-2 owing to its affordability and high-throughput capacity. MALDI-MS has been proposed for use on commonly available respiratory samples, without specialized sample preparation, a feature that makes MALDI-MS especially suitable for implementation in low-resource settings. Here, we assessed the utility of MALDI-MS in differentiating SARS-CoV-2 versus non-COVID acute respiratory infections (NCARI) in a clinical lab setting of Kazakhstan.
### Methods: 
Nasopharyngeal swabs were collected from in- and outpatients with respiratory symptoms and from asymptomatic controls (AC) in 2020-2022. PCR was used to differentiate SARS-CoV-2+ and NCARI cases. MALDI-MS mass spectra were obtained for a total of 252 samples (115 SARS-CoV-2+, 98 NCARI and 39 AC) without specialized sample preparation. In our first sub-analysis, we followed a previously established protocol for peak preprocessing and Machine Learning (ML), which was trained on publicly available spectra from South American SARS-CoV-2+ and NCARI samples. In our second sub-analysis, we trained ML models on a peak intensity matrix representative of both South American (SA) and Kazakhstan (Kaz) samples.
### Results: 
Applying the previously established MALDI-MS pipeline "as is" resulted in a high detection rate for SARS-CoV-2+ samples (91.0%), but low accuracy for NCARI (48.0%) and AC (67.0%) by the top-performing random forest model. After re-training of the ML algorithms on the SA-Kaz peak intensity matrix, the accuracy of detection by the top-performing decision tree model was at 91.0, 95.0 and 70% for the Kazakhstan SARS-CoV-2+, NCARI, and AC subjects, respectively with a SARS-CoV-2 vs. rest ROC AUC of 0.97 [0.97, 0.98]; a high differentiation accuracy was maintained for the South American SARS-CoV-2 and NCARI. 
### Conclusions:
MALDI-MS/ML is a feasible approach for the differentiation of respiratory infections. Its utility would be highest in the early phases of respiratory endemics/pandemics, when limited knowledge is available on the infectious pathogen’s identity. However, MALDI-MS/ML implementation in real clinical lab settings will necessitate continuous ML training to keep up with the rapidly evolving landscape of respiratory conditions. 

### Ethics approval and consent to participate
All study procedures were approved by the Research Ethics Board of Karaganda Medical University under Protocol 12  (approved 45) from  06.04.2020. Written informed consent was obtained from all participants.

Incorporating Measurement Values into Patient-Level Prediction with Missing Entries: a Feasibility Study
=============

<img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Repo Created">

- Analytics use case(s): **Patient-Level Prediction**
- Study type: **Methods Research**
- Tags: **Bayesian Inference, Missing Imputation, Data Standardization, Measurement Feasability**
- Study lead: **-**
- Study lead forums tag: **[[Lead tag]](https://forums.ohdsi.org/u/[Lead tag])**
- Publications: **-**
- Results explorer: **-**

This research probes the integration of measurement data into prediction models developed under the OHDSI PatientLevelPrediction framework. The study recognizes that while measurement data such as body weight could enhance the predictive capability of these models, their application is hampered by inconsistent units and incomplete entries. The research investigates five databases adhering to the OMOP Common Data Model (CDM) to discern which measurements could potentially be standardized and included in these models. The findings indicate that claims data typically lack an ample amount of measurements for at least 50% of the target population, creating complications for validation across multiple databases. Conversely, the Optum EHR dataset appears promising due to its broader measurement coverage, suggesting its potential utility for prediction models integrating measurements. The study concludes by calling for more comprehensive research to comprehend the effects of missing values and the practicality of imputing these gaps on the performance of prediction models.

All visualizations can be found in *results* folder, and the whole analysis code can be found in *abstract_code*.

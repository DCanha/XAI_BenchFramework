# XAI_BenchFramework
This is the code associated with the master thesis "Building a Benchmark Framework for XAI Methods" (link will be provided soon). The work was developed at Aalto University, integrated in a research group of the Computer Science Department. The thesis was presented at University of Lisbon.

Abstract:

"Artificial intelligence (AI), namely its sub-fields machine learning and deep learning, have demonstrated impressive outcomes in a variety of scientific research domains, such as medicine, security, and finance. However, complex AI systems, despite demonstrating great results and accuracy performances, are seen as black-boxes that suffer from lack of explainability. Therefore, as AI systems continue to grow, it becomes important for humans to understand how each black-box arrived to a certain result. This way, the field of eXplainable artificial intelligence (XAI) arose from the necessity of solving the black-box problem. XAI field has been growing fast, but in different directions, revealing the difficulty the scientific community faces to agree on common definitions and evaluation criteria, which are often formulated in a subjective manner. To overcome this gap in research, the present dissertation proposes a benchmark framework for XAI methods, which is designed based on a methodological systematic literature review in order to derive objective and measurable performance indicators in a comprehensive and consensual manner. This framework is then applied to compare 9 well-known or promising XAI methods (PDP, ICE, PFI, LIME, Anchors, Shapley values, SHAP, Counterfactuals, CIU) considering a tabular dataset from the medicine domain (heart disease prediction). This benchmark study showed the relevancy of the CIU method, which covers to a better extent the 10 selected properties of explainability, when compared to other methods. Moreover, the proposed framework contributes to the settlement of common formalism and taxonomy, which promotes the uniformity that is lacking in the XAI field."

The 10 selected properties are:

- Representativeness: Describes the extent to which the method "looks" inside the black-box model.
- Structure & Speed: Describes how and how fast the explanation is provided.
- Selectivity: Considers the size of the explanation.
- Contrastivity: Assesses how contrastive the explanation is w.r.t. some reference target.
- Interactivity: Assesses the extent to which a user can control or explore the explanations.
- Fidelity: Assesses if the method creates a surrogate model or makes linearity assumptions.
- Faithfulness: Describes how reliable the explanation is w.r.t. the black box.
- Truthfulness: Describes how reliable the explanation is w.r.t. the true world.
- Stability: Assesses how stable and consistent the method is.
- (Un)Certainty: Assesses if the method provides (un)certainty measurements together with the explanation.

The application of the framework showed that explainability is a multi-faceted concept and that the 10 properties are all connected to some extent and contribute to the complete evaluation of XAI methods regarding its explanation quality or validation and its target group. Validation properties are fidelity, faithfulness, stability, (un)certainty, contrastivity, and truthfulness. Contrastivity and truthfulness also assess the quality of the explanations, together with the remaining 5 properties. These are relevant, but can usually depend on the application, and sometimes can actually be improved. Objective metrics were proposed to evaluate each of the mentioned properties. All metrics, whether qualitative or quantitative, should be accompanied by careful and relevant discussion. Quantitative metrics were implemented for 5 properties: Selectivity, Contrastivity, Fidelity, Faithfulness, and Stability. Qualitative properties were used for the remaining properties. 

All used software was written in R and is available as opensource on Github, including the source code for producing the results shown here. The experiments were run using R JupyterLab 5.0.11 (with R version 4.2.1), available for general use in JupyterHub for anyone at Aalto University.

The code developed here to implement the quantitative metrics is ready to be used for tabular datasets and both classification and regression problems - see file “Benchmark.R”.

“Global.R” script contains the code to compute the global feature importance values for shapley values, kernelSHAP, and CIU.

The following jupyter notebooks are possible to access:
 - 01_Data: Data preprocessing results and main conclusions drawn after performing an exploratory data analysis (EDA) on the training data. Cleaned data (data) and training + testing datasets were saved as csv files in folder "Data". Raw data is also present in this folder (heart).
 - 02_Models: Implementation of logistic regression, random forest, and support vector machine models, together with relevant evaluation metrics for both training and testing datasets. Trained models were saved as RDS files - folder "Models".
 - 03_Explanations: Code to produce the explanations for all used XAI methods. Global explanations are saved in folder "Global". Local explanations for patientas A, B, and C are saved in folders "Local_A", "Local_B", and "Local_C", respectively.
 - 04_Benchmark: Code to produce the quantitative evaluation results for all used XAI methods.
 - kernelSHAP: Code for producing the results for kernelSHAP. This is done in a separate notebook because "shapper" package only works with a lower version of R than the one provided by JupyerLab from Aalto University. Therefore, all the source code for producing the results for kernelSHAP was written using Anaconda 2.3.2.
 
 "Adversarial" folder contains the clinical (feature) values of patient A after an adversarial attack simulation on this patient's data (considering the 3 trained models). 

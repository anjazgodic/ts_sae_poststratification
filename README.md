## Code for Tourette Syndrome Analysis in "Using Small-Area Estimation (SAE) to Estimate Prevalence of Child Health Outcomes at the Census Regional, State, and County Levels"

Small area estimation (SAE) is a powerful tool with many strengths. Primarily, it estimates prevalence rates in areas with small sample sizes by borrowing information from areas with larger sample sizes. Moreover, it is also a flexible technique. SAE can employ individual-level as well as area-level predictors for data that is publicly available (e.g., Tourette Syndrome), for data that requires simplifying assumptions (e.g., overweight/obesity, Zgodic 2020), and for data that relies on other SAE models  (e.g., autism spectrum disorder). SAE can also incorporate spatial information using intrinsic conditional autoregressive random intercept terms (ICAR; Banerjee et al., 2004; Cressie, 1993). To aggregate group level estimated prevalences, a common tool is poststratification, which creates weighted averages that reflect a population’s underlying distribution of characteristics. 

The code files in this repository provide an example of SAE with poststratification using data from the National Survey of Children's Health. The outcome of interest is Tourette Syndrome status at the child level. The SAS code file $\verbatim{code_model_ts.sas}$

# Data Dictionary for Q17 Coding

- `Rep_flag`: Based on the reason for replication and the definition of replication, does the survey respondent appear to be thinking of replication consistently with the NASEM definition?
  - `NA`: Respondent has not attempted a replication study or does not know if they have attempted a replication study.
  - `0`: Not consistent with NASEM definition. Possible reasons include internal replication (i.e. multiple tests within a research project/lab) or reproduction (i.e. using same data and methods to achieve identical results). These responses should be excluded from further analysis of researcher experience with replication studies.
  - `1`: Consistent with NASEM definition, or reports conducting a replication study but does not elaborate with qualitative information about the motivation for the replication study.
- `Flag_Reasoning`: Justification for the Rep_flag code
- `open-repeatable`: Is the motivation for the replication to assess how well the study conforms to open science principles? In other words, is the replicating researcher's motivation to answer the question: is the study documentation thorough enough to make a replication study possible?
  - `1`: Yes, emphasizes quality of research as reproducible or repeatable based on publication, code, data, etc.
- `validate-external`: Is the motivation for the replication to externally validate the claims of the original study by repeating the methods with new data in a new context? The underlying goal may be to evaluate the generalizability of the prior study claims or to check questionable results.
  - `1`: Yes, There is an emphasis on externally validating or generalizing with results from new contexts and studies, or an emphasis on comparing findings / conclusions based on results from new studies. Most definitions including new context and similar results will get this flag.
- `unique-space-time`: Is the motivation for the replication to assess the significance of unique geographic or temporal contexts? For example, is the researcher motivated by concerns about whether the methods and claims of a study will apply in a new place or time? Is the researcher concerned about geographic or historical differences?
  - `1`: Yes, there is recognition of challenges that spatial / temporal heterogeneity will cause for replication
- `validate-internal`: Is the motivation for the replication to assess the internal validity of the original study, e.g. by testing the original study's robustness or sensitivity to research design decisions? The underlying goal may be to check questionable results.
  - `1`: Yes, explains replication as a process internal to the research project by repeating and comparing samples/measurements/locations within a population/experiment/landscape, or explicitly references internal validation as a goal of replication through internal repetition, reanalysis, and/or assessment of construct validity.
- `epistemology`: Is the motivation for replication rooted in epistemological and ontological discussions of replication as fundamental to knowledge creation?
  - `1`: Yes, discusses replication in the context of knowledge creation or epistemology in a field of study.
- `learning`: Is the motivation for the replication to learn from the prior study? This motivation may be the first step for a researcher seeking to extend a study, to learn current techniques in a field of study, or for teaching or mentoring.
  - `1`: Yes
- `extension`: Is the motivation for the replication to extend a prior study with improvements in methodology or data?
  - `1`: Yes  
- `q19-jh`: Notes about coding by Joseph Holler

## Coding Notes

Many definitions are clearly aimed at validating prior findings or attempting to invalidate prior studies perceived to be flawed. However, it is not always clear whether this (in)validation is *internal* or *external*. Furthermore, validation is implicit in any motivations with nods to the epistemological function of replication in science. Therefore, the most reliable analysis of these codes might combine the information from internal validation, external validation, and epistemology. The particular codes may still be useful for additional context or for finding qualitative quotes.

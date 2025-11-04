---
title: "Emotion-Based Decision Feedback (Iowa Gambling Task)"
type: project
summary: "Feedback on use of affective signals in IGT; data collected with PsyToolkit."
---
Below is an **R script** that generates feedback reports based on how well students perform on an affective decision-making task under uncertainty — the **Iowa Gambling Task (IGT)**.  
The task is a classic measure used to test the **Somatic Marker Hypothesis** — the idea that emotional cues help guide our choices when outcomes are uncertain.

The version used here runs on a free, browser-based platform called **PsyToolkit** (see link below). It’s straightforward to set up, and there are plenty of tutorials online explaining how to build and run the experiment.  

When you download data from PsyToolkit, you’ll receive **one text file per participant**, containing data from all 100 trials (e.g., which deck they chose, how much they won or lost, and how long they took to decide).  

To use the feedback generator script below, you’ll first need to **combine all individual participant files into a single dataset** and then **reshape the data into long format** (one row per trial).  
This preprocessing step isn’t yet automated in the R code, but the rest of the workflow — generating per-student PDF reports — is fully handled by the script.

---
### Resources (hosted on OSF)

- 💻 **R script:** [igt_feedback.R](https://osf.io/6hevq)

- 📄 **Example feedback report (PDF):** [IGT_feedbackexample.pdf](https://osf.io/e4g29)

- 🌐 **Source task (PsyToolkit):** [https://www.psytoolkit.org/experiment-library/igt.html](https://www.psytoolkit.org/experiment-library/igt.html)

Example figure from the report:

![](/resources/figure2.png)

---

### References

Bechara, A., Damasio, A. R., Damasio, H., & Anderson, S. W. (1994). *Insensitivity to future consequences following damage to human prefrontal cortex.* *Cognition, 50*(1–3), 7–15. https://doi.org/10.1016/0010-0277(94)90018-3  

Stoet, G. (2017). *PsyToolkit: A novel web-based method for running online questionnaires and reaction-time experiments.* *Teaching of Psychology, 44*(1), 24–31. https://doi.org/10.1177/0098628316677643  

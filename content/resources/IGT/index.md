---
title: "Emotion-based decision feedback (Iowa Gambling Task)"
type: project
summary: "Feedback on use of affective signals in IGT; data collected with PsyToolkit."
---
Below is an R script that generates feedback reports based on how well students perform on an affective decision-making task under uncertainty — the Iowa Gambling Task (IGT).
The task is a classic measure used to test the somatic marker hypothesis — the idea that emotional cues help guide our choices when outcomes are uncertain.

The version I use runs on a free, browser-based platform called PsyToolkit (see link below). It’s easy to set up and there are plenty of tutorials online for how to build and run the experiment.
When you download data from PsyToolkit, you’ll get one text file per participant, with data from all 100 trials (e.g., which deck they chose, how much they won or lost, and how long they took to decide).
To use the feedback generator script below, you’ll first need to merge all these individual data files into one dataset and reshape it to long format. I haven’t automated that step yet in the R code — hopefully I’ll get around to it soon.

- ⬇️ <a href="/resources/igt_feedback.r" download>Download R script</a>

- 🌐 Source task in PsyToolkit: https://www.psytoolkit.org/experiment-library/igt.html

- 📄 [Example report (PDF)](/resources/IGT_feedbackexample.pdf)

- Example figure from the report: ![](/resources/figure2.png)


**References**  
Bechara, A., Damasio, A. R., Damasio, H., & Anderson, S. W. (1994). Insensitivity to future consequences following damage to human prefrontal cortex.* *Cognition, 50*(1–3), 7–15.  
Stoet, G. (2017). PsyToolkit: A novel web-based method for running online questionnaires and reaction-time experiments. *Teaching of Psychology, 44*(1), 24-31. https://doi.org/10.1177/0098628316677643 
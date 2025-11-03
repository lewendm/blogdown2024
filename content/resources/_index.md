---
title: "Teaching"
subtitle: "Courses and resources"
layout: single
show_title_as_headline: true
draft: false
---

Some resources/tools I have created or come across that I use in my teaching. Feel free to use and adapt.

### Courses / Topics
- Decision Making Processes in Organizations
- Quantitative Research Methods

### Materials

1. **Decision-making style feedback report (REI + CRT)**
   - This report is based on the **Rational–Experiential Inventory (REI)**, which gives scores on four styles (rational ability, rational engagement, experiential ability, experiential engagement).
   - It also includes the **Cognitive Reflection Test (CRT)** to show how often people override an intuitive but wrong answer.
   - The script produces a **radar/spider chart** so people can *see* how strong they are on each of the four REI dimensions, and then it drops them into one of four quadrants (e.g. high rational / low experiential).
   - 📄 Example report (PDF): [decisionmakingstyle_example.pdf](/resources/decisionmakingstyle_feedback_example.pdf)
   - 🧪 Example Qualtrics survey you can use to collect the data: [decisionmakingstyle_survey.qsf](/resources/decisionmakingstyle_qualtrics.qsf)
   - 👨‍💻 <a href="/resources/decisionmakingstyle_feedback.R" download>Download R code to generate decision-making style reports</a>

2. **Emotion-based decision feedback (Iowa Gambling Task performance)**
   - This one is about **how well people use affective / emotional signals in decision making**, using performance on the **Iowa Gambling Task (IGT)**.
   - It gives people feedback on whether they are picking up the reward–punishment structure over time (i.e. whether they’re using somatic markers efficiently).
   - I collected the data for this using **PsyToolkit** (https://www.psytoolkit.org/experiment-library/igt.html), but you could adapt it to other platforms.
   - 📄 Example report (PDF): [IGT_feedbackexample.pdf](/resources/IGT_feedbackexample.pdf)
   - 👨‍💻 <a href="/resources/IGT_feedback.R" download>Download R code for emotion & IGT feedback</a>


### Example figures

Below are example plots from the two reports, to can see what the output looks like.

![Example figure: decision-making style radar chart](/resources/figure1.png)

![Example figure: Iowa Gambling Task feedback](/resources/figure2.png)

### References
- Pacini, R. & Epstein, S. (1999). The relation of rational and experiential information processing styles to personality, basic beliefs, and the ratio-bias phenomenon. *Journal of Personality and Social Psychology, 76*(6), 972-987.  
- Bechara, A., Damasio, A. R., Damasio, H., & Anderson, S. W. (1994). Insensitivity to future consequences following damage to human prefrontal cortex. *Cognition, 50*(1-3), 7-15.  
- Stoet, G. (2017). PsyToolkit: A novel web-based method for running online questionnaires and reaction-time experiments. *Teaching of Psychology, 44*(1), 24-31. https://doi.org/10.1177/0098628316677643 


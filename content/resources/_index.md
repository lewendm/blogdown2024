---
title: "Teaching"
subtitle: "Courses and resources"
layout: single
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
   - 🧑‍💻 R script: [R code to generate decision-making style reports](/resources/decisionmakingstyle_feedback.r)

2. **Emotion-based decision feedback (Iowa Gambling Task performance)**
   - This one is about **how well people use affective / emotional signals in decision making**, using performance on the **Iowa Gambling Task (IGT)**.
   - It gives people feedback on whether they are picking up the reward–punishment structure over time (i.e. whether they’re using somatic markers efficiently).
   - I collected the data for this using **PsyToolkit** (https://www.psytoolkit.org/experiment-library/igt.html), but you could adapt it to other platforms.
   - 📄 Example report (PDF): [IGT_feedbackexample.pdf](/resources/IGT_feedbackexample.pdf)
   - 🧑‍💻 R script: [R code to generate emotion and decision-making style reports](/resources/IGT_feedback.r). Note: before running the code, you will have to compile all of the individual files from psytoolkit into a single datasheet and then transform the dataset from wide to long format.


### Example figures

Below are example plots from the two reports, so visitors can see what the output looks like.

![Example figure: decision-making style radar chart](/resources/figure1.png)

![Example figure: Iowa Gambling Task feedback](/resources/figure2.png)

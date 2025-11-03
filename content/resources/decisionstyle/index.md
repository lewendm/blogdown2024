---
title: "Decision-making style feedback report (REI + CRT)"
subtitle: "Rational–Experiential Inventory + Cognitive Reflection Test"
date: 2025-01-01
layout: single
draft: false
---
Below is an R script that generates individualized feedback reports based on students’ responses to two well-established measures of reasoning and decision-making style. You’ll also find the Qualtrics file used in my course, which the R code is built around; it can be imported directly into Qualtrics.

Students complete the Rational–Experiential Inventory (REI) (Pacini & Epstein, 1999), which measures general preferences for intuitive and analytical thinking, and the Cognitive Reflection Test (CRT) (Frederick, 2005), which captures the tendency to override an initial intuitive response through deliberation.

The feedback report summarizes students’ CRT responses, produces a radar (spider) chart of the four REI dimensions, and classifies individuals into quadrants that illustrate how intuitive and analytical styles interact.

- 📄 [Example report (PDF)](/resources/decisionmakingstyle_example.pdf)
- 🧪 [Qualtrics survey (.qsf)](/resources/decisionmakingstyle_survey.qsf)
- ‍💻 <a href="/resources/feedback-report.R" download>Download R script</a>
- Example figure from the report: ![](/resources/figure1.png)


**Reference**  

Pacini, R., & Epstein, S. (1999). The relation of rational and experiential information processing styles to personality, basic beliefs, and the ratio-bias phenomenon. *Journal of Personality and Social Psychology, 76*(6), 972–987.

Frederick, S. (2005). Cognitive reflection and decision making. *Journal of Economic Perspectives, 19*(4), 25–42.
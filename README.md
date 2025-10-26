**News Sentiment Visualisation**  
**Sentiment Analysis of News Headlines (2012–2022)**

**Overview**  
This repository offers a systematic and reproducible analysis of news sentiment from 2012–2022, integrating NLP, visualization design, and media theory.
It investigates how sentiment and topical focus evolved across time and categories—revealing shifts in media framing, public discourse, and collective sentiment. 

**Objectives**  
•	Analyze sentiment evolution in news headlines (2012–2022).  
•	Compare sentiment trends across major categories (e.g., politics, technology, business).  
•	Visualize topic prevalence and sentiment distribution over time.  
•	Evaluate media framing effects using accessible and aesthetic visualization techniques.  

**Dataset**  
Source: Misra News Category Dataset  
Size: ~200,000 headlines  
Period: 2012–2022  
Fields:  
•	headline — textual content of the news title  
•	category — assigned topic (e.g., “politics”, “tech”)  
•	date — publication date  
•	sentiment_score — numerical sentiment score (computed)  

**Methodology**  
Tools and Libraries  
•	Language: R  
•	Libraries: ggplot2, dplyr, tidyr, lubridate, yarrr, wesanderson  
•	Approach: NLP-based sentiment scoring + time-series and category-level visual analytics  

Framework: ASSERT  
•	Ask a Question – How does sentiment vary over time across categories?  
•	Search for Information – Load and preprocess Misra dataset.  
•	Structure the Data – Clean, categorize, and timestamp records.  
•	Envision the Answer – Select visualization types to highlight trends.  
•	Represent the Visualization – Implement using R and ggplot2.  
•	Tell a Story – Interpret visual findings in historical and social context.  

**Visualizations**  

<img width="468" height="269" alt="image" src="https://github.com/user-attachments/assets/8e964475-120b-4376-8a36-26352ab84f4d" />

Accessibility:  
•	Color-blind–friendly palettes (yarrr_mix, Wes Anderson)  
•	High-contrast labels and readable text  
•	Minimalist layouts to reduce visual clutter  
•	Future Enhancements:  
  o	Interactive visualizations (tooltips, filters)  
  o	Alt text and ARIA metadata for screen readers  
  o	Compliance with WCAG 2.1 (W3C, 2018)  

Alternatives:  
•	Heatmaps for word frequency comparison  
•	Stacked area charts for cumulative sentiment composition  

**Results & Insights**  
•	Temporal Shifts: Political and health categories peak during major global events.  
•	Emotional Framing: Negative sentiment dominates crisis periods (e.g., 2020–2021).  
•	Positive Narratives: Tech and environment categories show optimism growth post-2018.  
•	Media Reflexivity: Headlines mirror societal tensions and recovery cycles.  



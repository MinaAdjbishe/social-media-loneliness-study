# Social Media Use, Rumination, and Loneliness  
### A mediation analysis exploring cognitive mechanisms behind social media’s emotional impact

## Overview
This project investigates whether **rumination** mediates the relationship between **social media use** and **loneliness** in adults aged 18–50. Although social media is designed to connect people, research suggests it can also heighten negative emotions. This study tests whether repetitive negative thinking (rumination) explains why heavier social media use is associated with greater loneliness.

The project includes:
- Data cleaning and scoring of three validated psychological scales  
- Reliability checks (Cronbach’s alpha)  
- Descriptive statistics and correlations  
- A mediation model using SEM (lavaan)  
- Visualisations of distributions and age relationships  

## Key Findings
- Social media use **significantly predicted rumination**.  
- Rumination **significantly predicted loneliness**.  
- The **direct effect** of social media use on loneliness was **non‑significant**.  
- The **indirect effect** via rumination was **significant**, indicating **full mediation**.  

This suggests that **how people think while using social media** (rumination), rather than how much they use it, is the key mechanism influencing loneliness.

## Methods
### Measures
- **SMUIS** — Social Media Use Integration Scale  
- **RRS‑SF** — Ruminative Response Scale  
- **UCLA Loneliness Scale**  

### Analysis Steps
- Cleaned and renamed scale items  
- Reverse‑coded negatively keyed items  
- Calculated total scores  
- Ran reliability checks  
- Conducted mediation analysis using `lavaan`  
- Produced descriptive statistics and visualisations  

## Code
The full analysis is available in `analysis.R`, including:
- Data cleaning  
- Reliability analysis  
- Mediation model  
- Visualisations  

## Visualisations
Add your exported PNGs here:
- Distribution boxplots  
- Mean + SD bar charts  
- Scatterplots with regression lines  
- Mediation diagram (optional)

## Interpretation
The results support cognitive models of social media effects: emotional outcomes are shaped less by usage frequency and more by **maladaptive thought patterns** triggered during use. Interventions targeting rumination may be more effective than reducing screen time alone.

## Files
- `analysis.R` — full R script  
- `scales_data.csv` — scale responses (if allowed)  
- `demo_data.csv` — demographics (if allowed)  
- `plots/` — exported visualisations  

## Author
Mina Adjbishe  
BSc Psychology with Counselling  

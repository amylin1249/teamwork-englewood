# Addressing Disproportionate Gun Possession Charges Among Young Black Men in Englewood

## Members
- Amy Lin (Project Manager)
- Anya Gizis
- Cherin Lee
- Elie Nowlis
- Manas Subbaraman


## Abstract
Teamwork Englewood is a local Chicago nonprofit that unites the many organizations serving Englewood residents to build a stronger community. They bridge the gap between release from incarceration and  successful reintegration for justice-impacted individuals by providing holistic services and assisting participants with finding employers who hire people with current or previous involvement in the criminal justice system. Given this mission, the organization identified a critical need for data analysis identifying longitudinal trends among Black men ages 18-40 in Englewood with gun possession charges. 

The motivation for this project is driven by four recent, major policy shifts in Illinois: the SAFE-T Act, which eliminated cash bail in 2023; the implementation of Expedited Felony Review in 2025, which allows officers to directly file certain nonviolent felony gun possession offenses without obtaining approval from the Cook County State's Attorney's Office; the enactment of SB 1899 in 2026, which allows eligible individuals to participate in a new diversion program that includes application for a Firearm Owner’s Identification (FOID) card and dismissal of their charges; and the shifting eligibility of gun possession cases in Restorative Justice Community Courts in 2025 and 2026. This project establishes historical baselines and, for datasets that span these transitions, evaluates the initial impacts of these policy changes.

To support these goals, I architected an analytical pipeline in R to synthesize disparate public data sources from the City of Chicago and Cook County. The analysis focuses on demographic trends to surface patterns in arrests, felony review, and bond outcomes. By translating these complex administrative records into a Data Brief and Policy Memorandum, our team provided Teamwork Englewood with the empirical evidence needed to engage in high-level conversations with the State’s Attorney’s Office and lead data-driven community education efforts.


## Final Deliverables
- [Data Brief](./deliverables/data_brief.pdf)
- [Policy Memorandum](./deliverables/policy_memorandum.pdf)


## Setup and Execution
1. Clone our repository by running `git clone git@github.com:amylin1249/teamwork-englewood.git` in your terminal.
2. After cloning the repository, open the `teamwork-englewood.Rproj` file in RStudio to ensure `here::here()` paths resolve to the project root.
3. In the R console, run `install.packages(c("tidyverse", "lubridate", "scales", "here", "slider"))` to install the required packages.
4. Execute the scripts located in the `analysis_scripts/` directory to regenerate the visualizations, which will appear in `figures`.


## Data Sources
### Data Source #1: City of Chicago Data Portal
#### Data Source #1.1: Arrests Dataset
- https://data.cityofchicago.org/Public-Safety/Arrests/dpt3-jri9/about_data
#### Data Source #1.2: Crimes – 2001 to Present Dataset
- https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2/about_data
### Data Source #2: Cook County Government Open Data
#### Data Source #2.1: Initiation Dataset
- https://datacatalog.cookcountyil.gov/Legal-Judicial/Initiation/7mck-ehwz/about_data
#### Data Source #2.2: Diversion Dataset
- https://datacatalog.cookcountyil.gov/Legal-Judicial/Diversion/gpu3-5dfh/about_data


## Acknowledgments
Many thanks to Teamwork Englewood for their partnership and for the opportunity to support their advocacy efforts through this analysis.

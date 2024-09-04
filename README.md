# AI Audit Tool Database Analysis
Authors: Victor Ojewale, Ryan Steed, Briana Vecchione, Abeba Birhane, Deb Raji

Developers: Ryan Steed, Victor Ojewale

This tool powers the landscape visualization and data analysis for the Open Source Audit Tooling project, including results in our paper, "[Towards AI Accountability Infrastructure: Gaps and Opportunities in AI Audit Tooling](https://rbsteed.com/papers/accountability-infrastructure)."

## Installation
`make venv`

## Analysis & Replication
All code used to analyze our database and produce the results in our paper can be found in `analysis.Rmd`, drawing on code from `utils.R`. All analysis requires a cleaned and pre-processed version of our Airtable database, produced using the instructions below. `data/airtable.csv` contains the most recent copy of our Airtable database.

### Generating data for analysis
1. [Optional] Download most recent [Airtable](https://airtable.com/shrJZqqWyXepTyVFG) CSV -> `data/airtable.csv`.
2. [Optional] Obtain access to Crunchbase and Github data using the instructions below.  If you do not have access to Crunchbase data or a Github personal access token, you can skip this data with the options `--no-crunchbase` or `--no-github`. You will also need to skip code chunks requiring variables beginning with `gh_` or `cb_` in `analysis.Rmd`.
3. Run `python clean.py pivot data/airtable.csv`. This endpoint explodes the `Taxonomy` field into three levels, cleans the data, and joins in Github and Crunchbase data. Cleaned output is stored in `output/airtable_for_pivot.csv`.

### Github data
Add your Github personal access token to `secrets.json`, with permission to read public repositories. The file should look like this:
```json
{
    "github_token": "YOUR_TOKEN"
}
```
`clean.py pivot` will scrape from Github using this token.

### [Crunchbase](https://crunchbase.com) data
Crunchbase does not allow us to redistribute the data we used for our analysis. To obtain a copy of your own, request access to the [Crunchbase Research Access](https://support.crunchbase.com/hc/en-us/articles/360041692693-How-to-Request-Access-to-Crunchbase-s-Academic-Research-Access-Program) program or buy a subscription to Crunchbase Pro.

Note that you may need to change variable names in `analysis.Rmd` if the Crunchbase schema has changed.

#### Crunchbase Pro (Recommended)
1. Create a file of organization names and domains needed, based on `data/airtable.csv` (e.g., `crunchbase/cb_query.csv`).
2. Use Crunchbase's import function to create a List of those organizations.
3. Download all available columns to `crunchbase`, creating separate files (`crunchbase/cb-query_*.csv`) for `companies`, `investors`, and `schools`.

#### Research Access (does not include some variables---e.g. revenue data)
With research access, download the [Daily CSV Export](https://data.crunchbase.com/docs/daily-csv-export). Using sqlite3, import `organizations.csv`:
```sqlite
.mode csv
.import PATH_TO_CSV organizations
.save organizations.db
```
Then, use the flag `--from-sql` in calls to `clean.py pivot`.

## Landscape Visualization
To generate YAML for the [landscape visualization](tools.auditing-ai.com),
1. [Optional] Download most recent [Airtable](https://airtable.com/shrJZqqWyXepTyVFG) CSV -> `data/airtable.csv`.
2. Run `python clean.py yaml data/airtable.csv ../landscape.yml`.

For Mac, you may need to additionally install `graphviz`:
```bash
brew install graphviz
pip install --global-option=build_ext --global-option="-I/usr/local/Cellar/graphviz/8.0.5/include/"  --global-option="-L/usr/local/Cellar/graphviz/8.0.5/lib/" pygraphviz
```

## Contents
- `clean.py`: Script for cleaning and joining data from Airtable, Github, and Crunchbase.
    - `clean.py pivot`: Clean and join data for analysis.
    - `clean.py yaml`: Clean and join data used to generate landscape visualization.
- `analysis.Rmd`: R Markdown file for generating plots and results used in our paper.
- `utils.R`: R utility functions for generating plots and results used in our paper.
- `data/`
    - `airtable.csv`: Most recent copy of our Airtable database.
    - `taxonomy.json`: JSON copy of our taxonomy tree.
- `output/`
    - `airtable_for_pivot.csv`: Cleaned and joined data for `analysis.Rmd`.
    - `landscape.yml`: YAML for landscape visualization.
- `crunchbase/`: Directory for storing Crunchbase data and (optionally) Crunchbase query file.

Africa CDC Publications Tracker (Shiny)
A Shiny web app that tracks and visualizes PubMed publications affiliated with Africa CDC.
It fetches data from PubMed (via rentrez), caches in memory for the session, and provides:

A Dashboard with KPIs and charts

An Author Explorer with clean author selection, trends, and top journals

A Publications Table with export options

Non-blocking refresh (background fetch) + skeleton first load + overlay spinner

Counts inside bars only (no duplicate labels) with clean hover tooltips

Accurate unique author counts (case-insensitive, filtered for “Author/Authors” noise)

✨ Features
Fast UX

Background refresh using promises + future::multisession

First-load skeleton UI (shimmer placeholders) so the page never looks empty

Overlay spinner card during manual refresh (but the UI stays usable)

Clean visuals

Counts shown inside bars (Plotly), no duplicated numbers on hover

Dynamic left margin + dynamic height so long author names fit

Consistent Africa CDC color palette and polished CSS

Smarter author handling

Removes stray “Author/Authors” tokens

Case-insensitive unique counts (e.g., “Jane Doe” == “jane doe”)

PubMed batching

Robust fetch with use_history = TRUE and pagination

Query:

css
Copy
Edit
(Africa Centres for Disease Control[Affiliation] AND Prevention[Affiliation]) OR (Africa CDC[Affiliation])
🧱 Tech stack
R / Shiny

Data wrangling: dplyr, stringr, lubridate

Charts: plotly

Tables: DT

PubMed API: rentrez

UX helpers: shinyjs

Async: promises, future (multisession)

📁 Project structure
bash
Copy
Edit
africa-cdc-pubs-tracker/
├─ app.R                    # the Shiny app (UI + server)
├─ www/
│  └─ acdc_logo.png         # static assets (images, css, ...)
├─ README.md                # this file
├─ .gitignore
├─ renv.lock                # (optional) locked package versions
├─ runtime.txt              # (optional) pin R version, e.g. r-4.3.2
└─ .github/workflows/
   └─ deploy-shinyapps.yml  # (optional) CI/CD to shinyapps.io
Place all static assets (e.g., acdc_logo.png) in www/. In HTML, you can still reference it as src="acdc_logo.png" because Shiny serves /www at the web root.

🚀 Run locally
1) Install R packages
If you use renv (recommended):

r
Copy
Edit
install.packages("renv")
renv::restore()            # installs from renv.lock
Without renv:

r
Copy
Edit
install.packages(c(
  "shiny","DT","dplyr","stringr","lubridate","plotly",
  "rentrez","htmltools","shinyjs","promises","future"
))
2) Start the app
r
Copy
Edit
shiny::runApp()
The app will open in your browser.
On first load, you’ll see skeleton placeholders; data renders as soon as PubMed results arrive.

⚙️ Configuration
Background fetching
The app runs PubMed fetches in a background R session:

r
Copy
Edit
future::plan(multisession)            # app.R
# On shinyapps.io you may prefer:
# future::plan(multisession, workers = 1)
PubMed limits / optional API key
The app works without an API key.

For heavier use, you can set an NCBI API key (faster + higher rate limits):

r
Copy
Edit
Sys.setenv(ENTREZ_KEY = "<your-ncbi-api-key>")
# or at runtime:
rentrez::set_entrez_key(Sys.getenv("ENTREZ_KEY"))
Get one from NCBI account settings. Not required for normal usage.

Change batch size / retmax
Inside fetch_africacdc_pubs():

r
Copy
Edit
fetch_africacdc_pubs(retmax = 5000, batch_size = 200)
Tweak if you need fewer/more records per refresh.

📊 How to use
Dashboard

KPIs: total publications, unique authors, journals

Publications over time (with counts inside bars and a separate trend line)

Top journals (bars with counts inside)

Top authors (bars with counts inside; dynamic left margin for long names)

Author Explorer

Select an author via typeahead

See their yearly trend and top journals (counts inside bars)

Their publications list with PubMed links

Publications Table

Full table with copy/CSV/Excel/Print actions

Title and IDs link to PubMed; DOI links to publisher

☁️ Deploy to shinyapps.io (manual)
From R:

r
Copy
Edit
install.packages("rsconnect")
rsconnect::setAccountInfo(
  name   = "YOUR_ACCOUNT",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)
rsconnect::deployApp(".")
Get your token/secret at: shinyapps.io → Account → Tokens.

🤖 CI/CD: Auto-deploy from GitHub (optional)
Create shinyapps.io token/secret
shinyapps.io → Account → Tokens → Add Token → Show Secret.

Add repo secrets on GitHub → Settings → Secrets and variables → Actions:

SHINYAPPS_ACCOUNT

SHINYAPPS_TOKEN

SHINYAPPS_SECRET

(optional) SHINYAPPS_APPNAME

Add workflow at .github/workflows/deploy-shinyapps.yml:

yaml
Copy
Edit
name: Deploy to shinyapps.io

on:
  push:
    branches: [ "main" ]
  workflow_dispatch:

jobs:
  deploy:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2
        with:
          use-public-rspm: true

      - uses: r-lib/actions/setup-pandoc@v2

      - name: Cache R packages
        uses: actions/cache@v4
        with:
          path: ${{ env.R_LIBS_USER }}
          key: ${{ runner.os }}-r-${{ hashFiles('**/renv.lock') }}
          restore-keys: ${{ runner.os }}-r-

      - name: Install system dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y libcurl4-openssl-dev libxml2-dev libssl-dev

      - name: Install R packages
        run: |
          Rscript -e 'if (file.exists("renv.lock")) { install.packages("renv"); renv::restore() } else { install.packages(c("rsconnect","shiny","plotly","dplyr","lubridate","DT","rentrez","shinyjs","stringr","htmltools","promises","future")) }'

      - name: Deploy to shinyapps.io
        env:
          SHINYAPPS_ACCOUNT: ${{ secrets.SHINYAPPS_ACCOUNT }}
          SHINYAPPS_TOKEN:   ${{ secrets.SHINYAPPS_TOKEN }}
          SHINYAPPS_SECRET:  ${{ secrets.SHINYAPPS_SECRET }}
          SHINYAPPS_APPNAME: ${{ secrets.SHINYAPPS_APPNAME }}
        run: |
          Rscript -e 'install.packages("rsconnect")'
          Rscript -e 'rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPPS_ACCOUNT"), token=Sys.getenv("SHINYAPPS_TOKEN"), secret=Sys.getenv("SHINYAPPS_SECRET"))'
          Rscript -e 'appname <- Sys.getenv("SHINYAPPS_APPNAME"); if(nzchar(appname)) rsconnect::deployApp(appDir=".", appName=appname, forceUpdate=TRUE) else rsconnect::deployApp(appDir=".", forceUpdate=TRUE)'
Push to main → GitHub Actions builds and deploys automatically.

If your app lives in a subfolder, change appDir="." to that folder.

🧪 Troubleshooting
No data shows initially
First load fetches in background; you’ll see skeletons briefly. If it fails, a notification appears. Click Refresh PubMed Data to retry.

NCBI rate-limiting
Heavy use can trigger limits. Consider setting ENTREZ_KEY (see above) or reducing retmax.

Plotly warnings about insidetextanchor
We apply text/label options only to bar traces; scatter traces use hovertemplate only to avoid warnings.

Long author names get cut
The Top Authors plot dynamically adjusts left margin and plot height based on label length and author count. If labels still clip, increase the max margin multiplier in app.R.

Static files not loading
Ensure images are in /www. Shiny serves them at root; src="acdc_logo.png" is correct when the file lives at www/acdc_logo.png.

# Africa CDC Publications Tracker (Shiny)

A Shiny web app that fetches and visualizes **Africa CDC–affiliated** publications from PubMed. It features a polished UI, background (non-blocking) refresh, skeleton placeholders on first load, and clean charts with counts shown **inside** bars only.

---

## Table of Contents

- [Features](#features)
- [Screens](#screens)
- [Tech Stack](#tech-stack)
- [Folder Structure](#folder-structure)
- [Requirements](#requirements)
- [Quick Start (Local)](#quick-start-local)
- [Configuration](#configuration)
  - [NCBI API Key (optional)](#ncbi-api-key-optional)
  - [Batch Size / Limits](#batch-size--limits)
- [Deploy to shinyapps.io (Manual)](#deploy-to-shinyappsio-manual)
- [CI/CD: Auto-deploy from GitHub (Optional)](#cicd-auto-deploy-from-github-optional)
  - [1) Create shinyapps.io Token/Secret](#1-create-shinyappsio-tokensecret)
  - [2) Add GitHub Repo Secrets](#2-add-github-repo-secrets)
  - [3) Add GitHub Actions Workflow](#3-add-github-actions-workflow)
- [Troubleshooting](#troubleshooting)
- [Repo Checklist](#repo-checklist)
- [License](#license)
- [Acknowledgements](#acknowledgements)

---

## Features

- **Fast UX & Resilience**
  - Background refresh using `promises` + `future::multisession` (UI never blocks)
  - First-load **skeleton placeholders** so the page never looks empty
  - Overlay **spinner card** during manual refresh (site remains usable)

- **Clean Visuals**
  - Counts displayed **inside bars only**, with hover tooltips (no duplicated labels)
  - Adaptive left margin & height so **long author names** fit on the y-axis
  - Consistent **Africa CDC** color palette and polished CSS

- **Better Author Handling**
  - Strips stray “Author/Authors” tokens
  - Case-insensitive de-duplication for unique author counts

- **Robust PubMed Fetching**
  - Uses `rentrez` with `use_history = TRUE` and batching
  - PubMed query:

    ```text
    (Africa Centres for Disease Control[Affiliation] AND Prevention[Affiliation]) OR (Africa CDC[Affiliation])
    ```

---

## Screens

- **Dashboard**: KPIs, publications over time (bars + trend line), top journals, top authors
- **Author Explorer**: author picker, yearly trend, top journals, and a publications table
- **Publications Table**: searchable/exportable DT table with PubMed/DOI links

---

## Tech Stack

- R / **Shiny**
- **dplyr**, **stringr**, **lubridate**
- **plotly** (charts), **DT** (tables)
- **rentrez** (PubMed API)
- **shinyjs** (UI helpers)
- **promises** + **future** (async background work)

---

## Folder Structure

```text
africa-cdc-pubs-tracker/
├─ app.R
├─ www/
│  └─ acdc_logo.png           # static assets (images, css, ...)
├─ .gitignore
├─ README.md
├─ renv.lock                  # (optional) lock package versions
├─ runtime.txt                # (optional) pin R version, e.g., r-4.3.2
└─ .github/workflows/
   └─ deploy-shinyapps.yml    # (optional) CI/CD to shinyapps.io
```

> Put static assets in `www/`. In HTML, reference as `src="acdc_logo.png"` (Shiny serves `www/` at the web root).

---

## Requirements

- R 4.2+ (recommended 4.3+)
- Packages:
  - `shiny`, `DT`, `dplyr`, `stringr`, `lubridate`, `plotly`,
  - `rentrez`, `htmltools`, `shinyjs`, `promises`, `future`
- (Optional) **renv** for reproducible package versions

---

## Quick Start (Local)

### 1) Install packages

**With renv (recommended):**
```r
install.packages("renv")
renv::restore()    # installs from renv.lock
```

**Without renv:**
```r
install.packages(c(
  "shiny","DT","dplyr","stringr","lubridate","plotly",
  "rentrez","htmltools","shinyjs","promises","future"
))
```

### 2) Run the app
```r
shiny::runApp()
```

- On first load you’ll see skeleton placeholders; content appears as soon as PubMed data is ready.
- Manual refresh uses a non-blocking background request (the UI keeps working).

---

## Configuration

### NCBI API Key (optional)

The app works without an API key. For heavier use (faster & higher limits):

```r
Sys.setenv(ENTREZ_KEY = "<your-ncbi-api-key>")
rentrez::set_entrez_key(Sys.getenv("ENTREZ_KEY"))
```

Create an NCBI API key in your NCBI account settings.

### Batch Size / Limits

Inside `app.R` (fetch function):

```r
fetch_africacdc_pubs(retmax = 5000, batch_size = 200)
```

- Reduce `retmax` if you want faster, smaller refreshes.
- Increase `batch_size` for fewer API calls (be mindful of limits).

---

## Deploy to shinyapps.io (Manual)

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(
  name   = "YOUR_ACCOUNT",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)
rsconnect::deployApp(".")
```

- Get the token/secret at shinyapps.io → **Account → Tokens**.
- For background work on shinyapps.io, a conservative plan is:
  ```r
  future::plan(multisession, workers = 1)
  ```

---

## CI/CD: Auto-deploy from GitHub (Optional)

### 1) Create shinyapps.io Token/Secret
- shinyapps.io → **Account → Tokens → Add Token → Show Secret**  
  Note your **Account name**, **Token**, and **Secret**.

### 2) Add GitHub Repo Secrets
Repo → **Settings → Secrets and variables → Actions → New repository secret**:

- `SHINYAPPS_ACCOUNT` = your shinyapps.io account name  
- `SHINYAPPS_TOKEN`   = the token  
- `SHINYAPPS_SECRET`  = the secret  
- *(optional)* `SHINYAPPS_APPNAME` = desired app name in shinyapps.io

### 3) Add GitHub Actions Workflow
Create `.github/workflows/deploy-shinyapps.yml`:

```yaml
name: Deploy to shinyapps.io

on:
  push:
    branches: [ "main" ]
  workflow_dispatch:

jobs:
  deploy:
    runs-on: ubuntu-latest

    steps:
      - name: Checkout repo
        uses: actions/checkout@v4

      - name: Set up R
        uses: r-lib/actions/setup-r@v2
        with:
          use-public-rspm: true

      - name: Set up Pandoc
        uses: r-lib/actions/setup-pandoc@v2

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
```

- If your app is in a subfolder, change `appDir="."` to that folder.
- Push to `main` → Action runs → app deploys/updates on shinyapps.io automatically.

---

## Troubleshooting

- **Nothing appears at first load**  
  The app shows skeleton placeholders while data loads. If fetching fails, a toast notification appears—use **Refresh PubMed Data** to retry.

- **Rate limiting / slow refresh**  
  Consider setting an NCBI API key, lowering `retmax`, or increasing `batch_size` cautiously.

- **Plotly warnings**  
  We apply label options only to **bar** traces; the trend line is a **scatter** trace with `hovertemplate` only to avoid duplicated labels and warnings.

- **Author labels clipped**  
  The “Top Authors” chart auto-sizes margin/height based on label length and count. Increase the scaling factor in `app.R` if needed.

- **Static files not found**  
  Ensure assets are in `/www`. Refer to them like `src="acdc_logo.png"` (served from the root).

---

## Repo Checklist

- [ ] `app.R` added  
- [ ] `www/` contains `acdc_logo.png` (and any other assets)  
- [ ] `.gitignore` present  
- [ ] *(optional)* `renv.lock` committed  
- [ ] *(optional)* `runtime.txt` with desired R version  
- [ ] *(optional)* `.github/workflows/deploy-shinyapps.yml` for CI/CD  
- [ ] README.md (this file)

---

## License

MIT (change if you prefer). To formalize, add a `LICENSE` file.

---

## Acknowledgements

- Data: **NCBI E-utilities** via **rentrez**  
- Charts: **plotly** • Tables: **DT**  
- Async: **promises** + **future**  
- UI helpers: **shinyjs**

---

*Questions or improvements? Open an issue or PR!*

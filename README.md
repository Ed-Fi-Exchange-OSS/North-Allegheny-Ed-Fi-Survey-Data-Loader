# North Allegheny Ed-Fi Survey Data Loader

The Survey Data Loader tool is a lightweight web application enabling end users to easily map and upload survey data flat files to an Ed-Fi ODS. Supported use cases include any surveys administered at the district, school, or classroom level that leverage Google Forms, Microsoft forms, Typeform, or other survey platforms that output response results in a standard, tidy (each row represents one survey response; each column represents one item response or survey response property (e.g. completion time)) spreadsheet format. 

Within the application, users load survey metadata and question metadata to their Ed-Fi ODS before uploading survey responses. Surveys can be loaded without manual field mapping or bootstrap configuration outside of simple user prompts within the UI. An admin module allows data/IT admins to pre-configure secure ODS connections, enabling business users to engage with the application without deep technical knowledge or familiarity with the Ed-Fi data model.

A preview of the application can be accessed here (https://r.northallegheny.org/surveyLoaderDemo/). Preview users are welcome to try uploading their own sample data to their own sample ODS (e.g. Populated Template/Grand Bend dataset); for obvious reasons, do not send actual student data through the demo application.

## Deployment Instructions

### Docker Deployment
**Users with a Docker installation can deploy a local instance of the application by running the following statements at the command line** (note step #3 may take on the order of 15-40 minutes to run on first execution): 
1. *git clone https://github.com/Ed-Fi-Exchange-OSS/North-Allegheny-Ed-Fi-Survey-Data-Loader*
2. *cd North-Allegheny-Ed-Fi-Survey-Data-Loader*
3. *docker build -t edfi-survey-loader .*
4. *docker run -dp 3000:80 edfi-survey-loader*

### Manual Deployment
Alternatively, the instructions below are for deploying an isolated instance of the application interactively on a Windows OS. For users with an active RStudio Connect license or Shiny Server instance, follow your team's usual procedure for publishing an application to the server once local deployment has succeeded.

### Pre-Requisites
- Ed-Fi Operational Data Store (> v3.3–sufficiently recent that the survey domain is included)
- Computer/Server with
    - Existing R installation*, >=v3.1.3
    - RStudio installation*
    - RTools installation*
    - Git*
    - renv R package installed ( install.packages(“renv”) )
- Survey data flat file - such as any generic results export from Google Forms, Microsoft Forms, Typeform, etc.

*_May require local admin rights on your machine/server._

### Deployment Steps
1. Create a fresh R project in RStudio, and select “Version Control” as the project location type, then select “Git”:
2. Enter https://github.com/Ed-Fi-Exchange-OSS/North-Allegheny-Ed-Fi-Survey-Data-Loader.git as the repository URL
3. (If not already done) Install “renv” package: install.packages(“renv”)
4. Run the command renv::restore()
  - Choose Y/Yes for the package restore prompts
  - Go get some coffee or a snack–package building could take a while…
5. Open the app.R file within the “EdFi-Survey-Loader” folder in the project directory and run the application (in a browser window).


## User Instructions
A step-by-step guide for application users is built into the app interface with the "How to Use This Page" button. Users seeking an app overview without having to deploy their own version can __[follow along with the user tutorial video](https://www.youtube.com/watch?v=pmdO5QVGg4o)__ and/or visit a demo version of the app, hosted [here](https://r.northallegheny.org/surveyLoaderDemo/).

## Legal Information

Copyright (c) 2021 Ed-Fi Alliance, LLC and contributors.

Licensed under the [Apache License, Version 2.0](LICENSE) (the "License").

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied. See the License for the
specific language governing permissions and limitations under the License.

See [NOTICES](NOTICES.md) for additional copyright and license notifications.

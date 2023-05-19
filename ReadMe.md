---
title: "ReadMe"
author: "Vatsal"
date: "2/20/2023"
output: html_document
---
  
# Assignment
  
  This dashboard has been created by Vatsal Garg as an assignment where data is upload by the user (accepts `csv` file only). This dashboard is not intended to be used for marketing purposes and should be considered only as a medium for showcasing skill set on R Shiny and related technologies. Different services used in this dashboard include shinydashboard, highcharter, shinyjs and more.

To know more about the assignment, please refer [this](00_Quantzig_RShiny_InterviewCodingProject.pdf)

## How to use

This app follows the following structure:
  
  * The main folder consist of app.R which can be used to build the application
* The modules are stored in modules folder
* One additional file `global.R` is used to add user defined functions.

## Architechture

* This app is divided into 2 modules: Dashboard and About.
* Each module is composed of two functions 1) a piece of UI, and 2) a fragment of server logic that uses that UI – similar to the way that Shiny apps are split into UI and server logic.
* The function body of ui starts with the statement `ns <- NS(id)`. It takes the string `id` and creates a namespace function.
* All input and output IDs that appear in the function body are wrapped in a call to `ns()`.
* The ui structure follows the `fluidPage()`, `fluidRow()`, `Column()` and more for formatting.
* The server logic is encapsulated in a single function for every module called as module server function.
* Every server function has a call to `moduleServer()`, to which two things are passed. One is the `id`, and the second is the module function.

## Scope and Tests

* In Scope test cases used:-
  + Whether the uploaded file is in *.CSV* format or not.
  + Factors are treated as characters.
* The tests includes the unit test cases for functions and the server functions tests.

## Optimizations

* The use of `dplyr` is minimized and replaced with base R wherever possible.
* All the plots have been cached (app level) with the required set of inputs.
* Use of `conditionPanel` can be seen in the app along with `insertUI` and `removeUI` functions.
* Faster execution for user defined function with help of caching use of `memoise` and `bindCache`.

## User experience

* The app has been created with shinydashboard and *purple* skin.
* The elements follows shiny layouts so as to avoid any overflows.
* `highcharter` package is used to create the plots as it is more optimized and can be modified futher.
* Implementation of Modal for cleaner and focused experience by showing all the relevant details for uploaded data.
* Alert modal prompt is provided in case user does not upload a *.CSV* file.

## Selling Points

* Minimalistic modern user interface.
* Very easy to use and responsive to inputs.
* Wide variety of components are used giving user a streamline approach for the process.
* Faster execution for user defined function with help of caching use of `meimose` and `bindCache`.
* Code is standardized to make it more usable.
* Modal for confirming the data upload showing all the relevant details of data uploaded to user.
* Followed `app.R` approach for simple execution of tab.
* Disclaimer included in About tab to avoid any legal issues.

## Infrastucture

* This app can be accessed via shinyapps.io server using [Dashboard Link](https://voidvatsal.shinyapps.io/AssignmentQuantZig/)
* All the dependencies are available in `renv`.
* Packages and there version used:-
 1. markdown       -1.4   
 2. highcharter    -0.9.4  
 3. memoise        -2.0.1  
 4. tidyr          -1.2.0  
 5. shinyjs        -2.1.0  
 6. DT             -0.26   
 7. dplyr          -1.0.9  
 8. shinydashboard -0.7.2  
 9. shinyWidgets   -0.7.0  
10. shiny          -1.7.3  
11. stats          -4.1.3  
12. graphics       -4.1.3  
13. grDevices      -4.1.3  
14. utils          -4.1.3  
15. datasets       -4.1.3  
16. methods        -4.1.3  
17. base           -4.1.3

## Please note

* This app tries to implement multiple functionalities. Therefore, it is possible the same thing has been done using different techniques. This has been done intentionally as a proof of concept. For example, In the dashboard tab, the filters are created separately from graphs used inside `insertUI`, whereas Graph have there reserved UI with `conditionalPanel`. 

Made by Vatsal Garg

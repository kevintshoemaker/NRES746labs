# NRES746labs

This package contains no functions and is only intended to allow students to access interactive versions of the lab exerciises for NRES 746 (Advanced Analysis Methods for NRES) at University of Nevada, Reno.

## Interactive labs

To access the interactive labs, use the following steps:

### Step 1: open R/Rstudio

### Step 1.1: install 'devtools' (first time only)

To do this, use the "Install" button on the "Packages" tab in Rstudio, or use the following command:

```

install.packages("devtools")

```

NOTE: you only have to do this once! That is, once you've installed a package on your machine it will always be there!  

### Step 2: install the "NRES746labs" package from GitHub

Ultimately I may load this package to CRAN, but for now it lives on GitHub (since I'll be updating it a lot!). You can use the following code to install the package from GitHub:

```

devtools::install_github("kevintshoemaker/NRES746labs")

```

NOTE: this can take a while the first time, and lots of messages will pop up. If you're asked to choose packages to update, I don't think it matters what option you choose.

NOTE: you should re-install the package for every lab, since I'll be updating frequently. For example, the package currently only includes lab 1 (I haven't made interactive versions of lab 2-5 yet).

### Step 3: run the lab!

Now you have everything you need to run the lab. Use the following code:

```

library(NRES746labs)
learnr::run_tutorial("lab1","NRES746labs")

```

NOTE: the system doesn't seem to work for Microsoft Edge, so make sure your default browser is Chrome or Safari. 

NOTE: the shiny app will "use up" your R session, so if you want to work on your own script at the same time as you're running the app, you'll have to open up a second R session. 


Hope this works! Good luck!
 


# GOOGLE TRENDS README

This is the README file for my project. It provides information about the project, its purpose, how to use it, and any other relevant details.

## Getting Started

To get started with this project, follow these steps:

1. Clone the repository.
```R
https://github.com/Mamba1099/google-trends-R.git
```
2.Activate virtual environment
```R
cd renv
```

```R
Rscript activate.R
```

3.move out of that directory after successfully activating renv

```R
cd ..
```

## install Dependencies by running the scripts with the required packages
This project requires the following R packages:

## open R in terminal
 install required packages

```R
Rscript install_package.R
```

load the packages into your R studio

```R
Rscript load_packages.R
```

## install dependencies using virtual environment
ensure that you do this in your work directory
1. Launch R in your terminal by typing R in your terminal
2. load the renv package
```R
library(renv)
```
3.initialize the environment in your work directory
```R
renv::init()
```
4.activate your environment
```R
renv::activate()
```
5. renv::install() this will update and install the required packages in the renv.lock
```R
renv::install()
```
or
```R
renv::snapshot()
```
6.Load the package
```R
renv::load()
```

## run the app
ensure that your are in the path
cd/path/to/your_project.
the path will depend on the you have named your project or the directory you are working on.
it's not supposed to be same as mine.
in order to run the project, run the following commands in your terminal
```R
Rscript app.R
```
follow the link to view the app
```R
http://127.0.0.1:6152
```
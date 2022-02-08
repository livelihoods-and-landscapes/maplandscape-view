## Setup Config

```
cd app
cp config-example.yml config.yml
```

Configure config.yml to connect to cloud storage. 

## Docker

*maplandscape-view* is built using the [rocker/r-ver:4.1.2](https://github.com/rocker-org/rocker-versioned2/blob/master/dockerfiles/shiny_4.1.2.Dockerfile) docker base image. This is R version 4.1.2 based on Ubuntu LTS 20.04. 

The script `system-dependencies.R` in the directory's root will generate a list of the R packages the app depends on via a call to `renv::dependencies()` and the Ubuntu system libraries the app depends on via a call to `remotes::system_requirements()`. This information is used to construct the `Dockerfile`. 




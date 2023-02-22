# accClusters: Analysis of Road Accident Clusters

## What the project does

accClusters does two things:

1.  It prepares data on traffic accidents in the Czech Republic for visualization in a [web application](https://github.com/s-mikula/trafficacc).
2.  It finds dangerous spots on Czech roads, i.e., clusters of roads accidents in the Czech Republic.

The project is supported by the grant CK01000049 funded by [Technology Agency of the Czech Republic](https://www.tacr.cz/en/).

## Why the project is useful

We hope this project will help the Police to improve her resource allocation, and hence reduce the number of serious traffic accidents.

## How users can get started with the project

The easiest way to run the project is build a docker image and use it. For this, use

```{bash}
make docker
```

You can learn what data you need and how to use the software in the user manual (only in Czech). The manual is placed in the `man` folder. You can build it with

```{bash}
make man
```

Alternatively, you can run the system on your local computer. For this, you need

-   [R](https://cran.r-project.org/) (system for statistical computation and graphics)

-   necessary packages: in R, install `renv` and run `renv::restore()`

    -   on Linux, you may need a lot of Linux libraries; you can grab their list in the Docker file if you use Ubuntu

-   necessary data (see the user manual)

-   run `update_data.sh` bash script; you would have to translate it yourself to Powershell if you use Windows

## Where users can get help with your project

If you are interested in the project, let me know please.

## Who maintains and contributes to the project

-   Michal Kvasnička, [Masaryk University](https://www.muni.cz/en)

#!/bin/bash

sudo docker run -it -v /home/francois/.R/library/:/library -v /home/francois/R-dev/:/R-dev fmichonneau/phylo-docker-r-devel /usr/local/bin/RD

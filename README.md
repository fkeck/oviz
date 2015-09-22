
[![Build Status](https://travis-ci.org/fkeck/oviz.svg?branch=master)](https://travis-ci.org/fkeck/oviz)

# oviz
Vizualisation Methods in Freshwater Sciences (well, for now you can just plot a Maucha diagram!)

## Install

To install the package you can use devtools:

    devtools::install_github("fkeck/phylosignal")
    
    
## Maucha diagram
    library(oviz)
    data(ionwaters)
    maucha(ionwaters)

![maucha_demo](http://www.pieceofk.fr/wp-content/uploads/2015/09/maucha_demo.png)

## License
The package `oviz` is developed by Kálmán Tapolczai and François Keck. It is released under the GNU General Public License (GPL-3) in the hope that it will be useful, but without any warranty.


# Previsões operacionais do NMME

O foco é desenvolver métodos de pós-processamento das previsões do NMME para serem usados em modo operacional para o Brasil. Portanto, deve-se considerar os modelos operacionais, ou seja, com em tempo real. O pós-processamento exige o maior período de dados possível para treino e teste dos métodos. A série de 1982-2016 parece ser o maior período entre os modelos.


## Previsões em tempo real e modelos correspondentes no site do IRI

As previsões do NMME são disponibilizadas em https://ftp.cpc.ncep.noaa.gov/NMME/realtime_anom/. A visualização das previsões são encontradas em https://www.cpc.ncep.noaa.gov/products/NMME/monanom.shtml.

Os modelos e on links no IRI são:

- [CFSv2](http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NCEP-CFSv2/) (NCEP- National Centers for Environmental Prediction)

  - HINDCAST (Jan 1982 - Nov 2010)
  
  - FORECAST (Mar 2011 - Jul 2022)


- [CanCM4i](http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanCM4i/) (CMC1, CMC - Canadian Meteorological Center).

  - HINDCAST (Jan 1981 - Nov 2018)

  - FORECAST(Dec 2016 - Oct 2021)

- [GEM5_NEMO](https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.CanSIPS-IC3/.GEM5-NEMO/) (CMC2, CMC - Canadian Meteorological Center)

  - HINDCAST (Jan 1980 - Nov 2020)
  
  - FORECAST (Oct 2021 - Jun 2022)

- [GFDL_SPEAR](http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.GFDL-SPEAR/.HINDCAST/.MONTHLY) (Geophysical Fluid Dynamics Laboratory)

  - HINDCAST (1991-2020)

  - FORECAST (Dec 2020 - Jun 2022)


- [NCAR_CCSM4](http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY) (National Center for Atmospheric Research) 

  - MONTHLY (Jan 1982 - Jun 2022)
<br />  
- [NASA_GEOS5v2](http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GMAO-062012/.MONTHLY/) provavelmente corresponde ao modelo [NASA-GMAO-062012]() no IRI (National Aeronautics and Space Administration, Goddard Earth Observing System Model, Version 5) ou ao [GEOSS2S](https://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GEOSS2S/).

  - MONTHLY (1981-2017) se NASA-GMAO-062012
  
  - FORECAST (Feb 2017 - Jun 2022) se NASA-GEOSS2S
  
  - HINDCAST (Feb 1981 - Dec 2016) se NASA-GEOSS2S
  

> Conforme [notícias sobre o NMME](https://www.cpc.ncep.noaa.gov/products/NMME/about.html):

1. Como parte do CanSIPS, o *GEM_NEMO foi atualizado pelo GEM5_NEMO no Environment and Climate Change Canada (ECCC) após novembro de 2021*.

2. A partir da rodada de fevereiro de 2021, *GFDL FLOR e CM2.1 foram substituídos por GFDL SPEAR*

3. *CMC1 e CMC2 descontinuados e substituídos por CanCM4i e GEM_NEMO após agosto de 2019*.

4. **As médias do conjunto de NMME para T2m e prate são calculadas para 7 (?) modelos, incluindo CanCM4i e GEM_NEMO em novembro de 2019**.



> As climatologias são disponibilizadas em:

https://ftp.cpc.ncep.noaa.gov/NMME/clim/



### Outros sites para download das previsões em tempo real

> A outro local com as previsões em formato do GRADS. 

https://ftp.cpc.ncep.noaa.gov/International/nmme/
https://ftp.cpc.ncep.noaa.gov/International/nmme/binary_monthly/

### Outro site com as previsões passadas por membro e para todo GLOBO

https://www.earthsystemgrid.org/search.html?Project=NMME&rpp=100&Frequency=Monthly&q=&sort=PUB_DESC&page=1

Exemplo de link para os dados do 1° membro do GEOS-5:

https://tds.ucar.edu/thredds/fileServer/datazone/nmme/output1/NASA-GMAO/GEOS-5/20041101/day/atmos/tas/tas_day_GEOS-5_20041101_r9i1p1.nc



The NMME models include the CFSv2, **two versions of the Canadian models** CanCM4i (CMC1) and GEM-NEMO (CMC2), NOAA’s Geophysical Fluid Dynamic (GFDL), ~~NOAA’s Geophysical Fluid Dynamic (GFDL-FLOR)~~, National Aeronautics and Space Administration (NASA-GEOS5v2), the National Center for Atmospheric Research (NCAR-CCSM4), and the ensemble mean of all the models.


## NASA_GEOS5v2

> dúvida - será que NASA-GMAO-062012 corresponde ao NASA_-GMAO-062012_GEOS5v2?

NASA-GMAO-062012 (1981-2017)

http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.NASA-GMAO-062012/.MONTHLY/.prec/


The GEOS-5 AGCM is currently in use in the NASA Goddard Modeling and Assimilation Office (GMAO) for simulations at a wide range of resolutions, in atmosphere only, coupled ocean-atmosphere, and data assimilation modes.


## NCAR_CCSM4 

http://iridl.ldeo.columbia.edu/SOURCES/.Models/.NMME/.COLA-RSMAS-CCSM4/.MONTHLY

University of Miami’s Rosenstiel School of Marine and
Atmospheric Science participates in NMME by providing
hindcast and forecast CCSM4 climate predictions

https://conference.ifas.ufl.edu/NCER2016/presentations/40_1440_Infanti.pdf


- - - 

## Referências úteis

https://iri.columbia.edu/our-expertise/climate/forecasts/seasonal-climate-forecasts/

https://iri.columbia.edu/our-expertise/climate/forecasts/seasonal-climate-forecasts/methodology/

https://weather.gc.ca/grib/grib2_cansips_e.html

https://dd.weather.gc.ca/ensemble/cansips/grib2/hindcast/raw/

https://www.cpc.ncep.noaa.gov/products/international/nmme/nmme_monthly_body.html

https://ftp.cpc.ncep.noaa.gov/International/nmme/binary_monthly/




Boa tarde Prof. Piquini e secretári@s.
Como nas ofertas anteriores, encaminho em anexo tabela da oferta de disciplinas de 2022.1 com as disciplinas do curso de Meteorologia e os professores responsáveis indicados, após consulta a tod@s professor@s da Meteorologia. Alterações em relação ao semestre 2021.1 estão destacadas em vermelho e negrito. Entre as mudanças está indicado a disponibilidade do Prof. Franciano Punhales em ministrar as disciplinas de Fìsica I (FSC 1001) e Lab. Física I (FSC 122) para as turmas de meteorologia (131, T10).
À disposição para qualquer esclarecimento. 
Obrigado.
At.te. 
JDT

markdown
# README
The full name of FIO-AOW is Atmosphere-Ocean-Wave coupled model which is developed at the First Institute of Oceanography, Ministry of Natural Resources. It consists of atmosphere component [WRF](https://github.com/wrf-model/WRF)，ocean surface wave component MASNUM and ocean circulation components [POM](http://www.ccpo.odu.edu/POMWEB/)&[ROMS](https://github.com/myroms/roms). These three components are integrated together through a community [C-Coupler](https://github.com/C-Coupler-Group/c-coupler-lib). It should be noted that all the components of FIO-AOW are open-source except MASNUM. The copyright for MASNUM is owned by FIO, and its use requires permission and approval. In FIO-AOW, the POM and MASNUM were speeded up through MPI technique by Dr. Guansuo Wang (wang et al., 2010).

<div align="center">
    <img src="https://github.com/Biao-Zhao/Biao-Zhao.github.io/blob/main/images/fio-aow.png" alt="FIO-AOW framework">
</div>
<div align="center" style="font-family: 'Microsoft YaHei'; font-size: 40px; font-weight: bold;">
    <em>FIO-AOW framework</em>
</div>
<br><br>
FIO-AOW has been used to study the impacts of ocean surface waves on tropical cyclone. Currently, the following wave related physical processes have been considered in FIO-AOW, including
1. Thermal effect of sea spray on air-sea heat and moisture fluxes
2. Sea state dependent air-sea momentum flux
3. Non-breaking wave-induced vertical mixing
4. Relative wind speed
5. Rain-induced surface cooling
   
<div align="center">
    <img src="https://github.com/Biao-Zhao/Biao-Zhao.github.io/blob/main/images/tc.png" alt="Atmosphere-Ocean-Wave shematic">
</div>
<div align="center" style="font-family: 'Microsoft YaHei'; font-size: 40px; font-weight: bold;">
    <em>Atmosphere-Ocean-Wave interaction shematic</em>
</div>
<br><br>

For further details on the FIO-AOW, readers may refer to the papers listed below.

1. **Zhao, B.**, Wu, L., Wang, G., Zhang, J. A., et al., (2024). A numerical study of tropical cyclone and ocean responses to air-sea momentum flux at high winds. Journal of Geophysical Research: Oceans, 129(7), e2024JC020956, https://doi.org/10.1029/2024JC020956
   
2. **Zhao, B.**, Wang, G., Zhang, J. A., Liu, L., Liu, J., Xu, J., et al. (2022). The effects of ocean surface waves on tropical cyclone intensity: Numerical simulations using a regional atmosphere-ocean-wave coupled model. Journal of Geophysical Research: Oceans, 127, e2022JC019015, https://doi.org/10.1029/2022JC019015
   
3. **Zhao, B.**, Qiao, F., Cavaleri, L., Wang, G., Bertotti, L., and Liu, L. (2017), Sensitivity of typhoon modeling to surface waves and rainfall, J. Geophys. Res. Oceans, 122, 1702–1723, https://doi:10.1002/2016JC012262.
   
4. Wang, G., **Zhao, B.**, Qiao, F. et al. (2018). Rapid intensification of Super Typhoon Haiyan: the important role of a warm-core ocean eddy. Ocean Dynamics 68, 1649–1661, https://doi.org/10.1007/s10236-018-1217-x

5. Li, S., **Zhao, B.**, Ma, S., Yin, X., Ji, D., Qiao, F., 2024. Effects of Sea Spray on Extreme Precipitation Forecasting: A case study in Beijing of China. Geophysical Research Letters, 51, e2024GL109923. https://doi.org/10.1029/2024GL109923

6.	Sun, C., Liu, L., Li, R., Yu, X., Yu, H., **Zhao, B.**, Wang, G., Liu, J., Qiao, F., and Wang, B. (2021). Developing a common, flexible and efficient framework for weakly coupled ensemble data assimilation based on C-Coupler2.0, Geosci. Model Dev., 14, 2635-2657, https://doi.org/10.5194/gmd-14-2635-2021.
   
7.	Wang, G., Qiao, F. & Xia, C. (2010). Parallelization of a coupled wave-circulation model and its application. Ocean Dynamics 60, 331–339, https://doi.org/10.1007/s10236-010-0274-6

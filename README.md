# RothC-Climate-Change


This script calculates the change in carbon stock (tC/ha) over time, considering soil conditions and climate change. It calibrates and simulates data that return no errors. Some data on evapotranspiration, temperature, and precipitation can result in unrealistic respiration vectors, leading to errors. In such cases, the `soilR` library used in the RothC model returns an error. The script identifies these errors and processes only the datasets that do not produce error messages.


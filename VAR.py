# -*- coding: utf-8 -*-
"""
Created on Fri Jul 22 10:50:11 2022

@author: Karengi
"""

#=====================IMpORT PACKAGES=========================================#

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import statsmodels.api as sm

#=======================DATA==================================================#

data= sm.datasets.get_rdataset("USMacroG","AER")

print(data.data)

dk = sm.tsa

## ---------------------------------------------------------------
## Description: model births and deaths by racial and ethnic group
## ---------------------------------------------------------------

import matplotlib.cm as cm
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from pathlib import Path

from regmod.composite_models import TreeModel
from regmod.data import Data
from regmod.utils import SplineSpecs
from regmod.variable import Variable, SplineVariable

my_cmap = cm.get_cmap('tab20')

# Load and format data #####################################

# Load the data for an all ages births or deaths model 
df = pd.read_csv('FILEPATH')  

# Cascade variables 
df['national'] = 'US National'   
df['region_name'] = df.region_name
df['race_by_region'] = df.race_group + ' in ' + df.region_name
df['race_by_state'] = df.race_group + ' in ' + df.location_name
 
# Offset variable (population)
df['log_pop'] = np.log(df['pop'])

# Model parameters #########################################

# Data parameters
dependent_var = 'deaths' # specify births or deaths 
independent_var = 'year_id'  
extra_vars = []
offset_var = 'log_pop'

# Variable parameters
intercept_mask_value = 10 # set
independent_mask_value = 10 # set

# Spline parameters
knots = np.linspace(0.0, 1.0, 2)
degree = 2                    # default 3
l_linear = False              # default False
r_linear = False              # default False
knots_type = 'rel_domain'     # default 'abs'

# Model parameters
model_specification_name = 'deaths_collapse_allages_race_by_state' # specify whether collapsing or not collapsing
cascade_levels = ['national','region_name', 'race_by_region', 'race_by_state']
mtype = 'poisson'
level_masks = [1.0, 1.0, 1.0, 1.0] 

# Model setup ###############################################

# Data object
model_data = Data(
    col_obs=dependent_var,
    col_covs=[independent_var], 
    col_offset=offset_var
)

# Intercept variable
intercept_var = Variable('intercept')
intercept_mask = np.ones(intercept_var.size)*intercept_mask_value

# Spline variables
independent_vars = [
    SplineVariable(
        independent_var,
        spline_specs=SplineSpecs(
            knots=knots,
            degree=degree,
            l_linear=l_linear,
            r_linear=r_linear,
            knots_type=knots_type
        )
    )
]
independent_masks = np.ones(independent_vars[0].size)*independent_mask_value

# Model object
cascade_model = TreeModel.get_simple_tree(
    name=model_specification_name,
    df=df,
    col_label=cascade_levels,
    model_specs={
        'name': model_specification_name,
        'data': model_data,
        'variables': [intercept_var] + independent_vars, 
        'mtype' : mtype
    },
    var_masks={
        'intercept': intercept_mask_value, 
        independent_var: independent_masks   
    },
    lvl_masks=level_masks
)

# Fit models ################################################

print(f"There are {len(cascade_model)} models.")
cascade_model.fit()


# Predict the model at each cascade level ####################

# Set parameters 
out_path = Path('FILEPATH') 

national = ['US National']
regions = ['South', 'West', 'Midwest', 'Northeast']
res = ['Hispanic', 'NH API', 'NH AIAN','NH Black', 'NH White']
states = {
'Midwest':['Illinois', 'Indiana', 'Iowa', 'Kansas', 'Michigan', 'Minnesota', 'Missouri', 'Nebraska', 'North Dakota', 'Ohio', 'South Dakota', 'Wisconsin'],
'South': ['Alabama', 'Arkansas', 'Delaware', 'District of Columbia', 'Florida', 'Georgia', 'Kentucky','Louisiana', 'Maryland', 'Mississippi', 'North Carolina', 'Oklahoma', 'South Carolina', 'Tennessee', 'Texas', 'Virginia', 'West Virginia'], 
'West':['Alaska', 'Arizona', 'California', 'Colorado', 'Hawaii', 'Idaho', 'Montana', 'Nevada', 'New Mexico', 'Oregon', 'Utah', 'Washington', 'Wyoming'],
'Northeast': ['Connecticut', 'Maine', 'Massachusetts', 'New Hampshire', 'New Jersey', 'New York','Pennsylvania', 'Rhode Island', 'Vermont']
}

df_pred = pd.DataFrame({"year_id": [1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019]})


# Incorporate uncertainty #####################################

num_draws = 1000

all_dfs = []
national_dfs = []
for national in national:
    sub_model = cascade_model[f"mmr_allages_race_by_state/{national}"] #specify (deaths, births)
    df_pred_country = sub_model.get_draws(df_pred, size=num_draws)
    draws_country = df_pred_country[[f"value_{i}" for i in range(num_draws)]].values
    national_dfs.append(sub_model.predict(df_pred_country).assign(id=f"{national}"))
    region_dfs = []
    for region in regions:
        sub_model = cascade_model[f"deaths_allages_race_by_state/{national}/{region}"] #specify (deaths, births) 
        df_pred_region = sub_model.get_draws(df_pred, size=num_draws)
        draws_region = df_pred_region[[f"value_{i}" for i in range(num_draws)]].values
        region_dfs.append(sub_model.predict(df_pred_region).assign(id=f"{region}"))
        re_dfs = []
        for re in res:
            sub_model = cascade_model[f"deaths_allages_race_by_state/{national}/{region}/{re} in {region}"] #specify (deaths, births)
            df_pred_res = sub_model.get_draws(df_pred, size=num_draws) 
            draws_res = df_pred_res[[f"value_{i}" for i in range(num_draws)]].values
            re_dfs.append(sub_model.predict(df_pred_res).assign(id=f"{region}/{re}"))
            state_dfs = []
            for state in states[region]:
                sub_model = cascade_model[f"deaths_allages_race_by_state/{national}/{region}/{re} in {region}/{re} in {state}"] #specify (deaths, births)
                df_pred_state = sub_model.get_draws(df_pred, size=num_draws) 
                draws_state = df_pred_state[[f"value_{i}" for i in range(num_draws)]].values
                state_dfs.append(sub_model.predict(df_pred_state).assign(id=f"{region}/{re}/{state}"))
            all_dfs += state_dfs
        all_dfs += re_dfs
    all_dfs += region_dfs
all_dfs += national_dfs
pd.concat(all_dfs).to_csv(out_path / 'model_output_deaths_15.csv', index=False) #specify (deaths, births)

# sh /FILEPATH/
# conda activate regmod
# python

from functools import partial
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.sparse import csr_matrix, block_diag
from dimsm.dimension import Dimension
from dimsm.measurement import Measurement
from dimsm.process import Process, default_gen_vmat
from dimsm.prior import GaussianPrior
from dimsm.utils import reshape_var
from dimsm.smoother import Smoother
import os

# load data
df = pd.read_csv("/FILEPATH/deaths_regmod_2021_10_01.csv")
df["group_id"] = (df.location_id.astype(str) + df.population_group_id.astype(str)).astype(int)
# df = df.merge(df_results[["location_id", "group_id", "year_id", "age_group_id", "sex_id", "regmod_pred"]],
#               on=["location_id", "group_id", "year_id", "age_group_id", "sex_id"], how="outer")

# smooth deaths
meas = Measurement(df, col_value="deaths", imat=df.population.values)
dims = [
    Dimension("age_group_id", grid=df.age_group_id.unique()),
    Dimension("year_id", grid=df.year_id.unique()),
    Dimension("group_id", grid=df.group_id.unique()),
    Dimension("sex_id", grid=df.sex_id.unique())
]
prcs = {"year_id": Process(order=1, gen_vmat=partial(default_gen_vmat, size=2, sigma=0.005)),}

var_shape = tuple(dim.size for dim in dims)
dim_index = 1
n = var_shape[dim_index]
k = np.prod(var_shape) // n

mat = np.zeros((2, n))
mat[0, :2] = 1.0
mat[1, -2:] = 1.0
mat = csr_matrix(block_diag([mat]*k))
indices = reshape_var(
    np.arange(n*k), var_shape, dim_index, reverse=True
)
mat = mat[:, indices]
gpriors = {"year_id": [GaussianPrior(mean=0.0, imat=100.0, mat=mat)]}

smoother = Smoother(dims, meas, prcs, gpriors=gpriors)
smoother.fit(verbose=True)
meas = Measurement(df, col_value="deaths")
meas.update_dim(dims)
df["dimsm_pred"] = meas.mat.dot(smoother.opt_vars[0])

out_path = Path('/FILEPATH/')
os.chdir(out_path.as_posix())
out_filepath = (out_path / 'dimsm_sigma_p005.csv').as_posix()
out_df = df.copy()
out_df.to_csv(out_filepath, index = False)

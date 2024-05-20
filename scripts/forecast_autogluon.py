import pandas as pd
import numpy as np
import gc
from autogluon.timeseries import TimeSeriesDataFrame, TimeSeriesPredictor

r.datos_train.head()
r.datos_train.isnull().values.any()

# train

datos_train = r.datos_train
datos_train

ts_train = TimeSeriesDataFrame.from_data_frame(
    datos_train,
    id_column="cod",
    timestamp_column="fecha_corte"
)

predictor_ts_train.leaderboard()

ts_train

predictor_ts_train = TimeSeriesPredictor(
    prediction_length=28,
    path="modelos/autogluon_ts_train",
    target="creci_dia",
    #eval_metric="RMSE",
    quantile_levels = [0.05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5,
                       0.6, 0.7, 0.75, 0.8, 0.9, 0.95],
    freq = "D",
    known_covariates_names=["dia_no_habil", "trm", "ipc"]
)

predictor_ts_train.fit(
    ts_train,
    presets="best_quality",
    time_limit=7200,
    num_val_windows = 13,
    random_seed =4981
)

test_data_ts = r.datos_test.drop(["creci_dia"], axis=1)
test_data_ts

ts_test = TimeSeriesDataFrame.from_data_frame(
    test_data_ts,
    id_column="cod",
    timestamp_column="fecha_corte")

ts_test

predictions_test = predictor_ts_train.predict(ts_train, known_covariates=ts_test)
predictions_test

r.datos_test
datos_comp = TimeSeriesDataFrame.from_data_frame(
    r.datos_test,
    id_column="cod",
    timestamp_column="fecha_corte")
datos_comp


def rmse(predictions, targets):
    return np.sqrt(((predictions - targets) ** 2).mean())

rmse(predictions_test["mean"].values, datos_comp["creci_dia"].values)

###########
import matplotlib.pyplot as plt

plt.figure(figsize=(20, 3))

item_id = "5_16_1_10936"
y_past = ts_train.loc[item_id]["creci_dia"]
y_pred = predictions_test.loc[item_id]
y_test = datos_comp.loc[item_id]["creci_dia"]

plt.plot(y_past[-200:], label="Past time series values")
plt.plot(y_pred["mean"], label="Mean forecast")
plt.plot(y_test, label="Future time series values")

plt.fill_between(
    y_pred.index, y_pred["0.05"], y_pred["0.95"], color="red", alpha=0.1, label=f"5%-95% confidence interval"
)
plt.fill_between(
    y_pred.index, y_pred["0.25"], y_pred["0.75"], color="blue", alpha=0.1, label=f"25%-75% confidence interval"
)
plt.legend()
plt.show()

gc.collect()
###########
r.datos_modelar.tail()
r.datos_modelar.isnull().values.any()

train_data = TimeSeriesDataFrame.from_data_frame(
    r.datos_modelar,
    id_column="cod",
    timestamp_column="fecha_corte"
)

train_data

predictor = TimeSeriesPredictor(
    prediction_length=28,
    path="modelos/autogluon_full",
    target="creci_dia",
    #eval_metric="RMSE",
    quantile_levels = [0.05, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5,
                       0.6, 0.7, 0.75, 0.8, 0.9, 0.95],
    freq = "D",
    known_covariates_names=["dia_no_habil", "trm", "ipc"]
)

predictor.fit(
    train_data,
    presets="best_quality",
    time_limit=7200,
    num_val_windows = 13,
    random_seed =4981
)

#predictor = TimeSeriesPredictor.load("modelos/autogluon_full")

predictor.leaderboard()

r.datos_forecast.head()
r.datos_forecast.isnull().values.any()
test_data = r.datos_forecast.drop(["creci_dia"], axis=1)
test_data

datos_test = TimeSeriesDataFrame.from_data_frame(
    test_data,
    id_column="cod",
    timestamp_column="fecha_corte")
    
datos_test

datos_comp = TimeSeriesDataFrame.from_data_frame(
    r.datos_forecast,
    id_column="cod",
    timestamp_column="fecha_corte")
    
datos_comp

predictions = predictor.predict(train_data, known_covariates=datos_test)
predictions.head()

###
plt.figure(figsize=(20, 3))

item_id = "5_16_1_10936"
y_past = train_data.loc[item_id]["creci_dia"]
y_pred = predictions.loc[item_id]
y_test = datos_comp.loc[item_id]["creci_dia"]

plt.plot(y_past[-200:], label="Past time series values")
plt.plot(y_pred["mean"], label="Mean forecast")
plt.plot(y_test, label="Future time series values")

plt.fill_between(
    y_pred.index, y_pred["0.05"], y_pred["0.95"], color="red", alpha=0.1, label=f"5%-95% confidence interval"
)
plt.fill_between(
    y_pred.index, y_pred["0.25"], y_pred["0.75"], color="blue", alpha=0.1, label=f"25%-75% confidence interval"
)
plt.legend()
plt.show()

predictions_full = predictions.reset_index()
predictions_full.head()

## volver a R

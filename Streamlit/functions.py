import pandas as pd
import numpy as np
import plotly.express as px
import matplotlib.pyplot as plt
import math

# less point on circle grid
def dart_points():
    a = 10
    b = 3
    r1 = 2
    r2 = 1

    #The lower this value the higher quality the circle is with more points generated
    stepSize1 = 0.8
    stepSize2 = 1.57

    #Generated vertices
    positions = []
    t = 0
    while t < 2 * math.pi:
        positions.append((r1 * math.cos(t) + a, r1 * math.sin(t) + b))
        t += stepSize1
    t = 0
    while t < 2 * math.pi:
        positions.append((r2 * math.cos(t) + a, r2 * math.sin(t) + b))
        t += stepSize2
    # middle
    positions.append((a, b))
    df_positions = pd.DataFrame(positions)
    df_positions = df_positions.rename(columns={0:'x', 1:'y'})

    return df_positions


def get_df_locations(df):
    df_len = len(df)
    positions = dart_points()

    # get sample possitions based on len(df)
    positions = positions.sample(df_len, replace=False).reset_index(drop=True)

    # join df's
    df_lunch_location = pd.concat([df, positions], axis=1)
    df_lunch_location['weights'] = df_lunch_location['weights'] ** 1

    return df_lunch_location
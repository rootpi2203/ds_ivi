# Streamlit app
# https://www.youtube.com/watch?v=Sb0A9i6d320&list=PLaumXX-v9ZSjjpvw4XZus46xlPvh573nz&index=3&ab_channel=CodingIsFun
# run the app with the command "streamlit run app.py"

import pandas as pd
import plotly.express as px
import streamlit as st

from functions import *

# Tabname and icon, emoji cheat sheet https://www.webfx.com/tools/emoji-cheat-sheet/
st.set_page_config(page_title='Lunch Fate',
                   page_icon=':dart:',
                   layout='wide')

# ----- get data -----
lunch_dict = {
    'PO': 'Papa Oros',
    'FH': 'FH Kantine',
    'KA': 'Kantina',
    'SW': 'Subway',
    'MI': 'Migros',
    'CO': 'Coop',
    'KE': 'Kebap',
    'DA': 'Dampfschiff',
    'KU': 'Kugis',
    'NÜ': 'Nüt',
    'EG': 'EG bar (nur Bier)'
}

resti = {
    'lunch_at': ['Papa Oros', 'FH Kantine', 'Kantina', 'Subway', 'Migros', 'Coop',
                 'Kebap', 'Dampfschiff', 'Kugis', 'Nüt', 'EG bar (nur Bier)'],
    'weights': [1, 3, 2, 2, 2, 2, 1, 2, 1, 1, 1]
}
# get dataframe with restaurants
df_lunch = pd.DataFrame.from_dict(resti)


# ---- sidebar -----
st.sidebar.header('Lunch Brugg')
columns_org = st.sidebar.multiselect(
    'select lunch options here:',
    options=df_lunch['lunch_at'].to_list(),
    default=df_lunch['lunch_at'].to_list()
)

#st.sidebar.markdown(""" div.stButton > button:first-child {
#background-color: #00cc00;color:white;font-size:20px;height:3em;width:30em;border-radius:10px 10px 10px 10px;
#}
#""", unsafe_allow_html=True)

if st.sidebar.button('rearrange dart'):
    st.experimental_rerun()

weight_po = st.sidebar.slider(f'{lunch_dict["PO"]}', 0, 10, 1)
weight_fg = st.sidebar.slider(f'{lunch_dict["FH"]}', 0, 10, 3)
weight_ka = st.sidebar.slider(f'{lunch_dict["KA"]}', 0, 10, 2)
weight_sw = st.sidebar.slider(f'{lunch_dict["SW"]}', 0, 10, 2)
weight_mi = st.sidebar.slider(f'{lunch_dict["MI"]}', 0, 10, 2)
weight_co = st.sidebar.slider(f'{lunch_dict["CO"]}', 0, 10, 2)
weight_ke = st.sidebar.slider(f'{lunch_dict["KE"]}', 0, 10, 1)
weight_da = st.sidebar.slider(f'{lunch_dict["DA"]}', 0, 10, 2)
weight_ku = st.sidebar.slider(f'{lunch_dict["KU"]}', 0, 10, 1)
weight_nü = st.sidebar.slider(f'{lunch_dict["NÜ"]}', 0, 10, 1)
weight_eg = st.sidebar.slider(f'{lunch_dict["EG"]}', 0, 10, 1)

# write weights
weights = [weight_po, weight_fg, weight_ka, weight_sw, weight_mi, weight_co,
           weight_ke, weight_da, weight_ku, weight_nü, weight_eg]
df_lunch['weights'] = weights

# df after selection
df_lunch_selection = df_lunch[df_lunch['lunch_at'].isin(columns_org)]

# get dart positions
df_lunch_location = get_df_locations(df_lunch_selection)


# ---- main page ----
st.title(':dart: Lunch Fate')
st.markdown('##')

# Top KPI's
st.header('tired of choosing a lunch destination for your group, look no more. With this dashboard fate will lead you... or not :) ')
st.write('Rules: are..')
# KPI's here
st.markdown('---')

#df_lunch_location

# dart fig
fig_dart = px.scatter(df_lunch_location,
               x='x',
               y='y',
               size='weights',
               color='lunch_at',
               hover_name='lunch_at',
               title='dart lunch game',
               template='plotly_dark')
fig_dart.update_layout(
            plot_bgcolor='rgba(0,0,0,0)', yaxis_range=[0, 6], xaxis_range=[7, 13],
            height=800, width=800)
fig_dart.update_xaxes(gridcolor='black')
fig_dart.update_yaxes(gridcolor='black')



st.plotly_chart(fig_dart)

#---- hide STREAMLIT style ----
hide_st_style = '''
    <style>
    #MainMenu {visibility: hidden;}
    footer {visibility: hidden;}
    header {visibility: hidden;}
    </style>
    '''
st.markdown(hide_st_style, unsafe_allow_html=True)
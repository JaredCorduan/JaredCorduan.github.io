import plotly.express as px
import pandas as pd
 

source = pd.DataFrame([
    {"": "Charles Babbage",     "start": "1791-12-26", "end": "1871-10-18"},
    {"": "Paul Bernays",        "start": '1888-10-17', "end": '1977-09-18'},
    {"": "G. Gentzen",          "start": '1909-11-24', "end": '1945-08-04'},
    {"": "Hermann Grassmann",   "start": '1809-04-15', "end": '1877-09-26'},
    {"": "Charles Peirce",      "start": '1839-09-10', "end": '1914-04-19'},
    {"": "Giuseppe Peano",      "start": '1858-08-27', "end": '1932-04-20'},
    {"": "Gottlob Frege",       "start": '1848-11-08', "end": '1925-07-26'},
    {"": "Kurt Gödel",          "start": "1906-04-28", "end": "1978-01-14"},
    {"": "Alan Turing",         "start": "1912-06-23", "end": "1954-06-7" },
    {"": "Thoralf Skolem",      "start": "1887-05-23", "end": "1963-03-23"},
    {"": "David Hilbert",       "start": "1862-01-23", "end": "1943-02-14"},
    {"": "Bertrand Russel",     "start": "1872-05-18", "end": "1970-02-02"},
    {"": "Wilhelm Ackermann",   "start": "1896-03-29", "end": "1962-12-24"},
    {"": "Rózsa Péter",         "start": "1905-02-17", "end": "1977-02-16"},
    {"": "Richard Dedekind",    "start": "1831-10-06", "end": "1916-02-12"},
    {"": "Georg Cantor",        "start": "1845-02-19", "end": "1918-01-06"},
    {"": "Leopold Kronecker",   "start": "1823-12-07", "end": "1891-12-29"},
    {"": "Paul Bernays",        "start": "1888-10-17", "end": "1977-09-18"},
    {"": "Alfred Whitehead",    "start": "1861-02-15", "end": "1947-12-30"},
    {"": "Emil Post",           "start": "1897-02-11", "end": "1954-04-21"},
    {"": "Alonzo Church",       "start": "1903-06-14", "end": "1995-08-11"},
    {"": "Stephen Kleene",      "start": "1909-01-05", "end": "1994-01-25"},
    {"": "Haskell Curry",       "start": "1900-09-12", "end": "1982-09-01"},
    {"": "Georg Kreisel",       "start": "1923-09-15", "end": "2015-03-01"},
    {"": "Andrzej Grzegorczyk", "start": "1922-08-22", "end": "2014-03-20"},
    {"": "John von Neumann",    "start": "1903-12-28", "end": "1957-02-08"},
])
 

source['start'] = pd.to_datetime(source['start'])
source['end'] = pd.to_datetime(source['end'])
 

fig = px.timeline(source.sort_values('start'),
                  x_start="start",
                  x_end="end",
                  y="",
                  text="",
                  width=800,
                  height=800,
                  color_discrete_sequence=["mediumpurple", "blue"])

#fig.add_vrect(x0="1900-01-01", x1="1900-06-01",
#              annotation_text="Hilbert's 23", annotation_position="bottom left",
#              fillcolor="blue", opacity=1.00, line_width=0,
#              )
fig.add_vrect(x0="1946-01-01", x1="1946-06-01",
              annotation_text="ENIAC", annotation_position="bottom left",
              fillcolor="blue", opacity=1.00, line_width=0,
              )
fig.add_vrect(x0="1936-01-01", x1="1936-06-01",
              annotation_text="Church-Turing Thesis", annotation_position="top right",
              fillcolor="blue", opacity=1.00, line_width=0,
              )
fig.update_traces(textfont=dict(color='black', size=16,family='Times New Roman'))
fig.show()
fig.write_image("timeline.png")

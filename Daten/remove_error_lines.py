import pandas as pd 
import csv

df = pd.read_csv(filepath_or_buffer="data_with_sentiment.csv", sep=",", on_bad_lines="warn")

df["offer_description"] = "haha"

df.to_csv("bug_finding2.csv", quoting=csv.QUOTE_ALL)
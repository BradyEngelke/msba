## syllabus: https://docs.google.com/document/d/1gAsLqEwmxqr372_AEefi_ktt-jlqB7vUkEFsX9R66Ks/edit#

## packages
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
import spacy as s

## data
books = pd.read_csv('data/bestsellers_with_categories.csv') # https://www.kaggle.com/sootersaalu/amazon-top-50-bestselling-books-2009-2019
housing = pd.read_csv('data/Melbourne_housing_FULL.csv') # https://www.kaggle.com/anthonypino/melbourne-housing-market
nba_combine = pd.read_csv('data/nba_draft_combine_all_years.csv') # https://data.world/achou/nba-draft-combine-measurements
netflix = pd.read_csv('data/netflix_titles.csv') # https://www.kaggle.com/shivamb/netflix-shows
hulu = pd.read_csv('data/hulu_titles.csv')
dplus = pd.read_csv('data/disney_plus_titles.csv')
amazon = pd.read_csv('data/amazon_prime_titles.csv')
customers = pd.read_csv('data/Churn_Modelling.csv') # https://www.kaggle.com/shubh0799/churn-modelling/
trip_reviews = pd.read_csv('data/tripadvisor_hotel_reviews.csv') # https://www.kaggle.com/andrewmvd/trip-advisor-hotel-reviews
planets = pd.read_csv('data/cumulative.csv') # https://www.kaggle.com/nasa/kepler-exoplanet-search-results

## DS methods
# scatter plot | https://seaborn.pydata.org/generated/seaborn.relplot.html#seaborn.relplot
X = pd.DataFrame({'a': [1, 6, 3, 6, 8, 3, 4, 9], 'b': [5, 7, 2, 4, 9, 1, 5, 4], 'c': [8, 6, 1, 2, 6, 8, 3, 7], 'd': [9, 2, 7, 7, 3, 3, 3, 3]})
sns.relplot(data=X, x='a', y='b')
plt.show()

# line plot | https://seaborn.pydata.org/generated/seaborn.lineplot.html#seaborn.lineplot
X = pd.DataFrame({'a': [1, 6, 3, 6, 8, 3, 4, 9], 'b': [5, 7, 2, 4, 9, 1, 5, 4], 'c': [8, 6, 1, 2, 6, 8, 3, 7], 'd': [9, 2, 7, 7, 3, 3, 3, 3]})
sns.lineplot(data=X, x='c', y='d')
plt.show()

# regression | https://www.statsmodels.org/dev/generated/statsmodels.regression.linear_model.OLS.html#statsmodels.regression.linear_model.OLS
X = pd.DataFrame({'a': [1, 6, 3, 6, 8, 3, 4, 9], 'b': [5, 7, 2, 4, 9, 1, 5, 4], 'c': [8, 6, 1, 2, 6, 8, 3, 7], 'd': [9, 2, 7, 7, 3, 3, 3, 3]})
Y = [100, 60, 34, 65, 81, 32, 42, 95]
results = sm.OLS(Y, X).fit()
results.summary()
X_test = pd.DataFrame({'a': [6], 'b': [7], 'c': [8], 'd': [3]})
round(results.predict(X_test).values[0], 1)

# classification
# logistic regression | # https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LogisticRegression.html#sklearn.linear_model.LogisticRegression
X = pd.DataFrame({'a': [1, 6, 3, 6, 8, 3, 4, 9], 'b': [5, 7, 2, 4, 9, 1, 5, 4], 'c': [8, 6, 1, 2, 6, 8, 3, 7], 'd': [9, 2, 7, 7, 3, 3, 3, 3]})
Y = [1, 0, 1, 0, 1, 0, 1, 1]
clf = LogisticRegression(random_state=0).fit(X, Y)
X_test = pd.DataFrame({'a': [6], 'b': [7], 'c': [8], 'd': [3]})
clf.predict(X_test)[0]
clf.predict_proba(X_test)

# random forest | https://scikit-learn.org/stable/modules/generated/sklearn.ensemble.RandomForestClassifier.html
X = pd.DataFrame({'a': [1, 6, 3, 6, 8, 3, 4, 9], 'b': [5, 7, 2, 4, 9, 1, 5, 4], 'c': [8, 6, 1, 2, 6, 8, 3, 7], 'd': [9, 2, 7, 7, 3, 3, 3, 3]})
Y = [1, 0, 1, 0, 1, 0, 1, 1]
clf = RandomForestClassifier(max_depth=2, random_state=0).fit(X, Y)
X_test = pd.DataFrame({'a': [6], 'b': [7], 'c': [8], 'd': [3]})
clf.predict(X_test)[0]
clf.feature_importances_

# clustering | https://scikit-learn.org/stable/modules/generated/sklearn.cluster.KMeans.html
X = pd.DataFrame({'a': [1, 6, 3, 6, 8, 3, 4, 9], 'b': [5, 7, 2, 4, 9, 1, 5, 4], 'c': [8, 6, 1, 2, 6, 8, 3, 7], 'd': [9, 2, 7, 7, 3, 3, 3, 3]})
kmeans = KMeans(n_clusters=4, random_state=0).fit(X)

SSEs = []
for i in range(2, 7):
    kmeans_instance = KMeans(n_clusters=i, random_state=0).fit(X)
    SSEs.append(kmeans_instance.inertia_)

sns.lineplot(x=range(2, 7), y=SSEs)
plt.show()

k = 4
kmeans_final = KMeans(n_clusters=k, random_state=0).fit(X)
kmeans_final.labels_
kmeans_final.predict([[7, 7, 7, 6], [5, 7, 4, 3]])
kmeans_final.cluster_centers_

# pca | https://scikit-learn.org/stable/modules/generated/sklearn.decomposition.PCA.html
X = pd.DataFrame({'a': [1, 6, 3, 6, 8, 3, 4, 9], 'b': [5, 7, 2, 4, 9, 1, 5, 4], 'c': [8, 6, 1, 2, 6, 8, 3, 7], 'd': [9, 2, 7, 7, 3, 3, 3, 3]})
pca = PCA(n_components=3).fit(X)
pca.explained_variance_ratio_[2]
pca.components_

explained_variances = []
for i in range(2, 5):
    pca_instance = PCA(n_components=i).fit(X)
    explained_variances.append(pca_instance.explained_variance_ratio_[i-1])

sns.lineplot(x=range(2, 5), y=explained_variances)
plt.show()

# NLP | https://v2.spacy.io/usage/spacy-101
nlp = s.load('en_core_web_sm')
doc = nlp("Apple is looking at buying U.K. startup for $1 billion")

# tokenization
for token in doc:
    print(token.text, token.lemma_, token.pos_, token.tag_, token.dep_,
          token.shape_, token.is_alpha, token.is_stop)

# named entity extraction
for ent in doc.ents:
    print(ent.text, ent.start_char, ent.end_char, ent.label_)

# visualize sentence structure
from spacy import displacy
displacy.serve(doc)
import pandas as pd
import numpy as np
from pandas.io.parsers import read_csv
import sys

"""
usage:

To use the default date range and output result as 'submit1.csv'
python meanValue.py 1

To use all data from Jan. 1 to Apr. 30
python meanValue.py 1 '2013-01-01'
"""

# edit DATA_PATH to the directory with train and test
DATA_PATH = '../data/'
# edit SUBMIT_PATH to where you want it to go
SUBMIT_PATH = '../submissions/submit%d.csv'
TRAIN_PATH = DATA_PATH + 'train.csv'
TEST_PATH = DATA_PATH + 'test.csv'
LABEL_COLS = ['num_views', 'num_votes', 'num_comments']
SUBMIT_COLS = ['id', 'num_views', 'num_votes', 'num_comments']


def meanValueByCityAndSource(submit_num, start='2013-04-03'):
    train = read_csv(TRAIN_PATH)
    test = read_csv(TEST_PATH)
    # Sets the index to a time series so we can slice by date.
    train.index = pd.DatetimeIndex(train.created_time)
    train = train[start:]
    # log transform targets
    for col in LABEL_COLS:
        train[col] = np.log(train[col] + 1)
    train.source.fillna('nodata', inplace=True)
    test.source.fillna('nodata', inplace=True)
    # collapse source to 3 values:
    # remote_api=2, city_initiated=1, everything else=0
    train['src'] = 2*(train.source == 'remote_api_created') + \
        (train.source == 'city_initiated')
    test['src'] = 2*(test.source == 'remote_api_created') + \
        (test.source == 'city_initiated')
    # add city: NH=0, CHI=1, RICH=2, OAK=3
    train['city'] = 2*(train.latitude < 40) + (train.longitude < -80)
    test['city'] = 2*(test.latitude < 40) + (test.longitude < -80)
    train = train[['city', 'src', 'num_views', 'num_votes', 'num_comments']]
    # predict the mean value, grouped by city and (reduced) source
    mean_vals = train.groupby(['city', 'src']).mean()
    test = test.merge(mean_vals,
                      how='left',
                      left_on=['city', 'src'],
                      right_index=['city', 'src'],
                      sort=False,
                      copy=False)
    # raw transform predictions
    for col in LABEL_COLS:
        test[col] = np.exp(test[col]) - 1
    submitpath = SUBMIT_PATH % submit_num
    test[SUBMIT_COLS].to_csv(submitpath, float_format='%.6f', index=False)


if __name__ == '__main__':
    submit_num = int(sys.argv[1])
    if len(sys.argv) == 3:
        meanValueByCityAndSource(submit_num, sys.argv[2])
    else:
        meanValueByCityAndSource(submit_num)

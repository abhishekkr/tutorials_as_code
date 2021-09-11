"""
## Pandas DataFrane UltraQuick Tutorial

### DataFrames

* central data structure in pandas API, a 2D mutable-size heterogenous tabular data with labeled axes. A dict like container for Series objects.

* similar to an in-memory spreadsheet
"""

## import NumPy and Pandas module
import numpy as np
import pandas as pd

"""
## creating a dataframe

* following code cell creates a simple DataFrame with 10 cells: 5 rows, 2 columns ('temperature' and `'activity')

'pd.DataFrame' class to instantiate DataFrame, takes 2 args where 1st provides data & 2nd identifies names of 2 columns
"""
my_dataset = [[0,3], [5,4], [10,5], [15,6], [20,7]]
my_columns = ['temperature', 'activity']
my_df = pd.DataFrame(
    data=np.array(my_dataset),
    columns=my_columns,
)
print(my_df)


## adding new column to existing DataFrame, following creates 3rd column named 'adjusted'
my_df['adjusted'] = my_df['activity'] + 2
print(my_df)


## Pandas provide multiple ways to isolate specific rows, columns, slices or cells in DataFrame
print("Rows #0, #1, #2:")
print(my_df.head(3), '\n')

print("Row #2:")
print(my_df.iloc[[2]], '\n')

print("Rows #1, #2, #3:")
print(my_df[1:4], '\n')

print("Column 'temperature:'")
print(my_df['temperature'])


"""
Task.1 Create a DataFrame

* create 3x4 DataFrame with columns 'monday', 'tuesday', 'wednesday', 'thursday'

> populate each of 12 cells with random int between 0 & 50

* output entire DataFrame, value in cell of row#1 of 'monday' column

* create 5th column named 'friday' populated by row sums of 'tuesday' & 'wednesday'
"""

my_columns = ['monday', 'tuesday', 'wednesday', 'thursday']

random_ints_0_to_50 = np.random.randint(low=0, high=51, size=(3, 4))

my_df = pd.DataFrame(data=random_ints_0_to_50, columns=my_columns)

print(my_df)

print("\nSecond row of the 'monday' column: %d\n" % my_df['monday'][1])

my_df['friday'] = my_df['tuesday'] + my_df['wednesday']

print(my_df)


"""
Task.2 Copying a DataFrame

Pandas allow 2 ways to duplicate DataFrame:

    * 'Referencing', assigning to new dataframe with changes reflected back

    * 'Copying', with 'pd.DataFrame.copy' creating independent copy
"""
# reference
ref_my_df = my_df
print("Starting value of my_df: %d | ref_my_df: %d" % (my_df['monday'][1], ref_my_df['monday'][1]))

my_df.at[1, 'monday'] = my_df['monday'][1] + 1
print("Updated value of my_df: %d | ref_my_df: %d" % (my_df['monday'][1], ref_my_df['monday'][1]))

# copying
copy_my_df = my_df.copy()
print("Starting value of my_df: %d | copy_my_df: %d" % (my_df['monday'][1], copy_my_df['monday'][1]))

my_df.at[1, 'monday'] = my_df['monday'][1] + 1
print("Updated value of my_df: %d | copy_my_df: %d" % (my_df['monday'][1], copy_my_df['monday'][1]))

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# read in data
# table = pd.read_csv("/data/tmp07wuam09/data/cereal.csv")

def time(yy, mm, dd):
    s = f'{yy}-{mm}-{dd}'
    return s

def importData(planet, yy1, mm1, dd1, yy2, mm2, dd2):
    s1 = time(yy1, mm1, dd1)
    s2 = time(yy2, mm2, dd2)
    ddir = '../data'
    fname = f'{ddir}/{planet}_{s1}_{s2}.csv'
    table = pd.read_csv(fname)
    return table


def scatter3( df ):
    ax.scatter( df.iloc[:,1], df.iloc[:,2], df.iloc[:,3])

# Press the green button in the gutter to run the script.
if __name__ == '__main__':

    df1 = importData('Io', 2001, 1, 1, 2001, 2, 2)
    df2 = importData('Mercury', 2001, 1, 1, 2001, 2, 2)
    df4 = importData('Venus', 2001, 1, 1, 2001, 6, 6)
    df5 = importData('Moon', 2001, 1, 1, 2001, 12, 12)
    df6 = importData('Mars', 2001, 1, 1, 2001, 6, 6)

    ax = plt.figure().add_subplot(projection='3d')

    # ax.scatter(x='X', y='Y', z='Z', zdir='y', label='...')
    # ax.scatter( df.iloc[:,1], df.iloc[:,2], df.iloc[:,3])
    scatter3(df5)
    # scatter3(df6)

    # Make legend, set axes limits and labels
    ax.legend()
    ax.set_xlabel('X')
    ax.set_ylabel('Y')
    ax.set_zlabel('Z')

    ax.view_init(elev=20., azim=-35)

    plt.show()

    # print_hi('PyCharm')
    # print(table)
    # print( time(2011, 1, 1) )


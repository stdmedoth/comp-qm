import matplotlib.pyplot as plt
import numpy as np

data = np.loadtxt('data.csv')

print(data[0])

fig = plt.figure()
ax = fig.add_subplot(projection='3d')
ax.scatter(data[:, 0], data[:, 1], data[:, 2])

plt.show()
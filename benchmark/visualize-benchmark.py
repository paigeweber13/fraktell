import pandas as pd
import matplotlib.pyplot as plt

seq = pd.read_csv('benchmark/per-image-size-sequential.csv')
par = pd.read_csv('benchmark/per-image-size-parallel.csv')

fig = plt.figure(figsize=(10, 7))
plt.plot(seq['num Mpixels'], seq[' time (s)'], marker='o', color='r')
plt.plot(par['num Mpixels'], par[' time (s)'], marker='o', color='b')
plt.legend(['sequential execution', 'parallel execution'])
plt.xlabel('Size of Image (Mp)')
plt.ylabel('Time to generate (s)')
plt.title('Execution time comparison')

plt.show()

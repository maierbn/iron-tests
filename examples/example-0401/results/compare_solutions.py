#!/usr/bin/env python
#
# script to compare nodal field values between OpenCMISS-iron and Matlab

from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
import exnode_reader

print "Numpy version: "+np.version.version
print ""

# tolerance used to check numerical solution
tol         = 1.0e-10

# number of elements in each coordinate direction for different refinement levels
NumberOfElements = np.zeros((1,2), dtype=int)
NumberOfElements[0][0] = 24
NumberOfElements[0][1] = 24

NumberOfTests       = 0
NumberOfFailedTests = 0

failedtests_file = open("failed.tests", "w")

# for all solver types
for s in np.arange(0, 1, 1):
  # for all interpolation orders
  for i in np.arange(1, 2, 1):
    # for all spatial resolutions
    for r in np.arange(0, NumberOfElements.shape[0], 1):
      NumberOfTests += 1
      nx = NumberOfElements[r][0]
      ny = NumberOfElements[r][1]
      for t in [1, 3]:
        ####################################################################
        # read reference data
        foldername  = "reference/matlab/l1x1_n"+str(nx)+"x"+str(ny)+"_i"+str(i)+"_s0/"
        filename    = foldername + "vm_"+str(t)+".csv"
        
        matlab_data = np.loadtxt(filename)
        
        # read iron data
        foldername  = "current_run/l1x1_n"+str(nx)+"x"+str(ny)+"_i"+str(i)+"_s"+str(s)+"/"            
        pde_time_step = 0.05
        filenumber = int(t / pde_time_step)
        
        filename = foldername + "Time_2_"+str(filenumber)+".part0.exnode"
        
        iron_data = exnode_reader.parse_file(filename, [["Vm", 1]])   # extract field Vm, component 1
        
        # convert to single list
        iron_data = [item[0] for item in iron_data[1:]]

        #print "len of matlab_data: ", len(matlab_data), ", len of iron_data: ", len(iron_data)
        
        X = range(nx+1)
        Y = range(ny+1)
        X, Y = np.meshgrid(X, Y)
        Z = X.copy()
        for i in range(nx):
          for j in range(ny):
            Z[i,j] = matlab_data[i*ny+j]
        
        fig = plt.figure()
        ax = fig.gca(projection='3d')
        ax.plot_surface(X, Y, Z, cmap=cm.coolwarm, linewidth=1, antialiased=False)
        plt.show()
        
        ####################################################################
        # compute difference
        l2diff = np.linalg.norm(matlab_data-iron_data) / np.linalg.norm(matlab_data)
        if l2diff > tol:
          status = filename+"       | Matlab   - Iron |_2 = "+str(l2diff)
          print status
          if (NumberOfFailedTests == 0):
              failedtests_file.write("Failed tests:\n")
          failedtests_file.write(status)
          NumberOfFailedTests += 1
if (NumberOfFailedTests == 0):
  failedtests_file.write("No failed tests.\n")
failedtests_file.close()
f       = open("results.summary", "w")
status  = "Passed tests: "+str(NumberOfTests-NumberOfFailedTests)+" / "+str(NumberOfTests)+"\n"
print status
f.write(status)
f.close()

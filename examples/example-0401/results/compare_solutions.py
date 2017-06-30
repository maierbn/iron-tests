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
      nx = NumberOfElements[r][0]
      ny = NumberOfElements[r][1]
      for t in [0, 0.1, 0.2, 1, 2,3]:
        NumberOfTests += 1
        print "t=",t
        
        ####################################################################
        # read reference data
        foldername  = "reference/matlab/l1x1_n"+str(nx)+"x"+str(ny)+"_i"+str(i)+"_s0/"
        matlab_filename = "vm_"+str(t)+".csv"
        
        matlab_data = np.loadtxt(foldername+matlab_filename)
        
        # read iron data
        foldername  = "current_run/l1x1_n"+str(nx)+"x"+str(ny)+"_i"+str(i)+"_s"+str(s)+"/"            
        pde_time_step = 0.005
        filenumber = int(t / pde_time_step)
        
        # determine which file is the same
        if True:
          files = exnode_reader.get_exnode_files(foldername)
          min_l2diff = -1
          min_filename = ""
          for filename in files:
            iron_data = exnode_reader.parse_file(foldername+filename, [["Vm", 1]])   # extract field Vm, component 1
            
            # convert to single list
            iron_data = [item[0] for item in iron_data[1:]]
          
            l2diff = np.linalg.norm(matlab_data-iron_data) / np.linalg.norm(matlab_data)
            print "file ",filename, ", l2 norm: ", l2diff
            
            if l2diff < min_l2diff or min_l2diff == -1:
              min_l2diff = l2diff
              min_filename = filename
              
          print "t="+str(t)+", matlab:",matlab_filename,", best fit for iron: ",min_filename,", l2: ",min_l2diff
        
        iron_filename = "Time_2_"+str(filenumber)+".part0.exnode"
        iron_data = exnode_reader.parse_file(foldername+iron_filename, [["Vm", 1]])   # extract field Vm, component 1
        
        # convert to single list
        iron_data = [item[0] for item in iron_data[1:]]

        #print "len of matlab_data: ", len(matlab_data), ", len of iron_data: ", len(iron_data)
        
        X = range(nx+1)
        Y = range(ny+1)
        X, Y = np.meshgrid(X, Y)
        Z_matlab = X.copy()
        Z_iron = X.copy()
        Z_diff = X.copy()
        for x in range(nx+1):
          for y in range(ny+1):
            Z_matlab[x,y] = matlab_data[x*(ny+1)+y]
            Z_iron[x,y] = iron_data[x*(ny+1)+y]
            Z_diff[x,y] = Z_matlab[x,y] - Z_iron[x,y]
        
        print "max:",np.amax(Z_matlab)
        
        fig = plt.figure(figsize=(18,8))
        plt.title("t="+str(t))
        ax = fig.add_subplot(131, projection='3d')
        ax.plot_surface(X, Y, Z_matlab, cmap=cm.coolwarm, linewidth=1, antialiased=False, rstride=1, cstride=1)
        plt.title("matlab file "+str(matlab_filename)+" max:"+str(np.amax(Z_matlab)))
        
        ax = fig.add_subplot(132, projection='3d')
        ax.plot_surface(X, Y, Z_iron, cmap=cm.coolwarm, linewidth=1, antialiased=False, rstride=1, cstride=1)
        plt.title("iron file "+str(iron_filename)+" max:"+str(np.amax(Z_iron)))
        
        ax = fig.add_subplot(133, projection='3d')
        ax.plot_surface(X, Y, Z_diff, cmap=cm.coolwarm, linewidth=1, antialiased=False, rstride=1, cstride=1)
        plt.title("matlab - iron")
        
        plt.tight_layout()
        plt.show()
        
        ####################################################################
        # compute difference
        l2diff = np.linalg.norm(matlab_data-iron_data) / np.linalg.norm(matlab_data)
        if l2diff > tol:
          status = iron_filename+"       | Matlab   - Iron |_2 = "+str(l2diff)
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

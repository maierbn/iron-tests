#!/usr/bin/env python
#
# script to compare nodal field values between OpenCMISS-iron and Matlab

from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
import sys
sys.path.append('../src/scripts/')    # add path of exnode_reader
import exnode_reader
import os

print "Numpy version: "+np.version.version
print ""

# tolerance used to check numerical solution
tol_matlab = 0.001    # matlab solution differs from iron solution
tol_iron = 0.05

# number of elements in each coordinate direction for different refinement levels
NumberOfElements = np.zeros((1,1), dtype=int)
NumberOfElements[0][0] = 64

NumberOfTests       = 0
NumberOfFailedTests = 0

failedtests_file = open("failed.tests", "w")

# for problem types
for [nx,i,s,p] in [[64,2,0,1]]:
  
  
  #print "case ",[nx,i,s,p]
  for t in [0,0.05,0.1,2,3,5]:
    NumberOfTests += 1
    print "t=",t
    
    ####################################################################
    # read reference data
    # read matlab reference data
    foldername  = "reference/matlab/l1x1_n"+str(nx)+"_i"+str(i)+"_s0_05/"
    matlab_filename = "vm_"+str(t)+".csv"
    
    status = "   matlab file: {}\n".format(foldername+matlab_filename)
    
    matlab_data = None
    if os.path.exists(foldername):
      matlab_data = np.loadtxt(foldername+matlab_filename)
    
    # read iron reference data
    foldername  = "reference/iron/l1x1_n"+str(nx)+"_i"+str(i)+"_s0_05/"
    pde_time_step = 0.05
    filenumber = int(t / pde_time_step)
    print "filenumber: ", filenumber
    
    iron_reference_filename = "Time_2_"+str(filenumber)+".part0.exnode"
    
    
    status += "   iron reference file: {}\n".format(foldername+iron_reference_filename)
    iron_reference_data = exnode_reader.parse_file(foldername+iron_reference_filename, [["Vm", 1]])   # extract field Vm, component 1
    
    # read iron data of current run
    foldername  = "current_run/l1x1_n"+str(nx)+"_i"+str(i)+"_s0_05/"      
    pde_time_step = 0.05
    filenumber = int(t / pde_time_step)
    
    iron_filename = "Time_2_"+str(filenumber)+".part0.exnode"
    status += "   iron file: {}\n".format(foldername+iron_filename)
    iron_data = exnode_reader.parse_file(foldername+iron_filename, [["Vm", 1]])   # extract field Vm, component 1
    
    if iron_data is not None:
      #print "size of initial iron_data:",len(iron_data)
      #print "iron_data",iron_data
      iron_data = iron_data[0:len(iron_data):2]
      #print "iron_data 2",iron_data
    
    if iron_data is None:
      print "Warning! no current iron data available for:\n"+status
      continue
    
    # determine which iron file would produce the least l2 error to the current matlab file
    if False:
      files = exnode_reader.get_exnode_files(foldername)
      min_l2diff = -1
      min_filename = ""
      for filename in files:
        iron_data = exnode_reader.parse_file(foldername+filename, [["Vm", 1]])   # extract field Vm, component 1
        
        l2diff = np.linalg.norm(matlab_data-iron_data) / np.linalg.norm(matlab_data)
        print "file ",filename, ", l2 norm: ", l2diff
        
        if l2diff < min_l2diff or min_l2diff == -1:
          min_l2diff = l2diff
          min_filename = filename
          
      print "t="+str(t)+", matlab:",matlab_filename,", best fit for iron: ",min_filename,", l2: ",min_l2diff
    
    # plot values
    if True:
      X = range(nx+1)
      Z_matlab = range(nx+1)
      Z_iron = range(nx+1)
      Z_diff = range(nx+1)
      for x in range(nx+1):
        Z_matlab[x] = matlab_data[x]
        Z_iron[x] = iron_data[x]
        Z_diff[x] = Z_matlab[x] - Z_iron[x]
      
      print "max:",np.amax(Z_matlab)
      
      fig = plt.figure(figsize=(18,8))
      #plt.title("                                                         t="+str(t))
      ax = fig.add_subplot(131)
      ax.plot(X, Z_matlab)
      plt.title("matlab file "+str(matlab_filename)+" max:"+str(np.amax(Z_matlab)))
      
      ax = fig.add_subplot(132)
      ax.plot(X, Z_iron)
      plt.title("iron file "+str(iron_filename)+" max:"+str(np.amax(Z_iron)))
      
      ax = fig.add_subplot(133)
      ax.plot(X, Z_diff)
      plt.title("matlab - iron")
      
      plt.tight_layout()
      plt.show()
      
    ####################################################################
    # compute difference
    
    # matlab reference - iron
    if matlab_data is not None:
      
      print "size of iron_data:",len(iron_data)
      print "size of matlab_data:",len(matlab_data)
      
      l2diff_matlab_iron = np.linalg.norm(matlab_data-iron_data) / np.linalg.norm(matlab_data)
      
      if l2diff_matlab_iron > tol_matlab:
        status = foldername+iron_filename+"       | Matlab   - Iron |_2 = "+str(l2diff_matlab_iron)+"\n"
        print status
        if (NumberOfFailedTests == 0):
            failedtests_file.write("Failed tests:\n")
        failedtests_file.write(status)
        NumberOfFailedTests += 1
    
    # iron reference - iron
    if iron_reference_data is not None:
      
      l2diff_iron_iron = np.linalg.norm(iron_reference_data-iron_data) / np.linalg.norm(iron_data)
      
      if l2diff_iron_iron > tol_iron:
        status = foldername+iron_filename+"       | Iron   - Iron |_2 = "+str(l2diff_iron_iron)+"\n"
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

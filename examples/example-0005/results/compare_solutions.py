#!/bin/python
#
# script to compare nodal field values between OpenCMISS-iron and CHeart
################################################################################
import numpy as np
import sys
################################################################################
print "Numpy version: "+np.version.version
################################################################################
NumberOfTests       = 0
NumberOfFailedTests = 0

# tolerance used to check numerical solution
tolu         = 1.0e-9
# 2D test geometry values
xmin2d = 0
xmax2d = 0.24
# 3D test geometry values
xmin3d = 0
xmax3d = 1
# Dirichlet boundary values
Dirich_left  = 0
Dirich_right = 1

failedtests_file = open("failed.tests", "w")

################################################################################
# 2D
################################################################################
print "  Testing 2D"
################################################################################
# exnode header size
skiprows    = 9
# size of block containing node ID, coordinates, solution values in exnode file
blocksize   = 5

# for all solver types
for s in np.arange(0, 2, 1):
    # for all interpolation orders
    for i in np.arange(1, 3, 1):
        # for 2d
        dim = 2
        ####################################################################
        # read current_run
        foldername  = "current_run/d"+str(dim)+"_i"+str(i)+"_s"+str(s)+"/"
        filename    = foldername + "Example.part0.exnode"
        linecount = 0
        ivals = []
        xvals = []
        matched   = 0
        try:
            with open(filename) as f:
                NumberOfTests += 1
                for line in f:
                    # skip header
                    if linecount < skiprows:
                        linecount += 1
                        continue
                    linecount += 1
                    # node ID
                    if not(np.mod(linecount-skiprows-1, blocksize)):
                        continue
                    # x-coordinate
                    elif not(np.mod(linecount-skiprows-2, blocksize)):
                        ix      = float(line)
                        xvals.append(ix)
                    # y-coordinate
                    elif not(np.mod(linecount-skiprows-3, blocksize)):
                        iy      = float(line)
                    # scalar value
                    elif not(np.mod(linecount-skiprows-4, blocksize)):
                        ival    = float(line)
                        ivals.append(ival)
            xvals = np.array(xvals)
            ivals = np.array(ivals) 
            ####################################################################
            # compute reference (analytical) solution 
            rvals = (Dirich_right-Dirich_left)/(xmax2d-xmin2d)*(xvals-xmin2d)+Dirich_left
            ####################################################################
            # compute difference
            l2diff   = np.linalg.norm(rvals-ivals, 2)
            if (l2diff > tolu):
                status = filename+"       | Analytical - Iron |_2 = "+str(l2diff)
                print status
                failedtests_file.write(status+"\n")
                NumberOfFailedTests += 1
        except:
            pass

################################################################################
# 3D
################################################################################
print "  Testing 3D"
################################################################################
# exnode header size
skiprows    = 10
# size of block containing node ID, coordinates, solution values in exnode file
blocksize   = 6

# for all solver types
for s in np.arange(0, 2, 1):
    # for all interpolation orders
    for i in np.arange(1, 3, 1):
        # for 3d
        dim = 3
        ####################################################################
        # read current_run
        foldername  = "current_run/d"+str(dim)+"_i"+str(i)+"_s"+str(s)+"/"
        filename    = foldername + "Example.part0.exnode"
        linecount = 0
        ivals = []
        xvals = []
        matched   = 0
        try:
            with open(filename) as f:
                NumberOfTests += 1
                for line in f:
                    # skip header
                    if linecount < skiprows:
                        linecount += 1
                        continue
                    linecount += 1
                    # node ID
                    if not(np.mod(linecount-skiprows-1, blocksize)):
                        continue
                    # x-coordinate
                    elif not(np.mod(linecount-skiprows-2, blocksize)):
                        ix      = float(line)
                        xvals.append(ix)
                    # y-coordinate
                    elif not(np.mod(linecount-skiprows-3, blocksize)):
                        iy      = float(line)
                    # z-coordinate
                    elif not(np.mod(linecount-skiprows-4, blocksize)):
                        iz      = float(line)
                    # scalar value
                    elif not(np.mod(linecount-skiprows-5, blocksize)):
                        ival    = float(line)
                        ivals.append(ival)
            xvals = np.array(xvals)
            ivals = np.array(ivals) 
            ####################################################################
            # compute reference (analytical) solution 
            rvals = (Dirich_right-Dirich_left)/(xmax3d-xmin3d)*(xvals-xmin3d)+Dirich_left
            ####################################################################
            # compute difference
            l2diff   = np.linalg.norm(rvals-ivals, 2)
            if (l2diff > tolu):
                status = filename+"       | Analytical - Iron |_2 = "+str(l2diff)
                print status
                failedtests_file.write(status+"\n")
                NumberOfFailedTests += 1
        except:
            pass

if (NumberOfFailedTests == 0):
    failedtests_file.write("No failed tests.\n")
failedtests_file.close()
f       = open("results.summary", "w")
status  = "Passed tests: "+str(NumberOfTests-NumberOfFailedTests)+" / "+str(NumberOfTests)
print status
f.write(status+"\n")
f.close()

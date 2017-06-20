#!/bin/python
#
# script to compare nodal field values between OpenCMISS-iron and CHeart

import numpy as np

print "Numpy version: "+np.version.version
print ""

# exnode header size
skiprows    = 10
# size of block containing node ID, coordinates, solution values in exnode file
blocksize   = 6
# tolerance used to match nodes between CHeart and iron
tolx         = 1.0e-6
# tolerance used to check numerical solution
tolu         = 1.0e-10

# number of elements in each coordinate direction for different refinement levels
NumberOfElements = np.zeros((3,3), dtype=int)
NumberOfElements[0][0] = 2
NumberOfElements[0][1] = 1
NumberOfElements[0][2] = 1
NumberOfElements[1][0] = 4
NumberOfElements[1][1] = 2
NumberOfElements[1][2] = 2
NumberOfElements[2][0] = 8
NumberOfElements[2][1] = 4
NumberOfElements[2][2] = 4

NumberOfTests       = 0
NumberOfFailedTests = 0

failedtests_file = open("failed.tests", "w")

# for all solver types
for s in np.arange(0, 2, 1):
    # for all interpolation orders
    for i in np.arange(1, 3, 1):
        # for all spatial resolutions
        for r in np.arange(0, NumberOfElements.shape[0], 1):
            NumberOfTests += 1
            nx = NumberOfElements[r][0]
            ny = NumberOfElements[r][1]
            nz = NumberOfElements[r][2]
            ####################################################################
            # read reference data
            foldername  = "reference/cheart/l2x1x1_n"+str(nx)+"x"+str(ny)+"x"+str(nz)+"_i"+str(i)+"/"
            filename    = foldername + "SpaceVariable-1.D"
            # load CHeart space variable, skip first line
            cxyz        = np.loadtxt(filename, dtype=float, skiprows=1)
            filename    = foldername + "ScalarVariable-1.D"
            # load CHeart scalar variable, skip first line
            cvals       = np.loadtxt(filename, dtype=float, skiprows=1)
            # now start reading exnode data
            ivals0       = 0.0 * cvals
            foldername  = "reference/iron/l2x1x1_n"+str(nx)+"x"+str(ny)+"x"+str(nz)+"_i"+str(i)+"_s"+str(s)+"/"
            filename    = foldername + "Example.part0.exnode"
            linecount = 0
            matched   = 0
            with open(filename) as f:
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
                    # y-coordinate
                    elif not(np.mod(linecount-skiprows-3, blocksize)):
                        iy      = float(line)
                    # z-coordinate
                    elif not(np.mod(linecount-skiprows-4, blocksize)):
                        iz      = float(line)
                    # scalar value
                    elif not(np.mod(linecount-skiprows-5, blocksize)):
                        ival    = float(line)
                        # match nodes between CHeart and iron
                        # this can be expensive, but we only consider small spatial resolution here...
                        for node in np.arange(0, cxyz.shape[0], 1):
                            cx      = cxyz[node][0]
                            cy      = cxyz[node][1]
                            cz      = cxyz[node][2]
                            l2diff  = np.sqrt((cx-ix)**2.0+(cy-iy)**2.0+(cz-iz)**2.0)
                            if (l2diff < tolx):
                                ivals0[node] = ival
                                matched     += 1
                                break
            ####################################################################
            # read current_run
            ivals       = 0.0 * cvals
            foldername  = "current_run/l2x1x1_n"+str(nx)+"x"+str(ny)+"x"+str(nz)+"_i"+str(i)+"_s"+str(s)+"/"
            filename    = foldername + "Example.part0.exnode"
            linecount = 0
            matched   = 0
            with open(filename) as f:
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
                    # y-coordinate
                    elif not(np.mod(linecount-skiprows-3, blocksize)):
                        iy      = float(line)
                    # z-coordinate
                    elif not(np.mod(linecount-skiprows-4, blocksize)):
                        iz      = float(line)
                    # scalar value
                    elif not(np.mod(linecount-skiprows-5, blocksize)):
                        ival    = float(line)
                        # match nodes between CHeart and iron
                        # this can be expensive, but we only consider small spatial resolution here...
                        for node in np.arange(0, cxyz.shape[0], 1):
                            cx      = cxyz[node][0]
                            cy      = cxyz[node][1]
                            cz      = cxyz[node][2]
                            l2diff  = np.sqrt((cx-ix)**2.0+(cy-iy)**2.0+(cz-iz)**2.0)
                            if (l2diff < tolx):
                                ivals[node]  = ival
                                matched     += 1
                                break
            ####################################################################
            # compute difference
            l2diff_ci   = np.linalg.norm(cvals-ivals, 2)
            l2diff_i0i  = np.linalg.norm(ivals0-ivals, 2)
            if ((l2diff_ci > tolu) and (l2diff_i0i > tolu)):
                status = filename+"       | CHeart   - Iron |_2 = "+str(l2diff_ci) \
                    +"       | Iron_ref - Iron |_2 = "+str(l2diff_i0i)
                print status
                failedtests_file.write(status)
                NumberOfFailedTests += 1
            elif (l2diff_ci > tolu):
                status = filename+"       | CHeart   - Iron |_2 = "+str(l2diff_ci)
                failedtests_file.write(status)
                NumberOfFailedTests += 1
            elif (l2diff_ci > tolu):
                status = filename+"       | Iron_ref - Iron |_2 = "+str(l2diff_i0i)
                failedtests_file.write(status)
                NumberOfFailedTests += 1
failedtests_file.close()
f       = open("results.summary", "w")
status  = "Passed tests: "+str(NumberOfTests-NumberOfFailedTests)+" / "+str(NumberOfTests)+"\n"
print status
f.write(status)
f.close()

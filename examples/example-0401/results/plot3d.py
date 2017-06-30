#!/usr/bin/env python
#
# script to compare nodal field values between OpenCMISS-iron and Matlab

from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
import exnode_reader
from matplotlib import animation
import sys

def animate(t):
  ax1.clear()
  
  foldername = "."
  if len(sys.argv) > 1:
    foldername = sys.argv[1]
  print "folder [{}]".format(foldername)

  files = exnode_reader.get_exnode_files(foldername)
  iron_filename = files[t]
  #for iron_filename in files:
  iron_data = exnode_reader.parse_file(foldername+iron_filename, [["Vm", 1]])   # extract field Vm, component 1
  
  # convert to single list
  iron_data = [item[0] for item in iron_data[1:]]

  n = len(iron_data)
  nx = int(np.sqrt(n))-1
  ny = int(np.sqrt(n))-1
  print "n=",n,", nx=",nx, ", ny=", ny

  X = range(nx+1)
  Y = range(ny+1)
  X, Y = np.meshgrid(X, Y)
  Z_iron = X.copy()
  for x in range(nx+1):
    for y in range(ny+1):
      Z_iron[x,y] = iron_data[x*(ny+1)+y]
        
      #fig = plt.figure(figsize=(18,8))
      #ax = fig.add_subplot(131, projection='3d')

  ax1.plot_surface(X, Y, Z_iron, cmap=cm.coolwarm, linewidth=1, antialiased=False, rstride=1, cstride=1)
  ax1.set_zlim(-80,30)
  plt.title("t = "+str(t*0.005))
      #plt.title("iron file "+str(iron_filename)+" max:"+str(np.amax(Z_iron)))
      
      #plt.tight_layout()
      #plt.show()
      
fig = plt.figure(1)
#ax1 = fig.add_subplot(1,1,1)
ax1 = fig.gca(projection='3d')
plt.tight_layout()
animate(0)
      

# call the animator.  blit=True means only re-draw the parts that have changed.
anim = animation.FuncAnimation(fig, animate,
                               frames=1000, interval=20)
plt.show()

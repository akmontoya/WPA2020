* Encoding: UTF-8.
T-TEST PAIRS=comm_G WITH comm_I (PAIRED). 

T-TEST PAIRS=int_G WITH int_I (PAIRED). 

compute int_diff = int_G - int_I. 
compute comm_diff = comm_G - comm_I. 
compute comm_sum = comm_G+comm_I.
compute comm_sum = comm_G+comm_I - 8.325490. 
EXECUTE.  
regression dep = int_diff /method = enter comm_diff comm_sum. 

regression dep = int_diff /method = enter comm_I comm_G. 

regression dep = int_G /method = enter comm_G. 
regression dep = int_I /method = enter comm_I. 

MEMORE m = comm_G comm_I /y = int_G int_I /model = 1. 

MEMORE m = comm_I comm_G diff_I diff_G /y = int_I int_G. 

regression /dep = int_diff /method = enter order. 

MEMORE y = int_G int_I /w = order /model = 2. 

MEMORE w = order /y = int_G int_I /model = 2  /plot = 1. 

DATA LIST FREE/order YdiffHAT int_GHAT int_IHAT. 
 
BEGIN DATA. 
 
     1.0000     -.2826     3.1196     3.4022 
     2.0000      .9107     4.2321     3.3214 
 
END DATA. 
 
GRAPH/SCATTERPLOT = order WITH YdiffHAT. 
GRAPH/SCATTERPLOT = order WITH int_GHAT. 
GRAPH/SCATTERPLOT = order WITH int_IHAT


regression /dep = int_diff /method = enter grppref. 

MEMORE w = grppref /y = int_G int_I /model = 2 /jn = 1 /plot = 1. 

MEMORE m = grppref order /y = int_G int_I /model = 3. 
MEMORE m = grppref order /y = int_G int_I /model = 2. 

**Wide to Long. 
VARSTOCASES
  /MAKE interest FROM int_I int_G
  /MAKE comm FROM comm_I comm_G
  /MAKE difficult FROM diff_I diff_G
  /INDEX=Index1(2) 
  /KEEP=Subject Group_Position pers_comm
  /NULL=KEEP.


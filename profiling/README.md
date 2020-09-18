# Maskies Profiling
## A profiling-enabled compilation of the Maskies game
### because it's a pain switching back and forth.
  
To compile the executable, download the contents from the maskies directory (that includes the profiling directory which is the parent of this read-me).   

You need:  
- GHC 8.8.3 or newer,   
- Cabal 3.2.0 or newer.  
  
At a terminal prompt in a Unix or Linux based computer, type the following to compile the executable:  
```bash:  
$ cabal build
```  
  
If successful, it will place the executable "Maskies-Profiling" in a sub directory buried deep in the new ./dist-newstyle/ folder. You can copy it out of there to a location of your choosing. And then run it using this terminal command:
```bash:  
$ ./Maskies-Profiling +RTS -sstderr  
```  
  
It will run the game, and you can play it. When you quit the game or reach its end, a barf of performance measures will get puked all over your screen. An explanation of all that can be found here:  
https://wiki.haskell.org/Performance/GHC#Measuring_performance  

It will also create 2 new files:  
- Maskies-Profiling.prof  
- Maskies-Profiling.hp  

Both contain profiling and measuring results of various metrics that are interesting if you're looking to optimize memory and CPU usage. 
  
I, the author, intend to add these files to source control, so I can see differences over time. 

This is all new territory to me. Your feedback, advice, and help, is welcome.

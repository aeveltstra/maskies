# Maskie's Ice Cream Parlor  
## A text adventure horror survival game.  
### Try not to die.  

To play the game, you can try and run the executable "Maskies" that is available here. Chances are it won't run. If so, build the game from source.

To build this game on your own Linux/Unix machine, you need:
- GHC 8.8.3 or newer
- Cabal 3.2.0 or newer

Download the source code (everything here) into a folder of your choosing and run the following at a terminal prompt:
```bash:
maskies:$ cabal install Maskies
```

This will resolve and download any dependencies the game source code needs to compile, compile it, and create an executable named "Maskies", and symlink it so you can run it from anywhere.

Then you can from a terminal prompt:
```bash:
~:$ Maskies
```

If compiling fails, reach out to me, the author.  
- Mastodon: @aeveltstra@mastodon.social
- Email: aev@sdf.org

And then you can copy the progam Maskies to wherever you want, CD into its folder, and play the game with this terminal prompt command:  
```bash:
$ ./Maskies  
``` 
A suite of tests is available in:  
maskies/src/test/  

Good luck.

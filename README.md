# Maskie's Ice Cream Parlor  
## A text adventure horror survival game.  
### Try not to die.  

To build this game on your own Linux/Unix machine, you need:
- GHC 8.8.3 or newer
- Cabal 3.2.0 or newer

Download the source code into a folder of your choosing and run the following at a terminal prompt to download the game code's dependencies, and to make sure you can compile it:  
```bash:
maskies/src/main/haskell:$ cabal build  
```

If it compiles correctly, you can build it with GHC using the following command at a terminal prompt:  
```bash:
maskies/src/main/haskell:$ ghc --make -O -o Maskies ./Main.hs -package random  
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

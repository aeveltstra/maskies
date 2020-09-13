### Maskie's Ice Cream Parlor  
## A text adventure horror survival game.  
# Try not to die.  

To build this game on your own Linux/Unix machine, you need:
- GHC 8.8.3 or newer
- Cabal 3.2.0 or newer

Download the source code into a folder of your choosing and run the following at a terminal prompt to download the game code's dependencies, and to make sure you can compile it:  
```bash:
$ cabal build  
```

If it compiles correctly, you can build it with GHC using the following command at a terminal prompt:  
```bash:
$ ghc --make -O -o Maskies ./Main.hs -package random  
```

If compiling fails, reach out to me, the author.  
- Mastodon: @aeveltstra@mastodon.social
- Email: aev@sdf.org


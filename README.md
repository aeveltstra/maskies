# Maskie's Ice Cream Parlor  
## A text adventure horror survival game.  
### Try not to die.  

## The programming has yet to complete 
This game is not finished. You are welcome to try it out and play it to its end... but you will find it just stops and breaks at some point. Or at several points. Check back for updates! 


## Starting the game

### On MS Windows:

To play the game, you can copy the executable "Maskies.exe" that is available here. It got compiled and tested on Microsoft Windows 7. It should work on any newer version too. 

If asked to enter a letter, confirm your entry by pressing the Enter or Return button.

This game is a text adventure. It runs in the command prompt. Start a command prompt, navigate to the folder in which you placed Maskies.exe, and start the game. For instance, if you downloaded the Maskies.exe executable into your Downloads folder, you can start it using a command prompt like this:

```batch:
%userprofile%\Downloads\> Maskies
```

### On Linux and Unix systems:

To play the game, you can try and run the executable "Maskies" that is available here. Chances are it won't run. If so, build the game from source.

To build this game on your own Linux/Unix machine, you need:
- GHC 8.8.3 or newer
- Cabal 3.2.0 or newer

Download the source code (everything here) into a folder of your choosing and run the following at a terminal prompt:
```bash:
trezjrtrove:$ cabal install Maskies
```

This will resolve and download any dependencies the game source code needs to compile, compile it, and create an executable named "Maskies", and symlink it so you can run it from anywhere.

Then you can from a terminal prompt:
```bash:
~:$ Maskies
```


## Testing

A suite of tests is available in:  
maskies/src/test/  

Good luck.


## For feedback and support, reach out to Treasure Trove Studio:

- Fediverse: [@trezjrtrove @pleroma.site](https://pleroma.site/@trezjrtrove)
- Email: [trezjrtrove@protonmail.com](mailto:trezjrtrove@protonmail.com)
- Twitter: [@trezjrtrove](https://twitter.com/trezjrtrove/)


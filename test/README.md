# Testing  
  
The game is a text adventure. It expects repeated key presses, and responds with texts. We can use its stdin to give it key presses. I have chosen to call these "wires", as the stages of the game are wired together. Thus, to test the game, I have provided a few wires. These should lead through specific known stages, to a specific expected outcome. None of the wires should make the game crash.  
  
Some stages are not yet wired up. At the moment of writing this document, it was not yet clear how all of the game would be programmed. As the game nears completion, more and more wires will be added and will need to get tested.  
  
To use a wire, we can have a Linux command prompt read its contents, and send it to Maskies' stdin, like so:  
  
```
$> cat ./wire-1.txt | ../Maskies  
```

It would be nice to have / create program that automates this, and inspects the resulting stdout.
  
---
  
# Wires  
  
1. wire-1: reach the end of night 1, then quit. 
2. wire-2: reach night 2, open the letter, then quit.
3. wire-3: reach night 2, die in the dark storage closet, then quit.
4. wire-4: reach night 2, visit the toilets, die in the hallway, then quit.
5. wire-5: reach night 2, visit the parlor, die there, then quit.
6. wire-6: reach night 2, find the lantern, die from food poisoning, then quit.
7. wire-7: reach night 2, find the lantern, survive the food poisoning, then quit.
8. wire-8: reach night 2, find the lantern, die from courage, then quit.
9. wire-9: reach night 2, find the lantern, find the letter, then quit.

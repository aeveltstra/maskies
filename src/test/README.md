# Testing  
  
The game is a text adventure. It expects repeated key presses, and responds with texts. We can use its stdin to give it key presses. I have chosen to call these "wires", as the stages of the game are wired together. Thus, to test the game, I have provided a few wires. These should lead through specific known stages, to a specific expected outcome. None of the wires should make the game crash.  
  
Some stages are not yet wired up. At the moment of writing this document, it was not yet clear how all of the game would be programmed. As the game nears completion, more and more wires will be added and will need to get tested.  
  
To use a wire, we can have a Linux command prompt read its contents, and send it to Maskies' stdin, like so:  
  
```bash:
$ cat ./resources/wire-1.txt | ../../Maskies  
```  
  
or like this:  
```bash:  
$ ../../Maskies < ./resources/wire-1.txt  
```  

If you symlinked the game executable, it may be available globally, so you can omit the relative directory-ups:  
```bash:  
$ Maskies < ./resources/wire-1.txt  
```  
  
We have a program named MaskiesWireTest that accepts the output of the above command as input. We also need to tell it which wire we passed in. Like so:

```bash:
$ cat ./resources/wire-1.txt | ../../Maskies | tail -7 | ./MaskiesWireTest "wire-1"  
```

That test will return either SUCCESS or FAILURE.

To run all wire tests, execute the "test" bash script. 
  
---

# Name validations

1. name-1.txt: Provides a paragraph of text instead of  a name. The game should recognize this, substitute it for a shorter name. Then quit.
2. name-2.txt: Provides nothing but spaces instead of a name. The game should recognize this, substitute it for a short name. Then quit.
3. name-3.txt: Provides a name that contains the number pi with a bunch of decimals. One of the game testers entered that. The game should recognize this, substitute it for "Pie". Then quit.
4. name-4.txt: Provides nothing for a name. The game should recognize that and assign the player a name. Then quit.
5. name-5.txt: Provides a perfectly good name. The game should recognize that and let the player keep it. Then quit.

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
10. wire-10: reach night 2, find the lantern, ignore the supplies, die due to not having anything to protect you, then quit.
11. wire-11: reach night 2, view the video, report it to the boss, get ridiculed, then start night 3.
12. wire-12: reach night 3, read the boss' letter, then quit.
13. wire-13: reach night 3 with only the memory stick but not having seen its video, skip Masky's briefing, start the obstacle course, then quit.

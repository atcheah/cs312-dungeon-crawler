# Dungeon Crawler

Dungeon Crawler is a game where you explore a dungeon, alternating between fighting monsters and leveling up. The project was made using the Gloss API (https://hackage.haskell.org/package/gloss)

## GAME OVERVIEW
Players are allowed to choose their own stats (HP, DMG, Bleed, Life Steal, Priority), and then face against a default monster.
The player fights the monster in a basic animation, and then either wins or loses. 
If they win, they progress to a level-up screen and choose a stat to increase. After leveling up, the player progresses and fights a new monster stronger than before. 
If they lose, the game ends when the player loses a fight. 

## INSTALLATION GUIDE
To install Gloss try running
\#
cabal install --lib gloss
\#

And to run the game
\#
ghci -fno-ghci-sandbox
:load Main
main
\#
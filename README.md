# perpetual-haskelling-initiative
Main repository for the CS141 Perpetual Haskelling Initiative

# Rules
Game consists of two 'players', which are represented in game by 'programming languages'
At the start of a game, both players start with 4 cards and 30 'health' points.
Turns alternate between the two players. A players turn ends when they end their turn or a time runs out. (time tbd)

On a given players turn, they start by drawing a card from their deck.
They can then proceed to play cards from their hand and 'attack' the opponent.
Each card will have a 'point cost'. As the number of rounds increases the number of 'points' a player can spend by playing cards increases to a maximum of 10 'points'

Once a players 'health' depleats to 0, the other player wins the game.

If their deck runs out of cards, instead of drawing a card at the start of a turn they take damage.
 -- This damage should increase as the number of turns with an empty deck increases (exact numbders tbd)

# Decks
Decks can be constructed by the user, but every deck consists of two main parts: 
- Cards from a BASE DECK
- Cards from an EXPANSION

Cards from a BASE DECK are focussed on the behaviours of programming paradigms
Cards from an EXPANSION are focussed on the behaviours of specific programming languages
# Card Types
There are _ many card types
- Programs (minions)
- Events (Spells)
- Errors (Negative result from a spell)

Programs and Events are playabe, whereas Errors occur as a result of a failed Event. 

Programs act as standard minions would in hearthstone. They have an 'attack' and 'health' rating, and are present on the board until 'killed' or removed
Events are single use items which have some effect on the current state of the game. 
Errors may be either a Program or Event, but work against the player which caused the Error
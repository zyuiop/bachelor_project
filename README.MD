# Engine

A Computer Science undergraduate project, written in spring 2019.

## Goals

The initial goal of the project was to build a text-based game based on real historic facts. The idea was to "generate" the
game automatically, using a large dataset of events and historical characters. However, this proved to be too difficult
in the current state of research. We therefore adopted a new idea.

The project instead focused on building a game in which the main character, the player, could control other characters, to change
their timeline.

## Engine concepts

The engine was made to allow anyone to write their own games with little code. It has a few important components.

### The levels

A game is made of multiple levels. When the player plays the game, he first plays the first level, then the second, than
the third, and etc until there is no more level to play.

When a level start, all the entities of the level are instantiated and spawned, and the clock starts ticking. The player
is presented with a small message, providing him the context and the situation.

The level defines two conditions: the success condition, and the failure condition. These conditions are evaluated after 
each player turn. If the success condition is met, then the game moves to the next level, after displaying a success text
to the player. If the failure condition is met, however, the level restarts from the beggining, and the player has to find 
what he can do to win.

### The map

The spacial dimension of the level is provided by the map. It is similar to a graph: the vertices of the graph are the
`rooms` and the edges are the `doors`. 

A `room` is a place, in which there can be items, characters, and where the action 
happens. A room has a descriptive text (an ambient) that is shown to the player when he enters. 
 
A `door` is a connexion between two `rooms`. The player simply crosses it to go from a room to an other. It can be locked:
if so, some conditions need to be met in order for the player to be able to cross the door.

### The scheduler

In this game, actions take some _time_. The central part of the engine that allows to keep track of the time is the scheduler.
Any action that needs to be ran in the future, or repeated regularly, is registered to the scheduler, that ensures that all
actions are ran at the appropriate time.

Player actions take some time, in general a few seconds. A small pause is created by the scheduler to simulate some time
passing. Indeed, the time goes faster in the game. We don't want a game hour to last an actual real hour.

### The characters   

The characters are all the "living" things of the game, player included. These are divided in two main categories. The NPCs
(non playable characters), and the PPCs (potentially playable characters). Both have a program (a routine they execute that
defines their behaviour) as well as triggers (some parts of the program that are ran when a specific event happens), but
the formers can also be controlled by the player. This means that the player can stop their program from running and just
control all their moves.

The main idea of the game will therefore be to use some characters to solve puzzles that you wouldn't be able to solve
on your own. 

## Technical specification

### Defining levels

#### File structure

All the levels are in the `data` folder.
Each level has its own folder, named `level-<i>` where `i` is the position of the level (level 1, level 2, ...).
A level directory contains multiple files, that will all be parsed at once as a single long file.

#### File content

The file syntax is quite simple. Usually, you define some kind of entity, using the following syntax:

```
[someEntity]

key = "value"
intKey = 12

listKey.0 = "A value"
listKey.1 = "an other value"

mapKey.hello = "World"

inventory.item = 12
```

There is, however, a special type of entity: the program. A program is defined as follow:

``` 
[programStart]

...The content of the program...

[programEnd]
```

#### Entities types

A level contains multiple types of entities, some of them being mandatory.

#### `level`

It **MUST** be provided **EXACTLY ONCE**.

- `name` (string): The name of the level
- `currency` (string): The item that will be used as a currency 
- `levelFailure` (string): The conditions for the level to fail
- `levelSuccess` (string): The conditions for the level to succeed
- `startTime` (int): The item that will be used as a currency 
- `startText` (opt list): A text that will be printed when the level begins 
- `endText` (opt list): A text that will be printed when the player successfully completes the level

The conditions syntax is the same than in programs. The environment provided is:
- `time` (int): the current time of the day, in seconds
- `totalTime` (int): the total time of the level, in seconds
- `characters` (map): all the characters of the game, mapped by their display name. The player has an alias (`characters.player`)
- `rooms` (map): all the rooms, mapped by their id
- `state` (object): the state of the game

#### `player`

It **MUST** be provided **EXACTLY ONCE**.

- `room` (string): The id of the room in which the player will spawn
- `inv` (inventory): The items that the player inventory contains at the begining of the level. 

#### `doortype`

- `name` (string): the name of the doortype (eg: door, stairs, portal, ...)
- `leftToRight` (list): the messages to print when the player goes from the left side of the door to the right side. If multiple messages are provided, one is picked at random each time.
- `rightToLeft` (opt list): same, but in the other direction. If no message is provided, `leftToRight` is used instead.

#### `room`

- `id` (string): the identifier of the room, to use with doors and characters position for example
- `name` (string): the name of the room, that will be displayed to the user
- `ambient` (string): the ambient text, to print when the player enters the room
- `inv` (inventory): the items located on the floor of the room

A room can also contain multiple items. An item is something the characters can interact with.

##### `item`

- `type` (string): the type of item (see below)
- `position` (string): the position of the item in the room (Center or a cardinal orientation: South, East, ...)
- `name` (string): the name of the item, displayed to the player and used internally in programs

Depending on the type, there are more properties:
- `type == inventory`: the item will be an inventory holder (a cupboard, ...)
    - `inv` (inventory): the content of the held inventory
- `type == switch`: the item will be a switch, meaning an item that changes state when one interacts
    - `states` (map): all the states this switch can have, in order (if you have 4 states, transitions will always be 1 -> 2 -> 3 -> 4 -> 1 -> 2 ...). The values should be the text displayed when the switch takes the key state.
    - `id` (string): an id for the item, used in programs to find it easily. It should be unique.
    - `time` (optional int): the time (in seconds) it takes to use the switch
- `type == descriptive`: the item will simply show a text when used
    - `time` (optional int): the time (in seconds) it takes to use the switch
    - `lore` (string): the text to print when the item is used
    
#### `door`

A connexion between two rooms.

- `left` (string): the id of the room at the left of the door
- `right` (string): the id of the room at the right of the door
- `leftPos` (string): the position of the door in the left room (cardinal direction)
- `rightPos` (string): the position of the door in the right room (cardinal direction)
- `doorType` (string): the type of the door, defined via a `[doortype]` entity.
- `openCondition` (opt string): the condition to open the door, as in programs.

#### `routine` 

A message that is printed regularly. 

- `start` (int): the time at which the routine should run for the first time
- `repeat` (int): at each interval (in seconds) the routine should run
- `message` (string): the message to print each time the routine runs

#### `character`

A character in the game.

- `type` (string): the type of character (see beloew)
- `room` (string): the id of the room in which the character will spawn
- `position` (string, default Center): the position of the character in the room (Center or a cardinal orientation: South, East, ...)
- `name` (string): the name of the character, displayed to the player and used internally in programs
- `inv` (inventory): the inventory of the character

Types:
- `npc` / `ppc`: a programmable NPC that cannot (respectively can) be taken control of
  - `attr` (map): attributes to give to the entity (can be set and read from programs, doors, and level conditions)
  - must be followed by a program block (defined before), to describe the behaviour of the program
- `trader`: a NPC that trades items against the level currency
  - `price` (inventory): an inventory in which the items quantities actually represent the prices of the items. All the priced items must also be in the character inventory to be sold. 

# Engine

A Computer Science undergraduate project, written in spring 2019.

## Goals

The project focuses on building a game in which the main character, the player, can control other characters, to change
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
    - `states` (list): all the states this switch can have, in order (if you have 4 states, transitions will always be 1 -> 2 -> 3 -> 4 -> 1 -> 2 ...). 
    - `transitions` (map): the messages displayed when the state changes. The values should be the text displayed when the switch takes the key state.
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

## Programming

The NPCs are scripted using a custom DSL. The grammar of the language is:

```
    digit ::= ('0' | '1' | ... | '9')
    character ::= 'a' | 'b' | ... | 'z' | 'A' | ... | 'Z'
    intLitteral ::= digit+ | (digit digit ':' digit digit ':' digit digit)
    stringLitteral ::= ('"' [^"]+ '"') | ('"""' .+ '"""')
    booleanLitteral ::= 'true' | 'false'
    identifier ::= ((character | '_') (character | '_' | digit)*) | '`' [^`]+ '`'
    
    Identifier ::= identifier ('.' identifier)*
    Litteral ::= stringLitteral | intLitteral | booleanLitteral | 'null'
    
    Operation ::= Value ('+' | '-' | '/' | '*' | '%' | '==' | '!=' | '<=' | '<' | '>=' | '>' | 'in' | '||' | '&&') Value
    Value ::= Identifier | Literal | Operation | '(' Operand ')' | '!' Operand
    
    Ite ::= 'if' '(' Value ')' Expr ('else' Expr)?
    While ::= 'while' '(' Value ')' Expr
    Do ::= 'do' ('now')? Value
    Set ::= Identifier ':=' Value
    Block ::= '{' Expr* '}'
        
    Expr ::= Ite | Do | Set | While | Block
    
    When ::= 'when' '(' Value ')' ('(' intLitteral ')')? Expr
    On ::= 'on' '(' identifier ('||' identifier)* ')' ('(' intLitteral ')')? Expr

    Program = Expr | When | On
    
```

Here is the details on what each of these commands do.

### Structure of a program

A program, at its top level, is composed of expressions, `when` blocks, and `on` blocks. `when` and `on`, also
refered to as interrupts, can only be at the top of the program. All other constructs mentioned in this guide can be 
inside blocks or other constructs.

### Trigger actions when conditions are met: `when`

A `when` block looks like this:
```
when (condition) {
  expr 1
  expr 2
  ...
}

when (condition) expr

when (condition)(10) {
  expr 1
  expr 2
  ...
}
```

The condition is a value that can be resolved to a boolean (see below for value resolution). If, on a given tick, this 
boolean is true, the expression is ran. 
The second, optional, parameter defines the priority of the block. If multiple `when` are true at the same tick, they 
will be scheduled in order, highest priority first.

### React to external events: `on`

A `on` block looks like this:
```
on (Event) {
  expr 1
  expr 2
  ...
}

on (Event) expr

on (Event)(10) {
  expr 1
  expr 2
  ...
}
```

The Event is the name of an event. You can react to multiple events at once by separating the event names with `||`. For
example, to react to both taking control and releasing control, you can write `on (TakenControl || ReleasedControl)`.

The second, optional, parameter defines the priority of the block. If multiple `on` react to the same event, they 
will be scheduled in order, highest priority first.

### Interrupts management for `on` and `when`

The script runner keeps a stack of running states. When a `on` or `when` expression runs, branching occurs. It means the
current execution state (including the current running expression and the expressions that still have to be executed) 
are saved and added on the top of the stack. A new state is then initialized with the `on` or `when` expression.

If multiple interrupts trigger, the program will branch multiple times.

Interrupts are scheduled in order of priority. Lowest priority gets scheduled first, so it means it's executed at the end.

### Execute multiple expressions

A program is, by itself, a block. It means it can contain multiple expressions. But if you want to execute multiple 
expressions inside an interrupt, or an if, for example, you'll need to wrap them in a block.

A block looks like this:
```
{
 expr 1
 expr 2
 expr 3
 ...
}
```

A block is considered as a single expression, that, when executed, splits in multiple expressions.

### Execute an expression only if a condition is met: `if`

An `if` block looks like this:
```
if (condition) {
  expr 1
  expr 2
  ...
} else {
  expr 1
  ...
}
```

The brackets are optional if you only run a single expression.

The else block is optional.

This construct executes the expression if the condition is met (evaluates to `true`). If not, it
executes the else block (if present) or does nothing (if not).

### Execute an expression as long as a condition is met: `while`

A `while` block looks like this:
```
while (condition) {
  expr 1
  expr 2
  ...
}
```

It executes all the expressions inside it as long as the condition evaluates to `true`. 
The condition is tested before executing the block, but if it is true all the block is executed
before testing the condition again.

When the condition is false, the program moves to the next expression.

### Make the character do things: `do`

A `do` expression looks like this:
```
do now "say hello"
do "say I am " + characters.me.name
```
 
It takes a string value, the command, and runs it. A command takes some time to execute. If the `now` keyword
is present, this time will always be 0 (meaning the next expression will be executed the same tick). If not, the
NPC program is paused for `duration` ticks (where `duration` is the time it took to run the command).

A paused program can still react to interrupts (`when` and `on` clauses). The pause time is actually part of the state,
meaning that when the program branches, the pause time is set to 0 for the new branch but kept aside so that it can be 
recovered when coming back to the current branch.

### Set a variable to a value: `:=`

Example:
```
currentRoom := room.id
ready = true
nextMessage = "Hello !"
```

A `:=` operator sets a given identifier (left hand side) to a given value (right hand side).

The identifier must be writable. Currently, only attributes are writable.

### About values and conditions

Values were mentioned multiple times before. Here are a few things to know about them:
- There are multiple types of values: `integers`, `strings`, `booleans`, `sets` and `nulls`
    - `integers` are numbers. They can also be specified using a time construct: `hh:mm:ss` (evaluates to `hh * 3600 + mm * 60 + ss`)
    - `strings` are strings, specified using `"some text"` or `"""some text"""` if you need to be able to use the `"` character in your string
    - `booleans` are specified as `true` or `false`
    - `sets` are sets of values, they cannot be specified in the code but can be resolved
    - `nulls` represent the absence of value, they are specified as `null` in the code
- A condition is a value that evaluates to `boolean`

All operation results are values.
Here are all the operators and their effect:
- `int / int` evaluates to `int` the division of the left argument by the right argument
- `int * int` evaluates to `int` the product of the left argument by the right argument
- `int % int` evaluates to `int` the module of the left argument by the right argument
- `int + int` evaluates to `int` the sum of the left argument and the right argument
- `string|int + string` evaluates to `string` the concatenation of the left argument and the right argument
- `string + string|int` evaluates to `string` the concatenation of the left argument and the right argument
- `int - int` evaluates to `int` the difference of the left argument and the right argument

- `value == value` evaluates to `true` if the values are equal and `false` if not 
- `value != value` evaluates to `false` if the values are equal and `true` if not 
- `int <= int` evaluates to `true` if left <= right, `false` if not
- `int < int` evaluates to `true` if left < right, `false` if not
- `int > int` evaluates to `true` if left > right, `false` if not
- `int >= int` evaluates to `true` if left >= right, `false` if not
- `string|set in string|set` evaluates to `true` if left appears in right

- `boolean && boolean` evaluates to `true` if both left and right are true
- `boolean || boolean` evaluates to `true` if either left or right is true
- `! boolean` evaluates to `false` if the argument is `true`, and `true` if the argument is `false`

Identifiers (or "variables") are evaluated to values depending on the execution context (or environment).
Their type can be moving (example: "10" can be evaluated as both a string or an int).

### Execution context

The execution context can be seen as a tree. To access a given node, you will use the following syntax:
`first_node.second_node.leaf`.

Some node names contain spaces or special characters. You can refer to them using the \` character.
```first_node.`Space Node Name`.leaf```

Some nodes can be represented as lists. To access a given element in a list, use the following syntax:
```list_node.`0````. You can indeed replace 0 with any index in the list.

If a given value doesn't exist, it will be resolved to `null`. Beware, as `null` values can cause the program to crash
if they are used in integer comparisons, for example. 

Also note that a node can also be interpreted as a list of its children. For example, if we have the following tree:
```
node
 |       |      |
leaf1  leaf2  child1
                |
              leaf3
```
Then `node` will resolve to a list with `leaf1`, `leaf2` and `child1`. Therefore, `"leaf1" in node` resolves to `true`, 
and `"leaf3" in node` resolves to false.

#### Common pieces

Some node types are redundant in the environments. Here are their main properties:
- `inventory`: a node with one leaf per item in the inventory. The leaf value will be the amount available.
- `character`: represents a character in the game
     - `name` (`string`): the name of the character
     - `attributes`: a node with only leaves, that can be freely set. See the attributes as variables of a character.
     - `inventory` (`inventory`): the entity inventory
     - `currentRoom` (`room`): the room the entity is in
     - `currentPosition` (`string`): the current position of the entity in the room (cardinal point)
     - `currentOpenInventory` (`list`): a list, containing either 0 or 1 inventory, corresponding to the inventory the character is interacting with
     - `activeRequests` (`list`): a list of `message`, the requests not yet answered
- `room`: a room in the game
     - `inventory` (`inventory`): the room inventory (the items on the floor, basically)
     - `id` (`string`): the identifier of the room
     - `name` (`string`): the name of the room
     - `ambient` (`string`): the ambient text of the room
     - `describe` (`string`): the complete description of the room
     - `switches`: a node containing a leaf for each switch in the room. The leaf contains the switch value, and not the switch itself.
- `message`: a signal sent between characters.
     - `__name` (`string`): the name of the message 
     - There are different types of `message`. Here is their names, that can be used for the `on` interrupt and compared with the `__name` property.
          - `TalkingMessage`: somebody in the room talked
               - `sentBy` (`character`): the character that sent the message
               - `content` (`string`): the message sent
          - `SystemMessage`: a message sent by the system (i.e. "The sun rises", ...)
               - `content` (`string`): the message sent
          - `RoomMovement`: somebody entered or left the room
               - `sentBy` (`character`): the character that entered or left
               - `entering` (`boolean`): if `true` the character entered the room, if `false` it left.
          - `TakenControl` or `ReleasedControl`: a player took/released control of this entity
               - `sentBy` (`character`): the player who took/released control
          - `SwitchChangeState`: a switch in the room changed state
               - `switchId` (`string`): the id of the switch
               - `sourceState` (`string`): the previous state of the switch
               - `targetState` (`string`): the new state of the switch
          - `InventoryTradeRequest`: a trade request
               - `sentBy` (`character`): the character that sent the request
               - `target` (`character`): the character that received the request
               - `sentItems` (`inventory`): the items that `sentBy` wants to give to `target`
               - `requestedItems` (`inventory`): the items that `sentBy` wants `target` to provide
          - `TradeRequestResult`: the result of a trade request
               - `request` (`InventoryTradeRequest`): the initial request
               - `success` (`boolean`): `true` if the trade was accepted, `false` if it wasn't

The following nodes are present in all the environments
 
- `time` (`int`): the current day-time of the level, in seconds (0h00 maps to 0, 23h59:59 maps to 86'399 and is the
maximum value)
- `totalTime` (`int`): the total time of the level, namely all the seconds elapsed since the day 1 of the level at 0h00:00
- `state`: the state of the game _(some additional properties are not documented)_
     - `currency`: the item used as the currency in the game
     - `level`: the map _(some additional properties are not documented)_
        - `rooms`: a map of the rooms, where the key is the id of the room and the value is a `room`
     - `registry`: a registry of all the characters in the game
          - `player` (`character`): the player
          - `entities`: a list of all the characters in the game
 
#### Character environment

- `characters`: a map of all the characters of the room. To access a given character, access `characters.characterName`.
    - `characters.me` is a shortcut to the current character
    - `characters.player` is a shortcut to the player, even if he is not in the room
- `room`: the room details
- all the attributes of the character are mapped directly to the top level
- if you are in a `on` block, the currently ran interrupt is in the `trigger` node and is a `message`.

#### Condition environment

This environment runs all the conditions defined in the level files (success, failure, door opening).

- `characters`: a map of all the characters of the game. To access a given character, access `characters.characterName`.
    - `characters.player` is a shortcut to the player
- `rooms`: a map of all the rooms of the game. To access a given room, access `rooms.idOfTheRoom`

If the condition is relative to a door opening, the character trying to open the door is mapped to `self`
    
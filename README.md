# Game-server
A server to allow people to write AI that will compete against each other.

For an AI to connect to a server, you connect to games.recurse.com on
the port specified by the game. If the game supports flags, the first
line of input you send to the server should be a list of the flags you
want. Flags allow you to customize how you send/receive the I/O. Flags
allow you to customize the format of the I/O. All of the flags
supported by a game and their descriptions are listed after the
game. Once the game starts, each player will be sent their player
number, starting from 1.

## Tic-Tac-Toe: Port 7000

Regular old Tic-Tac-Toe.

### Flags

No flags are supported for this game. To send in your move, send "row
col" on its own line. Once your opponent makes their move, you will
receive it as "row col" on its own line. This game is zero indexed. If
you are going first you are expected to immediately send the move. If
you are going second you are expected to wait for your opponent to
make a move and then you send yours in.

## [Super-Tic-Tac-Toe](http://www.scheinerman.net/jonah/supertictactoe.html): Port 8000

Like regular Tic-Tac-Toe, only with more strategy.

### Flags

For sending in your input, you can either send in just "inner-row
inner-col" or send in "outer-row outer-col inner-row inner-col" on its
own line. If you get to choose the board you are playing on, you must
use the second format. This game is zero indexed.

##### Always-Four

With this flag set, you will always receive "inner-row inner-col
outer-row outer-col" of your opponents move.

##### Maybe-Four

With this flag set, you will receive "inner-row inner-col" as long as
your opponent does not get to choose the board they are playing on. If
they do get to choose, you will receive "outer-row outer-col inner-row
inner-col".
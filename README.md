# Game-server
A server to allow people to write AI that will compete against each other.

For an AI to connect to the server, run the rcgames script included in
this repository followed by the name of the game and then the shell
commands to run your AI. Your AI will receive the I/O from the server
as standard input and standard output. Just make sure that your AI
doesn't include any kind of banner when it starts up.

If the game supports flags, the first line of input you send to the
server should be a list of the flags you want. Flags allow you to
customize the format of the I/O. All of the flags supported by a game
and their descriptions are listed after the game. Once the game
starts, each player will be sent their player number, starting from
1. If you are player one you are expected to sumbit your
move. Otherwise you are expected to wait for the other player(s) to
sumbit their move(s) and then respond with your own.

## Tic-Tac-Toe

Regular old Tic-Tac-Toe.

### Flags

No flags are supported for this game. To send in your move, send "row
col" on its own line. Once your opponent makes their move, you will
receive it as "row col" on its own line. This game is zero indexed.

## [Super-Tic-Tac-Toe](http://www.scheinerman.net/jonah/supertictactoe.html)

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

## Checkers

This is Checkers where you have to capture a piece if you are able to.

### Flags

Player 1 is on the top of the board and player 2 is on the bottom. The
input and output format is a list of squares seperated by a hypen
('-'). If a piece moves only a single square, you list the starting
square followed up by the ending square. If a piece is jumping over
other pieces, each square the piece lands on along the way will be
part of the list.

##### Traditional

For this format each square is represented as a number between 1 and
32. To see the mapping look at
[this](http://media-2.web.britannica.com/eb-media/43/57543-004-1F90064A.jpg)
image.

##### Algebraic

This is where each square is represented by its row and column using a
letter for the column and a number for the row. For example the square
"a2" is the second square in the bottom row.
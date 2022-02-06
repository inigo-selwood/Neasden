# Neasden

**Neasden:** A stop on the London Underground, and a word which alliterates with 'Noughts and Crosses'

An implementation of the classic game of Noughts and Crosses, written in F#.

## Usage

```
dotnet run Neasden.fsproj
```

Ensure you have the [latest dotnet runtime environment](https://dotnet.microsoft.com/en-us/download/dotnet/6.0) installed prior to running.

## Principles

The next move is one which satisfies the first of the following:

1. Win, if possible
2. Block the player from winning
3. Fork (create more than one opportunity for winning)
3. Block the opponent from forking
4. Take the centre cell
5. Take the opposite corner from one occupied by the opponent
6. Occupy an empty corner
7. Occupy an empty side

The board is represented by 9 cells with a value in the range `(-1, 0, 1)`, where `0` means empty, and `(1, -1)` represent the two players.

This way, any axis on the board with a sum of `±3` is won/lost, and one with a sum `±2` is a fork.

The board is stored as a list of 9 elements, where each element corresponds to a cell. The index is calculated by flattening in [column-major](https://en.wikipedia.org/wiki/Row-_and_column-major_order) order.

See the implementation in [Board.fs](source\Board.fs).

## Future improvements

- Add a rubrick, and a turn counter to the interface
- Add colours using ANSI embed codes

# PostgreTETRIS: Tetris in PostgreSQL
Implements a simplified version of Tetris using PostgreSQL. When run in an empty database, 
the `tetris.sql` script file generates the tables, views, functions, and procedures needed for the Tetris application. 
Once the script has been run, the "reset" procedure must be called in order to prepare the components and print 
the blank playfield. To place a piece, you insert a (pos, orient) tuple into the "moves" table using `CALL place(pos, orient)`, 
where "pos" is an integer from 1 to 10 that marks the leftmost column to place the piece, and "orient" is an integer that 
determines the rotation of the piece.

An explanation of the code and a more detailed explanation of the application can be found in the `walkthrough.md` markdown
file.

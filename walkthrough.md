# Playing Tetris in PostgreSQL

This notebook describes how a form of Tetris can be implemented in PostgreSQL, using code written in the PL/pgSQL scripting language. The object definitions are all contained in the `tetris.sql` file, which can be executed by running `\i tetris.sql` in psql after connecting to an empty database.

## Table structure

The data needed for the Tetris application to function is stored in five tables: `moves`, `lines`, `piece_log`, `board`, and `pieces`. The table creation statements for the first four tables are given below:


```python
CREATE TABLE IF NOT EXISTS moves (
    pos INT,
    orient INT
);

CREATE TABLE IF NOT EXISTS lines (
    num_lines INT,
    num_cleared INT
);

CREATE TABLE IF NOT EXISTS piece_log (piece_name)
    AS SELECT 'blank';
    
CREATE TABLE IF NOT EXISTS board (
    height INT,
    width INT,
    state BOOL DEFAULT 'false'
);
```

The `moves` table is the only one that the player interacts with directly, and has a column for the position of the Tetris piece (measured from its leftmost block) and its orientation. These are the two pieces of information that the player most provide to make a move, a process which is described in more detail later. The `lines` table is used to keep track of the number (column `num_cleared`) and type (column `num_lines`) of each line clear that occurs, and serves as the effective scoreboard of the game. The `piece_log` is used to keep track of the current piece, and only ever contains a single value. Finally, the highly dynamic `board` table keeps track of where pieces are placed, and lies at the heart of the application. Each row in the table corresponds to a specific tile in the 20x10 playfield, with the `height` and `width` columns defining its exact position. The `state` boolean column stores whether the tile is currently occupied by a piece block, and thus plays a key role in determining where the player can drop pieces and which lines need to be cleared. Upon calling the `reset` procedure (described later), the `board` table is populated using the following INSERT statement:


```python
INSERT INTO board (height, width) 
    SELECT * 
    FROM generate_series(1, 20) t1 
        CROSS JOIN generate_series(1, 10) t2;
```

which generates all 200 rows with a default state of `false`, indicating that the associated tile is unoccupied. The placement of a piece is performed by simply setting the state of the affected rows to `true`.

The last table to discuss is the `pieces` table, which is unique in that it is not altered in any manner after creation. This is because it holds immutable information about the seven Tetris pieces, laid down when Tetris was first invented in the eighties. The code to create the `pieces` table and define its contents is given below:


```python
CREATE TABLE IF NOT EXISTS pieces (
    name, orient, b1, b2, b3, b4, 
        prev1, prev2, prev3, prev4)
AS VALUES
    ('sq', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [1,0], ARRAY [1,1], 
         'X X', 'X X', '   ', '   '),
    ('i', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [0,2], ARRAY [0,3], 
         'X X X X', '       ', '       ', '       '),
    ('i', 2, ARRAY [-1,0], ARRAY [0,0], ARRAY [1,0], ARRAY [2,0], 
         'X', 'X', 'X', 'X'),
    ('t', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [1,1], ARRAY [0,2], 
         'X X X', '  X  ', '     ', '     '),
    ('t', 2, ARRAY [0,0], ARRAY [0,1], ARRAY [1,1], ARRAY [-1,1], 
         '  X', 'X X', '  X', '   '),
    ('t', 3, ARRAY [0,0], ARRAY [0,1], ARRAY [-1,1], ARRAY [0,2], 
         '  X  ', 'X X X', '     ', '     '),
    ('t', 4, ARRAY [0,0], ARRAY [0,1], ARRAY [-1,0], ARRAY [1,0], 
         'X  ', 'X X', 'X  ', '   '),
    ('zr', 1, ARRAY [1,0], ARRAY [1,1], ARRAY [0,1], ARRAY [0,2], 
         '  X X', 'X X  ', '     ', '     '),
    ('zr', 2, ARRAY [0,0], ARRAY [-1,0], ARRAY [0,1], ARRAY [1,1], 
         'X  ', 'X X', '  X', '   '),
    ('zl', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [1,1], ARRAY [1,2], 
         'X X  ', '  X X', '     ', '     '),
    ('zl', 2, ARRAY [0,0], ARRAY [1,0], ARRAY [0,1], ARRAY [-1,1], 
         '  X', 'X X', 'X  ', '   '),
    ('lr', 1, ARRAY [0,0], ARRAY [1,0], ARRAY [0,1], ARRAY [0,2], 
         'X X X', 'X    ', '     ', '     '),
    ('lr', 2, ARRAY [0,1], ARRAY [-1,1], ARRAY [-1,0], ARRAY [1,1], 
         'X X', '  X', '  X', '   '),
    ('lr', 3, ARRAY [0,0], ARRAY [0,1], ARRAY [0,2], ARRAY [-1,2], 
         '    X', 'X X X',  '     ', '     '),
    ('lr', 4, ARRAY [0,0], ARRAY [-1,0], ARRAY [1,0], ARRAY [1,1], 
         'X  ', 'X  ', 'X X', '   '),
    ('ll', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [0,2], ARRAY [1,2], 
         'X X X', '    X', '     ', '     '),
    ('ll', 2, ARRAY [1,0], ARRAY [1,1], ARRAY [0,1], ARRAY [-1,1], 
         '  X', '  X', 'X X', '   '),
    ('ll', 3, ARRAY [0,0], ARRAY [-1,0], ARRAY [0,1], ARRAY [0,2], 
         'X    ', 'X X X', '     ', '     '),
    ('ll', 4, ARRAY [0,0], ARRAY [-1,0], ARRAY [-1,1], ARRAY [1,0], 
         'X X', 'X  ', 'X  ', '   ');   
```

Each row in the table describes a specific piece (given by the `name` column) in a specific orientation (given by the `orient` column), with information about the relative positions of the four blocks (columns `b1`, `b2`, `b3`. and `b4`) and the text strings needed to properly illustrate the pieces (columns `prev1`, `prev2`, `prev3`, and `prev4`, to be discussed more later). The block positions are each provided as a two-element integer array, with the first integer giving its relative height and the second giving its relative width. These values are taken relative to the block assigned `[0, 0]`, which is always in the leftmost position (although at varying heights). Because of the way the board is configured, a smaller height value actually indicates a piece that is further up the playfield (i.e. closer to the top from the perspective of the player), with pieces appearing initiallity a height of 1 and then falling into place at some larger value (maximum of 20).

## Starting a game

To start a new game of Tetris, the move list and line counts must be reset, and all pieces from the previous game must be cleared from the playfield. This is done using the `reset` procedure (`CALL reset();`), which is defined below:


```python
CREATE OR REPLACE PROCEDURE reset()
LANGUAGE plpgsql
AS $$
    DECLARE
        col_index INT;
        row_index INT;
    BEGIN
        TRUNCATE TABLE moves;
        TRUNCATE TABLE board;
        TRUNCATE TABLE lines;
        INSERT INTO lines(num_lines, num_cleared) 
            VALUES (1, 0), (2, 0), (3, 0), (4, 0);
        INSERT INTO board (height, width) 
            SELECT * 
            FROM generate_series(1, 20) t1 
                CROSS JOIN generate_series(1, 10) t2;
        PERFORM prepare_next()
        COMMIT;
    END;
$$;
```

The procedure begins by truncating the `moves`, `board`, and `lines` tables, and then inserting in the appropriate starting values for the latter two tables. With tables now in a fresh state, a final function `prepare_next` is called which draws a random first piece and prepares the "graphical" interface for the player to use. The code for this function is given below:


```python
CREATE OR REPLACE FUNCTION prepare_next()
    RETURNS VOID
    LANGUAGE plpgsql
AS $$
    DECLARE
        piece_txt TEXT;
    BEGIN
        PERFORM print_board();
        PERFORM print_clears();
        RAISE NOTICE ' ';
        piece_txt = get_piece();
        PERFORM print_piece(piece_txt);
        UPDATE piece_log SET piece_name = piece_txt;
    END;
$$;
```

We can see that `prepare_next` first calls a pair of functions that print the current state of the playfield (empty at this point) and thes number of line clears (all zero). The code for these functions is given below: 


```python
CREATE OR REPLACE FUNCTION print_board()
    RETURNS VOID
    LANGUAGE plpgsql
AS $$
    DECLARE
        row_itr RECORD;
    BEGIN
        FOR row_itr IN 
            SELECT * FROM readable
        LOOP
            RAISE NOTICE '| % | % | % | % | % | % | % | % | % | % |', 
                row_itr.c1, row_itr.c2, row_itr.c3, row_itr.c4, row_itr.c5,
                row_itr.c6, row_itr.c7, row_itr.c8, row_itr.c9, row_itr.c10;
        END LOOP;
        RAISE NOTICE '  1   2   3   4   5   6   7   8   9   10'; 
    END;
$$;

CREATE OR REPLACE FUNCTION print_clears()
    RETURNS VOID
    LANGUAGE plpgsql
AS $$
    DECLARE
        lines INT[];
    BEGIN
        lines = array(SELECT num_cleared FROM lines ORDER BY num_lines);
        RAISE NOTICE 'Single: %  Double: %  Triple: %  Tetris: %',
            lines[1], lines[2], lines[3], lines[4];
    END;
$$;
```

As can be seen, the graphical display is produced by raising notices with appropriately-formatted strings. Since the `board` table is not structured to be especially readable, we intead employ a view called `readable` that constructs a pivot table whose columns are the columns of the playfield: 


```python
CREATE EXTENSION IF NOT EXISTS tablefunc;

CREATE OR REPLACE VIEW readable AS
    SELECT * FROM crosstab(
        'SELECT height, width, CASE WHEN state THEN ''X'' ELSE '' '' END AS val 
            FROM board ORDER BY 1,2') 
        AS t (height INT, c1 TEXT, c2 TEXT, c3 TEXT, c4 TEXT, c5 TEXT, 
              c6 TEXT, c7 TEXT, c8 TEXT, c9 TEXT, c10 TEXT);
```

Once the `board` table is repackaged in this more intuitive format, it can be easily displayed to the player by simply raising a notice for each row. Since the `crosstab` function is not available by default, we must import the `tablefunc` extension.

Looking back a the second half of the `prepare_next` function, we can see that it next calls the `get_piece` function, which returns a random piece name that will serve as the next piece to be placed. The code for this function is given below: 


```python
CREATE OR REPLACE FUNCTION get_piece()
    RETURNS TEXT
    LANGUAGE plpgsql
AS $$
    DECLARE
        piece TEXT;
    BEGIN
        SELECT CASE
            WHEN val BETWEEN 0 AND 1 THEN 'sq'
            WHEN val BETWEEN 1 AND 2 THEN 'i'
            WHEN val BETWEEN 2 AND 3 THEN 't'
            WHEN val BETWEEN 3 AND 4 THEN 'zl'
            WHEN val BETWEEN 4 AND 5 THEN 'zr'
            WHEN val BETWEEN 5 AND 6 THEN 'll'
            ELSE 'lr' END
        INTO piece
        FROM (VALUES (random() * 7)) AS t (val);
        RETURN piece;
    END;
$$;
```

While a bit verbose, this function operates in a simple manner by first sampling a random number uniformly between 0 and 7, and then returning a piece name based on which two integers it lies most tighlty between. Once the piece has been selected, `prepare_next` stores its name in the `piece_log` table (which is the table's only purpose) and then displays its shape to the player by calling the `print_piece` function. The code for this function is given below: 


```python
CREATE OR REPLACE FUNCTION print_piece(piece_name TEXT)
    RETURNS VOID
    LANGUAGE plpgsql
AS $$
    DECLARE
        piece_text1 RECORD;
        piece_text2 RECORD;
        piece_text3 RECORD;
        piece_text4 RECORD;
    BEGIN
        SELECT prev1, prev2, prev3, prev4 INTO piece_text1 FROM pieces 
            WHERE name = piece_name AND orient = 1;
        SELECT prev1, prev2, prev3, prev4 INTO piece_text2 FROM pieces 
            WHERE name = piece_name AND orient = 2;
        SELECT prev1, prev2, prev3, prev4 INTO piece_text3 FROM pieces 
            WHERE name = piece_name AND orient = 3;
        SELECT prev1, prev2, prev3, prev4 INTO piece_text4 FROM pieces 
            WHERE name = piece_name AND orient = 4;
        RAISE NOTICE 'Next piece:  %  |  %  |  %  |  %', 
            piece_text1.prev1, 
            COALESCE(piece_text2.prev1, ''), 
            COALESCE(piece_text3.prev1, ''), 
            COALESCE(piece_text4.prev1, '');
        RAISE NOTICE '             %  |  %  |  %  |  %',
            piece_text1.prev2, 
            COALESCE(piece_text2.prev2, ''), 
            COALESCE(piece_text3.prev2, ''), 
            COALESCE(piece_text4.prev2, '');
        RAISE NOTICE '             %  |  %  |  %  |  %',
            piece_text1.prev3, 
            COALESCE(piece_text2.prev3, ''), 
            COALESCE(piece_text3.prev3, ''), 
            COALESCE(piece_text4.prev3, '');
        RAISE NOTICE '             %  |  %  |  %  |  %',
            piece_text1.prev4, 
            COALESCE(piece_text2.prev4, ''), 
            COALESCE(piece_text3.prev4, ''), 
            COALESCE(piece_text4.prev4, '');
    END;
$$;
```

While the function is fairly long, it is really just repeating the same basic step. First, the `prev1`, `prev2`, `prev3`, and `prev4` strings for the desired piece are retrieved from the `pieces` table at a given orientation. The "prev" + *n* naming scheme simply indicates that the string serves as the *n*th row in the display graphic. Once this information has been retrieved for all orientations, the piece illustrations can be printed to the player. For pieces which don't have all orientations, any NULL values are replaced with an empty string before printing.

## Placing a piece

Once a new game has been started, the player can place a piece by performing an insertion into the `moves` table: `INSERT INTO moves (pos, orient) VALUES (int1, int2);`. The value of `int1`, which must be between 1 and 10, denotes the leftmost column that the piece will occupy. The value of `int2` specifies which orientation to place the piece. This value must be greater than zero but no larger than the maximum number of orientations for the piece, which can be determined by simply looking at the number of different illustration that are displayed. Once the orientation and horizontal position have been chosen, the piece will be dropped vertically onto the stack of previous pieces.

Internally, the process described above is carried out using a trigger on the `moves` table that is called before each row insertion. This trigger executes the `place` function, which is given below:


```python
CREATE OR REPLACE FUNCTION place()
    RETURNS TRIGGER
    LANGUAGE plpgsql
AS $$
    DECLARE
        pos_block BOOL;
        row_itr INT;
        row0 Int;
        col0 INT;
        collision BOOL;
        piece RECORD;
    BEGIN
        SELECT * INTO piece FROM pieces WHERE orient = NEW.orient AND 
            pieces.name = (SELECT piece_log.piece_name FROM piece_log);
        IF piece IS NULL THEN
            PERFORM print_board();
            RAISE NOTICE 'Piece does not have orientation %.', NEW.orient;
            PERFORM print_piece((SELECT piece_log.piece_name FROM piece_log));
            RETURN NULL;
        END IF;
        col0 = NEW.pos;
        FOR row_itr IN 1..21 LOOP
            collision = check_piece_collision(row_itr, col0, piece.b1, piece.b2, 
                                              piece.b3, piece.b4);
            row0 = row_itr - 1;
            EXIT WHEN collision;
        END LOOP;
        IF row0 = 0 THEN
            PERFORM print_board();
            RAISE NOTICE 'Piece cannot be placed.';
            PERFORM print_piece(piece.name);
            RETURN NULL;
        ELSE
            PERFORM add_piece(row0, col0, piece.b1, piece.b2, piece.b3, piece.b4);
            PERFORM clear_lines();
        END IF;
        PERFORM prepare_next();
        RETURN NEW;
    END;
$$;

CREATE OR REPLACE TRIGGER piece_placed BEFORE INSERT ON moves FOR EACH ROW
    EXECUTE PROCEDURE place();
```

The function begins by retrieving the necessary piece information and checking that the orientation requested by the player is valid. Next, it loops through the playfield rows starting from the top, and checks if the piece will collide with the existing piece stack at the given height. This collision checking is done using the `check_piece_collision` and `check_block_collision` functions, which are given below:


```python
CREATE OR REPLACE FUNCTION check_piece_collision(
    row0 INT, col0 INT, b1 INT[], b2 INT[], b3 INT[], b4 INT[])
    RETURNS BOOL
    LANGUAGE plpgsql
AS $$
    DECLARE
        row1 INT;
        col1 INT;
        collision BOOL = 'false';
    BEGIN
        collision = collision OR check_block_collision(row0 + b1[1], col0 + b1[2]);
        collision = collision OR check_block_collision(row0 + b2[1], col0 + b2[2]);
        collision = collision OR check_block_collision(row0 + b3[1], col0 + b3[2]);
        collision = collision OR check_block_collision(row0 + b4[1], col0 + b4[2]);
        RETURN collision;
    END;
$$;

CREATE OR REPLACE FUNCTION check_block_collision(row0 INT, col0 INT)
    RETURNS BOOL
    LANGUAGE plpgsql
AS $$
    DECLARE
        collision BOOL = 'true';
    BEGIN
        IF (col0 BETWEEN 1 AND 10) AND row0 <= 20 THEN
            IF row0 > 0 THEN
                SELECT state INTO collision 
                FROM board WHERE height = row0 AND width = col0;
            ELSE
                collision = 'false';
            END IF;
        END IF;
        RETURN collision;
    END;
$$; 
```

The first function to be called is `check_piece_collision`, which takes as arguments the row and column of the playfield to be checked and the relative block positions of the piece that were retrieved from `pieces`. It computes the absolute positions of the four piece blocks by adding the specified row and column to the first and second elements respectively of each integer array. The rows and columns for these positions are then passed to `check_block_collision`, which determines whether the specified block is in-bounds and not currently occupied by another piece.

Going back to the loop in function `place`, we can see that it terminates once `check_block_collision` indicates that a collision is detected at the specified height. After subtracting one from this height to get the largest *non-colliding* height, the function then calls `add_piece`, which is defined below:


```python
CREATE OR REPLACE FUNCTION add_piece(
    row0 INT, col0 INT, b1 INT[], b2 INT[], b3 INT[], b4 INT[])
    RETURNS VOID
    LANGUAGE plpgsql
AS $$
    BEGIN
        UPDATE board SET state = 'true' WHERE 
            (height = row0 + b1[1] AND width = col0 + b1[2]) OR
            (height = row0 + b2[1] AND width = col0 + b2[2]) OR
            (height = row0 + b3[1] AND width = col0 + b3[2]) OR
            (height = row0 + b4[1] AND width = col0 + b4[2]);
    END;
$$;
```

This function simply updates the values in `board` which correspond to the final position of the piece. Once the state of those tiles is changed to `true`, the `place` function cleares any filled lines by calling the `clear_lines` function, which is given below:


```python
CREATE OR REPLACE FUNCTION clear_lines()
    RETURNS VOID
    LANGUAGE plpgsql
AS $$
    DECLARE
        row_itr INT;
        filled BOOL;
        cleared_count INT = 0;
    BEGIN
        FOR row_itr IN 
            SELECT height FROM board GROUP BY height HAVING BOOL_AND(state) ORDER BY height
        LOOP
            DELETE FROM board WHERE height = row_itr;
            UPDATE board SET height = height + 1 WHERE height < row_itr;
            cleared_count = cleared_count + 1;
        END LOOP;
        INSERT INTO board(height, width) 
            SELECT * FROM generate_series(1, cleared_count) t1 CROSS JOIN generate_series(1, 10) t2;
        UPDATE lines SET num_cleared = num_cleared + 1 WHERE num_lines = cleared_count;
    END;
$$;
```

When clearing a line, the blocks in the target line must be deleted, and all lines that are above it in the playfield must be dropped down by one row. This rearrangement can be carried out for any number of simultaneous line clears by starting from the top of the playfield (i.e. at a height of 1) and iterating down the rows. When a row is determined to be full, it is deleted from `board` and all rows above it have their heights incremented by 1. By repeating this for heights 1 through 20, all line clears can be correctly implemented in a single pass. Finally, `clear_lines` updates the `lines` table based on how many lines were cleared, ranging from 0 (in which case there is no update) to 4.

After performing any line clears, the `place` function finishes by calling `prepare_next`, which prints the updated playfield and selects the next piece for the player. This process repeats for each new piece until no valid moves remain, at which point the player must call `reset` to begin a new game (this can be used to end a game at any point).

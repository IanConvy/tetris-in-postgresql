/* 
When run in an empty database, this script file generates the tables, views, functions, and
procedured needed for the Tetris application. Once the scipt has been run, the "reset" procedure
must be called in order to prepare the components and print the blank playfield. To place a piece,
you insert a (pos, orient) tuple into the "moves" table, where "pos" is an integer from 1 to 10 that
marks the leftmost column to place the piece, and "orient" is an integer that determines the rotation
of the piece.
*/

CREATE EXTENSION IF NOT EXISTS tablefunc;

CREATE TABLE IF NOT EXISTS board ( -- Holds the instantaneous state of the playfield
    height INT,
    width INT,
    state BOOL DEFAULT 'false'
);

CREATE OR REPLACE VIEW readable AS -- A view of "board" that a human being could read from
    SELECT * FROM crosstab(
        'SELECT height, width, CASE WHEN state THEN ''X'' ELSE '' '' END AS val FROM board ORDER BY 1,2') AS 
            t (height INT, c1 TEXT, c2 TEXT, c3 TEXT, c4 TEXT, c5 TEXT, c6 TEXT, c7 TEXT, c8 TEXT, c9 TEXT, c10 TEXT);

CREATE TABLE IF NOT EXISTS lines ( -- Holds the number of line clears
    num_lines INT,
    num_cleared INT
);

CREATE TABLE IF NOT EXISTS moves ( -- Holds the moves that have been made
    pos INT,
    orient INT
);

CREATE TABLE IF NOT EXISTS piece_log (piece_name) -- Holds the current piece
    AS SELECT 'blank';

CREATE TABLE IF NOT EXISTS pieces (name, orient, b1, b2, b3, b4, prev1, prev2, prev3, prev4)
    AS VALUES -- Holds information that characterizes the seven Tetris pieces
        ('sq', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [1,0], ARRAY [1,1], 'X X', 'X X', '   ', '   '),
        ('i', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [0,2], ARRAY [0,3], 'X X X X', '       ', '       ', '       '),
        ('i', 2, ARRAY [-1,0], ARRAY [0,0], ARRAY [1,0], ARRAY [2,0], 'X', 'X', 'X', 'X'),
        ('t', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [1,1], ARRAY [0,2], 'X X X', '  X  ', '     ', '     '),
        ('t', 2, ARRAY [0,0], ARRAY [0,1], ARRAY [1,1], ARRAY [-1,1], '  X', 'X X', '  X', '   '),
        ('t', 3, ARRAY [0,0], ARRAY [0,1], ARRAY [-1,1], ARRAY [0,2], '  X  ', 'X X X', '     ', '     '),
        ('t', 4, ARRAY [0,0], ARRAY [0,1], ARRAY [-1,0], ARRAY [1,0], 'X  ', 'X X', 'X  ', '   '),
        ('zr', 1, ARRAY [1,0], ARRAY [1,1], ARRAY [0,1], ARRAY [0,2], '  X X', 'X X  ', '     ', '     '),
        ('zr', 2, ARRAY [0,0], ARRAY [-1,0], ARRAY [0,1], ARRAY [1,1], 'X  ', 'X X', '  X', '   '),
        ('zl', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [1,1], ARRAY [1,2], 'X X  ', '  X X', '     ', '     '),
        ('zl', 2, ARRAY [0,0], ARRAY [1,0], ARRAY [0,1], ARRAY [-1,1], '  X', 'X X', 'X  ', '   '),
        ('lr', 1, ARRAY [0,0], ARRAY [1,0], ARRAY [0,1], ARRAY [0,2], 'X X X', 'X    ', '     ', '     '),
        ('lr', 2, ARRAY [0,1], ARRAY [-1,1], ARRAY [-1,0], ARRAY [1,1], 'X X', '  X', '  X', '   '),
        ('lr', 3, ARRAY [0,0], ARRAY [0,1], ARRAY [0,2], ARRAY [-1,2], '    X', 'X X X',  '     ', '     '),
        ('lr', 4, ARRAY [0,0], ARRAY [-1,0], ARRAY [1,0], ARRAY [1,1], 'X  ', 'X  ', 'X X', '   '),
        ('ll', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [0,2], ARRAY [1,2], 'X X X', '    X', '     ', '     '),
        ('ll', 2, ARRAY [1,0], ARRAY [1,1], ARRAY [0,1], ARRAY [-1,1], '  X', '  X', 'X X', '   '),
        ('ll', 3, ARRAY [0,0], ARRAY [-1,0], ARRAY [0,1], ARRAY [0,2], 'X    ', 'X X X', '     ', '     '),
        ('ll', 4, ARRAY [0,0], ARRAY [-1,0], ARRAY [-1,1], ARRAY [1,0], 'X X', 'X  ', 'X  ', '   ');   

CREATE OR REPLACE PROCEDURE reset() -- Prepares all tables for a new game
LANGUAGE plpgsql
AS $$
    DECLARE
        col_index INT;
        row_index INT;
    BEGIN
        TRUNCATE TABLE moves;
        TRUNCATE TABLE board;
        TRUNCATE TABLE lines;
        INSERT INTO lines(num_lines, num_cleared) VALUES (1, 0), (2, 0), (3, 0), (4, 0);
        INSERT INTO board (height, width) 
            SELECT * FROM generate_series(1, 20) t1 CROSS JOIN generate_series(1, 10) t2;
        PERFORM prepare_next()
        COMMIT;
    END;
$$;

CREATE OR REPLACE FUNCTION print_board() -- Displays the current playfield
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

CREATE OR REPLACE FUNCTION print_clears() -- Displays the current number of line clears
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

CREATE OR REPLACE FUNCTION print_piece(piece_name TEXT) -- Displays the current piece
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

CREATE OR REPLACE FUNCTION get_piece() -- Retrieves a random piece
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

CREATE OR REPLACE FUNCTION prepare_next() -- Displays new game state and gets next piece
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

-- Checks tile for collisions
CREATE OR REPLACE FUNCTION check_block_collision(row0 INT, col0 INT) -- Checks tile for collisions
    RETURNS BOOL 
    LANGUAGE plpgsql
AS $$
    DECLARE
        collision BOOL = 'true';
    BEGIN
        IF (col0 BETWEEN 1 AND 10) AND row0 <= 20 THEN
            IF row0 > 0 THEN
                SELECT state INTO collision FROM board WHERE height = row0 AND width = col0;
            ELSE
                collision = 'false';
            END IF;
        END IF;
        RETURN collision;
    END;
$$; 

CREATE OR REPLACE FUNCTION check_piece_collision( -- Checks all tiles at given position for collisions
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

CREATE OR REPLACE FUNCTION add_piece( -- Adds a piece to the playfield
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

CREATE OR REPLACE FUNCTION clear_lines() -- Clears all filled lines
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

CREATE OR REPLACE FUNCTION place() -- Runs game logic when a piece is placed
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
            collision = check_piece_collision(row_itr, col0, piece.b1, piece.b2, piece.b3, piece.b4);
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

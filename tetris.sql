CREATE EXTENSION IF NOT EXISTS tablefunc;

CREATE TABLE IF NOT EXISTS board (
    height INT,
    width INT,
    state BOOL DEFAULT 'false'
);

CREATE OR REPLACE VIEW readable AS
    SELECT * FROM crosstab(
        'SELECT height, width, CASE WHEN state THEN ''X'' ELSE '' '' END AS val FROM board ORDER BY 1,2') AS 
            t (height INT, c1 TEXT, c2 TEXT, c3 TEXT, c4 TEXT, c5 TEXT, c6 TEXT, c7 TEXT, c8 TEXT, c9 TEXT, c10 TEXT);

CREATE TABLE IF NOT EXISTS lines (
    num_lines INT,
    num_cleared INT
);

CREATE TABLE IF NOT EXISTS moves (
    pos INT,
    orient INT
);

CREATE TABLE IF NOT EXISTS piece_log (piece_name)
    AS SELECT 'blank';

CREATE TABLE IF NOT EXISTS pieces (name, orient, b1, b2, b3, b4)
    AS VALUES
        ('sq', 0, ARRAY [0,0], ARRAY [0,1], ARRAY [1,0], ARRAY [1,1]),
        ('i', 0, ARRAY [0,0], ARRAY [0,1], ARRAY [0,2], ARRAY [0,3]),
        ('i', 1, ARRAY [-1,0], ARRAY [0,0], ARRAY [1,0], ARRAY [2,0]),
        ('t', 0, ARRAY [0,0], ARRAY [0,1], ARRAY [1,1], ARRAY [0,2]),
        ('t', 1, ARRAY [0,0], ARRAY [0,1], ARRAY [1,1], ARRAY [-1,1]),
        ('t', 2, ARRAY [0,0], ARRAY [0,1], ARRAY [-1,1], ARRAY [0,2]),
        ('zr', 0, ARRAY [1,0], ARRAY [1,1], ARRAY [0,1], ARRAY [0,2]),
        ('zr', 1, ARRAY [0,0], ARRAY [-1,0], ARRAY [0,1], ARRAY [1,1]),
        ('zl', 0, ARRAY [0,0], ARRAY [0,1], ARRAY [1,1], ARRAY [1,2]),
        ('zl', 1, ARRAY [0,0], ARRAY [1,0], ARRAY [0,1], ARRAY [-1,1]),
        ('lr', 0, ARRAY [0,0], ARRAY [1,0], ARRAY [0,1], ARRAY [0,2]),
        ('lr', 1, ARRAY [0,1], ARRAY [-1,1], ARRAY [-1,0], ARRAY [1,1]),
        ('lr', 2, ARRAY [0,0], ARRAY [0,1], ARRAY [0,2], ARRAY [-1,2]),
        ('lr', 3, ARRAY [0,0], ARRAY [-1,0], ARRAY [1,0], ARRAY [1,1]),
        ('ll', 0, ARRAY [0,0], ARRAY [0,1], ARRAY [0,2], ARRAY [1,2]),
        ('ll', 1, ARRAY [1,0], ARRAY [1,1], ARRAY [0,1], ARRAY [-1,1]),
        ('ll', 2, ARRAY [0,0], ARRAY [-1,0], ARRAY [0,1], ARRAY [0,2]),
        ('ll', 3, ARRAY [0,0], ARRAY [-1,0], ARRAY [-1,1], ARRAY [1,0]),
        ('t', 3, ARRAY [0,0], ARRAY [0,1], ARRAY [-1,0], ARRAY [1,0]);   

CREATE OR REPLACE PROCEDURE reset()
LANGUAGE plpgsql
AS $$
    DECLARE
        col_index INT;
        row_index INT;
    BEGIN
        TRUNCATE TABLE moves RESTART IDENTITY;
        TRUNCATE TABLE board;
        TRUNCATE TABLE lines;
        INSERT INTO lines(num_lines, num_cleared) VALUES (1, 0), (2, 0), (3, 0), (4, 0);
        INSERT INTO board (height, width) 
            SELECT * FROM generate_series(1, 20) t1 CROSS JOIN generate_series(1, 10) t2;
        PERFORM prepare_next()
        COMMIT;
    END;
$$;

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

CREATE OR REPLACE FUNCTION prepare_next()
    RETURNS VOID
    LANGUAGE plpgsql
AS $$
    DECLARE
        piece_txt TEXT;
    BEGIN
        piece_txt = get_piece();
        PERFORM print_board();
        PERFORM print_clears();
        RAISE NOTICE 'Next piece is "%"', piece_txt;
        UPDATE piece_log SET piece_name = piece_txt;
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
                SELECT state INTO collision FROM board WHERE height = row0 AND width = col0;
            ELSE
                collision = 'false';
            END IF;
        END IF;
        RETURN collision;
    END;
$$; 

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
        col0 = NEW.pos;
        FOR row_itr IN 1..21 LOOP
            collision = check_piece_collision(row_itr, col0, piece.b1, piece.b2, piece.b3, piece.b4);
            row0 = row_itr - 1;
            EXIT WHEN collision;
        END LOOP;
        IF row0 = 0 THEN
            PERFORM print_board();
            RAISE NOTICE 'Piece cannot be placed.';
            RAISE NOTICE 'Current piece is "%"', piece.name;
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

--
-- PLTSQL -- DECLARation and use of "@"-prefixed variables
--

CREATE FUNCTION declare_and_print_atvar() RETURNS void AS $$
DECLARE @a int
BEGIN
    PRINT @a
END
$$ LANGUAGE pltsql;

SELECT declare_and_print_atvar();

CREATE FUNCTION declare_assign_and_print_atvar() RETURNS void AS $$
DECLARE @foo int
BEGIN
	@foo = 12;
    PRINT @foo
END
$$ LANGUAGE pltsql;

SELECT declare_assign_and_print_atvar();

CREATE FUNCTION define_and_return_atvar() RETURNS int AS $$
DECLARE @foo int = 120
BEGIN
    RETURN @foo;
END
$$ LANGUAGE pltsql;

SELECT define_and_return_atvar();

CREATE FUNCTION test_atvar_operator_interaction() RETURNS void AS $$
DECLARE @a int = 1
DECLARE @b int = 2
BEGIN
    SET @a = @b + @a + 1 +-@b
    SET @a = abs(-@a);
    RAISE INFO '%, %', @a, @b;
END
$$ LANGUAGE pltsql;

SELECT test_atvar_operator_interaction();

CREATE FUNCTION test_multiple_base_var_types() RETURNS void AS $$
DECLARE @a int = (23 + 45)
DECLARE @b decimal(10,2)
DECLARE @c decimal(100,2)
DECLARE @d decimal(5,2) = 12.4
DECLARE @e varchar(30)
BEGIN
    SET @c = 12.3
    SET @b = @c
    PRINT @a
    PRINT @b
    PRINT @c
    PRINT @d
    SET @e = 'au revoir'
    PRINT @e
END
$$ LANGUAGE pltsql;

SELECT test_multiple_base_var_types();

CREATE FUNCTION test_decl_line_breaks() RETURNS void AS $$
DECLARE
@a int = 1
DECLARE @b int
    = 2
DECLARE @c
    int
BEGIN
    PRINT
    @b
END
$$ LANGUAGE pltsql;

SELECT test_decl_line_breaks();

CREATE FUNCTION test_multiple_base_var_types_with_commas() RETURNS void AS $$
DECLARE @a int = (23 + 45),
        @b decimal(10,2),
        @c decimal(100,2),
        @d decimal(5,2) = 12.4,
        @e varchar(30)
BEGIN
    SET @c = 12.3;
    SET @b = @c;
    PRINT @a
    PRINT @b
    PRINT @c
    PRINT @d
    SET @e = 'au revoir';
    PRINT @e
END
$$ LANGUAGE pltsql;

SELECT test_multiple_base_var_types_with_commas();

CREATE FUNCTION test_multiple_base_var_types_with_commas_and_line_breaks() RETURNS void AS $$
DECLARE @a int
            = (23 + 45),
        @b
            decimal(10,2),
        @c decimal(100,2)
,       @d decimal(5,2) = 12.4,
        @e varchar(30)
BEGIN
    SET @c = 12.3;
    SET @b = @c;
    PRINT @a
    PRINT @b
    PRINT @c
    PRINT @d
    SET @e = 'au revoir';
    PRINT @e
END
$$ LANGUAGE pltsql;

SELECT test_multiple_base_var_types_with_commas_and_line_breaks();

CREATE FUNCTION test_quoted_atvars() RETURNS void AS $$
DECLARE "@a" int = 1
DECLARE "@b" int = 2
BEGIN
    SET "@a" = "@b" + "@a" + 1 +-"@b"
    SET "@a" = abs(-"@a");
    RAISE INFO '%, %', "@a", "@b";
END
$$ LANGUAGE pltsql;

SELECT test_quoted_atvars();

CREATE FUNCTION test_quoted_atvar_casing() RETURNS void AS $$
DECLARE @a int = 1
DECLARE "@A" int = 2
BEGIN
    PRINT @a
    PRINT "@A"
END
$$ LANGUAGE pltsql;

SELECT test_quoted_atvar_casing();

CREATE FUNCTION test_atvar_in_sql() RETURNS void AS $$
DECLARE @a int = 1
BEGIN
    CREATE TEMPORARY TABLE test (a int);
    INSERT INTO test (a) VALUES (@a);
    SELECT a INTO @a FROM test;
    PRINT @a
END
$$ LANGUAGE pltsql;

SELECT test_atvar_in_sql();

CREATE FUNCTION error_duplicate_atvar_decl() RETURNS void AS $$
DECLARE @foo int
DECLARE @foo int
BEGIN
    PRINT @foo
END
$$ LANGUAGE pltsql;

CREATE FUNCTION error_invalid_atvar_type_decl_1() RETURNS void AS $$
DECLARE @foo non-existent
BEGIN
    PRINT @foo
END
$$ LANGUAGE pltsql;

CREATE FUNCTION error_invalid_atvar_type_decl_2() RETURNS void AS $$
DECLARE @foo @non-existent
BEGIN
    PRINT @foo
END
$$ LANGUAGE pltsql;

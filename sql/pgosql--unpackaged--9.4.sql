/* 
 * Copyright (c) 2014, pgORA, Inc.
 */

ALTER EXTENSION plosql ADD PROCEDURAL LANGUAGE plosql;
-- ALTER ADD LANGUAGE doesn't pick up the support functions, so we have to.
ALTER EXTENSION plosql ADD FUNCTION plosql_call_handler();
ALTER EXTENSION plosql ADD FUNCTION plosql_inline_handler(internal);
ALTER EXTENSION plosql ADD FUNCTION plosql_validator(oid);

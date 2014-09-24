/*--------------------------------------------------------------------------------------------
 *
 * plosql.h		- Definitions for the PL/SQL
 *			  procedural language
 *
 * Copyright (c) 2014, pgORA, Inc.
 * Portions Copyright (c) 1996-2011, PostgreSQL Global Development Group
 * Portions Copyright (c) 1994, Regents of the University of California
 *
 *
 * IDENTIFICATION
 *	  src/plosql.h
 *
 *--------------------------------------------------------------------------------------------
 */

#ifndef PLOSQL_H
#define PLOSQL_H

#include "postgres.h"

#include "access/xact.h"
#include "commands/trigger.h"
#include "executor/spi.h"

/**********************************************************************
 * Definitions
 **********************************************************************/

/* define our text domain for translations */
#undef TEXTDOMAIN
#define TEXTDOMAIN PG_TEXTDOMAIN("plosql")

#undef _
#define _(x) dgettext(TEXTDOMAIN, x)

/* ----------
 * Compiler's namespace item types
 * ----------
 */
enum
{
	PLOSQL_NSTYPE_LABEL,
	PLOSQL_NSTYPE_VAR,
	PLOSQL_NSTYPE_ROW,
	PLOSQL_NSTYPE_REC
};

/* ----------
 * Datum array node types
 * ----------
 */
enum
{
	PLOSQL_DTYPE_VAR,
	PLOSQL_DTYPE_ROW,
	PLOSQL_DTYPE_REC,
	PLOSQL_DTYPE_RECFIELD,
	PLOSQL_DTYPE_ARRAYELEM,
	PLOSQL_DTYPE_EXPR
};

/* ----------
 * Variants distinguished in PLOSQL_type structs
 * ----------
 */
enum
{
	PLOSQL_TTYPE_SCALAR,		/* scalar types and domains */
	PLOSQL_TTYPE_ROW,			/* composite types */
	PLOSQL_TTYPE_REC,			/* RECORD pseudotype */
	PLOSQL_TTYPE_PSEUDO		/* other pseudotypes */
};

/* ----------
 * Execution tree node types
 * ----------
 */
enum PLOSQL_stmt_types
{
	PLOSQL_STMT_BLOCK,
	PLOSQL_STMT_ASSIGN,
	PLOSQL_STMT_IF,
	PLOSQL_STMT_CASE,
	PLOSQL_STMT_LOOP,
	PLOSQL_STMT_WHILE,
	PLOSQL_STMT_FORI,
	PLOSQL_STMT_FORS,
	PLOSQL_STMT_FORC,
	PLOSQL_STMT_FOREACH_A,
	PLOSQL_STMT_EXIT,
	PLOSQL_STMT_RETURN,
	PLOSQL_STMT_RETURN_NEXT,
	PLOSQL_STMT_RETURN_QUERY,
	PLOSQL_STMT_RAISE,
	PLOSQL_STMT_EXECSQL,
	PLOSQL_STMT_DYNEXECUTE,
	PLOSQL_STMT_DYNFORS,
	PLOSQL_STMT_GETDIAG,
	PLOSQL_STMT_OPEN,
	PLOSQL_STMT_FETCH,
	PLOSQL_STMT_CLOSE,
	PLOSQL_STMT_PERFORM
};


/* ----------
 * Execution node return codes
 * ----------
 */
enum
{
	PLOSQL_RC_OK,
	PLOSQL_RC_EXIT,
	PLOSQL_RC_RETURN,
	PLOSQL_RC_CONTINUE
};

/* ----------
 * GET DIAGNOSTICS information items
 * ----------
 */
enum
{
	PLOSQL_GETDIAG_ROW_COUNT,
	PLOSQL_GETDIAG_RESULT_OID,
	PLOSQL_GETDIAG_ERROR_CONTEXT,
	PLOSQL_GETDIAG_ERROR_DETAIL,
	PLOSQL_GETDIAG_ERROR_HINT,
	PLOSQL_GETDIAG_RETURNED_SQLSTATE,
	PLOSQL_GETDIAG_MESSAGE_TEXT
};

/* --------
 * RAISE statement options
 * --------
 */
enum
{
	PLOSQL_RAISEOPTION_ERRCODE,
	PLOSQL_RAISEOPTION_MESSAGE,
	PLOSQL_RAISEOPTION_DETAIL,
	PLOSQL_RAISEOPTION_HINT
};

/* --------
 * Behavioral modes for plosql variable resolution
 * --------
 */
typedef enum
{
	PLOSQL_RESOLVE_ERROR,		/* throw error if ambiguous */
	PLOSQL_RESOLVE_VARIABLE,	/* prefer plosql var to table column */
	PLOSQL_RESOLVE_COLUMN		/* prefer table column to plosql var */
} PLOSQL_resolve_option;


/**********************************************************************
 * Node and structure definitions
 **********************************************************************/


typedef struct
{								/* Postgres data type */
	char	   *typname;		/* (simple) name of the type */
	Oid			typoid;			/* OID of the data type */
	int			ttype;			/* PLOSQL_TTYPE_ code */
	int16		typlen;			/* stuff copied from its pg_type entry */
	bool		typbyval;
	Oid			typrelid;
	Oid			typioparam;
	Oid			collation;		/* from pg_type, but can be overridden */
	FmgrInfo	typinput;		/* lookup info for typinput function */
	int32		atttypmod;		/* typmod (taken from someplace else) */
} PLOSQL_type;


/*
 * PLOSQL_datum is the common supertype for PLOSQL_expr, PLOSQL_var,
 * PLOSQL_row, PLOSQL_rec, PLOSQL_recfield, and PLOSQL_arrayelem
 */
typedef struct
{								/* Generic datum array item		*/
	int			dtype;
	int			dno;
} PLOSQL_datum;

/*
 * The variants PLOSQL_var, PLOSQL_row, and PLOSQL_rec share these
 * fields
 */
typedef struct
{								/* Scalar or composite variable */
	int			dtype;
	int			dno;
	char	   *refname;
	int			lineno;
} PLOSQL_variable;

typedef struct PLOSQL_expr
{								/* SQL Query to plan and execute	*/
	int			dtype;
	int			dno;
	char	   *query;
	SPIPlanPtr	plan;
	Bitmapset  *paramnos;		/* all dnos referenced by this query */

	/* function containing this expr (not set until we first parse query) */
	struct PLOSQL_function *func;

	/* namespace chain visible to this expr */
	struct PLOSQL_nsitem *ns;

	/* fields for "simple expression" fast-path execution: */
	Expr	   *expr_simple_expr;		/* NULL means not a simple expr */
	int			expr_simple_generation; /* plancache generation we checked */
	Oid			expr_simple_type;		/* result type Oid, if simple */

	/*
	 * if expr is simple AND prepared in current transaction,
	 * expr_simple_state and expr_simple_in_use are valid. Test validity by
	 * seeing if expr_simple_lxid matches current LXID.  (If not,
	 * expr_simple_state probably points at garbage!)
	 */
	ExprState  *expr_simple_state;		/* eval tree for expr_simple_expr */
	bool		expr_simple_in_use;		/* true if eval tree is active */
	LocalTransactionId expr_simple_lxid;
} PLOSQL_expr;


typedef struct
{								/* Scalar variable */
	int			dtype;
	int			dno;
	char	   *refname;
	int			lineno;

	PLOSQL_type *datatype;
	int			isconst;
	int			notnull;
	PLOSQL_expr *default_val;
	PLOSQL_expr *cursor_explicit_expr;
	int			cursor_explicit_argrow;
	int			cursor_options;

	Datum		value;
	bool		isnull;
	bool		freeval;
} PLOSQL_var;


typedef struct
{								/* Row variable */
	int			dtype;
	int			dno;
	char	   *refname;
	int			lineno;

	TupleDesc	rowtupdesc;

	/*
	 * Note: TupleDesc is only set up for named rowtypes, else it is NULL.
	 *
	 * Note: if the underlying rowtype contains a dropped column, the
	 * corresponding fieldnames[] entry will be NULL, and there is no
	 * corresponding var (varnos[] will be -1).
	 */
	int			nfields;
	char	  **fieldnames;
	int		   *varnos;
} PLOSQL_row;


typedef struct
{								/* Record variable (non-fixed structure) */
	int			dtype;
	int			dno;
	char	   *refname;
	int			lineno;

	HeapTuple	tup;
	TupleDesc	tupdesc;
	bool		freetup;
	bool		freetupdesc;
} PLOSQL_rec;


typedef struct
{								/* Field in record */
	int			dtype;
	int			dno;
	char	   *fieldname;
	int			recparentno;	/* dno of parent record */
} PLOSQL_recfield;


typedef struct
{								/* Element of array variable */
	int			dtype;
	int			dno;
	PLOSQL_expr *subscript;
	int			arrayparentno;	/* dno of parent array variable */
	/* Remaining fields are cached info about the array variable's type */
	Oid			parenttypoid;	/* type of array variable; 0 if not yet set */
	int32		parenttypmod;	/* typmod of array variable */
	Oid			arraytypoid;	/* OID of actual array type */
	int32		arraytypmod;	/* typmod of array (and its elements too) */
	int16		arraytyplen;	/* typlen of array type */
	Oid			elemtypoid;		/* OID of array element type */
	int16		elemtyplen;		/* typlen of element type */
	bool		elemtypbyval;	/* element type is pass-by-value? */
	char		elemtypalign;	/* typalign of element type */
} PLOSQL_arrayelem;


typedef struct PLOSQL_nsitem
{								/* Item in the compilers namespace tree */
	int			itemtype;
	int			itemno;
	struct PLOSQL_nsitem *prev;
	char		name[1];		/* actually, as long as needed */
} PLOSQL_nsitem;


typedef struct
{								/* Generic execution node		*/
	int			cmd_type;
	int			lineno;
} PLOSQL_stmt;


typedef struct PLOSQL_condition
{								/* One EXCEPTION condition name */
	int			sqlerrstate;	/* SQLSTATE code */
	char	   *condname;		/* condition name (for debugging) */
	struct PLOSQL_condition *next;
} PLOSQL_condition;

typedef struct
{
	int			sqlstate_varno;
	int			sqlerrm_varno;
	List	   *exc_list;		/* List of WHEN clauses */
} PLOSQL_exception_block;

typedef struct
{								/* One EXCEPTION ... WHEN clause */
	int			lineno;
	PLOSQL_condition *conditions;
	List	   *action;			/* List of statements */
} PLOSQL_exception;


typedef struct
{								/* Block of statements			*/
	int			cmd_type;
	int			lineno;
	char	   *label;
	List	   *body;			/* List of statements */
	int			n_initvars;
	int		   *initvarnos;
	PLOSQL_exception_block *exceptions;
} PLOSQL_stmt_block;


typedef struct
{								/* Assign statement			*/
	int			cmd_type;
	int			lineno;
	int			varno;
	PLOSQL_expr *expr;
} PLOSQL_stmt_assign;

typedef struct
{								/* PERFORM statement		*/
	int			cmd_type;
	int			lineno;
	PLOSQL_expr *expr;
} PLOSQL_stmt_perform;

typedef struct
{								/* Get Diagnostics item		*/
	int			kind;			/* id for diagnostic value desired */
	int			target;			/* where to assign it */
} PLOSQL_diag_item;

typedef struct
{								/* Get Diagnostics statement		*/
	int			cmd_type;
	int			lineno;
	bool		is_stacked;		/* STACKED or CURRENT diagnostics area? */
	List	   *diag_items;		/* List of PLOSQL_diag_item */
} PLOSQL_stmt_getdiag;


typedef struct
{								/* IF statement				*/
	int			cmd_type;
	int			lineno;
	PLOSQL_expr *cond;			/* boolean expression for THEN */
	List	   *then_body;		/* List of statements */
	List	   *elsif_list;		/* List of PLOSQL_if_elsif structs */
	List	   *else_body;		/* List of statements */
} PLOSQL_stmt_if;

typedef struct					/* one ELSIF arm of IF statement */
{
	int			lineno;
	PLOSQL_expr *cond;			/* boolean expression for this case */
	List	   *stmts;			/* List of statements */
} PLOSQL_if_elsif;


typedef struct					/* CASE statement */
{
	int			cmd_type;
	int			lineno;
	PLOSQL_expr *t_expr;		/* test expression, or NULL if none */
	int			t_varno;		/* var to store test expression value into */
	List	   *case_when_list; /* List of PLOSQL_case_when structs */
	bool		have_else;		/* flag needed because list could be empty */
	List	   *else_stmts;		/* List of statements */
} PLOSQL_stmt_case;

typedef struct					/* one arm of CASE statement */
{
	int			lineno;
	PLOSQL_expr *expr;			/* boolean expression for this case */
	List	   *stmts;			/* List of statements */
} PLOSQL_case_when;


typedef struct
{								/* Unconditional LOOP statement		*/
	int			cmd_type;
	int			lineno;
	char	   *label;
	List	   *body;			/* List of statements */
} PLOSQL_stmt_loop;


typedef struct
{								/* WHILE cond LOOP statement		*/
	int			cmd_type;
	int			lineno;
	char	   *label;
	PLOSQL_expr *cond;
	List	   *body;			/* List of statements */
} PLOSQL_stmt_while;


typedef struct
{								/* FOR statement with integer loopvar	*/
	int			cmd_type;
	int			lineno;
	char	   *label;
	PLOSQL_var *var;
	PLOSQL_expr *lower;
	PLOSQL_expr *upper;
	PLOSQL_expr *step;			/* NULL means default (ie, BY 1) */
	int			reverse;
	List	   *body;			/* List of statements */
} PLOSQL_stmt_fori;


/*
 * PLOSQL_stmt_forq represents a FOR statement running over a SQL query.
 * It is the common supertype of PLOSQL_stmt_fors, PLOSQL_stmt_forc
 * and PLOSQL_dynfors.
 */
typedef struct
{
	int			cmd_type;
	int			lineno;
	char	   *label;
	PLOSQL_rec *rec;
	PLOSQL_row *row;
	List	   *body;			/* List of statements */
} PLOSQL_stmt_forq;

typedef struct
{								/* FOR statement running over SELECT	*/
	int			cmd_type;
	int			lineno;
	char	   *label;
	PLOSQL_rec *rec;
	PLOSQL_row *row;
	List	   *body;			/* List of statements */
	/* end of fields that must match PLOSQL_stmt_forq */
	PLOSQL_expr *query;
} PLOSQL_stmt_fors;

typedef struct
{								/* FOR statement running over cursor	*/
	int			cmd_type;
	int			lineno;
	char	   *label;
	PLOSQL_rec *rec;
	PLOSQL_row *row;
	List	   *body;			/* List of statements */
	/* end of fields that must match PLOSQL_stmt_forq */
	int			curvar;
	PLOSQL_expr *argquery;		/* cursor arguments if any */
} PLOSQL_stmt_forc;

typedef struct
{								/* FOR statement running over EXECUTE	*/
	int			cmd_type;
	int			lineno;
	char	   *label;
	PLOSQL_rec *rec;
	PLOSQL_row *row;
	List	   *body;			/* List of statements */
	/* end of fields that must match PLOSQL_stmt_forq */
	PLOSQL_expr *query;
	List	   *params;			/* USING expressions */
} PLOSQL_stmt_dynfors;


typedef struct
{								/* FOREACH item in array loop */
	int			cmd_type;
	int			lineno;
	char	   *label;
	int			varno;			/* loop target variable */
	int			slice;			/* slice dimension, or 0 */
	PLOSQL_expr *expr;			/* array expression */
	List	   *body;			/* List of statements */
} PLOSQL_stmt_foreach_a;


typedef struct
{								/* OPEN a curvar					*/
	int			cmd_type;
	int			lineno;
	int			curvar;
	int			cursor_options;
	PLOSQL_row *returntype;
	PLOSQL_expr *argquery;
	PLOSQL_expr *query;
	PLOSQL_expr *dynquery;
	List	   *params;			/* USING expressions */
} PLOSQL_stmt_open;


typedef struct
{								/* FETCH or MOVE statement */
	int			cmd_type;
	int			lineno;
	PLOSQL_rec *rec;			/* target, as record or row */
	PLOSQL_row *row;
	int			curvar;			/* cursor variable to fetch from */
	FetchDirection direction;	/* fetch direction */
	long		how_many;		/* count, if constant (expr is NULL) */
	PLOSQL_expr *expr;			/* count, if expression */
	bool		is_move;		/* is this a fetch or move? */
	bool		returns_multiple_rows;	/* can return more than one row? */
} PLOSQL_stmt_fetch;


typedef struct
{								/* CLOSE curvar						*/
	int			cmd_type;
	int			lineno;
	int			curvar;
} PLOSQL_stmt_close;


typedef struct
{								/* EXIT or CONTINUE statement			*/
	int			cmd_type;
	int			lineno;
	bool		is_exit;		/* Is this an exit or a continue? */
	char	   *label;			/* NULL if it's an unlabelled EXIT/CONTINUE */
	PLOSQL_expr *cond;
} PLOSQL_stmt_exit;


typedef struct
{								/* RETURN statement			*/
	int			cmd_type;
	int			lineno;
	PLOSQL_expr *expr;
	int			retvarno;
} PLOSQL_stmt_return;

typedef struct
{								/* RETURN NEXT statement */
	int			cmd_type;
	int			lineno;
	PLOSQL_expr *expr;
	int			retvarno;
} PLOSQL_stmt_return_next;

typedef struct
{								/* RETURN QUERY statement */
	int			cmd_type;
	int			lineno;
	PLOSQL_expr *query;		/* if static query */
	PLOSQL_expr *dynquery;		/* if dynamic query (RETURN QUERY EXECUTE) */
	List	   *params;			/* USING arguments for dynamic query */
} PLOSQL_stmt_return_query;

typedef struct
{								/* RAISE statement			*/
	int			cmd_type;
	int			lineno;
	int			elog_level;
	char	   *condname;		/* condition name, SQLSTATE, or NULL */
	char	   *message;		/* old-style message format literal, or NULL */
	List	   *params;			/* list of expressions for old-style message */
	List	   *options;		/* list of PLOSQL_raise_option */
} PLOSQL_stmt_raise;

typedef struct
{								/* RAISE statement option */
	int			opt_type;
	PLOSQL_expr *expr;
} PLOSQL_raise_option;


typedef struct
{								/* Generic SQL statement to execute */
	int			cmd_type;
	int			lineno;
	PLOSQL_expr *sqlstmt;
	bool		mod_stmt;		/* is the stmt INSERT/UPDATE/DELETE? */
	/* note: mod_stmt is set when we plan the query */
	bool		into;			/* INTO supplied? */
	bool		strict;			/* INTO STRICT flag */
	PLOSQL_rec *rec;			/* INTO target, if record */
	PLOSQL_row *row;			/* INTO target, if row */
} PLOSQL_stmt_execsql;


typedef struct
{								/* Dynamic SQL string to execute */
	int			cmd_type;
	int			lineno;
	PLOSQL_expr *query;		/* string expression */
	bool		into;			/* INTO supplied? */
	bool		strict;			/* INTO STRICT flag */
	PLOSQL_rec *rec;			/* INTO target, if record */
	PLOSQL_row *row;			/* INTO target, if row */
	List	   *params;			/* USING expressions */
} PLOSQL_stmt_dynexecute;


typedef struct PLOSQL_func_hashkey
{								/* Hash lookup key for functions */
	Oid			funcOid;

	bool		isTrigger;		/* true if called as a trigger */

	/* be careful that pad bytes in this struct get zeroed! */

	/*
	 * For a trigger function, the OID of the relation triggered on is part of
	 * the hash key --- we want to compile the trigger separately for each
	 * relation it is used with, in case the rowtype is different.	Zero if
	 * not called as a trigger.
	 */
	Oid			trigrelOid;

	/*
	 * We must include the input collation as part of the hash key too,
	 * because we have to generate different plans (with different Param
	 * collations) for different collation settings.
	 */
	Oid			inputCollation;

	/*
	 * We include actual argument types in the hash key to support polymorphic
	 * PLOSQL functions.  Be careful that extra positions are zeroed!
	 */
	Oid			argtypes[FUNC_MAX_ARGS];
} PLOSQL_func_hashkey;


typedef struct PLOSQL_function
{								/* Complete compiled function	  */
	char	   *fn_signature;
	Oid			fn_oid;
	TransactionId fn_xmin;
	ItemPointerData fn_tid;
	bool		fn_is_trigger;
	Oid			fn_input_collation;
	PLOSQL_func_hashkey *fn_hashkey;	/* back-link to hashtable key */
	MemoryContext fn_cxt;

	Oid			fn_rettype;
	int			fn_rettyplen;
	bool		fn_retbyval;
	FmgrInfo	fn_retinput;
	Oid			fn_rettypioparam;
	bool		fn_retistuple;
	bool		fn_retset;
	bool		fn_readonly;

	int			fn_nargs;
	int			fn_argvarnos[FUNC_MAX_ARGS];
	int			out_param_varno;
	int			found_varno;
	int			new_varno;
	int			old_varno;
	int			tg_name_varno;
	int			tg_when_varno;
	int			tg_level_varno;
	int			tg_op_varno;
	int			tg_relid_varno;
	int			tg_relname_varno;
	int			tg_table_name_varno;
	int			tg_table_schema_varno;
	int			tg_nargs_varno;
	int			tg_argv_varno;

	PLOSQL_resolve_option resolve_option;

	int			ndatums;
	PLOSQL_datum **datums;
	PLOSQL_stmt_block *action;

	/* these fields change when the function is used */
	struct PLOSQL_execstate *cur_estate;
	unsigned long use_count;
} PLOSQL_function;


typedef struct PLOSQL_execstate
{								/* Runtime execution data	*/
	PLOSQL_function *func;		/* function being executed */

	Datum		retval;
	bool		retisnull;
	Oid			rettype;		/* type of current retval */

	Oid			fn_rettype;		/* info about declared function rettype */
	bool		retistuple;
	bool		retisset;

	bool		readonly_func;

	TupleDesc	rettupdesc;
	char	   *exitlabel;		/* the "target" label of the current EXIT or
								 * CONTINUE stmt, if any */
	ErrorData  *cur_error;		/* current exception handler's error */

	Tuplestorestate *tuple_store;		/* SRFs accumulate results here */
	MemoryContext tuple_store_cxt;
	ResourceOwner tuple_store_owner;
	ReturnSetInfo *rsi;

	int			found_varno;
	int			ndatums;
	PLOSQL_datum **datums;

	/* temporary state for results from evaluation of query or expr */
	SPITupleTable *eval_tuptable;
	uint32		eval_processed;
	Oid			eval_lastoid;
	ExprContext *eval_econtext; /* for executing simple expressions */
	PLOSQL_expr *cur_expr;		/* current query/expr being evaluated */

	/* status information for error context reporting */
	PLOSQL_stmt *err_stmt;		/* current stmt */
	const char *err_text;		/* additional state info */

	void	   *plugin_info;	/* reserved for use by optional plugin */
} PLOSQL_execstate;


/*
 * A PLOSQL_plugin structure represents an instrumentation plugin.
 * To instrument PL/TSQL, a plugin library must access the rendezvous
 * variable "PLOSQL_plugin" and set it to point to a PLOSQL_plugin struct.
 * Typically the struct could just be static data in the plugin library.
 * We expect that a plugin would do this at library load time (_PG_init()).
 * It must also be careful to set the rendezvous variable back to NULL
 * if it is unloaded (_PG_fini()).
 *
 * This structure is basically a collection of function pointers --- at
 * various interesting points in pl_exec.c, we call these functions
 * (if the pointers are non-NULL) to give the plugin a chance to watch
 * what we are doing.
 *
 *	func_setup is called when we start a function, before we've initialized
 *	the local variables defined by the function.
 *
 *	func_beg is called when we start a function, after we've initialized
 *	the local variables.
 *
 *	func_end is called at the end of a function.
 *
 *	stmt_beg and stmt_end are called before and after (respectively) each
 *	statement.
 *
 * Also, immediately before any call to func_setup, PL/TSQL fills in the
 * error_callback and assign_expr fields with pointers to its own
 * plosql_exec_error_callback and exec_assign_expr functions.	This is
 * a somewhat ad-hoc expedient to simplify life for debugger plugins.
 */

typedef struct
{
	/* Function pointers set up by the plugin */
	void		(*func_setup) (PLOSQL_execstate *estate, PLOSQL_function *func);
	void		(*func_beg) (PLOSQL_execstate *estate, PLOSQL_function *func);
	void		(*func_end) (PLOSQL_execstate *estate, PLOSQL_function *func);
	void		(*stmt_beg) (PLOSQL_execstate *estate, PLOSQL_stmt *stmt);
	void		(*stmt_end) (PLOSQL_execstate *estate, PLOSQL_stmt *stmt);

	/* Function pointers set by PL/TSQL itself */
	void		(*error_callback) (void *arg);
	void		(*assign_expr) (PLOSQL_execstate *estate, PLOSQL_datum *target,
											PLOSQL_expr *expr);
} PLOSQL_plugin;


/* Struct types used during parsing */

typedef struct
{
	char	   *ident;			/* palloc'd converted identifier */
	bool		quoted;			/* Was it double-quoted? */
} PLword;

typedef struct
{
	List	   *idents;			/* composite identifiers (list of String) */
} PLcword;

typedef struct
{
	PLOSQL_datum *datum;		/* referenced variable */
	char	   *ident;			/* valid if simple name */
	bool		quoted;
	List	   *idents;			/* valid if composite name */
} PLwdatum;

/**********************************************************************
 * Global variable declarations
 **********************************************************************/

typedef enum
{
	IDENTIFIER_LOOKUP_NORMAL,	/* normal processing of var names */
	IDENTIFIER_LOOKUP_DECLARE,	/* In DECLARE --- don't look up names */
	IDENTIFIER_LOOKUP_EXPR		/* In SQL expression --- special case */
} IdentifierLookup;

extern IdentifierLookup plosql_IdentifierLookup;

extern int	plosql_variable_conflict;

extern bool plosql_check_syntax;
extern bool plosql_DumpExecTree;

extern PLOSQL_stmt_block *plosql_parse_result;

extern int	plosql_nDatums;
extern PLOSQL_datum **plosql_Datums;

extern char *plosql_error_funcname;

extern PLOSQL_function *plosql_curr_compile;
extern MemoryContext compile_tmp_cxt;

extern PLOSQL_plugin **plugin_ptr;

/**********************************************************************
 * Function declarations
 **********************************************************************/

/* ----------
 * Functions in pl_comp.c
 * ----------
 */
extern PLOSQL_function *plosql_compile(FunctionCallInfo fcinfo,
				bool forValidator);
extern PLOSQL_function *plosql_compile_inline(char *proc_source);
extern void plosql_parser_setup(struct ParseState *pstate,
					 PLOSQL_expr *expr);
extern bool plosql_parse_word(char *word1, const char *yytxt,
				   PLwdatum *wdatum, PLword *word);
extern bool plosql_parse_dblword(char *word1, char *word2,
					  PLwdatum *wdatum, PLcword *cword);
extern bool plosql_parse_tripword(char *word1, char *word2, char *word3,
					   PLwdatum *wdatum, PLcword *cword);
extern PLOSQL_type *plosql_parse_wordtype(char *ident);
extern PLOSQL_type *plosql_parse_cwordtype(List *idents);
extern PLOSQL_type *plosql_parse_wordrowtype(char *ident);
extern PLOSQL_type *plosql_parse_cwordrowtype(List *idents);
extern PLOSQL_type *plosql_build_datatype(Oid typeOid, int32 typmod,
					   Oid collation);
extern PLOSQL_variable *plosql_build_variable(const char *refname, int lineno,
					   PLOSQL_type *dtype,
					   bool add2namespace);
extern PLOSQL_rec *plosql_build_record(const char *refname, int lineno,
					 bool add2namespace);
extern int plosql_recognize_err_condition(const char *condname,
								bool allow_sqlstate);
extern PLOSQL_condition *plosql_parse_err_condition(char *condname);
extern void plosql_adddatum(PLOSQL_datum *new);
extern int	plosql_add_initdatums(int **varnos);
extern void plosql_HashTableInit(void);

/* ----------
 * Functions in pl_handler.c
 * ----------
 */
extern void _PG_init(void);
extern Datum plosql_call_handler(PG_FUNCTION_ARGS);
extern Datum plosql_inline_handler(PG_FUNCTION_ARGS);
extern Datum plosql_validator(PG_FUNCTION_ARGS);

/* ----------
 * Functions in pl_exec.c
 * ----------
 */
extern Datum plosql_exec_function(PLOSQL_function *func,
					  FunctionCallInfo fcinfo);
extern HeapTuple plosql_exec_trigger(PLOSQL_function *func,
					 TriggerData *trigdata);
extern void plosql_xact_cb(XactEvent event, void *arg);
extern void plosql_subxact_cb(SubXactEvent event, SubTransactionId mySubid,
				   SubTransactionId parentSubid, void *arg);
extern Oid exec_get_datum_type(PLOSQL_execstate *estate,
					PLOSQL_datum *datum);
extern void exec_get_datum_type_info(PLOSQL_execstate *estate,
						 PLOSQL_datum *datum,
						 Oid *typeid, int32 *typmod, Oid *collation);

/* ----------
 * Functions for namespace handling in pl_funcs.c
 * ----------
 */
extern void plosql_ns_init(void);
extern void plosql_ns_push(const char *label);
extern void plosql_ns_pop(void);
extern PLOSQL_nsitem *plosql_ns_top(void);
extern void plosql_ns_additem(int itemtype, int itemno, const char *name);
extern PLOSQL_nsitem *plosql_ns_lookup(PLOSQL_nsitem *ns_cur, bool localmode,
				  const char *name1, const char *name2,
				  const char *name3, int *names_used);
extern PLOSQL_nsitem *plosql_ns_lookup_label(PLOSQL_nsitem *ns_cur,
						const char *name);

/* ----------
 * Other functions in pl_funcs.c
 * ----------
 */
extern const char *plosql_stmt_typename(PLOSQL_stmt *stmt);
extern const char *plosql_getdiag_kindname(int kind);
extern void plosql_free_function_memory(PLOSQL_function *func);
extern void plosql_dumptree(PLOSQL_function *func);

/* ----------
 * Scanner functions in pl_scanner.c
 * ----------
 */
extern int	plosql_base_yylex(void);
extern int	plosql_yylex(void);
extern void plosql_push_back_token(int token);
extern void plosql_append_source_text(StringInfo buf,
						   int startlocation, int endlocation);
extern void plosql_peek2(int *tok1_p, int *tok2_p, int *tok1_loc,
			  int *tok2_loc);
extern int	plosql_scanner_errposition(int location);
extern void plosql_yyerror(const char *message);
extern int	plosql_location_to_lineno(int location);
extern int	plosql_latest_lineno(void);
extern void plosql_scanner_init(const char *str);
extern void plosql_scanner_finish(void);

/* ----------
 * Externs in gram.y
 * ----------
 */
extern int	plosql_yyparse(void);

#endif   /* PLOSQL_H */

/* eventsql.c: event log to SQLite importer.
 * 
 * $Id$
 * 
 * Copyright (c) 2012-2014 Ravenbrook Limited.  See end of file for license.
 *
 * This is a command-line tool that imports events from a text-format
 * MPS telemetry file into a SQLite database file.
 *
 * The default MPS library will write a binary-format telemetry file
 * which can be converted into a text-format file using the eventcnv
 * program (q.v.).
 *
 * Each event type gets its own table in the database.  These tables
 * are created from the definitions in eventdef.h if they don't
 * already exist.  Each event becomes a single row in the appropriate
 * table, which has a column for each event parameter, a time column
 * for the event time field, and a log_serial column to identify the
 * source log file.  Because the database schema depends on the event
 * definitions in eventdef.h, eventsql has to be compiled using the
 * same event header files as those used to compile the MPS and
 * eventcnv which generated and processed the telemetry output.
 *
 * The program also creates several other tables: three 'glue' tables
 * containing event metadata - event_kind (one row per kind),
 * event_type (one row per type), and event_param (one row per
 * parameter), all derived from eventdef.h - and the event_log table
 * which has one row per log file imported (the log_serial column in
 * the event tables is a primary key to this event_log table).
 *
 * No tables are created if they already exist, unless the -r
 * (rebuild) switch is given.
 *
 * Options:
 *
 * -v (verbose): Increase verbosity.  eventsql logs to stderr.  By
 *  default, it doesn't log much; it can be made more and more
 *  loquacious by adding more -v switches.
 *
 * -p (progress): Show progress with a series of dots written to
 * standard output (one dot per 100,000 events processed).  Defaults
 * on if -v specified, off otherwise. 
 * 
 * -t (test):  Run unit tests on parts of eventsql.  There aren't many
 * of these.  TODO: write more unit tests.
 *
 * -d (delete): Delete the SQL file before importing.
 * 
 * -f (force): Import the events to SQL even if the SQL database
 * already includes a record of importing a matching log file.
 * 
 * -r (rebuild): Drop the glue tables from SQL, which will force them
 * to be recreated.  Important if you change event types or kinds in
 * eventdef.h.
 * 
 * -i <logfile>: Import events from the named logfile.  Defaults to
 * standard input.  If the specified file (matched by size and
 * modtime) has previously been imported to the same database, it will
 * not be imported again unless -f is specified.
 *
 * -o <database>: Import events to the named database file.  If not
 * specified, eventsql will use the MPS_TELEMETRY_DATABASE environment
 * variable, and default to "mpsevent.db".
 *
 * $Id$
 */

#include "misc.h"
#include "config.h"
#include "eventdef.h"
#include "eventcom.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

/* on Windows, we build SQLite locally from the amalgamated sources */
#ifdef MPS_BUILD_MV
#include "sqlite3.h"
#else
#include <sqlite3.h>
#endif

#define DATABASE_NAME_ENVAR   "MPS_TELEMETRY_DATABASE"
#define DEFAULT_DATABASE_NAME "mpsevent.db"

#ifdef MPS_BUILD_MV
#define strtoll _strtoi64
#endif


typedef sqlite3_int64 int64;

/* At non-zero verbosity levels we output rows of dots.  One dot per
 * SMALL_TICK events, BIG_TICK dots per row. */

#define SMALL_TICK 100000
#define BIG_TICK   50

/* Utility code for logging to stderr with multiple log levels,
 * and for reporting errors.
 */

unsigned int verbosity = 0;

#define LOG_ALWAYS    0
#define LOG_OFTEN     1
#define LOG_SOMETIMES 2
#define LOG_SELDOM    3
#define LOG_RARELY    4

ATTRIBUTE_FORMAT((printf, 2, 0))
static void vlog(unsigned int level, const char *format, va_list args)
{
  if (level <= verbosity) {
    fflush(stderr); /* sync */
    fprintf(stderr, "log %d: ", level);
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");
  }
}

ATTRIBUTE_FORMAT((printf, 2, 3))
static void evlog(unsigned int level, const char *format, ...)
{
  va_list args;
  va_start(args, format);
  vlog(level, format, args);
  va_end(args);
}

ATTRIBUTE_FORMAT((printf, 1, 2))
static void error(const char *format, ...)
{
  va_list args;
  fprintf(stderr, "Fatal error: ");
  va_start(args, format);
  vlog(LOG_ALWAYS, format, args);
  va_end(args);
  exit(1);
}

static void sqlite_error(int res, sqlite3 *db, const char *format, ...)
{
  va_list args;
  evlog(LOG_ALWAYS, "Fatal SQL error %d", res);
  va_start(args, format);
  vlog(LOG_ALWAYS, format, args);
  va_end(args);
  evlog(LOG_ALWAYS, "SQLite message: %s\n", sqlite3_errmsg(db));
  exit(1);
}

/* global control variables set by command-line parameters. */

static const char *prog; /* program name */
static int rebuild = FALSE;
static int deleteDatabase = FALSE;
static int runTests = FALSE;
static int force = FALSE;
static int progress = FALSE;
static const char *databaseName = NULL;
static const char *logFileName = NULL;

static void usage(void)
{
  fprintf(stderr,
          "Usage: %s [-rfdvt] [-i <logfile>] [-o <database>]\n"
          "    -h (help)    : this message.\n"
          "    -r (rebuild) : re-create glue tables.\n"
          "    -f (force)   : ignore previous import of same logfile.\n"
          "    -d (delete)  : delete and recreate database file.\n"
          "    -v (verbose) : increase logging to stderr.\n"
          "    -p (progress): show progress with dots to stdout.\n"
          "    -t (test)    : run self-tests.\n"
          "    -i <logfile> : read logfile (defaults to stdin)\n"
          "    -o <database>: write database (defaults to\n"
          "                   "
          DATABASE_NAME_ENVAR " or " DEFAULT_DATABASE_NAME ").\n",
          prog);
}

static void usageError(void)
{
  usage();
  error("Bad usage");
}

/* parseArgs -- parse command line arguments */

static void parseArgs(int argc, char *argv[])
{
  int i = 1;

  if (argc >= 1)
    prog = argv[0];
  else
    prog = "unknown";

  while(i < argc) { /* consider argument i */
    if (argv[i][0] == '-') { /* it's an option argument */
      char *p = argv[i] + 1;
      while(*p) {
        switch (*p) {
        case 'v': /* verbosity */
          ++ verbosity;
          break;
        case 'p': /* progress */
          progress = TRUE;
          break;
        case 'r': /* rebuild */
          rebuild = TRUE;
          break;
        case 'd': /* rebuild */
          deleteDatabase = TRUE;
          break;
        case 'f': /* force */
          force = TRUE;
          break;
        case 't': /* run tests */
          runTests = TRUE;
          break;
        case 'i': /* input (log file) name */
          if (p[1] == '\0') { /* last character in this arg; name is next arg */
            logFileName = argv[i+1];
            ++ i;
          } else { /* not last character in arg; name is rest of arg */
            logFileName = p+1;
          }
          goto next_i;
        case 'o': /* output (database file) name */
          if (p[1] == '\0') { /* last character in this arg; name is next arg */
            databaseName = argv[i+1];
            ++ i;
          } else { /* not last character in arg; name is rest of arg */
            databaseName = p+1;
          }
          goto next_i;
        case 'h':
          usage();
          exit(EXIT_SUCCESS);
        default:
          usageError();
        }
        ++ p;
      }
    } else { /* not an option argument */
      usageError();
    }
  next_i:
    ++ i;
  }
  if (verbosity > LOG_ALWAYS)
    progress = TRUE;
}

/* openDatabase(p) opens the database file and returns a SQLite 3
 * database connection object. */

static sqlite3 *openDatabase(void)
{
  sqlite3 *db;
  int res;

  if (!databaseName) {
    databaseName = getenv(DATABASE_NAME_ENVAR);
    if(!databaseName)
      databaseName = DEFAULT_DATABASE_NAME;
  }

  if (deleteDatabase) {
    res = remove(databaseName);
    if (res)
      evlog(LOG_ALWAYS, "Could not remove database file %s", databaseName);
    else
      evlog(LOG_OFTEN, "Removed database file %s", databaseName);
  }
          
  res = sqlite3_open_v2(databaseName,
                        &db,
                        SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE,
                        NULL); /* use default sqlite_vfs object */
        
  if (res != SQLITE_OK)
    sqlite_error(res, db, "Opening %s failed", databaseName);

  evlog(LOG_OFTEN, "Writing to %s.",databaseName);
        
  return db;
}

/* closeDatabase(db) closes the database opened by openDatabase(). */

static void closeDatabase(sqlite3 *db)
{
  int res = sqlite3_close(db);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "Closing database failed"); 
  evlog(LOG_SOMETIMES, "Closed %s.", databaseName);
}

/* Utility functions for SQLite statements. */

static sqlite3_stmt *prepareStatement(sqlite3 *db,
                                      const char *sql)
{
  int res;
  sqlite3_stmt *statement;
  evlog(LOG_SELDOM, "Preparing statement %s", sql);
  res = sqlite3_prepare_v2(db, sql,
                           -1, /* prepare whole string as statement */
                           &statement,
                           NULL);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "statement preparation failed: %s", sql);
  return statement;
}

static void finalizeStatement(sqlite3 *db,
                              sqlite3_stmt *statement)
{
  int res;
  res = sqlite3_finalize(statement);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "statement finalize failed");
}

static void runStatement(sqlite3 *db,
                         const char *sql,
                         const char *description)
{
  int res;
  evlog(LOG_SELDOM, "%s: %s", description, sql);
  res = sqlite3_exec(db,
                     sql,
                     NULL, /* No callback */
                     NULL, /* No callback closure */
                     NULL); /* error messages handled by sqlite_error */
  if (res != SQLITE_OK)
    sqlite_error(res, db, "%s failed - statement %s", description, sql);
}

/* Test for the existence of a table using sqlite_master table.
 */

static int tableExists(sqlite3* db, const char *tableName)
{
  int res;
  int exists = 0;
  sqlite3_stmt *statement = NULL;

  statement = prepareStatement(db,
                               "SELECT 1 FROM sqlite_master WHERE type='table' AND name=?");
  res = sqlite3_bind_text(statement, 1, tableName, -1, SQLITE_STATIC);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "table existence bind of name failed.");
  res = sqlite3_step(statement);
  switch(res) {
  case SQLITE_DONE:
    exists = 0;
    break;
  case SQLITE_ROW:
    exists = 1;
    break;
  default:
    sqlite_error(res, db, "select from sqlite_master failed.");
  }
  finalizeStatement(db, statement);
  return exists;
}

/* Unit test for tableExists() */

static struct {
  const char* name;
  int exists;
} tableTests[] = {
  {"event_kind", TRUE},
  {"spong", FALSE},
  {"EVENT_SegSplit", TRUE}
};

static void testTableExists(sqlite3 *db)
{
  size_t i;
  int defects = 0;
  int tests = 0;
  for (i=0; i < (sizeof(tableTests)/sizeof(tableTests[0])); ++i) {
    const char *name = tableTests[i].name;
    int exists = tableExists(db, name);
    if (exists)
      evlog(LOG_OFTEN, "Table exists: %s", name);
    else 
      evlog(LOG_OFTEN, "Table does not exist: %s", name);
    if (exists != tableTests[i].exists) {
      evlog(LOG_ALWAYS, "tableExists test failed on table %s", name);
      ++ defects;
    }
    ++ tests;
  }
  evlog(LOG_ALWAYS, "%d tests, %d defects found.", tests, defects);
}

/* Every time we put events from a log file into a database file, we
 * add the log file to the event_log table, and get a serial number
 * from SQL which is then attached to all event rows from that log.
 * We use this to record overall SQL activity, to deter mistaken
 * attempts to add the same log file twice, and to allow events from
 * several different log files to share the same SQL file.
 *
 * When reading events from stdin, we can't so easily avoid the
 * duplication (unless we, e.g., take a hash of the event set); we
 * have to assume that the user is smart enough not to do that.
 */

static int64 logSerial = 0;

static void registerLogFile(sqlite3 *db,
                            const char *filename)
{
  sqlite3_stmt *statement;
  int res;
  const unsigned char *name;
  int64 completed;
  int64 file_size;
  int64 file_modtime;

  if (filename) {
    struct stat st;
    res = stat(filename, &st);
    if (res != 0)
      error("Couldn't stat() %s", filename);
    file_size = st.st_size;
    file_modtime = st.st_mtime;
                        
    statement = prepareStatement(db,
                                 "SELECT name, serial, completed FROM event_log"
                                 " WHERE size = ? AND modtime = ?");
    res = sqlite3_bind_int64(statement, 1, file_size);
    if (res != SQLITE_OK)
      sqlite_error(res, db, "event_log bind of size failed.");
    res = sqlite3_bind_int64(statement, 2, file_modtime);
    if (res != SQLITE_OK)
      sqlite_error(res, db, "event_log bind of modtime failed.");
    res = sqlite3_step(statement); 
    switch(res) {
    case SQLITE_DONE:
      evlog(LOG_SOMETIMES, "No log file matching '%s' found in database.", filename);
      break;
    case SQLITE_ROW:
      name = sqlite3_column_text(statement, 0);
      logSerial = sqlite3_column_int64(statement, 1);
      completed = sqlite3_column_int64(statement, 2);
      evlog(force ? LOG_OFTEN : LOG_ALWAYS, "Log file matching '%s' already in event_log, named \"%s\" (serial %llu, completed %llu).",
            filename, name, logSerial, completed);
      if (force) {
        evlog(LOG_OFTEN, "Continuing anyway because -f specified.");
      } else {
        evlog(LOG_ALWAYS, "Exiting.  Specify -f to force events into SQL anyway.");
        exit(0);
      }
      break;
    default:
      sqlite_error(res, db, "select from event_log failed.");
    }
    finalizeStatement(db, statement);
  } else { /* stdin */
    filename = "<stdin>";
    file_size = 0;
    file_modtime = 0;
  }
  statement = prepareStatement(db,
                               "INSERT into event_log (name, size, modtime, completed)"
                               " VALUES (?, ?, ?, 0)");
  res = sqlite3_bind_text(statement, 1, filename, -1, SQLITE_STATIC);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event_log insert bind of name failed.");
  res = sqlite3_bind_int64(statement, 2, file_size);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event_log insert bind of size failed.");
  res = sqlite3_bind_int64(statement, 3, file_modtime);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event_log insert bind of modtime failed.");
  res = sqlite3_step(statement); 
  if (res != SQLITE_DONE)
    sqlite_error(res, db, "insert into event_log failed.");
  logSerial = sqlite3_last_insert_rowid(db);
  evlog(LOG_SOMETIMES, "Log file %s added to event_log with serial %llu",
        filename, logSerial);
  finalizeStatement(db, statement);
}

static void logFileCompleted(sqlite3 *db,
                             int64 completed)
{
  sqlite3_stmt *statement;
  int res;

  statement = prepareStatement(db,
                               "UPDATE event_log SET completed=? WHERE serial=?");
  res = sqlite3_bind_int64(statement, 2, logSerial);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event_log update bind of serial failed.");
  res = sqlite3_bind_int64(statement, 1, completed);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event_log update bind of completed failed.");
  res = sqlite3_step(statement); 
  if (res != SQLITE_DONE)
    sqlite_error(res, db, "insert into event_log failed.");
  evlog(LOG_SOMETIMES, "Marked in event_log: %llu events", completed);
  finalizeStatement(db, statement);
}

/* Macro magic to make a CREATE TABLE statement for each event type. */

#define EVENT_PARAM_SQL_TYPE_A "INTEGER"
#define EVENT_PARAM_SQL_TYPE_P "INTEGER"
#define EVENT_PARAM_SQL_TYPE_U "INTEGER"
#define EVENT_PARAM_SQL_TYPE_W "INTEGER"
#define EVENT_PARAM_SQL_TYPE_D "REAL   "
#define EVENT_PARAM_SQL_TYPE_S "TEXT   "
#define EVENT_PARAM_SQL_TYPE_B "INTEGER"

#define EVENT_PARAM_SQL_COLUMN(X, index, sort, ident) \
        "\"" #ident "\" " EVENT_PARAM_SQL_TYPE_##sort ", "

#define EVENT_TABLE_CREATE(X, name, code, always, kind) \
        "CREATE TABLE IF NOT EXISTS EVENT_" #name " ( " \
        EVENT_##name##_PARAMS(EVENT_PARAM_SQL_COLUMN, X) \
        "time INTEGER, " \
        "log_serial INTEGER)",

/* An array of table-creation statement strings. */

const char *createStatements[] = {
  "CREATE TABLE IF NOT EXISTS event_kind (name    TEXT,"
  "                                       description TEXT,"
  "                                       enum    INTEGER PRIMARY KEY)",

  "CREATE TABLE IF NOT EXISTS event_type (name    TEXT,"
  "                                       code    INTEGER PRIMARY KEY,"
  "                                       always  INTEGER,"
  "                                       kind    INTEGER,"
  "  FOREIGN KEY (kind) REFERENCES event_kind(enum));",

  "CREATE TABLE IF NOT EXISTS event_param (type   INTEGER,"
  "                                        param_index   INTEGER,"
  "                                        sort    TEXT,"
  "                                        ident   TEXT,"
  "  FOREIGN KEY (type) REFERENCES event_type(code));",

  "CREATE TABLE IF NOT EXISTS event_log (name TEXT,"
  "                                      size INTEGER,"
  "                                      modtime INTEGER,"
  "                                      completed INTEGER,"
  "                                      serial INTEGER PRIMARY KEY AUTOINCREMENT)",

  EVENT_LIST(EVENT_TABLE_CREATE, X)
};

/* makeTables makes all the tables. */

static void makeTables(sqlite3 *db)
{
  size_t i;
  evlog(LOG_SOMETIMES, "Creating tables.");
        
  for (i=0; i < (sizeof(createStatements)/sizeof(createStatements[0])); ++i) {
    runStatement(db, createStatements[i], "Table creation");
  }
}

const char *glueTables[] = {
  "event_kind",
  "event_type",
  "event_param",
};

static void dropGlueTables(sqlite3 *db)
{
  size_t i;
  int res;
  char sql[1024];

  evlog(LOG_ALWAYS, "Dropping glue tables so they are rebuilt.");
        
  for (i=0; i < (sizeof(glueTables)/sizeof(glueTables[0])); ++i) {
    evlog(LOG_SOMETIMES, "Dropping table %s", glueTables[i]);
    sprintf(sql, "DROP TABLE %s", glueTables[i]);
    res = sqlite3_exec(db,
                       sql,
                       NULL, /* No callback */
                       NULL, /* No callback closure */
                       NULL); /* error messages handled by sqlite_error */
    /* Don't check for errors. */
    (void)res;
  }
}

/* Populate the metadata "glue" tables event_kind, event_type, and
 * event_param. */

#define EVENT_KIND_DO_INSERT(X, name, description)    \
        res = sqlite3_bind_text(statement, 1, #name, -1, SQLITE_STATIC); \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_kind bind of name \"" #name "\" failed."); \
        res = sqlite3_bind_text(statement, 2, description, -1, SQLITE_STATIC); \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_kind bind of description \"" description "\" failed."); \
        res = sqlite3_bind_int(statement, 3, i);                        \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_kind bind of enum %d failed.", i); \
        ++i;                                                            \
        res = sqlite3_step(statement);                                  \
        if (res != SQLITE_DONE)                                         \
                sqlite_error(res, db, "event_kind insert of name \"" #name "\" failed."); \
        if (sqlite3_changes(db) != 0)                                   \
                evlog(LOG_SOMETIMES, "Insert of event_kind row for \"" #name "\" affected %d rows.", sqlite3_changes(db)); \
        res = sqlite3_reset(statement);                                 \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "Couldn't reset event_kind insert statement.");

#define EVENT_TYPE_DO_INSERT(X, name, code, always, kind)          \
        res = sqlite3_bind_text(statement, 1, #name, -1, SQLITE_STATIC); \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_type bind of name \"" #name "\" failed."); \
        res = sqlite3_bind_int(statement, 2, code);                     \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_type bind of code %d failed.", code); \
        res = sqlite3_bind_int(statement, 3, always);                   \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_type bind of always for name \"" #name "\" failed."); \
        res = sqlite3_bind_int(statement, 4, EventKind##kind);          \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_type bind of kind for name \"" #name "\" failed."); \
        res = sqlite3_step(statement);                                  \
        if (res != SQLITE_DONE)                                         \
                sqlite_error(res, db, "event_type insert of name \"" #name "\" failed."); \
        if (sqlite3_changes(db) != 0)                                   \
                evlog(LOG_SOMETIMES, "Insert of event_type row for \"" #name "\" affected %d rows.", sqlite3_changes(db)); \
        res = sqlite3_reset(statement);                                 \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "Couldn't reset event_type insert statement.");

#define EVENT_PARAM_DO_INSERT(code, index, sort, ident)   \
        res = sqlite3_bind_int(statement, 1, code);                     \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_param bind of code %d failed.", code); \
        res = sqlite3_bind_int(statement, 2, index);                     \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_param bind of index %d failed.", index); \
        res = sqlite3_bind_text(statement, 3, #sort, -1, SQLITE_STATIC); \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_type bind of sort \"" #sort "\" failed."); \
        res = sqlite3_bind_text(statement, 4, #ident, -1, SQLITE_STATIC); \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "event_type bind of ident \"" #ident "\" failed."); \
        res = sqlite3_step(statement);                                  \
        if (res != SQLITE_DONE)                                         \
                sqlite_error(res, db, "event_param insert of ident \"" #ident "\" for code %d failed.", code); \
        if (sqlite3_changes(db) != 0)                                   \
                evlog(LOG_SOMETIMES, "Insert of event_param row for code %d,  ident \"" #ident "\" affected %d rows.", code, sqlite3_changes(db)); \
        res = sqlite3_reset(statement);                                 \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "Couldn't reset event_param insert statement.");

#define EVENT_TYPE_INSERT_PARAMS(X, name, code, always, kind) \
        EVENT_##name##_PARAMS(EVENT_PARAM_DO_INSERT, code)

static void fillGlueTables(sqlite3 *db)
{
  int i;
  sqlite3_stmt *statement;
  int res;
                
  statement = prepareStatement(db,
                               "INSERT OR IGNORE INTO event_kind (name, description, enum)"
                               "VALUES (?, ?, ?)");
        
  i = 0;
  EventKindENUM(EVENT_KIND_DO_INSERT, X);
        
  finalizeStatement(db, statement);
        
  statement = prepareStatement(db, 
                               "INSERT OR IGNORE INTO event_type (name, code, always, kind)"
                               "VALUES (?, ?, ?, ?)");
  EVENT_LIST(EVENT_TYPE_DO_INSERT, X);
        
  finalizeStatement(db, statement);

  statement = prepareStatement(db,
                               "INSERT OR IGNORE INTO event_param (type, param_index, sort, ident)"
                               "VALUES (?, ?, ?, ?)");
  EVENT_LIST(EVENT_TYPE_INSERT_PARAMS, X);
        
  finalizeStatement(db, statement);
}

/* Populate the actual event tables. */

#define EVENT_TYPE_DECLARE_STATEMENT(X, name, code, always, kind) \
        sqlite3_stmt *stmt_##name;

#define EVENT_PARAM_PREPARE_IDENT(X, index, sort, ident) "\"" #ident "\", "

#define EVENT_PARAM_PREPARE_PLACE(X, index, sort, ident) "?, "

#define EVENT_TYPE_PREPARE_STATEMENT(X, name, code, always, kind) \
        stmt_##name = \
            prepareStatement(db, \
                             "INSERT INTO EVENT_" #name " (" \
                             EVENT_##name##_PARAMS(EVENT_PARAM_PREPARE_IDENT, X)        \
                             "log_serial, time) VALUES (" \
                             EVENT_##name##_PARAMS(EVENT_PARAM_PREPARE_PLACE,X) \
                             "?, ?)");

#define EVENT_TYPE_FINALIZE_STATEMENT(X, name, code, always, kind) \
        finalizeStatement(db, stmt_##name);

#define EVENT_PARAM_BIND_A bind_int
#define EVENT_PARAM_BIND_P bind_int
#define EVENT_PARAM_BIND_U bind_int
#define EVENT_PARAM_BIND_W bind_int
#define EVENT_PARAM_BIND_D bind_real   
#define EVENT_PARAM_BIND_S bind_text   
#define EVENT_PARAM_BIND_B bind_int

#define EVENT_PARAM_BIND(X, index, sort, ident) \
        p = EVENT_PARAM_BIND_##sort (db, statement, eventCount, index+1, p); \
        last_index = index+1;

#define EVENT_TYPE_WRITE_SQL(X, name, code, always, kind) \
        case code: \
                statement = stmt_##name; \
                /* bind all the parameters of this particular event with macro magic. */ \
                EVENT_##name##_PARAMS(EVENT_PARAM_BIND, X) \
                break;

static char *bind_int(sqlite3 *db, sqlite3_stmt *stmt, int64 count, int field, char *p)
{
  char *q;
  long long val;
  int res;

  while(*p == ' ')
    ++p;

  val = strtoll(p, &q, 16);
  if (q == p)
    error("event %llu field %d not an integer: %s",
          count, field, p);

  res = sqlite3_bind_int64(stmt, field, val);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event %llu field %d bind failed", count, field);
  return q;
}

static char *bind_real(sqlite3 *db, sqlite3_stmt *stmt, int64 count, int field, char *p)
{
  char *q;
  double val;
  int res;

  while(*p == ' ')
    ++p;

  val = strtod(p, &q);
  if (q == p)
    error("event %llu field %d not a floating-point value: %s",
          count, field, p);

  res = sqlite3_bind_double(stmt, field, val);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event %llu field %d bind failed", count, field);
  return q;
}

static char *bind_text(sqlite3 *db, sqlite3_stmt *stmt, int64 count, int field, char *p)
{
  char *q;
  int res;

  while(*p == ' ')
    ++p;

  q = p;
  while((*q != '\n') && (*q != '\0')) {
    ++ q;
  }
  if ((q == p) || (q[-1] != '"'))
    error("event %llu string field %d has no closing quote mark.",
          count, field);

  res = sqlite3_bind_text(stmt, field, p, (int)(q-p-1), SQLITE_STATIC);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event %llu field %d bind failed", count, field);
  return q;
}

/* this is overkill, at present. */

#define MAX_LOG_LINE_LENGTH 1024

/* readLog -- read and parse log.  Returns the number of events written.
 */

static int64 readLog(FILE *input,
                     sqlite3 *db)
{
  int64 eventCount = 0;

  /* declare statements for every event type */
  EVENT_LIST(EVENT_TYPE_DECLARE_STATEMENT, X);

  /* prepare statements for every event type */
  EVENT_LIST(EVENT_TYPE_PREPARE_STATEMENT, X);

  runStatement(db, "BEGIN", "Transaction start");

  while (TRUE) { /* loop for each event */
    char line[MAX_LOG_LINE_LENGTH];
    char *p;
    char *q;
    int last_index=0;
    sqlite3_stmt *statement = NULL;
    int res;
    int64 clock_field;
    long code;

    p = fgets(line, MAX_LOG_LINE_LENGTH, input);
    if (!p) {
      if (feof(input))
        break;
      else
        error("Couldn't read line after event %llu", eventCount);
    }

    eventCount++;

    clock_field = strtoll(p, &q, 16);

    if (q == p)
      error("event %llu clock field not a hex integer: %s",
            eventCount, p);

    if (*q != ' ')
      error("event %llu code field not preceded by ' ': %s",
            eventCount, q);
    while(*q == ' ')
      ++q;

    p = q;
    code = strtol(p, &q, 16);
    if (q == p)
      error("event %llu code field not an integer: %s",
            eventCount, p);
    p = q;

    /* Write event to SQLite. */
    switch (code) {
      /* this macro sets statement and last_index */
      EVENT_LIST(EVENT_TYPE_WRITE_SQL, X);
    default:
      error("Event %llu has Unknown event code %ld", eventCount, code);
    }
    /* bind the fields we store for every event */ \
    res = sqlite3_bind_int64(statement, last_index+1, logSerial);
    if (res != SQLITE_OK)
      sqlite_error(res, db, "Event %llu bind of log_serial failed.", eventCount);
    res = sqlite3_bind_int64(statement, last_index+2, clock_field);
    if (res != SQLITE_OK)
      sqlite_error(res, db, "Event %llu bind of clock failed.", eventCount);
    res = sqlite3_step(statement);
    if (res != SQLITE_DONE)
      sqlite_error(res, db, "insert of event %llu failed.", eventCount);
    res = sqlite3_reset(statement);
    if (res != SQLITE_OK)
      sqlite_error(res, db, "Couldn't reset insert statement of event %llu", eventCount);

    if (progress) {
      if ((eventCount % SMALL_TICK) == 0) {
        printf(".");
        fflush(stdout);
        if (((eventCount / SMALL_TICK) % BIG_TICK) == 0) {
          printf("\n");
          fflush(stdout);
          evlog(LOG_SOMETIMES, "%lu events.", (unsigned long)eventCount);
        }
      }
    }
  }
  if (progress) {
    printf("\n");
    fflush(stdout);
  }
  runStatement(db, "COMMIT", "Transaction finish");
  logFileCompleted(db, eventCount);

  /* finalize all the statements */
  EVENT_LIST(EVENT_TYPE_FINALIZE_STATEMENT, X);

  return eventCount;
}

/* openLog -- open the log file doors, HAL */

static FILE *openLog(sqlite3 *db)
{
  FILE *input;

  registerLogFile(db, logFileName);
  if (!logFileName) {
    input = stdin;
    logFileName = "<stdin>";
  } else {
    input = fopen(logFileName, "r");
    if (input == NULL)
      error("unable to open %s", logFileName);
  }

  evlog(LOG_OFTEN, "Reading %s.", logFileName ? logFileName : "standard input");

  return input;
}

static int64 writeEventsToSQL(sqlite3 *db)
{
  FILE *input;
  int64 count;
  input = openLog(db);
  count = readLog(input, db);
  (void)fclose(input);
  return count;
}


int main(int argc, char *argv[])
{
  sqlite3 *db;
  int64 count;

  parseArgs(argc, argv);
        
  db = openDatabase();
  if (rebuild) {
    dropGlueTables(db);
  }
  makeTables(db);
  fillGlueTables(db);
  count = writeEventsToSQL(db);
  evlog(LOG_ALWAYS, "Imported %llu events from %s to %s, serial %llu.",
        count, logFileName, databaseName, logSerial);

  if (runTests) {
    /* TODO: more unit tests in here */
    testTableExists(db);
  }

  closeDatabase(db);
  return 0;
}

/* COPYRIGHT AND LICENSE
 *
 * Copyright (c) 2012-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

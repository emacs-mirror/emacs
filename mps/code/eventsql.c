#include "config.h"

#include "eventdef.h"
#include "eventpro.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sqlite3.h>
#include <sys/stat.h>

#define DATABASE_NAME_ENVAR   "MPS_EVENT_DATABASE"
#define DEFAULT_DATABASE_NAME "mpsevent.db"

#define TELEMETRY_FILENAME_ENVAR "MPS_TELEMETRY_FILENAME"
#define DEFAULT_TELEMETRY_FILENAME "mpsio.log"

/* we output rows of dots.  One dot per SMALL_TICK events,
 * BIG_TICK dots per row. */

#define SMALL_TICK 1000
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

static void vlog(unsigned int level, const char *format, va_list args)
{
        if (level <= verbosity) {
                fflush(stderr); /* sync */
                fprintf(stderr, "log %d: ", level);
                vfprintf(stderr, format, args);
                fprintf(stderr, "\n");
        }
}

static void log(unsigned int level, const char *format, ...)
{
        va_list args;
        va_start(args, format);
        vlog(level, format, args);
        va_end(args);
}

static void error(const char *format, ...)
{
        va_list args;
        fprintf(stderr, "Fatal error:  ");
        va_start(args, format);
        vlog(LOG_ALWAYS, format, args);
        va_end(args);
        exit(1);
}

static void sqlite_error(int res, sqlite3 *db, const char *format, ...)
{
        log(LOG_ALWAYS, "Fatal SQL error %d", res);
        va_list args;
        va_start(args, format);
        vlog(LOG_ALWAYS, format, args);
        va_end(args);
        log(LOG_ALWAYS, "SQLite message: %s\n", sqlite3_errmsg(db));
        exit(1);
}

static char *prog; /* program name */
static int rebuild = FALSE;
static int runTests = FALSE;
static int force = FALSE;
static char *databaseName = NULL;
static char *logFileName = NULL;

static void usage(void)
{
  fprintf(stderr,
          "Usage: %s [-rtf] [-l <logfile>] [-d <database>] [-v -v -v ...]\n",
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

  while (i < argc) { /* consider argument i */
    if (argv[i][0] == '-') { /* it's an option argument */
      switch (argv[i][1]) {
      case 'v': /* verbosity */
        ++ verbosity;
        break;
      case 'r': /* rebuild */
        rebuild = TRUE;
        break;
      case 'f': /* force */
        force = TRUE;
        break;
      case 't': /* run tests */
        runTests = TRUE;
        break;
      case 'l': /* log file name */
        ++ i;
        if (i == argc)
          usageError();
        else
          logFileName = argv[i];
        break;
      case 'd': /* database file name */
        ++ i;
        if (i == argc)
          usageError();
        else
          databaseName = argv[i];
        break;
      case '?': case 'h': /* help */
        usage();
        break;
      default:
        usageError();
      }
    } /* if option */
    ++ i;
  }
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
        res = sqlite3_open_v2(databaseName,
                              &db,
                              SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE,
                              NULL); /* use default sqlite_vfs object */
        
        if (res != SQLITE_OK)
                sqlite_error(res, db, "Opening %s failed", databaseName);

        log(LOG_ALWAYS, "Writing to  %s.",databaseName);
        
        return db;
}

/* closeDatabase(db) closes the database opened by openDatabase(). */

static void closeDatabase(sqlite3 *db)
{
        int res = sqlite3_close(db);
        if (res != SQLITE_OK)
                sqlite_error(res, db, "Closing database failed"); 
        log(LOG_ALWAYS, "Closed %s.", databaseName);
}

/* We need to be able to test for the existence of a table.  The
 * SQLite3 API seems to have no way to explore metadata like this,
 * unless it is compiled in a particular way (in which case the
 * function sqlite3_table_column_metadata could be used).  Without
 * that assistance, we can use a simple SQL trick (which could also
 * tell us the number of rows in the table if we cared).
 *
 * TODO: Update this to use the sqlite_master table:
 *    SELECT FROM sqlite_master WHERE type='table' AND name=?
 * and fix the above comment.
 */

static int tableExists(sqlite3* db, const char *tableName)
{
        const char *format = "SELECT SUM(1) FROM %s";
        char *sql;
        int res;

        sql = malloc(strlen(format) + strlen(tableName));
        if (!sql)
                error("Out of memory.");
        sprintf(sql, format, tableName);
        log(LOG_RARELY, "Testing for existence of table '%s' with SQL: %s", tableName, sql);
        res = sqlite3_exec(db,
                           sql,
                           NULL, /* put in a callback here if we really want to know the number of rows */
                           NULL, /* callback closure */
                           NULL); /* error messages handled by sqlite_error */
        free(sql);

        switch(res) {
        case SQLITE_OK:
                log(LOG_RARELY, "Table '%s' exists.", tableName);

                return 1; /* table exists */
                break;
        case SQLITE_ERROR:
                log(LOG_RARELY, "Table '%s' does not exist.", tableName);
                return 0; /* table does not exist; we can
                             probably do a better test for this case. */
                break;
        default:
                sqlite_error(res, db, "Table test failed: %s", tableName);
        }
        /* UNREACHED */
        return 0;
}

/* Unit test for tableExists() */

static const char *tableTests[] = {
        "event_kind",
        "spong",
        "EVENT_SegSplit",
};

static void testTableExists(sqlite3 *db)
{
        int i;
        for (i=0; i < (sizeof(tableTests)/sizeof(tableTests[0])); ++i) {
                if (tableExists(db, tableTests[i]))
                        printf("Table exists: %s\n", tableTests[i]);
                else 
                        printf("Table does not exist: %s\n", tableTests[i]);
        }
}

/* Utility functions for SQLite statements. */

static sqlite3_stmt *prepareStatement(sqlite3 *db,
                                      const char *sql)
{
        int res;
        sqlite3_stmt *statement;
        log(LOG_SELDOM, "Preparing statement %s", sql);
        res = sqlite3_prepare_v2(db, sql,
                                 -1, /* prepare whole string as statement */
                                 &statement,
                                 NULL);
        if (res != SQLITE_OK)
                sqlite_error(res, db, "statementpreparation failed: %s", sql);
        return statement;
}

static void finalizeStatement(sqlite3 *db,
                              sqlite3_stmt *statement)
{
        int res;
        res = sqlite3_finalize(statement);
        if (res != SQLITE_OK)
                sqlite_error(res, db, "event_type finalize failed");
}

static void runStatement(sqlite3 *db,
                         const char *sql,
                         const char *description)
{
        int res;
        res = sqlite3_exec(db,
                           sql,
                           NULL, /* No callback */
                           NULL, /* No callback closure */
                           NULL); /* error messages handled by sqlite_error */
        if (res != SQLITE_OK)
                sqlite_error(res, db, "%s failed - statement %s", description, sql);
}


/* Every time we put events from a log file into a database file, we
 * add the log file to the event_log table, and get a serial number
 * from SQL which is then attached to all event rows from that log.
 * We use this to record overall SQL activity, to deter mistaken
 * attempts to add the same log file twice, and to allow events from
 * several different log files to share the same SQL file. */

static unsigned long logSerial = 0;

static void registerLogFile(sqlite3 *db,
                            const char *filename)
{
        struct stat st;
        sqlite3_stmt *statement;
        int res;
        const unsigned char *name;
        unsigned long completed;

        res = stat(filename, &st);
        if (res != 0)
                error("Couldn't stat() %s", filename);
                
        statement = prepareStatement(db,
                                     "SELECT name, serial, completed FROM event_log"
                                     " WHERE file_id = ? AND size = ? AND modtime = ?");
        res = sqlite3_bind_int64(statement, 1, st.st_ino);
        if (res != SQLITE_OK)
                sqlite_error(res, db, "event_log bind of file_id failed.");
        res = sqlite3_bind_int64(statement, 2, st.st_size);
        if (res != SQLITE_OK)
                sqlite_error(res, db, "event_log bind of size failed.");
        res = sqlite3_bind_int64(statement, 3, st.st_mtime);
        if (res != SQLITE_OK)
                sqlite_error(res, db, "event_log bind of modtime failed.");

        res = sqlite3_step(statement); 
        switch(res) {
        case SQLITE_DONE:
                log(LOG_SOMETIMES, "No log file matching '%s' found in database.", filename);
                break;
        case SQLITE_ROW:
                name = sqlite3_column_text(statement, 0);
                logSerial = sqlite3_column_int(statement, 1);
                completed = sqlite3_column_int(statement, 2);
                log(LOG_ALWAYS, "Log file matching '%s' already in event_log, named \"%s\" (serial %lu, completed %lu).",
                    filename, name, logSerial, completed);
                if (!force) {
                        log(LOG_ALWAYS, "Exiting.  Specify -f to force events into SQL anyway.");
                        exit(0);
                }
                log(LOG_ALWAYS, "Continuing anyway because -f specified.");
                break;
        default:
                sqlite_error(res, db, "select from event_log failed.");
        }
        finalizeStatement(db, statement);
        statement = prepareStatement(db,
                                     "INSERT into event_log (name, file_id, size, modtime, completed)"
                                     " VALUES (?, ?, ?, ?, 0)");
        res = sqlite3_bind_text(statement, 1, filename, -1, SQLITE_STATIC);
        if (res != SQLITE_OK)
                sqlite_error(res, db, "event_log insert bind of name failed.");
        res = sqlite3_bind_int64(statement, 2, st.st_ino);
        if (res != SQLITE_OK)
                sqlite_error(res, db, "event_log insert bind of file_id failed.");
        res = sqlite3_bind_int64(statement, 3, st.st_size);
        if (res != SQLITE_OK)
                sqlite_error(res, db, "event_log insert bind of size failed.");
        res = sqlite3_bind_int64(statement, 4, st.st_mtime);
        if (res != SQLITE_OK)
                sqlite_error(res, db, "event_log insert bind of modtime failed.");
        res = sqlite3_step(statement); 
        if (res != SQLITE_DONE)
                sqlite_error(res, db, "insert into event_log failed.");
        logSerial = sqlite3_last_insert_rowid(db);
        log(LOG_SOMETIMES, "Log file added to event_log with serial %lu",
            filename, logSerial);
        finalizeStatement(db, statement);
}

static void logFileCompleted(sqlite3 *db,
                             unsigned long completed)
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
        log(LOG_OFTEN, "Marked in event_log: %lu events", completed);
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

        "CREATE TABLE IF NOT EXISTS event_log (name TEXT,"
        "                                      file_id INTEGER,"
        "                                      size INTEGER,"
        "                                      modtime INTEGER,"
        "                                      completed INTEGER,"
        "                                      serial INTEGER PRIMARY KEY AUTOINCREMENT)",

EVENT_LIST(EVENT_TABLE_CREATE, X)
};

/* makeTables makes all the tables. */

static void makeTables(sqlite3 *db)
{
        int i;
        
        for (i=0; i < (sizeof(createStatements)/sizeof(createStatements[0])); ++i) {
                log(LOG_SOMETIMES, "Creating tables.  SQL command: %s", createStatements[i]);
                runStatement(db, createStatements[i], "Table creation");
        }
}

const char *glueTables[] = {
        "event_kind",
        "event_type",
};

static void dropGlueTables(sqlite3 *db)
{
        int i;
        int res;
        char sql[1024];

        log(LOG_ALWAYS, "Dropping glue tables so they are rebuilt.");
        
        for (i=0; i < (sizeof(glueTables)/sizeof(glueTables[0])); ++i) {
                log(LOG_SOMETIMES, "Dropping table %s", glueTables[i]);
                sprintf(sql, "DROP TABLE %s", glueTables[i]);
                res = sqlite3_exec(db,
                                   sql,
                                   NULL, /* No callback */
                                   NULL, /* No callback closure */
                                   NULL); /* error messages handled by sqlite_error */
                /* Don't check for errors. */
        }
}

/* Populate the metadata "glue" tables event_kind and event_type. */

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
                log(LOG_SOMETIMES, "Insert of event_kind row for \"" #name "\" affected %d rows.", sqlite3_changes(db)); \
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
                log(LOG_SOMETIMES, "Insert of event_type row for \"" #name "\" affected %d rows.", sqlite3_changes(db)); \
        res = sqlite3_reset(statement);                                 \
        if (res != SQLITE_OK)                                           \
                sqlite_error(res, db, "Couldn't reset event_type insert statement.");

static void fillGlueTables(sqlite3 *db)
{
        int i;
        Res res;
        sqlite3_stmt *statement;
                
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

#define EVENT_PARAM_BIND_INTEGER(name, index, sort, ident) \
        res = sqlite3_bind_int64(statement, index+1, (unsigned long) event->name.f##index);

#define EVENT_PARAM_BIND_REAL(name, index, sort, ident) \
        res = sqlite3_bind_double(statement, index+1, event->name.f##index);

#define EVENT_PARAM_BIND_TEXT(name, index, sort, ident) \
        res = sqlite3_bind_text(statement, index+1, event->name.f##index, -1, SQLITE_STATIC);

#define EVENT_PARAM_BIND_A EVENT_PARAM_BIND_INTEGER
#define EVENT_PARAM_BIND_P EVENT_PARAM_BIND_INTEGER
#define EVENT_PARAM_BIND_U EVENT_PARAM_BIND_INTEGER
#define EVENT_PARAM_BIND_W EVENT_PARAM_BIND_INTEGER
#define EVENT_PARAM_BIND_D EVENT_PARAM_BIND_REAL   
#define EVENT_PARAM_BIND_S EVENT_PARAM_BIND_TEXT   
#define EVENT_PARAM_BIND_B EVENT_PARAM_BIND_INTEGER

#define EVENT_PARAM_BIND(name, index, sort, ident) \
        EVENT_PARAM_BIND_##sort (name, index, sort, ident) \
        if (res != SQLITE_OK) \
                sqlite_error(res, db, "Event " #name " bind of ident " #ident "failed."); \
        last_index = index+1;


#define EVENT_TYPE_WRITE_SQL(X, name, code, always, kind) \
        case code: \
        { \
                sqlite3_stmt *statement = stmt_##name; \
                int last_index = 0; \
                int res; \
                /* bind all the parameters of this particular event with macro magic. */ \
                EVENT_##name##_PARAMS(EVENT_PARAM_BIND, name) \
                /* bind the fields we store for every event */ \
                res = sqlite3_bind_int64(statement, last_index+1, logSerial); \
                if (res != SQLITE_OK) \
                        sqlite_error(res, db, "Event " #name " bind of log_serial failed."); \
                res = sqlite3_bind_int64(statement, last_index+2, event->any.clock); \
                if (res != SQLITE_OK) \
                        sqlite_error(res, db, "Event " #name " bind of clock failed."); \
                res = sqlite3_step(statement); \
                if (res != SQLITE_DONE) \
                        sqlite_error(res, db, "insert of event \"" #name "\" failed."); \
                res = sqlite3_reset(statement); \
                if (res != SQLITE_OK) \
                        sqlite_error(res, db, "Couldn't reset insert statement for \"" #name "\"."); \
        } \
        break;

/* readLog -- read and parse log
 */

static void readLog(EventProc proc,
                    sqlite3 *db)
{
        size_t eventCount = 0;

        /* declare statements for every event type */
        EVENT_LIST(EVENT_TYPE_DECLARE_STATEMENT, X);

        /* prepare statements for every event type */
        EVENT_LIST(EVENT_TYPE_PREPARE_STATEMENT, X);

        runStatement(db, "BEGIN", "Transaction start");

        while (TRUE) { /* loop for each event */
                Event event;
                EventCode code;
                Res res;
                
                /* Read and parse event. */
                res = EventRead(&event, proc);
                if (res == ResFAIL) break; /* eof */
                if (res != ResOK) error("Truncated log");
                code = event->any.code;
                
                /* Write event to SQLite. */
                switch (code) {
                        EVENT_LIST(EVENT_TYPE_WRITE_SQL, X);
                }
                EventDestroy(proc, event);
                eventCount++;
                if (verbosity > LOG_ALWAYS) {
                        if ((eventCount % SMALL_TICK) == 0) {
                                printf(".");
                                fflush(stdout);
                                if (((eventCount / SMALL_TICK) % BIG_TICK) == 0) {
                                        log(LOG_OFTEN, "%lu events.", (unsigned long)eventCount);
                                }
                        }
                }
        }
        if (verbosity > LOG_ALWAYS) {
                printf("\n");
                fflush(stdout);
        }
        runStatement(db, "COMMIT", "Transaction finish");

        log(LOG_ALWAYS, "Wrote %lu events to SQL.", (unsigned long)eventCount);
        logFileCompleted(db, eventCount);

        /* finalize all the statements */
        EVENT_LIST(EVENT_TYPE_FINALIZE_STATEMENT, X);
}

static Res logReader(void *file, void *p, size_t len)
{
        size_t n;
        
        n = fread(p, 1, len, (FILE *)file);
        return (n < len) ? (feof((FILE *)file) ? ResFAIL : ResIO) : ResOK;
}

static FILE *openLog(sqlite3 *db)
{
        FILE *input;

        if (!logFileName) {
                logFileName = getenv(TELEMETRY_FILENAME_ENVAR);
                if(logFileName == NULL)
                        logFileName = DEFAULT_TELEMETRY_FILENAME;
        }

        registerLogFile(db, logFileName);
        input = fopen(logFileName, "rb");

        if (input == NULL)
                error("unable to open %s", logFileName);

        log(LOG_ALWAYS, "Reading %s.", logFileName);

        return input;
}

static void writeEventsToSQL(sqlite3 *db)
{
        Res res;
        EventProc proc;
        FILE *input;
        
        input = openLog(db);
        res = EventProcCreate(&proc, logReader, (void *)input);
        if (res != ResOK)
                error("Can't init EventProc module: error %d.", res);
        
        readLog(proc, db);

        EventProcDestroy(proc);
        (void)fclose(input);
}


int main(int argc, char *argv[])
{
        sqlite3 *db;

        parseArgs(argc, argv);
        
        db = openDatabase();
        if (rebuild) {
                dropGlueTables(db);
        }
        makeTables(db);
        fillGlueTables(db);
        writeEventsToSQL(db);

        if (runTests) {
                /* TODO: more unit tests in here */
                testTableExists(db);
        }

        closeDatabase(db);
        return 0;
}

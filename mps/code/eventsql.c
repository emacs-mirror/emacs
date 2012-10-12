#include "config.h"

#include "eventdef.h"
#include "eventpro.h"

#include <stdio.h>
#include <stdlib.h>
#include <sqlite3.h>

unsigned int verbosity = 4;

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

#if 0 /* UNUSED */

static void error(const char *format, ...)
{
  va_list args;
  fprintf(stderr, "Fatal error:  ");
  va_start(args, format);
  vlog(LOG_ALWAYS, format, args);
  va_end(args);
  exit(1);
}
#endif /* UNUSED */
  
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

static void openDatabase(sqlite3 **dbReturn)
{
  sqlite3 *db;
  int res;

  const char *filename = getenv("MPS_EVENT_DATABASE");
  if(filename == NULL)
    filename = "mpsevent.db";
  log(LOG_OFTEN, "Opening %s.", filename);

  res = sqlite3_open_v2(filename,
			&db,
			SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE,
			NULL); /* use default sqlite_vfs object */

  if (res != SQLITE_OK)
    sqlite_error(res, db, "Opening %s failed", filename);
  *dbReturn = db;
  return;
}

const char *createStatements[] = {
  "CREATE TABLE IF NOT EXISTS event_kind (name    TEXT,"
  "                                       description TEXT,"
  "                                       enum    INTEGER PRIMARY KEY)",
  "CREATE TABLE IF NOT EXISTS event_type (name    TEXT,"
  "                                       code    INTEGER PRIMARY KEY,"
  "                                       always  INTEGER,"
  "                                       kind    INTEGER,"
  "  FOREIGN KEY (kind) REFERENCES event_kind(enum));",
};

const char *populateStatements[] = {
};

#define EVENT_KIND_DO_INSERT(X, name, description)    \
  res = sqlite3_bind_text(statement, 1, #name, -1, SQLITE_STATIC); \
  if (res != SQLITE_OK) \
    sqlite_error(res, db, "event_kind bind of name \"" #name "\" failed."); \
  res = sqlite3_bind_text(statement, 2, description, -1, SQLITE_STATIC); \
  if (res != SQLITE_OK) \
    sqlite_error(res, db, "event_kind bind of description \"" description "\" failed."); \
  res = sqlite3_bind_int(statement, 3, i); \
  if (res != SQLITE_OK) \
    sqlite_error(res, db, "event_kind bind of enum %d failed.", i);	\
  ++i; \
  res = sqlite3_step(statement); \
  if (res != SQLITE_DONE) \
    sqlite_error(res, db, "event_kind insert of name \"" #name "\" failed."); \
  if (sqlite3_changes(db) != 0)\
    log(LOG_SOMETIMES, "Insert of event_kind row for \"" #name "\" affected %d rows.", sqlite3_changes(db)); \
  res = sqlite3_reset(statement); \
  if (res != SQLITE_OK) \
    sqlite_error(res, db, "Couldn't reset event_kind insert statement.");

#define EVENT_TYPE_DO_INSERT(X, name, code, always, kind)	   \
  res = sqlite3_bind_text(statement, 1, #name, -1, SQLITE_STATIC); \
  if (res != SQLITE_OK) \
    sqlite_error(res, db, "event_type bind of name \"" #name "\" failed."); \
  res = sqlite3_bind_int(statement, 2, code); \
  if (res != SQLITE_OK) \
    sqlite_error(res, db, "event_type bind of code %d failed.", code); \
  res = sqlite3_bind_int(statement, 3, always); \
  if (res != SQLITE_OK) \
    sqlite_error(res, db, "event_type bind of always for name \"" #name "\" failed."); \
  res = sqlite3_bind_int(statement, 4, EventKind##kind); \
  if (res != SQLITE_OK) \
    sqlite_error(res, db, "event_type bind of kind for name \"" #name "\" failed."); \
  res = sqlite3_step(statement); \
  if (res != SQLITE_DONE) \
    sqlite_error(res, db, "event_type insert of name \"" #name "\" failed."); \
  if (sqlite3_changes(db) != 0) \
    log(LOG_SOMETIMES, "Insert of event_type row for \"" #name "\" affected %d rows.", sqlite3_changes(db)); \
  res = sqlite3_reset(statement); \
  if (res != SQLITE_OK) \
    sqlite_error(res, db, "Couldn't reset event_type insert statement.");

static void fillTables(sqlite3 *db)
{
  int i;
  sqlite3_stmt *statement;

  int res = sqlite3_prepare_v2(db,
			       "INSERT OR IGNORE INTO event_kind (name, description, enum)"
			       "VALUES (?, ?, ?)",
			       -1, /* prepare whole string as statement */
			       &statement,
			       NULL);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event_kind preparation failed");

  i = 0;
  EventKindENUM(EVENT_KIND_DO_INSERT, X);

  res = sqlite3_finalize(statement);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event_kind finalize failed");

  res = sqlite3_prepare_v2(db,
			   "INSERT OR IGNORE INTO event_type (name, code, always, kind)"
			   "VALUES (?, ?, ?, ?)",
			   -1, /* prepare whole string as statement */
			   &statement,
			   NULL);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event_type preparation failed");

  EVENT_LIST(EVENT_TYPE_DO_INSERT, X);

  res = sqlite3_finalize(statement);
  if (res != SQLITE_OK)
    sqlite_error(res, db, "event_type finalize failed");
}

static void makeTables(sqlite3 *db)
{
  int i;

  for (i=0; i < (sizeof(createStatements)/sizeof(createStatements[0])); ++i) {
    log(LOG_SOMETIMES, "Creating tables.  SQL command: %s", createStatements[i]);
    int res = sqlite3_exec(db,
			   createStatements[i],
			   NULL, /* No callback */
			   NULL, /* No callback closure */
			   NULL); /* error messages handled by sqlite_error */
    if (res != SQLITE_OK)
      sqlite_error(res, db, "Table creation failed: %s", createStatements[i]);
  }
}


int main(void)
{
  sqlite3 *db;
  
  openDatabase(&db);
  makeTables(db);
  fillTables(db);
  return 0;
}


## Transaction Log functions

> source: [dbrnd.com](https://www.dbrnd.com/2017/12/postgresql-10-important-name-change-from-xlog-to-wal-and-location-to-lsn/)

* Before Version 10

```
## directories
pg_xlog
pg_clog

## functions
pg_current_xlog_flush_location
pg_current_xlog_insert_location
pg_current_xlog_location
pg_is_xlog_replay_paused
pg_last_xlog_receive_location
pg_last_xlog_replay_location
pg_switch_xlog
pg_xlog_location_diff
pg_xlog_replay_pause
pg_xlog_replay_resume
pg_xlogfile_name
pg_xlogfile_name_offset
```

* Version 10 onwards

```
## directories
pg_wal
pg_xact

## functions
pg_current_wal_flush_lsn
pg_current_wal_insert_lsn
pg_current_wal_lsn
pg_is_wal_replay_paused
pg_last_wal_receive_lsn
pg_last_wal_replay_lsn
pg_switch_wal
pg_wal_lsn_diff
pg_wal_replay_pause
pg_wal_replay_resume
pg_walfile_name
pg_walfile_name_offset
```

---

;;; pg.el --- Emacs Lisp socket-level interface to the PostgreSQL RDBMS
;;;
;;; Author: Eric Marsden <emarsden@mail.dotcom.fr>
;;; Version: 0.2
;;; Keywords: data comm database postgresql
;;; Copyright: (C) 1999  Eric Marsden
;;   
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; The latest version of this package (as well as a Common Lisp
;; version and one for the Scheme Shell) should be available from
;;
;;     <URL:http://www.chez.com/emarsden/downloads/>

;;; Commentary:

;;; Overview ==========================================================
;;
;; This module lets you access the PostgreSQL object-relational DBMS
;; from Emacs, using its socket-level frontend/backend protocol. The
;; module is capable of automatic type coercions from a range of SQL
;; types to the equivalent Emacs Lisp type. This is a low level API,
;; and won't be useful to end users. Should work with GNU Emacs 19.34
;; and up, and XEmacs 20 and up. Performance is very poor when not
;; byte-compiled.

;;; Entry points =======================================================
;;
;; (pg:connect dbname user [password host port]) -> connection
;;     Connect to the database DBNAME on HOST (defaults to localhost)
;;     at PORT (defaults to 5432) via TCP/IP and log in as USER. If
;;     the database requires a password, send PASSWORD as clear text.
;;     Set the output date type to 'ISO', and initialize our type
;;     parser tables.
;;
;; (pg:exec connection &rest sql) -> pgresult
;;     Concatenate the SQL strings and send to the backend. Retrieve
;;     all the information returned by the database and return it in
;;     an opaque record PGRESULT.
;;
;; (pg:result pgresult what &rest args) -> info
;;     Extract information from the PGRESULT. The WHAT keyword can be
;;     one of
;;          * :connection
;;          * :status
;;          * :attributes
;;          * :tuples
;;          * :tuple tupleNumber
;;          * :oid
;;     `:connection' allows you to retrieve the database connection.
;;     `:status' is a string returned by the backend to indicate the
;;     status of the command; it is something like "SELECT" for a
;;     select command, "DELETE 1" if the deletion affected a single
;;     row, etc. `:attributes' is a list of tuples providing metadata:
;;     the first component of each tuple is the attribute's name as a
;;     string, the second an integer representing its PostgreSQL type,
;;     and the third an integer representing the size of that type.
;;     `:tuples' returns all the data retrieved from the database, as a
;;     list of lists, each list corresponding to one row of data
;;     returned by the backend. `:tuple num' can be used to extract a
;;     specific tuple (numbering starts at 0). `:oid' allows you to
;;     retrieve the OID returned by the backend if the command was an
;;     insertion; the OID is a unique identifier for that row in the
;;     database (this is PostgreSQL-specific, please refer to the
;;     documentation for more details).
;;
;; (pg:disconnect connection) -> nil
;;     Close the database connection.
;;
;; (pg:databases connection) -> list of strings
;;     Return a list of the databases available at this site (a
;;     database is a set of tables; in a virgin PostgreSQL
;;     installation there is a single database named "template1").
;;
;; (pg:tables connection) -> list of strings
;;     Return a list of the tables present in the database to which we
;;     are currently connected. Only include user tables: system
;;     tables are excluded.
;;
;; (pg:columns connection table) -> list of strings
;;     Return a list of the columns (or attributes) in TABLE, which
;;     must be a table in the database to which we are currently
;;     connected. We only include the column names; if you want more
;;     detailed information (attribute types, for example), it can be
;;     obtained from `pg:result' on a SELECT statement for that table.
;;
;; Boolean variable `pg:disable-type-coercion' which can be set to
;; non-nil (before initiating a connection) to disable the library's
;; type coercion facility. Default is t.
;;
;;
;; The interface is pretty slow (byte compiling helps a lot). Maybe
;; someone can suggest a better way of reading input from the network
;; stream. Please note that your postmaster has to be started with the
;; `-i' option in order to accept TCP/IP connections (this is not the
;; default). For more information about PostgreSQL see
;; <URL:http://www.postgreSQL.org/>.

;;; TODO ============================================================
;; * large-object support (started)
;; * add a mechanism for parsing user-defined types. The user should
;;   be able to define a parse function and a type-name; we query
;;   pg_type to get the type's OID and add the information to
;;   pg:parsers.


;;; Code:

(require 'cl)

(defvar pg:disable-type-coercion nil  
  "*Non-nil disables the type coercion mechanism.
The default is nil, which means that data recovered from the database
is coerced to the corresponding Emacs Lisp type before being returned;
for example numeric data is transformed to Emacs Lisp numbers, and
booleans to booleans.

The coercion mechanism requires an initialization query to the
database, in order to build a table mapping type names to OIDs. This
option is provided mainly in case you wish to avoid the overhead of
this initial query. The overhead is only incurred once per Emacs
session (not per connection to the backend).")

(defconst pg:NAMEDATALEN 32)              ; postgres_ext.h
(defconst pg:PG_PROTOCOL_LATEST_MAJOR 1)  ; libpq/pgcomm.h
(defconst pg:PG_PROTOCOL_LATEST_MINOR 0)
(defconst pg:SM_DATABASE 64)
(defconst pg:SM_USER     32)
(defconst pg:SM_OPTIONS  64)
(defconst pg:SM_UNUSED   64)
(defconst pg:SM_TTY      64)

(defconst pg:AUTH_REQ_OK       0)
(defconst pg:AUTH_REQ_KRB4     1)
(defconst pg:AUTH_REQ_KRB5     2)
(defconst pg:AUTH_REQ_PASSWORD 3)
(defconst pg:AUTH_REQ_CRYPT    4)

(defconst pg:STARTUP_MSG            7)
(defconst pg:STARTUP_KRB4_MSG      10)
(defconst pg:STARTUP_KRB5_MSG      11)
(defconst pg:STARTUP_PASSWORD_MSG  14)

(defconst pg:StartupPacketSize
  (+ 4 4 pg:SM_DATABASE pg:SM_USER pg:SM_OPTIONS pg:SM_UNUSED pg:SM_TTY))

(defconst pg:MAX_MESSAGE_LEN    8192)     ; libpq-fe.h


(defstruct pgcon process position (binaryp nil))
(defstruct pgresult connection status attributes tuples portal)

(defsubst pg:flush (connection)
  (accept-process-output (pgcon-process connection)))

;; GNU Emacs doesn't distinguish between integers and characters,
;; whereas in XEmacs they are separate data types
(unless (fboundp 'int-to-char)
  (defsubst int-to-char (c) c))


(defun* pg:connect (dbname user
                   &optional (password "") (host "localhost") (port 2020))
  "Initiate a connection with the PostgreSQL backend.
Connect to the database DBNAME with the username USER, on PORT of
HOST, providing PASSWORD if necessary. Return a connection to the
database (as an opaque type). PORT defaults to 5432, HOST to
`localhost', and PASSWORD to an empty string."  
  (let* ((buf (generate-new-buffer " *PostgreSQL*"))
         process connection
         (user-packet-length (+ pg:SM_USER pg:SM_OPTIONS pg:SM_UNUSED pg:SM_TTY)))
    (setq process (open-network-stream "postgres" buf host port))
    ;; try to disable MULE stuff if necessary
    (and (fboundp 'set-buffer-process-coding-system)
         (save-excursion
           (set-buffer buf)
           (set-buffer-process-coding-system 'no-conversion 'no-conversion)))
    (setq connection (make-pgcon :process process :position 1))
    ;; send the startup packet
    (message "Sending startup packet")
    (pg:send-int connection pg:StartupPacketSize 4)
    (pg:send-int connection pg:PG_PROTOCOL_LATEST_MAJOR 2)
    (pg:send-int connection pg:PG_PROTOCOL_LATEST_MINOR 2)
    (pg:send connection dbname pg:SM_DATABASE)
    (pg:send connection user user-packet-length)
    (pg:flush connection)
    (message "Startup packet sent")
    (loop for c = (pg:read-char connection) do
      (cond ((eq ?E c) (error "Backend error: %s" (pg:read-string connection 4096)))
            ((eq ?R c)
             (let ((areq (pg:read-net-int connection 4)))
               (cond
                ((= areq pg:AUTH_REQ_OK)
                 (and (not pg:disable-type-coercion)
                     (null pg:parsers)
                     (pg:initialize-parsers connection))
                 (pg:exec connection "SET datestyle = 'ISO'")
                 (return-from pg:connect connection))
                ((= areq pg:AUTH_REQ_PASSWORD)
                 (pg:send-int connection (+ 5 (length password)) 4)
                 (pg:send connection password)
                 (pg:send-int connection 0 1)
                 (pg:flush connection))
                ((= areq pg:AUTH_REQ_CRYPT)
                 ;; FIXME find a way to access crypt()
                 (error "Crypt authentication not supported"))
                ((= areq pg:AUTH_REQ_KRB4)
                 (error "Kerberos4 authentication not supported"))
                ((= areq pg:AUTH_REQ_KRB5)
                 (error "Kerberos5 authentication not supported"))
                (t
                 (error "Can't do that type of authentication: %s" areq)))))
            (t
             (error "Problem connecting: expected an authentication response"))))))

(defun* pg:exec (connection &rest args)
  "Execute the SQL command given by the concatenation of ARGS
on the database to which we are connected via CONNECTION. Return
a result structure which can be decoded using `pg:result'."
  (let ((sql (apply #'concat args))
        (tuples '())
        (attributes '())
        (result (make-pgresult :connection connection)))
    (if (> (length sql) pg:MAX_MESSAGE_LEN)
        (error "SQL statement too long: %s" sql))
    (pg:send connection (format "%c%s%c" ?Q sql 0))
    (pg:flush connection)
    (loop for c = (pg:read-char connection) do
          (case c
            (?A                         ; asynchronous notify
             (let ((pid (pg:read-int connection 4))
                   (msg (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (message "Asynchronous notify %s" msg)))

            (?B                         ; binary data transfer
             (setf (pgcon-binaryp connection) t)
             (or attributes (error "Tuple received before metadata"))
             (push (pg:read-tuple connection attributes) tuples))

            (?C                         ; command status
             (let* ((status (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (setf (pgresult-status result) status)
               (setf (pgresult-tuples result) (nreverse tuples))
               (setf (pgresult-attributes result) attributes)
               (return-from pg:exec result)))

            (?D                         ; text data transfer
             (setf (pgcon-binaryp connection) nil)
             (or attributes (error "Tuple received before metadata"))
             (push (pg:read-tuple connection attributes) tuples))

            (?E                         ; error message
             (let ((msg (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (error "Backend error: %s" msg)))

            ;; indicates the end of a series of command statuses, for example
            (?I                         ; empty query
             (let ((c (pg:read-char connection)))
               ))

            (?N                         ; error notification
             (let ((notice (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (message "NOTICE: N%s" notice)))

            (?P                         ; synchronous portal
             (let ((portal (pg:read-string connection pg:MAX_MESSAGE_LEN)))
               (setf (pgresult-portal result) portal)))

            (?T                         ; metadata field description
             (and attributes (error "Cannot handle multiple result group"))
             (setq attributes (pg:read-attributes connection)))
        
            (t
             (error "Unknown response type from backend: %s" c))))))

(defun pg:result (result what &rest arg)
  "Extract WHAT component of RESULT.
RESULT should be a structure obtained from a call to `pg:exec',
and the keyword WHAT should be one of
   :connection -> return the connection object
   :status -> return the status string provided by the database
   :attributes -> return the metadata, as a list of lists
   :tuples -> return the data, as a list of lists
   :tuple n -> return the nth component of the data
   :oid -> return the OID (a unique identifier generated by PostgreSQL
           for each row resulting from an insertion)"
  (cond ((eq :connection what) (pgresult-connection result))
        ((eq :status what)     (pgresult-status result))
        ((eq :attributes what) (pgresult-attributes result))
        ((eq :tuples what)     (pgresult-tuples result))
        ((eq :tuple what)
         (let ((which (if (integerp (car arg)) (car arg)
                        (error "%s is not an integer" arg)))
               (tuples (pgresult-tuples result)))
           (nth which tuples)))
        ((eq :oid what)
         (let ((status (pgresult-status result)))
           (if (string= "INSERT" (substring status 0 6))
               (string-to-number (substring status 7 (position ? status :start 7)))
               (error "Only INSERT commands generate an oid: %s" status))))
        (t
         (error "Unknown result request %s" what))))

(defun pg:disconnect (connection)
  "Close the database connection.
This command should be used when you have finished with the database.
It will release memory used to buffer the data transfered between
PostgreSQL and Emacs. CONNECTION should no longer be used."
  (pg:send connection "X")
  (pg:flush connection)
  (delete-process (pgcon-process connection))
  (kill-buffer (process-buffer (pgcon-process connection))))


;; type coercion support ==============================================
;;
;; When returning data from a SELECT statement, PostgreSQL starts by
;; sending some metadata describing the attributes. This information
;; is read by `pg:read-attributes', and consists of each attribute's
;; name (as a string), its size (in bytes), and its type (as an oid
;; which identifies a row in the PostgreSQL system table pg_type). Each
;; row in pg_type includes the type's name (as a string).
;;
;; We are able to parse a certain number of the PostgreSQL types (for
;; example, numeric data is converted to a numeric Emacs Lisp type,
;; dates are converted to the Emacs date representation, booleans to
;; Emacs Lisp booleans). However, there isn't a fixed mapping from a
;; type to its OID which is guaranteed to be stable across database
;; installations, so we need to build a table mapping OIDs to parser
;; functions.
;;
;; This is done by the procedure `pg:initialize-parsers', which is run
;; the first time a connection is initiated with the database from
;; this invocation of Emacs, and which issues a SELECT statement to
;; extract the required information from pg_type. This initialization
;; imposes a slight overhead on the first request, which you can avoid
;; by setting `pg:disable-type-coercion' to non-nil if it bothers you.
;; ====================================================================

;; alist of (oid . parser) pairs. This is built dynamically at
;; initialization of the connection with the database (once generated,
;; the information is shared between connections).
(defvar pg:parsers '())

;; this is a var not a const to allow user-defined types (a PostgreSQL
;; feature not present in ANSI SQL). The user can add a (type-name .
;; type-parser) pair and call `pg:initialize-parsers', after which the
;; user-defined type should be returned parsed from `pg:result'.
;; Untested.
(defvar pg:type-parsers
  `(("bool"      . ,'pg:bool-parser)
    ("char"      . ,'pg:text-parser)
    ("char2"     . ,'pg:text-parser)
    ("char4"     . ,'pg:text-parser)
    ("char8"     . ,'pg:text-parser)
    ("char16"    . ,'pg:text-parser)
    ("text"      . ,'pg:text-parser)
    ("varchar"   . ,'pg:text-parser)
    ("int2"      . ,'pg:number-parser)
    ("int28"     . ,'pg:number-parser)
    ("int4"      . ,'pg:number-parser)
    ("oid"       . ,'pg:number-parser)
    ("float4"    . ,'pg:number-parser)
    ("float8"    . ,'pg:number-parser)
    ("money"     . ,'pg:number-parser)
    ("abstime"   . ,'pg:isodate-parser)
    ("date"      . ,'pg:isodate-parser)
    ("timestamp" . ,'pg:isodate-parser)
    ("datetime"  . ,'pg:isodate-parser)
    ("time"      . ,'pg:text-parser)     ; preparsed "15:32:45"
    ("reltime"   . ,'pg:text-parser)     ; don't know how to parse these
    ("timespan"  . ,'pg:text-parser)
    ("tinterval" . ,'pg:text-parser)))

;; see `man pgbuiltin' for details on PostgreSQL builtin types
(defun pg:number-parser (str) (string-to-number str))
(defsubst pg:text-parser (str) str)
(defun pg:bool-parser (str)
  (cond ((string= "t" str) t)
        ((string= "f" str) nil)
        (t (error "Badly formed boolean from backend: %s" str))))

;;  format for abstime/timestamp etc with ISO output syntax is
;;;    "1999-01-02 14:32:53+01"
;; which we convert to the internal Emacs date/time representation
(defun pg:isodate-parser (str)
  (let ((year    (string-to-number (substring str 0 4)))
        (month   (string-to-number (substring str 5 7)))
        (day     (string-to-number (substring str 8 10)))
        (hours   (string-to-number (substring str 11 13)))
        (minutes (string-to-number (substring str 14 16)))
        (seconds (string-to-number (substring str 17 19)))
        (tz      (string-to-number (substring str 19 22))))
    (encode-time seconds minutes hours day (- month 1) year tz)))

(defun pg:initialize-parsers (connection)
  (let* ((pgtypes (pg:exec connection "SELECT typname,oid FROM pg_type"))
         (tuples (pg:result pgtypes :tuples)))
    (setq pg:parsers '())
    (mapcar
     #'(lambda (tuple)
       (let* ((typname (first tuple))
              (oid (string-to-number (second tuple)))
              (type (assoc* typname pg:type-parsers :test #'string=)))
         (if (consp type)
             (push (cons oid (cdr type)) pg:parsers))))
     tuples)))

(defun pg:parse (str oid)
  (let ((parser (assoc* oid pg:parsers :test #'eq)))
    (if (consp parser)
        (funcall (cdr parser) str)
      str)))


;; DBMS metainformation ================================================
;;
;; Metainformation such as the list of databases present in the
;; database management system, list of tables, attributes per table.
;; This information is not available directly, but can be deduced by
;; querying the system tables.
;;
;; Based on the queries issued by psql in response to user commands
;; `\d' and `\d tablename'; see file
;; /usr/local/src/pgsql/src/bin/psql/psql.c
;; =====================================================================
(defun pg:databases (conn)
  "Return a list of the databases available at this site."
  (let ((res (pg:exec conn "SELECT datname FROM pg_database")))
    (apply #'append (pg:result res :tuples))))

(defun pg:tables (conn)
  "Return a list of the tables present in this database."
  (let ((res (pg:exec conn "SELECT relname FROM pg_class, pg_user WHERE "
                      "(relkind = 'r' OR relkind = 'i' OR relkind = 'S') AND "
                      "relname !~ '^pg_' AND usesysid = relowner ORDER BY relname")))
    (apply #'append (pg:result res :tuples))))
    
(defun pg:columns (conn table)
  "Return a list of the columns present in TABLE."
  (let* ((sql (format "SELECT * FROM %s WHERE 0 = 1" table))
         (res (pg:exec conn sql)))
    (mapcar #'car (pg:result res :attributes))))



;; support routines ============================================================

;; Attribute information is as follows
;;    attribute-name (string)
;;    attribute-type as an oid from table pg_type
;;    attribute-size (in bytes?)
(defun pg:read-attributes (connection)
  (let ((attribute-count (pg:read-net-int connection 2))
        (attributes '()))
    (do ((i attribute-count (- i 1)))
        ((zerop i) (nreverse attributes))
      (let ((type-name (pg:read-string connection pg:MAX_MESSAGE_LEN))
            (type-id   (pg:read-net-int connection 4))
            (type-len  (pg:read-net-int connection 2)))
        (push (list type-name type-id type-len) attributes)))))

;; a bitmap is a string, which we interpret as a sequence of bytes
(defun pg:bitmap-ref (bitmap ref)
  (multiple-value-bind (char-ref bit-ref)
      (floor* ref 8)
    (let ((int (aref bitmap char-ref)))
      (logand 128 (ash int bit-ref)))))
    
;; the backend starts by sending a bitmap indicating which tuples are
;; NULL
(defun pg:read-tuple (connection attributes)
  (let* ((num-attributes (length attributes))
         (num-bytes (car (ceiling* num-attributes 8)))
         (bitmap (pg:read-chars connection num-bytes))
         (correction (if (pgcon-binaryp connection) 0 -4))
         (tuples '()))
    (do ((i 0 (+ i 1))
         (type-ids (mapcar #'second attributes) (cdr type-ids)))
        ((= i num-attributes) (nreverse tuples))
      (cond ((zerop (pg:bitmap-ref bitmap i))
             (push nil tuples))
            (t
             (let* ((len (+ (pg:read-net-int connection 4) correction))
                    (raw (pg:read-chars connection (max 0 len)))
                    (parsed (pg:parse raw (car type-ids))))
               (push parsed tuples)))))))

;; blech
(defun pg:read-char (connection)
  (let ((process (pgcon-process connection))
        (position (pgcon-position connection)))        
    (save-excursion
      (set-buffer (process-buffer process))
      (incf (pgcon-position connection))
      (if (null (char-after position))
          (accept-process-output (pgcon-process connection)))
      (char-after position))))

;; FIXME should be more careful here; the integer could overflow. I
;; wanna real Lisp!
(defun pg:read-net-int (connection bytes)
  (do ((i bytes (- i 1))
       (accum 0))
      ((zerop i) accum)
    (setq accum (+ (* 256 accum) (pg:read-char connection)))))

(defun pg:read-int (connection bytes)
  (do ((i bytes (- i 1))
       (multiplier 1 (* multiplier 256))
       (accum 0))
      ((zerop i) accum)
    (incf accum (* multiplier (pg:read-char connection)))))

(defun pg:read-chars (connection howmany)
  (do ((i 0 (+ i 1))
       (chars (make-string howmany ?.)))
      ((= i howmany) chars)
    (aset chars i (pg:read-char connection))))

;; read a null-terminated string
(defun pg:read-string (connection maxbytes)
  (loop for i from 1 to maxbytes
        for ch = (pg:read-char connection)
        until (= ch (int-to-char 0))
        concat (char-to-string ch)))

;; higher order bits first
(defun pg:send-int (connection num bytes)
  (let ((process (pgcon-process connection))
        (str (make-string bytes 0))
        (i (- bytes 1)))
    (while (>= i 0)
      (aset str i (% num 256))
      (setq num (floor num 256))
      (decf i))
    (process-send-string process str)
    (message str)))
  
(defun pg:send (connection str &optional bytes)
  (let ((process (pgcon-process connection))
        (padding (if (and (numberp bytes) (> bytes (length str)))
                     (make-string (- bytes (length str)) 0)
                   (make-string 0 0))))
    (process-send-string process (concat str padding))
    (message (concat str padding))))


;; This (limited) testing code assumes you have a database user
;; "postgres" either with no password or with password "postgres", and
;; a database named "template1". It should clean up after itself.
;;
;; * is the postmaster running?
;; * was the postmaster started with the `-i' commandline option?
;;
;; This code has been tested with GNU Emacs 19.34 and 20.3, and XEmacs
;; 20.4 on Debian GNU/Linux 2.1 against PostgreSQL v6.3. I would
;; appreciate reports of successful use on different architectures
;; (particularly with a different byte order).
(defun pg:test ()
  (interactive)
  (let* ((conn (pg:connect "template1" "ignacio" ""))
         (databases (pg:databases conn)))
    (unwind-protect
        (progn 
          (if (member "pgeltest" databases)
              (pg:exec conn "DROP DATABASE pgeltest"))
          (pg:exec conn "CREATE DATABASE pgeltest"))
      (pg:disconnect conn)))
  (message "Testing insertions...")
  (pg:test-insert)
  (message "Testing date routines...")
  (pg:test-date)
  (message "Testing field extraction routines...")
  (pg:test-result)
  (let ((conn (pg:connect "template1" "ignacio" "")))
    (unwind-protect
        (pg:exec conn "DROP DATABASE pgeltest")
      (pg:disconnect conn)))
  (message "Tests passed ok"))

;; this will be *real* slow unless byte-compiled
(defun pg:test-insert ()
  (let ((conn (pg:connect "pgeltest" "ignacio" ""))
        res)
    (unwind-protect
        (progn
          (pg:exec conn "CREATE TABLE count_test(key int, val int)")
          (loop for i from 1 to 100
                for sql = (format "INSERT INTO count_test VALUES(%s, %s)"
                                  i (* i i))
                do (pg:exec conn sql))
          (setq res (pg:exec conn "SELECT count(val) FROM count_test"))
          (assert (= 100 (first (pg:result res :tuple 0))))
          (setq res (pg:exec conn "SELECT sum(key) FROM count_test"))
          (assert (= 5050 (first (pg:result res :tuple 0)))))
      (pg:exec conn "DROP TABLE count_test")
      (pg:disconnect conn))))

;; Testing for the time handling routines. Expected output is
;; something like (in buffer *Messages*)
;;
;; timestamp = (14189 17420)
;; abstime = (14189 17420)
;; time = 19:42:06
(defun pg:test-date ()
  (let ((conn (pg:connect "pgeltest" "ignacio" ""))
        res)
    (unwind-protect
        (progn
          (pg:exec conn "CREATE TABLE date_test(a timestamp, b abstime, c time)")
          (pg:exec conn "INSERT INTO date_test VALUES "
                   "(current_timestamp, 'now', 'now')")
          (setq res (pg:exec conn "SELECT * FROM date_test"))
          (setq res (pg:result res :tuple 0))
          (message "timestamp = %s" (first res))
          (message "abstime = %s" (second res))
          (message "time = %s" (third res)))
      (pg:exec conn "DROP TABLE date_test")
      (pg:disconnect conn))))
  
;; Testing for the data access functions. Expected output is something
;; like
;;
;; ==============================================
;; status of CREATE is CREATE
;; status of INSERT is INSERT 22506 1
;; oid of INSERT is 22506
;; status of SELECT is SELECT
;; attributes of SELECT are ((a 23 4) (b 1043 65535))
;; tuples of SELECT are ((3 zae) (66 poiu))
;; second tuple of SELECT is (66 poiu)
;; status of DROP is DROP
;; ==============================================
(defun pg:test-result ()
  (let* ((conn (pg:connect "pgeltest" "ignacio" ""))
         (r1 (pg:exec conn "CREATE TABLE resulttest (a int, b VARCHAR(4))"))
         (r2 (pg:exec conn "INSERT INTO resulttest VALUES (3, 'zae')"))
         (r3 (pg:exec conn "INSERT INTO resulttest VALUES (66, 'poiu')"))
         (r4 (pg:exec conn "SELECT * FROM resulttest"))
         (r5 (pg:exec conn "DROP TABLE resulttest")))
        (message "==============================================")
        (message "status of CREATE is %s" (pg:result r1 :status))
        (message "status of INSERT is %s" (pg:result r2 :status))
        (message "oid of INSERT is %s"    (pg:result r2 :oid))
        (message "status of SELECT is %s" (pg:result r4 :status))
        (message "attributes of SELECT are %s" (pg:result r4 :attributes))
        (message "tuples of SELECT are %s" (pg:result r4 :tuples))
        (message "second tuple of SELECT is %s" (pg:result r4 :tuple 1))
        (message "status of DROP is %s" (pg:result r5 :status))
        (message "==============================================")
        (pg:disconnect conn)))


(defun pg:cleanup ()
  (interactive)
  (loop for b in (buffer-list) do
        (if (string-match " \\*PostgreSQL\\*" (buffer-name b))
            (kill-buffer b))))


(provide 'pg)

;;; pg.el ends here




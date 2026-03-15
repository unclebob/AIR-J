(ns airj.stdlib
  (:require [airj.interface-sources :as interface-sources]
            [airj.parser :as parser]))

(declare reachable-source-map)

(def ^:private standard-sources
  {'airj/core
   "(module airj/core
      (imports
        (java java.lang.NumberFormatException))
      (export Diagnostic Interchange Option Result parse-int)
      (data Diagnostic
        (field phase String)
        (field message String)
        (field detail String))
      (union Interchange
        (variant Null)
        (variant BoolValue
          (field value Bool))
        (variant IntValue
          (field value Int))
        (variant DoubleValue
          (field value Double))
        (variant StringValue
          (field value String))
        (variant SeqValue
          (field value (Seq Interchange)))
        (variant MapValue
          (field value (Map String Interchange))))
      (union Option
        (type-params T)
        (variant None)
        (variant Some
          (field value T)))
      (union Result
        (type-params Ok Err)
        (variant Ok
          (field value Ok))
        (variant Err
          (field error Err)))
      (fn parse-int
        (params (text String))
        (returns (Result Int Diagnostic))
        (effects ())
        (requires true)
        (ensures true)
        (try
          (variant (Result Int Diagnostic)
                   Ok
                   (string->int (local text)))
          (catch (Java java.lang.NumberFormatException) ex
            (variant (Result Int Diagnostic)
                     Err
                     (construct Diagnostic
                                \"parse\"
                                \"Invalid integer.\"
                                (local text)))))))"

   'airj/json
   "(module airj/json
      (imports
        (airj airj/core Diagnostic Interchange Result)
        (java java.lang.RuntimeException))
      (export parse parse-result write write-result)
      (fn parse
        (params (text String))
        (returns Interchange)
        (effects (Foreign.Throw))
        (requires true)
        (ensures true)
        (json-parse (local text)))
      (fn parse-result
        (params (text String))
        (returns (Result Interchange Diagnostic))
        (effects ())
        (requires true)
        (ensures true)
        (try
          (variant (Result Interchange Diagnostic)
                   Ok
                   (call (local parse) (local text)))
          (catch (Java java.lang.RuntimeException) ex
            (variant (Result Interchange Diagnostic)
                     Err
                     (construct Diagnostic
                                \"json\"
                                \"Parse failed.\"
                                (local text))))))
      (fn write
        (params (value Interchange))
        (returns String)
        (effects (Foreign.Throw))
        (requires true)
        (ensures true)
        (json-write (local value)))
      (fn write-result
        (params (value Interchange))
        (returns (Result String Diagnostic))
        (effects ())
        (requires true)
        (ensures true)
        (try
          (variant (Result String Diagnostic)
                   Ok
                   (call (local write) (local value)))
          (catch (Java java.lang.RuntimeException) ex
            (variant (Result String Diagnostic)
                     Err
                     (construct Diagnostic
                                \"json\"
                                \"Write failed.\"
                                \"interchange\"))))))"

   'airj/bytes
   "(module airj/bytes
      (imports
        (java java.lang.Object)
        (java java.lang.String)
        (java java.lang.reflect.Array))
      (export utf8-encode utf8-decode length)
      (fn utf8-encode
        (params (text String))
        (returns Bytes)
        (effects (Foreign.Throw))
        (requires true)
        (ensures true)
        (java/call
          (local text)
          getBytes
          (signature (String) Bytes)
          \"UTF-8\"))
      (fn utf8-decode
        (params (bytes Bytes))
        (returns String)
        (effects (Foreign.Throw))
        (requires true)
        (ensures true)
        (java/new java.lang.String (local bytes) \"UTF-8\"))
      (fn length
        (params (bytes Bytes))
        (returns Int)
        (effects (Foreign.Throw))
        (requires true)
        (ensures true)
        (java/static-call
          java.lang.reflect.Array
          getLength
          (signature ((Java java.lang.Object)) Int)
          (local bytes))))"

   'airj/env
   "(module airj/env
      (imports
        (airj airj/core Option)
        (java java.lang.System))
      (export get cwd)
      (fn get
        (params (name String))
        (returns (Option String))
        (effects (Env.Read))
        (requires true)
        (ensures true)
        (env-get (local name)))
      (fn cwd
        (params)
        (returns String)
        (effects (Env.Read Foreign.Throw))
        (requires true)
        (ensures true)
        (java/static-call
          java.lang.System
          getProperty
          (signature (String) String)
          \"user.dir\")))"

   'airj/process
   "(module airj/process
      (imports
        (airj airj/core Diagnostic Result)
        (java java.lang.RuntimeException))
      (export ProcessResult run run-result)
      (data ProcessResult
        (field exit-code Int)
        (field stdout Bytes)
        (field stderr Bytes))
      (fn run
        (params (command (Seq String)) (stdin Bytes))
        (returns ProcessResult)
        (effects (Process.Run Foreign.Throw))
        (requires true)
        (ensures true)
        (process-run (local command) (local stdin)))
      (fn run-result
        (params (command (Seq String)) (stdin Bytes))
        (returns (Result ProcessResult Diagnostic))
        (effects (Process.Run))
        (requires true)
        (ensures true)
        (try
          (variant (Result ProcessResult Diagnostic)
                   Ok
                   (call (local run) (local command) (local stdin)))
          (catch (Java java.lang.RuntimeException) ex
            (variant (Result ProcessResult Diagnostic)
                     Err
                     (construct Diagnostic
                                \"process\"
                                \"Process execution failed.\"
                                \"process-run\"))))))"

   'airj/test
   "(module airj/test
      (imports
        (airj airj/core Diagnostic Interchange Option Result))
      (export AssertionFailure
              TestOutcome
              TestSummary
              pass
              fail
              error
              assert-true
              assert-false
              assert-bool-eq
              assert-int-eq
              assert-string-eq
              assert-diagnostic-message
              assert-interchange-string-field
              assert-none-string
              assert-some-string
              assert-ok-int
              assert-err-message)
      (data AssertionFailure
        (field expected String)
        (field actual String))
      (union TestOutcome
        (variant Pass
          (field name String))
        (variant Fail
          (field name String)
          (field diagnostic Diagnostic))
        (variant Error
          (field name String)
          (field diagnostic Diagnostic)))
      (data TestSummary
        (field passed Int)
        (field failed Int)
        (field errored Int)
        (field outcomes (Seq TestOutcome)))
      (fn pass
        (params (name String))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (variant TestOutcome Pass (local name)))
      (fn fail
        (params (name String) (message String) (detail String))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (variant TestOutcome
                 Fail
                 (local name)
                 (construct Diagnostic
                            \"test\"
                            (local message)
                            (local detail))))
      (fn error
        (params (name String) (message String) (detail String))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (variant TestOutcome
                 Error
                 (local name)
                 (construct Diagnostic
                            \"test\"
                            (local message)
                            (local detail))))
      (fn assert-true
        (params (name String) (actual Bool))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (if
          (local actual)
          (call (local pass) (local name))
          (call (local fail)
                (local name)
                \"Expected true.\"
                \"Assertion evaluated to false.\")))
      (fn assert-false
        (params (name String) (actual Bool))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (if
          (local actual)
          (call (local fail)
                (local name)
                \"Expected false.\"
                \"Assertion evaluated to true.\")
          (call (local pass) (local name))))
      (fn assert-bool-eq
        (params (name String) (actual Bool) (expected Bool))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (if
          (bool-eq (local actual) (local expected))
          (call (local pass) (local name))
          (call (local fail)
                (local name)
                \"Boolean equality assertion failed.\"
                \"Expected and actual differ.\")))
      (fn assert-int-eq
        (params (name String) (actual Int) (expected Int))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (if
          (int-eq (local actual) (local expected))
          (call (local pass) (local name))
          (call (local fail)
                (local name)
                \"Integer equality assertion failed.\"
                (string-concat
                  (string-concat \"expected=\" (int->string (local expected)))
                  (string-concat \", actual=\" (int->string (local actual)))))))
      (fn assert-string-eq
        (params (name String) (actual String) (expected String))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (if
          (string-eq (local actual) (local expected))
          (call (local pass) (local name))
          (call (local fail)
                (local name)
                \"String equality assertion failed.\"
                (string-concat
                  (string-concat \"expected=\" (local expected))
                  (string-concat \", actual=\" (local actual))))))
      (fn assert-diagnostic-message
        (params (name String) (actual Diagnostic) (expected String))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (call (local assert-string-eq)
              (local name)
              (record-get (local actual) message)
              (local expected)))
      (fn assert-interchange-string-field
        (params (name String) (payload Interchange) (field String) (expected String))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (match (local payload)
          (case (MapValue entries)
            (match (map-get (local entries) (local field))
              (case (Some actual-value)
                (match (local actual-value)
                  (case (StringValue value)
                    (call (local assert-string-eq)
                          (local name)
                          (local value)
                          (local expected)))
                  (case _
                    (call (local fail)
                          (local name)
                          \"Expected string field value.\"
                          (local field)))))
              (case (None)
                (call (local fail)
                      (local name)
                      \"Missing interchange field.\"
                      (local field)))))
          (case _
            (call (local fail)
                  (local name)
                  \"Expected interchange object.\"
                  (local field)))))
      (fn assert-none-string
        (params (name String) (actual (Option String)))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (match (local actual)
          (case (None)
            (call (local pass) (local name)))
          (case (Some value)
            (call (local fail)
                  (local name)
                  \"Expected None.\"
                  (string-concat \"actual=Some(\" (string-concat (local value) \")\"))))))
      (fn assert-some-string
        (params (name String) (actual (Option String)) (expected String))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (match (local actual)
          (case (None)
            (call (local fail)
                  (local name)
                  \"Expected Some value.\"
                  \"actual=None\"))
          (case (Some value)
            (call (local assert-string-eq) (local name) (local value) (local expected)))))
      (fn assert-ok-int
        (params (name String) (actual (Result Int Diagnostic)) (expected Int))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (match (local actual)
          (case (Ok value)
            (call (local assert-int-eq) (local name) (local value) (local expected)))
          (case (Err diagnostic)
            (call (local fail)
                  (local name)
                  \"Expected Ok result.\"
                  (record-get (local diagnostic) message)))))
      (fn assert-err-message
        (params (name String) (actual (Result Int Diagnostic)) (expected String))
        (returns TestOutcome)
        (effects ())
        (requires true)
        (ensures true)
        (match (local actual)
          (case (Ok value)
            (call (local fail)
                  (local name)
                  \"Expected Err result.\"
                  (string-concat \"actual=Ok(\" (string-concat (int->string (local value)) \")\"))))
          (case (Err diagnostic)
            (call (local assert-string-eq)
                  (local name)
                  (record-get (local diagnostic) message)
                  (local expected))))))))"

   'airj/test-runner
   "(module airj/test-runner
      (imports
        (airj airj/core Diagnostic Interchange Result)
        (airj airj/json write-result)
        (airj airj/test TestOutcome TestSummary))
      (export empty-summary
              record
              summarize
              run
              run-json
              failure-count
              exit-code
              diagnostic-interchange
              outcome-interchange
              outcomes-interchange
              summary-interchange
              render-json-summary
              render-outcome
              render-summary
              print-json-summary
              print-summary)
      (fn empty-summary
        (params)
        (returns TestSummary)
        (effects ())
        (requires true)
        (ensures true)
        (construct TestSummary
                   0
                   0
                   0
                   (seq-empty TestOutcome)))
      (fn record
        (params (summary TestSummary) (outcome TestOutcome))
        (returns TestSummary)
        (effects ())
        (requires true)
        (ensures true)
        (match (local outcome)
          (case (Pass name)
            (construct TestSummary
                       (int-add (record-get (local summary) passed) 1)
                       (record-get (local summary) failed)
                       (record-get (local summary) errored)
                       (seq-append (record-get (local summary) outcomes)
                                   (local outcome))))
          (case (Fail name diagnostic)
            (construct TestSummary
                       (record-get (local summary) passed)
                       (int-add (record-get (local summary) failed) 1)
                       (record-get (local summary) errored)
                       (seq-append (record-get (local summary) outcomes)
                                   (local outcome))))
          (case (Error name diagnostic)
            (construct TestSummary
                       (record-get (local summary) passed)
                       (record-get (local summary) failed)
                       (int-add (record-get (local summary) errored) 1)
                       (seq-append (record-get (local summary) outcomes)
                                   (local outcome))))))
      (fn summarize
        (params (outcomes (Seq TestOutcome)))
        (returns TestSummary)
        (effects ())
        (requires true)
        (ensures true)
        (loop ((remaining (local outcomes))
               (summary (call (local empty-summary))))
          (if
            (seq-empty? (local remaining))
            (local summary)
            (recur (seq-rest (local remaining))
                   (call (local record)
                         (local summary)
                         (seq-first (local remaining)))))))
      (fn run
        (params (outcomes (Seq TestOutcome)))
        (returns Int)
        (effects (Stdout.Write))
        (requires true)
        (ensures true)
        (let ((summary (call (local summarize) (local outcomes))))
          (seq
            (call (local print-summary) (local summary))
            (call (local exit-code) (local summary)))))
      (fn run-json
        (params (module-name String) (outcomes (Seq TestOutcome)))
        (returns Int)
        (effects (Stdout.Write))
        (requires true)
        (ensures true)
        (let ((summary (call (local summarize) (local outcomes))))
          (seq
            (call (local print-json-summary) (local module-name) (local summary))
            (call (local exit-code) (local summary)))))
      (fn failure-count
        (params (summary TestSummary))
        (returns Int)
        (effects ())
        (requires true)
        (ensures true)
        (int-add
          (record-get (local summary) failed)
          (record-get (local summary) errored)))
      (fn exit-code
        (params (summary TestSummary))
        (returns Int)
        (effects ())
        (requires true)
        (ensures true)
        (if
          (int-eq (call (local failure-count) (local summary)) 0)
          0
          1))
      (fn diagnostic-interchange
        (params (diagnostic Diagnostic))
        (returns Interchange)
        (effects ())
        (requires true)
        (ensures true)
        (variant Interchange
                 MapValue
                 (map-set
                   (map-set
                     (map-set
                       (map-empty Interchange)
                       \"phase\"
                       (variant Interchange StringValue (record-get (local diagnostic) phase)))
                     \"message\"
                     (variant Interchange StringValue (record-get (local diagnostic) message)))
                   \"detail\"
                   (variant Interchange StringValue (record-get (local diagnostic) detail)))))
      (fn outcome-interchange
        (params (outcome TestOutcome))
        (returns Interchange)
        (effects ())
        (requires true)
        (ensures true)
        (match (local outcome)
          (case (Pass name)
            (variant Interchange
                     MapValue
                     (map-set
                       (map-set
                         (map-empty Interchange)
                         \"status\"
                         (variant Interchange StringValue \"pass\"))
                       \"name\"
                       (variant Interchange StringValue (local name)))))
          (case (Fail name diagnostic)
            (variant Interchange
                     MapValue
                     (map-set
                       (map-set
                         (map-set
                           (map-empty Interchange)
                           \"status\"
                           (variant Interchange StringValue \"fail\"))
                         \"name\"
                         (variant Interchange StringValue (local name)))
                       \"diagnostic\"
                       (call (local diagnostic-interchange) (local diagnostic)))))
          (case (Error name diagnostic)
            (variant Interchange
                     MapValue
                     (map-set
                       (map-set
                         (map-set
                           (map-empty Interchange)
                           \"status\"
                           (variant Interchange StringValue \"error\"))
                         \"name\"
                         (variant Interchange StringValue (local name)))
                       \"diagnostic\"
                       (call (local diagnostic-interchange) (local diagnostic)))))))
      (fn outcomes-interchange
        (params (outcomes (Seq TestOutcome)))
        (returns Interchange)
        (effects ())
        (requires true)
        (ensures true)
        (loop ((remaining (local outcomes))
               (values (seq-empty Interchange)))
          (if
            (seq-empty? (local remaining))
            (variant Interchange SeqValue (local values))
            (recur (seq-rest (local remaining))
                   (seq-append (local values)
                               (call (local outcome-interchange)
                                     (seq-first (local remaining))))))))
      (fn summary-interchange
        (params (module-name String) (summary TestSummary))
        (returns Interchange)
        (effects ())
        (requires true)
        (ensures true)
        (variant Interchange
                 MapValue
                 (map-set
                   (map-set
                     (map-set
                       (map-set
                         (map-set
                           (map-empty Interchange)
                           \"module\"
                           (variant Interchange StringValue (local module-name)))
                         \"passed\"
                         (variant Interchange IntValue (record-get (local summary) passed)))
                       \"failed\"
                       (variant Interchange IntValue (record-get (local summary) failed)))
                     \"errored\"
                     (variant Interchange IntValue (record-get (local summary) errored)))
                   \"outcomes\"
                   (call (local outcomes-interchange) (record-get (local summary) outcomes)))))
      (fn render-json-summary
        (params (module-name String) (summary TestSummary))
        (returns String)
        (effects ())
        (requires true)
        (ensures true)
        (match (call (local write-result)
                     (call (local summary-interchange) (local module-name) (local summary)))
          (case (Ok text)
            (local text))
          (case (Err diagnostic)
            (string-concat
              (string-concat
                \"{\\\"message\\\":\\\"\"
                (record-get (local diagnostic) message))
              \"\\\"}\"))))
      (fn render-outcome
        (params (outcome TestOutcome))
        (returns String)
        (effects ())
        (requires true)
        (ensures true)
        (match (local outcome)
          (case (Pass name)
            (string-concat \"PASS \" (local name)))
          (case (Fail name diagnostic)
            (string-concat
              (string-concat
                (string-concat \"FAIL \" (local name))
                \": \")
              (record-get (local diagnostic) message)))
          (case (Error name diagnostic)
            (string-concat
              (string-concat
                (string-concat \"ERROR \" (local name))
                \": \")
              (record-get (local diagnostic) message)))))
      (fn render-summary
        (params (summary TestSummary))
        (returns String)
        (effects ())
        (requires true)
        (ensures true)
        (string-concat
          (string-concat
            (string-concat
              (string-concat
                (string-concat
                  (string-concat \"Summary: \"
                                 (int->string (record-get (local summary) passed)))
                  \" passed, \")
                (int->string (record-get (local summary) failed)))
              \" failed, \")
            (int->string (record-get (local summary) errored)))
          \" errored\"))
      (fn print-summary
        (params (summary TestSummary))
        (returns Unit)
        (effects (Stdout.Write))
        (requires true)
        (ensures true)
        (loop ((remaining (record-get (local summary) outcomes)))
          (if
            (seq-empty? (local remaining))
            (io/println (call (local render-summary) (local summary)))
            (seq
              (io/println (call (local render-outcome) (seq-first (local remaining))))
              (recur (seq-rest (local remaining)))))))
      (fn print-json-summary
        (params (module-name String) (summary TestSummary))
        (returns Unit)
        (effects (Stdout.Write))
        (requires true)
        (ensures true)
        (io/println (call (local render-json-summary) (local module-name) (local summary))))))"

   'airj/file
   "(module airj/file
      (imports
        (airj airj/core Diagnostic Result)
        (java java.io.File)
        (java java.io.FileOutputStream)
        (java java.io.FileWriter)
        (java java.io.IOException)
        (java java.lang.String)
        (java java.nio.file.Files))
      (export exists?
              read-string
              read-string-result
              read-bytes
              read-bytes-result
              read-lines
              read-lines-result
              write-string
              write-string-result
              write-bytes
              write-bytes-result
              write-lines
              write-lines-result)
      (fn exists?
        (params (path String))
        (returns Bool)
        (effects (Foreign.Throw))
        (requires true)
        (ensures true)
        (java/call
          (java/new java.io.File (local path))
          exists
          (signature () Bool)))
      (fn read-string
        (params (path String))
        (returns String)
        (effects (File.Read Foreign.Throw))
        (requires true)
        (ensures true)
        (java/static-call
          java.nio.file.Files
          readString
          (signature ((Java java.nio.file.Path)) String)
          (java/call
            (java/new java.io.File (local path))
            toPath
            (signature () (Java java.nio.file.Path)))))
      (fn read-bytes
        (params (path String))
        (returns Bytes)
        (effects (File.Read Foreign.Throw))
        (requires true)
        (ensures true)
        (java/static-call
          java.nio.file.Files
          readAllBytes
          (signature ((Java java.nio.file.Path)) Bytes)
          (java/call
            (java/new java.io.File (local path))
            toPath
            (signature () (Java java.nio.file.Path)))))
      (fn write-string
        (params (path String) (contents String))
        (returns Unit)
        (effects (File.Write Foreign.Throw))
        (requires true)
        (ensures true)
        (let ((writer
                (java/new java.io.FileWriter (local path))))
          (try
            (java/call
              (local writer)
              write
              (signature (String) Unit)
              (local contents))
            (finally
            (java/call
              (local writer)
              close
              (signature () Unit))))))
      (fn write-bytes
        (params (path String) (contents Bytes))
        (returns Unit)
        (effects (File.Write Foreign.Throw))
        (requires true)
        (ensures true)
        (let ((stream
                (java/new java.io.FileOutputStream (local path))))
          (try
            (java/call
              (local stream)
              write
              (signature (Bytes) Unit)
              (local contents))
            (finally
              (java/call
                (local stream)
                close
                (signature () Unit))))))
      (fn read-lines
        (params (path String))
        (returns (Seq String))
        (effects (File.Read Foreign.Throw))
        (requires true)
        (ensures true)
        (java/static-call
          java.nio.file.Files
          readAllLines
          (signature ((Java java.nio.file.Path)) (Seq String))
          (java/call
            (java/new java.io.File (local path))
            toPath
            (signature () (Java java.nio.file.Path)))))
      (fn write-lines
        (params (path String) (lines (Seq String)))
        (returns Unit)
        (effects (File.Write Foreign.Throw))
        (requires true)
        (ensures true)
        (call (local write-string)
              (local path)
              (java/static-call
                java.lang.String
                join
                (signature ((Java java.lang.CharSequence) (Java java.lang.Iterable)) String)
                \"\n\"
                (local lines))))
      (fn read-string-result
        (params (path String))
        (returns (Result String Diagnostic))
        (effects (File.Read))
        (requires true)
        (ensures true)
        (try
          (variant (Result String Diagnostic)
                   Ok
                   (call (local read-string) (local path)))
          (catch (Java java.io.IOException) ex
            (variant (Result String Diagnostic)
                     Err
                     (construct Diagnostic
                                \"file\"
                                \"Read failed.\"
                                (local path))))))
      (fn read-bytes-result
        (params (path String))
        (returns (Result Bytes Diagnostic))
        (effects (File.Read))
        (requires true)
        (ensures true)
        (try
          (variant (Result Bytes Diagnostic)
                   Ok
                   (call (local read-bytes) (local path)))
          (catch (Java java.io.IOException) ex
            (variant (Result Bytes Diagnostic)
                     Err
                     (construct Diagnostic
                                \"file\"
                                \"Read bytes failed.\"
                                (local path))))))
      (fn read-lines-result
        (params (path String))
        (returns (Result (Seq String) Diagnostic))
        (effects (File.Read))
        (requires true)
        (ensures true)
        (try
          (variant (Result (Seq String) Diagnostic)
                   Ok
                   (call (local read-lines) (local path)))
          (catch (Java java.io.IOException) ex
            (variant (Result (Seq String) Diagnostic)
                     Err
                     (construct Diagnostic
                                \"file\"
                                \"Read lines failed.\"
                                (local path))))))
      (fn write-string-result
        (params (path String) (contents String))
        (returns (Result Bool Diagnostic))
        (effects (File.Write))
        (requires true)
        (ensures true)
        (try
          (variant (Result Bool Diagnostic)
                   Ok
                   (seq
                     (call (local write-string) (local path) (local contents))
                     true))
          (catch (Java java.io.IOException) ex
            (variant (Result Bool Diagnostic)
                     Err
                     (construct Diagnostic
                                \"file\"
                                \"Write failed.\"
                                (local path))))))
      (fn write-bytes-result
        (params (path String) (contents Bytes))
        (returns (Result Bool Diagnostic))
        (effects (File.Write))
        (requires true)
        (ensures true)
        (try
          (variant (Result Bool Diagnostic)
                   Ok
                   (seq
                     (call (local write-bytes) (local path) (local contents))
                     true))
          (catch (Java java.io.IOException) ex
            (variant (Result Bool Diagnostic)
                     Err
                     (construct Diagnostic
                                \"file\"
                                \"Write bytes failed.\"
                                (local path))))))
      (fn write-lines-result
        (params (path String) (lines (Seq String)))
        (returns (Result Bool Diagnostic))
        (effects (File.Write))
        (requires true)
        (ensures true)
        (try
          (variant (Result Bool Diagnostic)
                   Ok
                   (seq
                     (call (local write-lines) (local path) (local lines))
                     true))
          (catch (Java java.io.IOException) ex
            (variant (Result Bool Diagnostic)
                     Err
                     (construct Diagnostic
                                \"file\"
                                \"Write lines failed.\"
                                (local path)))))))"})

(defn source-map
  []
  standard-sources)

(defn interfaces
  []
  (interface-sources/sources->interfaces standard-sources))

(defn interfaces-for-module
  [module]
  (interface-sources/sources->interfaces (reachable-source-map module)))

(defn stdlib-module?
  [module-name]
  (contains? standard-sources module-name))

(defn- imported-stdlib-modules
  [module]
  (->> (:imports module)
       (filter #(= :airj-import (:op %)))
       (map :module)
       (filter stdlib-module?)
       distinct))

(defn reachable-source-map
  [root-module]
  (loop [pending (vec (imported-stdlib-modules root-module))
         seen {}
         visited #{}]
    (if-let [module-name (first pending)]
      (if (visited module-name)
        (recur (subvec pending 1) seen visited)
        (let [source (get standard-sources module-name)
              module (parser/parse-module source)]
          (recur (into (subvec pending 1) (imported-stdlib-modules module))
                 (assoc seen module-name source)
                 (conj visited module-name))))
      seen)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-15T14:39:15.260935-05:00", :module-hash "-81485556", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 3, :hash "1849576384"} {:id "form/1/declare", :kind "declare", :line 5, :end-line 5, :hash "-1313016324"} {:id "def/standard-sources", :kind "def", :line 7, :end-line 978, :hash "-1544073402"} {:id "defn/source-map", :kind "defn", :line 980, :end-line 982, :hash "981959532"} {:id "defn/interfaces", :kind "defn", :line 984, :end-line 986, :hash "801379587"} {:id "defn/interfaces-for-module", :kind "defn", :line 988, :end-line 990, :hash "-1218712190"} {:id "defn/stdlib-module?", :kind "defn", :line 992, :end-line 994, :hash "1879715354"} {:id "defn-/imported-stdlib-modules", :kind "defn-", :line 996, :end-line 1002, :hash "667554956"} {:id "defn/reachable-source-map", :kind "defn", :line 1004, :end-line 1017, :hash "-1038087388"}]}
;; clj-mutate-manifest-end

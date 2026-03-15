(ns airj.test-runner-spec
  (:require [airj.test-runner :as sut]
            [speclj.core :refer :all]))

(describe "test runner"
  (it "runs exported AIR-J tests from source and summarizes pass fail and error outcomes"
    (let [source "(module example/tests
                    (imports
                      (airj airj/test TestOutcome assert-true assert-false))
                    (export passing failing exploding)
                    (fn passing
                      (params)
                      (returns TestOutcome)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (call (local assert-true) \"passing\" true))
                    (fn failing
                      (params)
                      (returns TestOutcome)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (call (local assert-false) \"failing\" true))
                    (fn exploding
                      (params)
                      (returns TestOutcome)
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (seq
                        (string->int \"boom\")
                        (call (local assert-true) \"exploding\" true))))"
          summary (sut/run-source-tests! source)]
      (should= 1 (:passed summary))
      (should= 1 (:failed summary))
      (should= 1 (:errored summary))
      (should= [{:status "pass"
                 :name "passing"}
                {:status "fail"
                 :name "failing"
                 :diagnostic {:phase "test"
                              :message "Expected false."
                              :detail "Assertion evaluated to true."}}
                {:status "error"
                 :name "exploding"
                 :diagnostic {:phase "runtime"
                              :message "Execution error."
                              :detail "java.lang.NumberFormatException: For input string: \"boom\""}}]
               (:outcomes summary))))

  (it "runs exported AIR-J tests from project sources"
    (let [module-sources
          {'alpha/math "(module alpha/math
                          (imports)
                          (export tick)
                          (fn tick
                            (params (x Int))
                            (returns Int)
                            (effects ())
                            (requires true)
                            (ensures true)
                            (local x)))"
           'example/tests "(module example/tests
                             (imports
                             (airj alpha/math tick)
                             (airj airj/test TestOutcome assert-int-eq))
                             (export tick-test)
                             (fn tick-test
                               (params)
                               (returns TestOutcome)
                               (effects ())
                               (requires true)
                               (ensures true)
                               (call (local assert-int-eq)
                                     \"tick-test\"
                                     (call (local tick) 7)
                                     7)))"}
          summary (sut/run-project-source-tests! module-sources 'example/tests)]
      (should= 1 (:passed summary))
      (should= 0 (:failed summary))
      (should= 0 (:errored summary))
      (should= [{:status "pass"
                 :name "tick-test"}]
               (:outcomes summary))))

  (it "prefers an exported AIR-J tests suite function when present"
    (let [source "(module example/tests
                    (imports
                      (airj airj/test TestOutcome assert-true assert-false))
                    (export tests passing ignored)
                    (fn passing
                      (params)
                      (returns TestOutcome)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (call (local assert-false) \"passing\" true))
                    (fn ignored
                      (params)
                      (returns TestOutcome)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (call (local assert-false) \"ignored\" true))
                    (fn tests
                      (params)
                      (returns (Seq TestOutcome))
                      (effects ())
                      (requires true)
                      (ensures true)
                      (seq-append
                        (seq-empty TestOutcome)
                        (call (local assert-true) \"suite-pass\" true)))))"
          summary (sut/run-source-tests! source)]
      (should= 1 (:passed summary))
      (should= 0 (:failed summary))
      (should= 0 (:errored summary))
      (should= [{:status "pass"
                 :name "suite-pass"}]
               (:outcomes summary)))))

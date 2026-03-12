(ns airj.project-graph-spec
  (:require [airj.project-graph :as sut]
            [speclj.core :refer :all]))

(describe "project-graph"
  (it "collects reachable AIR-J modules from a root"
    (let [sources {'alpha/math "(module alpha/math
                                  (imports)
                                  (export tick)
                                  (fn tick
                                    (params (x Int))
                                    (returns Int)
                                    (effects ())
                                    (requires true)
                                    (ensures true)
                                    (local x)))"
                   'example/use "(module example/use
                                   (imports
                                     (airj alpha/math tick))
                                   (export main)
                                   (fn main
                                     (params)
                                     (returns Int)
                                     (effects ())
                                     (requires true)
                                     (ensures true)
                                     (call (local tick) 1)))"
                   'broken/unused "(module broken/unused
                                     (imports)
                                     (export main)
                                     (fn main
                                       (params)
                                       (returns Int)
                                       (effects ())
                                       (requires true)
                                       (ensures true)
                                       (local nope)))"}]
      (should= #{'alpha/math 'example/use}
               (set (keys (sut/reachable-source-map sources 'example/use))))))

  (it "fails when a reachable imported module source is missing"
    (let [sources {'example/use "(module example/use
                                   (imports
                                     (airj alpha/math tick))
                                   (export main)
                                   (fn main
                                     (params)
                                     (returns Int)
                                     (effects ())
                                     (requires true)
                                     (ensures true)
                                     (call (local tick) 1)))"}]
      (should-throw clojure.lang.ExceptionInfo
                    "Missing project module source."
                    (sut/reachable-source-map sources 'example/use))))

  (it "fails on AIR-J import cycles"
    (let [sources {'alpha/math "(module alpha/math
                                  (imports
                                    (airj beta/math ping))
                                  (export tick)
                                  (fn tick
                                    (params (x Int))
                                    (returns Int)
                                    (effects ())
                                    (requires true)
                                    (ensures true)
                                    (call (local ping) (local x))))"
                   'beta/math "(module beta/math
                                 (imports
                                   (airj alpha/math tick))
                                 (export ping)
                                 (fn ping
                                   (params (x Int))
                                   (returns Int)
                                   (effects ())
                                   (requires true)
                                   (ensures true)
                                   (call (local tick) (local x))))"}]
      (should-throw clojure.lang.ExceptionInfo
                    "Project import cycle."
                    (sut/reachable-source-map sources 'alpha/math)))))

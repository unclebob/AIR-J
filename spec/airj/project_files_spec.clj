(ns airj.project-files-spec
  (:require [airj.project-files :as sut]
            [clojure.java.io :as io]
            [speclj.core :refer :all]))

(defn- temp-dir
  [prefix]
  (.toString (java.nio.file.Files/createTempDirectory
              prefix
              (make-array java.nio.file.attribute.FileAttribute 0))))

(describe "project-files"
  (it "discovers AIR-J module sources from a project directory"
    (let [project-dir (temp-dir "airj-project-files")
          _ (spit (io/file project-dir "alpha.airj")
                  "(module alpha/math
                     (imports)
                     (export tick)
                     (fn tick
                       (params (x Int))
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (local x)))")
          _ (doto (io/file project-dir "nested") .mkdirs)
          _ (spit (io/file project-dir "nested" "use.airj")
                  "(module example/use
                     (imports
                       (airj alpha/math tick))
                     (export main)
                     (fn main
                       (params)
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (call (local tick) 1)))")
          _ (spit (io/file project-dir "ignored.txt") "nope")]
      (should= #{'alpha/math 'example/use}
               (set (keys (sut/source-map project-dir))))))

  (it "derives compiler options for one root module from sibling AIR-J files"
    (let [project-dir (temp-dir "airj-project-options")
          _ (spit (io/file project-dir "math.airj")
                  "(module alpha/math
                     (imports)
                     (export tick)
                     (fn tick
                       (params (x Int))
                       (returns Int)
                       (effects (Clock.Read))
                       (requires true)
                       (ensures true)
                       (local x)))")
          _ (spit (io/file project-dir "use.airj")
                  "(module example/use
                     (imports
                       (airj alpha/math tick))
                     (export main)
                     (fn main
                       (params)
                       (returns Int)
                       (effects (Clock.Read))
                       (requires true)
                       (ensures true)
                       (call (local tick) 1)))")]
      (should= ['Clock.Read]
               (-> (sut/compiler-options project-dir 'example/use)
                   :interfaces
                   (get 'alpha/math)
                   :decls
                   first
                   :effects))))

  (it "selects only the root module and its reachable AIR-J imports"
    (let [project-dir (temp-dir "airj-project-reachable")
          _ (spit (io/file project-dir "math.airj")
                  "(module alpha/math
                     (imports)
                     (export tick)
                     (fn tick
                       (params (x Int))
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (local x)))")
          _ (spit (io/file project-dir "use.airj")
                  "(module example/use
                     (imports
                       (airj alpha/math tick))
                     (export main)
                     (fn main
                       (params)
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (call (local tick) 1)))")
          _ (spit (io/file project-dir "broken.airj")
                  "(module broken/unused
                     (imports)
                     (export main)
                     (fn main
                       (params)
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (local nope)))")]
      (should= #{'alpha/math 'example/use}
               (set (keys (sut/reachable-source-map project-dir 'example/use)))))))

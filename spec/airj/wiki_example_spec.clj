(ns airj.wiki-example-spec
  (:require [airj.compiler :as compiler]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [speclj.core :refer :all])
  (:import (java.net ServerSocket URI URLEncoder)
           (java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers)
           (java.nio.file Files)))

(defn- binary-name
  [internal-name]
  (.replace ^String internal-name \/ \.))

(defn- define-classes
  [bytecode-map]
  (let [loader (clojure.lang.DynamicClassLoader.)]
    (into {}
          (map (fn [[internal-name bytecode]]
                 [internal-name
                  (.defineClass loader (binary-name internal-name) bytecode nil)]))
          (sort-by key bytecode-map))))

(defn- wiki-server-classes
  []
  (-> (compiler/compile-project-dir "examples/Wiki" 'example/wiki_server)
      define-classes))

(defn- free-port
  []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn- client
  []
  (HttpClient/newHttpClient))

(defn- get-text
  [port path]
  (.send (client)
         (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" port path)))
             (.GET)
             (.build))
         (HttpResponse$BodyHandlers/ofString)))

(defn- post-form
  [port path body]
  (.send (client)
         (-> (HttpRequest/newBuilder (URI/create (str "http://127.0.0.1:" port path)))
             (.header "Content-Type" "application/x-www-form-urlencoded")
             (.POST (HttpRequest$BodyPublishers/ofString body))
             (.build))
         (HttpResponse$BodyHandlers/ofString)))

(defn- encode-form
  [pairs]
  (clojure.string/join
   "&"
   (map (fn [[k v]]
          (str (URLEncoder/encode k "UTF-8")
               "="
               (URLEncoder/encode v "UTF-8")))
        pairs)))

(defn- method
  [klass name parameter-types]
  (.getMethod klass name (into-array Class parameter-types)))

(describe "wiki example"
  (let [classes (delay (wiki-server-classes))
        wiki-server-class (delay (get @classes "example/wiki_server"))
        http-server-class (delay (get @classes "airj/http$HttpServer"))
        start-server (delay (method @wiki-server-class
                                    "start_server"
                                    [(class (into-array String []))]))
        current-port (delay (method @wiki-server-class
                                    "current_port"
                                    [@http-server-class]))
        initial-state (delay (method @wiki-server-class
                                     "initial_state"
                                     [String]))
        serve-once (delay (method @wiki-server-class
                                  "serve_once"
                                  [@http-server-class
                                   (get @classes "example/wiki$Wiki")
                                   String]))
        stop-server (delay (method @wiki-server-class
                                  "stop_server"
                                  [@http-server-class]))]
    (it "serves canonical JSON routes over HTTP"
      (let [requested-port (free-port)
            state-path (.toString (Files/createTempFile "airj-wiki-state" ".json" (make-array java.nio.file.attribute.FileAttribute 0)))
            server (.invoke @start-server
                            nil
                            (object-array [(into-array String [(str requested-port) state-path])]))
            actual-port (.invoke @current-port nil (object-array [server]))
            initial-wiki (.invoke @initial-state nil (object-array [state-path]))]
        (try
          (let [create-future (future (.invoke @serve-once nil (object-array [server initial-wiki state-path])))
                create-response (post-form actual-port
                                           "/api/create"
                                           (encode-form [["title" "Docs"]
                                                         ["body" "# Hello"]]))
                wiki-after-create @create-future
                page-future (future (.invoke @serve-once nil (object-array [server wiki-after-create state-path])))
                page-response (get-text actual-port "/api/page/Docs")
                wiki-after-page @page-future
                history-future (future (.invoke @serve-once nil (object-array [server wiki-after-page state-path])))
                history-response (get-text actual-port "/api/history/Docs")
                wiki-after-history @history-future
                search-future (future (.invoke @serve-once nil (object-array [server wiki-after-history state-path])))
                search-response (get-text actual-port "/api/search/Hello")
                wiki-after-search @search-future
                pages-future (future (.invoke @serve-once nil (object-array [server wiki-after-search state-path])))
                pages-response (get-text actual-port "/api/pages")
                wiki-after-pages @pages-future
                missing-future (future (.invoke @serve-once nil (object-array [server wiki-after-pages state-path])))
                missing-response (get-text actual-port "/api/page/Missing")
                _ @missing-future
                page-json (json/read-str (.body page-response))
                history-json (json/read-str (.body history-response))
                search-json (json/read-str (.body search-response))
                pages-json (json/read-str (.body pages-response))
                missing-json (json/read-str (.body missing-response))]
            (should= 200 (.statusCode create-response))
            (should= ["application/json; charset=utf-8"]
                     (get (.map (.headers page-response)) "content-type"))
            (should= "Docs" (get page-json "title"))
            (should= "# Hello" (get page-json "body"))
            (should= "Docs" (get history-json "title"))
            (should= ["# Hello"] (get history-json "history"))
            (should= "Hello" (get search-json "query"))
            (should (some #{"Docs"} (get search-json "titles")))
            (should (some #{"Home"} (get pages-json "titles")))
            (should (some #{"Docs"} (get pages-json "titles")))
            (should= 404 (.statusCode missing-response))
            (should= "page.not_found" (get-in missing-json ["error" "detail"])))
          (finally
            (.invoke @stop-server nil (object-array [server]))))))

    (it "persists wiki state across restarts"
      (let [state-path (.toString (Files/createTempFile "airj-wiki-restart" ".json" (make-array java.nio.file.attribute.FileAttribute 0)))
            first-port (free-port)
            second-port (free-port)
            first-server (.invoke @start-server
                                  nil
                                  (object-array [(into-array String [(str first-port) state-path])]))]
        (try
          (let [wiki-before-write (.invoke @initial-state nil (object-array [state-path]))
                write-future (future (.invoke @serve-once nil (object-array [first-server wiki-before-write state-path])))
                write-response (post-form (.invoke @current-port nil (object-array [first-server]))
                                          "/api/update"
                                          (encode-form [["title" "Home"]
                                                        ["body" "# Persisted"]]))]
            (should= 200 (.statusCode write-response))
            @write-future)
          (finally
            (.invoke @stop-server nil (object-array [first-server]))))
        (let [second-server (.invoke @start-server
                                     nil
                                     (object-array [(into-array String [(str second-port) state-path])]))]
          (try
            (let [wiki-before-read (.invoke @initial-state nil (object-array [state-path]))
                  read-future (future (.invoke @serve-once nil (object-array [second-server wiki-before-read state-path])))
                  page-response (get-text (.invoke @current-port nil (object-array [second-server])) "/api/page/Home")
                  page-json (json/read-str (.body page-response))
                  file-json (json/read-str (slurp (io/file state-path)))]
              (should= 200 (.statusCode page-response))
              (should= "# Persisted" (get page-json "body"))
              (should= "# Persisted" (get-in file-json ["pages" 0 "body"]))
              @read-future)
            (finally
              (.invoke @stop-server nil (object-array [second-server])))))))

    (it "renders a create link for missing wiki pages"
      (let [requested-port (free-port)
            state-path (.toString (Files/createTempFile "airj-wiki-missing-link" ".json" (make-array java.nio.file.attribute.FileAttribute 0)))
            server (.invoke @start-server
                            nil
                            (object-array [(into-array String [(str requested-port) state-path])]))
            actual-port (.invoke @current-port nil (object-array [server]))
            initial-wiki (.invoke @initial-state nil (object-array [state-path]))]
        (try
          (let [create-future (future (.invoke @serve-once nil (object-array [server initial-wiki state-path])))
                create-response (post-form actual-port
                                           "/create"
                                           (encode-form [["title" "Docs"]
                                                         ["body" "See [[Missing Guide]]"]]))
                wiki-after-create @create-future
                view-future (future (.invoke @serve-once nil (object-array [server wiki-after-create state-path])))
                view-response (get-text actual-port "/view/Docs")
                wiki-after-view @view-future
                new-future (future (.invoke @serve-once nil (object-array [server wiki-after-view state-path])))
                new-response (get-text actual-port "/new/Missing+Guide")
                _ @new-future]
            (should= 200 (.statusCode create-response))
            (should= 200 (.statusCode view-response))
            (should (str/includes? (.body view-response) "<a href=\"/new/Missing Guide\">?</a>"))
            (should= 200 (.statusCode new-response))
            (should (str/includes? (.body new-response) "value=\"Missing Guide\"")))
          (finally
            (.invoke @stop-server nil (object-array [server]))))))))

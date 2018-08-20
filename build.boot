(set-env!
 :source-paths   #{"src"}
 :resource-paths #{}
 :dependencies '[[org.clojure/clojure "1.9.0" :scope "provided"]
                 [boot/core "2.8.1" :scope "provided"]
                 [org.clojure/tools.nrepl "0.2.13" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.reader
                  "1.3.0"
                  :exclusions
                  [org.clojure/clojure]]
                 [clj-stable-pprint "0.0.3" :exclusions [org.clojure/clojure]]
                 [adzerk/bootlaces "0.1.13" :scope "test"]
                 [metosin/boot-alt-test "0.3.3" :scope "test"]
                 [adzerk/boot-test "1.2.0" :scope "test"]
                 [clj-jgit "0.8.10"]])

(require
 '[adzerk.bootlaces :refer [bootlaces! build-jar push-snapshot push-release]]
 '[metosin.boot-alt-test :refer [alt-test]]
 '[adzerk.boot-test :refer [test]]
 '[boot.core        :as core :refer [deftask]])

(def +version+ "0.0.0-SNAPSHOT")

(bootlaces! +version+)

(task-options!
 pom {:project     'nha/boot-deps
      :version     +version+
      :description "Boot task to manage dependencies"
      :url         "https://github.com/nha/boot-deps"
      :scm         {:url "https://github.com/nha/boot-deps"}
      :license     {"Eclipse Public License" "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask dev
  "Dev process"
  []
  (comp
   (watch)
   (repl :server true)
   (pom)
   (jar)
   (install)))

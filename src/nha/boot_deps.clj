(ns nha.boot-deps
  (:require [boot.core :as core :refer [deftask with-pass-thru with-pre-wrap tmp-dir! commit! add-resource]]
            [boot.pedantic :as pedant]
            [boot.pod :as pod]
            [boot.util]
            [boot.git]
            ;;[boot.jgit]
            [clj-jgit.porcelain]
            [clj-jgit.querying]
            [clj-jgit.util]
            [clj-stable-pprint.core]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            ))

(deftask check-conflicts
  "Verify there are no dependency conflicts."
  []
  (with-pass-thru fs
    (if-let [conflicts (not-empty (pedant/dep-conflicts pod/env))]
      (throw (ex-info "Unresolved dependency conflicts. Use :exclusions to resolve them." conflicts))
      (boot.util/info "No dependency conflict"))))

(defn- dependencies->map [deps]
  (->> deps
       (map boot.util/dep-as-map)
       (reduce (fn [m p] (assoc m (:project p) p)) {})))

(deftest dependencies->map-test
  (is (= {'projectA {:scope "compile", :project 'projectA, :version "0.0.0"},
          'projectB {:scope "compile", :project 'projectB, :version "0.0.1"}}
         (dependencies->map [['projectA "0.0.0"]
                             ['projectB "0.0.1"]]))))

(defn map->dependencies [m]
  (mapv boot.util/map-as-dep (vals m)))

(deftest map->dependencies-test
  (is (=  [['projectA "0.0.0"]
           ['projectB "0.0.1"]]
          (map->dependencies {'projectA {:scope "compile", :project 'projectA, :version "0.0.0"},
                              'projectB {:scope "compile", :project 'projectB, :version "0.0.1"}})))
  (testing "can roundtrip "
    (let [original [['projectA "0.0.0"]
                    ['projectB "0.0.1"]]]
      (is (= original
             (map->dependencies (dependencies->map original)))))))

(defn- index-dependencies [dependencies]
  (reduce (fn [acc dep] (assoc acc (first dep) dep)) {} dependencies))

(deftest index-dependencies-test
  (is (= {'projectA ['projectA "0.0.0"],
          'projectB ['projectB "0.0.1"]}
         (index-dependencies [['projectA "0.0.0"]
                              ['projectB "0.0.1"]]))))

(defn- deindex-dependencies [dependencies]
  (vec (vals dependencies)))

(deftest deindex-dependencies-test
  (is (= [['projectA "0.0.0"]
          ['projectB "0.0.1"]]
         (deindex-dependencies {'projectA ['projectA "0.0.0"],
                                'projectB ['projectB "0.0.1"]})))
  (testing "can roundtrip"
    (let [original [['projectA "0.0.0"]
                    ['projectB "0.0.1"]]]
      (is (= original (deindex-dependencies (index-dependencies original)))))))

(defn- exclude-dependency [mapped-dependencies exclude]
  (let [[project guilty-projects] exclude]
    (reduce (fn [acc gp]
              (update acc gp #(boot.pod/apply-exclusions [project] %)))
            mapped-dependencies guilty-projects)))

(deftest exclude-dependency-test
  (let [deps {'projectA ['projectA "0.0.0"],
              'projectB ['projectB "0.0.1"]
              'projectC ['projectC "0.0.2"]}
        excl ['bad-deps '(projectB projectC)]]
    (is (= {'projectA ['projectA "0.0.0"],
            'projectB ['projectB "0.0.1" :exclusions ['bad-deps]]
            'projectC ['projectC "0.0.2" :exclusions ['bad-deps]]}
           (exclude-dependency deps excl)))))

(defn- exclude-dependencies [mapped-dependencies exclude]
  (let [exclusions (into [] exclude)]
    (reduce exclude-dependency mapped-dependencies exclusions)))

(deftest exclude-dependencies-test
  (let [deps {'projectA ['projectA "0.0.0"],
              'projectB ['projectB "0.0.1"]
              'projectC ['projectC "0.0.2"]}
        excl {'bad-deps1 '(projectA projectB)
              'bad-deps2 '(projectB projectC)}]
    (is (= {'projectA ['projectA "0.0.0" :exclusions ['bad-deps1]],
            'projectB ['projectB "0.0.1" :exclusions ['bad-deps1 'bad-deps2]]
            'projectC ['projectC "0.0.2" :exclusions ['bad-deps2]]}
           (exclude-dependencies deps excl)))))

(defn- missing-exclusions
  "given a conflict map,
  returns a sub-map that represent the :exclusions
  to be added to some of the :dependencies"
  [conflicts]
  (->> (into [] conflicts)
       (map (fn [[project conflicting-versions]]
              (let [by-version (sort-by identity boot.pedantic/compare-version (keys conflicting-versions))
                    most-recent (last by-version) ;; choosen version
                    missing-exclusions [project (dissoc conflicting-versions most-recent)]]
                missing-exclusions)))
       (into {})))

(deftest missing-exclusions-test
  (let [conflicts {'conflict1 {"0.0.1" '(projectA projectB)
                               "0.0.2" '(projectC projectD)}
                   'conflict2 {"0.0.1" '(projectA)
                               "0.0.2" '(projectD)}}
        expected {'conflict1 {"0.0.1" '(projectA projectB)},
                  'conflict2 {"0.0.1" '(projectA)}}]
    (is (= expected (missing-exclusions conflicts)))))

(defn- loose-version [conflicts]
  (->> conflicts
       (into [])
       (map (fn [[k v]]
              [k (apply concat (vals v))]))
       (into {})))

(deftest loose-version-test
  (let [c {'conflict1 {"0.0.1" '(projectA projectB)
                       "0.0.2" '(projectC)},
           'conflict2 {"0.0.1" '(projectA)}}
        res {'conflict1 '(projectA projectB projectC),
             'conflict2 '(projectA)}]
    (is (= res (loose-version c)))))

(defn keep-latest [env conflicts]
  (let [missing-exclusions (missing-exclusions conflicts)]
    (update env :dependencies (fn [dependencies]
                                (deindex-dependencies (exclude-dependencies (index-dependencies dependencies) (loose-version missing-exclusions)))))))

(deftest keep-latest-test
  (let [conflicts {'conflict1 {"0.0.1" '(projectA projectB)
                               "0.0.2" '(projectC projectD)}
                   'conflict2 {"0.0.1" '(projectA)
                               "0.0.2" '(projectD)}}
        env {:dependencies [['projectA "0.0.0"],
                            ['projectB "0.0.1"]
                            ['projectC "0.0.2"]
                            ['projectD "0.0.2"]]}]
    (is (= {:dependencies [['projectA "0.0.0" :exclusions ['conflict1 'conflict2]]
                           ['projectB "0.0.1" :exclusions ['conflict1]]
                           ['projectC "0.0.2"]
                           ['projectD "0.0.2"]]}
           (keep-latest env conflicts)))))

(deftask write-resolved
  "build a resources/exclusions.edn file that is conflict-free, by excluding older versions.
   This is to be run (for instance) every time a package is added, and added to the global :exclusions key"
  [f filepath  VAL  str  "path where to write the .edn file"]
  (with-pass-thru fs
    (if-let [conflicts (not-empty (pedant/dep-conflicts pod/env))]
      (let [conflicts-free (:dependencies (keep-latest pod/env conflicts))]
        (boot.util/info (str "• Writing conflict-free dependency file:" filepath "\n"))
        (io/make-parents filepath)
        (spit (io/file filepath)
              (with-out-str (clj-stable-pprint.core/pprint conflicts-free))))
      (boot.util/info (str "No dependency conflict.")))))

(deftask update-deps
  "Fetch and load dependencies from edn file"
  [d dependencies  VAL edn  "edn dependency vector"]
  (boot.core/with-pre-wrap [fs]
    (boot.util/info "Updating deps... \n" )
    (boot.core/set-env! :dependencies dependencies)
    fs))

(comment

  (:dependencies (keep-latest pod/env (pedant/dep-conflicts pod/env)))

  (boot.core/boot (comp (nha.boot-deps/write-resolved :filepath "aa/deps.edn")
                        (boot.task.built-in/target)))

  (let [res (:dependencies (keep-latest pod/env (pedant/dep-conflicts pod/env)))]
    (spit (io/file "dependencies-fix.edn")
          (with-out-str (clj-stable-pprint.core/pprint res))))

  (boot (write-global-exclusions) (write-resolved))

  (clojure.test/run-all-tests)
  )


(deftask add-version-txt
  "write a version to a text file"
  [v version  VAL  str  "version"
   f filepath  VAL  str "path where to write the .edn file"]
  (with-pre-wrap fs
    (let [t (tmp-dir!)]
      (spit (clojure.java.io/file t "version.txt") version)
      (-> fs (add-resource t) commit!))))

(defn jgit-sha1 []
  (clj-jgit.porcelain/with-repo "./"
    (.getName (first (clj-jgit.porcelain/git-log repo)))))

(defn git-sha1 []
  (or (System/getenv "CIRCLE_SHA1")
      (System/getenv "GIT_SHA1")
      (jgit-sha1)
      (some-> "git_sha.txt" clojure.java.io/resource slurp clojure.string/trim)))

(deftask add-git-sha-txt []
  (with-pre-wrap fs
    (let [t (tmp-dir!)]
      (spit (clojure.java.io/file t "git_sha.txt") (git-sha1))
      (-> fs (add-resource t) commit!))))

(comment
  (boot.core/boot (add-git-sha-txt) (add-version-txt :version "0.0.1") (boot.task.built-in/target))
  )

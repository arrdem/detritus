(defproject me.arrdem/detritus "_"
  :description "Arrdem's util library"
  :url "http://github.com/arrdem/detritus"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]]

  :plugins [[me.arrdem/lein-git-version "2.0.5"]
            [me.arrdem/lein-auto "0.1.4"]
            [lein-cljfmt "0.5.7"]]

  :release-tasks [["vcs" "assert-committed"]
                  ["update-in" ":version" "leiningen.release/bump-version" "release"]
                  ["vcs" "tag"]
                  ["vcs" "push"]
                  ["deploy"]]

  :git-version
  {:status-to-version
   (fn [{:keys [tag version branch ahead ahead? dirty?] :as git}]
     (if (and tag (not ahead?) (not dirty?))
       (do (assert (re-find #"\d+\.\d+\.\d+" tag)
                   "Tag is assumed to be a raw SemVer version")
           tag)
       (if (and tag (or ahead? dirty?))
         (let [[_ prefix patch] (re-find #"(\d+\.\d+)\.(\d+)" tag)
               patch            (Long/parseLong patch)
               patch+           (inc patch)]
           (format "%s.%d%s-SNAPSHOT" prefix patch+
                   (if (and (not= "master" branch)
                            (not (.contains branch "/")))
                     (str \- branch) "")))
         "0.1.0-SNAPSHOT")))})

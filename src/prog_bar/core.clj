(ns prog-bar.core
  (:require [taoensso.timbre :as t]
            [progrock.core :as pr]
            [com.climate.claypoole :as cp]))

(defn safe-println
  "From http://yellerapp.com/posts/2014-12-11-14-race-condition-in-clojure-println.html"
  [& more] (.write *out* (str (clojure.string/join " " more) "\n")))

(defn safe-print
  "From http://yellerapp.com/posts/2014-12-11-14-race-condition-in-clojure-println.html"
  [& more] (.write *out* (str (clojure.string/join " " more))))


(defn progress-println
  ([bar ]
   (safe-print (pr/render bar) "\r")
   (flush))
  ([bar & r]
   (safe-print "\r                                                                                                                       \r")
   (apply safe-println r)
   (safe-print (pr/render bar) "\r")
   (flush)))

(defn bar-printer-fn [data]
  (let [{:keys [output_ context]} data]
    (binding [*out* (if (:error? data) *err* *out*)]
      (progress-println context (force output_))
      )))

(defn map-prog
  "a wrapper for `map` functions, which adds a progress bar. 
   
   NOTE: Results here are evaluated EAGERLY!!"
  [mapper f & ss]
  (t/with-merged-config {:appenders {:println {:fn bar-printer-fn}}}
    (let [bar (atom (pr/progress-bar (reduce min (map count ss))))]
      (t/with-context @bar
        (let [m-fn (fn [& x]
                     (progress-println @bar)
                     (let [r (apply f x)]
                       (swap! bar pr/tick)
                       r))
              r (doall (apply mapper m-fn ss))] ;; eagerly evaluate results
          (pr/print (pr/done @bar))
          (flush)
          r)))))

(defn -main []
  (println "Starting...") (flush)
  (Thread/sleep 1000)
  (println (map-prog map (fn[x] (Thread/sleep 300) (t/info "Processing " x) (inc x)) (range 20)))
  (println "Next...") (flush)
  (Thread/sleep 1000)
  (println (map-prog (partial cp/upmap 10) (fn[x] (Thread/sleep 300) (t/info "Processing " x) (inc x)) (range 20)))
  (println "done!")

)


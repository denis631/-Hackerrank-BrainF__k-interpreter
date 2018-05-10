(ns brainfuck-interpreter.core
  (:gen-class))

;; Sample programs
(def plus-3-minus-3-program ",+. This program will 6 characters\n,+. For first 3 characters it will\n,+. print its successor\n,-. For last 3 characters it will\n,-. print its predecessor\n,-.")
(def hello-world-program "+++++ +++++             initialize counter (cell #0) to 10\n[                       use loop to set the next four cells to 70/100/30/10\n    > +++++ ++              add  7 to cell #1\n    > +++++ +++++           add 10 to cell #2\n    > +++                   add  3 to cell #3\n    > +                     add  1 to cell #4\n    <<<< -                  decrement counter (cell #0)\n]\n> ++ .                  print 'H'\n> + .                   print 'e'\n+++++ ++ .              print 'l'\n.                       print 'l'\n+++ .                   print 'o'\n> ++ .                  print ' '\n<< +++++ +++++ +++++ .  print 'W'\n> .                     print 'o'\n+++ .                   print 'r'\n----- - .               print 'l'\n----- --- .             print 'd'\n> + .                   print '!'")

;; Constants
(def default-tape-length 30000)
(def operation-count-threshold 100000)
(def language-symbols #{\> \< \, \. \- \+ \[ \]})

;; Brainfuck grammar
;; EXPR_LIST = EXPR EXPR_LIST | eps
;; EXPR = < | > | + | - | , | . | LOOP
;; LOOP = [ EXPR_LIST ]

(defn get-cell [runtime-metadata] ((:tape runtime-metadata) (:ptr runtime-metadata)))

(defn update-cell
  [runtime-metadata f]
  (update-in runtime-metadata [:tape]
             (fn [tape] (update-in tape [(:ptr runtime-metadata)] f))))

(defmulti execute (fn [expr runtime-metadata]
                    (if (< (:op-count runtime-metadata) operation-count-threshold)
                      (::bf-expr expr)
                      (throw (Exception. "\nPROCESS TIME OUT. KILLED!!!")))))

(defmethod execute ::< [_ runtime-metadata] (-> runtime-metadata
                                                (update-in [:ptr] dec)
                                                (update-in [:op-count] inc)))
(defmethod execute ::> [_ runtime-metadata] (-> runtime-metadata
                                                (update-in [:ptr] inc)
                                                (update-in [:op-count] inc)))
(defmethod execute ::plus [_ runtime-metadata] (-> runtime-metadata
                                                   (update-cell #(if (= % 255) 0 (inc %)))
                                                   (update-in [:op-count] inc)))
(defmethod execute ::minus [_ runtime-metadata] (-> runtime-metadata
                                                    (update-cell #(if (= % 0) 255 (dec %)))
                                                    (update-in [:op-count] inc)))
(defmethod execute ::comma [_ runtime-metadata] (-> runtime-metadata
                                                    (update-cell (fn [_] (int (first (:data runtime-metadata)))))
                                                    (update-in [:data] rest)
                                                    (update-in [:op-count] inc)))
(defmethod execute ::dot [_ runtime-metadata] (do (print (char (get-cell runtime-metadata)))
                                                  (update-in runtime-metadata [:op-count] inc)))
(defmethod execute ::loop [loop runtime-metadata]
  (let [cell-check (fn [runtime-metadata f]
                     (let [runtime-metadata (update-in runtime-metadata [:op-count] inc)]
                       (if-not (= 0 (get-cell runtime-metadata))
                         (f runtime-metadata)
                         runtime-metadata)))]
    (-> runtime-metadata
        ;; at [
        (cell-check (fn [runtime-metadata] (execute (:exprs loop) runtime-metadata)))
        ;; at ]
        (cell-check (fn [runtime-metadata] (execute loop runtime-metadata))))))
(defmethod execute :default [exprs runtime-metadata] (reduce (fn [runtime-metadata expr] (execute expr runtime-metadata))
                                                             runtime-metadata
                                                             exprs))

(defn scan [code] (->> code
                       (filter #(contains? language-symbols %))
                       (apply str)
                       (into [])))

(defn parse
  ([tokens] (parse tokens 0 []))
  ([tokens start acc]
   (if (< start (count tokens))
     (case (tokens start)
       \< (recur tokens (inc start) (conj acc {::bf-expr ::<}))
       \> (recur tokens (inc start) (conj acc {::bf-expr ::>}))
       \+ (recur tokens (inc start) (conj acc {::bf-expr ::plus}))
       \- (recur tokens (inc start) (conj acc {::bf-expr ::minus}))
       \, (recur tokens (inc start) (conj acc {::bf-expr ::comma}))
       \. (recur tokens (inc start) (conj acc {::bf-expr ::dot}))
       \[ (let [[end exprs] (parse tokens (inc start) [])]
            (recur tokens (inc end) (conj acc {::bf-expr ::loop :exprs exprs})))
       \] [start acc])
     acc)))

(defn read-code
  [line-count]
  (loop [i 0 acc []]
    (if (< i line-count)
      (recur (inc i) (conj acc (read-line)))
      (apply str acc))))

(defn -main [& args]
  (let [[input-count line-count] (->> (clojure.string/split (read-line) #" ")
                                      ;; map to ints
                                      (map #(Integer/parseInt %)))
        ;; remove last symbol ($) from the input string. Read all input beforehand
        data (take input-count (read-line))
        ;; read the code
        code (read-code line-count)
        ;; allocate tape memory
        tape (into [] (repeat default-tape-length 0))
        ;; runtime-metadata setup
        runtime-metadata {:tape tape :ptr 0 :data data :op-count 0}]
    (try
      (execute (parse (scan code)) runtime-metadata)
      (catch Exception e (print (.getMessage e))))))
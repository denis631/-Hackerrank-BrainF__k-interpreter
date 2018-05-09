(ns brainfuck-interpreter.core
  (:gen-class))

;; Example programs
(def hello-world-program "+++++ +++++ [ > +++++ ++ > +++++ +++++ > +++ > + <<<< - ] > ++ . > + . +++++ ++ . . +++ . > ++ . << +++++ +++++ +++++ . > . +++ . ----- - . ----- --- . > + .")

(def default-tape-length 30000)
(def operation-count-threshold 100000)
(def language-symbols #{\> \< \, \. \- \+ \[ \]})

(defn tokenize [code] (->> code
                           (filter #(contains? language-symbols %))
                           (apply str)
                           (into [])))

(defn execute
  [tokens tape ptr data]
  (loop [i 0
         tape tape
         ptr ptr
         data data
         loop-i []
         op-count 0]
    (when (< i (count tokens))
      (println (map #(int %) (take 5 tape)))
      (if-not (< op-count operation-count-threshold)
        (print "\nPROCESS TIME OUT. KILLED!!!")
        (case (tokens i)
          \> (recur (inc i) tape (inc ptr) data loop-i (inc op-count))
          \< (recur (inc i) tape (dec ptr) data loop-i (inc op-count))
          \, (recur (inc i)
                    (update-in tape [ptr] (fn [_] (int (first data))))
                    ptr (rest data) loop-i (inc op-count))
          \. (do (print (char (tape ptr)))
                 (recur (inc i) tape ptr data loop-i (inc op-count)))
          \- (recur (inc i) (update-in tape [ptr] #(if (= % 0) 225 (dec %))) ptr data loop-i (inc op-count))
          \+ (recur (inc i) (update-in tape [ptr] #(if (= % 255) 0 (inc %))) ptr data loop-i (inc op-count))
          \[ (recur (inc i) tape ptr data (conj loop-i (inc i)) (inc op-count))
          \] (if (= 0 (tape ptr))
               ;; skip loop
               (recur (inc i) tape ptr data (pop loop-i) (+ 2 op-count))
               ;; jump to first loop instruction
               (recur (peek loop-i) tape ptr data loop-i (+ 2 op-count))))))))

(defn read-code
  [instruction-count]
  (loop [i 0 acc []]
    (if (< i instruction-count)
      (recur (inc i) (conj acc (read-line)))
      (apply str acc))))

(defn -main [& args]
  (let [instruction-count (->> (clojure.string/split (read-line) #" ")
                               ;; map to ints
                               (map #(Integer/parseInt %))
                               ;; skip first int, representing string length
                               (second))
        ;; remove last symbol ($) from the input string
        data (drop-last (read-line))
        code (read-code instruction-count)
        tape (into [] (repeat default-tape-length 0))
        ptr 0]
    (execute (tokenize code) tape ptr data)))
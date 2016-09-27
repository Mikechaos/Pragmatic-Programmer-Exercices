(require '[clojure.string :as str])

(def debug-mode? false)

(defn toggle-debug [] (second (list (def debug-mode? (not debug-mode?)) debug-mode?)))
(defn- helper-prn-debug 
  ([s] (prn s))
  ([s & r] (prn s) (map prn r)))

(defmacro prn-debug [& args]
  `(if ~debug-mode? (apply helper-prn-debug '~args ) ~nil))

(def possible-formats ["4pm" "7:38pm" "23:42" "3:16" "03:16" "3:16am" "15h15", "1h24pm"])
(def error-formats ["4ampm" "17:38pm" "23h:42" "3:011" "03:16pm" "15h61", "24h24"])

; Grammar
;
; <time>                 ::= <12-hour><meridiem-indicator> | <12-hour><separator><minute><meridiem-indicator> | <24-hour><separator><minute> | <12-hour><separator><minute>
; <12-hour>              ::= <digit> | "10" | "11" | "12"
; <24-hour>              ::= <first-digit-hour><digit> | "2"<second-digit-hour>
; <minute>               ::= <first-digit-minute><second-digit-minute> | ""
; <digit>                ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
; <first-digit-hour>     ::= "0" | "1"
; <second-digit-hour>    ::= "0" | "1" | "2" | "3" | "4"
; <first-digit-minute>   ::= "0" | "1" | "2" | "3" | "4" | "5"
; <second-digit-minute>  ::= <digit>
; <separator>            ::= ":" | ""
; <meridiem-indicator>   ::= "am" | "pm"
;


(defn trim-program
  "Returns a program with no 2 consecutive spaces"
  [program] (vec (map #(str/replace % #"\s+" " ") program)))

(defmacro def-grammar [grammar-name primary grammar]
  `(def ~grammar-name (with-meta (trim-program ~grammar) {:primary ~primary})))

(def-grammar grammar :<time> [
  "<time>                 ::= <12-hour><meridiem-indicator> | <12-hour><separator><minute><meridiem-indicator> | <24-hour><separator><minute>"
  "<12-hour>              ::= <digit> | <two-digit-hour>"
  "<24-hour>              ::= <first-digit-hour><digit> | <2><second-digit-hour> | <12-hour>"
  "<minute>               ::= <first-digit-minute><digit>"
  "<digit>                ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9"
  "<two-digit-hour>       ::= 10 | 11 | 12"
  "<first-digit-hour>     ::= 0 | 1"
  "<second-digit-hour>    ::= 0 | 1 | 2 | 3"
  "<first-digit-minute>   ::= 0 | 1 | 2 | 3 | 4 | 5"
  ; "<second-digit-minute>  ::= <digit>"
  "<separator>            ::= : | <h>"
  "<2>                    ::= 2"
  "<h>                    ::= h"
  "<meridiem-indicator>   ::= am | pm"])

(defn get-primary-term [grammar] (:primary (meta grammar)))

(def parser-rule
  #"(?x)
  ^
  (<.*?>)
  \s*?
  ::=
  \s*?
  (\S.*?)
  $
  ")

(def term-re #"<.*?>")

(defn tokenize-grammar [grammar] (map (fn [[_ term rules]] { :term term :rules (str/split rules #"\s?\|\s?") }) (match-program parser-rule grammar)))

; (match-program parser-rule grammar)
(tokenize-grammar grammar)

(def-grammar simple-grammar :<bool> [
  "<bool>  ::= <true> | <false>" 
  "<true>  ::= true | 1"
  "<false> ::= false | 0"])

(def simple-tokens (tokenize-grammar simple-grammar))

(defn are-terminal-rules [rules] (empty? (filter #(keyword? %) (flatten [rules]))))

(defn get-elems [tokens] (map #(:term %) tokens))
(defn get-rules [tokens] (map #(:rules %) tokens))

(def term-re #"<[0-9A-Za-z\-]+?>")
(def terminal-token-re #"[\S\"]+")
(def space-re #"\s+")
(def token-re (re-pattern (str/join "|" [term-re terminal-token-re space-re])))
(def match-next-token (partial re-find token-re))
(defn split-rules [rules] (str/split rules #"\s?\|\s?"))
(defn tokenize-rule [rule]
  (let [t (map
    #(if (re-find #"<[0-9A-Za-z\-]+?>" %) (keyword %) %)
    (filter
      #(not (re-find #"\s+" %))
      (re-seq token-re rule)))] ;(prn-debug t) (prn-debug (count t)) (prn-debug (first t))
  ; (if (= 1 (count t)) (first t) t)))
  t))
(defn tokenize-rules [rules] ;(prn-debug rules)
  (let [
    split (split-rules rules)
    cnt (count split)
    tokens (map tokenize-rule split)
    single-tokens (empty? (filter #(and (not (keyword? %)) (not (are-terminal-rules %))) split))
    flatten-token (if (= 1 (count tokens)) (first tokens) tokens)] (prn-debug "single-tokens") (prn-debug single-tokens) (prn-debug "flatten-token") (prn-debug flatten-token)
    {:tokens flatten-token :count cnt :single-tokens single-tokens}))



(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (cart (rest colls))]
      (cons x more))))

(defn compile-rule [terminal-rule] (flatten (map (fn [rule] (if (= 1 (count (map count rule))) (flatten rule) (map #(str/join " " %) (map (fn [x] (flatten x)) (cart rule))))) terminal-rule)))
(defn compile-rule2 [terminal-rule] (prn-debug "terminal-rule - compile rule") (prn-debug terminal-rule)
  (let [
    test-rule (flatten [terminal-rule])
    count-rule (count test-rule)]
    (if (or (= test-rule terminal-rule) (= 1 count-rule))
      terminal-rule
      (map #(str/join %) (map flatten (cart terminal-rule))))))

(defn compile-rule3 [terminal-rule cnt] (prn-debug "== TERMINAL-RULE== ")(prn-debug terminal-rule) (prn-debug cnt)
  (let [
    test-rule (flatten [terminal-rule])
    count-rule (count test-rule)]
    (if (or (= test-rule terminal-rule) (= 1 count-rule))
      terminal-rule
      (if (= 1 cnt)
        (flatten (compile-rule2 terminal-rule))
        (flatten (map compile-rule2 terminal-rule))))))

(defn tokenize-grammar-alt [grammar]
  (let [
    lookup-list (map
      (fn [[_ term rules]] (let [
        rule-obj       (tokenize-rules rules)
        rule-coll      (:tokens rule-obj)
        cnt            (:count rule-obj)
        terminal-rule  (are-terminal-rules rule-coll)
        flatten-rule   (if (:single-tokens (meta rule-coll)) (flatten rule-coll) rule-coll)
        final-rule     flatten-rule] (prn-debug "rule-coll") (prn-debug rule-coll) (prn-debug "terminal-rule") (prn-debug terminal-rule) (prn-debug "final-rule") (prn-debug final-rule)
        (list (keyword term) {:rules final-rule :terminality terminal-rule :compiled terminal-rule :count cnt })))
      (match-program parser-rule grammar))] ;(prn-debug lookup-list)
    (zipmap (map first lookup-list) (map second lookup-list))))

(def lookup (tokenize-grammar-alt simple-grammar))
(def terminality (check-rule-terminality simple-tokens))

(defn find-next-expand [grammar] (first (filter (fn [x] (not (get-in grammar [x :terminality]))) (keys grammar))))
; (defn expand-term [grammar next-expand] (vec (flatten (map #(:rules (% grammar)) (:rules (next-expand grammar))))))
(defn expand-term [grammar next-expand] (prn-debug (next-expand grammar))
  (let [
    term (next-expand grammar)
    rules (:rules term)
    cnt (:count term)
    side-effect (prn-debug str "SIDE EFFECT" rules)
    side-effect-3 (prn-debug str "SIDE EFFECT3" cnt)
    expanded-terms (map
      (defn real-expand-term [rule] (prn-debug "rule") (prn-debug rule)
        (if (keyword? rule)
          (:rules (rule grammar))
          (if (are-terminal-rules [rule]) rule
            (map #(real-expand-term %) rule))))
          ; (prn-debug rule))))
    rules)
    side-effet-2 (prn-debug (flatten [expanded-terms]))
    is-flat? (empty? (filter #(and (not (keyword? %)) (not (are-terminal-rules [%]))) rules))] (prn-debug "is-flat") (prn-debug rules) (prn-debug is-flat?) (prn-debug is-flat?)
    (if (and is-flat? (not (= 1 cnt))) (flatten [expanded-terms]) expanded-terms)))


; Simple grammar steps
(def next-expand (find-next-expand lookup))
(def expanded-elem (expand-elem lookup next-expand))
(def first-iteration-grammar (iterate-grammar lookup expanded-elem next-expand))
(def compiled-simple-grammar (:rules ((first (first first-iteration-grammar)) first-iteration-grammar)))
(check-grammar compiled-simple-grammar true) ; => valid input
(check-grammar compiled-simple-grammar 0)    ; => valid input
(check-grammar compiled-simple-grammar "2")  ; => invalid input


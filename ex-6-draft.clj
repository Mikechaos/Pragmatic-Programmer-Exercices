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

(def term-pattern #"<.*?>")

(defn tokenize-grammar [grammar] (map (fn [[_ term rules]] { :term term :rules (str/split rules #"\s?\|\s?") }) (match-program parser-rule grammar)))

; (match-program parser-rule grammar)
(tokenize-grammar grammar)

(def-grammar simple-grammar :<bool> [
  "<bool>  ::= <true> | <false>" 
  "<true>  ::= true | 1"
  "<false> ::= false | 0"])

(def simple-tokens (tokenize-grammar simple-grammar))

(defn is-terminal-rule [rules]
  (loop [[rule & r] rules acc true ]
    (if (or (nil? acc) (nil? rule)) acc
      (if (or (nil? rule) (not (nil? (re-find term-pattern rule)))) nil
        (recur r acc)))))

(defn get-elems [tokens] (map #(:elem %) tokens))
(defn get-rules [tokens] (map #(:rules %) tokens))
(defn check-rule-terminality [tokens] (map is-terminal-rule (get-rules tokens)))

(defn tokenize-grammar-alt [grammar] (let [lookup-list (map (fn [[_ elem rules]] (let [rule-coll (str/split rules #"\s?\|\s?")] (list (keyword elem) {:rules rule-coll :terminality (is-terminal-rule rule-coll)}))) (match-program parser-rule grammar))] (zipmap (map first lookup-list) (map second lookup-list))))
(def lookup (tokenize-grammar-alt simple-grammar))
(def terminality (check-rule-terminality simple-tokens))

(defn find-next-expand [grammar] (first (filter (fn [x] (not (get-in grammar [x :terminality]))) (keys grammar))))
(defn expand-elem [grammar next-expand] (vec (flatten (map #(:rules (% grammar)) (map keyword (:rules (next-expand grammar)))))))
(defn iterate-grammar [grammar expanded-elem next-expand] (reduce-kv (fn [m k v] (let [fv (if (= k next-expand) {:rules expanded-elem :terminality (is-terminal-rule expanded-elem)} v)] (assoc m k fv))) {} grammar))
(defn check-grammar [rules input] (not (empty? (filter #(= (read-string %) input) rules))))

; Simple grammar steps
(def next-expand (find-next-expand lookup))
(def expanded-elem (expand-elem lookup next-expand))
(def first-iteration-grammar (iterate-grammar lookup expanded-elem next-expand))
(def compiled-simple-grammar (:rules ((first (first first-iteration-grammar)) first-iteration-grammar)))
(check-grammar compiled-simple-grammar true) ; => valid input
(check-grammar compiled-simple-grammar 0)    ; => valid input
(check-grammar compiled-simple-grammar "2")  ; => invalid input


(require '[clojure.string :as str])

(def possible-formats ["4pm" "7:38pm" "23:42" "3:16" "3:16am"])

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
  [program] (map #(replace % #"\s+" " ") program))

(def grammar (trim-program [
  "<time>                 ::= <12-hour><meridiem-indicator> | <12-hour><separator><minute><meridiem-indicator> | <24-hour><separator><minute> | <12-hour><separator><minute>"
  "<12-hour>              ::= <digit> | \"10\" | \"11\" | \"12\""
  "<24-hour>              ::= <first-digit-hour><digit> | \"2\"<second-digit-hour>"
  "<minute>               ::= <first-digit-minute><second-digit-minute>"
  "<digit>                ::= \"0\" | \"1\" | \"2\" | \"3\" | \"4\" | \"5\" | \"6\" | \"7\" | \"8\" | \"9\""
  "<first-digit-hour>     ::= \"0\" | \"1\""
  "<second-digit-hour>    ::= \"0\" | \"1\" | \"2\" | \"3\" | \"4\""
  "<first-digit-minute>   ::= \"0\" | \"1\" | \"2\" | \"3\" | \"4\" | \"5\""
  "<second-digit-minute>  ::= <digit>"
  "<separator>            ::= \":\""
  "<meridiem-indicator>   ::= \"am\" | \"pm\""]))

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

(def elem-pattern #"<.*?>")

(defn tokenize-grammar [grammar] (map (fn [[_ elem rules]] { :elem elem :rules (str/split rules #"\s?\|\s?") }) (match-program parser-rule grammar)))

; (match-program parser-rule grammar)
(tokenize-grammar grammar)

(def simple-grammar (trim-program [
  "<bool>  ::= <true> | <false>"
  "<true>  ::= true | 1"
  "<false> ::= false | 0"]))

(def simple-tokens (tokenize-grammar simple-grammar))

(defn is-terminal-rule [rules]
  (loop [[rule & r] rules acc true ]
    (if (or (nil? acc) (nil? rule)) acc
      (if (or (nil? rule) (not (nil? (re-find elem-pattern rule)))) nil
        (recur r acc)))))

(defn check-rule-terminality [grammar] (map is-terminal-rule (map #(:rules %) simple-tokens)))

(check-rule-terminality grammar)

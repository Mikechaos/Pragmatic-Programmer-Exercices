(require '[clojure.string :as str])

(def debug-mode? false)

(defn toggle-debug [] (second (list (def debug-mode? (not debug-mode?)) debug-mode?)))
(defn- helper-prn-debug
  ([s] (prn s))
  ([s & r] (prn s) (map prn r)))

(defmacro prn-debug [& args]
  `(if ~debug-mode? (helper-prn-debug ~@args) ~nil))

(defmacro prn-dbg
  "Duplicate all elements in a coll.
  Prints the first one as a symbol and prints the value of the second one
  Example:
  (def a-symbol :a-value) (def another-symbol 123)
  (print-dbg [a-symbol another-symbol]) => [a-symbol :a-value another-symbole 123]"
  [coll] (let [x# (reduce (fn [coll# e#] `(conj ~coll# '~e# ~e#)) [] coll)] `(prn-debug ~x#)))


(def possible-formats ["4pm" "7:38pm" "23:42" "3:16" "03:16" "3:16am" "15h15", "1h24pm"])
(def error-formats ["4ampm" "17:38pm" "23h:42" "3:011" "03:16pm" "15h61", "24h24"])

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
(defn split-rules
  "Split the rule on each |
  Returns a collection of rule"
  [rules] (str/split rules #"\s?\|\s?"))




(defn cart [colls]
  "Calculates the cartesian product of colls"
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (cart (rest colls))]
      (cons x more))))


(defn compile-rule [terminal-rule] (prn-dbg ['compile-rule terminal-rule])
  "Calculate the cartesian product of the rule and join the left groupings in strings
  Example :
  (compile-rule '((1 2) (am pm))) ==> (\"1am\" \"1pm\" \"2am\" \"2pm\")"
  (map #(str/join %) (map flatten (cart terminal-rule))))

(defn is-simple-terminal-rule? [terminal-rule]
  "If the rule if made from one simple grouping or on element
  it is already fully compiled.
  Examples :
  (is-simple-terminal-rule? '(1 2 3 4)) => true
  (is-simple-terminal-rule? '((1 2) (3 4)) => false"
  (let [test-rule (flatten [terminal-rule])
        count-rule (count test-rule)]
    (or (= test-rule terminal-rule) (= 1 count-rule))))

(def apply-appropriate-compilation #(if (is-simple-terminal-rule? %) % (compile-rule %)))
(defn compile-rule-expr [terminal-rule cnt] (prn-dbg [terminal-rule cnt])
  "Compiles a complete rule expr for a given term.
  It can be made of many different alternative rules
  The cnt represent the number of alternative rules.
  It is important to get the groupings interpretation right.
  TODO - This does not manage recursive constructs
  (recursive term or deeply nested rules)"
  (if (is-simple-terminal-rule? terminal-rule)
    terminal-rule
    (if (= 1 cnt)
      (flatten (compile-rule terminal-rule))
      (flatten (map apply-appropriate-compilation terminal-rule)))))

(defn tokenize-rule
  "Extract all tokens from rule, ignore space and
  transform each '<term>' into :<term> (str to keyword)"
  [rule]
  (let [tokenized-rule (map
    #(if (re-find term-re %) (keyword %) %)
    (filter
      #(not (re-find #"\s+" %))
      (re-seq token-re rule)))] (prn-debug [tokenized-rule (count tokenized-rule)])
  tokenized-rule))


(defn is-single-token?
  "Verify if the current token is a combination of many definition or a simple rule
  If it's a list with only one element, it is also extracted"
  [token] (or (keyword? token) (are-terminal-rules token) (empty? (rest token))))

(defn has-only-single-tokens?
  "When we have only single tokens, we can simplify our groupings.
  The groupings are critical when we apply the cartesian product while compiling the rules
  We look if we find only keywords (terms) and terminal rules (terminated expr)
  Returns false if it finds a list of tokens for example"
  [rules] (every? is-single-token? rules))

(defn remove-unecessary-groupings
  "When a rule is a single token, we omit the surrounding grouping
  (:<token>) => :<token>"
  [tokens] (if (= 1 (count tokens)) (first tokens) tokens))

(defn tokenize-rules
  "Splits the rules, iterates over each to tokenize it, gather meta-data
  and format the tokenized rule with proper groupings.
  \"<digit> | <letter> | + | *\" =>
  {:tokens ((:<digit>) (:<letter>) (\"+\") (\"*\")), :cnt 4} with meta {:single-tokens? true}"
  [rules] (prn-debug rules)
  (let [
    split (split-rules rules)
    cnt (count split)
    tokens (map tokenize-rule split)
    single-tokens? (has-only-single-tokens? tokens)
    final-tokens (remove-unecessary-groupings tokens)] (prn-debug ["tokens" tokens "single-tokens" single-tokens? "final-tokens" final-tokens])
    (with-meta {:tokens final-tokens :cnt cnt} {:single-tokens? single-tokens?} )))


(defn tokenize-grammar-alt [grammar]
  "Take a grammar and outputs a map {:<term> {:rules [x y z] :meta-data}}
  The meta-data need to be refactored. It keeps track of a few flag on the given rules"
  (let [
    lookup-list (map
      (fn [[_ term rules]] (let [
        rule-obj       (tokenize-rules rules)
        rule-expr      (:tokens rule-obj)
        cnt            (:cnt rule-obj)
        meta-obj       (meta rule-obj)
        terminal-rule  (are-terminal-rules rule-expr)
        ; flatten-rule   (if (:single-tokens meta-obj) rule-expr rule-expr)
        final-rule     (if terminal-rule (compile-rule-expr rule-expr cnt) rule-expr)] (prn-dbg [rule-expr meta-obj terminal-rule final-rule])
        (list (keyword term) (with-meta {:rules final-rule :terminality terminal-rule :compiled terminal-rule :cnt cnt} meta-obj))))
      (match-program parser-rule grammar))] (prn lookup-list)
    (zipmap (map first lookup-list) (map second lookup-list))))

(defn find-next-expand
  "Find the next rule to be expanded.
  Iterate over the grammar and finds the first terminal rule.
  TODO - Improve the way we discriminate which rule is chosen."
  [grammar] (first (filter (fn [x] (not (get-in grammar [x :terminality]))) (keys grammar))))

(defn expand-term
  "Iterate over a given rule expression and replace other rules occurence with their definition."
  [grammar next-expand] (prn-debug (next-expand grammar))
  (let [
    term (next-expand grammar)
    rules (:rules term)
    cnt (:cnt term)]
    (map
      (defn real-expand-term [rule] (prn-dbg [rule])
        (if (keyword? rule)
          (:rules (rule grammar))
          (if (are-terminal-rules [rule]) rule
            (map #(real-expand-term %) rule))))
      rules)))

(defn replace-term [grammar expanded-term next-expand]
  (reduce-kv
    (fn [m k v]
      (let [
        compiled? (:compiled v)
        terminal? (are-terminal-rules expanded-term)
        needCompile? (and (not compiled?) terminal?)
        cnt (:cnt v)
        final-term (if needCompile? (compile-rule-expr expanded-term cnt) expanded-term)
        compiled (or compiled? needCompile?)
        fv (if (= k next-expand)
          {:rules final-term :terminality terminal? :compiled compiled :cnt cnt }
          v)] ; (prn-debug compiled)
        (assoc m k fv)))
    {} grammar))
(defn is-grammar-terminal [grammar iteration] (true? (:terminality ((get-primary-term grammar) iteration))))

(defn iterate-grammar
  "Iterate a tokenized grammar to produce a new iteration with a new expanded rule."
  [iteration] (let [
    next-expand (find-next-expand iteration)
    expanded-term (expand-term iteration next-expand)] (prn-dbg [next-expand expanded-term])
  (replace-term iteration expanded-term next-expand)))

(defn compile-grammar [grammar iteration ]
  "Iterate a grammar until it is fully terminated."
  (loop [i iteration terminal? (is-grammar-terminal grammar i)]
    (if terminal?
      (:rules ((get-primary-term grammar) i))
      (let [new-i (iterate-grammar i) terminal? (is-grammar-terminal grammar new-i)]
      (recur new-i terminal?)))))

(defn check-grammar
  "Simple util to check if an input match the grammar."
  [rules input] (not (empty? (filter #(= % input) rules))))

; Simple grammar steps
(def lookup (tokenize-grammar-alt simple-grammar))
(def next-expand (find-next-expand lookup))
(def expanded-term (expand-term lookup next-expand))
(def first-iteration-grammar (replace-term lookup expanded-term next-expand))

(def compiled-simple-grammar (:rules ((first (first first-iteration-grammar)) first-iteration-grammar)))

; Complex grammar steps
(def tokenized-grammar (tokenize-grammar-alt grammar))
(def next-expand2 (find-next-expand tokenized-grammar))
(def expanded-elem2 (expand-term tokenized-grammar next-expand2))
(def first-iteration-comp-grammar (replace-term tokenized-grammar expanded-elem2 next-expand2))
(def compiled-grammar (compile-grammar grammar tokenized-grammar))

[(map (fn [frmt] [frmt (check-grammar compiled-grammar frmt)]) possible-formats)
(map (fn [frmt] [frmt (check-grammar compiled-grammar frmt)]) error-formats)]


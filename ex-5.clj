
(def program ["P 2 # select pen 2"
              "D # pen down"
              "W 2 # draw west 2cm"
              "N 1 # then north 1"
              "E 2 # then east 2"
              "S 1 # then back south"
              "U # pen up"])

(def commands
  { :D "Pen Down"
    :E "Draw East %cm"
    :N "Draw North %cm"
    :P "Select Pen %"
    :U "Pen Up"
    :S "Draw South %cm"
    :W "Draw West %cm" })


#_(Complex way of doing exactly (map first program))
(def extractCommand
  (fn [program]
    (loop [[cmd & rst] program acc []]
          (first cmd)
      (if (nil? cmd)
          acc
        (recur rst (concat acc [(first cmd)]))))))


#_(Flow control Macros)
(defmacro test-nil 
  "Simplifies testing if a value is nil
  Usage : (test-nil value body-then body-else)
  Expands: (if (not (nil? value)) body-then body-else)
  Example : (test-nil 1 :good-job :feelsbadman) ==> :good-job"
  [value body-then body-else] `(if (not (nil? ~value)) ~body-then ~body-else))



#_(String manipulations)
(def replace clojure.string/replace)
(defn safe-replace [v p r] (test-nil v (replace v p r) ""))
(defn trim [v] (safe-replace v #" " ""))

#_(Parse the program into tokens)
(defn match-coll [pattern input]
 (map first (map #(re-seq pattern %) input)))

(defn parser [program]
  (map (fn [[_ c m & rst]] {:cmd c :mod (trim m) :comment rst})
   (match-coll #"
      ([A-Z]+)   # Command grouping
      (\s[0-9])? # Modifier grouping
      (\s#.*)?$  # Comment grouping"
    program)))


#_(Token Accessors)
(def get-modifier :mod)

#_(To access the hashmap, we need the :keyword form of the command)
(defn keyword-command [token] (keyword (:cmd token)))
(defn get-command [token] (get commands (keyword-command token)))

#_(In this simple example, applying a command means replacing it's
   potential parameter with the corresponding modifier token)
(defn apply-command [token] (replace (get-command token) #"%" (get-modifier token)))

(def tokens (parser program))
(map #(apply-command %) tokens)
#_(Evaluates to => ("Select Pen 2" "Pen Down" "Draw West 2cm" "Draw North 1cm" "Draw East 2cm" "Draw South 1cm" "Pen Up"))

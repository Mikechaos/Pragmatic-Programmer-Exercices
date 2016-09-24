(def program ["P 2 # select pen 2"
            "D # pen down"
            "W 2 # draw west 2cm"
            "N 1 # then north 1"
            "E 2 # then east 2"
            "S 1 # then back south"
            "U # pen up"])


#_ "Complex way of doing exactly (map first program)"
(def extractCommand
  (fn [program]
    (loop [[cmd & rst] program acc []]
          (first cmd)
      (if (nil? cmd)
          acc
        (recur rst (concat acc [(first cmd)]))))))


#_ "String manipulations"
(def replace clojure.string/replace)
(defn safe-replace [v p r]
  (if (not (nil? v)) (replace v p r) ""))
(defn trim [v]
  (safe-replace v #" " ""))

#_ "Parse the program into tokens"
(defn parser [program]
  (map (fn [[_ c m & rst]] {:cmd c :mod (trim m) :comment rst})
       (map first
            (map #(re-seq #"([A-Z])(\s[0-9])?(\s#.*)?$" %) program))))

(def commands
  { :P "Select Pen %"
    :D "Pen Down"
    :N "Draw North %cm"
    :E "Draw East %cm"
    :S "Draw South %cm"
    :W "Draw West %cm"
    :U "Pen Up" })


(def tokens (parser program))
(map #(replace (get commands (keyword (:cmd %))) #"%" (:mod %)) tokens)

#_(Evaluates to => ("Select Pen 2" "Pen Down" "Draw West 2cm" "Draw North 1cm" "Draw East 2cm" "Draw South 1cm" "Pen Up"))

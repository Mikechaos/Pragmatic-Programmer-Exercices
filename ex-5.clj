; Implement a simple DSL to draw a rectangle
; Takes a program as input
; Parses it
; Executes corresponding commands

(def program [  "P 2 # select pen 2"
                "D # pen down"
                "W 2 # draw west 2cm"
                "N 1 # then north 1"
                "E 2 # then east 2"
                "S 1 # then back south"
                "U # pen up"])

(def commands { :D "Pen Down"
                :E "Draw East %cm"
                :N "Draw North %cm"
                :P "Select Pen %"
                :U "Pen Up"
                :S "Draw South %cm"
                :W "Draw West %cm" })

(def parse-rules
  #"(?x)    # Begin Regexp - (?x) allows comments
  ^         # Beginning of sequence
  ([A-Z]+)  # Command grouping
  \s?       # Command terminator
  ([0-9]+)? # Modifier grouping
  \s?       # Modifier terminator
  (\#.*)?   # Comment grouping
  $         # End of sequence")

(defn match-program
  "Extract matches from each program line
  Will match [full-command command modifier? comments?]"
  [pattern program] (map #(re-find pattern %) program))

(defn execute-command
  "Takes a command and a modifier and outputs the corresponding string"
  ([cmd] (get commands (keyword cmd)))
  ([cmd mod] (replace (get commands (keyword cmd)) #"%" mod)))

(defn apply-command
  "Extract the command and modifier from each match and apply the command"
  [[_ cmd mod & rst]] (execute-command cmd mod))

(defn execute
  "Execute each command in the program"
  [program] (map apply-command (match-program parse-rules program)))

(execute program)
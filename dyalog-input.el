;;; -*- coding: utf-8 -*-

(require 'quail)

(defvar dyalog-input-prefix "`")

(defvar dyalog-input-symbols `(
                               ("`" "⋄")
                               ("¬" "⌺")
                               ("1" "¨")
                               ("!" "⌶")
                               ("2" "¯")
                               ("\"" "⍫")
                               ("3" "<")
                               ("£" "⍒")
                               ("4" "≤")
                               ("$" "⍋")
                               ("5" "=")
                               ("%" "⌽")
                               ("6" "≥")
                               ("^" "⍉")
                               ("7" ">")
                               ("&" "⊖")
                               ("8" "≠")
                               ("*" "⍟")
                               ("9" "∨")
                               ("(" "⍱")
                               ("0" "∧")
                               (")" "⍲")
                               ("-" "×")
                               ("_" "!")
                               ("=" "÷")
                               ("+" "⌹")
                               ("q" "?")
                               ("Q" "⍰")
                               ("w" "⍵")
                               ("W" "⍵")
                               ("e" "∊")
                               ("E" "⍷")
                               ("r" "⍴")
                               ("R" "⌾")
                               ("t" "~")
                               ("T" "⍨")
                               ("y" "↑")
                               ("Y" "↑")
                               ("u" "↓")
                               ("U" "↓")
                               ("i" "⍳")
                               ("I" "⍸")
                               ("o" "○")
                               ("O" "⍥")
                               ("p" "*")
                               ("[" "←")
                               ("{" "⍞")
                               ("]" "→")
                               ("}" "⍬")
                               ("a" "⍺")
                               ("A" "⍺")
                               ("s" "⌈")
                               ("S" "⌈")
                               ("d" "⌊")
                               ("D" "⌊")
                               ("f" "_")
                               ("F" "_")
                               ("g" "∇")
                               ("G" "⍢")
                               ("h" "∆")
                               ("H" "∆")
                               ("j" "∘")
                               ("J" "⍤")
                               ("k" "'")
                               ("K" "⌸")
                               ("l" "⎕")
                               ("L" "⌷")
                               (";" "⍎")
                               (":" "≡")
                               ("'" "⍕")
                               ("@" "≢")
                               ("#" "⊢")
                               ("~" "⊣")
                               ("\\" "⊢")
                               ("|" "⊣")
                               ("z" "⊂")
                               ("Z" "⊆")
                               ("x" "⊃")
                               ("X" "⊃")
                               ("c" "∩")
                               ("C" "∩")
                               ("v" "∪")
                               ("V" "∪")
                               ("b" "⊥")
                               ("B" "⍭")
                               ("n" "⊤")
                               ("N" "⍡")
                               ("m" "|")
                               ("M" "∥")
                               ("," "⍝")
                               ("<" "⍪")
                               ("." "⍀")
                               (">" "⍙")
                               ("/" "⌿")
                               ("?" "⍠")
                               (" " ,dyalog-input-prefix)))

(quail-define-package "Dyalog" "UTF-8" "⍞" t
                      "Input mode for Dyalog APL"
                      `((,(kbd "C-i") . quail-completion))
                      t                 ; forget-last-selection
                      nil               ; deterministic
                      nil               ; kbd-translate
                      t                 ; show-layout
                      nil               ; create-decode-map
                      nil               ; maximum-shortest
                      nil               ; overlay-plist
                      nil               ; update-translation-function
                      nil               ; conversion-keys
                      t                 ; simple
                      )

(quail-select-package "Dyalog")

(eval `(quail-define-rules ,@(loop for e in dyalog-input-symbols
                                   collect (list (concat dyalog-input-prefix (car e)) (cdr e)))))

(provide 'dyalog-input)

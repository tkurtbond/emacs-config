;; Prompted by gemini://idiomdrottning.org/d12
;; See https://en.wikipedia.org/wiki/Demographics_of_sexual_orientation

(defun tkb-npc-random-sexuality (prefix)
  "Insert a string specifing a random sexuality for an NPC, or return it if a
prefix was specified.  Sexuality consists of sex (biological characteristics),
gender (social, psychological, cultrual, and behaviorable aspects), and sexual orientation "
  (interactive "P")
  (let* ((sex-roll (d100))
         (sex 
          (cond ((<= sex-roll 2) ?⚥)   ; Hermaphroditic 1.7% ⇒ 2%: 1–2
                ((<= sex-roll 51) ?♂)  ; Female 49%: 3–51
                (else ?♀)))            ; Male 52–100
         (gender-roll (d100))
         (gender
          (cond ((<= gender-roll xxx) ?⚧)  ; Transgender
                ((<= gender-roll xxx) ?🜬)  ; Non-binary
                (else xxx)))                ; cisgender
         (orientation-roll (d100))
         (orientation
          (cond ((<= orientation-roll xxx) asexual)
                ((<= orientation
                     ))

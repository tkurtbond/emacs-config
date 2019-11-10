(require 'cl)
(require 's)


(defun pulpadv (count)
  (interactive "nHow many? ")

  (let ((buf (get-buffer-create "pulpadv.md")))
    (with-current-buffer buf
      (erase-buffer)
      (switch-to-buffer buf)
      (cl-loop for i from 1 to count do (pulpadv-generate-story pulpadv-table))
      (goto-char (point-min))
      (set-buffer-modified-p nil))))


(defun pulpadv-generate-story (table)
  (insert "# Story")
  (insert (pulpadv-setup table))
  (insert (pulpadv-act-1 table))
  (insert (pulpadv-act-2 table))
  (insert (pulpadv-act-3 table))
  (insert "\n## Act 4: Climax\n\n"))


(defun pulpadv-setup (table)
  (let ((villian (pulpadv-lookup-roll-on 'villian table))
	(plot-p1 (pulpadv-lookup-roll-on 'fiendish-plot-p1 table))
        (plot-p2 (pulpadv-lookup-roll-on 'fiendish-plot-p2 table))
	(location (pulpadv-lookup-roll-on 'main-location table)))
    (concat
     "\n## Setup:\n\n"
     "* *Villian:*       " (s-join ", " villian) "\n"
     "* *Fiendish Plot:* " (s-join ", " plot-p1) " " (s-join ", " plot-p2) "\n"
     "* *Location:*      " (s-join ", " location) "\n")))


(defun pulpadv-act-1 (table)
  (let ((hook (pulpadv-lookup-roll-on 'hook table))
        (characters (pulpadv-characters table))
        (action (pulpadv-action-sequence table))
        (twist (pulpadv-plot-twist table)))
    (concat 
     "\n## Act 1\n\n"
     "* The Hook:\n\t* " (s-join "\n\t* " hook) "\n"
     "* Supporting Characters:\n\t* " (s-join "\n\t* " characters) "\n"
     "* Action Sequence:\n" (s-join "\n" action) "\n"
     "* Plot Twist:\n\t* " (s-join "\n\t* " twist) "\n")))


(defun pulpadv-act-2 (table)
  (let ((action (pulpadv-action-sequence table))
        (twist (pulpadv-plot-twist table)))
    (concat 
     "\n## Act 2\n\n"
     "* Action Sequence:\n" (s-join "\n" action) "\n"
     "* Plot Twist:\n\t* " (s-join "\n\t* " twist) "\n")))


(defun pulpadv-act-3 (table)
  (let ((action (pulpadv-action-sequence table))
        (twist (pulpadv-plot-twist table)))
    (concat
     "\n## Act 3\n\n"
     "* Action Sequence:\n" (s-join "\n" action) "\n"
     "* Plot Twist:\n\t* " (s-join "\n\t* " twist) "\n")))


(defun pulpadv-roll (count d)
    (cl-loop repeat count sum (+ 1 (random d))))


(defun pulpadv-action-sequence (table)
  (let ((rtn)
        (complications (pulpadv-lookup-roll-on 'action-sequence-complications table)))
    (push (concat "\t* *Setting:*       " (s-join " " (pulpadv-lookup-roll-on 'action-sequence-setting table))) rtn)
    (push (concat "\t* *Participants:*  " (s-join " " (pulpadv-lookup-roll-on 'action-sequence-participants table))) rtn)
    (push (concat "\t* *Type:*          " (s-join " " (pulpadv-lookup-roll-on 'action-sequence-type table))) rtn)

    (while (eq (car complications) 'new-sequence)
      (push "* New Sequence:" rtn)
      (push (concat "\t* *Setting:*       " (s-join " " (pulpadv-lookup-roll-on 'action-sequence-setting table))) rtn)
      (push (concat "\t* *Participants:*  " (s-join " " (pulpadv-lookup-roll-on 'action-sequence-participants table))) rtn)
      (push (concat "\t* *Type:*          " (s-join " " (pulpadv-lookup-roll-on 'action-sequence-type table))) rtn)

      (setq complications (pulpadv-lookup-roll-on 'action-sequence-complications table)))

    (push (concat   "\t* *Complications:* " (s-join " " complications)) rtn)
    (nreverse rtn)))


(defun pulpadv-plot-twist (table)
  (let ((twist (pulpadv-lookup-roll-on 'plot-twist table)))
    (if (eq (car twist) 'new-location)
        (list (concat "*New Location:* " (s-join ", " (pulpadv-lookup-roll-on 'main-location table))))
      twist)))


(defun pulpadv-characters (table)
  (cl-loop repeat (pulpadv-roll 2 4)
           collect (s-join " "
                           (list
                            (s-join " " (pulpadv-lookup-roll-on 'supporting-characters-d1 table))
                            (s-join " " (pulpadv-lookup-roll-on 'supporting-characters-d2 table))
                            (s-join " " (pulpadv-lookup-roll-on 'supporting-characters-type table))))))


(defun pulpadv-lookup (what table)
  (let ((v (assoc what table)))
    (if v (symbol-value (cdr v))
      nil)))


(defun pulpadv-select-random (table)
  (let* ((limit (cl-loop for i in table sum (car i)))
         (n (random limit))
         (sum 0))
    (cl-loop for i in table
             do (setq sum (+ sum (car i)))
             if (<= n sum) return (cdr i))))

(defun pulpadv-roll-on (table)
  (let ((n 1)
        (v)
        (rtn))
    (while (> n 0)
      (setq v (pulpadv-select-random table))
      (if (eq v 'roll-twice)
          (setq n (1+ n))
        (setq n (1- n))
        (push v rtn)))
    rtn))


(defun pulpadv-lookup-roll-on (what table)
  (pulpadv-roll-on (pulpadv-lookup what pulpadv-table)))



(defconst pulpadv-villian
  '((3 . "Alien Invader")
    (4 . "Anarchist")
    (4 . "Assassin")
    (4 . "Business Magnate")
    (4 . "Communist")
    (4 . "Crime Lord")
    (4 . "Crooked Cop")
    (4 . "Crooked Politician")
    (4 . "Cult Leader")
    (4 . "Dictator")
    (4 . "Femme Fatale")
    (4 . "Gangster")
    (4 . "Mad Scientist")
    (5 . "Mastermind")
    (4 . "Murderer")
    (4 . "Nazi")
    (4 . "Nemesis")
    (5 . "Occultist")
    (4 . "Pirate")
    (4 . "Ruler of Lost Civilization")
    (4 . "Society Swell")
    (4 . "Supernatural Threat")
    (4 . "Thief")
    (4 . "Wicked Foreigner")
    (3 . roll-twice)))

(defconst pulpadv-fiendish-plot-p1
  '((4 . "Acquire")
    (4 . "Attack")
    (4 . "Blackmail")
    (4 . "Bomb")
    (4 . "Control")
    (4 . "Create")
    (5 . "Destroy")
    (4 . "Extort")
    (4 . "Hijack")
    (4 . "Hunt")
    (4 . "Infiltrate")
    (4 . "Kill")
    (4 . "Manipulate")
    (4 . "Murder")
    (4 . "Obliterate")
    (4 . "Overthrow")
    (4 . "Ransom")
    (4 . "Rob")
    (4 . "Rule")
    (5 . "Sell")
    (4 . "Smuggle")
    (4 . "Steal")
    (3 . "Take")
    (4 . "Terrorize")
    (3 . roll-twice)))

(defconst pulpadv-fiendish-plot-p2
  '((4 . "A Business")
    (4 . "A City")
    (4 . "A Country")
    (4 . "A Lost World")
    (4 . "A Man")
    (4 . "A Ruler")
    (4 . "A Vehicle")
    (4 . "A Woman")
    (4 . "A rival")
    (4 . "An Enemy")
    (4 . "An Invention")
    (4 . "An Object")
    (5 . "Building")
    (4 . "Heroʼs Friends or Family")
    (5 . "Innocent Victims")
    (4 . "Jewels")
    (4 . "Money")
    (4 . "Monster")
    (4 . "People")
    (4 . "Someone famous")
    (4 . "The Hero (or team)")
    (3 . "The Law")
    (4 . "The World")
    (4 . "Treasure")
    (3 . roll-twice)))

(defconst pulpadv-main-location
  '((4 . "Another City")
    (4 . "Arctic/Antarctic")
    (4 . "Asian Country")
    (4 . "At Sea")
    (4 . "City: Chinatown")
    (4 . "City: Entertainment District")
    (4 . "City: Government")
    (3 . "City: Museums")
    (5 . "City: Skyscrapers")
    (4 . "City: Tenderloin District")
    (4 . "City: The Docks")
    (4 . "City: University")
    (4 . "City: Warehouses")
    (4 . "City: slums")
    (4 . "Cross-country (train, etc)")
    (4 . "Desert")
    (4 . "European Country")
    (5 . "Far-off Jungle")
    (4 . "Farmland")
    (4 . "Forest")
    (4 . "In the Air")
    (4 . "Lost City")
    (4 . "Secret Base")
    (4 . "Third-World Country")
    (3 . roll-twice)))

(defconst pulpadv-hook
  '((12 . "Attack")
    (14 . "Bizarre Occurance")
    (9 . "Dead Body")
    (10 . "Disaster")
    (12 . "Friend in Need")
    (9 . "News")
    (4 . "Other")
    (11 . "Solicitation")
    (13 . "Up To Our Necks")
    (6 . roll-twice)))

(defconst pulpadv-supporting-characters-d1
  '((3 . "Agile")
    (5 . "Beautiful")
    (4 . "Big")
    (4 . "Charming")
    (4 . "Clumsy")
    (4 . "Cold")
    (4 . "Dangerous")
    (4 . "Dense")
    (4 . "Famous")
    (4 . "Helpless")
    (4 . "Impulsive")
    (4 . "Lucky")
    (4 . "Quirky")
    (4 . "Secretive")
    (4 . "Shifty")
    (4 . "Sloppy")
    (4 . "Small")
    (4 . "Smart")
    (4 . "Strong")
    (4 . "Strong-willed")
    (3 . "Talented")
    (4 . "Troublesome")
    (4 . "Trustworthy")
    (5 . "Ugly")
    (4 . "Weak")))

(defconst pulpadv-supporting-characters-d2
  '((4 . "All-American")
    (4 . "Amateur")
    (4 . "Ambitious")
    (4 . "Distinctive")
    (5 . "Evil")
    (4 . "Fiesty")
    (4 . "Foreign")
    (4 . "Hard-boiled")
    (4 . "Helpful")
    (4 . "Menacing")
    (3 . "Native")
    (4 . "Neat")
    (4 . "Odd")
    (4 . "Old")
    (3 . "Ordinary")
    (4 . "Professional")
    (4 . "Rich")
    (4 . "Skilled")
    (4 . "Small-time")
    (5 . "Unlucky")
    (4 . "Urban")
    (4 . "Violent")
    (4 . "Weak-willed")
    (4 . "Wild")
    (4 . "Young")))

(defconst pulpadv-supporting-characters-type
  '((4 . "Academician")
    (4 . "Assistant")
    (4 . "Business Owner")
    (4 . "Contact")
    (4 . "Criminal")
    (4 . "Doctor")
    (4 . "Entertainer")
    (4 . "Expert")
    (4 . "Fanatic")
    (4 . "Guide")
    (5 . "Henchman")
    (4 . "Informant")
    (4 . "Investigator")
    (3 . "Kid")
    (4 . "Occultist")
    (5 . "Pilot")
    (4 . "Politician")
    (4 . "Scientist")
    (4 . "Servant")
    (4 . "Socialite")
    (4 . "Soldier")
    (4 . "Spouse")
    (4 . "Thug")
    (4 . "Vehicle Operator")
    (3 . "Worker")))

(defconst pulpadv-action-sequence-type
  '((6 . "Chase, vehicle")
    (6 . "Fight, Armed")
    (4 . "Fight, unarmed")
    (4 . "Chase, foot or mount")))

(defconst pulpadv-action-sequence-participants
  '((6 . "Few (1-2 per PC)")
    (5 . "Lots (5+ per PC)")
    (9 . "Some (3-4 per PC)")))

(defconst pulpadv-action-sequence-setting
  '((1 . "Business Setting (office, factory, warehouse, street market, etc.)")
    (1 . "Church/temple/other religious")
    (1 . "City Street")
    (1 . "Civic setting (post office, city hall)")
    (1 . "Educational (museum, college, etc.)")
    (1 . "Entertainment Setting (theatre, stadium, nightclub, etc.)")
    (1 . "Headquarters (PCs or others)")
    (1 . "Laboratory")
    (1 . "Landmark")
    (1 . "Military setting (base, etc.)")
    (1 . "Natural Setting (park , jungle, etc.)")
    (1 . "Nautical setting (ship, docks, etc.)")
    (1 . "Residential Setting")
    (1 . "Restaurant")
    (1 . "Rooftops")
    (1 . "Secret/hidden location")
    (1 . "Slum or Rough neighborhood")
    (1 . "Transportational Setting (airport, train station, or even on trains or plains)")
    (1 . "Unusual setting (underwater, in space, underground, etc.)")
    (1 . "“Middle of Nowhere”")))

(defconst pulpadv-action-sequence-complications
  '((5 . "Bystanders")
    (5 . "Environment")
    (5 . new-sequence) ;; TODO: ???
    (5 . "Props")))

(defconst pulpadv-plot-twist
  '((2 . "Betrayal!")
    (3 . "Bizarre Occurance")
    (1 . "Deus Ex Machina")
    (1 . "Greater Villain")
    (3 . "Hidden Plot")
    (7 . new-location)
    (1 . "Other")
    (2 . "Reversal!")))


(defconst pulpadv-table
  '((villian                       . pulpadv-villian)
    (fiendish-plot-p1              . pulpadv-fiendish-plot-p1)
    (fiendish-plot-p2              . pulpadv-fiendish-plot-p2)
    (main-location                 . pulpadv-main-location)
    (hook                          . pulpadv-hook)
    (supporting-characters-d1      . pulpadv-supporting-characters-d1)
    (supporting-characters-d2      . pulpadv-supporting-characters-d2)
    (supporting-characters-type    . pulpadv-supporting-characters-type)
    (action-sequence-type          . pulpadv-action-sequence-type)
    (action-sequence-participants  . pulpadv-action-sequence-participants)
    (action-sequence-setting       . pulpadv-action-sequence-setting)
    (action-sequence-complications . pulpadv-action-sequence-complications)
    (plot-twist                    . pulpadv-plot-twist)))

(setq tkb-wl-file-loaded t)

;; autoload configuration
;; (Not required if you have installed Wanderlust as XEmacs package)
(when nil 
  (progn
    (message "Before checking for LLWV1-TASC62907")
    (cond ((string-equal (system-name) "LLWV1-TASC62907")
	   (message "Setting LLWV1-TASC62907 wanderlust values")
	   (setq wl-smtp-posting-port 6005)
	   (message "wl-smtp-posting-port: %d" wl-smtp-posting-port)
	   (setq wl-bcc "Kurt_Bond@mpl.com"))))

  (autoload 'wl "wl" "Wanderlust" t)
  (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
  (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
  (autoload 'footnote-mode "footnote" nil t)
  (add-hook 'wl-draft-mode-hook 'footnote-mode)

  (autoload 'wl-user-agent-compose "wl-draft" nil t)
  )
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

(setq wl-alias-file "~/Mail/aliases")

(when nil
  (apply #'encode-time
	 (parse-time-string (elmo-message-entity-field entity 'date t))))

(when nil
  ;; This seems too weird to use.
  (defadvice wl-refile-get-field-value (around tkb-wl-refile-get-field-value-ad activate)
    "uh"
    (if (eq (intern (downcase field)) 'folder)
	(setq ad-return-value
	      (elmo-folder-name-internal wl-summary-buffer-elmo-folder))
      ad-do-it))

  (setq x '(("X" ("x" . "y")) ("z" ("1" . "2"))))
  (push '("Folder"
	  ("%INBOX" . "%out-of-this-world"))
	wl-refile-rule-alist))

(defun tkb-wl-refile-guess-by-rule-for-imap (entity)
  (when (string= "%INBOX"
		 (elmo-folder-name-internal wl-summary-buffer-elmo-folder))
    (let ((rules tkb-wl-refile-imap-inbox-rule-alist)
	  guess)
      (while rules
	(if (setq guess (wl-refile-evaluate-rule (car rules) entity))
	    (setq rules nil)
	  (setq rules (cdr rules))))
      guess)))

(setq tkb-wl-refile-imap-inbox-rule-alist
      '((("From" "To")
	 ("phil_loftis" . "%Users\\\\Loftis, Phil")
	 ("christy_benson" . "%Users\\\\Benson, Christy")
	 ("crystal_jones" . "%Users\\\\Jones, Crystal")
	 ("Ian Sneddon" . "%Users\\\\Sneddon, Ian")
	 ("christy_benson" . "%Users\\\\Benson, Christy")
	 ("hayes@mplvax.mpl.com" . "%Users\\\\Theiling, Hayes")
	 ("hayes_theiling@mail.mpl.com" . "%Users\\\\Theiling, Hayes")
	 ("herb@mpl.com" . "%Users\\\\Smith, Herb")
	 ("herb_smith@mail.mpl.com" . "%Users\\\\Smith, Herb")
	 ("herb_smith@mpl.com" . "%Users\\\\Smith, Herb")
	 ("herb_smith@wvlink.com" . "%Users\\\\Smith, Herb")
	 ("linda_wellings@mail.mpl.com" . "%Users\\\\Wellings, Linda")
	 ("linda_wellings@wvlink.com" . "%Users\\\\Wellings, Linda")
	 ("phil_loftis@mail.com" . "%Users\\\\Loftis, Phil")
	 ("phil_loftis@mail.mpl.com" . "%Users\\\\Loftis, Phil")
	 ("phil_loftis@wvlink.com" . "%Users\\\\Loftis, Phil")
	 ("rigo.ramirez-contractor@pwc.ca" . "%Users\\\\Ramirez, Rigo")
	 ))) 

(defun tkb-wl-is-mpl-mail (entity)
  (let ((summary-buffer-name (wl-summary-buffer-folder-name)))
    (and (not (empty-string-p summary-buffer-name))
	 (= ?% (aref summary-buffer-name 0)))))

(defvar tkb-mail-filing-by-year-flag-day (encode-time 0 0 0 1 1 2010 "EST")
  "Day TKB switched from filing MH mail by year and month to just year.")

(defun tkb-wl-refile-guess-by-rule (entity)
  (message "wl-summary-buffer-name: %s" (wl-summary-buffer-folder-name))
  (message "elmo-folder-type: %s" (elmo-folder-type entity))
  (let ((rules wl-refile-rule-alist)
	guess)
    (while rules
      (if (setq guess (wl-refile-evaluate-rule (car rules) entity))
	  (setq rules nil)
	(setq rules (cdr rules))))
    (when (and guess (/= (aref guess 0) ?%))
      (let ((date (elmo-message-entity-field entity 'date t)))
	(if (time-less-p date tkb-mail-filing-by-year-flag-day)
	    (setq guess (concat "+"
				(format-time-string "y%Y/m%m/" date) guess))
	  (setq guess (concat "+"
			      (format-time-string "y%Y/" date) guess)))))
    guess))

(eval-after-load "wl-action"
  '(progn
     (add-to-list 'wl-refile-guess-functions #'tkb-wl-refile-guess-by-rule)
     (add-to-list 'wl-refile-guess-functions
		  #'tkb-wl-refile-guess-by-rule-for-imap)))

(eval-after-load ""
  '(progn
     (add-to-list 'wl-auto-refile-guess-functions #'tkb-wl-refile-guess-by-rule)))
  

(setq elmo-msgdb-extra-fields
      '(
	;;"Cc"
	"Delivered-To"
	"Errors-To"
	;;"From"
	"List-Id"
	"List-Owner"
	"Mailing-List"
	"Message-Id"
	"Reply-To"
	"Resent-From"
	"Resent-Sender"
	"Resent-To"
	"Return-Path"
	"Sender"
	;;"Subject"
	;;"To"
	"X-BeenThere"
	"X-Digest"
	"X-Digest-Reply-To"
	"X-Groupname"
	"X-List-Check"
	"X-List-Id"
	"X-Mailing-List"
	"X-ML-Name"
	))

;; I should sort this by destination.
(setq wl-refile-rule-alist
      '(
	("X-ML-Name"
	 ("Wanderlust English" . "lists/wanderlust-english"))
	("X-BeenThere"
	 ("columbia-games-info@columbiagames.com" . "lists/columbia-games-info"))
	("X-Digest"
	 ("RQ-Rules Digest" . "lists/rq-rules")
	 ("TAOGM-L Digest" . "lists/taogm-l")
	 ("The DSSSList Digest" . "lists/dssslist")
	 ("The Glorantha Digest" . "lists/glorantha")
	 ("Traveller-digest" . "lists/traveller")
	 ("Warhammer FRP Mailing List" . "lists/wfrp")
	 ("plt-scheme Digest" . "lists/plt-scheme")
	 ("wfrp-digest" . "lists/wfrp"))
	("X-Digest-Reply-To"
	 ("ARIA-L@BROWNVM.BROWN.EDU" . "lists/aria-l")
	 ("HARN-L@MITVMA.MIT.EDU" . "lists/harn-l")
	 ("eternal-l@lists.io.com" . "lists/eternal-l")
	 ("glorantha@rpglist.org" . "lists/glorantha")
	 ("gmast-L@phoenyx.net" . "lists/gmast-l")
	 ("gmast@phoenyx.net" . "lists/gmast-l")
	 ("liad@io.com" . "lists/liad")
	 ("seabooks-l@majordomo.pobox.com" . "lists/seabooks")
	 ("traveller@MPGN.COM" . "lists/traveller")
	 ("wfrp@terrania.westfalen.de" . "lists/wfrp"))
	("X-Groupname"
	 ("fudge" . "lists/fudge-l"))
	("X-List-Check"
	 ("m3devel-pint-elegosoft-com-approved" . "lists/m3devel"))
	("X-List-Id"
	 ("fudge" . "lists/fudge-l")
	 ("taorp" . "lists/taorp"))
	("X-Mailing-List"
	 ("<ded@endsofinvention.com>" . "lists/ded")
	 ("<gnatcom-list@adapower.com>" . "lists/gwindows")
	 ("coda-announce@coda.cs.cmu.edu" . "lists/coda-announce")
	 ("codalist@coda.cs.cmu.edu" . "lists/codalist")
	 ("debian-devel-announce@lists.debian.org" . "lists/debian-devel-announce")
	 ("debian-news@lists.debian.org" . "lists/debian-news")
	 ("hw-rules@egroups.com" . "lists/hw-rules")
	 ("lesstif@Hungry.COM" . "lists/lesstif")
	 ("lout@ptc.spbu.ru" . "lists/lout")
	 ("pgsql-announce" . "lists/psgql-announce"))
	("Cc"
	 ("caml-list@inria.fr" . "lists/caml-list")
	 ("caml-list@pauillac.inria.fr" . "lists/caml-list")
	 ;;("cpaulbond@gmail.com" . "users/bond_cp")
	 ("gd-hackers@gwydiondylan.org" . "lists/gd-hackers")
	 ("groff@ffii.org" . "lists/groff")
	 ("lablgtk@kaba.or.jp" . "lists/lablgtk")
	 ("lablgtk@math.nagoya-u.ac.jp" . "lists/lablgtk")
	 ("prcs-list@xcf.berkeley.edu" . "lists/prcs-list"))
	("Delivered-To"
	 ("mailing list HeroQuest-rules@yahoogroups.com" . "lists/HeroQuest-rules"))
	("Errors-To"
	 ("retroforth-bounce@freelists.org" . "lists/retroforth"))
	
	("List-Id"
	 ("<silverbranchgames.yahoogroups.com>" . "lists/silverbranchgames")
	 ("<oberon.lists.inf.ethz.ch>" . "lists/oberon")
	 ;;("<schematics-users.lists.sourceforge.net>" . "lists/schematics-users")
	 ("<barbariansoflemuria.yahoogroups.com>" . "lists/barbariansoflemuria")
	 ("<cult-of-ore.googlegroups.com>" . "lists/cult-of-ore")
	 ("<fuzionforum.yahoogroups.com>" . "lists/fuzionforum")
	 ("<family.tkb.mpl.com>" . "lists/family")
	 ("fudgecommunity.yahoogroups.com" . "lists/fudgecommunity")
	 ("coblist.deatech.com" . "lists/coblist")
	 ("<action-l_goldrushgames.com.goldrushgames.com>" . "lists/action-l")
	 ("<alerts.us-cert.gov>" . "lists/us-cert-alerts")
	 ("<announcements.lists.squeakfoundation.org>" . "lists/squeak-announcements")
	 ("<asmp-zoz.yahoogroups.com>" . "lists/asmp-zoz")
	 ("<brpsystem.yahoogroups.com>" . "lists/brpsystem")
	 ("<caml-list.yquem.inria.fr>" . "lists/caml-list")
	 ("<cdr-announce.common-lisp.net>" . "lists/cdr-announce")
	 ("<cffi-devel.common-lisp.net>" . "lists/cffi-devel")
	 ("<clisp-list.lists.sourceforge.net>" . "lists/clisp-list")
	 ("<cminusminus.cminusminus.org>" . "lists/cminusminus")
	 ("<darcs-users.darcs.net>" . "lists/darcs-users")
	 ("<docutils-users.lists.sourceforge.net>" . "lists/docutils-users")
	 ("<ecls-list.lists.sourceforge.net>" . "lists/ecls-list")
	 ("<factor-talk.lists.sourceforge.net>" . "lists/factor-talk")
	 ("<freebsd-ports.freebsd.org>" . "lists/freebsd-ports")
	 ("<freebsd-security-notifications.freebsd.org>" . "lists/freebsd-security")
	 ("<gc-announce.linux.hpl.hp.com>" . "lists/gc-announce")
	 ("<gc.linux.hpl.hp.com>" . "lists/gc")
	 ("<gentoo-announce.gentoo.org>" . "lists/gentoo-announce")
	 ("<gnuwin32-users.lists.sourceforge.net>" . "lists/gnuwin32-users")
	 ("<gps-devel.lists.act-europe.fr>" . "lists/gps-devel")
	 ("<gps-users.lists.act-europe.fr>" . "lists/gps-users")
	 ("<gtkada.lists.adacore.com>" . "lists/gtkada")
	 ("<liadenuniversenews.fireopal.org>" . "lists/liadenuniversenews")
	 ("<local-gamers.tkb.mpl.com>" . "lists/local-gamers")
	 ("<nobilis-hitherby.com.lists.hitherby.com>" . "lists/nobilis")
	 ("<nonpareil.lists.planix.com>" . "lists/nonpareil")
	 ("<prcs-list.lists.xcf.berkeley.edu>" . "lists/prcs-list")
	 ("<rq-rules.crashbox.com>" . "lists/rq-rules")
	 ("<runtime-optimization.buytaert.net>" . "lists/runtime-optimization")
	 ("<ssax-sxml.lists.sourceforge.net>" . "lists/ssax-sxml")
	 ("<technical-alerts.us-cert.gov>" . "lists/us-cert-technical-alerts")
	 ("<tentaclesnews-int.darcsyde.org>" . "lists/tentaclesnews-int")
	 ("<tns.sjgames.com>" . "lists/traveller-news-service")
	 ("<ua.lists.unknown-armies.com>" . "lists/ua")
	 ("<ubuntu-news.lists.ubuntu.com>" . "lists/ubuntu-news")
	 ("<ubuntu-security-announce.lists.ubuntu.com>" . "lists/ubuntu-security-announce")
	 ("Caml users' mailing list" . "lists/caml-list")
	 ("Liaden Universe News <liadenuniversenews.fireopal.org>" . "lists/liadenuniversenews")
	 ("OpenJade hackers list <openjade-devel.lists.sourceforge.net>" . "lists/openjade-devel")
	 ("OpenJade support list <openjade-users.lists.sourceforge.net>" . "lists/openjade-users")
	 ("PLT Scheme discussion list" . "lists/plt-scheme")
	 ("Porting FreeBSD to the Alpha <freebsd-alpha.freebsd.org>" . "lists/freebsd-alpha")
	 ("Tabletop roleplaying magazine <twilighttime.burningvoid.com>" . "lists/twilighttime")
	 ("Tentacles Newsletter" . "lists/tenacles")
	 ("The Nice programming language mainling-list <nice-info.lists.sourceforge.net>" . "lists/nice-info")
	 ("announce.lists.adacore.com" . "lists/adacore-announce")
	 ("aws.lists.act-europe.fr" . "lists/aws")
	 ("blue-planet-list.yahoogroups.com" . "lists/blue-planet-list")
	 ("chaos-digest.chaosium.com" . "lists/chaos-digest")
	 ("chaos-digest_lists.chaosium.com.lists.chaosium.com" . "lists/chaos-digest")
	 ("chaos-info.chaosium.com" . "lists/chaos-info")
	 ("chaos-info_lists.chaosium.com.lists.chaosium.com" . "lists/chaos-info")
	 ("chicken-users.nongnu.org" . "lists/chicken-users")
	 ("clisp-announce.lists.sourceforge.net" . "lists/clisp-announce")
	 ("devel-announce-list.gnome.org" . "lists/gnome-devel-announce")
	 ("dglist.yahoogroups.com" . "lists/dglist")
	 ("emacs-nxml-mode.yahoogroups.com" . "lists/emacs-nxml-mode")
	 ("eternal-champion_lists.chaosium.com.lists.chaosium.com" . "lists/eternal-champion")
	 ("family.erekose.mpl.com" . "lists/family-alt")
	 ("family.tkb.mpl.com" . "lists/family")
	 ("farm.tkb.mpl.com" . "lists/farm")
	 ("fireflygames.yahoogroups.com" . "lists/fireflygames")
	 ("freebsd-announce.freebsd.org" . "lists/freebsd-announce")
	 ("gd-hackers.gwydiondylan.org" . "lists/gd-hackers")
	 ("gimpwin-users.yahoogroups.com" . "lists/gimpwin-users")
	 ("gnavi-discuss.lists.sourceforge.net" . "lists/gnavi-discuss")
	 ("googoogaga.lists.csail.mit.edu" . "lists/googoogaga")
	 ("groff.gnu.org" . "lists/groff")
	 ("gtk-app-devel-list.gnome.org" . "lists/gtk-app-devel-list")
	 ("gtk-devel-list.gnome.org" . "lists/gtk-devel-list")
	 ("gtk-list.gnome.org" . "lists/gtk-list")
	 ("gtkada.lists.act-europe.fr" . "lists/gtkada")
	 ("hackers.lists.opendylan.org" . "lists/gd-hackers")
	 ("ilisp.cons.org" . "lists/ilisp")
	 ("illuminator.sjgames.com" . "lists/illuminator")
	 ("lablgtk.yquem.inria.fr" . "lists/lablgtk")
	 ("lispweb.red-bean.com" . "lists/lispweb")
	 ("lout-users.lists.planix.com" . "lists/lout")
	 ("mailman-announce.python.org" . "lists/mailman-announce")
	 ("movitz-announce.common-lisp.net" . "lists/movitz-announce")
	 ("plt-announce.list.cs.brown.edu" . "lists/plt-announce")
	 ("small-cl-src.hexapodia.net" . "lists/small-cl-src")
	 ("tsoludhaliyal.yahoogroups.com" . "lists/tsoludhaliyal")
	 ("wfrp.tng.employees.org" . "lists/wfrp")
	 ("wfrp.www.employees.org" . "lists/wfrp")
	 ("wvhtcf-gamers.tkb.mpl.com" . "lists/wvhtcf-gamers"))
	("List-Owner"
	 ("<mailto:announce-owner@fudgefactor.org>" . "lists/fudgefactor-announce"))
	("Mailing-List"
	 ("list SPITBOL@yahoogroups.com" . "lists/spitbol")
	 ("list crngames@googlegroups.com" . "lists/crngames")
	 ("list fudgecommunity@yahoogroups.com" . "lists/fudgecommunity")
	 ("Kregen@egroups.com" . "lists/kregen")
	 ("MiracleGroup123@egroups.com" . "lists/MiracleGroup123")
	 ("Pulp_Games@egroups.com" . "lists/Pulp_Games")
	 ("Pulp_Games@yahoogroups.com" . "lists/Pulp_Games")
	 ("cminusminus-help@cminusminus.org" . "lists/cminusminus")
	 ("contact cygwin-announce-help@cygwin.com" . "lists/cygwin-announce")
	 ("contact dssslist-help@lists.mulberrytech.com" . "lists/dsssllist")
	 ("cygwin-help@sourceware.cygnus.com" . "lists/cygwin")
	 ("cygwin-xfree-announce-help@cygwin.com" . "lists/cygwin-xfree-announce")
	 ("d20ec@yahoogroups.com" . "lists/d20ec")
	 ("dyingearth@yahoogroups.com" . "lists/dyingearth")
	 ("hw-rules@egroups.com" . "lists/hw-rules")
	 ("jorune@egroups.com" . "lists/jorune")
	 ("list CGnD@yahoogroups.com" . "lists/cumberland-games")
	 ("list EABA@yahoogroups.com" . "lists/EABA")
	 ("list EternalChampion@yahoogroups.com" . "lists/EternalChampion")
	 ("list Everway-L@yahoogroups.com" . "lists/Everway-L")
	 ("list ExMachinav2@yahoogroups.com" . "lists/ExMachinav2")
	 ("list FateRPG@yahoogroups.com" . "lists/FateRPG")
	 ("list Fvlminata@yahoogroups.com" . "lists/Fvlminata")
	 ("list GURPS-Firefly@yahoogroups.com" . "lists/gurps-firefly")
	 ("list HeroQuest-RPG@yahoogroups.com" . "lists/HeroQuest-RPG")
	 ("list HeroQuest-rules@yahoogroups.com" . "lists/HeroQuest-rules")
	 ("list HeroWars@yahoogroups.com" . "lists/HeroWars")
	 ("list Hubris-Games@yahoogroups.com" . "lists/Hubris-Games")
	 ("list KingOfDragonPass@yahoogroups.com" . "lists/KingOfDragonPass")
	 ("list King_Arthur_Pendragon@yahoogroups.com" . "lists/King_Arthur_Pendragon")
	 ("list Masterbook-Shatterzone@yahoogroups.com" . "lists/Masterbook-Shatterzone")
	 ("list MetabaronsRPG@yahoogroups.com" . "lists/metabaronsrpg")
	 ("list Mythic_Russia@yahoogroups.com" . "lists/Mythic_Russia")
	 ("list Savage_Worlds@yahoogroups.com" . "lists/Savage_Worlds")
	 ("list Tekumel-Noncanon@yahoogroups.com" . "lists/Tekumel-Noncanon")
	 ("list Tradetalk-info@yahoogroups.com" . "lists/Tradetalk-info")
	 ("list Unspoken-Word-News@yahoogroups.com" . "lists/Unspoken-Word_news")
	 ("list WorldofGlorantha@yahoogroups.com" . "lists/WorldOfGlorantha")
	 ("list besm-d6@yahoogroups.com" . "lists/besm-d6")
	 ("list car-pga@yahoogroups.com" . "lists/car-pga")
	 ("list corps-players@egroups.com" . "lists/corps-players")
	 ("list dead-inside@yahoogroups.com" . "lists/dead-inside")
	 ("list derpg-announce@yahoogroups.com" . "lists/derpg-announce")
	 ("list dq-rules@yahoogroups.com" . "lists/dq-rules")
	 ("list dqnewsletter@yahoogroups.com" . "lists/dragonquest-news")
	 ("list encounter-critical@yahoogroups.com" . "lists/encounter-critical")
	 ("list eternalchampion@yahoogroups.com" . "lists/eternalchampion")
	 ("list fireflyrpg@yahoogroups.com" . "lists/fireflyrpg")
	 ("list flashing_blades@yahoogroups.com" . "lists/flashing_blades")
	 ("list goo-info@yahoogroups.com" . "lists/goo-info")
	 ("list gurps-browncoats@yahoogroups.com" . "lists/gurps-browncoats")
	 ("list historicalrpgs@yahoogroups.com" . "lists/historicalrpgs")
	 ("list hw-rules@yahoogroups.com" . "lists/hw-rules")
	 ("list jorune@yahoogroups.com" . "lists/jorune")
	 ("list langsmiths@yahoogroups.com" . "lists/langsmiths")
	 ("list liadenuniversenews@yahoogroups.com" . "lists/liadenuniversenews")
	 ("list metazine-announce@yahoogroups.com" . "lists/metazine")
	 ("list omnisys@yahoogroups.com" . "lists/omnisys")
	 ("list pcgen@yahoogroups.com" . "lists/pcgen")
	 ("list rq3@yahoogroups.com" . "lists/rq3")
	 ("list tekdevel@yahoogroups.com" . "lists/tekdevel")
	 ("list tekumel-moderated@yahoogroups.com" . "lists/tekumel-moderated")
	 ("list tekumelnovels@yahoogroups.com" . "lists/tekumelnovels")
	 ("list tekumelrpg@yahoogroups.com" . "lists/tekumelrpg")
	 ("list thedorispiserchiafanpage@yahoogroups.com" . "lists/thedorispiserchiafanpage")
	 ("list theeternalchampion@yahoogroups.com" . "lists/theeternalchampion")
	 ("list thieves-world@yahoogroups.com" . "lists/thieves-world")
	 ("list timpowers@yahoogroups.com" . "lists/timpowers")
	 ("list traveller-news-service@yahoogroups.com" . "lists/traveller-news-service")
	 ("list tri-statdX@yahoogroups.com" . "lists/tristat-dX")
	 ("list tristat@yahoogroups.com" . "lists/tristat")
	 ("list truthandjustice@yahoogroups.com" . "lists/truthandjustice")
	 ("list unison-announce@yahoogroups.com" . "lists/unison-announce")
	 ("list unison-users@yahoogroups.com" . "lists/unison-users")
	 ("list uresia@yahoogroups" . "lists/uresia")
	 ("list victoriangamers@yahoogroups.com" . "lists/victoriangamers")
	 ("list who-rpg@yahoogroups.com" . "lists/who-rpg")
	 ("list wmargin@yahoogroups.com" . "lists/wmargin")
	 ("list zeppelinage@yahoogroups.com" . "lists/zeppelinage")
	 ("steampunk@egroups.com" . "lists/steampunk")
	 ("tekumel@yahoogroups.com" . "lists/tekumel")
	 ("wmargin@egroups.com" . "lists/wmargin"))
	("Reply-To"
	 ("ARIA-L@LISTSERV.BROWN.EDU" . "lists/aria-l")
	 ("KingOfDragonPass@egroups.com" . "lists/KingOfDragonPass")
	 ("KingOfDragonPass@yahoogroups.com" . "lists/KingOfDragonPass")
	 ("Lace_And_Steel@egroups.com" . "lists/Lace_And_Steel")
	 ("MiracleGroup123@yahoogroups.com" . "lists/MiracleGroup123")
	 ("RAGNAROK@list.to" . "lists/ragnarok")
	 ("S1889@egroups.com" . "lists/s1889")
	 ("S1889@yahoogroups.com" . "lists/s1889")
	 ("dqn@ntsource.com" . "lists/dragonquest-news")
	 ("gd-hackers@gwydiondylan.org" . "lists/gd-hackers")
	 ("gnatlist@lyris.seas.gwu.edu" . "lists/gnatlist")
	 ("gnavi-list@gnavi.org" . "lists/gnavi-list")
	 ("heliogram@shore.net" . "lists/heliogram")
	 ("hw-rules@eGroups.com" . "lists/hw-rules")
	 ("jade-devel@infomansol.com" . "lists/jade-devel")
	 ("jorune@listbot.com" . "lists/jorune-listbot")
	 ("pendragon@chaosium.com" . "lists/pendragon")
	 ("squeak@cs.uiuc.edu" . "lists/squeak")
	 ("steampunk@yahoogroups.com" . "lists/steampunk")
	 ("tekumel@egroups.com" . "lists/tekumel")
	 ("virtmach@iecc.com" . "lists/virtmach")
	 ("wfrp@employees.org" . "lists/wfrp"))
	("Resent-From"
	 ("lout@ptc.spbu.ru" . "lists/lout")
	 ("pam-list@redhat.com" . "lists/pam-list")
	 ("squeak@cs.uiuc.edu" . "lists/squeak")
	 ("stk@kaolin.unice.fr" . "lists/stk"))
	("Resent-Sender"
	 ("debian-announce-request@lists.debian.org" . "lists/debian-announce")
	 ("lout-request@ptc.spbu.ru" . "lists/lout")
	 ("squeak-request@cs.uiuc.edu" . "lists/squeak"))
	("Resent-To"
	 ("lablgtk@math.nagoya-u.ac.jp" . "lists/lablgtk"))
	("Return-Path"
	 ("m3commit" . "lists/m3commit"))
	("Sender"
	 ("1PG@yahoogroups.com" . "lists/1PG")
	 ("ARIA-L@BROWNVM.BROWN.EDU" . "lists/aria-l")
	 ("GNATLIST@HERMES.GWU.EDU" . "lists/gnatlist")
	 ("GNATLIST@hermes.gwu.edu" . "lists/gnatlist")
	 ("RolePlayingTipsWeekly@lists.webvalence.com" . "lists/roleplaying_tips_weekly")
	 ("clisp-announce-admin@lists.sourceforge.net" . "lists/clisp-announce")
	 ("clisp-list@clisp.cons.org" . "lists/clisp-list")
	 ("clisp-list@seagull.cons.org" . "lists/clisp-list")
	 ("cygwin-announce-owner@sourceware.cygnus.com" . "lists/cygwin-announce")
	 ("deaGauss@Korval.com" . "lists/liad")
	 ("deagauss@korval.com" . "lists/liad")
	 ("gcc-announce-owner@gcc.gnu.org" . "lists/gcc-announce")
	 ("gmast-error@phoenyx.net" . "lists/gmast")
	 ("groff-admin@ffii.org" . "lists/groff")
	 ("icon-group-request@CS.Arizona.EDU" . "lists/icon-group")
	 ("kids-rpg@yahoogroups.com" . "lists/kids-rpg")
	 ("local-gamers-admin@tkb.mpl.com" . "lists/local-gamers")
	 ("netbsd-announce-owner@NetBSD.org" . "lists/netbsd-announce")
	 ("owner-caml-announce@pauillac.inria.fr" . "lists/caml-announce")
	 ("owner-chaos-digest@chaosium.com" . "lists/chaos-digest")
	 ("owner-chaos-info@chaosium.com" . "lists/chaos-info")
	 ("owner-dssslist@lists.mulberrytech.com" . "lists/dssslist")
	 ("owner-dssslist@mulberrytech.com" . "lists/dssslist")
	 ("owner-egcs-announce" . "lists/egcs-announce")
	 ("owner-eternal-champion@chaosium.com" . "lists/eternal-champion")
	 ("owner-eternal-l@eternal-l@lists.io.com" . "lists/eternal-l")
	 ("owner-falken-l@duke.poly.edu" . "lists/falken-l")
	 ("owner-gclist@iecc.com" . "lists/gclist")
	 ("owner-gclist@lists.iecc.com" . "lists/gclist")
	 ("owner-googoogaga@ai.mit.edu" . "lists/googoogaga")
	 ("owner-guile@cygnus.com" . "lists/guile")
	 ("owner-ilisp@.*cons.org" . "lists/ilisp")
	 ("owner-nobilis@nocturne.org" . "lists/nobilis")
	 ("owner-pendragon@chaosium.com" . "lists/pendragon")
	 ("owner-roleplaying-L@phoenyx.net" . "lists/roleplaying-l")
	 ("owner-runequest-rules@lists.ient.com" . "lists/runequest-rules")
	 ("owner-sp-prog@cygnus.uwa.edu.au" . "lists/sp-prog")
	 ("owner-virtmach@iecc.com" . "lists/virtmach")
	 ("owner-wfrp@employees.org" . "lists/wfrp")
	 ("port-alpha-owner@NetBSD.org" . "lists/netbsd-alpha")
	 ("ragnarok@fontcraft.com" . "lists/ragnarok")
	 ("wotgnews@yahoogroups.com" . "lists/wotgnews"))
	("Subject"
	 ("Anti-SPAM report" . "admin/logs")
	 ("MX mail queue" . "admin/logs")
	 ("\\[oroborus\\]" . "lists/oroborus")
	 ("\\[ragnarok\\]" . "lists/ragnarok")
	 ("\\[reign\\]" . "lists/reign")
	 ("\\[wvhtcf-gamers\\]" . "lists/wvhtcf-gamers"))
	
	("From"
	 ("@onebookshelf.com" . "utilities/rpgnow-plus")
	 ("@tripleacegames.com" . "utilities/tripleacegames.com")
	 ("service@paypal.com" . "utilities/paypal")
	 ("@rpgoverstock.com" . "utilities/rpgoverstock")
	 ("@rpgshop.com" . "utilties/rpgshop")
	 ("@funagain.com" . "utilities/funagaingames")
	 ("webmaster@rpgshop.com" . "utilities/gamersattic")
	 ("verizonbillpay@verizon.com" . "utilities/verizon")
	 ("payinsurance@amica.com" . "utilities/amica")
	 ("hayes@mplvax.mpl.com" . "users/theiling_hayes")
	 ("3048719600@messaging.sprintpcs.com" . "users/bond_deborah")
	 ("@thedebttrap.com" . "users/pbs")
	 ("BEEF Cow-Calf Weekly <beef-mag@pbinews.com>" . "lists/beef-cow-calf-weekly")
	 ("beefmagazine@pbinews.com" . "lists/beef-cow-calf-weekly")
	 ("C.PAUL.BOND@cpmx.mail.saic.com" . "users/bond_cp")
	 ("Charlie Root" . "admin/logs")
	 ("Cron Daemon" . "admin/logs")
	 ("Emergencyemail.ORG" . "lists/emergencyemail.org")
	 ("Eternal Tanelorn" . "lists/tanelorn")
	 ("Hayes_Theiling@mail.mpl.com" . "users/theiling_hayes")
	 ("James W. Atha II" . "users/atha_james")
	 ("Kartoon@aol.com" . "users/smith_ray")
	 ("Keith_Foster@wvlink.com" . "users/foster_keith")
	 ("Linda_Wellings@Mail.MPL.com" . "users/wellings_linda")
	 ("Linda_Wellings@wvlink.com" . "users/wellings_linda")
	 ("Netflix <info@netflix.com>" . "services/netflix")
	 ("Netflix Receiving" . "services/netflix")
	 ("Netflix Shipping" . "services/netflix")
	 ("PaulandSally@thedebttrap.com" . "users/pbs")
	 ("Tape Backup account <backup@mplvax.mpl.com>" . "admin/mplvax")
	 ("The Mighty Studio Foglio Mailing List" . "lists/studio-foglio")
	 ("\"RPGNow Newsletter\" <no-reply@onebookshelf.com>" . "lists/rpgnow-newsletter")
	 ("abond@uwsp.edu" . "users/bond_alan")
	 ("alanbond@sbcglobal.net" . "users/bond_alan")
	 ("bbond@inforesrch.com" . "users/bond_ben")
	 ("ben17380@gmail.com" . "users/bond_ben")
	 ("blueroom@mail.prin.edu" . "lists/blueroom")
	 ("bond_d@wvwc.edu" . "users/bond_deborah")
	 ("bondp@galaxy.mantech-wva.com" . "users/bond_cp")
	 ("bondp@mantech-wva.com" . "users/bond_cp")
	 ("c.paul.bond@cpmx.mail.saic.com" . "users/bond_cp")
	 ("c.paul.bond@cpmx.saic.com" . "users/bond_cp")
	 ("cbond@adelphia.net" . "users/bond_connie")
	 ("christy_benson" . "users/benson_christy")
	 ("christyennelilyanne@gmail.com" . "users/bond_lily")
	 ("clisp-list@lists.sourceforge.net" . "lists/clisp-list")
	 ("connieb124@gmail.com" . "users/bond_connie")
	 ("cpaulbond@gmail.com" . "users/bond_cp")
	 ("cpb@access.mountain.net" . "users/bond_cp")
	 ("daedalusx304@gmail.com" . "users/bond_ben")
	 ("dan620@gmail.com" . "users/bond_dan")
	 ("dbond@hsc.wvu.edu" . "users/bond_deborah")
	 ("debbie_bond@mail.mpl.com" . "users/bond_deborah")
	 ("debbie_bond@wvlink.com" . "users/bond_deborah")
	 ("deborahlgbond@msn.com" . "users/bond_deborah")
	 ("dgettier@access.mountain.net" . "users/gettier_dave")
	 ("ebond@access.k12.wv.us" . "users/bond_esther")
	 ("embond@citynet.net" . "users/bond_esther")
	 ("estherbond@hughes.net" . "users/bond_esther")
	 ("gd-hackers@gwydiondylan.org" . "lists/gd-hackers")
	 ("gettier@aol.com" . "users/gettier_dave")
	 ("help@access.mountain.net" . "users/mountain-net-help")
	 ("herb@mpl.com" . "users/smith_herb")
	 ("herb_smith@mail.mpl.com" . "users/smith_herb")
	 ("herb_smith@mpl.com" . "users/smith_herb")
	 ("herb_smith@wvlink.com" . "users/smith_herb")
	 ("hhhhiii@hotmail.com" . "users/hill_howard")
	 ("hhill@asset.com" . "users/hill_howard")
	 ("hhill@inforesrch.com" . "users/hill_howard")
	 ("hhill@ircwv.com" . "users/hill_howard")
	 ("hhowardhhill@gmail.com" . "users/hill_howard")
	 ("jwatha2@citynet.net" . "users/atha_james")
	 ("kartoon@aol.com" . "users/smith_ray")
	 ("kbezellabond@sbcglobal.net" . "users/karen_bazella-bond")
	 ("kwitt@isr.us" . "users/witt_ken")
	 ("kwitt@softwareresearch.org" . "users/witt_ken")
	 ("lablgtk@kaba.or.jp" . "lists/lablgtk")
	 ("mafh@comcast.net" . "users/harrison_mary_ann")
	 ("matt@steeds.com" . "users/elliott_matt")
	 ("mattel@greenbelt.com" . "users/elliott_matt")
	 ("mharrison@wvhtf.org" . "users/harrison_mary_ann")
	 ("nbond@sprynet.com" . "users/bond_alan")
	 ("paulandsally@thedebttrap.com" . "users/pbs")
	 ("pbond@citynet.net" . "users/bond_cp")
	 ("pbond@commerce-tech.com" . "users/bond_cp")
	 ("pbond@green.commerce-tech.com" . "users/bond_cp")
	 ("pbond@inforesrch.com" . "users/bond_cp")
	 ("pbond@ircwv.com" . "users/bond_cp")
	 ("pfritsch@wvhtf.org" . "users/fritsch_peter")
	 ("phil_loftis@mail.com" . "users/loftis_phil")
	 ("phil_loftis@mail.mpl.com" . "users/loftis_phil")
	 ("phil_loftis@wvlink.com" . "users/loftis_phil")
	 ("polings@verizon.net" . "users/poling_diana")
	 ("posting-system@google.com" . "net/postings")
	 ("pws@cc.gatech.edu" . "lists/pws")
	 ("rfwilson@adelphia.net" . "users/wilson_bob")
	 ("rigo.ramirez-contractor@pwc.ca" . "users/ramirez_rigo")
	 ("squeak@cs.uiuc.edu" . "lists/squeak")
	 ("stbond@citynet.net" . "users/bond_st")
	 ("stombond@hughes.net" . "users/bond_st")
	 ("system@host.CI.BUCKHANNON.WV.US" . "admin/cobk")
	 ("vicky@steeds.com" . "users/staubly_vicky")
	 ("wilson@iolinc.net" . "users/wilson_larry")
	 ("witt@access.mountain.net" . "users/witt_ken")
	 ("zanzair@access.mountain.net" . "users/zanzair")
	 ("zanzair@gatewaycomics.com" . "users/zanzair")
	 ("zanzair@mountain.net" . "users/zanzair")
	 ("zanzair@wvdsl.net" . "users/zanzair"))
	("To"
	 ("1PG@yahoogroups.com" . "lists/1PG")
	 ("3048719600@messaging.sprintpcs.com" . "users/bond_deborah")
	 ("9877083@wvpaging.com" . "users/bond_deborah")
	 ("<customerservice@studiofoglio.com>" . "lists/studio-foglio")
	 ("<farm@tkb.mpl.com>" . "lists/farm")
	 ("@thedebttrap.com" . "users/pbs")
	 ("ARIA-L@LISTSERV.BROWN.EDU" . "lists/aria-l")
	 ("C.PAUL.BOND@cpmx.mail.saic.com" . "users/bond_cp")
	 ("Debbie_Bond@mail.mpl.com" . "users/bond_debbie")
	 ("HARN-L-request@MITVMA.MIT.EDU" . "lists/harn-l.admin")
	 ("James W. Atha II" . "users/atha_james")
	 ("K12-LEWIS-CLOSINGS@LISTSERV.WVNET.EDU" . "lists/k12-lewis-closings")
	 ("K12-MONONGALIA-CLOSINGS@LISTSERV.WVNET.EDU" . "lists/k12-monongalia-closing")
	 ("K12-UPSHUR-CLOSINGS@LISTSERV.WVNET.EDU" . "lists/k12-upshur-closing")
	 ("Kartoon@aol.com" . "users/smith_ray")
	 ("KingOfDragonPass@yahoogroups.com" . "lists/KingOfDragonPass")
	 ("Lace_And_Steel@yahoogroups.com" . "lists/Lace_And_Steel")
	 ("Linda_Wellings@wvlink.com" . "users/wellings_linda")
	 ("MiracleGroup123@yahoogroups.com" . "lists/MiracleGroup123")
	 ("MountainNet_Customers@access.mountain.net" . "lists/MountainNet_Customers")
	 ("PaulandSally@thedebttrap.com" . "users/pbs")
	 ("Pulp_Games@yahoogroups.com" . "lists/Pulp_Games")
	 ("Savage_Worlds@yahoogroups.com" . "lists/Savage_Worlds")
	 ("X3J13@ai.sri.com" . "lists/x3j13")
	 ("abond@uwsp.edu" . "users/bond_alan")
	 ("alanbond@sbcglobal.net" . "users/bond_alan")
	 ("bbond@inforesrch.com" . "users/bond_ben")
	 ("ben17380@gmail.com" . "users/bond_ben")
	 ("blostcreek@aol.com" . "users/blostcreek")
	 ("bond_d@wvwc.edu" . "users/bond_deborah")
	 ("bondp@galaxy.mantech-wva.com" . "users/bond_cp")
	 ("bondp@mantech-wva.com" . "users/bond_cp")
	 ("c.paul.bond@cpmx.mail.saic.com" . "users/bond_cp")
	 ("c.paul.bond@cpmx.saic.com" . "users/bond_cp")
	 ("caml-announce@inria.fr" . "lists/caml-announce")
	 ("caml-announce@pauillac.inria.fr" . "lists/caml-announce")
	 ("caml-list@inria.fr" . "lists/caml-list")
	 ("caml-list@pauillac.inria.fr" . "lists/caml-list")
	 ("cbond@adelphia.net" . "users/bond_connie")
	 ("charli@mplvax.mpl.com" . "users/wellings_linda")
	 ("christy_benson" . "users/benson_christy")
	 ("christyennelilyanne@gmail.com" . "users/bond_lily")
	 ("clisp-list@lists.sourceforge.net" . "lists/clisp-list")
	 ("connieb124@gmail.com" . "users/bond_connie")
	 ("corps-players@yahoogroups.com" . "lists/corps-players")
	 ("cpaulbond@gmail.com" . "users/bond_cp")
	 ("cpb@access.mountain.net" . "users/bond_cp")
	 ("cygwin-announce-owner@sourceware.cygnus.com" . "lists/cygwin-announce")
	 ("daedalusx304@gmail.com" . "users/bond_ben")
	 ("dan620@gmail.com" . "users/bond_dan")
	 ("dbond@hsc.wvu.edu" . "users/bond_deborah")
	 ("debbe_bond@mail.mpl.com" . "users/bond_deborah")
	 ("debbie_bond@mail.mpl.com" . "users/bond_deborah")
	 ("debbie_bond@wvlink.com" . "users/bond_deborah")
	 ("deborahbond@wvdhhr.org" . "users/bond_deborah")
	 ("deborahlgbond@msn.com" . "users/bond_deborah")
	 ("dgettier@access.mountain.net" . "users/gettier_dave")
	 ("ebond@access.k12.wv.us" . "users/bond_esther")
	 ("embond@citynet.net" . "users/bond_esther")
	 ("estherbond@hughes.net" . "users/bond_esther")
	 ("eternal-champion@chaosium.com" . "lists/eternal-champion")
	 ("freebsd-ports@freebsd.org" . "lists/freebsd-ports")
	 ("fudge@phoenyx.net" . "lists/fudge-l")
	 ("gd-hackers@gwydiondylan.org" . "lists/gd-hackers")
	 ("gettier@aol.com" . "users/gettier_dave")
	 ("gmast@phoenyx.net" . "lists/gmast")
	 ("gnu-win32-announce@cygnus.com" . "lists/gnu-win32-announce")
	 ("googoogaga@ai.mit.edu" . "lists/googoogaga")
	 ("gtkada@lists.act-europe.fr" . "lists/gtkada")
	 ("guile@cygnus.com" . "lists/guile")
	 ("heliogram@shore.net" . "lists/heliogram")
	 ("help@access.mountain.net" . "users/mountain-net-help")
	 ("herb@mpl.com" . "users/smith_herb")
	 ("herb_smith@mail.mpl.com" . "users/smith_herb")
	 ("herb_smith@mpl.com" . "users/smith_herb")
	 ("herb_smith@wvlink.com" . "users/smith_herb")
	 ("hhhhiii@hotmail.com" . "users/hill_howard")
	 ("hhill@asset.com" . "users/hill_howard")
	 ("hhill@inforesrch.com" . "users/hill_howard")
	 ("hhill@ircwv.com" . "users/hill_howard")
	 ("hhill@mail.inforesrch.com" . "users/hill_howard")
	 ("hhowardhhill@gmail.com" . "users/hill_howard")
	 ("hw-rules@eGroups.com" . "lists/hw-rules")
	 ("hw-rules@egroups.com" . "lists/hw-rules")
	 ("icon-group@optima.CS.Arizona.EDU" . "lists/icon-group")
	 ("jhatcher@wvhtf.org" . "users/hatcher_joseph")
	 ("jwatha2@citynet.net" . "users/atha_james")
	 ("cx142940@citynet.net" . "users/atha_james")
	 ("kartoon@aol.com" . "users/smith_ray")
	 ("kbezellabond@sbcglobal.net" . "users/karen_bazella-bond")
	 ("kids-rpg@yahoogroups.com" . "lists/kids-rpg")
	 ("kwitt@softwareresearch.org" . "users/witt_ken")
	 ("lablgtk@kaba.or.jp" . "lists/lablgtk")
	 ("lablgtk@math.nagoya-u.ac.jp" . "lists/lablgtk")
	 ("linda_wellings@mail.mpl.com" . "users/wellings_linda")
	 ("linda_wellings@wvlink.com" . "users/wellings_linda")
	 ("local-gamers@tkb.mpl.com" . "lists/local-gamers")
	 ("m3devel@elegosoft.com" . "lists/m3devel")
	 ("mafh@comcast.net" . "users/harrison_mary_ann")
	 ("mailman-announce@python.org" . "lists/mailman-announce")
	 ("matt@steeds.com" . "users/elliott_matt")
	 ("mattel@greenbelt.com" . "users/elliott_matt")
	 ("mharrison@wvhtf.org" . "users/harrison_mary_ann")
	 ("nbond@sprynet.com" . "users/bond_alan")
	 ("paulandsally@thedebttrap.com" . "users/pbs")
	 ("pbond@citynet.net" . "users/bond_cp")
	 ("pbond@commerce-tech.com" . "users/bond_cp")
	 ("pbond@green.commerce-tech.com" . "users/bond_cp")
	 ("pbond@inforesrch.com" . "users/bond_cp")
	 ("pbond@ircwv.com" . "users/bond_cp")
	 ("pcal-interest@lists.sourceforge.net" . "lists/pcal-interest")
	 ("pendragon@chaosium.com" . "lists/pendragon")
	 ("pfritsch@wvhtf.org" . "users/fritsch_peter")
	 ("phil_loftis@mail.mpl.com" . "users/loftis_phil")
	 ("phil_loftis@mpl.com" . "users/loftis_phil")
	 ("phil_loftis@wvlink.com" . "users/loftis_phil")
	 ("prcs-list@xcf.berkeley.edu" . "lists/prcs-list")
	 ("pws@cc.gatech.edu" . "lists/pws")
	 ("ragnarok@fontcraft.com" . "lists/ragnarok")
	 ("rfwilson@adelphia.net" . "users/wilson_bob")
	 ("rigo.ramirez-contractor@pwc.ca" . "users/ramirez_rigo")
	 ("runequest-rules@lists.ient.com" . "lists/runequest-rules")
	 ("savannah@studiofoglio.com" . "lists/foglio-labs")
	 ("seabooks-l@admin.listbox.com" . "lists/seabooks-l")
	 ("stbond@citynet.net" . "users/bond_st")
	 ("steampunk@yahoogroups.com" . "lists/steampunk")
	 ("stombond@hughes.net" . "users/bond_st")
	 ("tekumel@yahoogroups.com" . "lists/tekumel")
	 ("traveller-news-service@egroups.com" . "lists/traveller-news-service")
	 ("traveller@pyramid.sjgames.com" . "lists/traveller-news-service")
	 ("ubuntu-announce@lists.ubuntu.com" . "lists/ubuntu-announce")
	 ("vicky@steeds.com" . "users/staubly_vicky")
	 ("virtmach@iecc.com" . "lists/virtmach")
	 ("wfrp@employees.org" . "lists/wfrp")
	 ("wfrp@squinny.net" . "lists/wfrp-new")
	 ("wilson@iolinc.net" . "users/wilson_larry")
	 ("witt@access.mountain.net" . "users/witt_ken")
	 ("wvhtcf-gamers@tkb.mpl.com" . "lists/wvhtcf-gamers")
	 ("zanzair@access.mountain.net" . "users/zanzair")
	 ("zanzair@gatewaycomics.com" . "users/zanzair")
	 ("zanzair@mountain.net" . "users/zanzair")
	 ("zanzair@wvdsl.net" . "users/zanzair"))))

(defun tkb-wl-read-folder-map ()
  (interactive)
  (save-current-buffer
    (let* ((buf (find-file-noselect "~/lib/emacs/tkb/tkb-folder-map.el")))
      (set-buffer buf)
      (save-excursion
	(goto-char (point-min))
	(let ((mapping (read buf)))
	  (setq wl-refile-rule-alist mapping))))))
      
      

(defun tkb-wl-inc ()
  (interactive)
  (let* ((buf (get-buffer-create " *WL INC*"))
	 (win (display-buffer-other-frame buf))
	 )
    ;;(fit-window-to-buffer win 10)
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-max)))
    (call-process "inc" nil buf t)
    (wl-summary-sync nil "update")	; 'unset-cursor
    ))
(eval-after-load "wl-summary"
  '(progn 
     (define-key wl-summary-mode-map "i" #'tkb-wl-inc)
     (define-key wl-summary-mode-map "I" #'tkb-wl-inc)))



(setq wl-folder-access-subscribe-alist
      '(("+" . (nil "[._]~$" "^\\." "^#"))))

(eval-after-load "wl-vars"
  '(progn
     (require 'mime-view)
     (mapc #'(lambda (header)
	       (add-to-list 'wl-message-ignored-field-list header))
	   (append
	    mime-view-ignored-field-list
	    '("^MIME-Version:"
	      "^X-.*:"
	      "^List-.*:"
	      "DKIM-Signature:"
	      "DomainKey-Signature:"
	      "Received:"
	      "Comment:"
	      "Content-class:"
	      "Thread-Topic:"
	      "thread-index:"
	      "Mailing-List:"
	      "Delivered-To:"
	      "Received-SPF:"
	      "Authentication-Results:"
	      "DomainKey-Status:"
	      "Bounces-to:"
	      )))))


(defface tkb-wl-to '((((class color) (min-colors 8))
		      :background "blue" :foreground "white"))
  "face for displaying the To string in WL when it's from me")
;(defvar tkb-wl-to-string #("> " 0 1 (face tkb-wl-to-face)))
(defvar tkb-wl-to-string "> ")
; (setq tkb-wl-to-string "> ")

(defun tkb-wl-summary-default-from (from)
  "Instance of `wl-summary-from-function'.
Ordinarily returns the sender name. Returns recipient names if (1)
summary's folder name matches with `wl-summary-showto-folder-regexp'
and (2) sender address is yours.

See also variable `wl-use-petname'."
  (let ((translator (if wl-use-petname
			(lambda (string)
			  (or (funcall wl-summary-get-petname-function string)
			      (car (std11-extract-address-components string))
			      string))
		      #'identity))
	to ng)
    (or (and
	 ;; I had the eq commented out, but that caused citations to fail.
	 (eq major-mode 'wl-summary-mode) 
	     (wl-address-user-mail-address-p from)
	     (cond
	      ((setq to (elmo-message-entity-field wl-message-entity 'to))
	       (concat tkb-wl-to-string (mapconcat translator to ",")))
	      ((setq ng (elmo-message-entity-field wl-message-entity
						   'newsgroups))
	       (concat "Ng:" ng))))
	(funcall translator from))))
(setq wl-summary-from-function #'tkb-wl-summary-default-from)


(eval-after-load "elmo-imap4"
  '(progn
     (message "Munging elmo-imap4-accept-ok")
     ;; These two fix problem with NO FETCH Note not found thingie.
     (defun elmo-imap4-accept-ok (session tag)
       "Accept only `OK' response from SESSION.
If response is not `OK' response, causes error with IMAP response text."
       (let ((response (elmo-imap4-read-response session tag)))
	 (if (elmo-imap4-response-ok-p response)
	     response
	   (if (elmo-imap4-response-bye-p response)
	       (elmo-imap4-process-bye session)
	     (if (elmo-imap4-response-no-p response)
		 response
	       (error "IMAP error: %s"
		      (or (elmo-imap4-response-error-text response)
			  "No `OK' response from server.")))))))


     (defmacro elmo-imap4-response-no-p (response)
       "Returns non-nil if RESPONSE is an 'NO' response."
       `(assq 'no ,response))))


(defun tkb-wl-y-or-n-p-with-scroll (prompt &optional junk)
  (let ((prompt (concat prompt "y=send; n|q=abort; d|j|v|space=down; ^|k|u|del=up")))
    (catch 'done
      (while t
	(discard-input)
	(case (let ((cursor-in-echo-area t))
		(cdr (wl-read-event-char prompt)))
	  ((?y ?Y)
	   (throw 'done t))
	  ((?v ?j ?J next)
	   (ignore-errors (scroll-up)))
	  ((?^ ?k ?K prior backspace del ?)
	   (ignore-errors (scroll-down)))
	  (t
	   (throw 'done nil)))))))

(eval-after-load* "wl-draft" ()
  (setq wl-draft-send-confirm-type #'tkb-wl-y-or-n-p-with-scroll))

(when nil
  ;; http://thread.gmane.org/gmane.mail.wanderlust.general.japanese/5686/focus=5688
  ;; if we are given a choice between html and a hot poker in the eye, 
  ;; choose the poker.

  (eval-after-load "semi-setup"
    '(set-alist 'mime-view-type-subtype-score-alist '(text . html) 0)))

(progn
  ;; Cygwin fortune sometimes returns the empty string.
  ;; No, I don't know why. 
  (defun tkb-get-fortune ()
    (interactive)
    (with-current-buffer (generate-new-buffer "*tkb-fortune*")
      (call-process "fortune" nil t t
		    (if (member system-type '(ms-dos windows-nt cygwin))
			"~/lib/data/fortunes"
		      (expand-file-name "~/lib/data/fortunes")))
      (buffer-string)))

  (defun tkb-fortune-finally ()
    (interactive)
    (loop for i from 1
	  for fortune = (tkb-get-fortune) then (tkb-get-fortune)
	  until (not (empty-string-p fortune))
	  do (message "%d: %s" i fortune)
	  finally return fortune))

  (defun tkb-insert-fortune ()
    (interactive)
    (goto-char (point-max))
    (insert "\n" (tkb-fortune-finally) "\n"))

  (tkb-keys ("\C-ckS" #'tkb-insert-fortune)))

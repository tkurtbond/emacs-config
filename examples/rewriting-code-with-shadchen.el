(setq x (cddr '(tkb-keys ((kbd "C-c k o C-c") #'org-ctrl-c-ctrl-c)
               ((kbd "C-c k o F B") #'(lambda () "Blog Ideas"
                                        (interactive)
                                        (find-file tkb-org-blog-ideas)))
               ((kbd "C-c k o F j") #'(lambda () "TKB's Journal"
                                        (interactive)
                                        (find-file tkb-org-journal)))
               ((kbd "C-c k o F c") #'(lambda ()
                                        "TKB's Contacts"
                                        (interactive)
                                        (find-file tkb-org-contacts)))
               ((kbd "C-c k o F h") #'(lambda ()
                                        "TKB's Health"
                                        (interactive)
                                        (find-file tkb-org-health)))
               ((kbd "C-c k o F n") #'(lambda ()
                                        "TKB's Notes"
                                        (interactive)
                                        (find-file tkb-org-notes)))
               ((kbd "C-c k o F r") #'(lambda ()
                                        "TKB's RPG"
                                        (interactive)
                                        (find-file tkb-org-rpg)))
               ((kbd "C-c k o F t") #'(lambda ()
                                        "TKB's Tasks"
                                        (interactive)
                                        (find-file tkb-org-tasks)))
               ((kbd "C-c k o F v") #'(lambda ()
                                        "TKB's Video"
                                        (interactive)
                                        (find-file tkb-org-video)))
               ((kbd "C-c k o F J") #'(lambda ()
                                        "MPL Journal"
                                        (interactive)
                                        (find-file tkb-org-mpl-journal)))
               ((kbd "C-c k o F M") #'(lambda ()
                                        "MHST Journal"
                                        (interactive)
                                        (find-file tkb-org-mhst-journal)))
               ((kbd "C-c k o F C") #'(lambda ()
                                        "MPL Contacts"
                                        (interactive)
                                        (find-file tkb-org-mpl-contacts)))
               ((kbd "C-c k o F N") #'(lambda ()
                                        "MPL Notes"
                                        (interactive)
                                        (find-file tkb-org-mpl-notes)))
               ((kbd "C-c k o F T") #'(lambda ()
                                        "MPL Tasks"
                                        (interactive)
                                        (find-file tkb-org-mpl-tasks)))
               )))

(setq y (car x))

(defun process-item (item)
  (match-let (((list key-binding
                     (list _ (list _ _ desc _ (list _ file-var))))
               item))
    (let* ((file-var-as-string (symbol-name file-var))
           (parts (split-string file-var-as-string "-"))
           (func-parts (cons (car parts)
                             (cons "find"
                                   (cdr parts))))
           (func-as-string (string-join func-parts "-"))
           (func-name (intern func-as-string)))
      `((defun ,func-name () ,(concat "Find Org " desc)
               (interactive) (find-file ,file-var))
        (tkb-keys ,key-binding #',func-name))
      )))


(loop for item in x append (process-item item))

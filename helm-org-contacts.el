;;; helm-org-contacts.el --- A helm source for org-contacts

;; Copyright 2015 Titus von der Malsburg <malsburg@posteo.de>

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 0.0.1
;; Package-Requires: ((helm "1.5.5") (s "1.9.0") (dash "2.6.0") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A helm source for org-contacts.  See the Github page for details.
;;
;;   https://github.com/tmalsburg/helm-org-contacts


;;; Install:

;; Put this file in a directory included in your load path.  Then add
;; the following in your Emacs startup file:
;;
;;     (require 'helm-org-contacts)
;;
;; Requirements are helm, s, dash, and f.  Also make sure that
;; org-contacts is configured.
;;
;; At this time helm-org-contacts only processes entries stored in the
;; first file `org-contacts-files'.

;;; Usage:

;; You can search entries using the command
;; `helm-org-contacts'.  Select an entry and press TAB to access all
;; available actions.  Apart from that, familiarize yourself with
;; Helm.  It's more powerful that you might think.

;;; Code:

(require 'helm)
(require 'cl-lib)
(require 'dash)
(require 's)

(setq helm-org-contacts-org-property-separators
      '((("PHONE" "EMAIL" "ADDRESS" "ORG") . "\x1F")))

(defun helm-org-contacts-org-entry-properties (&optional epom)
  "Like org-entry-properties but using our own org-property-separators."
  (let ((org-property-separators helm-org-contacts-org-property-separators))
    (seq-reduce
     #'append
     (--map (org-entry-properties nil it) '("ITEM" "PHONE" "EMAIL" "ADDRESS" "ORG"))
     nil)))

(defvar helm-org-contacts-cache nil
  "Cache for entries.")

(defvar helm-org-contacts-file-watch-descriptor nil
  "Contacts file to watch.  Is set when contacts are retrieved.")

(defvar helm-org-contacts-autoreload t
  "Non-nil means that contacts are automatically reloaded when the
  contacts file changes on disk.")

(defun helm-org-contacts-get-contacts (&optional arg)
  (let* ((file (car org-contacts-files))
         (cached (alist-get file helm-org-contacts-cache)))
    (when helm-org-contacts-autoreload
                                        ; Remove potentially outdated watch (if file has changed):
      (file-notify-rm-watch helm-org-contacts-file-watch-descriptor)
                                        ; Add watch for current contacts file:
      (setq helm-org-contacts-file-watch-descriptor
            (file-notify-add-watch file '(change)
                                   (lambda (event)
                                     (when (eq (cadr event) 'changed) (setq helm-org-contacts-cache nil))))))
                                        ; Return cached contacts or reparse:
    (if (and cached (not arg)) cached
      (message "Reread contacts ...")
      (let ((contacts (helm-org-contacts-parse-contacts file)))
        (add-to-list 'helm-org-contacts-cache (cons file contacts))
        contacts))))

(defun helm-org-contacts-parse-contacts (file)
  (with-current-buffer (find-file-noselect file)
    (widen)
    (outline-show-all)
    (goto-char (point-min))
    (when (not (outline-on-heading-p))
      (outline-next-heading))
    (cl-loop
     while (outline-on-heading-p)
     for item = (substring-no-properties (org-get-heading t t t t))
     for marker = (point-marker)
     collect (helm-org-contacts-org-entry-properties)
     into entries
     collect marker into markers
     do (outline-next-heading)
     finally return
     (--zip-with
      (list (s-join " " (-map 'cdr it)) other it)
      entries
      markers))))

;; This is adapted from `json--plist-to-alist':
(defun helm-org-contacts-plist-to-alist (plist)
  "Return an alist of the property-value pairs in PLIST."
  (let (res)
    (while plist
      (let ((prop (pop plist))
            (val (pop plist)))
        (push (cons prop val) res)))
    res))

;; TODO Show something useful in the third column.  If org is
;; undefined, show notes.  If org == item, show notes, too.
(defun helm-org-contacts-candidate-transformer (candidates source)
  (cl-loop
   with width = (with-helm-window (helm-org-contacts-window-width))
   with width = (- (with-helm-window (helm-org-contacts-window-width)) 2)
   ;; with width = 117
   for entry in candidates
   for entry = (nth 2 entry)
   ;; Format fields for display:
   ;; for entry = (--map (cons (car it) (helm-org-contacts-format-field (cdr it) (car it))) entry)
   ;; Format entry for display:
   maximize (-max (cons 0 (-map 'length (helm-org-contacts-alist-get-all "ITEM" entry))))  into max-item
   maximize (-max (cons 0 (-map 'length (helm-org-contacts-alist-get-all "PHONE" entry)))) into max-phone
   maximize (-max (cons 0 (-map 'length (helm-org-contacts-alist-get-all "EMAIL" entry)))) into max-email
   maximize (-max (cons 0 (-map 'length (helm-org-contacts-alist-get-all "ORG" entry))))   into max-org
   collect entry into entries
   finally return
   (--zip-with
    (cons (helm-org-contacts-entry-formatter it max-item max-phone max-email max-org width)
          (cdr other))
    entries
    candidates)))

(defun helm-org-contacts-entry-formatter (entry max-item max-phone max-email max-org width)
  (let* ((items   (list (cdr (assoc "ITEM" entry))))
         (phones  (helm-org-contacts-alist-get-all "PHONE" entry))
         (phones  (helm-org-contacts-alist-get-all "PHONE" entry))
         (emails  (helm-org-contacts-alist-get-all "EMAIL" entry))
         (orgs    (helm-org-contacts-alist-get-all "ORG"   entry))
         (max-num (-max (-map 'length (list items phones emails orgs))))
         (items   (append items  (-repeat (- max-num (length items))  "")))
         (phones  (append phones (-repeat (- max-num (length phones)) "")))
         (emails  (append emails (-repeat (- max-num (length emails)) "")))
         (orgs    (append orgs   (-repeat (- max-num (length orgs  )) "")))
         (items   (--map (truncate-string-to-width it max-item  0 ?\s) items))
         (phones  (--map (truncate-string-to-width it max-phone 0 ?\s) phones))
         (emails  (--map (truncate-string-to-width it max-email 0 ?\s) emails))
         (orgs    (--map (truncate-string-to-width it max-org   0 ?\s) orgs)))
    (cl-loop
     while (> (length items) 0)
     collect
     (truncate-string-to-width
      (s-join " " (list (pop items) (pop phones) (pop emails) (pop orgs)))
      width 0 ?\s)
     into lines
     finally return (s-join "\n" lines))))

(defun helm-org-contacts-window-width ()
  (if (and (not (featurep 'xemacs))
           (display-graphic-p)
           overflow-newline-into-fringe
           (/= (frame-parameter nil 'left-fringe) 0)
           (/= (frame-parameter nil 'right-fringe) 0))
      (window-body-width)
    (1- (window-body-width))))

(defun helm-org-contacts-alist-get-all (prop alist)
  "Returns a list containing the values of all entries of 
ALIST that have PROP as the key."
  (if-let* ((value (alist-get prop alist nil nil #'string=)))
      (string-split value (string ?\x1f))
    nil))

;; (helm-org-contacts-format-field ";;Keplerstraße 17;70174 Stuttgart;Germany;; (WORK);;Werastraße 101;Stuttgart;;70190;Germany (HOME)" "ADDRESS")

(defun helm-org-contacts-format-field (value prop)
  (cond
   ((not value) "")
   ((string= prop "PHONE")
    (let ((parts (s-match "^\\(.+\\) *(\\(.+\\))$" value)))
      (if parts 
          (s-concat (s-trim (nth 1 parts))
                    " "
                    (helm-org-contacts-replace-all
                     '(("HOME" . "⌂")
                       ("WORK" . "⚒")
                       ("VOICE" . "☏")
                       ("CELL" . "📱")
                       ("MSG" ."✉")
                       ("IPHONE" . "i")
                       ("OTHER" . "")
                       ("MAIN" . "")
                       ("FAX" . "🖷")
                       ("pref" . "⭐")
                       (", " . ""))
                     (nth 2 parts)))
        value)))
   ((string= prop "EMAIL")
    (let ((parts (s-match "^\\(.+\\) *(\\(.+\\))$" value)))
      (if parts 
          (s-concat (s-trim (nth 1 parts))
                    " "
                    (helm-org-contacts-replace-all
                     '(("HOME" . "⌂")
                       ("WORK" . "⚒")
                       ("INTERNET" . "")
                       ("OTHER" . "")
                       ("pref" . "⭐")
                       (", " . ""))
                     (nth 2 parts)))
        value)))
   ((string= prop "ADDRESS")
    (->> (s-replace-regexp " *;+ *" "\n" value)
         (s-trim)))
   ((string= prop "N")
    (let ((parts (s-split ";" value)))
      (s-collapse-whitespace
       (s-trim
        (s-join " " (list (nth 3 parts)
                          (nth 1 parts)
                          (nth 2 parts)
                          (nth 0 parts)
                          (nth 4 parts)))))))
   ((string= prop "ORG")
    (s-replace ";" ", " value))
   (t value)))

(defun helm-org-contacts-replace-all (replacements s)
  (cl-loop
   for r in replacements
   do (setq s (s-replace (car r) (cdr r) s)))
  s)

(defun helm-org-contacts-remove-flags (string)
  (replace-regexp-in-string " +([^)]+)$" "" string))

(defun helm-org-contacts-insert-addresses (_)
  "Insert marked elements at point."
  (let ((elements (helm-marked-candidates :with-wildcard t)))
    (with-helm-current-buffer
      (insert (s-join "\n\n" elements)))))

(defun helm-org-contacts-insert-address (entry)
  (print entry)
  (let* ((addresses (helm-org-contacts-alist-get-all "ADDRESS" (cadr entry)))
         (addresses (--map (helm-org-contacts-format-field it "ADDRESS") addresses))
         (addresses (--map (cons it (helm-org-contacts-remove-flags it)) addresses)))
    (pcase (length addresses)
      (`0 (message "No address found."))
      (`1 (with-helm-current-buffer
            (insert (cdar addresses))))
      (_ (run-with-timer 0.01 nil #'helm :sources (helm-build-sync-source "Addresses"
                                                    :candidates addresses
                                                    :multiline t
                                                    :action '(("Insert address" . helm-org-contacts-insert-addresses))))))))

(defun helm-org-contacts-insert-emails (_)
  "Insert marked elements at point."
  (let ((elements (helm-marked-candidates :with-wildcard t)))
    (with-helm-current-buffer
      (insert (s-join ", " elements)))))

(defun helm-org-contacts-insert-email-with-name (entry)
  (let* ((emails (helm-org-contacts-alist-get-all "EMAIL" (cadr entry)))
         (emails (--map (cons (helm-org-contacts-format-field it "EMAIL")
                              (format "%s <%s>"
                                      (cdr (assoc "ITEM" (cadr entry)))
                                      (helm-org-contacts-remove-flags it)))
                        emails)))
    (pcase (length emails)
      (`0 (message "No email address found."))
      (`1 (with-helm-current-buffer
            (insert (cdar emails))))
      (_ (run-with-timer 0.01 nil #'helm :sources (helm-build-sync-source "Emails"
                                                    :candidates emails
                                                    :action '(("Insert email address" . helm-org-contacts-insert-emails))))))))

(defun helm-org-contacts-insert-plain-email (entry)
  (let* ((emails (helm-org-contacts-alist-get-all "EMAIL" (cadr entry)))
         (emails (--map (cons (helm-org-contacts-format-field it "EMAIL")
                              (helm-org-contacts-remove-flags it))
                        emails)))
    (pcase (length emails)
      (`0 (message "No email address found."))
      (`1 (with-helm-current-buffer
            (insert (cdar emails))))
      (_ (run-with-timer 0.01 nil #'helm :sources (helm-build-sync-source "Emails"
                                                    :candidates emails
                                                    :action '(("Insert email address" . helm-org-contacts-insert-emails))))))))

(defun helm-org-contacts-insert-phone-number (entry)
  (let* ((phones (helm-org-contacts-alist-get-all "PHONE" (cadr entry)))
         (phones (--map (cons (helm-org-contacts-format-field it "PHONE")
                              (helm-org-contacts-remove-flags it))
                        phones)))
    (pcase (length phones)
      (`0 (message "No phone number found."))
      (`1 (with-helm-current-buffer
            (insert (cdar phones))))
      (_ (run-with-timer 0.01 nil #'helm :sources (helm-build-sync-source "Phone numbers"
                                                    :candidates phones
                                                    :action '(("Insert phone number" . helm-org-contacts-insert-emails))))))))

(defun helm-org-contacts-edit-entry (entry)
  (let* ((marker (car entry))
         (buffer (marker-buffer marker)))
    (switch-to-buffer buffer)
    (goto-char marker)
  (widen)
  (show-all)
  (goto-char (car entry))
  (org-narrow-to-subtree)
  (org-show-all)))

(defun helm-org-contacts-insert-contact-link (entry)
  (let ((name (alist-get "ITEM" (cadr entry) nil nil #'string=))
        (file (buffer-file-name (marker-buffer (car entry)))))
    (insert (format "[[file:%s::*%s][%s]]" file name name))))

(setq helm-source-org-contacts
      '((name                           . "Contacts")
        (multiline)
        (candidates                     . helm-org-contacts-get-contacts)
        (filtered-candidate-transformer . helm-org-contacts-candidate-transformer)
        (action . (("Insert postal address" . helm-org-contacts-insert-address)
                   ("Insert plain email address" . helm-org-contacts-insert-plain-email)
                   ("Insert email address with name" . helm-org-contacts-insert-email-with-name)
                   ("Insert phone number" . helm-org-contacts-insert-phone-number)
                   ("Insert contact link" . helm-org-contacts-insert-contact-link)
                   ("Show entry"        . helm-org-contacts-edit-entry)))))

(defun helm-org-contacts (&optional arg)
  "Search entries in org-contacts.

With a prefix ARG, the cache is invalidated and the contacts are
reloaded.  Note that a forced reload should not be necessary
since the contacts file is automatically reloaded when it changes
on disk."
  (interactive "P")
  (when arg
    (setq helm-org-contacts-cache nil))
  (helm :sources '(helm-source-org-contacts)
        :full-frame t
        :candidate-number-limit 500))

(provide 'helm-org-contacts)

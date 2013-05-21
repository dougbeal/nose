;;; nose2.el --- Easy Python test running in Emacs

;; Copyright (C) 2009 Jason Pellerin, Augie Fackler

;; Licensed under the same terms as Emacs.

;; Version: 0.1.1
;; Keywords: nose2 python testing
;; Created: 04 Apr 2009

;; This file is NOT part of GNU Emacs.

;; Licensed under the same terms as Emacs.

;;; Commentary:
;; This gives a bunch of functions that handle running nose2 on a
;; particular buffer or part of a buffer.

;;; Installation

;; In your emacs config:
;;
;; (require 'nose2)
;; ; next line only for people with non-eco non-global test runners
;; ; (add-to-list 'nose2-project-names "my/crazy/runner")

;; Note that if your global nose2 isn't called "nosetests", then you'll want to
;; redefine nose2-global-name to be the command that should be used.

;; By default, the root of a project is found by looking for any of the files
;; 'setup.py', '.hg' and '.git'. You can add files to check for to the file
;; list:
;;
;; ; (add-to-list 'nose2-project-root-files "something")

;; or you can change the project root test to detect in some other way
;; whether a directory is the project root:
;;
;; ; (setq nose2-project-root-test (lambda (dirname) (equal dirname "foo")))

;; If you want dots as output, rather than the verbose output:
;; (defvar nose2-use-verbose nil) ; default is t

;; Probably also want some keybindings:
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (local-set-key "\C-ca" 'nose2tests-all)
;;             (local-set-key "\C-cm" 'nose2tests-module)
;;             (local-set-key "\C-c." 'nose2tests-one)
;;             (local-set-key "\C-cpa" 'nose2tests-pdb-all)
;;             (local-set-key "\C-cpm" 'nose2tests-pdb-module)
;;             (local-set-key "\C-cp." 'nose2tests-pdb-one)))

(require 'cl) ;; for "reduce"

(defvar nose2-project-names '("eco/bin/test"))
(defvar nose2-project-root-files '("setup.py" ".hg" ".git"))
(defvar nose2-project-root-test 'nose-project-root)
(defvar nose2-global-name "nose2")
(defvar nose2-use-verbose t)

(defun run-nose2 (&optional tests debug failed)
  "run nose2"
  (let* ((nose2 (nose2-find-test-runner))
         (where (nose2-find-project-root))
         (args (concat (if debug "--pdb" "")
                       " "
                       (if failed "--failed" "")))
         (tnames (if tests tests "")))
    (if (not where)
        (error
         (format (concat "abort: nosemacs couldn't find a project root, "
                         "looked for any of %S") nose2-project-root-files)))
    (funcall (if debug
                 'pdb
               '(lambda (command)
                  (compilation-start command
                                     nil
                                     (lambda (mode) (concat "*nosetests*")))))
             (format
              (concat "%s "
                      (if nose2-use-verbose "-v " "")
                      "%s --start-dir %s --config %ssetup.cfg %s")
              (nose2-find-test-runner) args where where tnames)))
  )

(defun nose2tests-all (&optional debug failed)
  "run all tests"
  (interactive)
  (run-nose2 nil debug failed))

(defun nose2tests-failed (&optional debug)
  (interactive)
  (nose2tests-all debug t))

(defun nose2tests-pdb-all ()
  (interactive)
  (nose2tests-all t))

(defun nose2tests-module (&optional debug)
  "run nose2tests (via eggs/bin/test) on current buffer"
  (interactive)
  (run-nose2 buffer-file-name debug))

(defun nose2tests-pdb-module ()
  (interactive)
  (nose2tests-module t))

(defun nose2tests-one (&optional debug)
  "run nose2tests (via eggs/bin/test) on testable thing
 at point in current buffer"
  (interactive)
  (run-nose2 (format "%s:%s" buffer-file-name (nose2-py-testable)) debug))

(defun nose2tests-pdb-one ()
  (interactive)
  (nose2tests-one t))

(defun nose2-find-test-runner ()
  (message
   (let ((result
          (reduce '(lambda (x y) (or x y))
        (mapcar 'nose2-find-test-runner-names nose2-project-names))))
     (if result
         result
       nose2-global-name))))

(defun nose2-find-test-runner-names (runner)
  "find eggs/bin/test in a parent dir of current buffer's file"
  (nose2-find-test-runner-in-dir-named
   (file-name-directory buffer-file-name) runner))

(defun nose2-find-test-runner-in-dir-named (dn runner)
  (let ((fn (expand-file-name runner dn)))
    (cond ((file-regular-p fn) fn)
      ((equal dn "/") nil)
      (t (nose2-find-test-runner-in-dir-named
          (file-name-directory (directory-file-name dn))
          runner)))))

(defun nose2-py-testable ()
  (let* ((inner-obj (inner-testable))
         (outer (outer-testable))
         ;; elisp can't return multiple values
         (outer-def (car outer))
         (outer-obj (cdr outer)))
    (cond ((equal outer-def "def") outer-obj)
          ((equal inner-obj outer-obj) outer-obj)
          (t (format "%s.%s" outer-obj inner-obj)))))

(defun inner-testable ()
  (save-excursion
    (re-search-backward
     "^ \\{0,4\\}\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

(defun outer-testable ()
  (save-excursion
    (re-search-backward
     "^\\(class\\|def\\)[ \t]+\\([a-zA-Z0-9_]+\\)" nil t)
    (let ((result
            (buffer-substring-no-properties (match-beginning 2) (match-end 2))))

      (cons
       (buffer-substring-no-properties (match-beginning 1) (match-end 1))
       result))))

(defun nose2-find-project-root (&optional dirname)
  (let ((dn
         (if dirname
             dirname
           (file-name-directory buffer-file-name))))
    (cond ((funcall nose2-project-root-test dn) (expand-file-name dn))
          ((equal (expand-file-name dn) "/") nil)
        (t (nose2-find-project-root
             (file-name-directory (directory-file-name dn)))))))

(defun nose2-project-root (dirname)
  (reduce '(lambda (x y) (or x y))
          (mapcar (lambda (d) (member d (directory-files dirname)))
                  nose2-project-root-files)))

(provide 'nose2)

;;; nose2.el ends here

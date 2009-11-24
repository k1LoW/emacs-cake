;;; anything-c-cake.el --- CakePHP Minor Mode anything.el interface

;; Copyright (C) 2008-2009 by 101000code/101000LAB
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
;;
;; Version: 1.1.8
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org, http://trac.codecheck.in

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-c-cake-anything-only-source-cake'
;;    anything only anything-c-source-cake and anything-c-source-cake-model-function.
;;  `anything-c-cake-anything-only-model-function'
;;    anything only anything-c-source-cake-model-function.
;;  `anything-c-cake-anything-only-po'
;;    anything only anything-c-source-cake-po.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; Change Log
;; 1.1.8:Bug fix.
;; 1.1.7:New valiables anything-c-source-cake-component-function
;; 1.1.6:add migemo.
;; 1.1.5:grep command bug fix.
;; 1.1.4:New function anything-c-cake-anything-only-po. Fix doc.
;; 1.1.3:New valiables anything-c-cake-po.
;; 1.1.2:sed command bug fix.
;; 1.1.1:sed command bug fix.
;; 1.1.0:Refactor code.
;; 1.0.9:grep command bug fix.
;; 1.0.8:anything-c-cake-anything-only-source-cake bug fix.
;; 1.0.7:New function anything-c-cake-anything-only-model-function
;; 1.0.6:New valiables anything-c-source-cake-model-function.
;; 1.0.5:anything-c-cake-anything-only-source-cake関数を実装
;; 1.0.4:executable-findを導入
;; 1.0.3:cake.el 0.1.5に対応
;; 1.0.2:action(function)がprivateやpublic指定されていた場合表示が崩れるバグを修正
;; 1.0.1:anything-c-cake-set-namesでcake-singular-nameが空白になるバグ修正
;; 1.0.0:action "Switch to Controller","Switch to View","Switch to Model"作成

;; TODO
;; anyhing-c-cake-switch-to-*がcake-switch-to-*に似ているのでなんとか小さくしたい

;;; Code:

(require 'anything)
(require 'cake)

(defvar cake-candidate-function-name nil)

(defvar anything-c-cake-po-file-buffer-name "*Cake Po*")

(defvar anything-c-source-cake
  '((name . "Cake Switch")
    (init
     . (lambda ()
         (if
             (and (cake-set-app-path) (executable-find "grep") (executable-find "sed"))
             (call-process-shell-command
              (concat "grep '[^_]function' " cake-app-path "controllers/*controller.php --with-filename | sed 's/.\\+\\/\\(.\\+\\)_controller\\.php:.*function *\\([^ ]\\+\\) *(.*).*/\\1 \\/ \\2/g'") nil (anything-candidate-buffer 'global))
           (call-process-shell-command nil nil (anything-candidate-buffer 'global))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake-set-names)
    (action
     ("Switch to Contoroller" . (lambda (candidate)
                                  (anything-c-cake-switch-to-controller)))
     ("Switch to View" . (lambda (candidate)
                           (anything-c-cake-switch-to-view)))
     ("Switch to Model" . (lambda (candidate)
                            (anything-c-cake-switch-to-model)))
     )))

(defun anything-c-cake-set-names (candidate)
  "Set names by display-to-real"
  (progn
    (string-match "\\(.+\\) / \\(.+\\)" candidate)
    (setq cake-plural-name (match-string 1 candidate))
    (setq cake-action-name (match-string 2 candidate))
    (cake-convert-plural-to-singular cake-singular-rules)
    (setq cake-lower-camelized-action-name cake-action-name)
    (setq cake-snake-action-name (cake-snake cake-action-name))
    ))

(defun anything-c-cake-switch-to-model ()
  "Switch to model."
  (if (file-exists-p (concat cake-app-path "models/" cake-singular-name ".php"))
      (find-file (concat cake-app-path "models/" cake-singular-name ".php"))
    (if (y-or-n-p "Make new file?")
        (find-file (concat cake-app-path "models/" cake-singular-name ".php"))
      (message (format "Can't find %s" (concat cake-app-path "models/" cake-singular-name ".php"))))))

(defun anything-c-cake-switch-to-view ()
  "Switch to view."
  (progn
    (cond ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name "." cake-view-extension))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name "." cake-view-extension)))
          ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name ".thtml"))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name ".thtml")))
          ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name ".ctp"))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-snake-action-name ".ctp")))
          ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension)))
          ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-action-name ".thtml"))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-action-name ".thtml")))
          ((file-exists-p (concat cake-app-path "views/" cake-plural-name "/" cake-action-name ".ctp"))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-action-name ".ctp")))
          ((y-or-n-p "Make new file?")
           (unless (file-directory-p (concat cake-app-path "views/" cake-plural-name "/"))
             (make-directory (concat cake-app-path "views/" cake-plural-name "/")))
           (find-file (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension)))
          (t (message (format "Can't find %s" (concat cake-app-path "views/" cake-plural-name "/" cake-action-name "." cake-view-extension)))))))

(defun anything-c-cake-switch-to-controller ()
  "Switch to contoroller."
  (progn
    (if (file-exists-p (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
        (progn
          (find-file (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
          (goto-char (point-min))
          (if (not (re-search-forward (concat "function[ \t]*" cake-lower-camelized-action-name "[ \t]*\(") nil t))
              (progn
                (goto-char (point-min))
                (re-search-forward (concat "function[ \t]*" cake-action-name "[ \t]*\(") nil t))))
      (if (y-or-n-p "Make new file?")
          (find-file (concat cake-app-path "controllers/" cake-plural-name "_controller.php"))
        (message (format "Can't find %s" (concat cake-app-path "controllers/" cake-plural-name "_controller.php")))))))

(defun anything-c-cake-switch-to-component ()
  "Switch to component."
  (if (file-exists-p (concat cake-app-path "controllers/components/" cake-singular-name ".php"))
      (find-file (concat cake-app-path "controllers/components/" cake-singular-name ".php"))
    (if (y-or-n-p "Make new file?")
        (find-file (concat cake-app-path "controllers/components/" cake-singular-name ".php"))
      (message (format "Can't find %s" (concat cake-app-path "controllers/components/" cake-singular-name ".php"))))))

(defvar anything-c-source-cake-model-function
  '((name . "Cake Model Function Switch")
    (init
     . (lambda ()
         (if
             (and (cake-set-app-path) (executable-find "grep") (executable-find "sed"))
             (call-process-shell-command
              (concat "grep '[^_]function' " cake-app-path "models/*.php --with-filename | sed 's/.\\+\\/\\(.\\+\\)\\.php:.*function *\\([^ ]\\+\\) *(.*).*/\\1 \\/ \\2/g'") nil (anything-candidate-buffer 'global))
           (call-process-shell-command nil nil (anything-candidate-buffer 'global))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake-set-names2)
    (action
     ("Switch to Function" . (lambda (candidate)
                               (anything-c-cake-switch-to-model)
                               (goto-char (point-min))
                               (re-search-forward (concat "function[ \t]*" cake-candidate-function-name "[ \t]*\(") nil t)))
     )))

(defvar anything-c-source-cake-component-function
  '((name . "Cake Component Function Switch")
    (init
     . (lambda ()
         (if
             (and (cake-set-app-path) (executable-find "grep") (executable-find "sed"))
             (call-process-shell-command
              (concat "grep '[^_]function' " cake-app-path "controllers/components/*.php --with-filename | sed 's/.\\+\\/\\(.\\+\\)\\.php:.*function *\\([^ ]\\+\\) *(.*).*/\\1 \\/ \\2/g'") nil (anything-candidate-buffer 'global))
           (call-process-shell-command nil nil (anything-candidate-buffer 'global))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake-set-names2)
    (action
     ("Switch to Function" . (lambda (candidate)
                               (anything-c-cake-switch-to-component)
                               (goto-char (point-min))
                               (re-search-forward (concat "function[ \t]*" cake-candidate-function-name "[ \t]*\(") nil t)))
     )))

(defun anything-c-cake-set-names2 (candidate)
  "Set names by display-to-real"
  (progn
    (string-match "\\(.+\\) / \\(.+\\)" candidate)
    (setq cake-singular-name (match-string 1 candidate))
    (setq cake-candidate-function-name (match-string 2 candidate))
    ))

(defun anything-c-cake-create-po-file-buffer ()
  "Create buffer from po file."
  (let ((anything-buffer (anything-candidate-buffer 'global)))
    (catch 'invalid-po-file
      (unless (anything-c-cake-generate-po-file-buffer (concat cake-app-path "locale/" cake-po-file-path))
        (message "Can't find po file: %s" (concat cake-app-path "locale/" cake-po-file-path))
        (throw 'invalid-po-file nil))
      (with-current-buffer anything-buffer
        (set-syntax-table (with-current-buffer anything-current-buffer
                            (syntax-table)))
        (insert-buffer-substring anything-c-cake-po-file-buffer-name))
      )))

(defun anything-c-cake-generate-po-file-buffer (po-file)
  "Generate po file buffer"
  (when (and po-file
             (file-exists-p po-file)
             (file-regular-p po-file))
    (with-current-buffer (get-buffer-create anything-c-cake-po-file-buffer-name)

      (erase-buffer)
      (insert-file-contents po-file)

      (goto-char (point-min))
      (while (re-search-forward "^[^m].*\n" nil t)
        (replace-match ""))

      (goto-char (point-min))
      (while (re-search-forward "^msgid \"\\(.*\\)\"\nmsgstr \"\\(.*\\)\"$" nil t)
        (replace-match "\\1 / \\2"))
      )
    t)
  )

(defvar anything-c-source-cake-po
  '((name . "Cake po file's msgid and msgstr")
    (init . (anything-c-cake-create-po-file-buffer))
    (candidates-in-buffer)
    (action
     ("Insert __('msgid')." . (lambda (candidate)
                                (insert (concat "__('" (anything-c-cake-get-msgid candidate) "')"))))
     ("Insert __('msgid',true)." . (lambda (candidate)
                                     (insert (concat "__('" (anything-c-cake-get-msgid candidate) "',true)"))))
     ("Insert msgid." . (lambda (candidate)
                          (insert (anything-c-cake-get-msgid candidate))))
     ("Goto po file" . (lambda (candidate)
                         (find-file (concat cake-app-path "locale/" cake-po-file-path))
                         (goto-char (point-min))
                         (re-search-forward (concat "\"" (anything-c-cake-get-msgid candidate) "\"") nil t)))
     )))

(defun anything-c-cake-get-msgid (candidate)
  "Set msgid"
  (progn
    (string-match "\\(.+\\) /" candidate)
    (match-string 1 candidate)
    ))

(defun anything-c-cake-anything-only-source-cake ()
  "anything only anything-c-source-cake and anything-c-source-cake-model-function."
  (interactive)
  (anything (list anything-c-source-cake
                  anything-c-source-cake-model-function
                  anything-c-source-cake-component-function)
            nil "Find CakePHP Sources: " nil nil))

(defun anything-c-cake-anything-only-model-function ()
  "anything only anything-c-source-cake-model-function."
  (interactive)
  (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
    (anything '(anything-c-source-cake-model-function) initial-pattern "Find Model Functions: " nil)))

(defun anything-c-cake-anything-only-po ()
  "anything only anything-c-source-cake-po."
  (interactive)
  (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
    (anything '(anything-c-source-cake-po) initial-pattern "Find Msgid And Msgstr: " nil)))

(define-key cake-key-map "\C-cl" 'anything-c-cake-anything-only-source-cake)
(define-key cake-key-map "\C-co" 'anything-c-cake-anything-only-model-function)
(define-key cake-key-map "\C-cp" 'anything-c-cake-anything-only-po)

(provide 'anything-c-cake)

;;; Code ends

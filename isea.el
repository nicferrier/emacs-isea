;;; isea.el --- interactive server eval at mode, a comint for a daemonized emacs -*- lexical-binding: t -*-

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: processes, lisp

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

;; This let's you run an inferior lisp mode with a daemomized Emacs
;; process.

;; Just do

;;  M-x isea

;; Supply a daemon socket name (just the socket name, no need for the
;; path) and ISEA will open a comint and send the daemon stuff to eval.

;;; Code:

(require 'comint)
(require 'server)
(defconst isea-prompt "ISEA> ")

(defconst isea/lisp
  '(cond
    ((bufferp -isea-response)
     (format ":buffer %s created" (buffer-name -isea-response)))
    ((processp -isea-response)
     (format ":process %s created" (process-name -isea-response)))
    (t -isea-response)))

(defun isea/input-sender (proc input)
  "Send input to the associated daemon Emacs."
  (let ((buffer-read-only nil)
        (lb (- (line-beginning-position) 5))
        (lisp-to-send (car (read-from-string input))))
    (if (or (equal lisp-to-send '(quit))
            (equal lisp-to-send 'quit))
        (comint-quit-subjob)
        ;; Else send input normally
        (condition-case err
            (let ((output
                   (server-eval-at
                    (process-get proc :isea/daemon-name)
                    `(let ((-isea-response ,lisp-to-send)) ,isea/lisp))))
              (comint-output-filter proc (format "%s\n" output)))
          (error (comint-output-filter
                  proc
                  (format "something went wrong %S\n" err))))
        (comint-output-filter proc isea-prompt))))

(defvar isea/daemon-name nil
  "Designed to be let bound to the name of the daemon.

This is used as the communication channel between the mode
creation function and the main `isea' function.")

(define-derived-mode
    isea/mode comint-mode "ISEA"
    "Run a shell for an external Emacs Daemon."
    :syntax-table emacs-lisp-mode-syntax-table
    (setq comint-prompt-regexp (concat "^" (regexp-quote isea-prompt)))
    (setq comint-input-sender 'isea/input-sender)
    (unless (comint-check-proc (current-buffer))
      ;; Was cat, but on non-Unix platforms that might not exist, so
      ;; use hexl instead, which is part of the Emacs distribution.
      (let ((fake-proc
             (condition-case nil
                 (start-process "isea" (current-buffer) "hexl")
               (file-error (start-process "isea" (current-buffer) "cat")))))
        (process-put fake-proc :isea/daemon-name isea/daemon-name)
        (set-process-query-on-exit-flag fake-proc nil)
        ;; Add a silly header
        (insert "** Interactive Server Eval At Mode **\n")
        (set-marker
         (process-mark fake-proc) (point))
        (comint-output-filter fake-proc isea-prompt))))

(defvar isea/daemon-name-history nil
  "History of isea/daemon-names.")

(defun isea (daemon-name)
  "Open a Lisp shell to the Emacs daemon socket."
  (interactive
   (list
    (read-from-minibuffer
     "Inferior Deamon name: " nil nil nil 'isea/daemon-name-history)))
  (let ((isea/daemon-name daemon-name))
    (with-current-buffer
        (get-buffer-create (format "*isea-%s*" daemon-name))
      (isea/mode)
      (switch-to-buffer (current-buffer)))))

(provide 'isea)

;;; isea.el ends here
